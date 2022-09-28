from dataclasses import dataclass
from enum import Enum, unique
from functools import lru_cache, reduce, singledispatchmethod
from typing import Dict, List, Mapping, Optional, Protocol, Sequence, Tuple, Union, cast

import asn1tools as asn1
import frozendict
from asn1tools.codecs import ber
from asn1tools.codecs.ber import Tag as AsnTagNum
from more_itertools import windowed
from more_itertools.recipes import flatten

from rflx import model
from rflx.error import RecordFluxError, Subsystem, fail
from rflx.expression import And, Equal, Expr, Mul, Number, Size, Variable
from rflx.identifier import ID
from rflx.model.message import FINAL, INITIAL, Field, Link
from rflx.model.type_ import OPAQUE


def strid(ident: Union[str, Sequence[str], ID]) -> str:
    return str(ID(ident)) if ident else ""


def from_asn1_name(ident: str) -> str:
    """Convert an ASN.1 identifier to an Ada one."""
    return ident.replace("-", "_")


PRELUDE_NAME: str = "Prelude"


class AsnTagClass:
    UNIVERSAL = 0b00
    APPLICATION = 0b01
    CONTEXT_SPECIFIC = 0b10
    PRIVATE = 0b11


class AsnTagForm:
    PRIMITIVE = 0
    CONSTRUCTED = 1


@dataclass(frozen=True)
class AsnTag:
    num: int = AsnTagNum.END_OF_CONTENTS
    class_: int = AsnTagClass.UNIVERSAL
    form: int = AsnTagForm.PRIMITIVE

    LONG_TAG_UNSUPPORTED_ERROR = NotImplementedError(
        "Long ASN.1 Tags are not supported for the moment"
    )

    @classmethod
    @lru_cache
    def rflx_type(cls, skip_proof: bool = False) -> model.Type:
        """ASN Tag message type in RecordFlux."""
        return simple_message(
            strid([PRELUDE_NAME, "Asn_Tag"]),
            {
                "Class": ASN_TAG_CLASS_TY,
                "Form": ASN_TAG_FORM_TY,
                "Num": ASN_TAG_NUM_TY,
            },
            skip_proof=skip_proof,
        )

    @lru_cache
    def matches(self, ident: str) -> Expr:
        """Return the matching condition of this `AsnTag` in RecordFlux."""
        kvs = {"Class": self.class_, "Form": self.form, "Num": self.num}
        eqs = (cast(Expr, Equal(Variable(f"{ident}_{k}"), Number(v))) for k, v in kvs.items())
        return reduce(And, eqs)

    @property
    def as_bytearray(self) -> bytearray:
        byte: int = (self.class_ << 1 | self.form) << 5 | self.num
        return bytearray([byte])

    @classmethod
    def from_bytearray(cls, arr: bytearray) -> "AsnTag":
        if len(arr) != 1:
            raise cls.LONG_TAG_UNSUPPORTED_ERROR
        byte: int = arr[0]
        return AsnTag(
            num=byte & ((1 << 5) - 1),
            form=byte >> 5 & 1,
            class_=byte >> 6,
        )


@unique
class AsnRawBoolean(Enum):
    B_FALSE = 0x00
    B_TRUE = 0xFF


class BerType(Protocol):
    @property
    def path(self) -> str:
        """Parent path of this type, eg. `Prelude`."""
        return ""

    @property
    def ident(self) -> str:
        """Type identifier, eg. `INTEGER`."""
        raise NotImplementedError

    @property
    def full_ident(self) -> ID:
        """Fully qualified identifier of this type, eg. `Prelude::INTEGER`."""
        return ID(list(filter(None, [self.path, self.ident])))

    @property
    def tag(self) -> AsnTag:
        raise NotImplementedError(f"no tag definition found for type `{type(self)}`: got {self}")

    @lru_cache(1)
    def v_ty(self, skip_proof: bool = False) -> model.Type:
        """`RAW` RecordFlux representation of this type."""
        return OPAQUE

    @lru_cache
    def lv_ty(self, skip_proof: bool = False) -> model.Type:
        """`Untagged`, length-value (LV) encoding of this type."""
        f = Field
        links = [
            Link(INITIAL, f("Length")),
            Link(f("Length"), f("Value"), size=Mul(Variable("Length"), Number(8))),
            Link(f("Value"), FINAL),
        ]
        fields = {
            f("Length"): ASN_LENGTH_TY,
            f("Value"): self.v_ty(skip_proof=skip_proof),
        }
        full_ident = strid(list(filter(None, [self.path, "Untagged_" + self.ident])))
        try:
            result = (
                model.UnprovenMessage(full_ident, links, fields)
                .merged()
                .proven(skip_proof=skip_proof)
            )
        except RecordFluxError as e:
            fail(f'invalid message "{self}": {e}', subsystem=Subsystem.CONVERTER)

        return result

    @lru_cache
    def tlv_ty(self, skip_proof: bool = False) -> model.Type:
        """Tag-length-value (TLV) encoding of this type."""
        lv_ty = self.lv_ty(skip_proof=skip_proof)
        f = Field
        try:
            tag_match = self.tag.matches("Tag")
        except NotImplementedError:
            return self.v_ty(skip_proof=skip_proof)
        links = [
            Link(INITIAL, f("Tag")),
            Link(f("Tag"), f("Untagged"), condition=tag_match),
            Link(f("Untagged"), FINAL),
        ]
        fields = {f("Tag"): ASN_TAG_TY, f("Untagged"): lv_ty}
        try:
            result = (
                model.UnprovenMessage(self.full_ident, links, fields)
                .merged()
                .proven(skip_proof=skip_proof)
            )
        except RecordFluxError as e:
            fail(f'invalid message "{self.full_ident}": {e}', subsystem=Subsystem.CONVERTER)
        return result

    @lru_cache
    def implicitly_tagged(self, tag: AsnTag, path: Optional[str]) -> "ImplicitlyTaggedBerType":
        """
        `IMPLICIT` variant of this type.

        Its tag-length-value (TLV) encoding is equivalent to
        its regular TLV encoding with a custom tag override.
        """
        return ImplicitlyTaggedBerType(
            self,
            AsnTag(num=tag.num, class_=tag.class_, form=self.tag.form),
            path or self.path,
        )

    @lru_cache
    def explicitly_tagged(self, tag: AsnTag, path: str) -> "ImplicitlyTaggedBerType":
        """
        `EXPLICIT` tag-length-value (TLV) encoding of this type.

        It is equivalent to its regular TLV encoding nested in an implicitly-tagged,
        single-field `SEQUENCE` type.
        """
        return SequenceBerType(
            path,
            "Explicit_" + self.ident,
            # A `frozendict` is required here to comply with `lru_cache`.
            frozendict.FrozenOrderedDict({"Inner": self}),
        ).implicitly_tagged(tag, path)


@dataclass(frozen=True)
class SimpleBerType(BerType):
    """A `BerType` with a known tag."""

    _path: str

    @property
    def path(self) -> str:
        return self._path

    _ident: str

    @property
    def ident(self) -> str:
        return self._ident

    _tag: AsnTag

    @property
    def tag(self) -> AsnTag:
        return self._tag


@dataclass(frozen=True)
class DefiniteBerType(SimpleBerType):
    """
    A `SimpleBerType` with a known underlying RecordFlux Type other than `OPAQUE`.

    e.g. `BOOLEAN`, `NULL`
    """

    _v_ty: model.Type

    @lru_cache
    def v_ty(self, skip_proof: bool = False) -> model.Type:
        return self._v_ty

    @lru_cache
    def lv_ty(self, skip_proof: bool = False) -> model.Type:
        """`Untagged`, length-value (LV) encoding of this type."""
        f = Field
        v_ty = self.v_ty(skip_proof=skip_proof)
        links = [Link(INITIAL, f("Length"))]
        fields = {f("Length"): cast(model.Type, ASN_LENGTH_TY)}
        if isinstance(v_ty, model.AbstractMessage):
            if not v_ty.structure or not v_ty.types:
                # Special case for NULL, since its v_ty is `null message`.
                links.append(Link(f("Length"), FINAL))
        else:
            len_match = Equal(Size("Length"), Size(v_ty.full_name))
            links += [
                Link(f("Length"), FINAL, condition=-len_match),
                Link(f("Length"), f("Value"), condition=len_match),
                Link(f("Value"), FINAL),
            ]
            fields[f("Value")] = v_ty
        full_ident = strid(list(filter(None, [self.path, "Untagged_" + self.ident])))
        return model.Message(full_ident, links, fields, skip_proof=skip_proof)


@dataclass(frozen=True)
class SequenceBerType(BerType):
    _path: str

    @property
    def path(self) -> str:
        return self._path

    _ident: str

    @property
    def ident(self) -> str:
        return self._ident

    fields: Mapping[str, BerType]

    @lru_cache(1)
    def v_ty(self, skip_proof: bool = False) -> model.Type:
        # A `SEQUENCE` is just a `message` of all its `root_members`.
        return simple_message(
            strid(self.full_ident),
            {f: t.tlv_ty(skip_proof) for f, t in self.fields.items()},
            skip_proof=skip_proof,
        )

    @property
    def tag(self) -> AsnTag:
        return AsnTag(form=AsnTagForm.CONSTRUCTED, num=AsnTagNum.SEQUENCE)


@dataclass(frozen=True)
class SequenceOfBerType(BerType):
    _path: str

    @property
    def path(self) -> str:
        return self._path

    @property
    def ident(self) -> str:
        return "SEQUENCE_OF_" + self.elem_tlv_ty.name

    @property
    def tag(self) -> AsnTag:
        return AsnTag(form=AsnTagForm.CONSTRUCTED, num=AsnTagNum.SEQUENCE)

    elem_tlv_ty: model.Type

    @lru_cache
    def v_ty(self, skip_proof: bool = False) -> model.Type:
        # A `SEQUENCE OF` is mapped directly to `sequence of`.
        return model.Sequence(
            strid(list(filter(None, [self.path, "Asn_Raw_" + self.ident]))),
            self.elem_tlv_ty,
        )


@dataclass(frozen=True)
class ChoiceBerType(BerType):
    _path: str

    @property
    def path(self) -> str:
        return self._path

    _ident: str

    @property
    def ident(self) -> str:
        return self._ident

    @property
    def tag(self) -> AsnTag:
        raise NotImplementedError

    variants: Mapping[str, BerType]

    @lru_cache(1)
    def v_ty(self, skip_proof: bool = False) -> model.Type:
        variants: Dict[str, Tuple[AsnTag, model.Type]] = {}

        def populate_variants(tag: str, asn_type: BerType, prefix: str = "") -> None:
            pf = f"{prefix}_{tag}" if prefix else tag
            if isinstance(asn_type, ChoiceBerType):
                # Workaround for nested choices: expose the variants
                # of inner choices to the outer choice.
                for f1, t1 in asn_type.variants.items():
                    populate_variants(f1, t1, prefix=pf)
            else:
                variants[pf] = (
                    asn_type.tag,
                    asn_type.lv_ty(skip_proof=skip_proof),
                )

        try:
            for tag, asn_type in self.variants.items():
                populate_variants(tag, asn_type)
            # A `CHOICE` is mapped to a tagged union message:
            # different tags expose different underlying values.
            return tagged_union_message(strid(self.full_ident), variants, skip_proof=skip_proof)
        except NotImplementedError as e:
            raise ValueError("cannot construct CHOICE from untagged or invalid BerType") from e


@dataclass(frozen=True)
class ImplicitlyTaggedBerType(BerType):
    """
    A simple wrapper over another `BerType`.

    This wrapper keeps the same V and LV encoding `(v_ty, lv_ty)`,
    but the tag is replaced in the TLV encoding `tlv_ty`.
    """

    base: BerType

    _tag: AsnTag

    @property
    def tag(self) -> AsnTag:
        return self._tag

    _path: str

    @property
    def path(self) -> str:
        if self.tag.class_ == AsnTagClass.UNIVERSAL:
            return PRELUDE_NAME
        return self._path

    @property
    def ident(self) -> str:
        if self.tag.class_ == AsnTagClass.UNIVERSAL:
            return self.base.ident
        if self.tag.class_ == AsnTagClass.APPLICATION:
            prefix = "Appl"
        elif self.tag.class_ == AsnTagClass.CONTEXT_SPECIFIC:
            prefix = "Ctxt"
        elif self.tag.class_ == AsnTagClass.PRIVATE:
            prefix = "Priv"
        return f"{prefix}{self.tag.num:02}_{self.base.ident}"

    def v_ty(self, skip_proof: bool = False) -> model.Type:  # type: ignore [override]
        return self.base.v_ty(skip_proof=skip_proof)

    def lv_ty(self, skip_proof: bool = False) -> model.Type:  # type: ignore [override]
        return self.base.lv_ty(skip_proof=skip_proof)


def simple_message(
    ident: str, fields: Dict[str, model.Type], skip_proof: bool = False
) -> model.Message:
    """
    Return simple RecordFlux message.

    Input is a mapping from field names to their respective types.
    """
    fields_ = {Field(f): t for f, t in fields.items()}
    links = [
        Link(source, target)
        for source, target in windowed([INITIAL, *fields_.keys(), FINAL], 2)
        if source and target
    ]

    try:
        res = model.UnprovenMessage(ident, links, fields_)
        return res.merged().proven(skip_proof=skip_proof)
    except RecordFluxError as e:
        fail(f'invalid message "{ident}": {e}', subsystem=Subsystem.CONVERTER)


def tagged_union_message(
    ident: str, variants: Dict[str, Tuple[AsnTag, model.Type]], skip_proof: bool = False
) -> model.Message:
    """
    Return RecordFlux message.

    The results reprents a tagged union. Input is a mapping from field names to a tuple containing
    the tag and the type for each variant.
    """
    fields = {Field("Tag"): ASN_TAG_TY}
    fields.update({Field(f): t for f, (_, t) in variants.items()})
    matches = {Field(f): t.matches("Tag") for f, (t, _) in variants.items()}
    links = [
        Link(INITIAL, Field("Tag")),
        *flatten([Link(Field("Tag"), f, condition=m), Link(f, FINAL)] for f, m in matches.items()),
    ]

    try:
        result = model.UnprovenMessage(ident, links, fields).merged().proven(skip_proof=skip_proof)
    except RecordFluxError as e:
        fail(f'invalid message "{ident}": {e}', subsystem=Subsystem.CONVERTER)
    return result


HELPER_TYPES = [
    ASN_TAG_CLASS_TY := model.RangeInteger(
        strid([PRELUDE_NAME, "Asn_Tag_Class"]),
        first=Number(0b00),
        last=Number(0b11),
        size=Number(2),
    ),
    ASN_TAG_FORM_TY := model.RangeInteger(
        strid([PRELUDE_NAME, "Asn_Tag_Form"]),
        first=Number(0b0),
        last=Number(0b1),
        size=Number(1),
    ),
    ASN_TAG_NUM_TY := model.RangeInteger(
        strid([PRELUDE_NAME, "Asn_Tag_Num"]),
        first=Number(0b00000),
        last=Number(0b11111),
        size=Number(5),
    ),
    ASN_TAG_TY := AsnTag.rflx_type(),
    ASN_LENGTH_TY := model.RangeInteger(
        strid([PRELUDE_NAME, "Asn_Length"]),
        first=Number(0x00),
        last=Number(0x7F),
        size=Number(8),
    ),
    ASN_RAW_BOOLEAN_TY := model.Enumeration(
        strid([PRELUDE_NAME, "Asn_Raw_BOOLEAN"]),
        literals=[(i.name, Number(i.value)) for i in AsnRawBoolean],
        size=Number(8),
        always_valid=False,
    ),
    ASN_RAW_NULL_TY := model.Message(
        strid([PRELUDE_NAME, "Asn_Raw_NULL"]),
        structure=[],
        types={},
        skip_proof=True,
    ),
]

BER_TYPES = [
    # To avoid colliding with a keyword an `_` is needed at the end of `BOOLEAN` and `NULL`.
    BOOLEAN := DefiniteBerType(
        PRELUDE_NAME, "BOOLEAN_", AsnTag(num=AsnTagNum.BOOLEAN), ASN_RAW_BOOLEAN_TY
    ),
    NULL := DefiniteBerType(PRELUDE_NAME, "NULL_", AsnTag(num=AsnTagNum.NULL), ASN_RAW_NULL_TY),
    INTEGER := SimpleBerType(PRELUDE_NAME, "INTEGER", AsnTag(num=AsnTagNum.INTEGER)),
    OBJECT_IDENTIFIER := SimpleBerType(
        PRELUDE_NAME, "OBJECT_IDENTIFIER", AsnTag(num=AsnTagNum.OBJECT_IDENTIFIER)
    ),
    BIT_STRING := SimpleBerType(PRELUDE_NAME, "BIT_STRING", AsnTag(num=AsnTagNum.BIT_STRING)),
    OCTET_STRING := SimpleBerType(PRELUDE_NAME, "OCTET_STRING", AsnTag(num=AsnTagNum.OCTET_STRING)),
    PrintableString := SimpleBerType(
        PRELUDE_NAME, "PrintableString", AsnTag(num=AsnTagNum.PRINTABLE_STRING)
    ),
    IA5String := SimpleBerType(PRELUDE_NAME, "IA5String", AsnTag(num=AsnTagNum.IA5_STRING)),
]


@lru_cache
def prelude_model(skip_proof: bool = False) -> model.Model:
    """Return base prelude without any structured types."""
    return model.Model(types=HELPER_TYPES + [ty.tlv_ty(skip_proof=skip_proof) for ty in BER_TYPES])


@dataclass
class AsnTypeConverter:
    """A converter from `asn1tools`' BER types to RecordFlux types."""

    base_path: str = ""
    """
    Common base path of conversion.
    All the ASN.1 types converted by this converter will be under this path.
    """

    skip_proof: bool = True
    """
    Whether RecordFlux proofs executed before code generation should be skipped.

    In most cases, `True` is what you want, since when generated code gets compiled by
    RecordFlux, those proofs will be executed again.
    """

    def path(self, relpath: str) -> str:
        """Return the absolute path of `relpath` relative to `self.base_path`."""
        return strid(list(filter(None, [self.base_path, relpath])))

    # In Python 3.10+ this should be done with the `match-case` construct...
    @singledispatchmethod
    def convert(self, val: ber.Type, relpath: str = "") -> BerType:
        """Convert an ASN.1 type to `BerType` under the given `self.base_path`."""
        raise NotImplementedError(f"conversion not implemented for {val}")

    def __convert_implicit(self, base: BerType, tag_src: ber.Type, relpath: str = "") -> BerType:
        """Convert a `BerType` to implicitly tagged if its tag is not UNIVERSAL."""
        if not tag_src.tag_len:
            return base
        if tag_src.tag_len > 1:
            raise AsnTag.LONG_TAG_UNSUPPORTED_ERROR
        tag = AsnTag.from_bytearray(tag_src.tag)
        if tag == base.tag:
            return base
        return base.implicitly_tagged(tag, self.path(relpath))

    # ASN.1 Types

    # The following types can be directly converted to their `BerType` counterparts.
    # The only thing that we should care about is whether they are implicitly tagged.
    # If they are, then we should generate an `ImplicitlyTaggedBerType` instead.

    @convert.register
    def _(self, val: ber.Boolean, relpath: str = "") -> BerType:
        return self.__convert_implicit(BOOLEAN, val, relpath)

    @convert.register
    def _(self, val: ber.Null, relpath: str = "") -> BerType:
        return self.__convert_implicit(NULL, val, relpath)

    @convert.register
    def _(self, val: ber.Integer, relpath: str = "") -> BerType:
        return self.__convert_implicit(INTEGER, val, relpath)

    @convert.register
    def _(self, val: ber.ObjectIdentifier, relpath: str = "") -> BerType:
        return self.__convert_implicit(OBJECT_IDENTIFIER, val, relpath)

    @convert.register
    def _(self, val: ber.BitString, relpath: str = "") -> BerType:
        return self.__convert_implicit(BIT_STRING, val, relpath)

    @convert.register
    def _(self, val: ber.OctetString, relpath: str = "") -> BerType:
        return self.__convert_implicit(OCTET_STRING, val, relpath)

    @convert.register
    def _(self, val: ber.PrintableString, relpath: str = "") -> BerType:
        return self.__convert_implicit(PrintableString, val, relpath)

    @convert.register
    def _(self, val: ber.IA5String, relpath: str = "") -> BerType:
        return self.__convert_implicit(IA5String, val, relpath)

    # ASN.1 type constructors

    # Each of these types is a certain composition of previous types.
    # Apart from potential conversions to `ImplicitlyTaggedBerType`s,
    # we should also recursively converting their member types accordingly.

    @convert.register
    def _(self, message: ber.Sequence, relpath: str = "") -> BerType:
        fields: List[ber.Type] = message.root_members
        res = SequenceBerType(
            self.path(relpath),
            from_asn1_name(message.name or message.type_name),
            # A `frozendict` is required here to comply with `lru_cache`.
            frozendict.FrozenOrderedDict(
                {from_asn1_name(field.name): self.convert(field, relpath) for field in fields}
            ),
        )
        return self.__convert_implicit(res, message, relpath)

    @convert.register
    def _(self, sequence: ber.SequenceOf, relpath: str = "") -> BerType:
        res = SequenceOfBerType(
            self.path(relpath),
            self.convert(sequence.element_type, relpath).tlv_ty(skip_proof=self.skip_proof),
        )
        return self.__convert_implicit(res, sequence, relpath)

    @convert.register
    def _(self, message: ber.Choice, relpath: str = "") -> BerType:
        fields: List[ber.Type] = message.members
        res = ChoiceBerType(
            self.path(relpath),
            from_asn1_name(message.name or message.type_name),
            # A `frozendict` is required here to comply with `lru_cache`.
            frozendict.FrozenOrderedDict(
                {from_asn1_name(field.name): self.convert(field, relpath) for field in fields}
            ),
        )
        return self.__convert_implicit(res, message, relpath)

    @convert.register
    def _(self, tagged: ber.ExplicitTag, relpath: str = "") -> BerType:
        tag = AsnTag.from_bytearray(tagged.tag)
        return self.convert(cast(ber.Type, tagged.inner), relpath).explicitly_tagged(
            tag, self.path(relpath)
        )

    def convert_spec(self, spec: asn1.compiler.Specification) -> Dict[ID, model.Type]:
        """
        Convert an ASN.1 specification.

        Result is a mapping from qualified RecordFlux identifiers to the corresponding RecordFlux
        type.
        """
        res: Dict[ID, model.Type] = {}
        for path, tys in spec.modules.items():
            for ty in tys.values():
                ty1 = self.convert(ty.type, from_asn1_name(path)).tlv_ty(skip_proof=self.skip_proof)
                ident = ty1.qualified_identifier
                if not str(ident).startswith(PRELUDE_NAME):
                    # Exclude `Prelude` types.
                    res[ident] = ty1
        return res
