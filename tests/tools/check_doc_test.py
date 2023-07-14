import pytest

from rflx.common import STDIN
from tests.const import DATA_DIR
from tools.check_doc import CheckDocError, check_file, check_files


def test_invalid_no_code_blocks() -> None:
    with pytest.raises(
        CheckDocError,
        match=(
            r"^"
            r"No code blocks found [(]checked "
            f"{DATA_DIR}/no_code_blocks_1.rst, {DATA_DIR}/no_code_blocks_2.rst"
            r"[)]$"
        ),
    ):
        check_files([DATA_DIR / "no_code_blocks_1.rst", DATA_DIR / "no_code_blocks_2.rst"])


def test_invalid_missing_empty_line() -> None:
    with pytest.raises(
        CheckDocError,
        match=r"^<stdin>:4: missing empty line in code block$",
    ):
        check_file(
            STDIN,
            """
.. doc-check: ada
.. code:: ada
   null;
""",
        )


def test_invalid_use_of_code_block() -> None:
    with pytest.raises(
        CheckDocError,
        match=r"^<stdin>:2: code-block directive forbidden [(]use 'code::' instead[)]$",
    ):
        check_file(
            STDIN,
            """
.. code-block:: python

    print("")
""",
        )


def test_invalid_inconsistent_code_block() -> None:
    with pytest.raises(
        CheckDocError,
        match=r"^<stdin>:3: inconsistent code block type [(]block: Ada, doc: Python[)]$",
    ):
        check_file(
            STDIN,
            """
.. doc-check: python
.. code:: ada

   null;
""",
        )


def test_invalid_unknown_doc_check_1() -> None:
    with pytest.raises(
        CheckDocError,
        match=r'^<stdin>:2: invalid doc-check type "invalid"$',
    ):
        check_file(
            STDIN,
            """
.. doc-check: invalid
.. code:: ada

   null;
""",
        )


def test_invalid_unknown_doc_check_2() -> None:
    with pytest.raises(
        CheckDocError,
        match=r'^<stdin>:2: invalid doc-check type "invalid"$',
    ):
        check_file(
            STDIN,
            """
.. doc-check: invalid
.. code:: invalid

   null;
""",
        )


def test_invalid_ada_code() -> None:
    with pytest.raises(
        CheckDocError,
        match=(
            r"^<stdin>:3: error in code block\n"
            r"main.adb:1:01: (error: )?compilation unit expected\n"
            r"(gprbuild: [*][*][*] compilation phase failed\n)?$"
        ),
    ):
        check_file(
            STDIN,
            """
.. code:: ada

    invalid
""",
        )


def test_invalid_ada_api_style() -> None:
    with pytest.raises(
        CheckDocError,
        match=(
            r"^<stdin>:5: error in code block\n"
            r"main.adb:2:25: \(style\) space required\n"
            r"(gprbuild: [*][*][*] compilation phase failed\n)?$"
        ),
    ):
        check_file(
            STDIN,
            """\

.. doc-check: ada,api
.. code:: ada
    :number-lines:

    function Get_Tag (Ctx:Context) return RFLX.TLV.Tag_Type;

Some more text...
""",
        )


def test_invalid_rflx_spec() -> None:
    with pytest.raises(
        CheckDocError,
        match=(
            r"^<stdin>:6: error in code block\n"
            r"<stdin>:2:32: parser: error: Expected 'with', got ';'$"
        ),
    ):
        check_file(
            STDIN,
            """
Enumeration with missing size:

.. doc-check: rflx
.. code:: ada

    package Protocol is
        type Values is (E1, E2, E3);
    end Protocol;

Some more text...
""",
        )


def test_invalid_python_source() -> None:
    with pytest.raises(
        CheckDocError,
        match=(
            r"^<stdin>:5: error in code block\n"
            r"Traceback [(]most recent call last[)]:\n"
            r'  File "(.*/)?test.py", line 1, in <module>\n'
            r"    invalid\n"
            r"NameError: name \'invalid\' is not defined\n$"
        ),
    ):
        check_file(
            STDIN,
            """
Invalid Python code:

.. code:: python

    invalid

Some more text...
""",
        )


def test_invalid_rflx_rule() -> None:
    with pytest.raises(
        CheckDocError,
        match=(
            r"^<stdin>:6: error in code block\n"
            "<stdin>:1:4: parser: error: Cannot parse <factor>\n"
            r"<stdin>:1:8: parser: error: Expected '[(]', got '[*]'$"
        ),
    ):
        check_file(
            STDIN,
            """
An invalid RecordFlux expression:

.. doc-check: rflx,expression
.. code:: ada

   2 *** 32 - 1

Some more text...
""",
        )


def test_invalid_rflx_rule_style() -> None:
    with pytest.raises(
        CheckDocError,
        match=(
            r"^<stdin>:6: error in code block\n"
            r'<stdin>:1:8: style: error: missing space after "\*\*" \[token-spacing\]$'
        ),
    ):
        check_file(
            STDIN,
            """
A RecordFlux expression with invalid style:

.. doc-check: rflx,expression
.. code:: ada

    2 **32 - 1

Some more text...
""",
        )


def test_invalid_rflx_spec_style() -> None:
    with pytest.raises(
        CheckDocError,
        match=(
            r"^<stdin>:6: error in code block\n"
            r"<stdin>:3:4: style: error: unexpected keyword indentation \(expected 3 or 6\) "
            r"\[indentation\]$"
        ),
    ):
        check_file(
            STDIN,
            """
A RecordFlux specification looks as follows:

.. doc-check: rflx
.. code:: ada

    package Protocol is
       -- Invalid indentation
        type Len is range 0 .. 2 ** 8 - 1 with Size => 8;
    end Protocol;

Some more text...
""",
        )


def test_invalid_yaml_file() -> None:
    with pytest.raises(
        CheckDocError,
        match=r"^<stdin>:5: error in code block\nwhile parsing a block node\n.*\n.*$",
    ):
        check_file(
            STDIN,
            """
Invalid YAML code:

.. code:: yaml

    ] invalid

Some more text...
""",
        )


def test_valid_no_code_blocks() -> None:
    check_files([DATA_DIR / "no_code_blocks_1.rst", DATA_DIR / "with_code_blocks.rst"])


def test_valid_ada_procedure() -> None:
    check_file(
        STDIN,
        """
.. code:: ada

    procedure Main is
    begin
        null;
    end Main;
""",
    )


def test_valid_ada_declaration() -> None:
    check_file(
        STDIN,
        """
An Ada declaration looks as follows:

.. doc-check: ada,declaration
.. code:: ada
    :number-lines:

    I : Integer;

Some more text...
""",
    )


def test_valid_ada_api() -> None:
    check_file(
        STDIN,
        """\
An Ada API looks as follows:

.. doc-check: ada,api
.. code:: ada
    :number-lines:

    function Get_Tag    (Ctx : Context) return RFLX.TLV.Tag_Type;
    function Get_Length (Ctx : Context) return RFLX.TLV.Length_Type;
    function Get_Value  (Ctx : Context) return RFLX_Types.Bytes;
    procedure Get_Value (Ctx : Context; Data : out RFLX_Types.Bytes);
    generic
       with procedure Process_Value (Value : RFLX_Types.Bytes);
    procedure Generic_Get_Value (Ctx : Context);

Some more text...
""",
    )


def test_valid_rflx_spec() -> None:
    check_file(
        STDIN,
        """
A RecordFlux specification looks as follows:

.. doc-check: rflx
.. code:: ada

    package Protocol is
       type Len is range 0 .. 2 ** 8 - 1 with Size => 8;
       type Values is (E1, E2, E3) with Size => 16;
    end Protocol;

Some more text...
""",
    )


def test_valid_rflx_rule() -> None:
    check_file(
        STDIN,
        """
A RecordFlux expression looks as follows:

.. doc-check: rflx,expression
.. code:: ada

    2 ** 32 - 1

Some more text...
""",
    )


def test_valid_python_source() -> None:
    check_file(
        STDIN,
        """
A Python program looks as follows:

.. code:: python

   for i in range(1, 10):
       print(i)

Some more text...
""",
    )


def test_valid_yaml_file() -> None:
    check_file(
        STDIN,
        """
A YAML file looks as follows:

.. code:: yaml

   Root:
      Child1: 10
      Child2: 20

Some more text...
""",
    )


def test_valid_python_ignore() -> None:
    check_file(
        STDIN,
        """
A Python program looks as follows:

.. doc-check: ignore
.. code:: python

   invalid

Some more text...
""",
    )


def test_invalid_double_space() -> None:
    with pytest.raises(
        CheckDocError,
        match=r"^<stdin>:2: multiple consecutive whitespace$",
    ):
        check_file(
            STDIN,
            """
This is a  sentence with multiple consecutive spaces.

Some more text.
""",
        )


def test_invalid_heading_marker() -> None:
    with pytest.raises(
        CheckDocError,
        match=r"^<stdin>:2: heading marker length does not match heading length$",
    ):
        check_file(
            STDIN,
            """
Too short heading
====================

Some more text.
""",
        )


def test_invalid_missing_punctuation() -> None:
    with pytest.raises(
        CheckDocError,
        match=r"^<stdin>:2: no trailing punctuation$",
    ):
        check_file(
            STDIN,
            """
This is not a heading, but lacks punctuation

""",
        )


def test_invalid_trailing_whitespace() -> None:
    with pytest.raises(
        CheckDocError,
        match=r"^<stdin>:1: trailing whitespace$",
    ):
        check_file(
            STDIN,
            "Sentence with punctuation, but trailing whitespace. ",
        )


def test_invalid_multiple_sentences() -> None:
    with pytest.raises(
        CheckDocError,
        match=r"^<stdin>:2: multiple sentences on one line$",
    ):
        check_file(
            STDIN,
            """
The first sentence. The second sentence should be on a new line.
""",
        )


def test_invalid_single_line() -> None:
    with pytest.raises(
        CheckDocError,
        match=r"^<stdin>:1: no trailing punctuation$",
    ):
        check_file(
            STDIN,
            "Detecting this error error requires proper handling of the last line",
        )


def test_valid_special_elements() -> None:
    check_file(
        STDIN,
        """

Template elements should be accepted without punctuation:

{{ item }}

URLs should be accepted without punctuation:

http://example.com/example.html

Some more text.
""",
    )


def test_valid_heading() -> None:
    check_file(
        STDIN,
        """
Headings don't require punctuation
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Some more text.
""",
    )


def test_valid_empty_document() -> None:
    check_file(
        STDIN,
        "",
    )


def test_valid_title() -> None:
    check_file(
        STDIN,
        """
==============
Document title
==============

Some more text.
""",
    )
