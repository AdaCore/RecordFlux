#!/usr/bin/env python3

from cobrafuzz.main import CobraFuzz


@CobraFuzz
def fuzz(buf: bytes) -> None:
    import sys

    from rflx import error
    from rflx.model import Cache, Digest
    from rflx.specification import parser

    class SilentlyNeverVerify(Cache):
        def __init__(self) -> None:
            pass

        def is_verified(self, _: Digest) -> bool:
            return True

        def add_verified(self, _digest: Digest) -> None:
            pass

    try:
        string = buf.decode("utf-8")
        p = parser.Parser(cache=SilentlyNeverVerify())
        p.parse_string(string)
        p.create_model()
    except (UnicodeDecodeError, error.RecordFluxError):
        pass
    except KeyboardInterrupt:  # pragma: no cover
        sys.exit()


if __name__ == "__main__":
    fuzz()  # pragma: no cover
