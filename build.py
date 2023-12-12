from __future__ import annotations

import os
import shutil
import subprocess

from wheel.bdist_wheel import bdist_wheel


class BuildParser(bdist_wheel):  # type: ignore[misc]
    def run(self) -> None:
        env = os.environ
        if "GPR_PROJECT_PATH" in env:
            env["GPR_PROJECT_PATH"] += ":"
        else:
            env["GPR_PROJECT_PATH"] = ""
        env["GPR_PROJECT_PATH"] += ":".join(  # noqa: FLY002
            [
                "generated/langkit/langkit/support",
                "generated/gnatcoll-bindings/gmp",
                "generated/gnatcoll-bindings/iconv",
                "generated/adasat",
            ],
        )
        if "GNATCOLL_ICONV_OPT" not in env:
            env["GNATCOLL_ICONV_OPT"] = "-v"
        subprocess.run(
            [
                "gprbuild",
                "-p",
                "-j0",
                "-Pgenerated/librflxlang.gpr",
                "-XLIBRARY_TYPE=static-pic",
                "-XLIBRFLXLANG_LIBRARY_TYPE=relocatable",
                "-XLIBRFLXLANG_STANDALONE=encapsulated",
            ],
            env=env,
            check=True,
        )
        shutil.copy(
            "generated/lib/relocatable/dev/librflxlang.so",
            "rflx/lang/librflxlang.so",
        )
        super().run()


def build(setup_kwargs: dict[object, object]) -> None:
    setup_kwargs.update(
        cmdclass={"bdist_wheel": BuildParser},
    )
