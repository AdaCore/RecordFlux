#!/usr/bin/env python3

from __future__ import annotations

import re
import sys
from pathlib import Path


class GPRGenError(Exception):
    pass


def template(name: str, interfaces: list[str]) -> str:
    interfaces_str = ", ".join([f'"{i}"' for i in interfaces])
    return f"""
with "gnatcoll";

library project Lib{name} is
   type Library_Type_Type is ("relocatable", "static", "static-pic");
   Library_Type : Library_Type_Type := External ("LIBRARY_TYPE", "static");
   for Create_Missing_Dirs use "true";

   for Languages use ("Ada", "C");
   for Source_Dirs use ("src", "langkit/langkit/support", "gnatcoll-bindings/gmp", "gnatcoll-bindings/iconv");
   for Library_Name use "{name}";
   for Library_Kind use Library_Type;
   for Library_Dir use "python/librflxlang";
   for Interfaces use ({interfaces_str});
   for Object_Dir use "obj/dev";
   package Compiler is
      Common_Args := ("-fPIC", "-g", "-Ofast");
      for Default_Switches ("Ada") use Common_Args & "-gnatn2";
      for Default_Switches ("C") use Common_Args;
   end Compiler;
   for Library_Options use ("-lgmp");
end Lib{name};
"""


def extract(source: str) -> list[str]:
    with open(source, mode="r", encoding="utf-8") as f:
        data = f.read()
        search = re.search(r"for\s+Interfaces\s+use\s*\(([^)]*)\)\s*;", data)
        if search is None or len(search.groups()) != 1:
            raise GPRGenError(f'Project file "{source}" contains no interfaces')
        return [re.sub(r'^\s*"|"\s*$', "", r) for r in search[1].split(",")]


if len(sys.argv) != 4:
    print(f"Insufficient arguments: {sys.argv[0]} libname output-path input-gpr-file")
    sys.exit(1)


libname = sys.argv[1]
with open(Path(sys.argv[2]) / f"lib{libname}.gpr", "w", encoding="utf-8") as outfile:
    outfile.write(template(libname, extract(sys.argv[3])))
