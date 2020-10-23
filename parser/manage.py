#! /usr/bin/env python3

import os

from langkit.libmanage import ManageScript
from langkit.compile_context import CompileCtx

from language.lexer import rflx_lexer as lexer
from language.parser import rflx_grammar as grammar


class Manage(ManageScript):
    def create_context(self, args):
        return CompileCtx(lang_name='RecordFluxDSL',
                          lexer=lexer,
                          grammar=grammar)

if __name__ == '__main__':
    Manage().run()
