"""
Configuration file for the Sphinx documentation builder.

https://www.sphinx-doc.org/en/master/usage/configuration.html
"""


import sys
from pathlib import Path

from sphinx.highlighting import lexers

from rflx import __version__
from tools.rflxlexer import RFLXLexer

lexers["rflx"] = RFLXLexer(startinline=True)

# -- Path setup --------------------------------------------------------------

sys.path.insert(0, str(Path("../..").resolve()))

# -- Project information -----------------------------------------------------

project = "RecordFlux"
copyright = "2023, AdaCore"  # noqa: A001
author = "AdaCore"
recordflux_version = __version__
version = recordflux_version
release = recordflux_version

# -- General configuration ---------------------------------------------------

# Disable special handling of e.g. --
smartquotes = False

extensions = [
    "sphinx.ext.autodoc",
    "sphinx.ext.autosectionlabel",
    "sphinx.ext.autosummary",
    "sphinx.ext.todo",
    "sphinx.ext.viewcode",
    "sphinx_rtd_theme",
]

templates_path = ["../_templates"]

# -- Options for HTML output -------------------------------------------------

html_theme = "sphinx_rtd_theme"
html_static_path = ["../_static"]
html_css_files = ["css/custom.css"]
html_logo = "../images/adacore-logo-white.png"
html_favicon = "../images/favicon.ico"
# https://sphinx-rtd-theme.readthedocs.io/en/stable/configuring.html#theme-options
html_theme_options = {
    # Use AdaCore blue in the Table Of Content
    "style_nav_header_background": "#12284c",
}
html_title = "RecordFlux"
html_baseurl = ""

# -- Options for PDF output --------------------------------------------------

latex_documents = [("index", "user_guide.tex", "RecordFlux User's Guide", author, "howto", True)]

# -- Extension configuration -------------------------------------------------

autosummary_generate = True
always_document_param_types = True
autodoc_typehints = "description"
