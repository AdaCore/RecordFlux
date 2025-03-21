"""
Configuration file for the Sphinx documentation builder.

https://www.sphinx-doc.org/en/master/usage/configuration.html
"""

import sys
from datetime import datetime
from pathlib import Path

from sphinx.highlighting import lexers

from tools.rflxlexer import RFLXLexer

lexers["rflx"] = RFLXLexer(startinline=True)

# -- Path setup --------------------------------------------------------------

sys.path.insert(0, str(Path("../..").resolve()))

# -- Project information -----------------------------------------------------

project = "RecordFlux"
copyright = f"2023-{datetime.now().year}, AdaCore"  # noqa: A001, DTZ005
author = "AdaCore"

# Omit version to prevent conflicts caused by dynamic versioning
version = " "
release = " "

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

latex_documents = [
    ("index", "language_reference.tex", "RecordFlux Language Reference", author, "howto", True),
]

# -- Extension configuration -------------------------------------------------

autosummary_generate = True
always_document_param_types = True
autodoc_typehints = "description"
