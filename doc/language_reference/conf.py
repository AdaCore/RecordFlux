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
    "light_css_variables": {
        "color-brand-primary": "#00a650",
        "color-brand-content": "#00a650",
        "font-size--small--2": "87,5%",
        "code-font-size": "0.8em",
    },
    "dark_css_variables": {
        "color-brand-primary": "#00a650",
        "color-brand-content": "#00a650",
    },
    "sidebar_hide_name": True,
    "footer_icons": [
        {
            "name": "GitHub",
            "url": "https://github.com/AdaCore/RecordFlux",
            "html": """
                <svg stroke="currentColor" fill="currentColor" stroke-width="0" viewBox="0 0 16 16">
                    <path fill-rule="evenodd" d="M8 0C3.58 0 0 3.58 0 8c0 3.54 2.29 6.53 5.47 7.59.4.07.55-.17.55-.38 0-.19-.01-.82-.01-1.49-2.01.37-2.53-.49-2.69-.94-.09-.23-.48-.94-.82-1.13-.28-.15-.68-.52-.01-.53.63-.01 1.08.58 1.23.82.72 1.21 1.87.87 2.33.66.07-.52.28-.87.51-1.07-1.78-.2-3.64-.89-3.64-3.95 0-.87.31-1.59.82-2.15-.08-.2-.36-1.02.08-2.12 0 0 .67-.21 2.2.82.64-.18 1.32-.27 2-.27.68 0 1.36.09 2 .27 1.53-1.04 2.2-.82 2.2-.82.44 1.1.16 1.92.08 2.12.51.56.82 1.27.82 2.15 0 3.07-1.87 3.75-3.65 3.95.29.25.54.73.54 1.48 0 1.07-.01 1.93-.01 2.2 0 .21.15.46.55.38A8.013 8.013 0 0 0 16 8c0-4.42-3.58-8-8-8z"></path>
                </svg>
            """,  # noqa: E501
            "class": "",
        },
    ],
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
