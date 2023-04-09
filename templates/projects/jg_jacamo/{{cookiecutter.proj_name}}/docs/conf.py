# Configuration file for the Sphinx documentation builder.
#
# This file only contains a selection of the most common options. For a full
# list see the documentation:
# https://www.sphinx-doc.org/en/master/usage/configuration.html

# -- Path setup --------------------------------------------------------------

# If extensions (or modules to document with autodoc) are in another directory,
# add these directories to sys.path here. If the directory is relative to the
# documentation root, use os.path.abspath to make it absolute, like shown here.
#
import os
import sys
sys.path.insert(0, os.path.abspath('../{{cookiecutter.proj_name}}'))

import warnings
import tomler

data = tomler.load("../pyproject.toml")

with warnings.catch_warnings():
    warnings.simplefilter("ignore")
    import {{cookiecutter.proj_name}}

# -- Project information -----------------------------------------------------

project   = data.on_fail("Unknown Project Name").project.name()
copyright = data.on_fail("{% now 'utc' %}").tool.sphinx.copyright()
author    = data.on_fail("{{cookiecutter._author}}").tool.sphinx.author()

# -- General configuration ---------------------------------------------------

# Add any Sphinx extension module names here, as strings. They can be
# extensions coming with Sphinx (named 'sphinx.ext.*') or your custom
# ones.
extensions = data.on_fail([], list).tool.sphinx.extensions()

# Add any paths that contain templates here, relative to this directory.
templates_path = data.on_fail([], list).tool.sphinx.templates()

# List of patterns, relative to source directory, that match files and
# directories to ignore when looking for source files.
# This pattern also affects html_static_path and html_extra_path.
exclude_patterns = data.on_fail([], list).tool.sphinx.exclude()

if 'sphinx.ext.autosummary' in extensions:
    autosummary_generate = data.on_fail(false, bool).tool.sphinx.autosummary.generate()

if 'sphinx.ext.autodoc' in extensions:
    autodoc_default_options    = data.on_fail({}, dict).tool.sphinx.autodoc.defaults(wrapper=dict)
    add_module_names           = data.on_fail(false, bool).tool.sphinx.autodoc.add_module_names()
    autodoc_inherit_docstrings = data.on_fail(false, bool).tool.sphinx.autodoc.inherit_docstrings()

# -- Options for HTML output -------------------------------------------------

# The theme to use for HTML and HTML Help pages.  See the documentation for
# a list of builtin themes.
#
html_theme = data.on_fail("alabaster", str).tool.sphinx.html.theme()
# Add any paths that contain custom static files (such as style sheets) here,
# relative to this directory. They are copied after the builtin static files,
# so a file named "default.css" will overwrite the builtin "default.css".
html_static_path = data.on_fail([], list).tool.sphinx.html.static()
html_theme_options = data.on_fail({}, dict).tool.sphinx.html.options(wrapper=dict)
