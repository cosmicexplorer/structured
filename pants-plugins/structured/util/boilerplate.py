# coding=utf-8
# Copyright 2014 Pants project contributors (see CONTRIBUTORS.md).
# Licensed under the Apache License, Version 2.0 (see LICENSE).

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)


R_INSTALL_PACKAGE_BOILERPLATE = """
capture.output(
  {expr},
  file=stderr())
pkgs <- installed.packages(lib.loc='{outdir}')[,'Package']
cat(pkgs, sep='\\n')
"""
