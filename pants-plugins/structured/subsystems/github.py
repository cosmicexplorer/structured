# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.subsystem.subsystem import Subsystem
from pants.util.contextutil import temporary_dir
from pants.util.objects import datatype

from structured.subsystems.r_distribution import RDependency
from structured.util.boilerplate import R_INSTALL_PACKAGE_BOILERPLATE


class GithubDependency(datatype('GithubDependency', ['org', 'name', 'ref']),
                       RDependency):
  def repo_addr(self):
    return '{}/{}'.format(self.org, self.name)


class Github(Subsystem):
  options_scope = 'github'

  GITHUB_INSTALL_PACKAGE_BOILERPLATE = """
.libPaths('{tool_lib_dir}')
library(devtools)
.libPaths('{out_lib_dir}')
install_github('{repo_addr}', ref='{ref}', lib='{out_lib_dir}')
pkgs <- installed.packages(lib.loc='{out_lib_dir}')[,'Package']
cat(pkgs, sep='\\n')
"""

  def gen_github_install_input(self, tool_dir, github_dep, outdir):
    return self.GITHUB_INSTALL_PACKAGE_BOILERPLATE.format(
      tool_lib_dir=tool_dir,
      out_lib_dir=outdir,
      repo_addr=github_dep.repo_addr(),
      ref=github_dep.ref)
