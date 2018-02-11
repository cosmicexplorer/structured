# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.subsystem.subsystem import Subsystem
from pants.util.contextutil import temporary_dir
from pants.util.objects import datatype

from structured.util.boilerplate import R_INSTALL_PACKAGE_BOILERPLATE


class GithubDependency(datatype('GithubDependency', ['org', 'name', 'ref'])):
  def repo_addr(self):
    return '{}/{}'.format(self.org, self.name)


class Github(Subsystem):
  options_scope = 'github'

  def gen_github_install_input(self, github_dep, outdir):
    return R_INSTALL_PACKAGE_BOILERPLATE.format(
      expr="devtools::install_github('{}', ref='{}', lib='{}')".format(
        github_dep.repo_addr(), github_dep.ref, outdir),
      outdir=outdir,
    )
