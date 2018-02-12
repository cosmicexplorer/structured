# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.subsystem.subsystem import Subsystem
from pants.util.objects import datatype

from structured.subsystems.r_distribution import RDependency, RDistribution


class CRANDependency(datatype('CRANDependency', ['name', 'version']),
                     RDependency):
  """???"""

  def __new__(cls, name, version=None):
    return super(CRANDependency, cls).__new__(
      cls,
      name=RDistribution.check_valid_package_name(name),
      version=RDistribution.check_valid_version(version),
    )


class CRAN(Subsystem):
  options_scope = 'cran'

  CRAN_DEFAULT_REPOS = {
    'CRAN': 'https://mirrors.nics.utk.edu/cran/',
  }

  @classmethod
  def register_options(cls, register):
    super(CRAN, cls).register_options(register)
    register('--repos', fingerprint=True, advanced=True,
             metavar='<map>', type=dict,
             help='CRAN repositories to pull packages from.',
             default=cls.CRAN_DEFAULT_REPOS)

  CRAN_INSTALL_BOILERPLATE = """
options(repos=c({named_repos}))
.libPaths('{outdir}')
install.packages('{package_name}', lib='{outdir}')
pkgs <- installed.packages(lib.loc='{outdir}')[,'Package']
cat(pkgs, sep='\\n')
"""

  def gen_cran_install_input(self, cran_dep, outdir):
    package_name = cran_dep.name
    repos_dict = self.get_options().repos
    repos_named = [
      "'{}'='{}'".format(name, url) for name, url in repos_dict.iteritems()
    ]
    return self.CRAN_INSTALL_BOILERPLATE.format(
      named_repos=', '.join(repos_named),
      package_name=package_name,
      outdir=outdir,
    )
