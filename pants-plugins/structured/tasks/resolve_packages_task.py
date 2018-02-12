# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.base.exceptions import TaskError
from pants.util.dirutil import safe_mkdir
from pants.util.memo import memoized_property

from structured.subsystems.cran import CRAN, CRANDependency
from structured.subsystems.github import Github, GithubDependency
from structured.tasks.r_task import RTask


class ResolvePackagesTask(RTask):

  class ResolveError(TaskError):
    """???"""

  @classmethod
  def subsystem_dependencies(cls):
    return super(ResolvePackagesTask, cls).subsystem_dependencies() + (
      CRAN.scoped(cls),
      Github.scoped(cls),
    )

  @memoized_property
  def cran(self):
    return CRAN.scoped_instance(self)

  @memoized_property
  def github(self):
    return Github.scoped_instance(self)

  def resolve_dep(self, dep, outdir):
    if isinstance(dep, CRANDependency):
      installed_pkgs = self.r_distribution.install_cran_package(
        self.cran, self.context, dep, outdir)
    elif isinstance(dep, GithubDependency):
      installed_pkgs = self.r_distribution.install_github_package(
        self.github, self.context, dep, outdir)
    else:
      raise ResolveError("could not identify type of R dependency: '{}'"
                         .format(repr(dep)))
    return installed_pkgs

  def resolve_dep_list(self, r_deps, outdir):
    safe_mkdir(outdir)
    cur_installed_packages = self.r_distribution.get_installed_packages(
      self.context, outdir)
    self.context.log.debug("cur_installed_packages: '{}'".format(cur_installed_packages))
    for dep in r_deps:
      pkg_name = dep.name
      if pkg_name in cur_installed_packages:
        self.context.log.debug("continuing after '{}'".format(pkg_name))
        continue
        # TODO: figure out what to do here!
        # raise self.ResolveError("package '{}' is already installed in '{}'!"
        #                    .format(pkg_name, outdir))

      self.resolve_dep(dep, outdir)
      cur_installed_packages = self.r_distribution.get_installed_packages(
        self.context, outdir)
      self.context.log.debug(
        "resolved dep '{}' in '{}'. cur_installed_packages: '{}'".format(
          pkg_name, outdir, cur_installed_packages))
    return cur_installed_packages
