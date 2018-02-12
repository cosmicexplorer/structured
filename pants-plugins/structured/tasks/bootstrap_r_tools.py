# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.base.exceptions import TaskError
from pants.util.memo import memoized_method, memoized_property
from pants.util.objects import datatype

from structured.subsystems.cran import CRAN, CRANDependency
from structured.subsystems.github import Github, GithubDependency
from structured.subsystems.r_distribution import RDistribution
from structured.tasks.resolve_packages_task import ResolvePackagesTask


class BootstrapError(TaskError): pass

class BootstrapRTools(ResolvePackagesTask):

  R_BOOTSTRAP_PRODUCT = 'r_bootstrap_tools_packages'

  @memoized_property
  def modules_ref(self):
    return self.r_distribution.modules_git_ref

  def bootstrap_deps(self):
    return [
      CRANDependency(name=RDistribution.DEVTOOLS_CRAN_NAME),
      GithubDependency(
        org=RDistribution.MODULES_GITHUB_ORG_NAME,
        name=RDistribution.MODULES_GITHUB_REPO_NAME,
        ref=self.modules_ref,
      )
    ]

  @memoized_property
  def tools_dir(self):
    return self.r_distribution.tools_cache_dir

  @classmethod
  def product_types(cls):
    return [cls.R_BOOTSTRAP_PRODUCT]

  def execute(self):
    installed_packages = self.resolve_dep_list(
      self.bootstrap_deps(), self.tools_dir)
    self.context.products.register_data(
      self.R_BOOTSTRAP_PRODUCT, installed_packages)
