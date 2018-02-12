# coding=utf-8
# Copyright 2017 Pants project contributors (see CONTRIBUTORS.md).
# Licensed under the Apache License, Version 2.0 (see LICENSE).

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from structured.tasks.bootstrap_r_tools import BootstrapRTools
from structured.tasks.resolve_packages_task import ResolvePackagesTask


class RExecutionTask(ResolvePackagesTask):

  @classmethod
  def prepare(cls, options, round_manager):
    super(ResolvePackagesTask, cls).prepare(options, round_manager)
    round_manager.require_data(BootstrapRTools.R_BOOTSTRAP_TOOLS_DIR)

  def lib_paths(self):
    return [
      self.r_distribution.resolver_cache_dir,
      self.context.products.get_data(BootstrapRTools.R_BOOTSTRAP_TOOLS_DIR),
    ]

  def gen_libs_sources_input(self, targets):
    rel_r_sources = []
    for tgt in targets:
      rel_r_sources.extend(tgt.sources_relative_to_buildroot())

    libs_input = self.r_distribution.gen_libs_input(self.lib_paths())
    sources_input = self.r_distribution.gen_script_load_stmts(rel_r_sources)

    return libs_input + sources_input
