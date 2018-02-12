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

  @classmethod
  def gen_script_load_stmts(cls, sources_rel_path):
    el = sources_rel_path[0]
    source_stmts = ["source('{}')".format(s.encode('ascii')) for s in sources_rel_path]
    return '\n'.join(source_stmts) + '\n'

  @classmethod
  def gen_libs_sources_input(cls, lib_paths, sources_rel_path):
    if len(lib_paths) == 0:
      libs_input = ''
    elif len(lib_paths) == 1:
      libs_input = ".libPaths('{}')".format(lib_paths[0]) + '\n'
    else:
      libs_quoted = ["'{}'".format(p) for p in lib_paths]
      all_libs_charvec = "c({})".format(', '.join(libs_quoted))
      libs_input = ".libPaths({})".format(all_libs_charvec) + '\n'

    sources_input = cls.gen_script_load_stmts(sources_rel_path)

    return libs_input + sources_input

  def gen_input_loading_targets(self, targets):
    rel_r_sources = []
    for tgt in targets:
      rel_r_sources.extend(tgt.sources_relative_to_buildroot())

    # el = rel_r_sources[0]
    # self.context.log.debug("el: '{}', type(el): '{}'".format(el, type(el)))
    return self.gen_libs_sources_input(self.lib_paths, rel_r_sources)
