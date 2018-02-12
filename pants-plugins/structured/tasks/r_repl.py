# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from structured.targets.r_library import RLibrary
from structured.tasks.new_repl_task_mixin import NewReplTaskMixin
from structured.tasks.r_execution_task import RExecutionTask


# TODO: add support for connecting to jupyter ASAP!! -- see python_repl.py
class RRepl(NewReplTaskMixin, RExecutionTask):

  @classmethod
  def select_targets(cls, target):
    return isinstance(target, RLibrary)

  def setup_repl_session(self, targets):
    return self.gen_libs_sources_input(targets)

  def launch_repl_with_workunit(self, repl_init_input, workunit):
    return self.r_distribution.invoke_r_interactive(
      self.context,
      workunit,
      repl_init_input,
      self.r_distribution.chroot_cache_dir)
