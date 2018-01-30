# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

import os

from pants.engine.isolated_process import ExecuteProcessRequest, ExecuteProcessResult

from structured.targets.r_binary import RBinary
from structured.tasks.r_task import RTask

class RRun(RTask):
  """Run an R script."""

  @classmethod
  def register_options(cls, register):
    super(RRun, cls).register_options(register)
    register('--args', type=list, help='Run with these command-line arguments.')

  @classmethod
  def supports_passthru_args(cls):
    return True

  def execute(self):
    binary_target = self.require_single_root_target()
    if not isinstance(binary_target, RBinary):
      return

    argv = self.get_options().args + self.get_passthru_args()
    r_cmd = [
      self.r_binary(),
      binary_target.script,
    ] + argv
    env_path = ['PATH', os.environ.get('PATH')]
    req = ExecuteProcessRequest(r_cmd, env_path)
    execute_process_result, = self.context._scheduler.product_request(
      ExecuteProcessResult, [req])
    exit_code = execute_process_result.exit_code
    if exit_code != 0:
      raise TaskError(
        '{} ... exited non-zero ({}).'.format(' '.join(cmd), exit_code),
        exit_code=exit_code)
