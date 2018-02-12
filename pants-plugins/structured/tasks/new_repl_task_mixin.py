# coding=utf-8
# Copyright 2015 Pants project contributors (see CONTRIBUTORS.md).
# Licensed under the Apache License, Version 2.0 (see LICENSE).

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from abc import abstractmethod

from pants.base.exceptions import TaskError
from pants.base.workunit import WorkUnitLabel
from pants.console.stty_utils import STTYSettings
from pants.task.repl_task_mixin import ReplTaskMixin


class NewReplTaskError(TaskError): pass


class NewReplTaskMixin(ReplTaskMixin):
  def launch_repl(self, session_setup):
    raise NewReplTaskError('subclasses of NewReplTaskMixin '
                           'should override launch_repl_with_workunit(), '
                           'not launch_repl()')

  @abstractmethod
  def launch_repl_with_workunit(self, session_setup, workunit):
    """???"""

  def execute_for(self, targets):
    session_setup = self.setup_repl_session(targets)
    self.context.release_lock()
    with STTYSettings.preserved():
      with self.context.new_workunit(
          name='repl', labels=[WorkUnitLabel.REPL]) as workunit:
        print('')  # Start REPL output on a new line.
        try:
          return self.launch_repl_with_workunit(session_setup, workunit)
        except KeyboardInterrupt:
          # This is a valid way to end a REPL session in general, so just break out of execute and
          # continue.
          pass
