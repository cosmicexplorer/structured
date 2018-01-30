# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.task.repl_task_mixin import ReplTaskMixin

from structured.tasks.r_task import RTask


class RRepl(ReplTaskMixin, RTask):
  pass
