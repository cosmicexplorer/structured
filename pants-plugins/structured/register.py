# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.build_graph.build_file_aliases import BuildFileAliases
from pants.goal.task_registrar import TaskRegistrar as task

# from structured.subsystems.r_distribution import RDistribution
from structured.targets.r_binary import RBinary
from structured.targets.r_library import RLibrary
from structured.tasks.r_repl import RRepl
from structured.tasks.r_run import RRun

def build_file_aliases():
    return BuildFileAliases(
        targets={
          'r_library': RLibrary,
          'r_binary': RBinary,
        }
    )


def register_goals():
  task(name='r', action=RRepl).install('repl')
  task(name='r', action=RRun).install('run')
