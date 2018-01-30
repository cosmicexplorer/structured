# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.task.task import Task
from pants.util.memo import memoized_method

from structured.subsystems.r_distribution import RDistribution

class RTask(Task):

  @classmethod
  def subsystem_dependencies(cls):
    return super(RTask, cls).subsystem_dependencies() + (RDistribution.Factory,)

  @memoized_method
  def r_distribution(self):
    return RDistribution.Factory.global_instance().create()

  @memoized_method
  def r_binary(self):
    return self.r_distribution().install_r()
