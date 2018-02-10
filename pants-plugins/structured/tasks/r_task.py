# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.task.task import Task
from pants.util.memo import memoized_property

from structured.subsystems.r_distribution import RDistribution

class RTask(Task):

  @classmethod
  def subsystem_dependencies(cls):
    return super(RTask, cls).subsystem_dependencies() + (
      RDistribution.Factory.scoped(cls),
    )

  @memoized_property
  def r_distribution(self):
    return RDistribution.Factory.scoped_instance(self).create()
