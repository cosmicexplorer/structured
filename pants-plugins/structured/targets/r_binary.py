# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.base.exceptions import TargetDefinitionException
from pants.util.memo import memoized_property

from structured.targets.r_target import RTarget

class RBinary(RTarget):

  def __init__(self, script=None, **kwargs):
    if script is None:
      raise TargetDefinitionException(
        self, "An r binary target must specify an R script with 'script='.")
    self._script = script
    super(RBinary, self).__init__(**kwargs)

  @memoized_property
  def script(self):
    return self._script
