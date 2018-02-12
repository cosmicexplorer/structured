# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.base.payload import Payload

from structured.targets.r_target import RTarget

class RLibrary(RTarget):

  default_sources_globs = [
    '*.r',
    '*.R',
  ]

  def __init__(self, address=None, sources=None, payload=None, **kwargs):
    payload = payload or Payload()
    payload.add_fields({
      'sources': self.create_sources_field(
        sources, address.spec_path, key_arg='sources'),
    })
    super(RTarget, self).__init__(address=address, payload=payload, **kwargs)
