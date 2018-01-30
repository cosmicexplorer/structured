# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from pants.base.payload import Payload
from pants.base.payload_field import PrimitiveField
from pants.build_graph.target import Target

class RTarget(Target):

  default_sources_globs = [
    '*.r',
    '*.R',
  ]

  def __init__(self, address=None, sources=None, payload=None, **kwargs):
    payload = payload or Payload()
    payload.add_fields({
      'sources': self.create_sources_field(
        sources=sources,
        sources_rel_path=address.spec_path,
        key_arg='sources',
      ),
    })
    super(RTarget, self).__init__(address=address, payload=payload, **kwargs)
