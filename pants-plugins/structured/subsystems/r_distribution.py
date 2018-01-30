# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

import logging
import os

from pants.binaries.binary_util import BinaryUtil
from pants.fs.archive import TGZ
from pants.subsystem.subsystem import Subsystem
from pants.util.memo import memoized_method, memoized_property


logger = logging.getLogger(__name__)


class RDistribution(object):

  class Factory(Subsystem):
    options_scope = 'r-distribution'

    @classmethod
    def subsystem_dependencies(cls):
      return (BinaryUtil.Factory,)

    @classmethod
    def register_options(cls, register):
      super(RDistribution.Factory, cls).register_options(register)
      register('--version', fingerprint=True,
               help='R distribution version. Used as part of the path to '
                    'lookup the distribution with --binary-util-baseurls and '
                    '--pants-bootstrapdir.',
               default='3.4.3')

    def create(self):
      binary_util = BinaryUtil.Factory.create()
      options = self.get_options()
      return RDistribution(
        binary_util=binary_util,
        version=options.version,
      )

  def __init__(self, binary_util, version):
    self._binary_util = binary_util
    self._version = version
    logger.debug('R version: %s', version)

  @memoized_property
  def version(self):
    return self._version

  def unpack_distribution(self, supportdir, version, output_filename):
    tarball_filepath = self._binary_util.select_binary(
      supportdir=supportdir, version=version, name=output_filename)
    logger.debug('Tarball for %s(%s): %s', supportdir, version, tarball_filepath)
    work_dir = os.path.join(os.path.dirname(tarball_filepath), 'out')
    TGZ.extract(tarball_filepath, work_dir, concurrency_safe=True)
    return work_dir

  @memoized_method
  def install_r(self):
    r_dist_path = self.unpack_distribution(
      supportdir='bin/R', version=self.version, output_filename='r.tar.gz')
    r_bin_path = os.path.join(r_dist_path, 'R')
    return r_bin_path
