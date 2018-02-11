# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

import logging
import os

from pants.base.exceptions import TaskError
from pants.binaries.binary_util import BinaryUtil
from pants.engine.isolated_process import ExecuteProcessRequest, ExecuteProcessResult
from pants.fs.archive import TGZ
from pants.subsystem.subsystem import Subsystem
from pants.util.contextutil import temporary_file_path
from pants.util.memo import memoized_method, memoized_property

from structured.subsystems.cran import CRAN
from structured.subsystems.github import Github
from structured.util.boilerplate import R_INSTALL_PACKAGE_BOILERPLATE


logger = logging.getLogger(__name__)


class RDistribution(object):

  DEVTOOLS_CRAN_NAME = 'devtools'

  MODULES_GITHUB_ORG_NAME = 'klmr'
  MODULES_GITHUB_REPO_NAME = 'modules'

  class Factory(Subsystem):
    options_scope = 'r-distribution'

    @classmethod
    def subsystem_dependencies(cls):
      return super(RDistribution.Factory, cls).subsystem_dependencies() + (
        BinaryUtil.Factory,
      )

    @classmethod
    def register_options(cls, register):
      super(RDistribution.Factory, cls).register_options(register)
      register('--r-version', fingerprint=True,
               help='R distribution version. Used as part of the path to '
                    'lookup the distribution with --binary-util-baseurls and '
                    '--pants-bootstrapdir.',
               default='3.4.3')
      register('--modules-git-ref', fingerprint=True,
               help='git ref of the klmr/modules repo to use for R modules.',
               default='940b694027e78f2e98d3f605e5623afcc0113a3c')
      register('--tools-cache-dir', advanced=True, metavar='<dir>',
               default=None,
               help='The parent directory for downloaded R tools. '
                    'If unspecified, a standard path under the workdir is '
                    'used.')
      register('--resolver-cache-dir', advanced=True, metavar='<dir>',
               default=None,
               help='The parent directory for resolved R packages. '
                    'If unspecified, a standard path under the workdir is '
                    'used.')
      register('--artifact-cache-dir', advanced=True, metavar='<dir>',
               default=None,
               help='The parent directory for build R artifacts. '
                     'If unspecified, a standard path under the workdir is '
                     'used.')
      register('--chroot-cache-dir', advanced=True, metavar='<dir>',
               default=None,
               help='The parent directory for the chroot cache. '
                    'If unspecified, a standard path under the workdir is '
                    'used.')

    @memoized_property
    def scratch_dir(self):
      return os.path.join(
        self.get_options().pants_workdir, *self.options_scope.split('.'))

    def create(self):
      binary_util = BinaryUtil.Factory.create()
      options = self.get_options()
      tools_cache_dir = options.tools_cache_dir or os.path.join(
        self.scratch_dir, 'tools')
      resolver_cache_dir = options.resolver_cache_dir or os.path.join(
        self.scratch_dir, 'resolved_packages')
      artifact_cache_dir = options.artifact_cache_dir or os.path.join(
        self.scratch_dir, 'artifacts')
      chroot_cache_dir = options.chroot_cache_dir or os.path.join(
        self.scratch_dir, 'chroots')
      return RDistribution(
        binary_util,
        r_version=options.r_version,
        modules_git_ref=options.modules_git_ref,
        tools_cache_dir=tools_cache_dir,
        resolver_cache_dir=resolver_cache_dir,
        artifact_cache_dir=artifact_cache_dir,
        chroot_cache_dir=chroot_cache_dir,
      )

  def __init__(self, binary_util, r_version, modules_git_ref, tools_cache_dir,
               resolver_cache_dir, artifact_cache_dir, chroot_cache_dir):
    self._binary_util = binary_util
    self._r_version = r_version
    self.modules_git_ref = modules_git_ref
    self.tools_cache_dir = tools_cache_dir
    self.resolver_cache_dir = resolver_cache_dir
    self.artifact_cache_dir = artifact_cache_dir
    self.chroot_cache_dir = chroot_cache_dir

  def _unpack_distribution(self, supportdir, r_version, output_filename):
    logger.debug('unpacking R distribution, version: %s', r_version)
    tarball_filepath = self._binary_util.select_binary(
      supportdir=supportdir, version=r_version, name=output_filename)
    logger.debug('Tarball for %s(%s): %s', supportdir, r_version, tarball_filepath)
    work_dir = os.path.join(os.path.dirname(tarball_filepath), 'unpacked')
    TGZ.extract(tarball_filepath, work_dir, concurrency_safe=True)
    return work_dir

  @memoized_property
  def r_installation(self):
    r_dist_path = self._unpack_distribution(
      supportdir='bin/R', r_version=self._r_version, output_filename='r.tar.gz')
    return r_dist_path

  @memoized_property
  def r_bin_dir(self):
    return os.path.join(self.r_installation, 'bin')

  def invoke_rscript(self, context, stdin_input):
    logger.debug("stdin_input: '{}'".format(stdin_input))
    with temporary_file_path(suffix='.Rscript') as tmp_file_path:
      with open(tmp_file_path, 'w') as tmpfile:
        tmpfile.write(stdin_input)
      with open(tmp_file_path, 'r') as tmpfile:
        logger.debug("tmpfile: '{}'".format(tmpfile.read()))
      r_cmd = [
        'Rscript',
        '--verbose',
        tmp_file_path,
      ]
      env_path = ['PATH', '{}:{}'.format(
        self.r_bin_dir,
        os.environ.get('PATH'),
      )]
      req = ExecuteProcessRequest(tuple(r_cmd), env_path)
      execute_process_result, = context._scheduler.product_request(
        ExecuteProcessResult, [req])
      exit_code = execute_process_result.exit_code
      if exit_code != 0:
        raise TaskError(
          '{} ... exited non-zero ({}).\n-----stdout:\n{}\n-----\nstderr:\n'
          .format(' '.join(r_cmd),
                  exit_code,
                  execute_process_result.stdout,
                  execute_process_result.stderr),
          exit_code=exit_code)
      return execute_process_result.stdout

  def get_installed_packages(self, context, pkg_cache_dir):
    installed_packages_input = R_INSTALL_PACKAGE_BOILERPLATE.format(
      expr="cat('listing installed packages...', sep='\\n')",
      outdir=pkg_cache_dir,
    )
    return self.invoke_rscript(context, installed_packages_input).split('\n')

  def gen_source_install_input(self, source_dir, outdir):
    return R_INSTALL_PACKAGE_BOILERPLATE.format(
      expr="devtools::install_local('{}', lib='{}')".format(
        source_dir, outdir),
      outdir=outdir,
    )

  def install_source_package(self, context, source_dir, pkg_cache_dir):
    source_input = self.gen_source_install_input(source_dir, pkg_cache_dir)
    return self.invoke_rscript(context, source_input).split('\n')

  def install_cran_package(self, cran, context, cran_dep, outdir):
    cran_input = cran.gen_cran_install_input(cran_dep, outdir)
    return self.invoke_rscript(context, cran_input).split('\n')

  def install_github_package(self, github, context, github_dep, outdir):
    github_input = github.gen_github_install_input(github_dep, outdir)
    logger.debug("github_input: '{}'".format(github_input))
    return self.invoke_rscript(context, github_input).split('\n')
