# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

import logging
import os
import re
import subprocess
import sys
from contextlib import contextmanager

from pants.binaries.binary_util import BinaryUtil
from pants.engine.isolated_process import ExecuteProcessRequest, ExecuteProcessResult
from pants.fs.archive import TGZ
from pants.subsystem.subsystem import Subsystem
from pants.util.contextutil import environment_as, temporary_file_path
from pants.util.dirutil import safe_mkdir
from pants.util.memo import memoized_method, memoized_property
from pants.util.meta import AbstractClass
from pants.util.objects import datatype


logger = logging.getLogger(__name__)


class RDependency(AbstractClass):
  """???"""


class RInvocationException(Exception):

  INVOCATION_ERROR_BOILERPLATE = "`{cmd}` failed: {what_happened}"

  def __init__(self, cmd, what_happened):
    msg = self.INVOCATION_ERROR_BOILERPLATE.format(
      cmd=' '.join(cmd),
      what_happened=what_happened,
    )
    super(RInvocationException, self).__init__(msg)

class RSpawnFailure(RInvocationException):

  def __init__(self, cmd, err):
    super(RSpawnFailure, self).__init__(cmd=cmd, what_happened=repr(err))

class RProcessResultFailure(RInvocationException):

  PROCESS_RESULT_FAILURE_BOILERPLATE = "exited non-zero ({exit_code}){rest}"

  def __init__(self, cmd, exit_code, rest=''):
    what_happened = self.PROCESS_RESULT_FAILURE_BOILERPLATE.format(
      exit_code=exit_code,
      rest=rest,
    )
    super(RProcessResultFailure, self).__init__(
      cmd=cmd, what_happened=what_happened)

class RProcessInvokedForOutputFailure(RProcessResultFailure):

  INVOKE_OUTPUT_ERROR_BOILERPLATE = """
stdout:
{stdout}
stderr:
{stderr}
"""

  def __init__(self, cmd, exit_code, stdout, stderr):
    rest = self.INVOKE_OUTPUT_ERROR_BOILERPLATE.format(
      stdout=stdout,
      stderr=stderr,
    )
    super(RProcessInvokedForOutputFailure, self).__init__(
      cmd=cmd, exit_code=exit_code, rest=rest)


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
               default='d4199f2d216c6d20c3b092c691d3099c3325f2a3')
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
      chroot_cache_dir = options.chroot_cache_dir or os.path.join(
        self.scratch_dir, 'chroots')
      return RDistribution(
        binary_util,
        r_version=options.r_version,
        modules_git_ref=options.modules_git_ref,
        tools_cache_dir=tools_cache_dir,
        resolver_cache_dir=resolver_cache_dir,
        chroot_cache_dir=chroot_cache_dir,
      )

  def __init__(self, binary_util, r_version, modules_git_ref, tools_cache_dir,
               resolver_cache_dir, chroot_cache_dir):
    self._binary_util = binary_util
    self._r_version = r_version
    self.modules_git_ref = modules_git_ref
    self.tools_cache_dir = tools_cache_dir
    self.resolver_cache_dir = resolver_cache_dir
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

  R_SAVE_IMAGE_BOILERPLATE = """{initial_input}

save.image(file='{save_file_path}', safe=FALSE)
"""

  RDATA_FILE_NAME = '.Rdata'

  def r_invoke_isolated_process(self, context, cmd):
    logger.debug("isolated process '{}'".format(cmd))
    env_path = ['PATH', self.r_bin_dir]
    req = ExecuteProcessRequest(tuple(cmd), env_path)
    res, = context._scheduler.product_request(
      ExecuteProcessResult, [req])
    if res.exit_code != 0:
      raise RProcessInvokedForOutputFailure(
        cmd, res.exit_code, res.stdout, res.stderr)
    return res

  @contextmanager
  def r_isolated_invoke_with_input(self, context, stdin_input, suffix='.R'):
    logger.debug("isolated invoke with stdin_input:\n{}".format(stdin_input))
    with temporary_file_path(suffix=suffix) as tmp_file_path:
      with open(tmp_file_path, 'w') as tmpfile:
        tmpfile.write(stdin_input)
      yield tmp_file_path

  def r_invoke_repl_sandboxed(self, workunit, cmd, cwd):
    new_path = ':'.join([
      self.r_bin_dir,
      os.environ.get('PATH'),
    ])
    with environment_as(PATH=new_path):
      try:
        subproc = subprocess.Popen(
          cmd,
          stdin=sys.stdin,
          stdout=workunit.output('stdout'),
          stderr=workunit.output('stderr'),
          cwd=cwd,
        )
        return subproc.wait()
      except OSError as e:
        raise RSpawnFailure(cmd, e)
      except subprocess.CalledProcessError as e:
        raise RProcessResultFailure(cmd, e.returncode, e)

  def invoke_r_interactive(self, context, workunit, initial_input, chroot_dir,
                           clean_chroot=False):
    logger.debug("interactive in '{}', initial_input: '{}'".format(
      chroot_dir, initial_input))

    rdata_path = os.path.join(chroot_dir, self.RDATA_FILE_NAME)

    input_with_save = self.R_SAVE_IMAGE_BOILERPLATE.format(
      initial_input=initial_input,
      save_file_path=rdata_path,
    )

    safe_mkdir(chroot_dir, clean=clean_chroot)
    with self.r_isolated_invoke_with_input(
        context, input_with_save) as tmp_file_path:
      save_cmd = [
        'R',
        '--vanilla',
        '--slave',
        '--file={}'.format(tmp_file_path)
      ]
      self.r_invoke_isolated_process(context, save_cmd)

    r_cmd = [
      'R',
      '--save',
      '--restore',
      '--interactive',
    ]
    return self.r_invoke_repl_sandboxed(workunit, r_cmd, chroot_dir)

  def invoke_rscript(self, context, stdin_input):
    with self.r_isolated_invoke_with_input(
        context, stdin_input) as tmp_file_path:
      r_cmd = [
        'Rscript',
        '--verbose',
        tmp_file_path,
      ]
      return self.r_invoke_isolated_process(context, r_cmd)

  class PackageInfoFormatError(Exception):
    """???"""

  BLANK_LINE_REGEX = re.compile('^\s*$')

  @classmethod
  def is_valid_package_name(cls, name):
    return cls.BLANK_LINE_REGEX.match(name) is None

  @classmethod
  def check_valid_package_name(cls, name):
    if not cls.is_valid_package_name(name):
      raise PackageInfoFormatError(
        "'{}' is not a valid package name (must not be blank)".format(name))
    return name

  @classmethod
  def filter_packages_lines_stdout(cls, lines):
    return [p for p in lines if cls.is_valid_package_name(p)]

  VALID_VERSION_REGEX = re.compile('^[0-9]+(\.[0-9]+)*$')

  @classmethod
  def is_valid_version(cls, version):
    if version is None:
      return True
    return cls.VALID_VERSION_REGEX.match(version) is not None

  @classmethod
  def check_valid_version(cls, version):
    if not cls.is_valid_version(version):
      raise PackageInfoFormatError(
        "'{}' is not a valid package version "
        "(must be 'None' or match '{}')"
        .format(version, cls.VALID_VERSION_REGEX.pattern))
    return version

  @classmethod
  def gen_script_load_stmts(cls, srcs_rel):
    if len(srcs_rel) == 0:
      return ''

    source_stmts = ["source('{}')".format(s.encode('ascii')) for s in srcs_rel]
    return '\n'.join(source_stmts) + '\n'

  @classmethod
  def create_valid_r_charvec_input(cls, elements, drop_empty=False):
    if isinstance(elements, str):
      elements = [elements]

    if len(elements) == 0:
      if drop_empty:
        return None
      return 'character(0)'
    elif len(elements) == 1:
      return "'{}'".format(elements[0])

    quoted = ["'{}'".format(el) for el in elements]
    return "c({})".format(', '.join(quoted))

  @classmethod
  def gen_libs_input(cls, lib_paths):
    libs_charvec = cls.create_valid_r_charvec_input(lib_paths, drop_empty=True)
    if libs_charvec is None:
      return ''
    return ".libPaths({})".format(libs_charvec) + '\n'

  R_LIST_PACKAGES_BOILERPLATE = """{libs_input}
installed.packages(lib.loc={libs_joined})
"""

  def get_installed_packages(self, context, lib_paths):
    libs_input = self.gen_libs_input(lib_paths)
    libs_charvec = self.create_valid_r_charvec_input(lib_paths, drop_empty=True)
    if libs_charvec is None:
      libs_charvec="NULL"

    installed_packages_input = self.R_LIST_PACKAGES_BOILERPLATE.format(
      libs_input=libs_input,
      libs_joined=libs_charvec,
    )
    pkgs = self.invoke_rscript(context, installed_packages_input).stdout.split('\n')
    return self.filter_packages_lines_stdout(pkgs)

  # R_INSTALL_SOURCE_PACKAGE_BOILERPLATE = """???"""

  # def gen_source_install_input(self, source_dir, outdir):
  #   return self.R_INSTALL_SOURCE_PACKAGE_BOILERPLATE.format(
  #     expr="devtools::install_local('{}', lib='{}')".format(
  #       source_dir, outdir),
  #     outdir=outdir,
  #   )

  def install_source_package(self, context, source_dir, pkg_cache_dir):
    source_input = self.gen_source_install_input(source_dir, pkg_cache_dir)
    pkgs = self.invoke_rscript(context, source_input).stdout.split('\n')
    return self.filter_packages_lines_stdout(pkgs)

  def install_cran_package(self, cran, context, cran_dep, outdir):
    cran_input = cran.gen_cran_install_input(cran_dep, outdir)
    pkgs = self.invoke_rscript(context, cran_input).stdout.split('\n')
    return self.filter_packages_lines_stdout(pkgs)

  def install_github_package(self, github, context, github_dep, outdir):
    github_input = github.gen_github_install_input(
      self.tools_cache_dir, github_dep, outdir)
    logger.debug("github_input: '{}'".format(github_input))
    pkgs = self.invoke_rscript(context, github_input).stdout.split('\n')
    return self.filter_packages_lines_stdout(pkgs)
