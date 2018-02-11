# coding=utf-8

from __future__ import (absolute_import, division, generators, nested_scopes, print_function,
                        unicode_literals, with_statement)

from contextutil import contextmanager

from abc import abstractmethod, abstractproperty

from pants.base.payload_field import PayloadField
from pants.scm.git import Git
from pants.util.contextutil import temporary_dir
from pants.util.objects import datatype

from structured.targets.r_target import RTarget





class RPackage(RTarget):

  # existing_package_dict is a dict of package name -> r_package() target addr
  def extract_new_package(self, existing_package_dict):
    with temporary_dir() as tmpdir:
