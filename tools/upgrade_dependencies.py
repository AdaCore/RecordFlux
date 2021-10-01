#!/usr/bin/env python3

"""Upgrade all development dependencies of RecordFlux."""

from subprocess import call

from pkg_resources import get_distribution

dependencies = [f"'{p}'" for p in get_distribution("RecordFlux").requires(extras=("devel",))]
call("pip3 install --upgrade " + " ".join(dependencies), shell=True)
