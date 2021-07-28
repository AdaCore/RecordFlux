#!/usr/bin/env python3

from subprocess import call

from pkg_resources import get_distribution

dependencies = [f"'{p}'" for p in get_distribution("RecordFlux").requires(extras=("devel",))]
call("pip3 install --upgrade " + " ".join(dependencies), shell=True)
