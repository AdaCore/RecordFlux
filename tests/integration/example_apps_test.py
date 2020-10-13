import subprocess

import pytest


def test_icmp_socket() -> None:
    subprocess.run(["python3", "examples/apps/icmp_socket.py"], check=True)


@pytest.mark.root
def test_ping_python() -> None:
    subprocess.run(["make", "-C", "examples/apps/ping", "test_python"], check=True)


@pytest.mark.root
def test_ping_spark() -> None:
    subprocess.run(["make", "-C", "examples/apps/ping", "test_spark"], check=True)
