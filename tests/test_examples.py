import subprocess


def test_icmp_socket() -> None:
    subprocess.run(["python3", "examples/icmp_socket.py"], check=True)
