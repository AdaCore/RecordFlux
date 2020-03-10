from rflx.pyrflx import (Message, Package, PyRFLX)
import socket


class ICMPSocket:

    package_icmp: Package

    def __init__(self):
        pyrflx = PyRFLX([f"specs/icmp.rflx"])
        self.package_icmp = pyrflx["ICMP"]

    def send_icmp_request(self) -> None:

        # create the icmp message
        icmp_request: Message = self.__create_msg()

        icmp_socket = None
        # create the socket
        try:
            icmp_socket = socket.socket(family=socket.AF_INET, type=socket.SOCK_DGRAM, proto=socket.IPPROTO_ICMP)
        except OSError as e:
            print("Error while creating socket " + e.strerror)
            exit(1)

        # send the msg
        try:
            icmp_socket.sendto(icmp_request.binary, ('172.217.16.131', 1))
        except InterruptedError as e:
            icmp_socket.close()
            print("Error while sending icmp request" + e.strerror)
            exit(1)

        echo = icmp_socket.recv(4096)
        print(echo.hex())
        print(icmp_request.binary.hex())

    def __create_msg(self) -> Message:
        icmp = self.package_icmp["Echo_Message"]
        icmp.set("Tag", "Echo_Request")
        icmp.set("Code", 0)
        icmp.set("Checksum", 3218)
        icmp.set("Identifier", 5)
        icmp.set("Sequence_Number", 1)
        icmp.set(
            "Data",
            b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
            b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
            b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
        )

        return icmp


def main():
    s = ICMPSocket()
    s.send_icmp_request()


main()
