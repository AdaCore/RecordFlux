import socket
import sys

from rflx.pyrflx import MessageValue, PyRFLX


class ICMPSocket:
    def __init__(self) -> None:
        pyrflx = PyRFLX(["specs/icmp.rflx"])
        self.package_icmp = pyrflx["ICMP"]
        self.icmp_data = (
            b"\x4a\xfc\x0d\x00\x00\x00\x00\x00\x10\x11\x12\x13\x14\x15\x16\x17"
            b"\x18\x19\x1a\x1b\x1c\x1d\x1e\x1f\x20\x21\x22\x23\x24\x25\x26\x27"
            b"\x28\x29\x2a\x2b\x2c\x2d\x2e\x2f\x30\x31\x32\x33\x34\x35\x36\x37"
        )

    def send_icmp_request(self) -> None:
        icmp_request: MessageValue = self.__create_msg()
        try:
            icmp_socket = socket.socket(
                family=socket.AF_INET, type=socket.SOCK_DGRAM, proto=socket.IPPROTO_ICMP
            )
        except OSError as e:
            sys.exit(f"Error while creating socket {e}")
        try:
            icmp_socket.sendto(icmp_request.bytestring, ("localhost", 1))
        except InterruptedError as e:
            icmp_socket.close()
            sys.exit(f"Error while sending icmp request {e}")
        echo = icmp_socket.recv(4096)
        print(f"Request sent  : {icmp_request.bytestring.hex()}")
        print(f"Reply received: {echo.hex()}")
        if echo[8:] == self.icmp_data:
            print("ICMP data is equal")

    def __create_msg(self) -> MessageValue:
        icmp = self.package_icmp["Echo_Request_Reply_Message"]
        icmp.set("Tag", "Echo_Request")
        icmp.set("Code", 0)
        icmp.set("Checksum", 12824)
        icmp.set("Identifier", 5)
        icmp.set("Sequence_Number", 1)
        icmp.set("Data", self.icmp_data)
        return icmp


if __name__ == "__main__":
    SOCKET = ICMPSocket()
    SOCKET.send_icmp_request()
