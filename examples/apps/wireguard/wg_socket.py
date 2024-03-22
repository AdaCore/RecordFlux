import socket


class WireguardSocket:
    def __init__(self, ip: str, port: int, bind: bool = False) -> None:
        self.ip = ip
        self.port = port
        self.sk = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
        self.sk.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        if bind:
            self.sk.bind((ip, port))

    def connect(self) -> None:
        self.sk.connect((self.ip, self.port))

    def read(self) -> bytes:
        return self.sk.recv(4096)

    def send_all(self, message: bytes) -> None:
        sent_length = 0
        while sent_length < len(message):
            sent_length += self.sk.sendto(message[sent_length:], (self.ip, self.port))
