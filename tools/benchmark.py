#!/usr/bin/env -S python3 -O

import argparse
import cProfile
import sys
from collections.abc import Generator
from pathlib import Path
from time import perf_counter

from rflx.model import NeverVerify
from rflx.pyrflx import PyRFLX


class Benchmark:
    def __init__(self) -> None:
        print("Loading...")  # noqa: T201
        start = perf_counter()
        spec_dir = Path("examples/specs")
        self.__pyrflx = PyRFLX.from_specs(
            [str(spec_dir / "ipv4.rflx"), str(spec_dir / "icmp.rflx")],
            NeverVerify(),
            skip_message_verification=True,
        )
        self.__ipv4 = self.__pyrflx.package("IPv4")
        self.__icmp = self.__pyrflx.package("ICMP")
        print(f"Loaded in {perf_counter() - start} seconds")  # noqa: T201

    def generate(self, count: int = 2**16) -> Generator[bytes, None, None]:
        if count > 2**16:
            raise ValueError
        for ident in range(count):
            msg = self.__icmp.new_message("Message")
            pkt = self.__ipv4.new_message("Packet")
            msg.set("Tag", "Echo_Request")
            msg.set("Code_Zero", 0)
            msg.set("Checksum", 0)
            msg.set("Identifier", 0)
            msg.set("Sequence_Number", ident)
            msg.set("Data", bytes(8))

            pkt.set("Version", 4)
            pkt.set("IHL", 5)
            pkt.set("DSCP", 0)
            pkt.set("ECN", 0)
            pkt.set("Total_Length", 20 + len(msg.bytestring))
            pkt.set("Identification", 1)
            pkt.set("Flag_R", "False")
            pkt.set("Flag_DF", "False")
            pkt.set("Flag_MF", "False")
            pkt.set("Fragment_Offset", 0)
            pkt.set("TTL", 64)
            pkt.set("Protocol", "Protocol_Numbers.ICMP")
            pkt.set("Header_Checksum", 0)
            pkt.set("Source", 0)
            pkt.set("Destination", 0)
            pkt.set("Options", [])
            pkt.set("Payload", msg.bytestring)
            yield pkt.bytestring

    def run(self, duration: int = 1) -> None:
        start = perf_counter()
        for i, _ in enumerate(self.generate()):
            runtime = perf_counter() - start
            if runtime >= duration:
                print(f"\nGenerated {i/runtime:.1f} messages per second")  # noqa: T201
                return


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", "--profile", action="store_true", help="run profiler")
    parser.add_argument("-o", "--outfile", type=str, help="print profiler output to file")
    parser.add_argument("-d", "--duration", type=int, default=1, help="duration of benchmark")
    args = parser.parse_args(sys.argv[1:])
    benchmark = Benchmark()
    if args.profile:
        print("Profiling...")  # noqa: T201

        def run() -> None:
            for _ in benchmark.generate(100):
                pass

        cProfile.run("run()", args.outfile, "tottime")
    else:
        benchmark.run(args.duration)
