#!/usr/bin/env -S python3 -O

import argparse
import cProfile
import sys
from collections.abc import Generator
from pathlib import Path
from time import perf_counter

from tqdm import tqdm  # type: ignore[import-untyped]

from rflx.model import NeverVerify
from rflx.pyrflx import PyRFLX


class Benchmark:
    def __init__(self, specdir: Path) -> None:
        print("Loading...")  # noqa: T201
        start = perf_counter()
        self.__pyrflx = PyRFLX.from_specs(
            [str(specdir / "ipv4.rflx"), str(specdir / "icmp.rflx")],
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
            pkt.set("Protocol", "P_ICMP")
            pkt.set("Header_Checksum", 0)
            pkt.set("Source", 0)
            pkt.set("Destination", 0)
            pkt.set("Options", [])
            pkt.set("Payload", msg.bytestring)
            yield pkt.bytestring

    def run(self) -> None:
        start = perf_counter()
        for i, _ in enumerate(tqdm(self.generate())):
            if i % 500 == 0:
                if perf_counter() - start > 1:
                    print("Performance < 500it/s, stopping")  # noqa: T201
                    sys.exit(1)
                start = perf_counter()


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("-p", "--profile", action="store_true", help="run profiler")
    parser.add_argument("-o", "--outfile", type=str, help="print profiler output to file")
    parser.add_argument("specdir", type=Path, help="specification directory")
    args = parser.parse_args(sys.argv[1:])
    benchmark = Benchmark(args.specdir)
    if args.profile:
        print("Profiling...")  # noqa: T201

        def run() -> None:
            for _ in benchmark.generate(20):
                pass

        cProfile.run("run()", args.outfile)
    else:
        benchmark.run()
