#!/usr/bin/env python3

"""
This tool extracts all packets of a specified protocol layer from a PCAP file and writes the byte
representation of each packet into a separate file.
"""

import argparse
import pkgutil
import pyclbr
import sys
from math import ceil, log
from pathlib import Path
from pydoc import locate
from typing import Sequence, Union

import scapy.layers  # type: ignore
from scapy.utils import hexdump, rdpcap  # type: ignore


def main(argv: Sequence[str]) -> Union[bool, str]:
    available_layers = [
        f"{p.name}.{c}"
        for p in pkgutil.iter_modules(scapy.layers.__path__)
        for c in pyclbr.readmodule(f"scapy.layers.{p.name}")
    ]

    arg_parser = argparse.ArgumentParser()
    arg_parser.add_argument(
        "-l",
        "--layers",
        action="version",
        version=" ".join(available_layers),
        help="show list of all available Scapy layers",
    )
    arg_parser.add_argument(
        "layer",
        metavar="LAYER",
        type=str,
        help="Scapy layer in the form <package>.<class> (e.g., dhcp.BOOTP)",
    )
    arg_parser.add_argument("pcap", metavar="PCAP_FILE", type=Path)
    arg_parser.add_argument("output", metavar="OUTPUT_DIRECTORY", type=Path)
    args = arg_parser.parse_args(argv[1:])

    if "." not in args.layer:
        return "layer must be in the form <package>.<class> (e.g., dhcp.BOOTP)"

    if not args.output.exists():
        return f'output directory "{args.output}" does not exist'

    if not args.output.is_dir():
        return f'output directory "{args.output}" is not a directory'

    if args.layer not in available_layers:
        return (
            f'unknown layer "{args.layer}"'
            " (use --layers to show a list of all available Scapy layers)"
        )

    layer = locate(f"scapy.layers.{args.layer}")

    if layer is None:
        return f'layer "{args.layer}" not found'

    pkts = rdpcap(str(args.pcap))

    for i, pkt in enumerate(pkts):
        if pkt.haslayer(layer):
            p = pkt.getlayer(layer)
            prefix = args.pcap.stem.replace(" ", "_")
            number = str(i).zfill(ceil(log(len(pkts)) / log(10)))
            filename = args.output / f"{prefix}_{number}.raw"
            print(f"Creating {filename}")
            with open(filename, "wb") as f:
                f.write(bytes(p))
            hexdump(bytes(p))

    return False


if __name__ == "__main__":
    sys.exit(main(sys.argv))
