#!/usr/bin/env python3

"""
Extract all packets of a specified protocol layer from a PCAP file.

The byte representation of each packet is written into a separate file.
For more information run this script with the -h option.
"""

from __future__ import annotations

import argparse
import pkgutil
import pyclbr
import sys
from collections.abc import Sequence
from math import ceil, log
from pathlib import Path
from pydoc import locate

import scapy.layers  # type: ignore[import-untyped]
from scapy.utils import hexdump, rdpcap  # type: ignore[import-untyped]


def main(argv: Sequence[str]) -> bool | str:
    available_layers = [
        f"{p.name}.{c}"
        for p in pkgutil.iter_modules(scapy.layers.__path__)
        for c in pyclbr.readmodule(f"scapy.layers.{p.name}")
    ]

    arg_parser = argparse.ArgumentParser(
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="""
Extract all packets of a specified protocol layer from a PCAP file.

The PCAP file contains a capture of several networks packets with metadata. See
https://en.wikipedia.org/wiki/Pcap and
https://datatracker.ietf.org/doc/draft-ietf-opsawg-pcap for more information on
the file format.

The current script can be used to (a) split the capture into individual packets
and (b) extract only the sub-packets from a given layer. The byte
representation of each extracted packet is written into a separate file.

The script is based on Scapy https://scapy.net/.
""",
    )
    arg_parser.add_argument(
        "-l",
        "--layers",
        action="version",
        version=" ".join(available_layers),
        help="show list of all available Scapy layers",
    )
    arg_parser.add_argument(
        "-p",
        "--payload",
        required=False,
        action="store_true",
        help="extract payload of the layer instead of the whole layer",
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
            if args.payload:
                p = p.payload
            prefix = args.pcap.stem.replace(" ", "_")
            number = str(i).zfill(ceil(log(len(pkts)) / log(10)))
            filename = args.output / f"{prefix}_{number}.raw"
            print(f"Creating {filename}")  # noqa: T201
            Path(filename).write_bytes(bytes(p))
            hexdump(bytes(p))

    return False


if __name__ == "__main__":
    sys.exit(main(sys.argv))
