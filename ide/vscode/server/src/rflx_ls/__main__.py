from argparse import ArgumentParser

from rflx_ls.server import server

parser = ArgumentParser()
parser.add_argument("--version", action="version", version="0.1.0")
parser.parse_args()

server.start_io()
