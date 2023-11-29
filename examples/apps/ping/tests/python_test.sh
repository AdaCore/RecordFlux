#!/bin/bash

set -eu

VENV=$1
source ${VENV}/activate
python3 -O ping.py 127.0.0.1 | sed '/64 bytes from 127\.0\.0\.1: icmp_seq=0/q; $q1'
