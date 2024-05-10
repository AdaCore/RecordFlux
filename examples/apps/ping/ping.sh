#!/bin/bash -u

VENV=$1
shift
source ${VENV}/activate
python3 -O ping.py $*
