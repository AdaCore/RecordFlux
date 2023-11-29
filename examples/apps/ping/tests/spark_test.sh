#!/bin/bash

set -eu

build/obj/ping 127.0.0.1 | sed '/64 bytes from 127\.0\.0\.1: icmp_seq=0/q; $q1'
