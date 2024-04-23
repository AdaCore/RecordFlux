#!/usr/bin/env bash

set -e

VENV=$1
LINUX_WG_PORT=51820
WG_PY_PID_FILE='wireguard_py.pid'
BUILD_DIR='build'

mkdir -p "$BUILD_DIR"

# Linux wireguard keys
wg genkey > "$BUILD_DIR/linux_wg_privatekey"
wg pubkey < "$BUILD_DIR/linux_wg_privatekey" > "$BUILD_DIR/linux_wg_publickey"

# Python wireguard keys
wg genkey > "$BUILD_DIR/python_wg_privatekey"
wg pubkey < "$BUILD_DIR/python_wg_privatekey" > "$BUILD_DIR/python_wg_publickey"

# Setup linux wireguard
ip link add dev wg0 type wireguard
ip addr add dev wg0 10.1.0.1/16
wg set wg0 private-key "$BUILD_DIR/linux_wg_privatekey"
wg set wg0 listen-port "$LINUX_WG_PORT" \
    peer "$(cat $BUILD_DIR/python_wg_publickey)" \
    allowed-ips '10.1.0.0/16,10.2.0.0/16'

# Launch linux wireguard
ip link set wg0 up
ip route add 10.2.0.1 dev wg0 scope link

source "$VENV/activate"
rm -f "$WG_PY_PID_FILE"
./wireguard.py \
    --port "$LINUX_WG_PORT" \
    --static-private-key "$BUILD_DIR/python_wg_privatekey" \
    --static-public-key "$BUILD_DIR/python_wg_publickey" \
    --peer-public-key "$BUILD_DIR/linux_wg_publickey" &

while [ ! -f "$WG_PY_PID_FILE" ]; do
    echo "Waiting for the python implementation to be ready..."
    sleep 2
done

echo "Python wireguard ready at process $(cat $WG_PY_PID_FILE)"

ping -w 120 -c 5 10.2.0.1
RET="$?"

kill -SIGTERM "$(cat $WG_PY_PID_FILE)"
exit "$RET"
