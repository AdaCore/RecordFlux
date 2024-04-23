# WireGuard

This example shows how to use `PyRFLX` to implement a subset version of [WireGuard](https://www.wireguard.com/papers/wireguard.pdf).
The example app creates an encrypted tunnel between `10.1.0.1` (PyRFLX) and `10.2.0.1` (Linux WireGuard).
The tunnel is tested using the `ping` utility.
Note that `IP` packets and `ICMP` packets are also parsed by RecordFlux.

## Rust

This example also demonstrates how to integrate Rust code with `PyRFLX`.
The [tai64](https://docs.rs/tai64/latest/tai64/) crate is used to parse and create [TAI64N timestamps](https://cr.yp.to/libtai/tai64.html#tai64n).

## How to use?

### Build and install the tai64 binding

Make sure you're working from a Python virtual environment (e.g `poetry shell`).

```command
$ make -C tai64_bindings build
```

To make sure that the binding is installed the following command should run without errors.
```command
$ python -c 'import tai64_bindings'
```

### Setup WireGuard

To set up a WireGuard interface, you can look at [tests/wireguard_test.sh](./tests/wireguard_test.sh).
It follows the usual WireGuard interface creation process, including the generation of keys, allowed IPs, routes, etc.

You can now run the `wireguard.py` script and ping the `PyRFLX` side of the tunnel using ping (assuming that the configured python side of the tunnel is `10.2.0.2`):
```command
$ ping -c 5 10.2.0.1
```
