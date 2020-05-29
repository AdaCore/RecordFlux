# Development

## Installation

After cloning the repository into a directory called `RecordFlux`, `pip3` can be used to install the project in editable mode:

```Console
$ pip3 install -e "RecordFlux[devel]"
```

This will also install all development dependencies, except for GNAT, which must be installed separately.

On some Linux distributions (e.g., Debian, Ubuntu) a kernel parameter needs to be set in order that all tests can be executed successfully. The following command allows all users on the system to send ICMP messages using a special ICMP socket type.

```
# sysctl -w net.ipv4.ping_group_range="0 2147483647"
```

## Running tests

```Console
$ make test
```

## Automatic Code Formatting

```Console
$ make format
```

