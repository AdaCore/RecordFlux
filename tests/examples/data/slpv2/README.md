# Test Data Source

These messages are created with following steps:

- Install slpd and slptool
  - Latest official OpenSLP release (WARNING: HTTP ONLY): http://www.openslp.org/download.html
  - Github: https://github.com/openslp-org/openslp
- Start WireShark
  - https://www.wireshark.org/
- Apply WireShark SLP filter `srvloc`
- Start slpd
  - E.g. `slpd -start`
  - Sends a SRVLOC message
- Send more SLPv2 messages using slptool, part of OpenSLP
  - E.g. `slptool findsrvs service:myserv.x`
- In WireShark, mark Service Location Protocol part save packet bytes through File -> Export Packet Bytes...
- Modify message in hex editor to get invalid messages
