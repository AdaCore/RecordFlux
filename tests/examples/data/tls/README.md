# Message samples for TLS and DTLS

## General

The `pcap` files in the current folder contain WireShark captures that have been used to populate
several of the folders with the path prefix `../tls*` containing raw packets with specific TLS or
DTLS message types. See the original resources referenced below for the full details of the
applicable copyright terms. In addition, some of the folders contain also custom-built samples.
Those have been documented in the respective folders.

### About TLS 1.3 and DTLS 1.3 extensions

Several of the extensions defined for TLS 1.2 / DTLS 1.2 are no more neither required nor supported in TLS 1.3 / DTLS 1.3. See [RFC 8446 4.2](https://datatracker.ietf.org/doc/html/rfc8446#section-4.2) for the applicable extensions.

However, many available TLS 1.3 and DTLS 1.3 captures still contain some extensions that should not be there.
For instance, the Encrypt_Then_Mac extension (22).
Presumably, the other parties just ignore them.
However, for strict compliance they should be rejected.
See the `invalid` folders of the respective samples for more information.

## TLS: Captures

### Capture 1: Illustrated TLS (TLS 1.3)

* File: [tls13-1.pcap](tls13-1.pcap) -- TLS 1.3. session over TCP from "The Illustrated TLS
  Connection" [repository](https://tls13-1.pcap/syncsynchalt/illustrated-tls13). See also the related
  website: [https://tls13.xargs.org/](https://tls13.xargs.org/).

* File: [tls13-1-keylog.txt](tls13-1-keylog.txt) -- A [NSS key
log](https://web.archive.org/web/20230425034128/https://firefox-source-docs.mozilla.org/security/nss/legacy/key_log_format/index.html)
  for this capture.

  * Note: For TLS such files can be loaded in Wireshark by right-clicking a TLS packet and selecting
    `Protocol Preferences -> Transport Layer Security -> Pre-Master-Secret log filename` to let
    Wireshark decrypt and show TLS connection details.
  * Note: See [this IETF draft](https://datatracker.ietf.org/doc/draft-thomson-tls-keylogfile/) for
    followup documentation on that format.

License: MIT.

### Capture 2: TLS 1.3 WireShark test

File: [tls13-2.pcap](tls13-2.pcap) -- WireShark test sample for TLS 1.3. Original file: [tls13-rfc8446.pcap](https://gitlab.com/wireshark/wireshark/-/blob/master/test/captures/tls13-rfc8446.pcap).

License: GNU GPLv2.

## TLS: Extracting Protocol Data Units (PDUs)

For the extraction of TLS records from a `.pcap` file there are several possiblities.

Unfortunately, at this point the [extract_packets.py](../../../../tools/extract_packets.py) script doesn't yet support the extraction of the TLS layer packets directly. We could extract the TCP segments this way.
However, the TCP header doesn't have a fixed length (e.g., [see here](https://www.rfc-editor.org/rfc/rfc9293#name-header-format)).
So, we cannot simply trim the segments by a fixed number.

Instead, one solution is to export the TLS records manually from the Wireshark GUI by selecting the
relevant part of the packet and choosing the action "Export Packet Bytes".
This has been done for the `tls13-1.pcap` and parts of the `tls13-2.pcap` capture.

Notes:
* Not all the frames in that capture contain TLS records.
  Some are other TCP messages.
* Some TCP segments contain multiple TLS records.
* If the RecordFlux type does not correspond exactly to the packet tree in WireShark, then the
  extracted file might need further trimming.
  See the section [Extract DTLS packets](#2-extract-dtls-packets) for tips how to do that.

# DTLS: Captures

### Capture 1: dtls13-1.pcap (DTLS 1.3)

File: [ws-dtls13.pcap](dtls13-1.pcap) -- DTLS 1.3. session over UDP from:
https://gitlab.com/wireshark/wireshark/-/issues/18071.

License: No explicit license mentioned.

### Capture 2: Illustrated DTLS (DTLS 1.3)

* File: [dtls13-2.pcap](dtls13-2.pcap) -- DTLS 1.3. session over UDP from "The Illustrated DTLS
  Connection" [repository](https://github.com/syncsynchalt/illustrated-dtls). See also the related
  website: [https://dtls.xargs.org](https://dtls.xargs.org).

* File: [dtls13-2-keylog.txt](dtls13-2-keylog.txt) -- A [NSS key
  log](https://web.archive.org/web/20230425034128/https://firefox-source-docs.mozilla.org/security/nss/legacy/key_log_format/index.html)
  for this capture.

  * Note: For TLS such files can be loaded in Wireshark by right-clicking a TLS packet and selecting
    `Protocol Preferences -> Transport Layer Security -> Pre-Master-Secret log filename` to let
    Wireshark decrypt and show TLS connection details. However, as of Wireshark version version 4.0.6
    this capability is not yet supported in Wireshark for DTLS.
  * Note: See [this IETF draft](https://datatracker.ietf.org/doc/draft-thomson-tls-keylogfile/) for
    followup documentation on that format.

License: MIT.

## DTLS: Extracting Protocol Data Units (PDUs)

The extraction of DTLS packets from the `.pcap` files is done in several phases:

1. The [extract_packets.py](../../../../tools/extract_packets.py) script is used to extract the byte
   representation of the UDP datagrams from the `.pcap` files. (Unfortunately, at this point the
   script doesn't yet support the extraction of DTLS layer packets directly.)
1. Remove the UDP headers to obtain the payload bytes.
1. If the UDP payload contains multiple DTLS records, then split them further into several smaller
   files.

### 1. Extract UDP datagrams

For instance, a file `capture.pcap` capture can be split to UDP datagrams like this:

```console
RECORDFLUX_DIR/tools/extract_packets.py inet.UDP \
 CAPTURES_DIR/capture.pcap \
 OUTPUT_DIR
```
### 2. Extract DTLS packets

The UDP frame header has a fixed length of 8 bytes (e.g., [see
here](https://en.wikipedia.org/wiki/User_Datagram_Protocol#UDP_datagram_structure)). So, we can just
use any tool to drop off the first 8 bytes from the frame. For instance, it could be done using the
`tail` utility on Linux like that (note that the bytes that will be included start from the next
position, i.e. 9):

```console
$ tail -c +9 INPUT_FILE > OUTPUT_FILE
```

For example, assuming that the UDP frames have been stored to some directory one frame per file and
each file has the extension .raw, then you can do:

```console
$ cd UDP_DIR
$ find -maxdepth 1 -name "*.raw" -exec sh -c "tail -c +9 {} > DTLS_DIR/{}" \;
```

### 3. Split multi-record datagrams

A single UDP datagaram (packet) may carry multiple DTLS records. There is no generic means to
extract those records without decoding the record data first. For instance, the `dtls13-1` capture
03 contains 3 records, captures 06 and 08 contain 2 records. The lengths of these records have been
determined one by one and the respective sample files have been split to smaller sample files.

For example:

```console
$ # Extract 144 bytes from the beginning of a file.
$ head -c 144 dtls13-1_03.raw > dtls13-1_03a.raw
$ # Extract 36 bytes from the middle of the file starting with position 145.
$ tail -c +145 dtls13-1_03.raw | head -c 36 > dtls13-1_03b.raw
$ # Extract 77 bytes from the end of the file.
$ tail -c 77 dtls13-1_03.raw > dtls13-1_03c.raw
```
### Sample file naming scheme

The following naming convention has been used:

```
<capture_prefix>_<2 digit frame id><record id letter>?(_<packet type id><index>?)*.raw
```

For example:

```
tls13-1_04a_hs_ch_ex0.raw

where:
 * capture_prefix = `tls13-1`
 * 2 digit frame id = `04`     <-- Ethernet frame number 5.
 * record id letter = `a`      <-- 1st DTLS_Record (DTLS_Plaintext) in the TCP segment.
 * packet type id 1 = `hs`     <-- Handshake message
 * packet type id 2 = `ch`     <-- ClientHello message
 * packet type id 3 = `ex0`    <-- Extension number 1
```

```
dtls13-1_03b.raw

where:
 * capture_prefix = `dtls13-1`
 * 2 digit frame id = `03`     <-- Ethernet frame number 4.
 * record id letter = `b`      <-- 2nd DTLS_Record (DTLS_Ciphertext) in the packet.
```
