# DCCP

This project provides a simplified example set for a DCCP specification with a "client" and "server" application. Additional details about what each application does are described below.

## DCCP Server

This application acts as the "server," listening on a specific local port for messages in the DCCP format. It is able to receive a specific set of messages based on a small Wireshark sample data set. These messages include:

- DCCP Request
- DCCP Response
- DCCP Ack
- DCCP DataAck
- DCCP Data
- DCCP Close
- DCCP Reset

If one of the above messages is received in the expected format, the message information will be printed to the console window.

This application constantly "listens" for a message and will provide no further indication of operation until a message is received.

## DCCP Client

This application acts as the "client," sending a specific set of DCCP messages to a local port. This specific set of messages was pulled from a Wireshark DCCP sample set ([Link to archive](https://wiki.wireshark.org/uploads/__moin_import__/attachments/SampleCaptures/dccp_trace.pcap.gz)).

For simplicity, and to demonstrate how to write to/access message fields, these messages are hard-coded within the DCCP Client application.

## Using the Applications

The basic setup for these applications is simple. After compiling the code into an executable format:

1. Run the DCCP Server
2. Run the DCCP Client

Once the client is running and prints out message status information, the server should start receiving the same respective information and print the results to the console. 

## DCCP Specification

Note that the DCCP protocol was **NOT** implemented in its entirety. The core of the protocol is the **Generic Header**; a fairly simple implementation of this is implemented in RecordFlux. A small portion of **Additional Fields** and **Options** are also implemented so as to align with the Wireshark sample data set. Additionally, the **Application Data Area** of the protocol is lightly implemented here based on Wireshark data.

The DCCP Specification file is located in the `specs` directory.

The message graph (generated from the specification file) is located in the `graphs` directory. This provides a visual graph of the message itself, which can be useful for understanding and/or message interpretation.

## HOWTO: Generate a Message Graph

To generate a message graph using RecordFlux, locate the message spec and enter the following into the terminal window:

`rflx graph -d graphs specs/dccp.rflx`

For the above, `graphs` is the output directory for the graph and `dccp.rflx` is the spec file.

## HOWTO: Generate Code Files from a Specification

To generate code files for the RecordFlux specification, locate the message spec and enter the following into the terminal window:

`rflx generate -d generated specs/dccp.rflx`

For the above, `generated` is the output directory for the generated code files and `dccp.rflx` is the spec file.

## HOWTO: Validate the Specification

RecordFlux provides the ability to validate a message specification with real world files in a RAW format. To run validate and generate a corresponding report, locate the message spec and enter the following into the terminal window:

`rflx validate --coverage -v tests/samples/valid -- specs/dccp.rflx DCCP::Packet`

For the above, `tests/samples/valid` is the directory in which "valid" sample RAW files are located and `dccp.rflx` is the spec file.
