# Limitations

The RecordFlux toolset currently has the following limitations:

- Only unsigned integers can be used as field types. Signed integers or special integer encodings (e.g., variable-length integer encoding) are not supported.
- The element type of arrays is restricted to message types. If an array of an basic type is needed, a message type must be added which contains this basic type.
- The length of an array must be statically determinable. It is not possible to specify a message with two arrays whose length must be determined at runtime (e.g., by a delimiter).
