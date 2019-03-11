# Limitations

The RecordFlux toolset currently has the following limitations:

- Only unsigned integers can be used as field types. Signed integers or special integer encodings (e.g., variable-length integer encoding) are not supported.
- The length of an array must be statically determinable. It is not possible to specify a message with two arrays whose length must be determined at runtime (e.g., by a delimiter).
