# Design Guide

The following guidelines should be followed for clearity and consistency throughout the project:

- The formatting of error messages should be consistent. An error message starts with a lowercase letter and identifiers are highlighted by double quotes.
- Private functions are prefered over inner functions. Long inner functions can impede the comprehension of a function.
- Internal methods are prefixed by a single underscore.
- The methods of a class are sorted to make it easer to identify the public interface of a class:
    1. Special methods (starting and ending with double underscores)
    2. Public methods
    3. Internal methods (starting with single underscore).
