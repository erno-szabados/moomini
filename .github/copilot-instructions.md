# Copilot Instructions for the Moomini Project

- **Project Goal:** Implement the [Gemini protocol](../doc/protocol-specification.gmi) as described in the documentation.
- **Language:** GNU Modula-2, ISO dialect.
- **Style:** Write idiomatic Modula-2 code. Functions and procedures should be clear, concise, and well-structured.
- **Copilot support:** If a language feature is ambiguous (different behaviour in dialects), refer to examples in the `doc/` folder.
- **Best Practices:** 
  - Use meaningful names for modules, procedures, and variables.
  - Use PascalCase for module names, constants and procedures and camelCase for variables.
  - Export only what is necessary; keep the interface clean.
- **Dependencies:** Minimize external dependencies; prefer ISO Modula-2 libraries.

# Language Specifics
- For bitwise operations, use the builtin BITSET type, bitwise and is +, bitwise or is *.
- For bitwise shift operations, use the `SHIFT` and `ROTATE` operators from the SYSTEM module.
- For valid example on bitwise operations, see the example in `doc/BitSetExample.mod`.

## Building

- The project uses **GNU Make** for building.
- New modules and tests **must be added to the Makefile**.
- Tests can be executed with the `make test` target.

## Testing

- Place unit tests in the `test/` directory, using idiomatic Modula-2 test patterns.
- Reference style and approach: see `test/TestUTF8.mod`.
- Tests should be granular enough to help identify problems, but avoid excess verbosity.
- Minimize external dependencies; prefer ISO Modula-2 libraries where possible.