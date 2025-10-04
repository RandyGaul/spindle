# GLSL 450 Feature Gaps

The current front end focuses on a narrow subset of GLSL 450. The following areas still need implementation work before we can claim broad language coverage:

- **Structured types and interface blocks** – The keyword table does not intern `struct` or related declarations yet, so user-defined structs and uniform blocks cannot be parsed.
- **Switch statements** – There is no token kind for `switch`, `case`, or `default`, leaving structured multi-branch control flow unsupported.
- **Increment/decrement operators** – The token set lacks `++`/`--`, so post/pre increment expressions are rejected today.
- **Boolean literals** – `true`/`false` are not registered as keywords, which prevents direct boolean literals in shaders.
- **Preprocessor directives** – The lexer has no handling for `#` directives, so includes and defines are still out of scope.

Documenting these gaps keeps the roadmap visible as we expand snippet coverage and the core transpiler functionality.
