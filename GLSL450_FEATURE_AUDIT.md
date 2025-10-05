# GLSL 450 Feature Audit

## Currently Supported

- **Language scaffolding**
	- Tokenization covers numeric literals, identifiers, boolean keywords, and arithmetic/logical/assignment operators required for shader arithmetic.【F:main.c†L15-L155】
	- Parser routines handle declarations, statements, and expressions while threading symbol tables and IR builders for emission.【F:lex_parse.c†L1487-L1531】【F:lex_parse.c†L1916-L2099】

- **Type system**
	- Built-in scalar, vector, matrix, and opaque sampler/image types are registered up front so lookups succeed during parsing.【F:type.c†L300-L419】
	- Struct declarations and interface blocks capture member metadata and layout tags for downstream IR consumption.【F:lex_parse.c†L1000-L1090】【F:lex_parse.c†L1165-L1222】

- **Storage, qualifiers, and layouts**
	- `in`, `out`, `uniform`, `buffer`, and `shared` storage keywords plus qualifiers such as `const`, `coherent`, `volatile`, `restrict`, `readonly`, and `writeonly` are interned and applied to symbols during parsing.【F:lex_parse.c†L47-L105】【F:lex_parse.c†L840-L885】
	- Layout blocks record identifiers (`std140`, `std430`, etc.) and assignments (`set`, `binding`, `location`) for blocks and variables, and stage layout statements emit IR for compute `local_size_*` keys.【F:lex_parse.c†L887-L921】【F:lex_parse.c†L1487-L1510】【F:lex_parse.c†L2207-L2232】

- **Control flow**
	- `if`/`else`, loops (`for`/`while`/`do`), `switch`/`case`, and flow statements (`break`, `continue`, `return`, `discard`) emit structured IR with scope management.【F:lex_parse.c†L1916-L2099】

- **Built-in variables**
	- Vertex, fragment, and compute built-ins (`gl_Position`, `gl_FragCoord`, `gl_GlobalInvocationID`, etc.) register stage masks, storage classes, qualifiers, and array metadata.【F:lex_parse.c†L302-L338】

- **Built-in functions**
	- Texture, math, derivative, matrix, relational, and image atomic intrinsics are registered with kind metadata and arity for argument checking and return-type inference.【F:lex_parse.c†L350-L417】【F:type.c†L720-L1945】

## Not Yet Supported or Incomplete

- **Preprocessing**
	- `#` directives such as `#include` and `#define` remain unsupported; shaders must be preprocessed before parsing.【F:AGENTS.md†L31-L34】

- **Shader stage coverage**
	- Geometry, tessellation, and mesh/task shader stages remain out of scope pending IR and resource model extensions.【F:lex_parse.c†L302-L338】

- **Layout qualifiers**
	- Identifiers like `row_major`, `column_major`, and `packed` are interned but still lack semantic handling beyond preservation for future work.【F:lex_parse.c†L55-L105】

- **Precision qualifiers**
	- GLSL precision keywords (`highp`, `mediump`, `lowp`) and `precise` are not recognized or propagated through parsing today.【F:lex_parse.c†L83-L105】【F:lex_parse.c†L840-L875】

- **Stage-specific built-ins**
	- Geometry/tessellation built-ins (`gl_in`, `gl_InvocationID`, etc.) and stage-specific barriers are not registered yet.【F:lex_parse.c†L302-L338】

- **Post-processing and optimization**
	- No SPIR-V optimization or lowering passes exist; the focus remains on front-end validation before adding target-specific cleanups.【F:AGENTS.md†L42-L49】
