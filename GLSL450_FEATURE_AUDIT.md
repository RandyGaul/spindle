# GLSL 450 Feature Audit

## Currently Supported

-	*Language scaffolding**
	-	Tokenization covers numeric literals, identifiers, boolean keywords, and the full suite of arithmetic, logical, bitwise, and assignment operators required for shader arithmetic.【F:main.c†L15-L83】【F:main.c†L101-L154】
	-	Parser recognizes declarations, expression statements, blocks, and function definitions while threading the symbol table and IR builder.【F:lex_parse.c†L1172-L1207】【F:lex_parse.c†L1277-L1369】

-	*Type system**
	-	Built-in scalar, vector, and matrix types across float, double, signed/unsigned 32-bit, and 64-bit domains are pre-declared for lookup during parsing.【F:type.c†L304-L372】
	-	Sampler and image types span 1D/2D/3D/cube/array/multisample/rect variants with shadow and integer/bool flavors, enabling binding validation for texture operations.【F:type.c†L373-L438】
	-	Struct declarations, uniform/interface blocks, and struct member arrays are parsed and recorded with layout metadata for downstream IR emission.【F:lex_parse.c†L904-L1029】【F:lex_parse.c†L1043-L1108】

-	*Storage, qualifiers, and layouts**
	-	`in`, `out`, and `uniform` storage qualifiers plus `const` qualifiers can prefix declarations and propagate to symbol records.【F:lex_parse.c†L800-L843】【F:lex_parse.c†L567-L622】
	-	`layout(set = …)`, `layout(binding = …)`, and `layout(location = …)` assignments are accepted alongside `layout(std140)` for uniform buffers and interface blocks.【F:lex_parse.c†L904-L940】

-	*Control flow**
	-	`if`/`else`, `for`, `while`, `do`-`while`, and `switch`/`case`/`default` statements emit structured IR with appropriate scope handling, alongside `break`, `continue`, `return`, and `discard` statements.【F:lex_parse.c†L1814-L1998】【F:lex_parse.c†L2000-L2071】

-	*Built-in variables**
	-	Vertex and fragment stage built-ins such as `gl_Position`, `gl_VertexIndex`, `gl_FragCoord`, and `gl_FrontFacing` are created with stage masks, storage, qualifiers, and array shapes where applicable.【F:lex_parse.c†L258-L309】

-	*Built-in functions**
	-	A wide catalog of texture sampling, coordinate query, math, derivative, matrix, relational, and image atomic intrinsics is registered with kind metadata and arity to drive argument checking and return-type inference.【F:lex_parse.c†L322-L407】【F:type.c†L720-L1945】

## Not Yet Supported or Incomplete

-	*Preprocessing**
	-	`#` directives such as `#include`, `#define`, and macro conditionals are intentionally unsupported; shaders must be preprocessed before being handed to the transpiler.【F:AGENTS.md†L55-L60】

-	*Shader stage coverage**
	-	Only vertex and fragment shaders are targeted today; compute, geometry, tessellation, and mesh/task stages remain out of scope pending IR and resource model extensions.【F:AGENTS.md†L24-L38】

-	*Layout qualifiers**
	-	Unassigned layout identifiers other than `std140` (e.g., `std430`, `row_major`, `shared`) are rejected despite being interned for future support, and additional layout specifiers are not yet interpreted.【F:lex_parse.c†L69-L74】【F:lex_parse.c†L924-L940】

-	*Precision and memory qualifiers**
	-	GLSL precision qualifiers (`highp`, `mediump`, `lowp`) and storage classes like `buffer`, `shared`, or `coherent` have no parsing or semantic handling, limiting compatibility with broader GLSL 450 profiles.【F:lex_parse.c†L800-L843】【F:type.c†L304-L438】

-	*Extended type features**
	-	Pointers, opaque handles beyond samplers/images, and user-defined opaque types (e.g., acceleration structures) are not represented in the type table, constraining feature parity with modern GLSL revisions.【F:type.c†L304-L438】

-	*Stage-specific built-ins**
	-	Only a subset of fragment outputs (`gl_FragDepth`, `gl_SampleMask`) and vertex outputs (`gl_Position`, `gl_PointSize`) are emitted; geometry/tessellation built-ins (e.g., `gl_in`, `gl_InvocationID`) and compute built-ins (`gl_GlobalInvocationID`, barriers) are absent pending broader stage support.【F:lex_parse.c†L258-L309】

-	*Post-processing and optimization**
	-	No SPIR-V optimization or lowering passes exist yet; work is focused on front-end validation before adding target-specific cleanups or canonicalization stages.【F:AGENTS.md†L42-L49】
