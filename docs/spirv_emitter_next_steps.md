# SPIR-V Emitter Next Steps

## Current gaps
- The current emitter only writes module headers and empty function shells that return `OpUndef` or `OpReturn`, so no actual shader logic or interface decorations reach the module yet.
- Entry points are emitted without declaring the input/output interface variables, descriptor bindings, or execution modes required by SDL_Gpu shaders.
- The IR already captures declarations, storage classes, control flow markers, and expression ops, which gives us the raw material needed for a full translation pass once we map them to SPIR-V constructs.

## Prioritized roadmap
1. Establish module-level resources and decorations
	- Emit `OpString`, `OpSource`, and `OpName` for debugging plus `OpDecorate` for descriptor sets, bindings, locations, and builtins required by stage IO.
	- Map IR declarations to `OpVariable` in the correct storage classes (Input, Output, Uniform, UniformConstant, StorageBuffer, Workgroup).
	- Generate composite type definitions for structs, interface blocks, runtime arrays, and sampled images so descriptor-backed data shows up correctly for SDL_Shadercross.
2. Translate expressions and SSA value creation
	- Introduce a value table that turns IR temporaries into SSA ids and emit arithmetic, logical, comparison, and vector/matrix ops (`OpFAdd`, `OpIMul`, swizzle shuffles, constructors) that cover the builtin snippets in `spindle.h`.
	- Handle constant emission via `OpConstant`, `OpConstantComposite`, and specialization-friendly literals so uniform initializers and literal folding are representable.
3. Support control-flow constructs
	- Leverage the existing IR block markers to build SPIR-V structured control flow (`OpSelectionMerge`, `OpLoopMerge`, `OpBranch`, `OpBranchConditional`).
	- Ensure break/continue statements map to the right merge/continue blocks and cover `if`, `for`, `while`, `do-while`, and `switch` constructs already tested by the snippets.
4. Wire up stage interfaces and execution modes
	- Reflect vertex inputs, fragment outputs, local workgroup sizes, and builtins like `gl_Position` or `gl_FragCoord` into the entry point interface list and attach the required `OpDecorate` and `OpExecutionMode` instructions.
	- Validate resource set ordering rules documented in `AGENTS.md` so the emitted SPIR-V matches SDL_Gpu expectations without manual descriptor remapping.
5. Build validation and regression harness
	- Extend `main.c` to write the generated SPIR-V to disk and run it through `spirv-val` when available, catching structural issues early.
	- Add focused unit snippets that exercise each newly supported feature (descriptor blocks, loops, texture operations) and diff the emitted words for stability.

## Sequencing rationale
- Getting module-level resources right first unblocks downstream tasks because every other feature (control flow, math) needs their ids and storage classes in place.
- Expression/codegen support directly unlocks meaningful shader logic, which is the highest-impact deliverable for developers trying to ship SDL_Gpu workloads.
- Control flow depends on the expression layer for condition ids, so it naturally follows once value emission exists.
- Stage interface wiring refines the entry point after instructions exist, ensuring the module links with pipelines across Vulkan, Metal, and D3D.
- Automated validation cements the improvements and keeps regressions from creeping in as we iterate on the emitter.
