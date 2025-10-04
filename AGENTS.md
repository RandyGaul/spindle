# Brief

We're making a transpiler type of project using ckit.h. It will take glsl 450 as input and convert it to either gles300 compatible shader, or a SPIRV blob. We're doing this to support indie game developers who want to utilize SDL3's new SDL_Gpu API. SDL_Gpu provides wrappers around Metal/Vulkan/DX11/DX12. All of these vendors have APIs to compile shaders from SPIRV blobs, which includes their own optimization passes.

This means as long as we can convert to either SPIRV or gles300 we cover all major platforms, including web cross-compiles. Games written in C/C++ can have their shaders written in plain glsl450 (a limited subset) with an enforced-scope of kinds of input shaders, and our tool here we're making will transpile to either SPIRV or gles300. This covers consoles via Vulkan/DX, Linux via Vulkan, Apple devices via Metal, and emscripten web pages via GLES3.

This transpiler tool fills a missing gap in cross-platform shader solutions in the open source/indie world, where the only alternatives are solutions like large engines or large code projects such as glslang (which compile extremely slowly, are difficult to integrate, and have very poor runtime characteristics with complex APIs to wrangle). Instead, we will provide a very small standalone C project here.

Codename for this project is "transpiler".

# Scoping and Concept

Transpiler will simply be like a "front end" compiler, except it will have scoped down input form expectations. Only basic glsl 450 shaders with vertex/fragment shaders are allowed. A single uniform buffer, input textures, control constructs like if/else/while/ternary are supported. Input types are simple and expected types such as int, vec2..4, matrix4. Builtin functions like dot, clamp, frac, etc. are all supported. However complex other types of shaders like geometry or tessselation shaders are just simply not supported, as we want to gracefully map back onto GLES3 shaders.

# Implementation

We're using ckit.h to power our C fundamentals with dynamic array (such as apush, apop, etc.), and a Map such as map_add, map_get, etc. We also have `sintern` for string interning to be used by the transpiler.

When interning compile-time string literals or any nul-terminated strings, call `sintern` directly. Reserve `sintern_range` for substrings that don't have an existing terminator.

We're just starting out the implementation with a lex/parse combination that implements a pratt expression parser by the lexer setting up function pointers and passing to a tiny generic pratt handler.

The next steps would be to flesh out IR format as we expect to emit to SPIRV blob or to gles300 shaders. We want 100% functional transpiling and don't really care about optimization. The vendors will compile our SPIRV or gles300 further onto the target GPU anyways, and have thier own optimization passes we can piggy back off of.

We allocate the symbol table, type system, and any related structures ourselves. Don't add defensive null-pointer checks for these systems or for allocations—assume allocation succeeds and the pointers are valid.

Strings passed around the compiler are assumed to be non-null. Don't add defensive null checks for them; if a null string shows up it's fine to crash.

# GLSL 340

Here's a simple vertex shader

```glsl
layout (location = 0) in vec2 in_pos;
layout (location = 1) in vec2 in_uv;
layout (location = 2) in vec4 in_col;

layout (location = 0) out vec2 v_uv;
layout (location = 1) out vec4 v_col;

void main()
{
    vec4 posH = vec4(in_pos, 0, 1);
    v_uv = in_uv;
    v_col = in_col;
    gl_Position = posH;
}
```

And here's a simple fragment shader

```glsl
layout (location = 0) in vec2 v_uv;
layout (location = 1) in vec4 v_col;

layout(location = 0) out vec4 result;

layout (set = 2, binding = 0) uniform sampler2D u_image;

layout (set = 3, binding = 0) uniform uniform_block {
    vec2 u_texture_size;
};

#include "smooth_uv.shd"

void main()
{
    vec4 c = de_gamma(texture(u_image, smooth_uv(v_uv, u_texture_size)));
    if (c.a == 0) discard;
    result = c;
}
```

We expect specific form of resource sets:

```glsl
/**
 * For _VERTEX_ shaders:
 *  0: Sampled textures, followed by storage textures, followed by storage buffers
 *  1: Uniform buffers
 * For _FRAGMENT_ shaders:
 *  2: Sampled textures, followed by storage textures, followed by storage buffers
 *  3: Uniform buffers
 * 
 * Example _VERTEX shader:
 * layout (set = 0, binding = 0) uniform sampler2D u_image;
 * 
 * layout (set = 1, binding = 0) uniform uniform_block {
 *     vec2 u_texture_size;
 * };
 * 
 * Example _FRAGMENT_ shader:
 * 
 * layout (set = 2, binding = 0) uniform sampler2D u_image;
 * 
 * layout (set = 3, binding = 0) uniform uniform_block {
 *     vec2 u_texture_size;
 * };
 */
```

These above are some examples of things we expect to be able to parse, not fully inclusive but just as starter examples for context.

# Format

We expect tabs as regular /t tabs no spaces, except when aligning text at the end of a line we use spaces. Just look at main.cpp for aesthetic style and make any new patches in the same style. For C functions do not use void in the parameter list just leave it empty.