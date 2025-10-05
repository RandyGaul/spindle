#define _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_DEPRECATE

#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>

#define CKIT_IMPLEMENTATION
#include "ckit.h"

#define SPINDLE_IMPLEMENTATION
#include "spindle.h"

void compile(ShaderStage stage, const char* source)
{
	printf("Input : %s\n\n", source);
	compiler_set_shader_stage(stage);
	compiler_setup(source);
	dump_ir();
	printf("\n");
	dump_symbols();
	compiler_teardown();
}

int main()
{
	unit_test();

	typedef struct ShaderSnippet
	{
		const char* name;
		ShaderStage stage;
		const char* source;
	} ShaderSnippet;

	const ShaderSnippet snippets[] = {
		{ "basic_io", SHADER_STAGE_FRAGMENT, snippet_basic_io },
		{ "control_flow", SHADER_STAGE_FRAGMENT, snippet_control_flow },
		{ "ternary_vectors", SHADER_STAGE_FRAGMENT, snippet_ternary_vectors },
		{ "array_indexing", SHADER_STAGE_FRAGMENT, snippet_array_indexing },
		{ "swizzle_usage", SHADER_STAGE_FRAGMENT, snippet_swizzle },
		{ "function_calls", SHADER_STAGE_FRAGMENT, snippet_function_calls },
		{ "matrix_ops", SHADER_STAGE_FRAGMENT, snippet_matrix_ops },
		{ "looping", SHADER_STAGE_FRAGMENT, snippet_looping },
		{ "bitwise", SHADER_STAGE_FRAGMENT, snippet_bitwise },
		{ "numeric_literals", SHADER_STAGE_FRAGMENT, snippet_numeric_literals },
		{ "discard", SHADER_STAGE_FRAGMENT, snippet_discard },
		{ "switch", SHADER_STAGE_FRAGMENT, snippet_switch_stmt },
		{ "builtin_funcs", SHADER_STAGE_FRAGMENT, snippet_builtin_funcs },
		{ "texture_queries", SHADER_STAGE_FRAGMENT, snippet_texture_queries },
		{ "extended_intrinsics", SHADER_STAGE_FRAGMENT, snippet_extended_intrinsics },
		{ "const_qualifier", SHADER_STAGE_FRAGMENT, snippet_const_qualifier },
		{ "resource_types", SHADER_STAGE_FRAGMENT, snippet_resource_types },
		{ "struct_block", SHADER_STAGE_FRAGMENT, snippet_struct_block },
		{ "struct_constructor", SHADER_STAGE_FRAGMENT, snippet_struct_constructor },
		{ "preprocessor_passthrough", SHADER_STAGE_FRAGMENT, snippet_preprocessor_passthrough },
		{ "extended_types", SHADER_STAGE_FRAGMENT, snippet_extended_types },
		{ "stage_builtins_fragment", SHADER_STAGE_FRAGMENT, snippet_stage_builtins_fragment },
		{ "stage_builtins_vertex", SHADER_STAGE_VERTEX, snippet_stage_builtins_vertex },
		{ "stage_builtins_compute", SHADER_STAGE_COMPUTE, snippet_stage_builtins_compute },
	};

	for (int i = 0; i < (int)(sizeof(snippets) / sizeof(snippets[0])); ++i)
	{
		printf("=== %s ===\n", snippets[i].name);
		compile(snippets[i].stage, snippets[i].source);
		printf("\n");
	}

	return 0;
}
