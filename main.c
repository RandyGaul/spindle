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
	};

	for (int i = 0; i < (int)(sizeof(snippets) / sizeof(snippets[0])); ++i)
	{
		printf("=== %s ===\n", snippets[i].name);
		compile(snippets[i].stage, snippets[i].source);
		printf("\n");
	}

	return 0;
}
