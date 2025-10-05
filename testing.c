void dump_storage_flags(unsigned flags)
{
	if (!flags)
		return;
	printf(" storage=");
	int first = 1;
	if (flags & SYM_STORAGE_IN)
	{
		printf("%sin", first ? "" : "|");
		first = 0;
	}
	if (flags & SYM_STORAGE_OUT)
	{
		printf("%sout", first ? "" : "|");
		first = 0;
	}
	if (flags & SYM_STORAGE_UNIFORM)
	{
		printf("%suniform", first ? "" : "|");
		first = 0;
	}
}

void dump_qualifier_flags(unsigned flags)
{
	if (!flags)
		return;
	printf(" qualifiers=");
	int first = 1;
	if (flags & SYM_QUAL_CONST)
	{
		printf("%sconst", first ? "" : "|");
		first = 0;
	}
}

void dump_layout_info(unsigned layout_flags, int set, int binding, int location)
{
	if (!layout_flags)
		return;
	printf(" layout(");
	int first = 1;
	if (layout_flags & SYM_LAYOUT_SET)
	{
		printf("%sset=%d", first ? "" : ", ", set);
		first = 0;
	}
	if (layout_flags & SYM_LAYOUT_BINDING)
	{
		printf("%sbinding=%d", first ? "" : ", ", binding);
		first = 0;
	}
	if (layout_flags & SYM_LAYOUT_LOCATION)
	{
		printf("%slocation=%d", first ? "" : ", ", location);
		first = 0;
	}
	printf(")");
}

void dump_ir()
{
	printf("IR:\n");
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		printf("  %s", ir_op_name[inst->op]);
		switch (inst->op)
		{
		case IR_DECL_BEGIN:
		case IR_FUNC_PARAM_BEGIN:
			dump_storage_flags(inst->storage_flags);
			dump_qualifier_flags(inst->qualifier_flags);
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
			break;
		case IR_PUSH_INT:
			printf(" %d", inst->arg0);
			break;
		case IR_PUSH_FLOAT:
			printf(" %g", inst->float_val);
			break;
		case IR_PUSH_BOOL:
			printf(" %s", inst->arg0 ? "true" : "false");
			break;
		case IR_PUSH_IDENT:
		case IR_MEMBER:
		case IR_DECL_TYPE:
		case IR_DECL_VAR:
		case IR_FUNC_PARAM_TYPE:
		case IR_FUNC_PARAM_NAME:
			printf(" %s", inst->str0);
			dump_qualifier_flags(inst->qualifier_flags);
			break;
		case IR_SWIZZLE:
			printf(" %s count=%d mask=0x%x", inst->str0, inst->arg0, inst->arg1);
			break;
		case IR_UNARY:
		case IR_BINARY:
			printf(" %s", tok_name[inst->tok]);
			break;
		case IR_CALL:
			printf(" argc=%d", inst->arg0);
			break;
		case IR_CONSTRUCT:
			printf(" type=%s argc=%d", inst->str0 ? inst->str0 : "<null>", inst->arg0);
			break;
		case IR_FUNC_BEGIN:
			printf(" return=%s name=%s", inst->str0, inst->str1);
			dump_storage_flags(inst->storage_flags);
			dump_qualifier_flags(inst->qualifier_flags);
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
			break;
		case IR_STRUCT_BEGIN:
			printf(" name=%s", inst->str0 ? inst->str0 : "<anon>");
			break;
		case IR_STRUCT_MEMBER:
			printf(" name=%s type=%s", inst->str0 ? inst->str0 : "<anon>", inst->str1 ? inst->str1 : "<anon>");
			if (inst->arg0)
				printf(" array_len=%d", inst->arg0);
			dump_qualifier_flags(inst->qualifier_flags);
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
			break;
		case IR_STRUCT_END:
			break;
		case IR_BLOCK_DECL_BEGIN:
			printf(" type=%s", inst->str0 ? inst->str0 : "<anon>");
			dump_storage_flags(inst->storage_flags);
			dump_qualifier_flags(inst->qualifier_flags);
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
			break;
		case IR_BLOCK_DECL_LAYOUT:
			printf(" %s", inst->str0 ? inst->str0 : "<anon>");
			break;
		case IR_BLOCK_DECL_MEMBER:
			printf(" name=%s type=%s", inst->str0 ? inst->str0 : "<anon>", inst->str1 ? inst->str1 : "<anon>");
			if (inst->arg0)
				printf(" array_len=%d", inst->arg0);
			dump_qualifier_flags(inst->qualifier_flags);
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
			break;
		case IR_BLOCK_DECL_INSTANCE:
			printf(" %s", inst->str0 ? inst->str0 : "<anon>");
			break;
		case IR_BLOCK_DECL_END:
			break;
		case IR_RETURN:
			printf(" has_value=%d", inst->arg0);
			break;
		case IR_BREAK:
		case IR_CONTINUE:
		case IR_DISCARD:
			break;
		default:
			break;
		}
		printf("\n");
	}
}

void dump_symbols()
{
	printf("Symbols:\n");
	for (int i = 0; i < acount(st->symbols); ++i)
	{
		const Symbol* sym = &st->symbols[i];
		printf("  scope[%d] %s %s : %s", sym->scope_depth, symbol_kind_name[sym->kind], sym->name, sym->type_name);
		if (sym->type)
		{
			printf(" (tag=%s)", type_tag_name(sym->type->tag));
		}
		dump_storage_flags(sym->storage_flags);
		dump_qualifier_flags(sym->qualifier_flags);
		dump_layout_info(sym->layout_flags, sym->layout_set, sym->layout_binding, sym->layout_location);
		printf("\n");
	}
}

typedef void (*TestFunc)(void);

typedef struct TestCase
{
	const char* name;
	TestFunc func;
} TestCase;

#define DEFINE_TEST(name) static void name(void)
#define TEST_ENTRY(name) { #name, name }

static void run_test_case(const TestCase* test)
{
	printf("[test] %s\n", test->name);
	test->func();
}

static void run_tests(const TestCase* tests, int count)
{
	for (int i = 0; i < count; ++i)
	{
		run_test_case(&tests[i]);
	}
	printf("[test] %d tests passed\n", count);
}

DEFINE_TEST(test_type_system_registration)
{
	type_system_init_builtins();
	const char* float_name = sintern("float");
	Type* float_type = type_system_get(float_name);
	assert(float_type && float_type->tag == T_FLOAT);
	Type* vec4_type = type_system_get(sintern("vec4"));
	assert(vec4_type && vec4_type->tag == T_VEC && vec4_type->cols == 4);
	Type custom_type = (Type){ 0 };
	custom_type.tag = T_STRUCT;
	custom_type.cols = 1;
	custom_type.rows = 1;
	custom_type.base = T_FLOAT;
	custom_type.array_len = 0;
	const char* custom_name = sintern("test_struct");
	Type* declared_type = type_system_add_internal(custom_name, custom_type);
	assert(declared_type == type_system_get(custom_name));
	type_system_free();
}

DEFINE_TEST(test_symbol_table_scopes)
{
	type_system_init_builtins();
	symbol_table_init();
	Type int_type = (Type){ 0 };
	int_type.tag = T_INT;
	const char* value_name = sintern("value");
	Symbol* value_sym = symbol_table_add(value_name, sintern("int"), &int_type, SYM_VAR);
	assert(value_sym && value_sym->name == value_name);
	symbol_add_storage(value_sym, SYM_STORAGE_IN);
	assert(symbol_has_storage(value_sym, SYM_STORAGE_IN));
	symbol_table_enter_scope();
	Type float_type_local = (Type){ 0 };
	float_type_local.tag = T_FLOAT;
	const char* inner_name = sintern("inner_value");
	Symbol* inner_sym = symbol_table_add(inner_name, sintern("float"), &float_type_local, SYM_VAR);
	symbol_set_layout(inner_sym, SYM_LAYOUT_LOCATION, 3);
	assert(symbol_get_layout(inner_sym, SYM_LAYOUT_LOCATION) == 3);
	assert(symbol_table_find(inner_name) == inner_sym);
	symbol_table_leave_scope();
	assert(symbol_table_find(inner_name) == NULL);
	assert(symbol_table_find(value_name) == value_sym);
	const char* texture_name = sintern("texture");
	Symbol* texture_sym = symbol_table_find(texture_name);
	assert(texture_sym && texture_sym->builtin_kind == BUILTIN_TEXTURE);
	const char* frac_name = sintern("frac");
	Symbol* frac_sym = symbol_table_find(frac_name);
	assert(frac_sym && frac_sym->builtin_kind == BUILTIN_FRACT);
	symbol_table_free();
	type_system_free();
}

DEFINE_TEST(test_builtin_function_metadata)
{
	type_system_init_builtins();
	symbol_table_init();
	const struct
	{
		const char* name;
		BuiltinFuncKind kind;
		int params;
	} cases[] = {
		{ sintern("textureLod"), BUILTIN_TEXTURE_LOD, 3 },
		{ sintern("textureGrad"), BUILTIN_TEXTURE_GRAD, 4 },
		{ sintern("normalize"), BUILTIN_NORMALIZE, 1 },
		{ sintern("distance"), BUILTIN_DISTANCE, 2 },
	};
	for (int i = 0; i < (int)(sizeof(cases) / sizeof(cases[0])); ++i)
	{
		Symbol* sym = symbol_table_find(cases[i].name);
		assert(sym);
		assert(sym->builtin_kind == cases[i].kind);
		assert(sym->builtin_param_count == cases[i].params);
	}
	symbol_table_free();
	type_system_free();
}

DEFINE_TEST(test_resource_type_registration)
{
	type_system_init_builtins();
	const struct
	{
		const char* name;
		TypeTag tag;
		TypeTag base;
		uint8_t dim;
	} cases[] = {
		{ "sampler1D", T_SAMPLER, T_FLOAT, TYPE_DIM_1D },
		{ "sampler2D", T_SAMPLER, T_FLOAT, TYPE_DIM_2D },
		{ "sampler3D", T_SAMPLER, T_FLOAT, TYPE_DIM_3D },
		{ "samplerCube", T_SAMPLER, T_FLOAT, TYPE_DIM_CUBE },
		{ "sampler1DShadow", T_SAMPLER, T_FLOAT, TYPE_DIM_1D | TYPE_DIM_FLAG_SHADOW },
		{ "sampler2DShadow", T_SAMPLER, T_FLOAT, TYPE_DIM_2D | TYPE_DIM_FLAG_SHADOW },
		{ "samplerCubeShadow", T_SAMPLER, T_FLOAT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_SHADOW },
		{ "sampler1DArray", T_SAMPLER, T_FLOAT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY },
		{ "sampler2DArray", T_SAMPLER, T_FLOAT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY },
		{ "sampler1DArrayShadow", T_SAMPLER, T_FLOAT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY | TYPE_DIM_FLAG_SHADOW },
		{ "sampler2DArrayShadow", T_SAMPLER, T_FLOAT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY | TYPE_DIM_FLAG_SHADOW },
		{ "samplerCubeArray", T_SAMPLER, T_FLOAT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY },
		{ "samplerCubeArrayShadow", T_SAMPLER, T_FLOAT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY | TYPE_DIM_FLAG_SHADOW },
		{ "sampler2DMS", T_SAMPLER, T_FLOAT, TYPE_DIM_2D_MS },
		{ "sampler2DMSArray", T_SAMPLER, T_FLOAT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY },
		{ "samplerBuffer", T_SAMPLER, T_FLOAT, TYPE_DIM_BUFFER },
		{ "sampler2DRect", T_SAMPLER, T_FLOAT, TYPE_DIM_RECT },
		{ "sampler2DRectShadow", T_SAMPLER, T_FLOAT, TYPE_DIM_RECT | TYPE_DIM_FLAG_SHADOW },
		{ "isampler1D", T_SAMPLER, T_INT, TYPE_DIM_1D },
		{ "isampler2D", T_SAMPLER, T_INT, TYPE_DIM_2D },
		{ "isampler3D", T_SAMPLER, T_INT, TYPE_DIM_3D },
		{ "isamplerCube", T_SAMPLER, T_INT, TYPE_DIM_CUBE },
		{ "isampler1DArray", T_SAMPLER, T_INT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY },
		{ "isampler2DArray", T_SAMPLER, T_INT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY },
		{ "isamplerCubeArray", T_SAMPLER, T_INT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY },
		{ "isampler2DMS", T_SAMPLER, T_INT, TYPE_DIM_2D_MS },
		{ "isampler2DMSArray", T_SAMPLER, T_INT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY },
		{ "isamplerBuffer", T_SAMPLER, T_INT, TYPE_DIM_BUFFER },
		{ "isampler2DRect", T_SAMPLER, T_INT, TYPE_DIM_RECT },
		{ "usampler1D", T_SAMPLER, T_UINT, TYPE_DIM_1D },
		{ "usampler2D", T_SAMPLER, T_UINT, TYPE_DIM_2D },
		{ "usampler3D", T_SAMPLER, T_UINT, TYPE_DIM_3D },
		{ "usamplerCube", T_SAMPLER, T_UINT, TYPE_DIM_CUBE },
		{ "usampler1DArray", T_SAMPLER, T_UINT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY },
		{ "usampler2DArray", T_SAMPLER, T_UINT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY },
		{ "usamplerCubeArray", T_SAMPLER, T_UINT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY },
		{ "usampler2DMS", T_SAMPLER, T_UINT, TYPE_DIM_2D_MS },
		{ "usampler2DMSArray", T_SAMPLER, T_UINT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY },
		{ "usamplerBuffer", T_SAMPLER, T_UINT, TYPE_DIM_BUFFER },
		{ "usampler2DRect", T_SAMPLER, T_UINT, TYPE_DIM_RECT },
		{ "image1D", T_IMAGE, T_FLOAT, TYPE_DIM_1D },
		{ "image2D", T_IMAGE, T_FLOAT, TYPE_DIM_2D },
		{ "image3D", T_IMAGE, T_FLOAT, TYPE_DIM_3D },
		{ "imageCube", T_IMAGE, T_FLOAT, TYPE_DIM_CUBE },
		{ "imageBuffer", T_IMAGE, T_FLOAT, TYPE_DIM_BUFFER },
		{ "image1DArray", T_IMAGE, T_FLOAT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY },
		{ "image2DArray", T_IMAGE, T_FLOAT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY },
		{ "imageCubeArray", T_IMAGE, T_FLOAT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY },
		{ "image2DMS", T_IMAGE, T_FLOAT, TYPE_DIM_2D_MS },
		{ "image2DMSArray", T_IMAGE, T_FLOAT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY },
		{ "image2DRect", T_IMAGE, T_FLOAT, TYPE_DIM_RECT },
		{ "iimage1D", T_IMAGE, T_INT, TYPE_DIM_1D },
		{ "iimage2D", T_IMAGE, T_INT, TYPE_DIM_2D },
		{ "iimage3D", T_IMAGE, T_INT, TYPE_DIM_3D },
		{ "iimageCube", T_IMAGE, T_INT, TYPE_DIM_CUBE },
		{ "iimageBuffer", T_IMAGE, T_INT, TYPE_DIM_BUFFER },
		{ "iimage1DArray", T_IMAGE, T_INT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY },
		{ "iimage2DArray", T_IMAGE, T_INT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY },
		{ "iimageCubeArray", T_IMAGE, T_INT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY },
		{ "iimage2DMS", T_IMAGE, T_INT, TYPE_DIM_2D_MS },
		{ "iimage2DMSArray", T_IMAGE, T_INT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY },
		{ "iimage2DRect", T_IMAGE, T_INT, TYPE_DIM_RECT },
		{ "uimage1D", T_IMAGE, T_UINT, TYPE_DIM_1D },
		{ "uimage2D", T_IMAGE, T_UINT, TYPE_DIM_2D },
		{ "uimage3D", T_IMAGE, T_UINT, TYPE_DIM_3D },
		{ "uimageCube", T_IMAGE, T_UINT, TYPE_DIM_CUBE },
		{ "uimageBuffer", T_IMAGE, T_UINT, TYPE_DIM_BUFFER },
		{ "uimage1DArray", T_IMAGE, T_UINT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY },
		{ "uimage2DArray", T_IMAGE, T_UINT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY },
		{ "uimageCubeArray", T_IMAGE, T_UINT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY },
		{ "uimage2DMS", T_IMAGE, T_UINT, TYPE_DIM_2D_MS },
		{ "uimage2DMSArray", T_IMAGE, T_UINT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY },
		{ "uimage2DRect", T_IMAGE, T_UINT, TYPE_DIM_RECT },
	};
	for (int i = 0; i < (int)(sizeof(cases) / sizeof(cases[0])); ++i)
	{
		const char* name = sintern(cases[i].name);
		Type* type = type_system_get(name);
		assert(type);
		assert(type->tag == cases[i].tag);
		assert(type->base == cases[i].base);
		assert(type->dim == cases[i].dim);
	}
	type_system_free();
}

DEFINE_TEST(test_resource_texture_inference)
{
	compiler_setup(snippet_resource_types);
	const char* texture_name = sintern("texture");
	Type* float_vec4 = type_get_vector(T_FLOAT, 4);
	Type* int_vec4 = type_get_vector(T_INT, 4);
	Type* uint_vec4 = type_get_vector(T_UINT, 4);
	Type* float_scalar = type_get_scalar(T_FLOAT);
	int saw_float_vec = 0;
	int saw_shadow = 0;
	int saw_int_vec = 0;
	int saw_uint_vec = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op != IR_CALL || inst->str0 != texture_name)
			continue;
		if (inst->type == float_vec4)
			saw_float_vec = 1;
		else if (inst->type == float_scalar)
			saw_shadow = 1;
		else if (inst->type == int_vec4)
			saw_int_vec = 1;
		else if (inst->type == uint_vec4)
			saw_uint_vec = 1;
	}
	compiler_teardown();
	assert(saw_float_vec);
	assert(saw_shadow);
	assert(saw_int_vec);
	assert(saw_uint_vec);
}

DEFINE_TEST(test_ir_emit_push_int)
{
	IR_Cmd* saved_ir = g_ir;
	g_ir = NULL;
	IR_Cmd* emitted = ir_emit(IR_PUSH_INT);
	assert(emitted);
	assert(acount(g_ir) == 1);
	assert(g_ir[0].op == IR_PUSH_INT);
	afree(g_ir);
	g_ir = saved_ir;
}

DEFINE_TEST(test_basic_io_symbols)
{
	const char* u_texture = sintern("u_texture");
	const char* u_tint = sintern("u_tint");
	const char* out_color = sintern("out_color");
	compiler_setup(snippet_basic_io);
	Symbol* sampler_sym = symbol_table_find(u_texture);
	assert(sampler_sym);
	assert(symbol_has_storage(sampler_sym, SYM_STORAGE_UNIFORM));
	assert(symbol_get_layout(sampler_sym, SYM_LAYOUT_SET) == 0);
	assert(symbol_get_layout(sampler_sym, SYM_LAYOUT_BINDING) == 0);
	Symbol* tint_sym = symbol_table_find(u_tint);
	assert(tint_sym);
	assert(symbol_has_storage(tint_sym, SYM_STORAGE_UNIFORM));
	assert(symbol_get_layout(tint_sym, SYM_LAYOUT_SET) == 1);
	assert(symbol_get_layout(tint_sym, SYM_LAYOUT_BINDING) == 0);
	Symbol* out_sym = symbol_table_find(out_color);
	assert(out_sym);
	assert(symbol_has_storage(out_sym, SYM_STORAGE_OUT));
	assert(symbol_get_layout(out_sym, SYM_LAYOUT_LOCATION) == 0);
	compiler_teardown();
}

DEFINE_TEST(test_array_indexing_ir)
{
	compiler_setup(snippet_array_indexing);
	int saw_index = 0;
	int saw_float_index = 0;
	int saw_int_index = 0;
	int saw_uint_index = 0;
	int saw_bool_index = 0;
	int saw_vec_index = 0;
	int saw_mat_index = 0;
	int saw_bool_literal = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		if (g_ir[i].op == IR_PUSH_BOOL)
		{
			saw_bool_literal = 1;
		}
		if (g_ir[i].op != IR_INDEX)
			continue;
		saw_index = 1;
		Type* type = g_ir[i].type;
		if (!type)
			continue;
		if (type_is_scalar(type))
		{
			switch (type_base_type(type))
			{
			case T_FLOAT:
				saw_float_index = 1;
				break;
			case T_INT:
				saw_int_index = 1;
				break;
			case T_UINT:
				saw_uint_index = 1;
				break;
			case T_BOOL:
				saw_bool_index = 1;
				break;
			default:
				break;
			}
		}
		if (type_is_vector(type))
			saw_vec_index = 1;
		if (type_is_matrix(type))
			saw_mat_index = 1;
	}
	compiler_teardown();
	assert(saw_index);
	assert(saw_float_index);
	assert(saw_int_index);
	assert(saw_uint_index);
	assert(saw_bool_index);
	assert(saw_bool_literal);
	assert(saw_vec_index);
	assert(saw_mat_index);
}

DEFINE_TEST(test_swizzle_ir)
{
	compiler_setup(snippet_swizzle);
	int saw_swizzle[5] = { 0 };
	for (int i = 0; i < acount(g_ir); ++i)
	{
		if (g_ir[i].op != IR_SWIZZLE)
			continue;
		int count = g_ir[i].arg0;
		if (count >= 1 && count <= 4)
			saw_swizzle[count] = 1;
	}
	compiler_teardown();
	assert(saw_swizzle[1]);
	assert(saw_swizzle[2]);
	assert(saw_swizzle[3]);
	assert(saw_swizzle[4]);
}

DEFINE_TEST(test_control_flow_unary_ops)
{
	compiler_setup(snippet_control_flow);
	int saw_pre_inc = 0;
	int saw_post_inc = 0;
	int saw_pre_dec = 0;
	int saw_post_dec = 0;
	int saw_pre_inc_lvalue = 0;
	int saw_post_inc_lvalue = 0;
	int saw_pre_dec_lvalue = 0;
	int saw_post_dec_lvalue = 0;
	int saw_for_begin = 0;
	int saw_if_begin = 0;
	int saw_if_else = 0;
	int saw_if_end = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op == IR_FOR_BEGIN)
			saw_for_begin = 1;
		if (inst->op == IR_IF_BEGIN)
			saw_if_begin = 1;
		if (inst->op == IR_IF_ELSE)
			saw_if_else = 1;
		if (inst->op == IR_IF_END)
			saw_if_end = 1;
		if (inst->op != IR_UNARY)
			continue;
		if (inst->tok == TOK_PLUS_PLUS)
		{
			if (inst->arg1 == 0)
			{
				saw_pre_inc = 1;
				if (inst->arg0 >= 0 && inst->arg0 < acount(g_ir))
					saw_pre_inc_lvalue = g_ir[inst->arg0].is_lvalue;
			}
			else if (inst->arg1 == 1)
			{
				saw_post_inc = 1;
				if (inst->arg0 >= 0 && inst->arg0 < acount(g_ir))
					saw_post_inc_lvalue = g_ir[inst->arg0].is_lvalue;
			}
		}
		else if (inst->tok == TOK_MINUS_MINUS)
		{
			if (inst->arg1 == 0)
			{
				saw_pre_dec = 1;
				if (inst->arg0 >= 0 && inst->arg0 < acount(g_ir))
					saw_pre_dec_lvalue = g_ir[inst->arg0].is_lvalue;
			}
			else if (inst->arg1 == 1)
			{
				saw_post_dec = 1;
				if (inst->arg0 >= 0 && inst->arg0 < acount(g_ir))
					saw_post_dec_lvalue = g_ir[inst->arg0].is_lvalue;
			}
		}
	}
	compiler_teardown();
	assert(saw_pre_inc);
	assert(saw_post_inc);
	assert(saw_pre_dec);
	assert(saw_post_dec);
	assert(saw_pre_inc_lvalue);
	assert(saw_post_inc_lvalue);
	assert(saw_pre_dec_lvalue);
	assert(saw_post_dec_lvalue);
	assert(saw_for_begin);
	assert(saw_if_begin);
	assert(saw_if_else);
	assert(saw_if_end);
}

DEFINE_TEST(test_function_call_symbols)
{
	const char* saturate = sintern("saturate");
	const char* apply_gain = sintern("apply_gain");
	const char* main_name = sintern("main");
	compiler_setup(snippet_function_calls);
	int saw_saturate_symbol = 0;
	int saw_apply_gain_symbol = 0;
	int saw_main_symbol = 0;
	int saw_apply_gain_call = 0;
	for (int i = 0; i < acount(st->symbols); ++i)
	{
		Symbol* sym = &st->symbols[i];
		if (sym->name == saturate)
		{
			saw_saturate_symbol = sym->kind == SYM_FUNC;
			assert(sym->type && sym->type->tag == T_FLOAT);
		}
		if (sym->name == apply_gain)
		{
			saw_apply_gain_symbol = sym->kind == SYM_FUNC;
			assert(sym->type && sym->type->tag == T_VEC);
		}
		if (sym->name == main_name)
		{
			saw_main_symbol = sym->kind == SYM_FUNC;
		}
	}
	for (int i = 0; i < acount(g_ir); ++i)
	{
		if (g_ir[i].op == IR_CALL && g_ir[i].str0 == apply_gain)
		{
			saw_apply_gain_call = 1;
			break;
		}
	}
	compiler_teardown();
	assert(saw_saturate_symbol);
	assert(saw_apply_gain_symbol);
	assert(saw_main_symbol);
	assert(saw_apply_gain_call);
}

DEFINE_TEST(test_matrix_operations_ir)
{
	const char* mat3_name = sintern("mat3");
	const char* rect_a_name = sintern("rect_a");
	const char* rect_b_name = sintern("rect_b");
	const char* weights_name = sintern("weights");
	const char* row_combo_name = sintern("row_combo");
	compiler_setup(snippet_matrix_ops);
	Type* vec3_type = type_get_vector(T_FLOAT, 3);
	Type* vec2_type = type_get_vector(T_FLOAT, 2);
	Type* mat3_type = type_get_matrix(T_FLOAT, 3, 3);
	int saw_mat_ctor = 0;
	int index_count = 0;
	int saw_matrix_vector = 0;
	int saw_vector_matrix = 0;
	int saw_rectangular_matrix = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		if (g_ir[i].op == IR_CONSTRUCT && g_ir[i].str0 == mat3_name)
			saw_mat_ctor = 1;
		if (g_ir[i].op == IR_INDEX)
			index_count++;
		if (g_ir[i].op == IR_BINARY && g_ir[i].tok == TOK_STAR)
		{
			IR_Cmd* lhs = NULL;
			IR_Cmd* rhs = NULL;
			for (int j = i - 1; j >= 0 && (!lhs || !rhs); --j)
			{
				if (g_ir[j].op != IR_PUSH_IDENT)
					continue;
				if (!rhs)
				{
					rhs = &g_ir[j];
				}
				else if (!lhs)
				{
					lhs = &g_ir[j];
				}
			}
			if (g_ir[i].type == vec3_type && lhs && rhs && lhs->op == IR_PUSH_IDENT && rhs->op == IR_PUSH_IDENT && lhs->str0 == rect_a_name && rhs->str0 == weights_name)
				saw_matrix_vector = 1;
			if (g_ir[i].type == vec2_type && lhs && rhs && lhs->op == IR_PUSH_IDENT && rhs->op == IR_PUSH_IDENT && lhs->str0 == row_combo_name && rhs->str0 == rect_a_name)
				saw_vector_matrix = 1;
			if (g_ir[i].type == mat3_type && lhs && rhs && lhs->op == IR_PUSH_IDENT && rhs->op == IR_PUSH_IDENT && lhs->str0 == rect_a_name && rhs->str0 == rect_b_name)
				saw_rectangular_matrix = 1;
		}
	}
	compiler_teardown();
	assert(saw_mat_ctor);
	assert(index_count >= 2);
	assert(saw_matrix_vector);
	assert(saw_vector_matrix);
	assert(saw_rectangular_matrix);
}

DEFINE_TEST(test_looping_constructs)
{
	compiler_setup(snippet_looping);
	int saw_while_begin = 0;
	int saw_while_end = 0;
	int saw_do_begin = 0;
	int saw_do_end = 0;
	int saw_continue = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		if (g_ir[i].op == IR_WHILE_BEGIN)
			saw_while_begin = 1;
		if (g_ir[i].op == IR_WHILE_END)
			saw_while_end = 1;
		if (g_ir[i].op == IR_DO_BEGIN)
			saw_do_begin = 1;
		if (g_ir[i].op == IR_DO_END)
			saw_do_end = 1;
		if (g_ir[i].op == IR_CONTINUE)
			saw_continue = 1;
	}
	compiler_teardown();
	assert(saw_while_begin);
	assert(saw_while_end);
	assert(saw_do_begin);
	assert(saw_do_end);
	assert(saw_continue);
}

DEFINE_TEST(test_bitwise_operations)
{
	compiler_setup(snippet_bitwise);
	int saw_band = 0;
	int saw_bor = 0;
	int saw_bxor = 0;
	int saw_shl = 0;
	int saw_shr = 0;
	int saw_band_assign = 0;
	int saw_bor_assign = 0;
	int saw_bxor_assign = 0;
	int saw_shl_assign = 0;
	int saw_shr_assign = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op != IR_BINARY)
			continue;
		switch (inst->tok)
		{
		case TOK_AMP:
			saw_band = 1;
			break;
		case TOK_PIPE:
			saw_bor = 1;
			break;
		case TOK_CARET:
			saw_bxor = 1;
			break;
		case TOK_LSHIFT:
			saw_shl = 1;
			break;
		case TOK_RSHIFT:
			saw_shr = 1;
			break;
		case TOK_AND_ASSIGN:
			saw_band_assign = 1;
			break;
		case TOK_OR_ASSIGN:
			saw_bor_assign = 1;
			break;
		case TOK_XOR_ASSIGN:
			saw_bxor_assign = 1;
			break;
		case TOK_LSHIFT_ASSIGN:
			saw_shl_assign = 1;
			break;
		case TOK_RSHIFT_ASSIGN:
			saw_shr_assign = 1;
			break;
		default:
			break;
		}
	}
	compiler_teardown();
	assert(saw_band);
	assert(saw_bor);
	assert(saw_bxor);
	assert(saw_shl);
	assert(saw_shr);
	assert(saw_band_assign);
	assert(saw_bor_assign);
	assert(saw_bxor_assign);
	assert(saw_shl_assign);
	assert(saw_shr_assign);
}

DEFINE_TEST(test_numeric_literal_bases)
{
	compiler_setup(snippet_numeric_literals);
	int saw_hex = 0;
	int saw_bin = 0;
	int saw_oct = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op != IR_PUSH_INT)
			continue;
		if (inst->arg0 == 31)
		{
			saw_hex = 1;
		}
		else if (inst->arg0 == 10)
		{
			saw_bin = 1;
		}
		else if (inst->arg0 == 61)
		{
			saw_oct = 1;
		}
	}
	compiler_teardown();
	assert(saw_hex);
	assert(saw_bin);
	assert(saw_oct);
}

DEFINE_TEST(test_discard_instruction)
{
	compiler_setup(snippet_discard);
	int saw_discard = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		if (g_ir[i].op == IR_DISCARD)
		{
			saw_discard = 1;
			break;
		}
	}
	compiler_teardown();
	assert(saw_discard);
}

DEFINE_TEST(test_struct_block_layout)
{
	const char* light_struct = sintern("Light");
	const char* block_name = sintern("LightBlock");
	const char* instance_name = sintern("u_light_data");
	const char* member_name = sintern("lights");
	const char* std140_name = sintern("std140");
	compiler_setup(snippet_struct_block);
	int saw_struct = 0;
	int saw_block = 0;
	int saw_block_layout_identifier = 0;
	int saw_block_instance = 0;
	int saw_block_member_array = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op == IR_STRUCT_BEGIN && inst->str0 == light_struct)
		{
			saw_struct = 1;
		}
		if (inst->op == IR_BLOCK_DECL_BEGIN && inst->str0 == block_name)
		{
			saw_block = (inst->storage_flags & SYM_STORAGE_UNIFORM) &&
					(inst->layout_flags & SYM_LAYOUT_SET) && (inst->layout_flags & SYM_LAYOUT_BINDING) &&
					inst->layout_set == 1 && inst->layout_binding == 0;
		}
		if (inst->op == IR_BLOCK_DECL_LAYOUT && inst->str0 == std140_name)
		{
			saw_block_layout_identifier = 1;
		}
		if (inst->op == IR_BLOCK_DECL_INSTANCE && inst->str0 == instance_name)
		{
			saw_block_instance = 1;
		}
		if (inst->op == IR_BLOCK_DECL_MEMBER && inst->str0 == member_name && inst->arg0 == 2)
		{
			saw_block_member_array = 1;
		}
	}
	compiler_teardown();
	assert(saw_struct);
	assert(saw_block);
	assert(saw_block_layout_identifier);
	assert(saw_block_instance);
	assert(saw_block_member_array);
}

DEFINE_TEST(test_switch_statement_cases)
{
	compiler_setup(snippet_switch_stmt);
	int saw_switch_begin = 0;
	int saw_switch_end = 0;
	int saw_default_case = 0;
	int saw_fallthrough = 0;
	int case_count = 0;
	int case_zero = 0;
	int case_one = 0;
	int case_two = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op == IR_SWITCH_BEGIN)
			saw_switch_begin = 1;
		if (inst->op == IR_SWITCH_END)
			saw_switch_end = 1;
		if (inst->op != IR_SWITCH_CASE)
			continue;
		case_count++;
		if (inst->arg1 & SWITCH_CASE_FLAG_DEFAULT)
			saw_default_case = 1;
		if (inst->arg1 & SWITCH_CASE_FLAG_FALLTHROUGH)
			saw_fallthrough = 1;
		if (inst->arg0 == 0)
			case_zero = 1;
		if (inst->arg0 == 1)
			case_one = 1;
		if (inst->arg0 == 2)
			case_two = 1;
	}
	compiler_teardown();
	assert(saw_switch_begin);
	assert(saw_switch_end);
	assert(case_count == 4);
	assert(case_zero);
	assert(case_one);
	assert(case_two);
	assert(saw_default_case);
	assert(saw_fallthrough);
}

DEFINE_TEST(test_builtin_function_calls)
{
	const char* texture_name = sintern("texture");
	const char* max_name = sintern("max");
	const char* frac_name = sintern("frac");
	const char* dot_name = sintern("dot");
	const char* normalize_name = sintern("normalize");
	compiler_setup(snippet_builtin_funcs);
	int saw_texture = 0;
	int saw_max = 0;
	int saw_frac = 0;
	int saw_dot = 0;
	int saw_normalize = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op != IR_CALL)
			continue;
		if (inst->str0 == texture_name)
			saw_texture = 1;
		if (inst->str0 == max_name)
			saw_max = 1;
		if (inst->str0 == frac_name)
			saw_frac = 1;
		if (inst->str0 == dot_name)
			saw_dot = 1;
		if (inst->str0 == normalize_name)
			saw_normalize = 1;
	}
	compiler_teardown();
	assert(saw_texture);
	assert(saw_max);
	assert(saw_frac);
	assert(saw_dot);
	assert(saw_normalize);
}
DEFINE_TEST(test_preprocessor_passthrough)
{
	const char* out_color = sintern("out_color");
	const char* unused_constant = sintern("UNUSED_CONSTANT");
	compiler_setup(snippet_preprocessor_passthrough);
	Symbol* out_sym = symbol_table_find(out_color);
	assert(out_sym);
	assert(symbol_has_storage(out_sym, SYM_STORAGE_OUT));
	Symbol* macro_sym = symbol_table_find(unused_constant);
	assert(!macro_sym);
	int saw_func = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		if (g_ir[i].op == IR_FUNC_BEGIN)
		{
			saw_func = 1;
			break;
		}
	}
	assert(saw_func);
	compiler_teardown();
}

DEFINE_TEST(test_const_qualifier_metadata)
{
	const char* factor = sintern("factor");
	const char* mutable_val = sintern("mutable_val");
	compiler_setup(snippet_const_qualifier);
	int saw_const_symbol = 0;
	int saw_mutable_symbol = 0;
	for (int i = 0; i < acount(st->symbols); ++i)
	{
		Symbol* sym = &st->symbols[i];
		if (sym->name == factor)
		{
			saw_const_symbol = (sym->qualifier_flags & SYM_QUAL_CONST) != 0;
		}
		if (sym->name == mutable_val)
		{
			saw_mutable_symbol = (sym->qualifier_flags & SYM_QUAL_CONST) != 0;
		}
	}
	int saw_const_decl = 0;
	int saw_mutable_decl = 0;
	int saw_const_ident = 0;
	int saw_const_lvalue = 0;
	int saw_mutable_ident = 0;
	int saw_mutable_lvalue = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op == IR_DECL_VAR && inst->str0 == factor)
		{
			saw_const_decl = (inst->qualifier_flags & SYM_QUAL_CONST) != 0;
		}
		if (inst->op == IR_DECL_VAR && inst->str0 == mutable_val)
		{
			saw_mutable_decl = (inst->qualifier_flags & SYM_QUAL_CONST) != 0;
		}
		if (inst->op == IR_PUSH_IDENT && inst->str0 == factor)
		{
			saw_const_ident = (inst->qualifier_flags & SYM_QUAL_CONST) != 0;
			saw_const_lvalue |= inst->is_lvalue;
		}
		if (inst->op == IR_PUSH_IDENT && inst->str0 == mutable_val)
		{
			saw_mutable_ident = (inst->qualifier_flags & SYM_QUAL_CONST) != 0;
			saw_mutable_lvalue |= inst->is_lvalue;
		}
	}
	compiler_teardown();
	assert(saw_const_symbol);
	assert(!saw_mutable_symbol);
	assert(saw_const_decl);
	assert(!saw_mutable_decl);
	assert(saw_const_ident);
	assert(!saw_const_lvalue);
	assert(!saw_mutable_ident);
	assert(saw_mutable_lvalue);
}

void unit_test()
{
	init_keyword_interns();
	const TestCase tests[] = {
		TEST_ENTRY(test_type_system_registration),
		TEST_ENTRY(test_symbol_table_scopes),
		TEST_ENTRY(test_builtin_function_metadata),
		TEST_ENTRY(test_resource_type_registration),
		TEST_ENTRY(test_ir_emit_push_int),
		TEST_ENTRY(test_basic_io_symbols),
		TEST_ENTRY(test_array_indexing_ir),
		TEST_ENTRY(test_swizzle_ir),
		TEST_ENTRY(test_control_flow_unary_ops),
		TEST_ENTRY(test_function_call_symbols),
		TEST_ENTRY(test_matrix_operations_ir),
		TEST_ENTRY(test_looping_constructs),
		TEST_ENTRY(test_bitwise_operations),
		TEST_ENTRY(test_numeric_literal_bases),
		TEST_ENTRY(test_discard_instruction),
		TEST_ENTRY(test_struct_block_layout),
		TEST_ENTRY(test_switch_statement_cases),
		TEST_ENTRY(test_builtin_function_calls),
		TEST_ENTRY(test_preprocessor_passthrough),
		TEST_ENTRY(test_resource_texture_inference),
		TEST_ENTRY(test_const_qualifier_metadata),
	};
	run_tests(tests, (int)(sizeof(tests) / sizeof(tests[0])));
}
