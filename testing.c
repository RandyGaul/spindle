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
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
			break;
		case IR_STRUCT_BEGIN:
			printf(" name=%s", inst->str0 ? inst->str0 : "<anon>");
			break;
		case IR_STRUCT_MEMBER:
			printf(" name=%s type=%s", inst->str0 ? inst->str0 : "<anon>", inst->str1 ? inst->str1 : "<anon>");
			if (inst->arg0)
				printf(" array_len=%d", inst->arg0);
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
			break;
		case IR_STRUCT_END:
			break;
		case IR_BLOCK_DECL_BEGIN:
			printf(" type=%s", inst->str0 ? inst->str0 : "<anon>");
			dump_storage_flags(inst->storage_flags);
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
			break;
		case IR_BLOCK_DECL_LAYOUT:
			printf(" %s", inst->str0 ? inst->str0 : "<anon>");
			break;
		case IR_BLOCK_DECL_MEMBER:
			printf(" name=%s type=%s", inst->str0 ? inst->str0 : "<anon>", inst->str1 ? inst->str1 : "<anon>");
			if (inst->arg0)
				printf(" array_len=%d", inst->arg0);
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
		dump_layout_info(sym->layout_flags, sym->layout_set, sym->layout_binding, sym->layout_location);
		printf("\n");
	}
}

void unit_test()
{
	init_keyword_interns();

	// Validate that builtin scalar and vector types are registered and queryable.
	type_system_init_builtins();
	const char* float_name = sintern("float");
	Type* float_type = type_system_get(float_name);
	assert(float_type && float_type->tag == T_FLOAT);
	Type* vec4_type = type_system_get(sintern("vec4"));
	assert(vec4_type && vec4_type->tag == T_VEC && vec4_type->cols == 4);
	// Ensure user-declared struct types are interned and retrievable.
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

	// Confirm symbol table scope chaining, storage flags, and layout metadata handling.
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
	type_system_free();
	symbol_table_free();

	// Check that IR emission produces entries with the requested opcode.
	IR_Cmd* saved_ir = g_ir;
	g_ir = NULL;
	IR_Cmd* emitted = ir_emit(IR_PUSH_INT);
	assert(emitted);
	assert(acount(g_ir) == 1);
	assert(g_ir[0].op == IR_PUSH_INT);
	afree(g_ir);
	g_ir = saved_ir;

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

	compiler_setup(snippet_control_flow);
	int saw_pre_inc = 0;
	int saw_post_inc = 0;
	int saw_pre_dec = 0;
	int saw_post_dec = 0;
	int saw_pre_inc_lvalue = 0;
	int saw_post_inc_lvalue = 0;
	int saw_pre_dec_lvalue = 0;
	int saw_post_dec_lvalue = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
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

	compiler_setup(snippet_function_calls);
	assert(acount(g_ir) > 0);
	assert(acount(st->symbols) > 0);
	compiler_teardown();

	compiler_setup(snippet_looping);
	assert(acount(g_ir) > 0);
	compiler_teardown();

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

	compiler_setup(snippet_struct_block);
	const char* light_struct = sintern("Light");
	const char* block_name = sintern("LightBlock");
	const char* instance_name = sintern("u_light_data");
	const char* member_name = sintern("lights");
	const char* std140_name = sintern("std140");
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
