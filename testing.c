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

void dump_symbols(const SymbolTable* st)
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
	TypeSystem ts = (TypeSystem){ 0 };
	type_system_init_builtins(&ts);
	const char* float_name = sintern("float");
	Type* float_type = type_system_get(&ts, float_name);
	assert(float_type && float_type->tag == T_FLOAT);
	Type* vec4_type = type_system_get(&ts, sintern("vec4"));
	assert(vec4_type && vec4_type->tag == T_VEC && vec4_type->cols == 4);
	// Ensure user-declared struct types are interned and retrievable.
	const char* custom_name = sintern("test_struct");
	Type* declared_type = type_system_declare_struct(&ts, custom_name);
	assert(declared_type == type_system_get(&ts, custom_name));
	type_struct_begin_definition(declared_type);
	const char* field_name = sintern("value");
	StructField* struct_field = type_struct_add_field(declared_type, field_name, float_type, -1);
	assert(struct_field && struct_field->type == float_type);
	type_struct_complete_definition(declared_type);
	assert(type_struct_is_defined(declared_type));
	assert(type_struct_find_field(declared_type, field_name) == struct_field);
	type_system_free(&ts);

	// Confirm symbol table scope chaining, storage flags, and layout metadata handling.
	SymbolTable st = (SymbolTable){ 0 };
	symbol_table_init(&st);
	Type int_type = (Type){ 0 };
	int_type.tag = T_INT;
	const char* value_name = sintern("value");
	Symbol* value_sym = symbol_table_add(&st, value_name, sintern("int"), &int_type, SYM_VAR);
	assert(value_sym && value_sym->name == value_name);
	symbol_add_storage(value_sym, SYM_STORAGE_IN);
	assert(symbol_has_storage(value_sym, SYM_STORAGE_IN));
	symbol_table_enter_scope(&st);
	Type float_type_local = (Type){ 0 };
	float_type_local.tag = T_FLOAT;
	const char* inner_name = sintern("inner_value");
	Symbol* inner_sym = symbol_table_add(&st, inner_name, sintern("float"), &float_type_local, SYM_VAR);
	symbol_set_layout(inner_sym, SYM_LAYOUT_LOCATION, 3);
	assert(symbol_get_layout(inner_sym, SYM_LAYOUT_LOCATION) == 3);
	assert(symbol_table_find(&st, inner_name) == inner_sym);
	symbol_table_leave_scope(&st);
	assert(symbol_table_find(&st, inner_name) == NULL);
	assert(symbol_table_find(&st, value_name) == value_sym);
	symbol_table_free(&st);

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
	for (int i = 0; i < acount(g_ir); ++i)
	{
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

	compiler_setup(snippet_function_calls);
	assert(acount(g_ir) > 0);
	assert(acount(g_symbols.symbols) > 0);
	compiler_teardown();

	compiler_setup(snippet_looping);
	assert(acount(g_ir) > 0);
	compiler_teardown();

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

	compiler_setup(snippet_structs);
	const char* light_name = sintern("Light");
	Type* light_type = type_system_get(&g_types, light_name);
	assert(light_type && light_type->tag == T_STRUCT);
	const char* color_name = sintern("color");
	const char* intensity_name = sintern("intensity");
	StructField* color_field = type_struct_find_field(light_type, color_name);
	assert(color_field && color_field->type && type_is_vector(color_field->type) && color_field->type->cols == 3);
	StructField* intensity_field = type_struct_find_field(light_type, intensity_name);
	assert(intensity_field && intensity_field->type && type_is_scalar(intensity_field->type) && intensity_field->type->tag == T_FLOAT);
	const char* sun_name = sintern("sun");
	const char* copy_name = sintern("copy");
	Symbol* sun_sym = NULL;
	Symbol* copy_sym = NULL;
	for (int i = 0; i < acount(g_symbols.symbols); ++i)
	{
		Symbol* sym = &g_symbols.symbols[i];
		if (sym->name == sun_name)
		{
			sun_sym = sym;
			continue;
		}
		if (sym->name == copy_name)
		{
			copy_sym = sym;
			continue;
		}
	}
	assert(sun_sym && sun_sym->type == light_type);
	assert(copy_sym && copy_sym->type == light_type);
	int saw_struct_construct = 0;
	int saw_member_color = 0;
	int saw_member_intensity = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		if (g_ir[i].op == IR_CONSTRUCT && g_ir[i].str0 == light_name)
		{
			saw_struct_construct = 1;
			continue;
		}
		if (g_ir[i].op == IR_MEMBER && g_ir[i].str0 == color_name)
		{
			saw_member_color = 1;
			continue;
		}
		if (g_ir[i].op == IR_MEMBER && g_ir[i].str0 == intensity_name)
		{
			saw_member_intensity = 1;
			continue;
		}
	}
	compiler_teardown();
	assert(saw_struct_construct);
	assert(saw_member_color);
	assert(saw_member_intensity);
}