typedef enum SymbolStorage
{
	SYM_STORAGE_IN = 1 << 0,
	SYM_STORAGE_OUT = 1 << 1,
	SYM_STORAGE_UNIFORM = 1 << 2,
} SymbolStorage;

typedef enum SymbolLayout
{
	SYM_LAYOUT_SET = 1 << 0,
	SYM_LAYOUT_BINDING = 1 << 1,
	SYM_LAYOUT_LOCATION = 1 << 2,
} SymbolLayout;

typedef struct SymbolScope
{
	Map map;
} SymbolScope;

typedef struct SymbolTable
{
	dyna Symbol* symbols;
	dyna SymbolScope* scopes;
} SymbolTable;

const char* kw_in;
const char* kw_out;
const char* kw_uniform;
const char* kw_layout;
const char* kw_set;
const char* kw_binding;
const char* kw_location;
const char* kw_if;
const char* kw_else;
const char* kw_return;
const char* kw_break;
const char* kw_continue;
const char* kw_discard;
const char* kw_for;
const char* kw_while;
const char* kw_do;

void init_keyword_interns()
{
	kw_in = sintern("in");
	kw_out = sintern("out");
	kw_uniform = sintern("uniform");
	kw_layout = sintern("layout");
	kw_set = sintern("set");
	kw_binding = sintern("binding");
	kw_location = sintern("location");
	kw_if = sintern("if");
	kw_else = sintern("else");
	kw_return = sintern("return");
	kw_break = sintern("break");
	kw_continue = sintern("continue");
	kw_discard = sintern("discard");
	kw_for = sintern("for");
	kw_while = sintern("while");
	kw_do = sintern("do");
}

SymbolTable g_symbol_table;
SymbolTable* st = &g_symbol_table;
const char* current_decl_type_name;
Type* current_decl_type_type;
const char* current_param_type_name;
Type* current_param_type_type;
Type** current_function_params;
Symbol* current_decl_symbol;
Symbol* current_param_symbol;

void symbol_table_enter_scope()
{
	apush(st->scopes, (SymbolScope){ 0 });
}

void symbol_table_leave_scope()
{
	int count = acount(st->scopes);
	if (!count)
		return;
	SymbolScope* scope = &st->scopes[count - 1];
	map_free(scope->map);
	apop(st->scopes);
}

void symbol_table_init()
{
	st->symbols = NULL;
	st->scopes = NULL;
	symbol_table_enter_scope(st);
}

Symbol* symbol_table_add(const char* name, const char* type_name, Type* type, SymbolKind kind)
{
	SymbolScope* scope = &alast(st->scopes);
	uint64_t key = (uint64_t)name;
	uint64_t existing = map_get(scope->map, key);
	if (existing)
		return &st->symbols[(int)existing - 1];
	Symbol sym = (Symbol){ 0 };
	sym.name = name;
	sym.type_name = type_name;
	sym.type = type;
	sym.kind = kind;
	sym.scope_depth = acount(st->scopes) - 1;
	apush(st->symbols, sym);
	int idx = acount(st->symbols);
	map_add(scope->map, key, (uint64_t)idx);
	return &st->symbols[idx - 1];
}

Symbol* symbol_table_find(const char* name)
{
	for (int i = acount(st->scopes) - 1; i >= 0; --i)
	{
		SymbolScope* scope = &st->scopes[i];
		uint64_t idx = map_get(scope->map, (uint64_t)name);
		if (idx)
			return &st->symbols[(int)idx - 1];
	}
	return NULL;
}

void symbol_add_storage(Symbol* sym, unsigned flags)
{
	sym->storage_flags |= flags;
}

int symbol_has_storage(const Symbol* sym, unsigned flag)
{
	return (sym->storage_flags & flag) != 0;
}

void symbol_set_layout(Symbol* sym, unsigned layout_flag, int value)
{
	sym->layout_flags |= layout_flag;
	switch (layout_flag)
	{
	case SYM_LAYOUT_SET:
		sym->layout_set = value;
		break;
	case SYM_LAYOUT_BINDING:
		sym->layout_binding = value;
		break;
	case SYM_LAYOUT_LOCATION:
		sym->layout_location = value;
		break;
	default:
		break;
	}
}

int symbol_has_layout(const Symbol* sym, unsigned layout_flag)
{
	return (sym->layout_flags & layout_flag) != 0;
}

int symbol_get_layout(const Symbol* sym, unsigned layout_flag)
{
	if (!symbol_has_layout(sym, layout_flag))
		return -1;
	switch (layout_flag)
	{
	case SYM_LAYOUT_SET:
		return sym->layout_set;
	case SYM_LAYOUT_BINDING:
		return sym->layout_binding;
	case SYM_LAYOUT_LOCATION:
		return sym->layout_location;
	default:
		break;
	}
	return -1;
}

void symbol_apply_type_spec(Symbol* sym, const TypeSpec* spec)
{
	symbol_add_storage(sym, spec->storage_flags);
	if (spec->layout_flags & SYM_LAYOUT_SET)
		symbol_set_layout(sym, SYM_LAYOUT_SET, spec->layout_set);
	if (spec->layout_flags & SYM_LAYOUT_BINDING)
		symbol_set_layout(sym, SYM_LAYOUT_BINDING, spec->layout_binding);
	if (spec->layout_flags & SYM_LAYOUT_LOCATION)
		symbol_set_layout(sym, SYM_LAYOUT_LOCATION, spec->layout_location);
}

void symbol_mark_array(Symbol* sym, Type* element_type)
{
	if (!sym)
		return;
	sym->array_element_type = element_type;
	sym->array_dimensions += 1;
	sym->array_type = (Type){ 0 };
	sym->array_type.tag = T_ARRAY;
	sym->array_type.base = element_type ? element_type->tag : T_VOID;
	sym->array_type.cols = element_type ? element_type->cols : 1;
	sym->array_type.rows = element_type ? element_type->rows : 1;
	sym->array_type.array_len = -1;
	sym->array_type.user = element_type;
	sym->array_type.name = NULL;
	sym->type = &sym->array_type;
}

void symbol_set_function_signature(Symbol* sym, Type** params, int param_count)
{
	if (!sym || sym->kind != SYM_FUNC)
		return;
	if (sym->param_signature_set)
	{
		if (sym->param_count != param_count)
		{
			type_check_error("function %s redeclared with %d parameters but previously had %d", sym->name, param_count, sym->param_count);
		}
		for (int i = 0; i < param_count; ++i)
		{
			Type* existing = (sym->params && i < acount(sym->params)) ? sym->params[i] : NULL;
			Type* incoming = (params && i < acount(params)) ? params[i] : NULL;
			if (existing && incoming)
			{
				if (!type_equal(existing, incoming))
				{
					type_check_error("function %s parameter %d type mismatch (%s vs %s)", sym->name, i + 1, type_display(existing), type_display(incoming));
				}
			}
			else if (existing != incoming)
			{
				type_check_error("function %s parameter %d type mismatch", sym->name, i + 1);
			}
		}
		return;
	}
	aclear(sym->params);
	for (int i = 0; i < param_count; ++i)
	{
		Type* incoming = params ? params[i] : NULL;
		apush(sym->params, incoming);
	}
	sym->param_count = param_count;
	sym->param_signature_set = 1;
}

void symbol_table_free()
{
	while (acount(st->scopes) > 0)
	{
		symbol_table_leave_scope(st);
	}
	for (int i = 0; i < acount(st->symbols); ++i)
	{
		Symbol* sym = &st->symbols[i];
		afree(sym->params);
	}
	afree(st->symbols);
	afree(st->scopes);
}

typedef enum Prec
{
	PREC_EXPR = 0,
	PREC_ASSIGN = 10, // right-assoc
	PREC_TERNARY = 20, // ?:
	PREC_OR_OR = 30,
	PREC_AND_AND = 40,
	PREC_EQ = 80, // == !=
	PREC_REL = 90, // < <= > >=
	PREC_ADD = 100, // + -
	PREC_MUL = 110, // * / %
	PREC_POSTFIX = 120, // () [] .
	PREC_UNARY = 130 // prefix + - ! ~
} Prec;

const char* in;
const char* at;
char ch;

struct
{
	Tok kind;
	Prec prec;
	void (*lexpr)(); // Start an expression.
	void (*rexpr)(); // Continue an expression (binary/postfix/etc).
	int int_val;
	double float_val;
	const char* lexeme;
	int len;
} tok;

void parse_error(const char* msg)
{
	fprintf(stderr, "Parse error: %s", msg);
	if (tok.kind < TOK_COUNT)
	{
		fprintf(stderr, " (token %s", tok_name[tok.kind]);
		if (tok.len)
		{
			fprintf(stderr, " '%.*s'", tok.len, tok.lexeme);
		}
		fprintf(stderr, ")");
	}
	fprintf(stderr, "\n");
	exit(1);
}

void next();
void expect(Tok k)
{
	if (tok.kind != k)
		parse_error("expected token");
	next();
}

void next_ch()
{
	ch = *at ? (unsigned char)*at++ : 0;
}
int is_space(int c)
{
	return c == ' ' || c == '\t' || c == '\r' || c == '\n';
}
int is_alpha(int c)
{
	return (c == '_') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z');
}
int is_digit(int c)
{
	return (c >= '0' && c <= '9');
}
int match_ch(int want)
{
	if (ch == want)
	{
		next_ch();
		return 1;
	}
	return 0;
}

void skip_ws_comments()
{
	while (1)
	{
		while (is_space(ch))
			next_ch();
		if (ch == '/' && at[0] == '/')
		{
			while (ch && ch != '\n')
			{
				next_ch();
			}
			continue;
		}
		if (ch == '/' && at[0] == '*')
		{
			next_ch();
			next_ch();
			while (ch && !(ch == '*' && at[0] == '/'))
				next_ch();
			if (ch == '*')
			{
				next_ch();
				next_ch();
			}
			continue;
		}
		break;
	}
}

unsigned storage_flag_from_keyword(const char* s)
{
	if (s == kw_in)
		return SYM_STORAGE_IN;
	if (s == kw_out)
		return SYM_STORAGE_OUT;
	if (s == kw_uniform)
		return SYM_STORAGE_UNIFORM;
	return 0;
}

unsigned layout_flag_from_keyword(const char* s)
{
	if (s == kw_set)
		return SYM_LAYOUT_SET;
	if (s == kw_binding)
		return SYM_LAYOUT_BINDING;
	if (s == kw_location)
		return SYM_LAYOUT_LOCATION;
	return 0;
}

void type_spec_add_storage(TypeSpec* spec, unsigned flags)
{
	spec->storage_flags |= flags;
}

void type_spec_set_layout(TypeSpec* spec, unsigned layout_flag, int value)
{
	spec->layout_flags |= layout_flag;
	switch (layout_flag)
	{
	case SYM_LAYOUT_SET:
		spec->layout_set = value;
		break;
	case SYM_LAYOUT_BINDING:
		spec->layout_binding = value;
		break;
	case SYM_LAYOUT_LOCATION:
		spec->layout_location = value;
		break;
	default:
		break;
	}
}

int is_type_name(const char* s)
{
	return type_system_get(s) != NULL;
}

int is_type_token()
{
	if (tok.kind != TOK_IDENTIFIER)
		return 0;
	const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
	if (is_type_name(name))
		return 1;
	if (storage_flag_from_keyword(tok.lexeme))
		return 1;
	if (tok.lexeme == kw_layout)
		return 1;
	return 0;
}

void parse_layout_block(TypeSpec* spec)
{
	next();
	expect(TOK_LPAREN);
	while (tok.kind != TOK_RPAREN)
	{
		if (tok.kind != TOK_IDENTIFIER)
			parse_error("expected identifier in layout");
		unsigned layout_flag = layout_flag_from_keyword(tok.lexeme);
		if (!layout_flag)
			parse_error("unknown layout identifier");
		next();
		expect(TOK_ASSIGN);
		if (tok.kind != TOK_INT)
			parse_error("expected integer in layout assignment");
		type_spec_set_layout(spec, layout_flag, tok.int_val);
		next();
		if (tok.kind == TOK_COMMA)
		{
			next();
			continue;
		}
		break;
	}
	expect(TOK_RPAREN);
}

void parse_type_qualifiers(TypeSpec* spec)
{
	while (tok.kind == TOK_IDENTIFIER)
	{
		unsigned storage_flag = storage_flag_from_keyword(tok.lexeme);
		if (storage_flag)
		{
			type_spec_add_storage(spec, storage_flag);
			next();
			continue;
		}
		if (tok.lexeme == kw_layout)
		{
			parse_layout_block(spec);
			continue;
		}
		break;
	}
}

TypeSpec parse_type_specifier()
{
	TypeSpec spec = (TypeSpec){ 0 };
	parse_type_qualifiers(&spec);
	if (tok.kind != TOK_IDENTIFIER)
		parse_error("expected type");
	spec.type_name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
	if (!is_type_name(spec.type_name))
		parse_error("expected type");
	spec.type = type_system_get(spec.type_name);
	if (!spec.type)
		parse_error("unknown type");
	next();
	return spec;
}

void ir_apply_type_spec(IR_Cmd* inst, const TypeSpec* spec)
{
	inst->storage_flags = spec->storage_flags;
	inst->layout_flags = spec->layout_flags;
	inst->layout_set = spec->layout_set;
	inst->layout_binding = spec->layout_binding;
	inst->layout_location = spec->layout_location;
}

IR_Cmd* ir_emit(IR_Op op)
{
	IR_Cmd inst = (IR_Cmd){ 0 };
	inst.op = op;
	apush(g_ir, inst);
	return &alast(g_ir);
}

void decl_emit_begin(const TypeSpec* spec)
{
	IR_Cmd* begin = ir_emit(IR_DECL_BEGIN);
	ir_apply_type_spec(begin, spec);
	current_decl_type_name = spec->type_name;
	current_decl_type_type = spec->type;
	IR_Cmd* inst = ir_emit(IR_DECL_TYPE);
	inst->str0 = spec->type_name;
}

void expr_binary(Prec min_prec);
void expr()
{
	expr_binary(PREC_EXPR);
}
void expr_error()
{
	parse_error("unexpected token in expression");
}

void stmt();
void stmt_decl();
void stmt_block();
void stmt_controlled();
void parse();

void decl_array_suffix()
{
	Symbol* sym = current_decl_symbol;
	Type* element_type = current_decl_type_type;
	while (tok.kind == TOK_LBRACK)
	{
		next();
		ir_emit(IR_DECL_ARRAY_BEGIN);
		if (sym)
		{
			symbol_mark_array(sym, element_type);
			element_type = sym->type;
		}

		if (tok.kind == TOK_RBRACK)
		{
			ir_emit(IR_DECL_ARRAY_UNSIZED);
		}
		else
		{
			ir_emit(IR_DECL_ARRAY_SIZE_BEGIN);
			expr();
			ir_emit(IR_DECL_ARRAY_SIZE_END);
		}
		expect(TOK_RBRACK);
		ir_emit(IR_DECL_ARRAY_END);
	}
}

void func_param_array_suffix()
{
	Symbol* sym = current_param_symbol;
	Type* element_type = current_param_type_type;
	while (tok.kind == TOK_LBRACK)
	{
		next();
		ir_emit(IR_FUNC_PARAM_ARRAY_BEGIN);
		if (sym)
		{
			symbol_mark_array(sym, element_type);
			element_type = sym->type;
		}
		if (tok.kind == TOK_RBRACK)
		{
			ir_emit(IR_FUNC_PARAM_ARRAY_UNSIZED);
		}
		else
		{
			ir_emit(IR_FUNC_PARAM_ARRAY_SIZE_BEGIN);
			expr();
			ir_emit(IR_FUNC_PARAM_ARRAY_SIZE_END);
		}
		expect(TOK_RBRACK);
		ir_emit(IR_FUNC_PARAM_ARRAY_END);
	}
}

void func_param()
{
	if (!is_type_token())
		parse_error("expected type in parameter");
	TypeSpec spec = parse_type_specifier();
	IR_Cmd* param = ir_emit(IR_FUNC_PARAM_BEGIN);
	ir_apply_type_spec(param, &spec);
	current_param_type_name = spec.type_name;
	current_param_type_type = spec.type;
	apush(current_function_params, spec.type);
	IR_Cmd* inst = ir_emit(IR_FUNC_PARAM_TYPE);
	inst->str0 = spec.type_name;
	if (tok.kind != TOK_IDENTIFIER)
		parse_error("expected identifier in parameter");
	const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
	inst = ir_emit(IR_FUNC_PARAM_NAME);
	inst->str0 = name;
	Symbol* sym = symbol_table_add(name, current_param_type_name, current_param_type_type, SYM_PARAM);
	symbol_apply_type_spec(sym, &spec);
	current_param_symbol = sym;
	next();
	func_param_array_suffix();
	current_param_symbol = NULL;
	ir_emit(IR_FUNC_PARAM_END);
}

void func_param_list()
{
	ir_emit(IR_FUNC_PARAMS_BEGIN);
	if (tok.kind != TOK_RPAREN)
	{
		while (1)
		{
			func_param();
			if (tok.kind == TOK_COMMA)
			{
				next();
				ir_emit(IR_FUNC_PARAM_SEPARATOR);
				continue;
			}
			break;
		}
	}
	expect(TOK_RPAREN);
	ir_emit(IR_FUNC_PARAMS_END);
}

void global_var_decl(TypeSpec spec, const char* first_name)
{
	IR_Cmd* inst;
	decl_emit_begin(&spec);
	inst = ir_emit(IR_DECL_VAR);
	inst->str0 = first_name;
	Symbol* sym = symbol_table_add(first_name, current_decl_type_name, current_decl_type_type, SYM_VAR);
	symbol_apply_type_spec(sym, &spec);
	current_decl_symbol = sym;
	decl_array_suffix();
	current_decl_symbol = NULL;
	if (tok.kind == TOK_ASSIGN)
	{
		next();
		ir_emit(IR_DECL_INIT_BEGIN);
		expr();
		ir_emit(IR_DECL_INIT_END);
	}
	while (tok.kind == TOK_COMMA)
	{
		next();
		ir_emit(IR_DECL_SEPARATOR);
		if (tok.kind != TOK_IDENTIFIER)
			parse_error("expected identifier in declaration");
		const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
		inst = ir_emit(IR_DECL_VAR);
		inst->str0 = name;
		sym = symbol_table_add(name, current_decl_type_name, current_decl_type_type, SYM_VAR);
		symbol_apply_type_spec(sym, &spec);
		current_decl_symbol = sym;
		next();
		decl_array_suffix();
		current_decl_symbol = NULL;
		if (tok.kind == TOK_ASSIGN)
		{
			next();
			ir_emit(IR_DECL_INIT_BEGIN);
			expr();
			ir_emit(IR_DECL_INIT_END);
		}
	}
	expect(TOK_SEMI);
	ir_emit(IR_DECL_END);
	current_decl_type_name = NULL;
	current_decl_type_type = NULL;
}

void func_decl_or_def(TypeSpec spec, const char* name)
{
	IR_Cmd* func = ir_emit(IR_FUNC_BEGIN);
	func->str0 = spec.type_name;
	func->str1 = name;
	ir_apply_type_spec(func, &spec);
	Symbol* sym = symbol_table_add(func->str1, spec.type_name, spec.type, SYM_FUNC);
	int sym_index = (int)(sym - st->symbols);
	if (sym->type && spec.type && !type_equal(sym->type, spec.type))
	{
		type_check_error("function %s redeclared with return type %s but previously %s", name, type_display(spec.type), type_display(sym->type));
	}
	if (!sym->type && spec.type)
	{
		sym->type = spec.type;
	}
	symbol_apply_type_spec(sym, &spec);
	if (current_function_params)
		aclear(current_function_params);
	expect(TOK_LPAREN);
	symbol_table_enter_scope();
	func_param_list();
	sym = &st->symbols[sym_index];
	symbol_set_function_signature(sym, current_function_params, acount(current_function_params));
	if (current_function_params)
		aclear(current_function_params);
	if (tok.kind == TOK_SEMI)
	{
		next();
		symbol_table_leave_scope();
		ir_emit(IR_FUNC_PROTOTYPE_END);
		return;
	}
	if (tok.kind == TOK_LBRACE)
	{
		ir_emit(IR_FUNC_DEFINITION_BEGIN);
		stmt_block();
		symbol_table_leave_scope();
		ir_emit(IR_FUNC_DEFINITION_END);
		return;
	}
	symbol_table_leave_scope();
	parse_error("expected ';' or function body");
}

void stmt_decl()
{
	TypeSpec spec = parse_type_specifier();
	IR_Cmd* inst;
	decl_emit_begin(&spec);
	while (1)
	{
		if (tok.kind != TOK_IDENTIFIER)
			parse_error("expected identifier in declaration");
		const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
		inst = ir_emit(IR_DECL_VAR);
		inst->str0 = name;
		Symbol* sym = symbol_table_add(name, current_decl_type_name, current_decl_type_type, SYM_VAR);
		symbol_apply_type_spec(sym, &spec);
		current_decl_symbol = sym;
		next();
		decl_array_suffix();
		current_decl_symbol = NULL;
		if (tok.kind == TOK_ASSIGN)
		{
			next();
			ir_emit(IR_DECL_INIT_BEGIN);
			expr();
			ir_emit(IR_DECL_INIT_END);
		}
		if (tok.kind == TOK_COMMA)
		{
			next();
			ir_emit(IR_DECL_SEPARATOR);
			continue;
		}
		break;
	}
	expect(TOK_SEMI);
	ir_emit(IR_DECL_END);
	current_decl_type_name = NULL;
	current_decl_type_type = NULL;
}

void expr_int()
{
	IR_Cmd* inst = ir_emit(IR_PUSH_INT);
	inst->arg0 = tok.int_val;
	next();
}

void expr_float()
{
	IR_Cmd* inst = ir_emit(IR_PUSH_FLOAT);
	inst->float_val = tok.float_val;
	next();
}

void expr_ident()
{
	IR_Cmd* inst = ir_emit(IR_PUSH_IDENT);
	inst->str0 = sintern_range(tok.lexeme, tok.lexeme + tok.len);
	next();
}

void expr_paren()
{
	next(); // consume '('
	expr();
	expect(TOK_RPAREN);
}

void expr_call()
{
	int callee_idx = acount(g_ir) - 1;
	int argc = 0;
	if (tok.kind != TOK_RPAREN)
	{
		expr();
		argc++;
		while (tok.kind == TOK_COMMA)
		{
			next();
			expr();
			argc++;
		}
		expect(TOK_RPAREN);
	}
	else
	{
		next(); // consume ')'
	}
	Type* ctor_type = NULL;
	const char* ctor_name = NULL;
	const char* callee_name = NULL;
	if (callee_idx >= 0)
	{
		IR_Cmd* callee = &g_ir[callee_idx];
		if (callee->op == IR_PUSH_IDENT && callee->str0)
		{
			callee_name = callee->str0;
			Symbol* sym = symbol_table_find(callee->str0);
			Type* type = type_system_get(callee->str0);
			if (type && (!sym || sym->kind != SYM_FUNC))
			{
				callee->type = type;
				ctor_type = type;
				ctor_name = callee->str0;
			}
		}
	}
	IR_Cmd* inst = ir_emit(ctor_type ? IR_CONSTRUCT : IR_CALL);
	inst->arg0 = argc;
	if (ctor_type)
	{
		inst->str0 = ctor_name;
		inst->type = ctor_type;
	}
	else if (callee_name)
	{
		inst->str0 = callee_name;
	}
}

void expr_index()
{
	expr(); // parse index expr
	expect(TOK_RBRACK);
	ir_emit(IR_INDEX);
}

int swizzle_set_from_char(char c)
{
	if (c >= 'A' && c <= 'Z')
		c = (char)(c - 'A' + 'a');
	switch (c)
	{
	case 'x':
	case 'y':
	case 'z':
	case 'w':
		return 0;
	case 'r':
	case 'g':
	case 'b':
	case 'a':
		return 1;
	case 's':
	case 't':
	case 'p':
	case 'q':
		return 2;
	default:
		break;
	}
	return -1;
}

int swizzle_component_index(int set, char c)
{
	static const char* sets[] = { "xyzw", "rgba", "stpq" };
	if (c >= 'A' && c <= 'Z')
		c = (char)(c - 'A' + 'a');
	if (set < 0 || set >= (int)(sizeof(sets) / sizeof(sets[0])))
		return -1;
	const char* names = sets[set];
	for (int i = 0; i < 4; i = i + 1)
	{
		if (c == names[i])
			return i;
	}
	return -1;
}

int swizzle_is_valid(const char* name, int len, int* out_mask)
{
	if (len < 1 || len > 4)
		return 0;
	int set = swizzle_set_from_char(name[0]);
	if (set < 0)
		return 0;
	int mask = 0;
	for (int i = 0; i < len; ++i)
	{
		int comp = swizzle_component_index(set, name[i]);
		if (comp < 0)
			return 0;
		mask |= comp << (i * 4);
	}
	if (out_mask)
		*out_mask = mask;
	return 1;
}

void expr_member()
{
	if (tok.kind != TOK_IDENTIFIER)
		parse_error("expected identifier after '.'");
	const char* name = tok.lexeme;
	int mask = 0;
	if (swizzle_is_valid(name, tok.len, &mask))
	{
		IR_Cmd* inst = ir_emit(IR_SWIZZLE);
		inst->str0 = sintern_range(name, name + tok.len);
		inst->arg0 = tok.len;
		inst->arg1 = mask;
	}
	else
	{
		IR_Cmd* inst = ir_emit(IR_MEMBER);
		inst->str0 = sintern_range(name, name + tok.len);
	}
	next();
}

void expr_ternary()
{
	// current token was '?', already consumed by Pratt loop
	expr(); // then-branch
	expect(TOK_COLON);
	expr_binary(PREC_TERNARY - 1); // else-branch
	ir_emit(IR_SELECT);
}

void expr_binary_left(Tok op, Prec p)
{
	expr_binary(p);
	IR_Cmd* inst = ir_emit(IR_BINARY);
	inst->tok = op;
}

void expr_unary_common(Tok op)
{
	next(); // consume operator
	expr_binary(PREC_UNARY - 1);
	IR_Cmd* inst = ir_emit(IR_UNARY);
	inst->tok = op;
}

// Compact lex-parse technique learned from Per Vognsen
// https://gist.github.com/pervognsen/e61c6b91fca7275d90692831e2a55c9a
// https://gist.github.com/pervognsen/372aa279e48d58012825a66564757c40

#define EXPR_UNARY(name, token_enum) \
	void expr_##name() \
	{ \
		expr_unary_common(TOK_##token_enum); \
	}
#define EXPR_BINARY(name, precname, token_enum) \
	void expr_##name() \
	{ \
		expr_binary_left(TOK_##token_enum, PREC_##precname); \
	}

EXPR_UNARY(neg, MINUS);
EXPR_UNARY(pos, PLUS);
EXPR_UNARY(not, NOT);
EXPR_UNARY(bnot, TILDE);

EXPR_BINARY(add, ADD, PLUS);
EXPR_BINARY(sub, ADD, MINUS);
EXPR_BINARY(mul, MUL, STAR);
EXPR_BINARY(div, MUL, SLASH);
EXPR_BINARY(mod, MUL, PERCENT);
EXPR_BINARY(lt, REL, LT);
EXPR_BINARY(le, REL, LE);
EXPR_BINARY(gt, REL, GT);
EXPR_BINARY(ge, REL, GE);
EXPR_BINARY(eq, EQ, EQ);
EXPR_BINARY(ne, EQ, NE);
EXPR_BINARY(land, AND_AND, AND_AND);
EXPR_BINARY(lor, OR_OR, OR_OR);
EXPR_BINARY(assign, ASSIGN, ASSIGN);
EXPR_BINARY(plus_assign, ASSIGN, PLUS_ASSIGN);

void expr_binary(Prec min_prec)
{
	tok.lexpr(); // start: number/ident/paren/unary...

	while (tok.prec > min_prec)
	{
		void (*cont)() = tok.rexpr;
		next(); // consume operator -> next token begins RHS
		cont(); // parse RHS/args/member and "emit"
	}
}

void stmt_block()
{
	expect(TOK_LBRACE);
	ir_emit(IR_BLOCK_BEGIN);
	symbol_table_enter_scope();
	while (tok.kind != TOK_RBRACE && tok.kind != TOK_EOF)
	{
		stmt();
	}
	expect(TOK_RBRACE);
	symbol_table_leave_scope();
	ir_emit(IR_BLOCK_END);
}

void stmt_controlled()
{
	if (tok.kind == TOK_LBRACE)
	{
		stmt_block();
		return;
	}
	ir_emit(IR_BLOCK_BEGIN);
	stmt();
	ir_emit(IR_BLOCK_END);
}

void stmt_if()
{
	expect(TOK_IF);
	ir_emit(IR_IF_BEGIN);
	expect(TOK_LPAREN);
	expr();
	expect(TOK_RPAREN);
	ir_emit(IR_IF_THEN);
	stmt_controlled();
	if (tok.kind == TOK_ELSE)
	{
		next();
		ir_emit(IR_IF_ELSE);
		stmt_controlled();
	}
	ir_emit(IR_IF_END);
}

void stmt_expr()
{
	expr();
	expect(TOK_SEMI);
	ir_emit(IR_STMT_EXPR);
}

void stmt_return()
{
	expect(TOK_RETURN);
	int has_value = tok.kind != TOK_SEMI;
	if (has_value)
	{
		expr();
	}
	expect(TOK_SEMI);
	IR_Cmd* inst = ir_emit(IR_RETURN);
	inst->arg0 = has_value;
}

void stmt_break()
{
	expect(TOK_BREAK);
	expect(TOK_SEMI);
	ir_emit(IR_BREAK);
}

void stmt_continue()
{
	expect(TOK_CONTINUE);
	expect(TOK_SEMI);
	ir_emit(IR_CONTINUE);
}

void stmt_discard()
{
	expect(TOK_DISCARD);
	expect(TOK_SEMI);
	ir_emit(IR_DISCARD);
}

void stmt_while()
{
	expect(TOK_WHILE);
	ir_emit(IR_WHILE_BEGIN);
	expect(TOK_LPAREN);
	ir_emit(IR_WHILE_COND_BEGIN);
	expr();
	expect(TOK_RPAREN);
	ir_emit(IR_WHILE_COND_END);
	ir_emit(IR_WHILE_BODY_BEGIN);
	stmt_controlled();
	ir_emit(IR_WHILE_BODY_END);
	ir_emit(IR_WHILE_END);
}

void stmt_do()
{
	expect(TOK_DO);
	ir_emit(IR_DO_BEGIN);
	ir_emit(IR_DO_BODY_BEGIN);
	stmt_controlled();
	ir_emit(IR_DO_BODY_END);
	expect(TOK_WHILE);
	expect(TOK_LPAREN);
	ir_emit(IR_DO_COND_BEGIN);
	expr();
	expect(TOK_RPAREN);
	ir_emit(IR_DO_COND_END);
	expect(TOK_SEMI);
	ir_emit(IR_DO_END);
}

void stmt_for()
{
	expect(TOK_FOR);
	ir_emit(IR_FOR_BEGIN);
	expect(TOK_LPAREN);
	symbol_table_enter_scope();
	ir_emit(IR_FOR_INIT_BEGIN);
	if (tok.kind == TOK_SEMI)
	{
		next();
	}
	else if (is_type_token())
	{
		stmt_decl();
	}
	else
	{
		expr();
		expect(TOK_SEMI);
	}
	ir_emit(IR_FOR_INIT_END);
	ir_emit(IR_FOR_COND_BEGIN);
	if (tok.kind != TOK_SEMI)
	{
		expr();
	}
	expect(TOK_SEMI);
	ir_emit(IR_FOR_COND_END);
	ir_emit(IR_FOR_STEP_BEGIN);
	if (tok.kind != TOK_RPAREN)
	{
		expr();
	}
	expect(TOK_RPAREN);
	ir_emit(IR_FOR_STEP_END);
	ir_emit(IR_FOR_BODY_BEGIN);
	stmt_controlled();
	ir_emit(IR_FOR_BODY_END);
	ir_emit(IR_FOR_END);
	symbol_table_leave_scope();
}

void stmt()
{
	if (is_type_token())
	{
		stmt_decl();
		return;
	}
	switch (tok.kind)
	{
	case TOK_IF:
		stmt_if();
		break;
	case TOK_FOR:
		stmt_for();
		break;
	case TOK_WHILE:
		stmt_while();
		break;
	case TOK_DO:
		stmt_do();
		break;
	case TOK_LBRACE:
		stmt_block();
		break;
	case TOK_RETURN:
		stmt_return();
		break;
	case TOK_BREAK:
		stmt_break();
		break;
	case TOK_CONTINUE:
		stmt_continue();
		break;
	case TOK_DISCARD:
		stmt_discard();
		break;
	case TOK_SEMI:
		next();
		break;
	case TOK_EOF:
		break;
	default:
		stmt_expr();
		break;
	}
}

void top_level()
{
	if (!is_type_token())
		parse_error("expected type at top level");
	TypeSpec type_spec = parse_type_specifier();
	if (tok.kind != TOK_IDENTIFIER)
		parse_error("expected identifier after type");
	const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
	next();
	if (tok.kind == TOK_LPAREN)
	{
		func_decl_or_def(type_spec, name);
		return;
	}
	global_var_decl(type_spec, name);
}

void parse()
{
	while (tok.kind != TOK_EOF)
	{
		top_level();
	}
}

#define TOK_CHAR(ch1, tok1) \
	case ch1: \
		next_ch(); \
		tok.kind = TOK_##tok1; \
		tok.prec = 0; \
		tok.lexpr = expr_error; \
		tok.rexpr = expr_error; \
		break;

#define TOK_EXPR(ch1, tok1, prec1, lexpr1, rexpr1) \
	case ch1: \
		next_ch(); \
		tok.kind = TOK_##tok1; \
		tok.prec = PREC_##prec1; \
		tok.lexpr = expr_##lexpr1; \
		tok.rexpr = expr_##rexpr1; \
		break;

#define TOK_EXPR_EXPR(ch1, tok1, prec1, lexpr1, rexpr1, ch2, tok2, prec2, lexpr2, rexpr2) \
	case ch1: \
		next_ch(); \
		if (match_ch(ch2)) \
		{ \
			tok.kind = TOK_##tok2; \
			tok.prec = PREC_##prec2; \
			tok.lexpr = expr_##lexpr2; \
			tok.rexpr = expr_##rexpr2; \
		} \
		else \
		{ \
			tok.kind = TOK_##tok1; \
			tok.prec = PREC_##prec1; \
			tok.lexpr = expr_##lexpr1; \
			tok.rexpr = expr_##rexpr1; \
		} \
		break;

void lex_number()
{
	const char* start = at - 1;
	char* endptr = NULL;
	double val = strtod(start, &endptr);
	const char* end = endptr;
	const char* suffix = end;
	int is_float = 0;
	for (const char* p = start; p < end; ++p)
	{
		if (*p == '.' || *p == 'e' || *p == 'E')
		{
			is_float = 1;
			break;
		}
	}
	if (*suffix == 'f' || *suffix == 'F')
	{
		is_float = 1;
		++suffix;
	}
	if (*suffix == 'l' || *suffix == 'L')
	{
		is_float = 1;
		++suffix;
	}
	if (!is_float)
	{
		while (*suffix == 'u' || *suffix == 'U' || *suffix == 'l' || *suffix == 'L')
		{
			++suffix;
		}
	}
	tok.lexeme = start;
	tok.len = (int)(suffix - start);
	tok.prec = 0;
	tok.rexpr = expr_error;
	if (is_float)
	{
		tok.kind = TOK_FLOAT;
		tok.lexpr = expr_float;
		tok.float_val = val;
	}
	else
	{
		tok.kind = TOK_INT;
		tok.lexpr = expr_int;
		tok.int_val = 0;
		for (const char* p = start; p < end; ++p)
		{
			if (*p >= '0' && *p <= '9')
			{
				tok.int_val = tok.int_val * 10 + (*p - '0');
			}
		}
	}
	if (*suffix)
	{
		ch = (unsigned char)*suffix;
		at = suffix + 1;
	}
	else
	{
		ch = 0;
		at = suffix;
	}
}

void next()
{
	tok.kind = TOK_EOF;
	tok.prec = 0;
	tok.lexpr = expr_error;
	tok.rexpr = expr_error;
	tok.int_val = 0;
	tok.float_val = 0.0;
	tok.lexeme = at - 1;
	tok.len = 0;

	skip_ws_comments();

	if (ch == '.' && is_digit(at[0]))
	{
		lex_number();
		return;
	}

	switch (ch)
	{
		// single-char punctuation
		TOK_CHAR(0, EOF)
		TOK_CHAR(')', RPAREN)
		TOK_CHAR(']', RBRACK)
		TOK_CHAR('}', RBRACE)
		TOK_CHAR(',', COMMA)
		TOK_CHAR(';', SEMI)
		TOK_CHAR(':', COLON)
		TOK_EXPR('(', LPAREN, POSTFIX, paren, call)
		TOK_EXPR('[', LBRACK, POSTFIX, error, index)
		TOK_CHAR('{', LBRACE)
		TOK_EXPR('.', DOT, POSTFIX, error, member)

		// prefix + binary-ish
		TOK_EXPR('~', TILDE, UNARY, bnot, error)
		TOK_EXPR_EXPR('+', PLUS, ADD, pos, add, '=', PLUS_ASSIGN, ASSIGN, error, plus_assign)
		TOK_EXPR('-', MINUS, ADD, neg, sub)
		TOK_EXPR('*', STAR, MUL, error, mul)
		TOK_EXPR('/', SLASH, MUL, error, div)
		TOK_EXPR('%', PERCENT, MUL, error, mod)
		TOK_EXPR('?', QUESTION, TERNARY, error, ternary)

		// two-char combos
		TOK_EXPR_EXPR('<', LT, REL, error, lt, '=', LE, REL, error, le)
		TOK_EXPR_EXPR('>', GT, REL, error, gt, '=', GE, REL, error, ge)
		TOK_EXPR_EXPR('=', ASSIGN, ASSIGN, error, assign, '=', EQ, EQ, error, eq)
		TOK_EXPR_EXPR('!', NOT, UNARY, not, error, '=', NE, EQ, error, ne)

		// && and ||
		TOK_EXPR_EXPR('&', NOT, UNARY, error, error, '&', AND_AND, AND_AND, error, land)
		TOK_EXPR_EXPR('|', NOT, UNARY, error, error, '|', OR_OR, OR_OR, error, lor)

	default:
		break;
	}

	if (tok.kind != TOK_EOF)
		return;

	// identifiers
	if (is_alpha(ch))
	{
		const char* s = at - 1;
		while (is_alpha(ch) || is_digit(ch))
			next_ch();
		tok.len = (int)((at - 1) - s);
		const char* interned = sintern_range(s, s + tok.len);
		tok.lexeme = interned;
		tok.kind = TOK_IDENTIFIER;
		tok.prec = 0;
		tok.lexpr = expr_ident;
		tok.rexpr = expr_error;
		if (tok.lexeme == kw_if)
		{
			tok.kind = TOK_IF;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_else)
		{
			tok.kind = TOK_ELSE;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_for)
		{
			tok.kind = TOK_FOR;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_while)
		{
			tok.kind = TOK_WHILE;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_do)
		{
			tok.kind = TOK_DO;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_return)
		{
			tok.kind = TOK_RETURN;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_break)
		{
			tok.kind = TOK_BREAK;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_continue)
		{
			tok.kind = TOK_CONTINUE;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_discard)
		{
			tok.kind = TOK_DISCARD;
			tok.lexpr = expr_error;
		}
		return;
	}

	// numbers
	if (is_digit(ch))
	{
		lex_number();
		return;
	}

	// unknown char: consume and retry
	if (ch)
	{
		fprintf(stderr, "Unknown char: '%c'\n", ch);
		next_ch();
		next();
		return;
	}
}

void reset_parser_state()
{
	in = NULL;
	at = NULL;
	ch = 0;
	tok.kind = TOK_EOF;
	tok.prec = PREC_EXPR;
	tok.lexpr = expr_error;
	tok.rexpr = expr_error;
	tok.int_val = 0;
	tok.float_val = 0.0;
	tok.lexeme = NULL;
	tok.len = 0;
}

void compiler_teardown()
{
	symbol_table_free();
	type_system_free();
	afree(g_ir);
	afree(current_function_params);
	current_decl_type_name = NULL;
	current_decl_type_type = NULL;
	current_param_type_name = NULL;
	current_param_type_type = NULL;
	reset_parser_state();
}

void compiler_setup(const char* source)
{
	compiler_teardown();
	reset_parser_state();
	init_keyword_interns();
	in = source;
	at = in;
	next_ch();
	next();
	type_system_init_builtins();
	symbol_table_init();
	parse();
	type_check_ir();
}
