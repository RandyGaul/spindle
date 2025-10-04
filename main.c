#define _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_DEPRECATE

#include <stddef.h>

#define CKIT_IMPLEMENTATION
#include "ckit.h"

typedef struct IR_String
{
	const char* ptr;
	int len;
} IR_String;

typedef enum SymbolKind
{
	SYM_VAR,
	SYM_FUNC,
	SYM_PARAM,
	SYM_KIND_COUNT
} SymbolKind;

const char* symbol_kind_name[SYM_KIND_COUNT] = {
	[SYM_VAR]	= "var",
	[SYM_FUNC]	= "func",
	[SYM_PARAM]	= "param",
};

typedef struct Symbol
{
	IR_String name;
	IR_String type;
	SymbolKind kind;
	unsigned storage_flags;
	unsigned layout_flags;
	int layout_set;
	int layout_binding;
	int layout_location;
} Symbol;

typedef enum SymbolStorage
{
	SYM_STORAGE_IN	      = 1 << 0,
	SYM_STORAGE_OUT	      = 1 << 1,
	SYM_STORAGE_UNIFORM = 1 << 2,
} SymbolStorage;

typedef enum SymbolLayout
{
	SYM_LAYOUT_SET	      = 1 << 0,
	SYM_LAYOUT_BINDING  = 1 << 1,
	SYM_LAYOUT_LOCATION = 1 << 2,
} SymbolLayout;

typedef struct TypeSpec
{
	IR_String type;
	unsigned storage_flags;
	unsigned layout_flags;
	int layout_set;
	int layout_binding;
	int layout_location;
} TypeSpec;

typedef struct SymbolTable
{
	Map map;
	Symbol* symbols;
} SymbolTable;

typedef enum IROp
{
	IR_PUSH_INT,
	IR_PUSH_IDENT,
	IR_UNARY,
	IR_BINARY,
	IR_CALL,
	IR_INDEX,
	IR_MEMBER,
	IR_SELECT,
	IR_IF_BEGIN,
	IR_IF_THEN,
	IR_IF_ELSE,
	IR_IF_END,
	IR_BLOCK_BEGIN,
	IR_BLOCK_END,
	IR_STMT_EXPR,
	IR_DECL_BEGIN,
	IR_DECL_TYPE,
	IR_DECL_VAR,
	IR_DECL_ARRAY_BEGIN,
	IR_DECL_ARRAY_UNSIZED,
	IR_DECL_ARRAY_SIZE_BEGIN,
	IR_DECL_ARRAY_SIZE_END,
	IR_DECL_ARRAY_END,
	IR_DECL_INIT_BEGIN,
	IR_DECL_INIT_END,
	IR_DECL_SEPARATOR,
	IR_DECL_END,
	IR_FUNC_BEGIN,
	IR_FUNC_PARAMS_BEGIN,
	IR_FUNC_PARAM_BEGIN,
	IR_FUNC_PARAM_TYPE,
	IR_FUNC_PARAM_NAME,
	IR_FUNC_PARAM_ARRAY_BEGIN,
	IR_FUNC_PARAM_ARRAY_UNSIZED,
	IR_FUNC_PARAM_ARRAY_SIZE_BEGIN,
	IR_FUNC_PARAM_ARRAY_SIZE_END,
	IR_FUNC_PARAM_ARRAY_END,
	IR_FUNC_PARAM_END,
	IR_FUNC_PARAM_SEPARATOR,
	IR_FUNC_PARAMS_END,
	IR_FUNC_PROTOTYPE_END,
	IR_FUNC_DEFINITION_BEGIN,
	IR_FUNC_DEFINITION_END,
	IR_OP_COUNT
} IROp;

typedef enum Tok
{
	TOK_EOF, TOK_IDENTIFIER, TOK_INT,

	TOK_LPAREN, TOK_RPAREN, TOK_LBRACK, TOK_RBRACK, TOK_LBRACE, TOK_RBRACE, TOK_DOT, TOK_COMMA, TOK_SEMI, TOK_QUESTION, TOK_COLON,

	TOK_IF, TOK_ELSE,

	TOK_PLUS, TOK_MINUS, TOK_STAR, TOK_SLASH, TOK_PERCENT,
	TOK_NOT, TOK_TILDE,
	TOK_LT, TOK_LE, TOK_GT, TOK_GE, TOK_EQ, TOK_NE,
	TOK_AND_AND, TOK_OR_OR,
	TOK_ASSIGN,

	TOK_COUNT
} Tok;

const char* tok_name[TOK_COUNT] = {
	[TOK_EOF]        = "EOF",
	[TOK_IDENTIFIER] = "IDENT",
	[TOK_INT]        = "INT",

	[TOK_LPAREN]     = "(",
	[TOK_RPAREN]     = ")",
	[TOK_LBRACK]     = "[",
	[TOK_RBRACK]     = "]",
	[TOK_LBRACE]     = "{",
	[TOK_RBRACE]     = "}",
	[TOK_DOT]        = ".",
	[TOK_COMMA]      = ",",
	[TOK_SEMI]       = ";",
	[TOK_QUESTION]   = "?",
	[TOK_COLON]      = ":",

	[TOK_IF]         = "if",
	[TOK_ELSE]       = "else",

	[TOK_PLUS]       = "+",
	[TOK_MINUS]      = "-",
	[TOK_STAR]       = "*",
	[TOK_SLASH]      = "/",
	[TOK_PERCENT]    = "%",

	[TOK_NOT]        = "!",
	[TOK_TILDE]      = "~",

	[TOK_LT]         = "<",
	[TOK_LE]         = "<=",
	[TOK_GT]         = ">",
	[TOK_GE]         = ">=",
	[TOK_EQ]         = "==",
	[TOK_NE]         = "!=",

	[TOK_AND_AND]    = "&&",
	[TOK_OR_OR]      = "||",

	[TOK_ASSIGN]     = "=",
};

const char* ir_op_name[IR_OP_COUNT] = {
	[IR_PUSH_INT] = "push_int",
	[IR_PUSH_IDENT] = "push_ident",
	[IR_UNARY] = "unary",
	[IR_BINARY] = "binary",
	[IR_CALL] = "call",
	[IR_INDEX] = "index",
	[IR_MEMBER] = "member",
	[IR_SELECT] = "select",
	[IR_IF_BEGIN] = "if_begin",
	[IR_IF_THEN] = "if_then",
	[IR_IF_ELSE] = "if_else",
	[IR_IF_END] = "if_end",
	[IR_BLOCK_BEGIN] = "block_begin",
	[IR_BLOCK_END] = "block_end",
	[IR_STMT_EXPR] = "stmt_expr",
	[IR_DECL_BEGIN] = "decl_begin",
	[IR_DECL_TYPE] = "decl_type",
	[IR_DECL_VAR] = "decl_var",
	[IR_DECL_ARRAY_BEGIN] = "decl_array_begin",
	[IR_DECL_ARRAY_UNSIZED] = "decl_array_unsized",
	[IR_DECL_ARRAY_SIZE_BEGIN] = "decl_array_size_begin",
	[IR_DECL_ARRAY_SIZE_END] = "decl_array_size_end",
	[IR_DECL_ARRAY_END] = "decl_array_end",
	[IR_DECL_INIT_BEGIN] = "decl_init_begin",
	[IR_DECL_INIT_END] = "decl_init_end",
	[IR_DECL_SEPARATOR] = "decl_separator",
	[IR_DECL_END] = "decl_end",
	[IR_FUNC_BEGIN] = "func_begin",
	[IR_FUNC_PARAMS_BEGIN] = "func_params_begin",
	[IR_FUNC_PARAM_BEGIN] = "func_param_begin",
	[IR_FUNC_PARAM_TYPE] = "func_param_type",
	[IR_FUNC_PARAM_NAME] = "func_param_name",
	[IR_FUNC_PARAM_ARRAY_BEGIN] = "func_param_array_begin",
	[IR_FUNC_PARAM_ARRAY_UNSIZED] = "func_param_array_unsized",
	[IR_FUNC_PARAM_ARRAY_SIZE_BEGIN] = "func_param_array_size_begin",
	[IR_FUNC_PARAM_ARRAY_SIZE_END] = "func_param_array_size_end",
	[IR_FUNC_PARAM_ARRAY_END] = "func_param_array_end",
	[IR_FUNC_PARAM_END] = "func_param_end",
	[IR_FUNC_PARAM_SEPARATOR] = "func_param_separator",
	[IR_FUNC_PARAMS_END] = "func_params_end",
	[IR_FUNC_PROTOTYPE_END] = "func_prototype_end",
	[IR_FUNC_DEFINITION_BEGIN] = "func_definition_begin",
	[IR_FUNC_DEFINITION_END] = "func_definition_end",
};

typedef struct IR_Cmd
{
	IROp op;
	IR_String str0;
	IR_String str1;
	int arg0;
	int arg1;
	Tok tok;
	unsigned storage_flags;
	unsigned layout_flags;
	int layout_set;
	int layout_binding;
	int layout_location;
} IR_Cmd;

SymbolTable g_symbols;
IR_Cmd* g_ir;
IR_String current_decl_type;
IR_String current_param_type;

IR_String ir_string(const char* s, int len)
{
	if (!s || len <= 0) return (IR_String){ 0 };
	return (IR_String){ sintern_range(s, s + len), len };
}

IR_Cmd* ir_emit(IROp op)
{
	IR_Cmd inst = (IR_Cmd){ 0 };
	inst.op = op;
	apush(g_ir, inst);
	return &g_ir[acount(g_ir) - 1];
}

void symbol_table_init(SymbolTable* st)
{
	st->map = (Map){ 0 };
	st->symbols = NULL;
}

Symbol* symbol_table_add(SymbolTable* st, IR_String name, IR_String type, SymbolKind kind)
{
	if (!name.ptr) return NULL;
	uint64_t key = (uint64_t)name.ptr;
	uint64_t existing = map_get(st->map, key);
	if (existing) return &st->symbols[(int)existing - 1];
	Symbol sym = (Symbol){ 0 };
	sym.name = name;
	sym.type = type;
	sym.kind = kind;
	apush(st->symbols, sym);
	int idx = acount(st->symbols);
	map_add(st->map, key, (uint64_t)idx);
	return &st->symbols[idx - 1];
}

void symbol_add_storage(Symbol* sym, unsigned flags)
{
	if (!sym) return;
	sym->storage_flags |= flags;
}

int symbol_has_storage(const Symbol* sym, unsigned flag)
{
	return sym && (sym->storage_flags & flag);
}

void symbol_set_layout(Symbol* sym, unsigned layout_flag, int value)
{
	if (!sym) return;
	sym->layout_flags |= layout_flag;
	switch (layout_flag) {
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
	return sym && (sym->layout_flags & layout_flag);
}

int symbol_get_layout(const Symbol* sym, unsigned layout_flag)
{
	if (!symbol_has_layout(sym, layout_flag)) return -1;
	switch (layout_flag) {
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
	if (!sym || !spec) return;
	symbol_add_storage(sym, spec->storage_flags);
	if (spec->layout_flags & SYM_LAYOUT_SET) symbol_set_layout(sym, SYM_LAYOUT_SET, spec->layout_set);
	if (spec->layout_flags & SYM_LAYOUT_BINDING) symbol_set_layout(sym, SYM_LAYOUT_BINDING, spec->layout_binding);
	if (spec->layout_flags & SYM_LAYOUT_LOCATION) symbol_set_layout(sym, SYM_LAYOUT_LOCATION, spec->layout_location);
}

void symbol_table_free(SymbolTable* st)
{
	map_free(st->map);
	if (st->symbols) afree(st->symbols);
	st->map = (Map){ 0 };
	st->symbols = NULL;
}

void dump_storage_flags(unsigned flags)
{
	if (!flags) return;
	printf(" storage=");
	int first = 1;
	if (flags & SYM_STORAGE_IN) {
		printf("%sin", first ? "" : "|");
		first = 0;
}
	if (flags & SYM_STORAGE_OUT) {
		printf("%sout", first ? "" : "|");
		first = 0;
}
	if (flags & SYM_STORAGE_UNIFORM) {
		printf("%suniform", first ? "" : "|");
		first = 0;
}
}

void dump_layout_info(unsigned layout_flags, int set, int binding, int location)
{
	if (!layout_flags) return;
	printf(" layout(");
	int first = 1;
	if (layout_flags & SYM_LAYOUT_SET) {
		printf("%sset=%d", first ? "" : ", ", set);
		first = 0;
}
	if (layout_flags & SYM_LAYOUT_BINDING) {
		printf("%sbinding=%d", first ? "" : ", ", binding);
		first = 0;
}
	if (layout_flags & SYM_LAYOUT_LOCATION) {
		printf("%slocation=%d", first ? "" : ", ", location);
		first = 0;
}
	printf(")");
}

void dump_ir()
{
	printf("IR:\n");
	for (int i = 0; i < acount(g_ir); ++i) {
		IR_Cmd* inst = &g_ir[i];
		printf("  %s", ir_op_name[inst->op]);
		switch (inst->op) {
		case IR_DECL_BEGIN:
		case IR_FUNC_PARAM_BEGIN:
			dump_storage_flags(inst->storage_flags);
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
			break;
		case IR_PUSH_INT:
			printf(" %d", inst->arg0);
			break;
		case IR_PUSH_IDENT:
		case IR_MEMBER:
		case IR_DECL_TYPE:
		case IR_DECL_VAR:
		case IR_FUNC_PARAM_TYPE:
		case IR_FUNC_PARAM_NAME:
			printf(" %.*s", inst->str0.len, inst->str0.ptr);
			break;
		case IR_UNARY:
		case IR_BINARY:
			printf(" %s", tok_name[inst->tok]);
			break;
		case IR_CALL:
			printf(" argc=%d", inst->arg0);
			break;
		case IR_FUNC_BEGIN:
			printf(" return=%.*s name=%.*s", inst->str0.len, inst->str0.ptr, inst->str1.len, inst->str1.ptr);
			dump_storage_flags(inst->storage_flags);
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
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
	for (int i = 0; i < acount(st->symbols); ++i) {
		const Symbol* sym = &st->symbols[i];
		printf("  %s %.*s : %.*s", symbol_kind_name[sym->kind], sym->name.len, sym->name.ptr, sym->type.len, sym->type.ptr);
		dump_storage_flags(sym->storage_flags);
		dump_layout_info(sym->layout_flags, sym->layout_set, sym->layout_binding, sym->layout_location);
		printf("\n");
	}
}


typedef enum Prec
{
	PREC_EXPR    = 0,
	PREC_ASSIGN  = 10,  // right-assoc
	PREC_TERNARY = 20,  // ?:
	PREC_OR_OR   = 30,
	PREC_AND_AND = 40,
	PREC_EQ      = 80,  // == !=
	PREC_REL     = 90,  // < <= > >=
	PREC_ADD     = 100, // + -
	PREC_MUL     = 110, // * / %
	PREC_POSTFIX = 120, // () [] .
	PREC_UNARY   = 130  // prefix + - ! ~
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
	const char* lexeme;
	int len;
} tok;

void parse_error(const char* msg)
{
	fprintf(stderr, "Parse error: %s", msg);
	if (tok.kind < TOK_COUNT) {
		fprintf(stderr, " (token %s", tok_name[tok.kind]);
		if (tok.lexeme && tok.len) {
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
	if (tok.kind != k) parse_error("expected token");
	next();
}

void next_ch() { ch = *at ? (unsigned char)*at++ : 0; }
int is_space(int c) { return c == ' ' || c == '\t' || c == '\r' || c == '\n'; }
int is_alpha(int c) { return (c == '_') || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'); }
int is_digit(int c) { return (c >= '0' && c <= '9'); }
int match_ch(int want) { if (ch == want) { next_ch(); return 1; } return 0; }

void skip_ws_comments()
{
	while (1) {
		while (is_space(ch)) next_ch();
		if (ch == '/' && at[0] == '/') {
			while (ch && ch != '\n') {
				next_ch();
			}
			continue;
		}
		if (ch == '/' && at[0] == '*') {
			next_ch();
			next_ch();
			while (ch && !(ch == '*' && at[0] == '/')) next_ch();
			if (ch == '*') {
				next_ch();
				next_ch();
			}
			continue;
		}
		break;
	}
}

int str_eq_n(const char* s, int n, const char* kw)
{
	size_t klen = strlen(kw);
	return n == (int)klen && strncmp(s, kw, n) == 0;
}

unsigned storage_flag_from_keyword(const char* s, int n)
{
	if (str_eq_n(s, n, "in")) return SYM_STORAGE_IN;
	if (str_eq_n(s, n, "out")) return SYM_STORAGE_OUT;
	if (str_eq_n(s, n, "uniform")) return SYM_STORAGE_UNIFORM;
	return 0;
}

unsigned layout_flag_from_keyword(const char* s, int n)
{
	if (str_eq_n(s, n, "set")) return SYM_LAYOUT_SET;
	if (str_eq_n(s, n, "binding")) return SYM_LAYOUT_BINDING;
	if (str_eq_n(s, n, "location")) return SYM_LAYOUT_LOCATION;
	return 0;
}

void type_spec_add_storage(TypeSpec* spec, unsigned flags)
{
	if (!spec) return;
	spec->storage_flags |= flags;
}

void type_spec_set_layout(TypeSpec* spec, unsigned layout_flag, int value)
{
	if (!spec) return;
	spec->layout_flags |= layout_flag;
	switch (layout_flag) {
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

int is_type_name(const char* s, int n)
{
	static const char* type_names[] = {
		"void", "float", "int", "uint", "bool",
		"vec2", "vec3", "vec4",
		"ivec2", "ivec3", "ivec4",
		"uvec2", "uvec3", "uvec4",
		"bvec2", "bvec3", "bvec4",
		"mat2", "mat3", "mat4",
		"mat2x3", "mat2x4", "mat3x2", "mat3x4", "mat4x2", "mat4x3",
		"sampler2D", "samplerCube", "sampler2DArray",
	};
	for (size_t i = 0; i < sizeof(type_names) / sizeof(type_names[0]); ++i) {
		if (str_eq_n(s, n, type_names[i])) {
			return 1;
		}
	}
	return 0;
}

int is_type_token()
{
	if (tok.kind != TOK_IDENTIFIER) return 0;
	if (is_type_name(tok.lexeme, tok.len)) return 1;
	if (storage_flag_from_keyword(tok.lexeme, tok.len)) return 1;
	if (str_eq_n(tok.lexeme, tok.len, "layout")) return 1;
	return 0;
}

void parse_layout_block(TypeSpec* spec)
{
	if (!spec) return;
	next();
	expect(TOK_LPAREN);
	while (tok.kind != TOK_RPAREN) {
		if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier in layout");
		unsigned layout_flag = layout_flag_from_keyword(tok.lexeme, tok.len);
		if (!layout_flag) parse_error("unknown layout identifier");
		next();
		expect(TOK_ASSIGN);
		if (tok.kind != TOK_INT) parse_error("expected integer in layout assignment");
		type_spec_set_layout(spec, layout_flag, tok.int_val);
		next();
		if (tok.kind == TOK_COMMA) {
			next();
			continue;
		}
		break;
	}
	expect(TOK_RPAREN);
}

void parse_type_qualifiers(TypeSpec* spec)
{
	while (tok.kind == TOK_IDENTIFIER) {
		unsigned storage_flag = storage_flag_from_keyword(tok.lexeme, tok.len);
		if (storage_flag) {
			type_spec_add_storage(spec, storage_flag);
			next();
			continue;
		}
		if (str_eq_n(tok.lexeme, tok.len, "layout")) {
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
	if (tok.kind != TOK_IDENTIFIER || !is_type_name(tok.lexeme, tok.len)) parse_error("expected type");
	spec.type = ir_string(tok.lexeme, tok.len);
	next();
	return spec;
}

void expr_binary(Prec min_prec);
void expr() { expr_binary(PREC_EXPR); }
void expr_error() { parse_error("unexpected token in expression"); }

void stmt();
void stmt_decl();
void stmt_block();
void parse();

void decl_array_suffix()
{
	while (tok.kind == TOK_LBRACK) {
		next();
		ir_emit(IR_DECL_ARRAY_BEGIN);
		if (tok.kind == TOK_RBRACK) {
			ir_emit(IR_DECL_ARRAY_UNSIZED);
		} else {
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
	while (tok.kind == TOK_LBRACK) {
		next();
		ir_emit(IR_FUNC_PARAM_ARRAY_BEGIN);
		if (tok.kind == TOK_RBRACK) {
			ir_emit(IR_FUNC_PARAM_ARRAY_UNSIZED);
		} else {
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
	if (!is_type_token()) parse_error("expected type in parameter");
	TypeSpec spec = parse_type_specifier();
	IR_Cmd* param = ir_emit(IR_FUNC_PARAM_BEGIN);
	param->storage_flags = spec.storage_flags;
	param->layout_flags = spec.layout_flags;
	param->layout_set = spec.layout_set;
	param->layout_binding = spec.layout_binding;
	param->layout_location = spec.layout_location;
	current_param_type = spec.type;
	IR_Cmd* inst = ir_emit(IR_FUNC_PARAM_TYPE);
	inst->str0 = spec.type;
	if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier in parameter");
	IR_String name = ir_string(tok.lexeme, tok.len);
	inst = ir_emit(IR_FUNC_PARAM_NAME);
	inst->str0 = name;
	Symbol* sym = symbol_table_add(&g_symbols, name, current_param_type, SYM_PARAM);
	symbol_apply_type_spec(sym, &spec);
	next();
	func_param_array_suffix();
	ir_emit(IR_FUNC_PARAM_END);
}

void func_param_list()
{
	ir_emit(IR_FUNC_PARAMS_BEGIN);
	if (tok.kind != TOK_RPAREN) {
		while (1) {
			func_param();
			if (tok.kind == TOK_COMMA) {
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

void global_var_decl(TypeSpec spec, IR_String first_name)
{
	IR_Cmd* inst;
	IR_Cmd* begin = ir_emit(IR_DECL_BEGIN);
	begin->storage_flags = spec.storage_flags;
	begin->layout_flags = spec.layout_flags;
	begin->layout_set = spec.layout_set;
	begin->layout_binding = spec.layout_binding;
	begin->layout_location = spec.layout_location;
	current_decl_type = spec.type;
	inst = ir_emit(IR_DECL_TYPE);
	inst->str0 = spec.type;
	inst = ir_emit(IR_DECL_VAR);
	inst->str0 = first_name;
	Symbol* sym = symbol_table_add(&g_symbols, first_name, current_decl_type, SYM_VAR);
	symbol_apply_type_spec(sym, &spec);
	decl_array_suffix();
	if (tok.kind == TOK_ASSIGN) {
	next();
	ir_emit(IR_DECL_INIT_BEGIN);
	expr();
	ir_emit(IR_DECL_INIT_END);
}
	while (tok.kind == TOK_COMMA) {
	next();
	ir_emit(IR_DECL_SEPARATOR);
	if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier in declaration");
	IR_String name = ir_string(tok.lexeme, tok.len);
	inst = ir_emit(IR_DECL_VAR);
	inst->str0 = name;
	sym = symbol_table_add(&g_symbols, name, current_decl_type, SYM_VAR);
	symbol_apply_type_spec(sym, &spec);
	next();
	decl_array_suffix();
	if (tok.kind == TOK_ASSIGN) {
	next();
	ir_emit(IR_DECL_INIT_BEGIN);
	expr();
	ir_emit(IR_DECL_INIT_END);
}
}
	expect(TOK_SEMI);
	ir_emit(IR_DECL_END);
	current_decl_type = (IR_String){ 0 };
}

void func_decl_or_def(TypeSpec spec, IR_String name)
{
	IR_Cmd* func = ir_emit(IR_FUNC_BEGIN);
	func->str0 = spec.type;
	func->str1 = name;
	func->storage_flags = spec.storage_flags;
	func->layout_flags = spec.layout_flags;
	func->layout_set = spec.layout_set;
	func->layout_binding = spec.layout_binding;
	func->layout_location = spec.layout_location;
	Symbol* sym = symbol_table_add(&g_symbols, func->str1, func->str0, SYM_FUNC);
	symbol_apply_type_spec(sym, &spec);
	expect(TOK_LPAREN);
	func_param_list();
	if (tok.kind == TOK_SEMI) {
	next();
	ir_emit(IR_FUNC_PROTOTYPE_END);
		return;
}
	if (tok.kind == TOK_LBRACE) {
	ir_emit(IR_FUNC_DEFINITION_BEGIN);
	stmt_block();
	ir_emit(IR_FUNC_DEFINITION_END);
		return;
}
	parse_error("expected ';' or function body");
}

void stmt_decl()
{
	TypeSpec spec = parse_type_specifier();
	IR_Cmd* inst;
	IR_Cmd* begin = ir_emit(IR_DECL_BEGIN);
	begin->storage_flags = spec.storage_flags;
	begin->layout_flags = spec.layout_flags;
	begin->layout_set = spec.layout_set;
	begin->layout_binding = spec.layout_binding;
	begin->layout_location = spec.layout_location;
	current_decl_type = spec.type;
	inst = ir_emit(IR_DECL_TYPE);
	inst->str0 = spec.type;
	while (1) {
	if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier in declaration");
	IR_String name = ir_string(tok.lexeme, tok.len);
	inst = ir_emit(IR_DECL_VAR);
	inst->str0 = name;
	Symbol* sym = symbol_table_add(&g_symbols, name, current_decl_type, SYM_VAR);
	symbol_apply_type_spec(sym, &spec);
	next();
	decl_array_suffix();
	if (tok.kind == TOK_ASSIGN) {
	next();
	ir_emit(IR_DECL_INIT_BEGIN);
	expr();
	ir_emit(IR_DECL_INIT_END);
}
	if (tok.kind == TOK_COMMA) {
	next();
	ir_emit(IR_DECL_SEPARATOR);
	continue;
}
	break;
}
	expect(TOK_SEMI);
	ir_emit(IR_DECL_END);
	current_decl_type = (IR_String){ 0 };
}

void expr_int()
{
	IR_Cmd* inst = ir_emit(IR_PUSH_INT);
	inst->arg0 = tok.int_val;
	next();
}

void expr_ident()
{
	IR_Cmd* inst = ir_emit(IR_PUSH_IDENT);
	inst->str0 = ir_string(tok.lexeme, tok.len);
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
	int argc = 0;
	if (tok.kind != TOK_RPAREN) {
		expr(); argc++;
		while (tok.kind == TOK_COMMA) { next(); expr(); argc++; }
		expect(TOK_RPAREN);
	} else {
		next(); // consume ')'
	}
	IR_Cmd* inst = ir_emit(IR_CALL);
	inst->arg0 = argc;
}

void expr_index()
{
	expr(); // parse index expr
	expect(TOK_RBRACK);
	ir_emit(IR_INDEX);
}

void expr_member()
{
	if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier after '.'");
	IR_Cmd* inst = ir_emit(IR_MEMBER);
	inst->str0 = ir_string(tok.lexeme, tok.len);
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

#define EXPR_UNARY(name, token_enum) void expr_##name() { expr_unary_common(TOK_##token_enum); }
#define EXPR_BINARY(name, precname, token_enum) void expr_##name() { expr_binary_left(TOK_##token_enum, PREC_##precname); }

EXPR_UNARY(neg,  MINUS);
EXPR_UNARY(pos,  PLUS);
EXPR_UNARY(not,  NOT);
EXPR_UNARY(bnot, TILDE);

EXPR_BINARY(add,    ADD,      PLUS);
EXPR_BINARY(sub,    ADD,      MINUS);
EXPR_BINARY(mul,    MUL,      STAR);
EXPR_BINARY(div,    MUL,      SLASH);
EXPR_BINARY(mod,    MUL,      PERCENT);
EXPR_BINARY(lt,     REL,      LT);
EXPR_BINARY(le,     REL,      LE);
EXPR_BINARY(gt,     REL,      GT);
EXPR_BINARY(ge,     REL,      GE);
EXPR_BINARY(eq,     EQ,       EQ);
EXPR_BINARY(ne,     EQ,       NE);
EXPR_BINARY(land,   AND_AND,  AND_AND);
EXPR_BINARY(lor,    OR_OR,    OR_OR);
EXPR_BINARY(assign, ASSIGN,   ASSIGN);

void expr_binary(Prec min_prec)
{
	tok.lexpr(); // start: number/ident/paren/unary...

	while (tok.prec > min_prec) {
		void (*cont)() = tok.rexpr;
		next(); // consume operator -> next token begins RHS
		cont(); // parse RHS/args/member and "emit"
	}
}

void stmt_block()
{
	expect(TOK_LBRACE);
	ir_emit(IR_BLOCK_BEGIN);
	while (tok.kind != TOK_RBRACE && tok.kind != TOK_EOF) {
		stmt();
	}
	expect(TOK_RBRACE);
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
	stmt();
	if (tok.kind == TOK_ELSE) {
		next();
		ir_emit(IR_IF_ELSE);
		stmt();
	}
	ir_emit(IR_IF_END);
}

void stmt_expr()
{
	expr();
	expect(TOK_SEMI);
	ir_emit(IR_STMT_EXPR);
}

void stmt()
{
	if (is_type_token()) {
		stmt_decl();
		return;
	}
	switch (tok.kind) {
	case TOK_IF:
		stmt_if();
		break;
	case TOK_LBRACE:
		stmt_block();
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
	if (!is_type_token()) parse_error("expected type at top level");
	TypeSpec type_spec = parse_type_specifier();
	if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier after type");
	IR_String name = ir_string(tok.lexeme, tok.len);
	next();
	if (tok.kind == TOK_LPAREN) {
		func_decl_or_def(type_spec, name);
		return;
}
	global_var_decl(type_spec, name);
}

void parse()
{
	while (tok.kind != TOK_EOF) {
		top_level();
	}
}

#define TOK_CHAR(ch1, tok1) \
	case ch1: next_ch(); tok.kind = TOK_##tok1; tok.prec = 0; tok.lexpr = expr_error; tok.rexpr = expr_error; break;

#define TOK_EXPR(ch1, tok1, prec1, lexpr1, rexpr1) \
	case ch1: next_ch(); tok.kind = TOK_##tok1; tok.prec = PREC_##prec1; tok.lexpr = expr_##lexpr1; tok.rexpr = expr_##rexpr1; break;

#define TOK_EXPR_EXPR(ch1, tok1, prec1, lexpr1, rexpr1, ch2, tok2, prec2, lexpr2, rexpr2) \
	case ch1: next_ch(); \
		if (match_ch(ch2)) { tok.kind = TOK_##tok2; tok.prec = PREC_##prec2; tok.lexpr = expr_##lexpr2; tok.rexpr = expr_##rexpr2; } \
		else               { tok.kind = TOK_##tok1; tok.prec = PREC_##prec1; tok.lexpr = expr_##lexpr1; tok.rexpr = expr_##rexpr1; } \
		break;

void next()
{
	tok.kind = TOK_EOF;
	tok.prec = 0;
	tok.lexpr = expr_error;
	tok.rexpr = expr_error;
	tok.lexeme = at ? at - 1 : NULL;
	tok.len = 0;

	skip_ws_comments();

	switch (ch) {
		// single-char punctuation
		TOK_CHAR(  0 , EOF)
		TOK_CHAR( ')', RPAREN)
		TOK_CHAR( ']', RBRACK)
		TOK_CHAR( '}', RBRACE)
		TOK_CHAR( ',', COMMA)
		TOK_CHAR( ';', SEMI)
		TOK_CHAR( ':', COLON)
		TOK_EXPR( '(', LPAREN,   POSTFIX, paren,  call )
		TOK_EXPR( '[', LBRACK,   POSTFIX, error,  index)
		TOK_CHAR( '{', LBRACE)
		TOK_EXPR( '.', DOT,      POSTFIX, error,  member)

		// prefix + binary-ish
		TOK_EXPR( '~', TILDE,    UNARY,   bnot,   error)
		TOK_EXPR( '+', PLUS,     ADD,     pos,    add )
		TOK_EXPR( '-', MINUS,    ADD,     neg,    sub )
		TOK_EXPR( '*', STAR,     MUL,     error,  mul )
		TOK_EXPR( '/', SLASH,    MUL,     error,  div )
		TOK_EXPR( '%', PERCENT,  MUL,     error,  mod )
		TOK_EXPR( '?', QUESTION, TERNARY, error,  ternary )

		// two-char combos
		TOK_EXPR_EXPR('<', LT,     REL,    error, lt,     '=', LE,  REL, error,   le)
		TOK_EXPR_EXPR('>', GT,     REL,    error, gt,     '=', GE,  REL, error,   ge)
		TOK_EXPR_EXPR('=', ASSIGN, ASSIGN, error, assign, '=', EQ,  EQ,  error,   eq)
		TOK_EXPR_EXPR('!', NOT,    UNARY,  not,   error,       '=', NE,  EQ,      error, ne)

		// && and ||
		TOK_EXPR_EXPR('&', NOT,    UNARY,  error, error,  '&', AND_AND,  AND_AND, error, land)
		TOK_EXPR_EXPR('|', NOT,    UNARY,  error, error,  '|', OR_OR,    OR_OR,   error, lor)

		default: break;
	}

	if (tok.kind != TOK_EOF) return;

	// identifiers
	if (is_alpha(ch)) {
		const char* s = at - 1;
		while (is_alpha(ch) || is_digit(ch)) next_ch();
		tok.len = (int)((at - 1) - s);
		tok.lexeme = s;
		tok.kind = TOK_IDENTIFIER;
		tok.prec = 0;
		tok.lexpr = expr_ident;
		tok.rexpr = expr_error;
		if (tok.len == 2 && strncmp(s, "if", 2) == 0) {
			tok.kind = TOK_IF;
			tok.lexpr = expr_error;
		}
		else if (tok.len == 4 && strncmp(s, "else", 4) == 0) {
			tok.kind = TOK_ELSE;
			tok.lexpr = expr_error;
		}
		return;
	}

	// integers (decimal)
	if (is_digit(ch)) {
		int v = (ch - '0'); next_ch();
		while (is_digit(ch)) { v = v * 10 + (ch - '0'); next_ch(); }
		tok.kind = TOK_INT;
		tok.prec = 0;
		tok.lexpr = expr_int;
		tok.rexpr = expr_error;
		tok.int_val = v;
		return;
	}

	// unknown char: consume and retry
	if (ch) {
		fprintf(stderr, "Unknown char: '%c'\n", ch);
		next_ch();
		next();
		return;
	}
}

int main()
{
#define STR(X) #X
	const char* input = STR(
		layout(location = 0) in vec2 in_pos;
		layout(location = 1) in vec2 in_uv;
		layout(location = 0) out vec2 v_uv;
		layout(location = 1) out vec4 v_col;
		layout(set = 0, binding = 0) uniform sampler2D u_texture;
		layout(set = 1, binding = 0) uniform sampler2D u_fallback;
		void main_image(layout(location = 0) in vec2 uv, layout(location = 1) in vec4 color)
		{
			v_uv = uv;
			vec4 sample = texture(u_texture, uv);
			if (sample.a == 0) {
				v_col = texture(u_fallback, uv);
			} else {
				v_col = color;
			}
		}
	);
	printf("Input : %s\n\n", input);

	in = input;
	at = in;
	next_ch();
	next();
	symbol_table_init(&g_symbols);
	parse();
	dump_ir();
	printf("\n");
	dump_symbols(&g_symbols);
	symbol_table_free(&g_symbols);
	if (g_ir) afree(g_ir);

	return 0;
}
