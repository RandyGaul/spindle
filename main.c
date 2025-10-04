#define _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_DEPRECATE

#include <stddef.h>
#include <stdlib.h>
#include <assert.h>

#define CKIT_IMPLEMENTATION
#include "ckit.h"

typedef enum SymbolKind
{
	SYM_VAR,
	SYM_FUNC,
	SYM_PARAM,
	SYM_KIND_COUNT
} SymbolKind;

const char* symbol_kind_name[SYM_KIND_COUNT] = {
	[SYM_VAR]   = "var",
	[SYM_FUNC]  = "func",
	[SYM_PARAM] = "param",
};

typedef enum TypeTag
{
	T_VOID,
	T_BOOL,
	T_INT,
	T_UINT,
	T_FLOAT,
	T_DOUBLE,
	T_VEC,
	T_MAT,
	T_SAMPLER,
	T_IMAGE,
	T_ARRAY,
	T_STRUCT,
	T_TYPE_COUNT
} TypeTag;

typedef struct Type
{
	TypeTag tag;
	uint8_t cols;
	uint8_t rows;
	uint8_t base;
	uint8_t dim;
	int array_len;
	void* user;
	const char* name;
} Type;

typedef struct Symbol
{
	const char* name;
	const char* type_name;
	Type* type;
	SymbolKind kind;
	unsigned storage_flags;
	unsigned layout_flags;
	int layout_set;
	int layout_binding;
	int layout_location;
	int scope_depth;
} Symbol;

typedef enum SymbolStorage
{
	SYM_STORAGE_IN      = 1 << 0,
	SYM_STORAGE_OUT     = 1 << 1,
	SYM_STORAGE_UNIFORM = 1 << 2,
} SymbolStorage;

typedef enum SymbolLayout
{
	SYM_LAYOUT_SET      = 1 << 0,
	SYM_LAYOUT_BINDING  = 1 << 1,
	SYM_LAYOUT_LOCATION = 1 << 2,
} SymbolLayout;

typedef struct TypeSpec
{
	const char* type_name;
	Type* type;
	unsigned storage_flags;
	unsigned layout_flags;
	int layout_set;
	int layout_binding;
	int layout_location;
} TypeSpec;

typedef struct SymbolScope
{
	Map map;
} SymbolScope;

typedef struct SymbolTable
{
	Symbol* symbols;
	SymbolScope* scopes;
} SymbolTable;

typedef struct TypeSystem
{
	Map map;
	Type* types;
} TypeSystem;

typedef enum IR_Op
{
	IR_PUSH_INT,
	IR_PUSH_IDENT,
	IR_PUSH_FLOAT,
	IR_UNARY,
	IR_BINARY,
	IR_CALL,
	IR_CONSTRUCT,
	IR_INDEX,
	IR_MEMBER,
	IR_SWIZZLE,
	IR_SELECT,
	IR_IF_BEGIN,
	IR_IF_THEN,
	IR_IF_ELSE,
	IR_IF_END,
	IR_BLOCK_BEGIN,
	IR_BLOCK_END,
	IR_STMT_EXPR,
	IR_RETURN,
	IR_BREAK,
	IR_CONTINUE,
	IR_DISCARD,
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
} IR_Op;

typedef enum Tok
{
        TOK_EOF, TOK_IDENTIFIER, TOK_INT, TOK_FLOAT,

	TOK_LPAREN, TOK_RPAREN, TOK_LBRACK, TOK_RBRACK, TOK_LBRACE, TOK_RBRACE, TOK_DOT, TOK_COMMA, TOK_SEMI, TOK_QUESTION, TOK_COLON,

	TOK_IF, TOK_ELSE, TOK_RETURN, TOK_BREAK, TOK_CONTINUE, TOK_DISCARD,

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
	[TOK_FLOAT]      = "FLOAT",

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
	[TOK_RETURN]     = "return",
	[TOK_BREAK]      = "break",
	[TOK_CONTINUE]   = "continue",
	[TOK_DISCARD]    = "discard",

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
	[IR_PUSH_FLOAT] = "push_float",
	[IR_UNARY] = "unary",
	[IR_BINARY] = "binary",
	[IR_CALL] = "call",
	[IR_CONSTRUCT] = "construct",
	[IR_INDEX] = "index",
	[IR_MEMBER] = "member",
	[IR_SWIZZLE] = "swizzle",
	[IR_SELECT] = "select",
	[IR_IF_BEGIN] = "if_begin",
	[IR_IF_THEN] = "if_then",
	[IR_IF_ELSE] = "if_else",
	[IR_IF_END] = "if_end",
	[IR_BLOCK_BEGIN] = "block_begin",
	[IR_BLOCK_END] = "block_end",
	[IR_STMT_EXPR] = "stmt_expr",
	[IR_RETURN] = "return",
	[IR_BREAK] = "break",
	[IR_CONTINUE] = "continue",
	[IR_DISCARD] = "discard",
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
	IR_Op op;
	const char* str0;
	const char* str1;
	Type* type;
	int arg0;
	int arg1;
	double float_val;
	Tok tok;
	unsigned storage_flags;
	unsigned layout_flags;
	int layout_set;
	int layout_binding;
	int layout_location;
} IR_Cmd;

TypeSystem g_types;
SymbolTable g_symbols;
IR_Cmd* g_ir;
const char* current_decl_type_name;
Type* current_decl_type_type;
const char* current_param_type_name;
Type* current_param_type_type;
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
}

IR_Cmd* ir_emit(IR_Op op)
{
	IR_Cmd inst = (IR_Cmd){ 0 };
	inst.op = op;
	apush(g_ir, inst);
	return &g_ir[acount(g_ir) - 1];
}

void symbol_table_enter_scope(SymbolTable* st)
{
	SymbolScope scope = (SymbolScope){ 0 };
	apush(st->scopes, scope);
}

void symbol_table_leave_scope(SymbolTable* st)
{
	int count = acount(st->scopes);
	if (!count) return;
	SymbolScope* scope = &st->scopes[count - 1];
	map_free(scope->map);
	scope->map = (Map){ 0 };
	apop(st->scopes);
}

void symbol_table_init(SymbolTable* st)
{
	st->symbols = NULL;
	st->scopes = NULL;
	symbol_table_enter_scope(st);
}

Symbol* symbol_table_add(SymbolTable* st, const char* name, const char* type_name, Type* type, SymbolKind kind)
{
	int depth = acount(st->scopes);
	SymbolScope* scope = &st->scopes[depth - 1];
	uint64_t key = (uint64_t)name;
	uint64_t existing = map_get(scope->map, key);
	if (existing) return &st->symbols[(int)existing - 1];
	Symbol sym = (Symbol){ 0 };
	sym.name = name;
	sym.type_name = type_name;
	sym.type = type;
	sym.kind = kind;
	sym.scope_depth = depth - 1;
	apush(st->symbols, sym);
	int idx = acount(st->symbols);
	map_add(scope->map, key, (uint64_t)idx);
	return &st->symbols[idx - 1];
}

Symbol* symbol_table_resolve(SymbolTable* st, const char* name)
{
	for (int i = acount(st->scopes) - 1; i >= 0; --i) {
		SymbolScope* scope = &st->scopes[i];
		uint64_t idx = map_get(scope->map, (uint64_t)name);
		if (idx) return &st->symbols[(int)idx - 1];
	}
	return NULL;
}

const char* type_tag_name(TypeTag tag)
{
	static const char* names[T_TYPE_COUNT] = {
		[T_VOID] = "void",
		[T_BOOL] = "bool",
		[T_INT] = "int",
		[T_UINT] = "uint",
		[T_FLOAT] = "float",
		[T_DOUBLE] = "double",
		[T_VEC] = "vec",
		[T_MAT] = "mat",
		[T_SAMPLER] = "sampler",
		[T_IMAGE] = "image",
		[T_ARRAY] = "array",
		[T_STRUCT] = "struct",
	};
	if (tag < 0 || tag >= T_TYPE_COUNT) return "unknown";
	return names[tag] ? names[tag] : "unknown";
}

static Type* type_system_add_internal(TypeSystem* ts, const char* name, Type type)
{
	uint64_t key = (uint64_t)name;
	uint64_t existing = map_get(ts->map, key);
	if (existing) return &ts->types[(int)existing - 1];
	type.name = name;
	apush(ts->types, type);
	int idx = acount(ts->types);
	map_add(ts->map, key, (uint64_t)idx);
	return &ts->types[idx - 1];
}

Type* type_system_get(TypeSystem* ts, const char* name)
{
	uint64_t idx = map_get(ts->map, (uint64_t)name);
	if (!idx) return NULL;
	return &ts->types[(int)idx - 1];
}

Type* type_system_declare(TypeSystem* ts, const char* name, Type type)
{
	return type_system_add_internal(ts, name, type);
}

void type_system_init_builtins(TypeSystem* ts)
{
	typedef struct TypeInit
	{
		const char* name;
		Type type;
	} TypeInit;
	const TypeInit builtins[] = {
		{ "void",     { .tag = T_VOID, .cols = 1, .rows = 1, .base = T_VOID,   .array_len = 0 } },
		{ "bool",     { .tag = T_BOOL, .cols = 1, .rows = 1, .base = T_BOOL,   .array_len = 0 } },
		{ "int",      { .tag = T_INT,  .cols = 1, .rows = 1, .base = T_INT,    .array_len = 0 } },
		{ "uint",     { .tag = T_UINT, .cols = 1, .rows = 1, .base = T_UINT,   .array_len = 0 } },
		{ "float",    { .tag = T_FLOAT, .cols = 1, .rows = 1, .base = T_FLOAT,  .array_len = 0 } },
		{ "double",   { .tag = T_DOUBLE, .cols = 1, .rows = 1, .base = T_DOUBLE, .array_len = 0 } },
		{ "vec2",     { .tag = T_VEC,  .cols = 2, .rows = 1, .base = T_FLOAT,  .array_len = 0 } },
		{ "vec3",     { .tag = T_VEC,  .cols = 3, .rows = 1, .base = T_FLOAT,  .array_len = 0 } },
		{ "vec4",     { .tag = T_VEC,  .cols = 4, .rows = 1, .base = T_FLOAT,  .array_len = 0 } },
		{ "ivec2",    { .tag = T_VEC,  .cols = 2, .rows = 1, .base = T_INT,    .array_len = 0 } },
		{ "ivec3",    { .tag = T_VEC,  .cols = 3, .rows = 1, .base = T_INT,    .array_len = 0 } },
		{ "ivec4",    { .tag = T_VEC,  .cols = 4, .rows = 1, .base = T_INT,    .array_len = 0 } },
		{ "uvec2",    { .tag = T_VEC,  .cols = 2, .rows = 1, .base = T_UINT,   .array_len = 0 } },
		{ "uvec3",    { .tag = T_VEC,  .cols = 3, .rows = 1, .base = T_UINT,   .array_len = 0 } },
		{ "uvec4",    { .tag = T_VEC,  .cols = 4, .rows = 1, .base = T_UINT,   .array_len = 0 } },
		{ "bvec2",    { .tag = T_VEC,  .cols = 2, .rows = 1, .base = T_BOOL,   .array_len = 0 } },
		{ "bvec3",    { .tag = T_VEC,  .cols = 3, .rows = 1, .base = T_BOOL,   .array_len = 0 } },
		{ "bvec4",    { .tag = T_VEC,  .cols = 4, .rows = 1, .base = T_BOOL,   .array_len = 0 } },
		{ "mat2",     { .tag = T_MAT,  .cols = 2, .rows = 2, .base = T_FLOAT,  .array_len = 0 } },
		{ "mat3",     { .tag = T_MAT,  .cols = 3, .rows = 3, .base = T_FLOAT,  .array_len = 0 } },
		{ "mat4",     { .tag = T_MAT,  .cols = 4, .rows = 4, .base = T_FLOAT,  .array_len = 0 } },
		{ "mat2x3",   { .tag = T_MAT,  .cols = 2, .rows = 3, .base = T_FLOAT,  .array_len = 0 } },
		{ "mat2x4",   { .tag = T_MAT,  .cols = 2, .rows = 4, .base = T_FLOAT,  .array_len = 0 } },
		{ "mat3x2",   { .tag = T_MAT,  .cols = 3, .rows = 2, .base = T_FLOAT,  .array_len = 0 } },
		{ "mat3x4",   { .tag = T_MAT,  .cols = 3, .rows = 4, .base = T_FLOAT,  .array_len = 0 } },
		{ "mat4x2",   { .tag = T_MAT,  .cols = 4, .rows = 2, .base = T_FLOAT,  .array_len = 0 } },
		{ "mat4x3",   { .tag = T_MAT,  .cols = 4, .rows = 3, .base = T_FLOAT,  .array_len = 0 } },
		{ "sampler2D",    { .tag = T_SAMPLER, .cols = 1, .rows = 1, .base = T_FLOAT, .dim = 2, .array_len = 0 } },
		{ "samplerCube",  { .tag = T_SAMPLER, .cols = 1, .rows = 1, .base = T_FLOAT, .dim = 4, .array_len = 0 } },
		{ "sampler2DArray",{ .tag = T_SAMPLER, .cols = 1, .rows = 1, .base = T_FLOAT, .dim = 5, .array_len = 0 } },
	};
	for (size_t i = 0; i < sizeof(builtins) / sizeof(builtins[0]); ++i) {
		const char* name = sintern_range(builtins[i].name, builtins[i].name + strlen(builtins[i].name));
		type_system_add_internal(ts, name, builtins[i].type);
	}
}

void type_system_free(TypeSystem* ts)
{
	map_free(ts->map);
	ts->map = (Map){ 0 };
	if (ts->types) {
		afree(ts->types);
		ts->types = NULL;
	}
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
	default: break;
	}
}

int symbol_has_layout(const Symbol* sym, unsigned layout_flag)
{
	return (sym->layout_flags & layout_flag) != 0;
}

int symbol_get_layout(const Symbol* sym, unsigned layout_flag)
{
	if (!symbol_has_layout(sym, layout_flag)) return -1;
	switch (layout_flag) {
	case SYM_LAYOUT_SET: return sym->layout_set;
	case SYM_LAYOUT_BINDING: return sym->layout_binding;
	case SYM_LAYOUT_LOCATION: return sym->layout_location;
	default: break;
	}
	return -1;
}

void symbol_apply_type_spec(Symbol* sym, const TypeSpec* spec)
{
	symbol_add_storage(sym, spec->storage_flags);
	if (spec->layout_flags & SYM_LAYOUT_SET) symbol_set_layout(sym, SYM_LAYOUT_SET, spec->layout_set);
	if (spec->layout_flags & SYM_LAYOUT_BINDING) symbol_set_layout(sym, SYM_LAYOUT_BINDING, spec->layout_binding);
	if (spec->layout_flags & SYM_LAYOUT_LOCATION) symbol_set_layout(sym, SYM_LAYOUT_LOCATION, spec->layout_location);
}

void symbol_table_free(SymbolTable* st)
{
	while (acount(st->scopes) > 0) {
		symbol_table_leave_scope(st);
	}
	afree(st->scopes);
	st->scopes = NULL;
	afree(st->symbols);
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
	for (int i = 0; i < acount(st->symbols); ++i) {
		const Symbol* sym = &st->symbols[i];
		printf("  scope[%d] %s %s : %s", sym->scope_depth, symbol_kind_name[sym->kind], sym->name, sym->type_name);
		if (sym->type) {
			printf(" (tag=%s)", type_tag_name(sym->type->tag));
		}
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
	double float_val;
	const char* lexeme;
	int len;
} tok;

void parse_error(const char* msg)
{
	fprintf(stderr, "Parse error: %s", msg);
	if (tok.kind < TOK_COUNT) {
		fprintf(stderr, " (token %s", tok_name[tok.kind]);
		if (tok.len) {
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

unsigned storage_flag_from_keyword(const char* s)
{
	if (s == kw_in) return SYM_STORAGE_IN;
	if (s == kw_out) return SYM_STORAGE_OUT;
	if (s == kw_uniform) return SYM_STORAGE_UNIFORM;
	return 0;
}

unsigned layout_flag_from_keyword(const char* s)
{
	if (s == kw_set) return SYM_LAYOUT_SET;
	if (s == kw_binding) return SYM_LAYOUT_BINDING;
	if (s == kw_location) return SYM_LAYOUT_LOCATION;
	return 0;
}

void type_spec_add_storage(TypeSpec* spec, unsigned flags)
{
	spec->storage_flags |= flags;
}

void type_spec_set_layout(TypeSpec* spec, unsigned layout_flag, int value)
{
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

int is_type_name(const char* s)
{
	return type_system_get(&g_types, s) != NULL;
}

int is_type_token()
{
	if (tok.kind != TOK_IDENTIFIER) return 0;
	const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
	if (is_type_name(name)) return 1;
	if (storage_flag_from_keyword(tok.lexeme)) return 1;
	if (tok.lexeme == kw_layout) return 1;
	return 0;
}

void parse_layout_block(TypeSpec* spec)
{
	next();
	expect(TOK_LPAREN);
	while (tok.kind != TOK_RPAREN) {
		if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier in layout");
		unsigned layout_flag = layout_flag_from_keyword(tok.lexeme);
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
		unsigned storage_flag = storage_flag_from_keyword(tok.lexeme);
		if (storage_flag) {
			type_spec_add_storage(spec, storage_flag);
			next();
			continue;
		}
		if (tok.lexeme == kw_layout) {
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
	if (tok.kind != TOK_IDENTIFIER) parse_error("expected type");
	spec.type_name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
	if (!is_type_name(spec.type_name)) parse_error("expected type");
	spec.type = type_system_get(&g_types, spec.type_name);
	if (!spec.type) parse_error("unknown type");
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
	ir_apply_type_spec(param, &spec);
	current_param_type_name = spec.type_name;
	current_param_type_type = spec.type;
	IR_Cmd* inst = ir_emit(IR_FUNC_PARAM_TYPE);
	inst->str0 = spec.type_name;
	if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier in parameter");
	const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
	inst = ir_emit(IR_FUNC_PARAM_NAME);
	inst->str0 = name;
	Symbol* sym = symbol_table_add(&g_symbols, name, current_param_type_name, current_param_type_type, SYM_PARAM);
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

void global_var_decl(TypeSpec spec, const char* first_name)
{
	IR_Cmd* inst;
	decl_emit_begin(&spec);
	inst = ir_emit(IR_DECL_VAR);
	inst->str0 = first_name;
	Symbol* sym = symbol_table_add(&g_symbols, first_name, current_decl_type_name, current_decl_type_type, SYM_VAR);
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
		const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
		inst = ir_emit(IR_DECL_VAR);
		inst->str0 = name;
		sym = symbol_table_add(&g_symbols, name, current_decl_type_name, current_decl_type_type, SYM_VAR);
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
	current_decl_type_name = NULL;
	current_decl_type_type = NULL;
}

void func_decl_or_def(TypeSpec spec, const char* name)
{
	IR_Cmd* func = ir_emit(IR_FUNC_BEGIN);
	func->str0 = spec.type_name;
	func->str1 = name;
	ir_apply_type_spec(func, &spec);
	Symbol* sym = symbol_table_add(&g_symbols, func->str1, spec.type_name, spec.type, SYM_FUNC);
	symbol_apply_type_spec(sym, &spec);
	expect(TOK_LPAREN);
	symbol_table_enter_scope(&g_symbols);
	func_param_list();
	if (tok.kind == TOK_SEMI) {
		next();
		symbol_table_leave_scope(&g_symbols);
		ir_emit(IR_FUNC_PROTOTYPE_END);
		return;
	}
	if (tok.kind == TOK_LBRACE) {
		ir_emit(IR_FUNC_DEFINITION_BEGIN);
		stmt_block();
		symbol_table_leave_scope(&g_symbols);
		ir_emit(IR_FUNC_DEFINITION_END);
		return;
	}
	symbol_table_leave_scope(&g_symbols);
	parse_error("expected ';' or function body");
}

void stmt_decl()
{
	TypeSpec spec = parse_type_specifier();
	IR_Cmd* inst;
	decl_emit_begin(&spec);
	while (1) {
		if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier in declaration");
		const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
		inst = ir_emit(IR_DECL_VAR);
		inst->str0 = name;
		Symbol* sym = symbol_table_add(&g_symbols, name, current_decl_type_name, current_decl_type_type, SYM_VAR);
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
	if (tok.kind != TOK_RPAREN) {
		expr();
		argc++;
		while (tok.kind == TOK_COMMA) {
			next();
			expr();
			argc++;
		}
		expect(TOK_RPAREN);
	} else {
		next(); // consume ')'
	}
	Type* ctor_type = NULL;
	const char* ctor_name = NULL;
	if (callee_idx >= 0) {
		IR_Cmd* callee = &g_ir[callee_idx];
		if (callee->op == IR_PUSH_IDENT && callee->str0) {
			Type* type = type_system_get(&g_types, callee->str0);
			if (type) {
				Symbol* sym = symbol_table_resolve(&g_symbols, callee->str0);
				if (!sym || sym->kind != SYM_FUNC) {
					callee->type = type;
					ctor_type = type;
					ctor_name = callee->str0;
				}
			}
		}
	}
	IR_Cmd* inst = ir_emit(ctor_type ? IR_CONSTRUCT : IR_CALL);
	inst->arg0 = argc;
	if (ctor_type) {
		inst->str0 = ctor_name;
		inst->type = ctor_type;
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
	if (c >= 'A' && c <= 'Z') c = (char)(c - 'A' + 'a');
	switch (c) {
	case 'x': case 'y': case 'z': case 'w': return 0;
	case 'r': case 'g': case 'b': case 'a': return 1;
	case 's': case 't': case 'p': case 'q': return 2;
	default: break;
	}
	return -1;
}

int swizzle_component_index(int set, char c)
{
	static const char* sets[] = { "xyzw", "rgba", "stpq" };
	if (c >= 'A' && c <= 'Z') c = (char)(c - 'A' + 'a');
	if (set < 0 || set >= (int)(sizeof(sets) / sizeof(sets[0]))) return -1;
	const char* names = sets[set];
	for (int i = 0; i < 4; ++i) {
		if (c == names[i]) return i;
	}
	return -1;
}

int swizzle_is_valid(const char* name, int len, int* out_mask)
{
	if (len < 1 || len > 4) return 0;
	int set = swizzle_set_from_char(name[0]);
	if (set < 0) return 0;
	int mask = 0;
	for (int i = 0; i < len; ++i) {
		int comp = swizzle_component_index(set, name[i]);
		if (comp < 0) return 0;
		mask |= comp << (i * 4);
	}
	if (out_mask) *out_mask = mask;
	return 1;
}

void expr_member()
{
	if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier after '.'");
	const char* name = tok.lexeme;
	int mask = 0;
	if (swizzle_is_valid(name, tok.len, &mask)) {
		IR_Cmd* inst = ir_emit(IR_SWIZZLE);
		inst->str0 = sintern_range(name, name + tok.len);
		inst->arg0 = tok.len;
		inst->arg1 = mask;
	} else {
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
	symbol_table_enter_scope(&g_symbols);
	while (tok.kind != TOK_RBRACE && tok.kind != TOK_EOF) {
		stmt();
	}
	expect(TOK_RBRACE);
	symbol_table_leave_scope(&g_symbols);
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

void stmt_return()
{
	expect(TOK_RETURN);
	int has_value = tok.kind != TOK_SEMI;
	if (has_value) {
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
	if (!is_type_token()) parse_error("expected type at top level");
	TypeSpec type_spec = parse_type_specifier();
	if (tok.kind != TOK_IDENTIFIER) parse_error("expected identifier after type");
	const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
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

void lex_number()
{
	const char* start = at - 1;
	char* endptr = NULL;
	double val = strtod(start, &endptr);
	const char* end = endptr;
	const char* suffix = end;
	int is_float = 0;
	for (const char* p = start; p < end; ++p) {
		if (*p == '.' || *p == 'e' || *p == 'E') {
			is_float = 1;
			break;
		}
	}
	if (*suffix == 'f' || *suffix == 'F') {
		is_float = 1;
		++suffix;
	}
	if (*suffix == 'l' || *suffix == 'L') {
		is_float = 1;
		++suffix;
	}
	tok.lexeme = start;
	tok.len = (int)(suffix - start);
	tok.prec = 0;
	tok.rexpr = expr_error;
	if (is_float) {
		tok.kind = TOK_FLOAT;
		tok.lexpr = expr_float;
		tok.float_val = val;
	} else {
		tok.kind = TOK_INT;
		tok.lexpr = expr_int;
		tok.int_val = 0;
		for (const char* p = start; p < end; ++p) {
			if (*p >= '0' && *p <= '9') {
				tok.int_val = tok.int_val * 10 + (*p - '0');
			}
		}
	}
	if (*suffix) {
		ch = (unsigned char)*suffix;
		at = suffix + 1;
	} else {
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

	if (ch == '.' && is_digit(at[0])) {
		lex_number();
		return;
	}

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
		const char* interned = sintern_range(s, s + tok.len);
		tok.lexeme = interned;
		tok.kind = TOK_IDENTIFIER;
		tok.prec = 0;
		tok.lexpr = expr_ident;
		tok.rexpr = expr_error;
		if (tok.lexeme == kw_if) {
			tok.kind = TOK_IF;
			tok.lexpr = expr_error;
			}
		else if (tok.lexeme == kw_else) {
			tok.kind = TOK_ELSE;
			tok.lexpr = expr_error;
			}
		else if (tok.lexeme == kw_return) {
			tok.kind = TOK_RETURN;
			tok.lexpr = expr_error;
			}
		else if (tok.lexeme == kw_break) {
			tok.kind = TOK_BREAK;
			tok.lexpr = expr_error;
			}
		else if (tok.lexeme == kw_continue) {
			tok.kind = TOK_CONTINUE;
			tok.lexpr = expr_error;
			}
		else if (tok.lexeme == kw_discard) {
			tok.kind = TOK_DISCARD;
			tok.lexpr = expr_error;
			}
		return;
	}

	// numbers
	if (is_digit(ch)) {
		lex_number();
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
	Type custom_type = (Type){ 0 };
	custom_type.tag = T_STRUCT;
	custom_type.cols = 1;
	custom_type.rows = 1;
	custom_type.base = T_FLOAT;
	custom_type.array_len = 0;
	const char* custom_name = sintern("test_struct");
	Type* declared_type = type_system_declare(&ts, custom_name, custom_type);
	assert(declared_type == type_system_get(&ts, custom_name));
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
	assert(symbol_table_resolve(&st, inner_name) == inner_sym);
	symbol_table_leave_scope(&st);
	assert(symbol_table_resolve(&st, inner_name) == NULL);
	assert(symbol_table_resolve(&st, value_name) == value_sym);
	symbol_table_free(&st);

	// Check that IR emission produces entries with the requested opcode.
	IR_Cmd* saved_ir = g_ir;
	g_ir = NULL;
	IR_Cmd* emitted = ir_emit(IR_PUSH_INT);
	assert(emitted);
	assert(acount(g_ir) == 1);
	assert(g_ir[0].op == IR_PUSH_INT);
	if (g_ir) afree(g_ir);
	g_ir = saved_ir;

	// Parse a representative shader snippet and ensure IR and symbol tables populate.
	const char* source =
		"layout(location = 0) in vec2 test_in;"
		"layout(location = 0) out vec4 test_out;"
		"void helper(float a, float b);"
		"void helper(float a, float b)"
		"{"
			"float local = a + b;"
			"if (local > 0.0) { test_out = vec4(local); } else { test_out = vec4(-local); }"
		"}";
	in = source;
	at = in;
	g_ir = NULL;
	type_system_init_builtins(&g_types);
	symbol_table_init(&g_symbols);
	next_ch();
	next();
	parse();
	assert(acount(g_ir) > 0);
	assert(acount(g_symbols.symbols) > 0);
	symbol_table_free(&g_symbols);
	type_system_free(&g_types);
	if (g_ir) afree(g_ir);
	g_ir = NULL;
	in = NULL;
	at = NULL;
	ch = 0;
	tok.kind = TOK_EOF;
	tok.prec = PREC_EXPR;
	tok.lexpr = NULL;
	tok.rexpr = NULL;
	tok.int_val = 0;
	tok.float_val = 0.0;
	tok.lexeme = NULL;
	tok.len = 0;
}

int main()
{
	#define STR(...) #__VA_ARGS__
	unit_test();
	const char* input = STR(
		layout(location = 0) in vec4 in_pos;
		layout(location = 1) in vec2 in_uv;
		layout(location = 2) in vec4 in_col;
		layout(location = 0) out vec4 out_color;
		layout(set = 0, binding = 0) uniform sampler2D u_texture;
		layout(set = 1, binding = 0) uniform sampler2DArray u_layers;
		layout(set = 2, binding = 1) uniform samplerCube u_environment;
		float weights[1 + 2];
		float exposure[];
		vec4 temp_color;
		void saturate_value(float v);
		void accumulate_layers(vec4 sample_color, float mods[3], float fallback[], int idx);
		void saturate_value(float v)
		{
			float clamped = clamp(v, 0.0, 1.0);
			weights[2] = clamped;
		}
		void accumulate_layers(vec4 sample_color, float mods[3], float fallback[], int idx)
		{
			float selected = mods[idx];
			float inverted = -selected;
			float threshold = selected > 0.5 ? selected : fallback[idx];
			vec4 tinted = vec4(sample_color.rgb * threshold, sample_color.a);
			if (tinted.a == 0.0)
			{
				temp_color = vec4(fallback[idx], fallback[idx], fallback[idx], 1.0);
			}
			else
			{
				saturate_value(tinted.a);
				temp_color = vec4(tinted.rgb + vec3(inverted), weights[2]);
			}
		}
		void main()
		{
			vec2 uv = in_uv, offset = vec2(0.0);
			float layer_index = weights[0];
			vec4 base_sample = texture(u_texture, uv + offset);
			vec4 layered_slice = texture(u_layers, vec3(uv, layer_index));
			vec4 env = texture(u_environment, vec3(uv, 0.5));
			int idx = int(layer_index);
			float fallback_values[1 + 2];
			fallback_values[0] = 0.25;
			fallback_values[1] = 0.5;
			fallback_values[2] = 0.75;
			accumulate_layers(base_sample + layered_slice, weights, fallback_values, idx);
			vec4 layered = temp_color;
			float exposure_bias = exposure[1];
			saturate_value(exposure_bias);
			vec4 combined = layered;
			combined.rgb = combined.rgb * (weights[2] + env.r);
			combined.a = combined.a == 0.0 ? env.a : combined.a;
			temp_color = combined;
			out_color = combined;
			temp_color.primary = out_color;
		}
	);
	printf("Input : %s\n\n", input);

	in = input;
	at = in;
	init_keyword_interns();
	next_ch();
	next();
	type_system_init_builtins(&g_types);
	symbol_table_init(&g_symbols);
	parse();
	dump_ir();
	printf("\n");
	dump_symbols(&g_symbols);
	symbol_table_free(&g_symbols);
	type_system_free(&g_types);
	if (g_ir) afree(g_ir);

	return 0;
}
