/*
	spindle.h — A single-header GLSL 450 to SPIR-V front end.

	Usage:
		#define CKIT_IMPLEMENTATION
		#include "ckit.h"
		#define SPINDLE_IMPLEMENTATION
		#include "spindle.h"

	Requires ckit.h for dynamic arrays, maps, and string interning.
	Be sure to include ckit before spindle to provide those facilities.

	This header exposes a small API for configuring the compiler stage,
	feeding in GLSL source, and inspecting the generated IR. The actual
	implementation lives below the SPINDLE_IMPLEMENTATION section and is
	organized into lexer, parser, type system, and testing helpers.
*/

#ifndef SPINDLE_H
#define SPINDLE_H

#include <stddef.h>
#include <stdint.h>

#ifdef __cplusplus
extern "C"
{
#endif

typedef enum ShaderStage
{
	SHADER_STAGE_VERTEX,
	SHADER_STAGE_FRAGMENT,
	SHADER_STAGE_COMPUTE,
	SHADER_STAGE_COUNT
} ShaderStage;

void compiler_set_shader_stage(ShaderStage stage);
void compiler_setup(const char* source);
void compiler_teardown();
void dump_ir();
void dump_symbols();
void unit_test();

#ifdef __cplusplus
}
#endif

#endif /* SPINDLE_H */

#ifdef SPINDLE_IMPLEMENTATION

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef CKIT_H
#error "spindle.h requires ckit.h to be included before it."
#endif

//--------------------------------------------------------------------------------------------------
// Tokenization, symbols, and IR definitions.

#define TOKEN_LIST(X) \
	X(EOF, "EOF") \
	X(IDENTIFIER, "IDENT") \
	X(INT, "INT") \
	X(FLOAT, "FLOAT") \
	X(BOOL, "BOOL") \
	X(LPAREN, "(") \
	X(RPAREN, ")") \
	X(LBRACK, "[") \
	X(RBRACK, "]") \
	X(LBRACE, "{") \
	X(RBRACE, "}") \
	X(DOT, ".") \
	X(COMMA, ",") \
	X(SEMI, ";") \
	X(QUESTION, "?") \
	X(COLON, ":") \
	X(IF, "if") \
	X(ELSE, "else") \
	X(FOR, "for") \
	X(WHILE, "while") \
	X(DO, "do") \
	X(RETURN, "return") \
	X(BREAK, "break") \
	X(CONTINUE, "continue") \
	X(DISCARD, "discard") \
	X(SWITCH, "switch") \
	X(CASE, "case") \
	X(DEFAULT, "default") \
	X(PLUS, "+") \
	X(MINUS, "-") \
	X(STAR, "*") \
	X(SLASH, "/") \
	X(PERCENT, "%") \
	X(PLUS_PLUS, "++") \
	X(MINUS_MINUS, "--") \
	X(NOT, "!") \
	X(TILDE, "~") \
	X(LT, "<") \
	X(LE, "<=") \
	X(GT, ">") \
	X(GE, ">=") \
	X(EQ, "==") \
	X(NE, "!=") \
	X(AND_AND, "&&") \
	X(OR_OR, "||") \
	X(AMP, "&") \
	X(PIPE, "|") \
	X(CARET, "^") \
	X(LSHIFT, "<<") \
	X(RSHIFT, ">>") \
	X(ASSIGN, "=") \
	X(PLUS_ASSIGN, "+=") \
	X(MINUS_ASSIGN, "-=") \
	X(STAR_ASSIGN, "*=") \
	X(SLASH_ASSIGN, "/=") \
	X(PERCENT_ASSIGN, "%=") \
	X(AND_ASSIGN, "&=") \
	X(OR_ASSIGN, "|=") \
	X(XOR_ASSIGN, "^=") \
	X(LSHIFT_ASSIGN, "<<=") \
	X(RSHIFT_ASSIGN, ">>=")

typedef enum Tok
{
#define TOKEN_ENUM(name, text)	TOK_##name,
	TOKEN_LIST(TOKEN_ENUM)
#undef TOKEN_ENUM
	TOK_COUNT
} Tok;

const char* tok_name[TOK_COUNT] = {
#define TOKEN_NAME(name, text)	[TOK_##name] = text,
	TOKEN_LIST(TOKEN_NAME)
#undef TOKEN_NAME
};
#undef TOKEN_LIST

#define SYMBOL_KIND_LIST(X) \
	X(VAR, "var") \
	X(FUNC, "func") \
	X(PARAM, "param") \
	X(BLOCK, "block")

typedef enum SymbolKind
{
#define SYMBOL_KIND_ENUM(name, text)	SYM_##name,
	SYMBOL_KIND_LIST(SYMBOL_KIND_ENUM)
#undef SYMBOL_KIND_ENUM
	SYM_KIND_COUNT
} SymbolKind;

const char* symbol_kind_name[SYM_KIND_COUNT] = {
#define SYMBOL_KIND_NAME(name, text)	[SYM_##name] = text,
	SYMBOL_KIND_LIST(SYMBOL_KIND_NAME)
#undef SYMBOL_KIND_NAME
};
#undef SYMBOL_KIND_LIST

typedef enum TypeTag
{
	T_VOID,
	T_BOOL,
	T_INT,
	T_UINT,
	T_INT64,
	T_UINT64,
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

typedef struct Type Type;

struct Type
{
	TypeTag tag;
	uint8_t cols;
	uint8_t rows;
	uint8_t base;
	uint8_t dim;
	int array_len;
	void* user;
	const char* name;
};

typedef struct StructMemberArrayDim
{
	Type type;
	int size;
	int unsized;
} StructMemberArrayDim;

typedef struct StructMember
{
	const char* name;
	Type* declared_type;
	Type* type;
	dyna StructMemberArrayDim* array_dims;
	int has_array;
	int array_len;
	int array_unsized;
	unsigned layout_flags;
	int layout_set;
	int layout_binding;
	int layout_location;
} StructMember;

typedef struct StructInfo
{
	const char* name;
	dyna StructMember* members;
	dyna const char** layout_identifiers;
} StructInfo;

Type* type_system_declare_struct(const char* name);
StructInfo* type_struct_info(Type* type);
void type_struct_clear(Type* type);
StructMember* type_struct_add_member(Type* type, const char* name, Type* member_type);
void type_struct_member_set_layout(StructMember* member, unsigned layout_flags, int set, int binding, int location);
void type_struct_member_mark_array(StructMember* member, int size, int unsized);
void type_struct_set_layout_identifiers(Type* type, const char** identifiers, int count);
StructMember* type_struct_find_member(Type* type, const char* name);
int type_struct_member_count(Type* type);
StructMember* type_struct_member_at(Type* type, int index);

typedef enum BuiltinFuncKind
{
	BUILTIN_NONE,
	BUILTIN_TEXTURE,
	BUILTIN_TEXTURE_LOD,
	BUILTIN_TEXTURE_PROJ,
	BUILTIN_TEXTURE_GRAD,
	BUILTIN_TEXTURE_OFFSET,
	BUILTIN_TEXTURE_LOD_OFFSET,
	BUILTIN_TEXTURE_PROJ_OFFSET,
	BUILTIN_TEXTURE_PROJ_LOD,
	BUILTIN_TEXTURE_PROJ_LOD_OFFSET,
	BUILTIN_TEXTURE_GRAD_OFFSET,
	BUILTIN_TEXTURE_PROJ_GRAD,
	BUILTIN_TEXTURE_PROJ_GRAD_OFFSET,
	BUILTIN_TEXTURE_GATHER,
	BUILTIN_TEXTURE_GATHER_OFFSET,
	BUILTIN_TEXTURE_GATHER_OFFSETS,
	BUILTIN_MIN,
	BUILTIN_MAX,
	BUILTIN_CLAMP,
	BUILTIN_ABS,
	BUILTIN_FLOOR,
	BUILTIN_CEIL,
	BUILTIN_FRACT,
	BUILTIN_MIX,
	BUILTIN_STEP,
	BUILTIN_SMOOTHSTEP,
	BUILTIN_DOT,
	BUILTIN_CROSS,
	BUILTIN_NORMALIZE,
	BUILTIN_LENGTH,
	BUILTIN_DISTANCE,
	BUILTIN_REFLECT,
	BUILTIN_REFRACT,
	BUILTIN_POW,
	BUILTIN_EXP,
	BUILTIN_EXP2,
	BUILTIN_LOG,
	BUILTIN_LOG2,
	BUILTIN_SQRT,
	BUILTIN_INVERSE_SQRT,
	BUILTIN_MOD,
	BUILTIN_SIN,
	BUILTIN_COS,
	BUILTIN_TAN,
	BUILTIN_ASIN,
	BUILTIN_ACOS,
	BUILTIN_ATAN,
	BUILTIN_SIGN,
	BUILTIN_TRUNC,
	BUILTIN_ROUND,
	BUILTIN_ROUND_EVEN,
	BUILTIN_DFDX,
	BUILTIN_DFDX_FINE,
	BUILTIN_DFDX_COARSE,
	BUILTIN_DFDY,
	BUILTIN_DFDY_FINE,
	BUILTIN_DFDY_COARSE,
	BUILTIN_FWIDTH,
	BUILTIN_FWIDTH_FINE,
	BUILTIN_FWIDTH_COARSE,
	BUILTIN_TEXTURE_SIZE,
	BUILTIN_TEXEL_FETCH,
	BUILTIN_TEXEL_FETCH_OFFSET,
	BUILTIN_TEXTURE_QUERY_LOD,
	BUILTIN_TEXTURE_QUERY_LEVELS,
	BUILTIN_INVERSE,
	BUILTIN_TRANSPOSE,
	BUILTIN_DETERMINANT,
	BUILTIN_OUTER_PRODUCT,
	BUILTIN_MATRIX_COMP_MULT,
	BUILTIN_LESS_THAN,
	BUILTIN_LESS_THAN_EQUAL,
	BUILTIN_GREATER_THAN,
	BUILTIN_GREATER_THAN_EQUAL,
	BUILTIN_EQUAL,
	BUILTIN_NOT_EQUAL,
	BUILTIN_ANY,
	BUILTIN_ALL,
	BUILTIN_IMAGE_ATOMIC_ADD,
	BUILTIN_IMAGE_ATOMIC_MIN,
	BUILTIN_IMAGE_ATOMIC_MAX,
	BUILTIN_IMAGE_ATOMIC_AND,
	BUILTIN_IMAGE_ATOMIC_OR,
	BUILTIN_IMAGE_ATOMIC_XOR,
	BUILTIN_IMAGE_ATOMIC_EXCHANGE,
	BUILTIN_IMAGE_ATOMIC_COMP_SWAP
} BuiltinFuncKind;

typedef struct Symbol
{
	const char* name;
	const char* type_name;
	Type* type;
	SymbolKind kind;
	unsigned storage_flags;
	unsigned qualifier_flags;
	unsigned layout_flags;
	int layout_set;
	int layout_binding;
	int layout_location;
	int scope_depth;
	dyna Type** params;
	int param_count;
	int param_signature_set;
	Type array_type;
	Type* array_element_type;
	int array_dimensions;
	BuiltinFuncKind builtin_kind;
	int builtin_param_count;
	int is_builtin;
	ShaderStage builtin_stage;
} Symbol;

typedef struct TypeLayoutAssignment
{
	const char* identifier;
	int value;
} TypeLayoutAssignment;

typedef struct TypeSpec
{
	const char* type_name;
	Type* type;
	unsigned storage_flags;
	unsigned qualifier_flags;
	unsigned layout_flags;
	int layout_set;
	int layout_binding;
	int layout_location;
	dyna const char** layout_identifiers;
	dyna TypeLayoutAssignment* layout_assignments;
	StructInfo* struct_info;
	int has_struct_definition;
	int is_interface_block;
	int is_stage_layout;
} TypeSpec;

typedef struct TypeSystem
{
	Map map;
	dyna Type* types;
} TypeSystem;

typedef enum IR_Op
{
	IR_PUSH_INT,
	IR_PUSH_IDENT,
	IR_PUSH_FLOAT,
	IR_PUSH_BOOL,
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
	IR_FOR_BEGIN,
	IR_FOR_INIT_BEGIN,
	IR_FOR_INIT_END,
	IR_FOR_COND_BEGIN,
	IR_FOR_COND_END,
	IR_FOR_STEP_BEGIN,
	IR_FOR_STEP_END,
	IR_FOR_BODY_BEGIN,
	IR_FOR_BODY_END,
	IR_FOR_END,
	IR_WHILE_BEGIN,
	IR_WHILE_COND_BEGIN,
	IR_WHILE_COND_END,
	IR_WHILE_BODY_BEGIN,
	IR_WHILE_BODY_END,
	IR_WHILE_END,
	IR_DO_BEGIN,
	IR_DO_BODY_BEGIN,
	IR_DO_BODY_END,
	IR_DO_COND_BEGIN,
	IR_DO_COND_END,
	IR_DO_END,
	IR_SWITCH_BEGIN,
	IR_SWITCH_SELECTOR_BEGIN,
	IR_SWITCH_SELECTOR_END,
	IR_SWITCH_CASE,
	IR_SWITCH_END,
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
	IR_STRUCT_BEGIN,
	IR_STRUCT_MEMBER,
	IR_STRUCT_END,
	IR_BLOCK_DECL_BEGIN,
	IR_BLOCK_DECL_LAYOUT,
	IR_BLOCK_DECL_MEMBER,
	IR_BLOCK_DECL_INSTANCE,
	IR_BLOCK_DECL_END,
	IR_STAGE_LAYOUT_BEGIN,
	IR_STAGE_LAYOUT_IDENTIFIER,
	IR_STAGE_LAYOUT_VALUE,
	IR_STAGE_LAYOUT_END,
	IR_OP_COUNT
} IR_Op;

enum
{
	SWITCH_CASE_FLAG_DEFAULT = 1 << 0,
	SWITCH_CASE_FLAG_FALLTHROUGH = 1 << 1,
	SWITCH_CASE_FLAG_HAS_BODY = 1 << 2,
};

const char* ir_op_name[IR_OP_COUNT] = {
	[IR_PUSH_INT] = "push_int",
	[IR_PUSH_IDENT] = "push_ident",
	[IR_PUSH_FLOAT] = "push_float",
	[IR_PUSH_BOOL] = "push_bool",
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
	[IR_FOR_BEGIN] = "for_begin",
	[IR_FOR_INIT_BEGIN] = "for_init_begin",
	[IR_FOR_INIT_END] = "for_init_end",
	[IR_FOR_COND_BEGIN] = "for_cond_begin",
	[IR_FOR_COND_END] = "for_cond_end",
	[IR_FOR_STEP_BEGIN] = "for_step_begin",
	[IR_FOR_STEP_END] = "for_step_end",
	[IR_FOR_BODY_BEGIN] = "for_body_begin",
	[IR_FOR_BODY_END] = "for_body_end",
	[IR_FOR_END] = "for_end",
	[IR_WHILE_BEGIN] = "while_begin",
	[IR_WHILE_COND_BEGIN] = "while_cond_begin",
	[IR_WHILE_COND_END] = "while_cond_end",
	[IR_WHILE_BODY_BEGIN] = "while_body_begin",
	[IR_WHILE_BODY_END] = "while_body_end",
	[IR_WHILE_END] = "while_end",
	[IR_DO_BEGIN] = "do_begin",
	[IR_DO_BODY_BEGIN] = "do_body_begin",
	[IR_DO_BODY_END] = "do_body_end",
	[IR_DO_COND_BEGIN] = "do_cond_begin",
	[IR_DO_COND_END] = "do_cond_end",
	[IR_DO_END] = "do_end",
	[IR_SWITCH_BEGIN] = "switch_begin",
	[IR_SWITCH_SELECTOR_BEGIN] = "switch_selector_begin",
	[IR_SWITCH_SELECTOR_END] = "switch_selector_end",
	[IR_SWITCH_CASE] = "switch_case",
	[IR_SWITCH_END] = "switch_end",
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
	[IR_STRUCT_BEGIN] = "struct_begin",
	[IR_STRUCT_MEMBER] = "struct_member",
	[IR_STRUCT_END] = "struct_end",
	[IR_BLOCK_DECL_BEGIN] = "block_decl_begin",
	[IR_BLOCK_DECL_LAYOUT] = "block_decl_layout",
	[IR_BLOCK_DECL_MEMBER] = "block_decl_member",
	[IR_BLOCK_DECL_INSTANCE] = "block_decl_instance",
	[IR_BLOCK_DECL_END] = "block_decl_end",
	[IR_STAGE_LAYOUT_BEGIN] = "stage_layout_begin",
	[IR_STAGE_LAYOUT_IDENTIFIER] = "stage_layout_identifier",
	[IR_STAGE_LAYOUT_VALUE] = "stage_layout_value",
	[IR_STAGE_LAYOUT_END] = "stage_layout_end",
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
	unsigned qualifier_flags;
	unsigned layout_flags;
	int layout_set;
	int layout_binding;
	int layout_location;
	int is_unsigned_literal;
	int is_lvalue;
} IR_Cmd;

// The global intermediate representation tape.
// This gets produced before outputting any SPIRV as a middle-step.
dyna IR_Cmd* g_ir;
TypeSystem g_type_system;
TypeSystem* ts = &g_type_system;
Type* g_type_void;
Type* g_type_bool;
Type* g_type_int;
Type* g_type_uint;
Type* g_type_int64;
Type* g_type_uint64;
Type* g_type_float;
Type* g_type_double;

void type_check_error(const char* fmt, ...);
int type_equal(const Type* a, const Type* b);
const char* type_display(const Type* type);
Type* type_system_get(const char* name);
void type_system_free();
void type_system_init_builtins();
Type* type_get_scalar(TypeTag base);
Type* type_get_vector(TypeTag base, int cols);
Type* type_get_matrix(TypeTag base, int cols, int rows);
Type* type_check_unary(const IR_Cmd* inst, Type* operand);
void type_check_ir();
Type* type_infer_builtin_call(const Symbol* sym, Type** args, int argc);
void dump_ir();
void dump_symbols();

//--------------------------------------------------------------------------------------------------
// Lexer and parser implementation.

typedef enum SymbolStorage
{
	SYM_STORAGE_IN = 1 << 0,
	SYM_STORAGE_OUT = 1 << 1,
	SYM_STORAGE_UNIFORM = 1 << 2,
	SYM_STORAGE_BUFFER = 1 << 3,
	SYM_STORAGE_SHARED = 1 << 4,
} SymbolStorage;

typedef enum SymbolLayout
{
	SYM_LAYOUT_SET = 1 << 0,
	SYM_LAYOUT_BINDING = 1 << 1,
	SYM_LAYOUT_LOCATION = 1 << 2,
} SymbolLayout;

typedef enum SymbolQualifier
{
	SYM_QUAL_CONST = 1 << 0,
	SYM_QUAL_VOLATILE = 1 << 1,
	SYM_QUAL_RESTRICT = 1 << 2,
	SYM_QUAL_READONLY = 1 << 3,
	SYM_QUAL_WRITEONLY = 1 << 4,
} SymbolQualifier;

typedef struct SymbolScopeEntry
{
	const char* name;
	int value_index;
} SymbolScopeEntry;

typedef struct SymbolScope
{
	Map map;
	dyna SymbolScopeEntry* entries;
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
const char* kw_struct;
const char* kw_set;
const char* kw_binding;
const char* kw_location;
const char* kw_std140;
const char* kw_std430;
const char* kw_column_major;
const char* kw_shared;
const char* kw_buffer;
const char* kw_packed;
const char* kw_volatile;
const char* kw_restrict;
const char* kw_readonly;
const char* kw_writeonly;
const char* kw_if;
const char* kw_else;
const char* kw_return;
const char* kw_break;
const char* kw_continue;
const char* kw_discard;
const char* kw_for;
const char* kw_while;
const char* kw_do;
const char* kw_switch;
const char* kw_case;
const char* kw_default;
const char* kw_true;
const char* kw_false;
const char* kw_const;

void init_keyword_interns()
{
	kw_in = sintern("in");
	kw_out = sintern("out");
	kw_uniform = sintern("uniform");
	kw_layout = sintern("layout");
	kw_struct = sintern("struct");
	kw_set = sintern("set");
	kw_binding = sintern("binding");
	kw_location = sintern("location");
	kw_std140 = sintern("std140");
	kw_std430 = sintern("std430");
	kw_column_major = sintern("column_major");
	kw_shared = sintern("shared");
	kw_buffer = sintern("buffer");
	kw_packed = sintern("packed");
	kw_volatile = sintern("volatile");
	kw_restrict = sintern("restrict");
	kw_readonly = sintern("readonly");
	kw_writeonly = sintern("writeonly");
	kw_if = sintern("if");
	kw_else = sintern("else");
	kw_return = sintern("return");
	kw_break = sintern("break");
	kw_continue = sintern("continue");
	kw_discard = sintern("discard");
	kw_for = sintern("for");
	kw_while = sintern("while");
	kw_do = sintern("do");
	kw_switch = sintern("switch");
	kw_case = sintern("case");
	kw_default = sintern("default");
	kw_true = sintern("true");
	kw_false = sintern("false");
	kw_const = sintern("const");
}

SymbolTable g_symbol_table;
SymbolTable* st = &g_symbol_table;
ShaderStage g_shader_stage = SHADER_STAGE_VERTEX;
const char* current_decl_type_name;
Type* current_decl_type_type;
const char* current_param_type_name;
Type* current_param_type_type;
Type** current_function_params;
Symbol* current_decl_symbol;
Symbol* current_param_symbol;

Symbol* symbol_table_add(const char* name, const char* type_name, Type* type, SymbolKind kind);
void symbol_add_storage(Symbol* sym, unsigned flags);
void symbol_add_qualifier(Symbol* sym, unsigned flags);
void symbol_mark_array(Symbol* sym, Type* element_type);

// Create a fresh scope for blocks like function bodies or if-statements.
// ...if (use_shadows) { vec3 atten = vec3(0.0); }
void symbol_table_enter_scope()
{
	apush(st->scopes, (SymbolScope){ 0 });
}

// Discard the innermost scope once we leave a block.
// ...}
void symbol_table_leave_scope()
{
	int count = acount(st->scopes);
	if (!count)
		return;
	SymbolScope* scope = &st->scopes[count - 1];
	if (scope->entries)
		afree(scope->entries);
	map_free(scope->map);
	(void)apop(st->scopes);
}

static SymbolScopeEntry* symbol_scope_entry(SymbolScope* scope, const char* name, int create)
{
	uint64_t idx = map_get(scope->map, (uint64_t)name);
	if (!idx)
	{
		if (!create)
			return NULL;
		SymbolScopeEntry entry = (SymbolScopeEntry){ 0 };
		entry.name = name;
		apush(scope->entries, entry);
		idx = (uint64_t)acount(scope->entries);
		map_add(scope->map, (uint64_t)name, idx);
	}
	return &scope->entries[(int)idx - 1];
}

static SymbolScopeEntry* symbol_scope_entry_at_depth(int scope_depth, const char* name, int create)
{
	if (scope_depth < 0 || scope_depth >= acount(st->scopes))
		return NULL;
	SymbolScope* scope = &st->scopes[scope_depth];
	return symbol_scope_entry(scope, name, create);
}

// Register a symbol for declarations such as float roughness; or void shade().
// ...float roughness;
static Symbol* symbol_table_add_internal(const char* name, const char* type_name, Type* type, SymbolKind kind, int scope_depth)
{
	if (!acount(st->scopes))
		return NULL;
	if (scope_depth < 0)
		scope_depth = 0;
	if (scope_depth >= acount(st->scopes))
		scope_depth = acount(st->scopes) - 1;
	SymbolScopeEntry* entry = symbol_scope_entry_at_depth(scope_depth, name, 1);
	if (!entry)
		return NULL;
	if (entry->value_index)
		return &st->symbols[entry->value_index - 1];
	Symbol sym = (Symbol){ 0 };
	sym.name = name;
	sym.type_name = type_name;
	sym.type = type;
	sym.kind = kind;
	sym.scope_depth = scope_depth;
	sym.builtin_param_count = -1;
	apush(st->symbols, sym);
	int idx = acount(st->symbols);
	entry->value_index = idx;
	return &st->symbols[idx - 1];
}

static Symbol* symbol_table_add_at_depth(const char* name, const char* type_name, Type* type, SymbolKind kind, int scope_depth)
{
	return symbol_table_add_internal(name, type_name, type, kind, scope_depth);
}

typedef struct BuiltinFunctionInit
{
	const char* name;
	BuiltinFuncKind kind;
	const char* return_type_name;
	int param_count;
} BuiltinFunctionInit;

typedef struct BuiltinVariableInit
{
	const char* name;
	unsigned stage_mask;
	const char* type_name;
	unsigned storage_flags;
	unsigned qualifier_flags;
	int array_dimensions;
	int array_length;
} BuiltinVariableInit;

/*
| Builtin Variable | Stage             | Type  | Access       |
|------------------|-------------------|-------|--------------|
| gl_Position      | Vertex (output)   | vec4  | write (out)  |
| gl_PointSize     | Vertex (output)   | float | write (out)  |
| gl_VertexIndex   | Vertex (input)    | int   | read (in)    |
| gl_InstanceIndex | Vertex (input)    | int   | read (in)    |
| gl_FragCoord     | Fragment (input)  | vec4  | read (in)    |
| gl_FrontFacing   | Fragment (input)  | bool  | read (in)    |
| gl_PointCoord    | Fragment (input)  | vec2  | read (in)    |
| gl_FragDepth     | Fragment (output) | float | write (out)  |
*/
#define STAGE_MASK(stage) (1u << (stage))
#define STAGE_MASK_VERTEX STAGE_MASK(SHADER_STAGE_VERTEX)
#define STAGE_MASK_FRAGMENT STAGE_MASK(SHADER_STAGE_FRAGMENT)
#define STAGE_MASK_COMPUTE STAGE_MASK(SHADER_STAGE_COMPUTE)

static const BuiltinVariableInit builtin_variables[] = {
	{ "gl_Position", STAGE_MASK_VERTEX, "vec4", SYM_STORAGE_OUT, 0, 0, -1 },
	{ "gl_PointSize", STAGE_MASK_VERTEX, "float", SYM_STORAGE_OUT, 0, 0, -1 },
	{ "gl_ClipDistance", STAGE_MASK_VERTEX, "float", SYM_STORAGE_OUT, 0, 1, -1 },
	{ "gl_CullDistance", STAGE_MASK_VERTEX, "float", SYM_STORAGE_OUT, 0, 1, -1 },
	{ "gl_VertexIndex", STAGE_MASK_VERTEX, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_InstanceIndex", STAGE_MASK_VERTEX, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_DrawID", STAGE_MASK_VERTEX, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_BaseVertex", STAGE_MASK_VERTEX, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_BaseInstance", STAGE_MASK_VERTEX, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_ViewIndex", STAGE_MASK_VERTEX | STAGE_MASK_FRAGMENT, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_FragCoord", STAGE_MASK_FRAGMENT, "vec4", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_FrontFacing", STAGE_MASK_FRAGMENT, "bool", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_PointCoord", STAGE_MASK_FRAGMENT, "vec2", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_PrimitiveID", STAGE_MASK_FRAGMENT, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_SampleID", STAGE_MASK_FRAGMENT, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_SamplePosition", STAGE_MASK_FRAGMENT, "vec2", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_SampleMaskIn", STAGE_MASK_FRAGMENT, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 1, -1 },
	{ "gl_HelperInvocation", STAGE_MASK_FRAGMENT, "bool", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_Layer", STAGE_MASK_FRAGMENT, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_ViewportIndex", STAGE_MASK_FRAGMENT, "int", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_ClipDistance", STAGE_MASK_FRAGMENT, "float", SYM_STORAGE_IN, SYM_QUAL_CONST, 1, -1 },
	{ "gl_CullDistance", STAGE_MASK_FRAGMENT, "float", SYM_STORAGE_IN, SYM_QUAL_CONST, 1, -1 },
	{ "gl_FragDepth", STAGE_MASK_FRAGMENT, "float", SYM_STORAGE_OUT, 0, 0, -1 },
	{ "gl_GlobalInvocationID", STAGE_MASK_COMPUTE, "uvec3", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_WorkGroupID", STAGE_MASK_COMPUTE, "uvec3", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_NumWorkGroups", STAGE_MASK_COMPUTE, "uvec3", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_LocalInvocationID", STAGE_MASK_COMPUTE, "uvec3", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_LocalInvocationIndex", STAGE_MASK_COMPUTE, "uint", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_WorkGroupSize", STAGE_MASK_COMPUTE, "uvec3", SYM_STORAGE_IN, SYM_QUAL_CONST, 0, -1 },
	{ "gl_SampleMask", STAGE_MASK_FRAGMENT, "int", SYM_STORAGE_OUT, 0, 1, -1 },
};

static void symbol_table_register_builtin(const BuiltinFunctionInit* init)
{
	const char* name = sintern(init->name);
	const char* return_type = init->return_type_name ? sintern(init->return_type_name) : NULL;
	Type* type = return_type ? type_system_get(return_type) : NULL;
	Symbol* sym = symbol_table_add(name, return_type, type, SYM_FUNC);
	sym->builtin_kind = init->kind;
	sym->builtin_param_count = init->param_count;
}

static void symbol_table_register_builtins()
{
	const BuiltinFunctionInit builtins[] = {
		{ "texture", BUILTIN_TEXTURE, "vec4", 2 },
		{ "textureLod", BUILTIN_TEXTURE_LOD, "vec4", 3 },
		{ "textureProj", BUILTIN_TEXTURE_PROJ, "vec4", -1 },
		{ "textureGrad", BUILTIN_TEXTURE_GRAD, "vec4", 4 },
		{ "textureOffset", BUILTIN_TEXTURE_OFFSET, NULL, 3 },
		{ "textureLodOffset", BUILTIN_TEXTURE_LOD_OFFSET, NULL, 4 },
		{ "textureProjOffset", BUILTIN_TEXTURE_PROJ_OFFSET, NULL, -1 },
		{ "textureProjLod", BUILTIN_TEXTURE_PROJ_LOD, NULL, 3 },
		{ "textureProjLodOffset", BUILTIN_TEXTURE_PROJ_LOD_OFFSET, NULL, 4 },
		{ "textureGradOffset", BUILTIN_TEXTURE_GRAD_OFFSET, NULL, 5 },
		{ "textureProjGrad", BUILTIN_TEXTURE_PROJ_GRAD, NULL, 4 },
		{ "textureProjGradOffset", BUILTIN_TEXTURE_PROJ_GRAD_OFFSET, NULL, 5 },
		{ "textureGather", BUILTIN_TEXTURE_GATHER, NULL, -1 },
		{ "textureGatherOffset", BUILTIN_TEXTURE_GATHER_OFFSET, NULL, -1 },
		{ "textureGatherOffsets", BUILTIN_TEXTURE_GATHER_OFFSETS, NULL, -1 },
		{ "textureSize", BUILTIN_TEXTURE_SIZE, NULL, -1 },
		{ "texelFetch", BUILTIN_TEXEL_FETCH, NULL, -1 },
		{ "texelFetchOffset", BUILTIN_TEXEL_FETCH_OFFSET, NULL, -1 },
		{ "textureQueryLod", BUILTIN_TEXTURE_QUERY_LOD, "vec2", 2 },
		{ "textureQueryLevels", BUILTIN_TEXTURE_QUERY_LEVELS, "int", 1 },
		{ "min", BUILTIN_MIN, NULL, 2 },
		{ "max", BUILTIN_MAX, NULL, 2 },
		{ "clamp", BUILTIN_CLAMP, NULL, 3 },
		{ "abs", BUILTIN_ABS, NULL, 1 },
		{ "floor", BUILTIN_FLOOR, NULL, 1 },
		{ "ceil", BUILTIN_CEIL, NULL, 1 },
		{ "fract", BUILTIN_FRACT, NULL, 1 },
		{ "frac", BUILTIN_FRACT, NULL, 1 },
		{ "mix", BUILTIN_MIX, NULL, 3 },
		{ "step", BUILTIN_STEP, NULL, 2 },
		{ "smoothstep", BUILTIN_SMOOTHSTEP, NULL, 3 },
		{ "dot", BUILTIN_DOT, NULL, 2 },
		{ "cross", BUILTIN_CROSS, NULL, 2 },
		{ "normalize", BUILTIN_NORMALIZE, NULL, 1 },
		{ "length", BUILTIN_LENGTH, "float", 1 },
		{ "distance", BUILTIN_DISTANCE, "float", 2 },
		{ "reflect", BUILTIN_REFLECT, NULL, 2 },
		{ "refract", BUILTIN_REFRACT, NULL, 3 },
		{ "pow", BUILTIN_POW, NULL, 2 },
		{ "exp", BUILTIN_EXP, NULL, 1 },
		{ "exp2", BUILTIN_EXP2, NULL, 1 },
		{ "log", BUILTIN_LOG, NULL, 1 },
		{ "log2", BUILTIN_LOG2, NULL, 1 },
		{ "sqrt", BUILTIN_SQRT, NULL, 1 },
		{ "inversesqrt", BUILTIN_INVERSE_SQRT, NULL, 1 },
		{ "mod", BUILTIN_MOD, NULL, 2 },
		{ "sin", BUILTIN_SIN, NULL, 1 },
		{ "cos", BUILTIN_COS, NULL, 1 },
		{ "tan", BUILTIN_TAN, NULL, 1 },
		{ "asin", BUILTIN_ASIN, NULL, 1 },
		{ "acos", BUILTIN_ACOS, NULL, 1 },
		{ "atan", BUILTIN_ATAN, NULL, -1 },
		{ "sign", BUILTIN_SIGN, NULL, 1 },
		{ "trunc", BUILTIN_TRUNC, NULL, 1 },
		{ "round", BUILTIN_ROUND, NULL, 1 },
		{ "roundEven", BUILTIN_ROUND_EVEN, NULL, 1 },
		{ "dFdx", BUILTIN_DFDX, NULL, 1 },
		{ "dFdy", BUILTIN_DFDY, NULL, 1 },
		{ "fwidth", BUILTIN_FWIDTH, NULL, 1 },
		{ "dFdxFine", BUILTIN_DFDX_FINE, NULL, 1 },
		{ "dFdxCoarse", BUILTIN_DFDX_COARSE, NULL, 1 },
		{ "dFdyFine", BUILTIN_DFDY_FINE, NULL, 1 },
		{ "dFdyCoarse", BUILTIN_DFDY_COARSE, NULL, 1 },
		{ "fwidthFine", BUILTIN_FWIDTH_FINE, NULL, 1 },
		{ "fwidthCoarse", BUILTIN_FWIDTH_COARSE, NULL, 1 },
		{ "inverse", BUILTIN_INVERSE, NULL, 1 },
		{ "transpose", BUILTIN_TRANSPOSE, NULL, 1 },
		{ "determinant", BUILTIN_DETERMINANT, NULL, 1 },
		{ "outerProduct", BUILTIN_OUTER_PRODUCT, NULL, 2 },
		{ "matrixCompMult", BUILTIN_MATRIX_COMP_MULT, NULL, 2 },
		{ "lessThan", BUILTIN_LESS_THAN, NULL, 2 },
		{ "lessThanEqual", BUILTIN_LESS_THAN_EQUAL, NULL, 2 },
		{ "greaterThan", BUILTIN_GREATER_THAN, NULL, 2 },
		{ "greaterThanEqual", BUILTIN_GREATER_THAN_EQUAL, NULL, 2 },
		{ "equal", BUILTIN_EQUAL, NULL, 2 },
		{ "notEqual", BUILTIN_NOT_EQUAL, NULL, 2 },
		{ "any", BUILTIN_ANY, "bool", 1 },
		{ "all", BUILTIN_ALL, "bool", 1 },
		{ "imageAtomicAdd", BUILTIN_IMAGE_ATOMIC_ADD, NULL, -1 },
		{ "imageAtomicMin", BUILTIN_IMAGE_ATOMIC_MIN, NULL, -1 },
		{ "imageAtomicMax", BUILTIN_IMAGE_ATOMIC_MAX, NULL, -1 },
		{ "imageAtomicAnd", BUILTIN_IMAGE_ATOMIC_AND, NULL, -1 },
		{ "imageAtomicOr", BUILTIN_IMAGE_ATOMIC_OR, NULL, -1 },
		{ "imageAtomicXor", BUILTIN_IMAGE_ATOMIC_XOR, NULL, -1 },
		{ "imageAtomicExchange", BUILTIN_IMAGE_ATOMIC_EXCHANGE, NULL, -1 },
		{ "imageAtomicCompSwap", BUILTIN_IMAGE_ATOMIC_COMP_SWAP, NULL, -1 }
	};
	for (size_t i = 0; i < sizeof(builtins) / sizeof(builtins[0]); ++i)
	{
		symbol_table_register_builtin(&builtins[i]);
	}
}

static void symbol_table_register_builtin_variables()
{
	unsigned current_stage_mask = STAGE_MASK(g_shader_stage);
	for (size_t i = 0; i < sizeof(builtin_variables) / sizeof(builtin_variables[0]); ++i)
	{
		const BuiltinVariableInit* init = &builtin_variables[i];
		if (!(init->stage_mask & current_stage_mask))
			continue;
		const char* name = sintern(init->name);
		const char* type_name = sintern(init->type_name);
		Type* type = type_system_get(type_name);
		Symbol* sym = symbol_table_add(name, type_name, type, SYM_VAR);
		sym->is_builtin = 1;
		sym->builtin_stage = g_shader_stage;
		symbol_add_storage(sym, init->storage_flags);
		symbol_add_qualifier(sym, init->qualifier_flags);
		Type* element_type = type;
		for (int dim = 0; dim < init->array_dimensions; ++dim)
		{
			symbol_mark_array(sym, element_type);
			if (sym->type && init->array_length >= 0)
				sym->type->array_len = init->array_length;
			element_type = sym->type;
		}
	}
}

void symbol_table_init()
{
	st->symbols = NULL;
	st->scopes = NULL;
	symbol_table_enter_scope();
	symbol_table_register_builtins();
	symbol_table_register_builtin_variables();
}

Symbol* symbol_table_add(const char* name, const char* type_name, Type* type, SymbolKind kind)
{
	int scope_depth = acount(st->scopes) ? acount(st->scopes) - 1 : 0;
	return symbol_table_add_internal(name, type_name, type, kind, scope_depth);
}

Symbol* symbol_table_find(const char* name)
{
	for (int i = acount(st->scopes) - 1; i >= 0; --i)
	{
		SymbolScope* scope = &st->scopes[i];
		uint64_t entry_idx = map_get(scope->map, (uint64_t)name);
		if (!entry_idx)
			continue;
		SymbolScopeEntry* entry = &scope->entries[(int)entry_idx - 1];
		if (entry->value_index)
			return &st->symbols[entry->value_index - 1];
	}
	return NULL;
}

void symbol_add_storage(Symbol* sym, unsigned flags)
{
	sym->storage_flags |= flags;
}

void symbol_add_qualifier(Symbol* sym, unsigned flags)
{
	sym->qualifier_flags |= flags;
}

int symbol_has_storage(const Symbol* sym, unsigned flag)
{
	return (sym->storage_flags & flag) != 0;
}

int symbol_has_qualifier(const Symbol* sym, unsigned flag)
{
	return (sym->qualifier_flags & flag) != 0;
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
	symbol_add_qualifier(sym, spec->qualifier_flags);
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
		symbol_table_leave_scope();
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
	PREC_BIT_OR = 50, // |
	PREC_BIT_XOR = 60, // ^
	PREC_BIT_AND = 70, // &
	PREC_EQ = 80, // == !=
	PREC_REL = 90, // < <= > >=
	PREC_SHIFT = 95, // << >>
	PREC_ADD = 100, // + -
	PREC_MUL = 110, // * / %
	PREC_POSTFIX = 120, // () [] .
	PREC_UNARY = 130 // prefix + - ! ~
} Prec;

const char* in;
const char* at;
char ch;

typedef struct Token
{
	Tok kind;
	Prec prec;
	void (*lexpr)(); // Start an expression.
	void (*rexpr)(); // Continue an expression (binary/postfix/etc).
	int int_val;
	double float_val;
	const char* lexeme;
	int len;
	int is_unsigned;
} Token;

Token tok;

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
		if (ch == '#')
		{
			next_ch();
			while (1)
			{
				int prev = 0;
				int prev_prev = 0;
				while (ch && ch != '\n')
				{
					prev_prev = prev;
					prev = ch;
					next_ch();
				}
				if (!ch)
					return;
				next_ch();
				if (prev == '\\' || (prev == '\r' && prev_prev == '\\'))
					continue;
				break;
			}
			continue;
		}
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
	if (s == kw_buffer)
		return SYM_STORAGE_BUFFER;
	if (s == kw_shared)
		return SYM_STORAGE_SHARED;
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

unsigned qualifier_flag_from_keyword(const char* s)
{
	if (s == kw_const)
		return SYM_QUAL_CONST;
	if (s == kw_volatile)
		return SYM_QUAL_VOLATILE;
	if (s == kw_restrict)
		return SYM_QUAL_RESTRICT;
	if (s == kw_readonly)
		return SYM_QUAL_READONLY;
	if (s == kw_writeonly)
		return SYM_QUAL_WRITEONLY;
	return 0;
}

void type_spec_add_storage(TypeSpec* spec, unsigned flags)
{
	spec->storage_flags |= flags;
}

void type_spec_add_qualifier(TypeSpec* spec, unsigned flags)
{
	spec->qualifier_flags |= flags;
}

void type_spec_add_layout_identifier(TypeSpec* spec, const char* ident)
{
	if (!ident)
		return;
	apush(spec->layout_identifiers, ident);
}

void type_spec_add_layout_assignment(TypeSpec* spec, const char* ident, int value)
{
	if (!ident)
		return;
	TypeLayoutAssignment entry = { 0 };
	entry.identifier = ident;
	entry.value = value;
	apush(spec->layout_assignments, entry);
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

StructInfo* parse_struct_body(Type* type, TypeSpec* spec);
void parse_struct_member_array_suffix(StructMember* member);
void emit_struct_ir(Type* type, StructInfo* info);
void interface_block_decl(const TypeSpec* spec, const char* instance_name);
static void validate_interface_block_layout(const TypeSpec* spec);
Token peek_token();
IR_Cmd* ir_emit(IR_Op op);
void ir_apply_type_spec(IR_Cmd* inst, const TypeSpec* spec);

int is_type_token()
{
	if (tok.kind != TOK_IDENTIFIER)
		return 0;
	const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
	if (is_type_name(name))
		return 1;
	if (tok.lexeme == kw_struct)
		return 1;
	if (storage_flag_from_keyword(tok.lexeme))
		return 1;
	if (qualifier_flag_from_keyword(tok.lexeme))
		return 1;
	if (tok.lexeme == kw_layout)
		return 1;
	return 0;
}

// Consume layout(...) annotations that precede declarations.
// ...layout(location = 0) out vec4 result;
void parse_layout_block(TypeSpec* spec)
{
	next();
	expect(TOK_LPAREN);
	while (tok.kind != TOK_RPAREN)
	{
		if (tok.kind != TOK_IDENTIFIER)
			parse_error("expected identifier in layout");
		const char* ident = tok.lexeme;
		unsigned layout_flag = layout_flag_from_keyword(ident);
		next();
		if (tok.kind == TOK_ASSIGN)
		{
			next();
			if (tok.kind != TOK_INT)
				parse_error("expected integer in layout assignment");
			int value = tok.int_val;
			next();
			if (layout_flag)
			{
				type_spec_set_layout(spec, layout_flag, value);
			}
			else
			{
				type_spec_add_layout_assignment(spec, ident, value);
			}
		}
		else
		{
			type_spec_add_layout_identifier(spec, ident);
		}
		if (tok.kind == TOK_COMMA)
		{
			next();
			continue;
		}
		break;
	}
	expect(TOK_RPAREN);
}

// Gather storage and qualifier keywords like uniform const.
// ...uniform sampler2D u_image;
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
		unsigned qualifier_flag = qualifier_flag_from_keyword(tok.lexeme);
		if (qualifier_flag)
		{
			type_spec_add_qualifier(spec, qualifier_flag);
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

// Parse qualifiers and the core type that lead a declaration or parameter list.
// ...layout(std140, binding = 0) uniform Globals { mat4 vp; } u_globals;
// ...vec3 normal;
TypeSpec parse_type_specifier()
{
	TypeSpec spec = (TypeSpec){ 0 };
	parse_type_qualifiers(&spec);
	if (tok.kind == TOK_IDENTIFIER && tok.lexeme == kw_struct)
	{
		next();
		if (tok.kind != TOK_IDENTIFIER)
			parse_error("expected identifier after struct");
		const char* struct_name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
		Token lookahead = peek_token();
		if (lookahead.kind == TOK_LBRACE)
		{
			next();
			Type* type = type_system_declare_struct(struct_name);
			if (!type || type->tag != T_STRUCT)
				parse_error("struct name conflicts with existing type");
			type_struct_clear(type);
			if (spec.layout_identifiers && acount(spec.layout_identifiers) > 0)
				type_struct_set_layout_identifiers(type, spec.layout_identifiers, acount(spec.layout_identifiers));
			StructInfo* info = parse_struct_body(type, &spec);
			spec.type_name = struct_name;
			spec.type = type;
			spec.struct_info = info;
			spec.has_struct_definition = 1;
			emit_struct_ir(type, info);
			return spec;
		}
		Type* type = type_system_get(struct_name);
		if (!type)
			parse_error("unknown struct type");
		next();
		spec.type_name = struct_name;
		spec.type = type;
		return spec;
	}
	if (tok.kind != TOK_IDENTIFIER)
	{
		int has_layout = (spec.layout_identifiers && acount(spec.layout_identifiers) > 0) || (spec.layout_assignments && acount(spec.layout_assignments) > 0);
		if (tok.kind == TOK_SEMI && (spec.storage_flags || has_layout))
		{
			spec.is_stage_layout = 1;
			return spec;
		}
		parse_error("expected type");
	}
	const char* type_name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
	if ((spec.storage_flags & (SYM_STORAGE_UNIFORM | SYM_STORAGE_BUFFER)))
	{
		Token lookahead = peek_token();
		if (lookahead.kind == TOK_LBRACE)
		{
			next();
			Type* type = type_system_declare_struct(type_name);
			if (!type || type->tag != T_STRUCT)
				parse_error("block name conflicts with existing type");
			type_struct_clear(type);
			if (spec.layout_identifiers && acount(spec.layout_identifiers) > 0)
				type_struct_set_layout_identifiers(type, spec.layout_identifiers, acount(spec.layout_identifiers));
			StructInfo* info = parse_struct_body(type, &spec);
			spec.type_name = type_name;
			spec.type = type;
			spec.struct_info = info;
			spec.has_struct_definition = 1;
			spec.is_interface_block = 1;
			return spec;
		}
	}
	if (!is_type_name(type_name))
		parse_error("expected type");
	spec.type_name = type_name;
	spec.type = type_system_get(spec.type_name);
	if (!spec.type)
		parse_error("unknown type");
	next();
	return spec;
}

// Parse trailing brackets on struct members like weights[4].
// ...struct Light { float weights[4]; };
void parse_struct_member_array_suffix(StructMember* member)
{
	while (tok.kind == TOK_LBRACK)
	{
		next();
		int unsized = 0;
		int size = -1;
		if (tok.kind == TOK_RBRACK)
		{
			unsized = 1;
		}
		else
		{
			if (tok.kind != TOK_INT)
				parse_error("expected integer array size");
			size = tok.int_val;
			next();
		}
		expect(TOK_RBRACK);
		if (member)
			type_struct_member_mark_array(member, size, unsized);
	}
}

StructInfo* parse_struct_body(Type* type, TypeSpec* spec)
{
	expect(TOK_LBRACE);
	while (tok.kind != TOK_RBRACE)
	{
		TypeSpec member_spec = parse_type_specifier();
		if (tok.kind != TOK_IDENTIFIER)
			parse_error("expected identifier in struct");
		while (1)
		{
			const char* member_name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
			StructMember* member = type_struct_add_member(type, member_name, member_spec.type);
			type_struct_member_set_layout(member, member_spec.layout_flags, member_spec.layout_set, member_spec.layout_binding, member_spec.layout_location);
			next();
			parse_struct_member_array_suffix(member);
			if (tok.kind == TOK_COMMA)
			{
				next();
				continue;
			}
			break;
		}
		expect(TOK_SEMI);
	}
	expect(TOK_RBRACE);
	StructInfo* info = type_struct_info(type);
	spec->struct_info = info;
	spec->has_struct_definition = 1;
	return info;
}

void emit_struct_ir(Type* type, StructInfo* info)
{
	if (!type || !info)
		return;
	IR_Cmd* begin = ir_emit(IR_STRUCT_BEGIN);
	begin->str0 = type->name;
	begin->type = type;
	for (int i = 0; i < acount(info->members); ++i)
	{
		StructMember* member = &info->members[i];
		IR_Cmd* inst = ir_emit(IR_STRUCT_MEMBER);
		inst->str0 = member->name;
		inst->str1 = member->declared_type ? member->declared_type->name : NULL;
		inst->type = member->type;
		inst->layout_flags = member->layout_flags;
		inst->layout_set = member->layout_set;
		inst->layout_binding = member->layout_binding;
		inst->layout_location = member->layout_location;
		if (member->has_array)
			inst->arg0 = member->array_unsized ? -1 : member->array_len;
	}
	ir_emit(IR_STRUCT_END);
}

// Emit IR for interface blocks so the backend sees each layout and field.
// ...layout(set = 1, binding = 0)
// ...uniform Globals { mat4 vp; } u_globals;
static void validate_interface_block_layout(const TypeSpec* spec)
{
	if (!spec)
		return;
	int is_uniform = (spec->storage_flags & SYM_STORAGE_UNIFORM) != 0;
	int is_buffer = (spec->storage_flags & SYM_STORAGE_BUFFER) != 0;
	int layout_count = spec->layout_identifiers ? acount(spec->layout_identifiers) : 0;
	if (!layout_count)
		return;
	if (is_uniform)
	{
		for (int i = 0; i < layout_count; ++i)
		{
			const char* ident = spec->layout_identifiers[i];
			if (ident != kw_std140)
				parse_error("uniform blocks only support std140 layout");
		}
		return;
	}
	if (is_buffer)
	{
		for (int i = 0; i < layout_count; ++i)
		{
			const char* ident = spec->layout_identifiers[i];
			if (ident != kw_std430)
				parse_error("storage blocks only support std430 layout");
		}
	}
}

void interface_block_decl(const TypeSpec* spec, const char* instance_name)
{
	if (!spec || !spec->type)
		return;
	validate_interface_block_layout(spec);
	IR_Cmd* begin = ir_emit(IR_BLOCK_DECL_BEGIN);
	begin->str0 = spec->type_name;
	begin->type = spec->type;
	ir_apply_type_spec(begin, spec);
	int layout_count = spec->layout_identifiers ? acount(spec->layout_identifiers) : 0;
	for (int i = 0; i < layout_count; ++i)
	{
		IR_Cmd* layout_inst = ir_emit(IR_BLOCK_DECL_LAYOUT);
		layout_inst->str0 = spec->layout_identifiers[i];
	}
	StructInfo* info = spec->struct_info ? spec->struct_info : type_struct_info(spec->type);
	if (info)
	{
		for (int i = 0; i < acount(info->members); ++i)
		{
			StructMember* member = &info->members[i];
			IR_Cmd* inst = ir_emit(IR_BLOCK_DECL_MEMBER);
			inst->str0 = member->name;
			inst->str1 = member->declared_type ? member->declared_type->name : NULL;
			inst->type = member->type;
			inst->layout_flags = member->layout_flags;
			inst->layout_set = member->layout_set;
			inst->layout_binding = member->layout_binding;
			inst->layout_location = member->layout_location;
			if (member->has_array)
				inst->arg0 = member->array_unsized ? -1 : member->array_len;
		}
	}
	if (instance_name)
	{
		IR_Cmd* inst = ir_emit(IR_BLOCK_DECL_INSTANCE);
		inst->str0 = instance_name;
	}
	ir_emit(IR_BLOCK_DECL_END);
	if (instance_name)
	{
		Symbol* sym = symbol_table_add(instance_name, spec->type_name, spec->type, SYM_BLOCK);
		symbol_apply_type_spec(sym, spec);
	}
}

void ir_apply_type_spec(IR_Cmd* inst, const TypeSpec* spec)
{
	inst->storage_flags = spec->storage_flags;
	inst->qualifier_flags = spec->qualifier_flags;
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
	inst->qualifier_flags = spec.qualifier_flags;
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
		inst->qualifier_flags = spec.qualifier_flags;
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
	if (current_function_params)
		aclear(current_function_params);
	expect(TOK_LPAREN);
	symbol_table_enter_scope();
	func_param_list();
	int param_count = current_function_params ? acount(current_function_params) : 0;
	int outer_scope = acount(st->scopes) - 2;
	if (outer_scope < 0)
		outer_scope = 0;
	SymbolScopeEntry* entry = symbol_scope_entry_at_depth(outer_scope, func->str1, 0);
	Symbol* sym = (entry && entry->value_index) ? &st->symbols[entry->value_index - 1] : NULL;
	if (sym && sym->kind != SYM_FUNC)
		type_check_error("identifier %s redeclared as function", name);
	if (!sym)
		sym = symbol_table_add_at_depth(func->str1, spec.type_name, spec.type, SYM_FUNC, outer_scope);
	if (!sym)
	{
		symbol_table_leave_scope();
		type_check_error("failed to declare function %s", name);
		return;
	}
	if (sym->type && spec.type && !type_equal(sym->type, spec.type))
	{
		type_check_error("function %s redeclared with return type %s but previously %s", name, type_display(spec.type), type_display(sym->type));
	}
	if (!sym->type && spec.type)
		sym->type = spec.type;
	symbol_apply_type_spec(sym, &spec);
	symbol_set_function_signature(sym, current_function_params, param_count);
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
	if (spec.is_stage_layout)
	{
		IR_Cmd* begin = ir_emit(IR_STAGE_LAYOUT_BEGIN);
		ir_apply_type_spec(begin, &spec);
		int layout_count = spec.layout_identifiers ? acount(spec.layout_identifiers) : 0;
		for (int i = 0; i < layout_count; ++i)
		{
			IR_Cmd* layout = ir_emit(IR_STAGE_LAYOUT_IDENTIFIER);
			layout->str0 = spec.layout_identifiers[i];
		}
		int assignment_count = spec.layout_assignments ? acount(spec.layout_assignments) : 0;
		for (int i = 0; i < assignment_count; ++i)
		{
			TypeLayoutAssignment* entry = &spec.layout_assignments[i];
			IR_Cmd* layout = ir_emit(IR_STAGE_LAYOUT_VALUE);
			layout->str0 = entry->identifier;
			layout->arg0 = entry->value;
		}
		expect(TOK_SEMI);
		ir_emit(IR_STAGE_LAYOUT_END);
		return;
	}
	if (spec.is_interface_block)
	{
		const char* instance_name = NULL;
		if (tok.kind == TOK_IDENTIFIER)
		{
			instance_name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
			next();
		}
		expect(TOK_SEMI);
		interface_block_decl(&spec, instance_name);
		return;
	}
	if (tok.kind == TOK_SEMI)
	{
		next();
		return;
	}
	IR_Cmd* inst;
	decl_emit_begin(&spec);
	while (1)
	{
		if (tok.kind != TOK_IDENTIFIER)
			parse_error("expected identifier in declaration");
		const char* name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
		inst = ir_emit(IR_DECL_VAR);
		inst->str0 = name;
		inst->qualifier_flags = spec.qualifier_flags;
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
	inst->is_unsigned_literal = tok.is_unsigned;
	next();
}

void expr_bool()
{
	IR_Cmd* inst = ir_emit(IR_PUSH_BOOL);
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
	Symbol* sym = symbol_table_find(inst->str0);
	if (sym)
	{
		inst->qualifier_flags = sym->qualifier_flags;
		if ((sym->kind == SYM_VAR || sym->kind == SYM_PARAM) && !symbol_has_qualifier(sym, SYM_QUAL_CONST))
			inst->is_lvalue = 1;
	}
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
	int base_idx = acount(g_ir) - 1;
	expr(); // parse index expr
	expect(TOK_RBRACK);
	IR_Cmd* inst = ir_emit(IR_INDEX);
	if (base_idx >= 0 && base_idx < acount(g_ir))
		inst->is_lvalue = g_ir[base_idx].is_lvalue;
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

int swizzle_is_assignable(const char* name, int len)
{
	if (len <= 1)
		return 1;
	int set = swizzle_set_from_char(name[0]);
	if (set < 0)
		return 0;
	int seen = 0;
	for (int i = 0; i < len; ++i)
	{
		int comp = swizzle_component_index(set, name[i]);
		if (comp < 0)
			return 0;
		int bit = 1 << comp;
		if (seen & bit)
			return 0;
		seen |= bit;
	}
	return 1;
}

void expr_member()
{
	int base_idx = acount(g_ir) - 1;
	int base_is_lvalue = (base_idx >= 0 && base_idx < acount(g_ir)) ? g_ir[base_idx].is_lvalue : 0;
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
		inst->is_lvalue = base_is_lvalue && swizzle_is_assignable(name, tok.len);
	}
	else
	{
		IR_Cmd* inst = ir_emit(IR_MEMBER);
		inst->str0 = sintern_range(name, name + tok.len);
		inst->is_lvalue = base_is_lvalue;
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

int ir_expect_lvalue_index(const char* error_msg)
{
	if (!acount(g_ir))
		parse_error(error_msg);
	int idx = acount(g_ir) - 1;
	if (!g_ir[idx].is_lvalue)
		parse_error(error_msg);
	return idx;
}

void expr_pre_inc()
{
	next();
	expr_binary(PREC_UNARY - 1);
	int idx = ir_expect_lvalue_index("operand of ++ must be an l-value");
	IR_Cmd* inst = ir_emit(IR_UNARY);
	inst->tok = TOK_PLUS_PLUS;
	inst->arg0 = idx;
	inst->arg1 = 0;
}

void expr_post_inc()
{
	int idx = ir_expect_lvalue_index("operand of ++ must be an l-value");
	IR_Cmd* inst = ir_emit(IR_UNARY);
	inst->tok = TOK_PLUS_PLUS;
	inst->arg0 = idx;
	inst->arg1 = 1;
}

void expr_pre_dec()
{
	next();
	expr_binary(PREC_UNARY - 1);
	int idx = ir_expect_lvalue_index("operand of -- must be an l-value");
	IR_Cmd* inst = ir_emit(IR_UNARY);
	inst->tok = TOK_MINUS_MINUS;
	inst->arg0 = idx;
	inst->arg1 = 0;
}

void expr_post_dec()
{
	int idx = ir_expect_lvalue_index("operand of -- must be an l-value");
	IR_Cmd* inst = ir_emit(IR_UNARY);
	inst->tok = TOK_MINUS_MINUS;
	inst->arg0 = idx;
	inst->arg1 = 1;
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
EXPR_BINARY(band, BIT_AND, AMP);
EXPR_BINARY(bor, BIT_OR, PIPE);
EXPR_BINARY(bxor, BIT_XOR, CARET);
EXPR_BINARY(shl, SHIFT, LSHIFT);
EXPR_BINARY(shr, SHIFT, RSHIFT);
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
EXPR_BINARY(minus_assign, ASSIGN, MINUS_ASSIGN);
EXPR_BINARY(mul_assign, ASSIGN, STAR_ASSIGN);
EXPR_BINARY(div_assign, ASSIGN, SLASH_ASSIGN);
EXPR_BINARY(mod_assign, ASSIGN, PERCENT_ASSIGN);
EXPR_BINARY(and_assign, ASSIGN, AND_ASSIGN);
EXPR_BINARY(or_assign, ASSIGN, OR_ASSIGN);
EXPR_BINARY(xor_assign, ASSIGN, XOR_ASSIGN);
EXPR_BINARY(shl_assign, ASSIGN, LSHIFT_ASSIGN);
EXPR_BINARY(shr_assign, ASSIGN, RSHIFT_ASSIGN);

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

int parse_switch_case_value()
{
	int sign = 1;
	if (tok.kind == TOK_PLUS || tok.kind == TOK_MINUS)
	{
		if (tok.kind == TOK_MINUS)
			sign = -1;
		next();
	}
	if (tok.kind != TOK_INT)
	{
		parse_error("expected integer literal in case label");
	}
	int value = tok.int_val;
	next();
	return sign * value;
}

void stmt_switch()
{
	expect(TOK_SWITCH);
	ir_emit(IR_SWITCH_BEGIN);
	expect(TOK_LPAREN);
	ir_emit(IR_SWITCH_SELECTOR_BEGIN);
	expr();
	expect(TOK_RPAREN);
	ir_emit(IR_SWITCH_SELECTOR_END);
	expect(TOK_LBRACE);
	symbol_table_enter_scope();
	int last_case_index = -1;
	while (tok.kind != TOK_RBRACE && tok.kind != TOK_EOF)
	{
		if (tok.kind == TOK_CASE || tok.kind == TOK_DEFAULT)
		{
			if (last_case_index >= 0)
			{
				IR_Cmd* prev_case = &g_ir[last_case_index];
				if (!(prev_case->arg1 & SWITCH_CASE_FLAG_HAS_BODY))
				{
					prev_case->arg1 |= SWITCH_CASE_FLAG_FALLTHROUGH;
				}
			}
			IR_Cmd* inst = ir_emit(IR_SWITCH_CASE);
			inst->arg0 = 0;
			inst->arg1 = 0;
			if (tok.kind == TOK_CASE)
			{
				next();
				inst->arg0 = parse_switch_case_value();
			}
			else
			{
				next();
				inst->arg1 |= SWITCH_CASE_FLAG_DEFAULT;
			}
			expect(TOK_COLON);
			last_case_index = acount(g_ir) - 1;
			continue;
		}
		if (last_case_index < 0)
		{
			parse_error("case label expected before statements in switch");
		}
		stmt();
		if (last_case_index >= 0)
		{
			g_ir[last_case_index].arg1 |= SWITCH_CASE_FLAG_HAS_BODY;
		}
	}
	if (last_case_index >= 0)
	{
		IR_Cmd* last_case = &g_ir[last_case_index];
		if (!(last_case->arg1 & SWITCH_CASE_FLAG_HAS_BODY))
		{
			last_case->arg1 |= SWITCH_CASE_FLAG_FALLTHROUGH;
		}
	}
	expect(TOK_RBRACE);
	symbol_table_leave_scope();
	ir_emit(IR_SWITCH_END);
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
	case TOK_SWITCH:
		stmt_switch();
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
	if (type_spec.is_stage_layout)
	{
		IR_Cmd* begin = ir_emit(IR_STAGE_LAYOUT_BEGIN);
		ir_apply_type_spec(begin, &type_spec);
		int layout_count = type_spec.layout_identifiers ? acount(type_spec.layout_identifiers) : 0;
		for (int i = 0; i < layout_count; ++i)
		{
			IR_Cmd* layout = ir_emit(IR_STAGE_LAYOUT_IDENTIFIER);
			layout->str0 = type_spec.layout_identifiers[i];
		}
		int assignment_count = type_spec.layout_assignments ? acount(type_spec.layout_assignments) : 0;
		for (int i = 0; i < assignment_count; ++i)
		{
			TypeLayoutAssignment* entry = &type_spec.layout_assignments[i];
			IR_Cmd* layout = ir_emit(IR_STAGE_LAYOUT_VALUE);
			layout->str0 = entry->identifier;
			layout->arg0 = entry->value;
		}
		expect(TOK_SEMI);
		ir_emit(IR_STAGE_LAYOUT_END);
		return;
	}
	if (type_spec.is_interface_block)
	{
		const char* instance_name = NULL;
		if (tok.kind == TOK_IDENTIFIER)
		{
			instance_name = sintern_range(tok.lexeme, tok.lexeme + tok.len);
			next();
		}
		expect(TOK_SEMI);
		interface_block_decl(&type_spec, instance_name);
		return;
	}
	if (tok.kind == TOK_SEMI)
	{
		next();
		return;
	}
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

#define TOK_EXPR_ASSIGN(ch1, tok1, prec1, lexpr1, rexpr1, tok_assign, prec_assign, lexpr_assign, rexpr_assign) \
	case ch1: \
		next_ch(); \
		if (match_ch('=')) \
		{ \
			tok.kind = TOK_##tok_assign; \
			tok.prec = PREC_##prec_assign; \
			tok.lexpr = expr_##lexpr_assign; \
			tok.rexpr = expr_##rexpr_assign; \
		} \
		else \
		{ \
			tok.kind = TOK_##tok1; \
			tok.prec = PREC_##prec1; \
			tok.lexpr = expr_##lexpr1; \
			tok.rexpr = expr_##rexpr1; \
		} \
		break;

#define TOK_EXPR_EXPR_ASSIGN(ch1, tok1, prec1, lexpr1, rexpr1, ch2, tok2, prec2, lexpr2, rexpr2, tok_assign, prec_assign, lexpr_assign, rexpr_assign) \
	case ch1: \
		next_ch(); \
		if (match_ch(ch2)) \
		{ \
			tok.kind = TOK_##tok2; \
			tok.prec = PREC_##prec2; \
			tok.lexpr = expr_##lexpr2; \
			tok.rexpr = expr_##rexpr2; \
		} \
		else if (match_ch('=')) \
		{ \
			tok.kind = TOK_##tok_assign; \
			tok.prec = PREC_##prec_assign; \
			tok.lexpr = expr_##lexpr_assign; \
			tok.rexpr = expr_##rexpr_assign; \
		} \
		else \
		{ \
			tok.kind = TOK_##tok1; \
			tok.prec = PREC_##prec1; \
			tok.lexpr = expr_##lexpr1; \
			tok.rexpr = expr_##rexpr1; \
		} \
		break;

#define TOK_SHIFT_REL(ch1, tok_rel, prec_rel, lexpr_rel, rexpr_rel, tok_rel_eq, prec_rel_eq, lexpr_rel_eq, rexpr_rel_eq, tok_shift, prec_shift, lexpr_shift, rexpr_shift, tok_shift_assign, prec_shift_assign, lexpr_shift_assign, rexpr_shift_assign) \
	case ch1: \
		next_ch(); \
		if (match_ch(ch1)) \
		{ \
			if (match_ch('=')) \
			{ \
				tok.kind = TOK_##tok_shift_assign; \
				tok.prec = PREC_##prec_shift_assign; \
				tok.lexpr = expr_##lexpr_shift_assign; \
				tok.rexpr = expr_##rexpr_shift_assign; \
			} \
			else \
			{ \
				tok.kind = TOK_##tok_shift; \
				tok.prec = PREC_##prec_shift; \
				tok.lexpr = expr_##lexpr_shift; \
				tok.rexpr = expr_##rexpr_shift; \
			} \
		} \
		else if (match_ch('=')) \
		{ \
			tok.kind = TOK_##tok_rel_eq; \
			tok.prec = PREC_##prec_rel_eq; \
			tok.lexpr = expr_##lexpr_rel_eq; \
			tok.rexpr = expr_##rexpr_rel_eq; \
		} \
		else \
		{ \
			tok.kind = TOK_##tok_rel; \
			tok.prec = PREC_##prec_rel; \
			tok.lexpr = expr_##lexpr_rel; \
			tok.rexpr = expr_##rexpr_rel; \
		} \
		break;

#define TOK_EXPR_OPTION(ch1, tok1, prec1, lexpr1, rexpr1, ch2, tok2, prec2, lexpr2, rexpr2) \
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

#define TOK_EXPR_OPTION2(ch1, tok1, prec1, lexpr1, rexpr1, ch2, tok2, prec2, lexpr2, rexpr2, ch3, tok3, prec3, lexpr3, rexpr3) \
	case ch1: \
		next_ch(); \
		if (match_ch(ch2)) \
		{ \
			tok.kind = TOK_##tok2; \
			tok.prec = PREC_##prec2; \
			tok.lexpr = expr_##lexpr2; \
			tok.rexpr = expr_##rexpr2; \
		} \
		else if (match_ch(ch3)) \
		{ \
			tok.kind = TOK_##tok3; \
			tok.prec = PREC_##prec3; \
			tok.lexpr = expr_##lexpr3; \
			tok.rexpr = expr_##rexpr3; \
		} \
		else \
		{ \
			tok.kind = TOK_##tok1; \
			tok.prec = PREC_##prec1; \
			tok.lexpr = expr_##lexpr1; \
			tok.rexpr = expr_##rexpr1; \
		} \
		break;

static int digit_value_for_base(char c, int base)
{
	int value = -1;
	if (c >= '0' && c <= '9')
		value = c - '0';
	else if (c >= 'a' && c <= 'f')
		value = 10 + (c - 'a');
	else if (c >= 'A' && c <= 'F')
		value = 10 + (c - 'A');
	return (value >= 0 && value < base) ? value : -1;
}

void lex_number()
{
	const char* start = at - 1;
	const char* suffix = NULL;
	const char* number_end = NULL;
	int is_float = 0;
	double float_val = 0.0;
	unsigned long long int_val = 0;
	int base = 10;
	int use_manual_base = 0;
	const char* digits_start = start;
	int has_unsigned_suffix = 0;
	if (*start == '0')
	{
		char next = start[1];
		if (next == 'x' || next == 'X')
		{
			base = 16;
			use_manual_base = 1;
			digits_start = start + 2;
		}
		else if (next == 'b' || next == 'B')
		{
			base = 2;
			use_manual_base = 1;
			digits_start = start + 2;
		}
		else if (next && next != '.' && next != 'e' && next != 'E')
		{
			base = 8;
			use_manual_base = 1;
			digits_start = start + 1;
		}
	}
	if (use_manual_base)
	{
		const char* p = digits_start;
		while (1)
		{
			int digit = digit_value_for_base(*p, base);
			if (digit < 0)
				break;
			int_val = int_val * (unsigned)base + (unsigned)digit;
			++p;
		}
		number_end = p;
		float_val = (double)int_val;
		if (base == 8 && digits_start == start + 1 && number_end == digits_start)
		{
			// Account for a standalone zero literal.
			number_end = digits_start;
		}
	}
	else
	{
		char* endptr = NULL;
		float_val = strtod(start, &endptr);
		number_end = endptr;
		for (const char* p = start; p < number_end; ++p)
		{
			if (*p == '.' || *p == 'e' || *p == 'E')
			{
				is_float = 1;
				break;
			}
		}
	}
	suffix = number_end;
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
			if (*suffix == 'u' || *suffix == 'U')
			{
				has_unsigned_suffix = 1;
			}
			++suffix;
		}
	}
	tok.is_unsigned = !is_float && has_unsigned_suffix;
	tok.lexeme = start;
	tok.len = (int)(suffix - start);
	tok.prec = 0;
	tok.rexpr = expr_error;
	if (is_float)
	{
		tok.kind = TOK_FLOAT;
		tok.lexpr = expr_float;
		tok.float_val = float_val;
	}
	else
	{
		tok.kind = TOK_INT;
		tok.lexpr = expr_int;
		if (use_manual_base)
		{
			tok.int_val = (int)int_val;
		}
		else
		{
			tok.int_val = 0;
			for (const char* p = start; p < number_end; ++p)
			{
				if (*p >= '0' && *p <= '9')
				{
					tok.int_val = tok.int_val * 10 + (*p - '0');
				}
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
	tok.is_unsigned = 0;

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
		TOK_EXPR_OPTION2('+', PLUS, ADD, pos, add, '+', PLUS_PLUS, POSTFIX, pre_inc, post_inc, '=', PLUS_ASSIGN, ASSIGN, error, plus_assign)
		TOK_EXPR_OPTION2('-', MINUS, ADD, neg, sub, '-', MINUS_MINUS, POSTFIX, pre_dec, post_dec, '=', MINUS_ASSIGN, ASSIGN, error, minus_assign)
		TOK_EXPR_ASSIGN('*', STAR, MUL, error, mul, STAR_ASSIGN, ASSIGN, error, mul_assign)
		TOK_EXPR_ASSIGN('/', SLASH, MUL, error, div, SLASH_ASSIGN, ASSIGN, error, div_assign)
		TOK_EXPR_ASSIGN('%', PERCENT, MUL, error, mod, PERCENT_ASSIGN, ASSIGN, error, mod_assign)
		TOK_EXPR('?', QUESTION, TERNARY, error, ternary)

		// two-char combos
		TOK_SHIFT_REL('<', LT, REL, error, lt, LE, REL, error, le, LSHIFT, SHIFT, error, shl, LSHIFT_ASSIGN, ASSIGN, error, shl_assign)
		TOK_SHIFT_REL('>', GT, REL, error, gt, GE, REL, error, ge, RSHIFT, SHIFT, error, shr, RSHIFT_ASSIGN, ASSIGN, error, shr_assign)
		TOK_EXPR_EXPR('=', ASSIGN, ASSIGN, error, assign, '=', EQ, EQ, error, eq)
		TOK_EXPR_EXPR('!', NOT, UNARY, not, error, '=', NE, EQ, error, ne)
		TOK_EXPR_EXPR_ASSIGN('&', AMP, BIT_AND, error, band, '&', AND_AND, AND_AND, error, land, AND_ASSIGN, ASSIGN, error, and_assign)
		TOK_EXPR_EXPR_ASSIGN('|', PIPE, BIT_OR, error, bor, '|', OR_OR, OR_OR, error, lor, OR_ASSIGN, ASSIGN, error, or_assign)
		TOK_EXPR_ASSIGN('^', CARET, BIT_XOR, error, bxor, XOR_ASSIGN, ASSIGN, error, xor_assign)
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
		else if (tok.lexeme == kw_switch)
		{
			tok.kind = TOK_SWITCH;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_case)
		{
			tok.kind = TOK_CASE;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_default)
		{
			tok.kind = TOK_DEFAULT;
			tok.lexpr = expr_error;
		}
		else if (tok.lexeme == kw_true || tok.lexeme == kw_false)
		{
			tok.kind = TOK_BOOL;
			tok.lexpr = expr_bool;
			tok.int_val = tok.lexeme == kw_true;
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

Token peek_token()
{
	Token saved_tok = tok;
	const char* saved_at = at;
	char saved_ch = ch;
	next();
	Token result = tok;
	tok = saved_tok;
	at = saved_at;
	ch = saved_ch;
	return result;
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
	tok.is_unsigned = 0;
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

void compiler_set_shader_stage(ShaderStage stage)
{
	g_shader_stage = stage;
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

//--------------------------------------------------------------------------------------------------
// Type system and semantic analysis helpers.

typedef struct Symbol Symbol;
typedef enum BuiltinFuncKind BuiltinFuncKind;
Symbol* symbol_table_find(const char* name);
int type_component_count(const Type* type);

const char* type_tag_name(TypeTag tag)
{
	static const char* names[T_TYPE_COUNT] = {
		[T_VOID] = "void",
		[T_BOOL] = "bool",
		[T_INT] = "int",
		[T_UINT] = "uint",
		[T_INT64] = "int64_t",
		[T_UINT64] = "uint64_t",
		[T_FLOAT] = "float",
		[T_DOUBLE] = "double",
		[T_VEC] = "vec",
		[T_MAT] = "mat",
		[T_SAMPLER] = "sampler",
		[T_IMAGE] = "image",
		[T_ARRAY] = "array",
		[T_STRUCT] = "struct",
	};
	if (tag < 0 || tag >= T_TYPE_COUNT)
		return "unknown";
	return names[tag] ? names[tag] : "unknown";
}

enum
{
	TYPE_DIM_UNKNOWN = 0,
	TYPE_DIM_1D = 1,
	TYPE_DIM_2D = 2,
	TYPE_DIM_3D = 3,
	TYPE_DIM_CUBE = 4,
	TYPE_DIM_RECT = 5,
	TYPE_DIM_BUFFER = 6,
	TYPE_DIM_2D_MS = 7,
	TYPE_DIM_FLAG_ARRAY = 1 << 4,
	TYPE_DIM_FLAG_SHADOW = 1 << 5,
};

static Type* type_system_add_internal(const char* name, Type type)
{
	uint64_t key = (uint64_t)name;
	uint64_t existing = map_get(ts->map, key);
	if (existing)
		return &ts->types[(int)existing - 1];
	type.name = name;
	apush(ts->types, type);
	int idx = acount(ts->types);
	map_add(ts->map, key, (uint64_t)idx);
	return &ts->types[idx - 1];
}

Type* type_system_get(const char* name)
{
	uint64_t idx = map_get(ts->map, (uint64_t)name);
	if (!idx)
		return NULL;
	return &ts->types[(int)idx - 1];
}

StructInfo* type_struct_info(Type* type)
{
	if (!type || type->tag != T_STRUCT)
		return NULL;
	return type->user ? (StructInfo*)type->user : NULL;
}

static StructInfo* type_struct_info_ensure(Type* type)
{
	if (!type || type->tag != T_STRUCT)
		return NULL;
	StructInfo* info = type_struct_info(type);
	if (!info)
	{
		info = (StructInfo*)calloc(1, sizeof(StructInfo));
		info->name = type->name;
		type->user = info;
	}
	return info;
}

// Ensure we have a struct type ready for declarations like struct Material.
// ...struct Material { vec4 albedo; };
Type* type_system_declare_struct(const char* name)
{
	Type* existing = type_system_get(name);
	if (existing)
	{
		if (existing->tag == T_STRUCT)
		{
			type_struct_clear(existing);
			return existing;
		}
		return existing;
	}
	Type type = { 0 };
	type.tag = T_STRUCT;
	type.cols = 1;
	type.rows = 1;
	type.base = T_VOID;
	type.array_len = 0;
	type.name = name;
	Type* result = type_system_add_internal(name, type);
	type_struct_clear(result);
	return result;
}
// Reset cached members so repeated declarations start clean.
// ...struct Material { vec4 albedo; };
void type_struct_clear(Type* type)
{
	StructInfo* info = type_struct_info_ensure(type);
	if (!info)
		return;
	info->name = type ? type->name : NULL;
	if (info->members)
	{
		for (int i = 0; i < acount(info->members); ++i)
		{
			StructMember* member = &info->members[i];
			if (member->array_dims)
			{
				afree(member->array_dims);
				member->array_dims = NULL;
			}
		}
		aclear(info->members);
	}
	if (info->layout_identifiers)
		aclear(info->layout_identifiers);
}
// Append a member for struct fields encountered in user code.
// ...struct Material { vec4 albedo; };
StructMember* type_struct_add_member(Type* type, const char* name, Type* member_type)
{
	StructInfo* info = type_struct_info_ensure(type);
	if (!info)
		return NULL;
	StructMember member = (StructMember){ 0 };
	member.name = name;
	member.declared_type = member_type;
	member.type = member_type;
	member.array_dims = NULL;
	member.has_array = 0;
	member.array_len = 0;
	member.array_unsized = 0;
	apush(info->members, member);
	return &alast(info->members);
}

// Store layout qualifiers that come from layout(...) blocks on members.
// ...layout(location = 0) vec4 color;
void type_struct_member_set_layout(StructMember* member, unsigned layout_flags, int set, int binding, int location)
{
	if (!member)
		return;
	member->layout_flags = layout_flags;
	member->layout_set = set;
	member->layout_binding = binding;
	member->layout_location = location;
}

// Update a member to reflect array declarations like float weights[4].
// ...float weights[4];
void type_struct_member_mark_array(StructMember* member, int size, int unsized)
{
	if (!member)
		return;
	StructMemberArrayDim dim = { 0 };
	dim.size = size;
	dim.unsized = unsized;
	dim.type = (Type){ 0 };
	dim.type.tag = T_ARRAY;
	apush(member->array_dims, dim);
	member->has_array = 1;
	Type* base = member->declared_type;
	for (int i = acount(member->array_dims) - 1; i >= 0; --i)
	{
		StructMemberArrayDim* info = &member->array_dims[i];
		Type* element = base;
		info->type.base = element ? element->tag : T_VOID;
		info->type.cols = element ? element->cols : 1;
		info->type.rows = element ? element->rows : 1;
		info->type.array_len = info->unsized ? -1 : info->size;
		info->type.user = element;
		info->type.name = NULL;
		base = &info->type;
	}
	member->type = base;
	if (member->array_dims && acount(member->array_dims) > 0)
	{
		StructMemberArrayDim* outer = &member->array_dims[0];
		member->array_len = outer->unsized ? -1 : outer->size;
		member->array_unsized = outer->unsized;
	}
}

void type_struct_set_layout_identifiers(Type* type, const char** identifiers, int count)
{
	StructInfo* info = type_struct_info_ensure(type);
	if (!info)
		return;
	if (info->layout_identifiers)
		aclear(info->layout_identifiers);
	for (int i = 0; i < count; ++i)
	{
		const char* ident = identifiers ? identifiers[i] : NULL;
		if (ident)
			apush(info->layout_identifiers, ident);
	}
}

StructMember* type_struct_find_member(Type* type, const char* name)
{
	StructInfo* info = type_struct_info(type);
	if (!info || !name)
		return NULL;
	for (int i = 0; i < acount(info->members); ++i)
	{
		StructMember* member = &info->members[i];
		if (member->name == name)
			return member;
	}
	return NULL;
}

int type_struct_member_count(Type* type)
{
	StructInfo* info = type_struct_info(type);
	if (!info)
		return 0;
	return acount(info->members);
}

StructMember* type_struct_member_at(Type* type, int index)
{
	StructInfo* info = type_struct_info(type);
	if (!info)
		return NULL;
	if (index < 0 || index >= acount(info->members))
		return NULL;
	return &info->members[index];
}

void type_system_init_builtins()
{
	typedef struct TypeInit
	{
		const char* name;
		Type type;
	} TypeInit;
#define SAMPLER(name, base_tag, dim_value) \
	{ \
		name, \
				{ .tag = T_SAMPLER, .cols = 1, .rows = 1, .base = base_tag, .dim = dim_value, .array_len = 0 } \
	}
#define IMAGE(name, base_tag, dim_value) \
	{ \
		name, \
				{ .tag = T_IMAGE, .cols = 1, .rows = 1, .base = base_tag, .dim = dim_value, .array_len = 0 } \
	}
	const TypeInit builtins[] = {
		{ "void", { .tag = T_VOID, .cols = 1, .rows = 1, .base = T_VOID, .array_len = 0 } },
		{ "bool", { .tag = T_BOOL, .cols = 1, .rows = 1, .base = T_BOOL, .array_len = 0 } },
		{ "int", { .tag = T_INT, .cols = 1, .rows = 1, .base = T_INT, .array_len = 0 } },
		{ "uint", { .tag = T_UINT, .cols = 1, .rows = 1, .base = T_UINT, .array_len = 0 } },
		{ "int64_t", { .tag = T_INT64, .cols = 1, .rows = 1, .base = T_INT64, .array_len = 0 } },
		{ "uint64_t", { .tag = T_UINT64, .cols = 1, .rows = 1, .base = T_UINT64, .array_len = 0 } },
		{ "atomic_uint", { .tag = T_UINT, .cols = 1, .rows = 1, .base = T_UINT, .array_len = 0 } },
		{ "float", { .tag = T_FLOAT, .cols = 1, .rows = 1, .base = T_FLOAT, .array_len = 0 } },
		{ "double", { .tag = T_DOUBLE, .cols = 1, .rows = 1, .base = T_DOUBLE, .array_len = 0 } },
		{ "vec2", { .tag = T_VEC, .cols = 2, .rows = 1, .base = T_FLOAT, .array_len = 0 } },
		{ "vec3", { .tag = T_VEC, .cols = 3, .rows = 1, .base = T_FLOAT, .array_len = 0 } },
		{ "vec4", { .tag = T_VEC, .cols = 4, .rows = 1, .base = T_FLOAT, .array_len = 0 } },
		{ "dvec2", { .tag = T_VEC, .cols = 2, .rows = 1, .base = T_DOUBLE, .array_len = 0 } },
		{ "dvec3", { .tag = T_VEC, .cols = 3, .rows = 1, .base = T_DOUBLE, .array_len = 0 } },
		{ "dvec4", { .tag = T_VEC, .cols = 4, .rows = 1, .base = T_DOUBLE, .array_len = 0 } },
		{ "ivec2", { .tag = T_VEC, .cols = 2, .rows = 1, .base = T_INT, .array_len = 0 } },
		{ "ivec3", { .tag = T_VEC, .cols = 3, .rows = 1, .base = T_INT, .array_len = 0 } },
		{ "ivec4", { .tag = T_VEC, .cols = 4, .rows = 1, .base = T_INT, .array_len = 0 } },
		{ "uvec2", { .tag = T_VEC, .cols = 2, .rows = 1, .base = T_UINT, .array_len = 0 } },
		{ "uvec3", { .tag = T_VEC, .cols = 3, .rows = 1, .base = T_UINT, .array_len = 0 } },
		{ "uvec4", { .tag = T_VEC, .cols = 4, .rows = 1, .base = T_UINT, .array_len = 0 } },
		{ "i64vec2", { .tag = T_VEC, .cols = 2, .rows = 1, .base = T_INT64, .array_len = 0 } },
		{ "i64vec3", { .tag = T_VEC, .cols = 3, .rows = 1, .base = T_INT64, .array_len = 0 } },
		{ "i64vec4", { .tag = T_VEC, .cols = 4, .rows = 1, .base = T_INT64, .array_len = 0 } },
		{ "u64vec2", { .tag = T_VEC, .cols = 2, .rows = 1, .base = T_UINT64, .array_len = 0 } },
		{ "u64vec3", { .tag = T_VEC, .cols = 3, .rows = 1, .base = T_UINT64, .array_len = 0 } },
		{ "u64vec4", { .tag = T_VEC, .cols = 4, .rows = 1, .base = T_UINT64, .array_len = 0 } },
		{ "bvec2", { .tag = T_VEC, .cols = 2, .rows = 1, .base = T_BOOL, .array_len = 0 } },
		{ "bvec3", { .tag = T_VEC, .cols = 3, .rows = 1, .base = T_BOOL, .array_len = 0 } },
		{ "bvec4", { .tag = T_VEC, .cols = 4, .rows = 1, .base = T_BOOL, .array_len = 0 } },
		{ "mat2", { .tag = T_MAT, .cols = 2, .rows = 2, .base = T_FLOAT, .array_len = 0 } },
		{ "mat3", { .tag = T_MAT, .cols = 3, .rows = 3, .base = T_FLOAT, .array_len = 0 } },
		{ "mat4", { .tag = T_MAT, .cols = 4, .rows = 4, .base = T_FLOAT, .array_len = 0 } },
		{ "mat2x3", { .tag = T_MAT, .cols = 2, .rows = 3, .base = T_FLOAT, .array_len = 0 } },
		{ "mat2x4", { .tag = T_MAT, .cols = 2, .rows = 4, .base = T_FLOAT, .array_len = 0 } },
		{ "mat3x2", { .tag = T_MAT, .cols = 3, .rows = 2, .base = T_FLOAT, .array_len = 0 } },
		{ "mat3x4", { .tag = T_MAT, .cols = 3, .rows = 4, .base = T_FLOAT, .array_len = 0 } },
		{ "mat4x2", { .tag = T_MAT, .cols = 4, .rows = 2, .base = T_FLOAT, .array_len = 0 } },
		{ "mat4x3", { .tag = T_MAT, .cols = 4, .rows = 3, .base = T_FLOAT, .array_len = 0 } },
		{ "dmat2", { .tag = T_MAT, .cols = 2, .rows = 2, .base = T_DOUBLE, .array_len = 0 } },
		{ "dmat3", { .tag = T_MAT, .cols = 3, .rows = 3, .base = T_DOUBLE, .array_len = 0 } },
		{ "dmat4", { .tag = T_MAT, .cols = 4, .rows = 4, .base = T_DOUBLE, .array_len = 0 } },
		{ "dmat2x3", { .tag = T_MAT, .cols = 2, .rows = 3, .base = T_DOUBLE, .array_len = 0 } },
		{ "dmat2x4", { .tag = T_MAT, .cols = 2, .rows = 4, .base = T_DOUBLE, .array_len = 0 } },
		{ "dmat3x2", { .tag = T_MAT, .cols = 3, .rows = 2, .base = T_DOUBLE, .array_len = 0 } },
		{ "dmat3x4", { .tag = T_MAT, .cols = 3, .rows = 4, .base = T_DOUBLE, .array_len = 0 } },
		{ "dmat4x2", { .tag = T_MAT, .cols = 4, .rows = 2, .base = T_DOUBLE, .array_len = 0 } },
		{ "dmat4x3", { .tag = T_MAT, .cols = 4, .rows = 3, .base = T_DOUBLE, .array_len = 0 } },
		SAMPLER("sampler1D", T_FLOAT, TYPE_DIM_1D),
		SAMPLER("sampler2D", T_FLOAT, TYPE_DIM_2D),
		SAMPLER("sampler3D", T_FLOAT, TYPE_DIM_3D),
		SAMPLER("samplerCube", T_FLOAT, TYPE_DIM_CUBE),
		SAMPLER("sampler1DShadow", T_FLOAT, TYPE_DIM_1D | TYPE_DIM_FLAG_SHADOW),
		SAMPLER("sampler2DShadow", T_FLOAT, TYPE_DIM_2D | TYPE_DIM_FLAG_SHADOW),
		SAMPLER("samplerCubeShadow", T_FLOAT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_SHADOW),
		SAMPLER("sampler1DArray", T_FLOAT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("sampler2DArray", T_FLOAT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("sampler1DArrayShadow", T_FLOAT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY | TYPE_DIM_FLAG_SHADOW),
		SAMPLER("sampler2DArrayShadow", T_FLOAT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY | TYPE_DIM_FLAG_SHADOW),
		SAMPLER("samplerCubeArray", T_FLOAT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("samplerCubeArrayShadow", T_FLOAT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY | TYPE_DIM_FLAG_SHADOW),
		SAMPLER("sampler2DMS", T_FLOAT, TYPE_DIM_2D_MS),
		SAMPLER("sampler2DMSArray", T_FLOAT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("samplerBuffer", T_FLOAT, TYPE_DIM_BUFFER),
		SAMPLER("sampler2DRect", T_FLOAT, TYPE_DIM_RECT),
		SAMPLER("sampler2DRectShadow", T_FLOAT, TYPE_DIM_RECT | TYPE_DIM_FLAG_SHADOW),
		SAMPLER("isampler1D", T_INT, TYPE_DIM_1D),
		SAMPLER("isampler2D", T_INT, TYPE_DIM_2D),
		SAMPLER("isampler3D", T_INT, TYPE_DIM_3D),
		SAMPLER("isamplerCube", T_INT, TYPE_DIM_CUBE),
		SAMPLER("isampler1DArray", T_INT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("isampler2DArray", T_INT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("isamplerCubeArray", T_INT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("isampler2DMS", T_INT, TYPE_DIM_2D_MS),
		SAMPLER("isampler2DMSArray", T_INT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("isamplerBuffer", T_INT, TYPE_DIM_BUFFER),
		SAMPLER("isampler2DRect", T_INT, TYPE_DIM_RECT),
		SAMPLER("usampler1D", T_UINT, TYPE_DIM_1D),
		SAMPLER("usampler2D", T_UINT, TYPE_DIM_2D),
		SAMPLER("usampler3D", T_UINT, TYPE_DIM_3D),
		SAMPLER("usamplerCube", T_UINT, TYPE_DIM_CUBE),
		SAMPLER("usampler1DArray", T_UINT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("usampler2DArray", T_UINT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("usamplerCubeArray", T_UINT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("usampler2DMS", T_UINT, TYPE_DIM_2D_MS),
		SAMPLER("usampler2DMSArray", T_UINT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY),
		SAMPLER("usamplerBuffer", T_UINT, TYPE_DIM_BUFFER),
		SAMPLER("usampler2DRect", T_UINT, TYPE_DIM_RECT),
		IMAGE("image1D", T_FLOAT, TYPE_DIM_1D),
		IMAGE("image2D", T_FLOAT, TYPE_DIM_2D),
		IMAGE("image3D", T_FLOAT, TYPE_DIM_3D),
		IMAGE("imageCube", T_FLOAT, TYPE_DIM_CUBE),
		IMAGE("imageBuffer", T_FLOAT, TYPE_DIM_BUFFER),
		IMAGE("image1DArray", T_FLOAT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY),
		IMAGE("image2DArray", T_FLOAT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY),
		IMAGE("imageCubeArray", T_FLOAT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY),
		IMAGE("image2DMS", T_FLOAT, TYPE_DIM_2D_MS),
		IMAGE("image2DMSArray", T_FLOAT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY),
		IMAGE("image2DRect", T_FLOAT, TYPE_DIM_RECT),
		IMAGE("iimage1D", T_INT, TYPE_DIM_1D),
		IMAGE("iimage2D", T_INT, TYPE_DIM_2D),
		IMAGE("iimage3D", T_INT, TYPE_DIM_3D),
		IMAGE("iimageCube", T_INT, TYPE_DIM_CUBE),
		IMAGE("iimageBuffer", T_INT, TYPE_DIM_BUFFER),
		IMAGE("iimage1DArray", T_INT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY),
		IMAGE("iimage2DArray", T_INT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY),
		IMAGE("iimageCubeArray", T_INT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY),
		IMAGE("iimage2DMS", T_INT, TYPE_DIM_2D_MS),
		IMAGE("iimage2DMSArray", T_INT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY),
		IMAGE("iimage2DRect", T_INT, TYPE_DIM_RECT),
		IMAGE("uimage1D", T_UINT, TYPE_DIM_1D),
		IMAGE("uimage2D", T_UINT, TYPE_DIM_2D),
		IMAGE("uimage3D", T_UINT, TYPE_DIM_3D),
		IMAGE("uimageCube", T_UINT, TYPE_DIM_CUBE),
		IMAGE("uimageBuffer", T_UINT, TYPE_DIM_BUFFER),
		IMAGE("uimage1DArray", T_UINT, TYPE_DIM_1D | TYPE_DIM_FLAG_ARRAY),
		IMAGE("uimage2DArray", T_UINT, TYPE_DIM_2D | TYPE_DIM_FLAG_ARRAY),
		IMAGE("uimageCubeArray", T_UINT, TYPE_DIM_CUBE | TYPE_DIM_FLAG_ARRAY),
		IMAGE("uimage2DMS", T_UINT, TYPE_DIM_2D_MS),
		IMAGE("uimage2DMSArray", T_UINT, TYPE_DIM_2D_MS | TYPE_DIM_FLAG_ARRAY),
		IMAGE("uimage2DRect", T_UINT, TYPE_DIM_RECT),
	};
#undef SAMPLER
#undef IMAGE
	for (size_t i = 0; i < sizeof(builtins) / sizeof(builtins[0]); ++i)
	{
		const char* name = sintern_range(builtins[i].name, builtins[i].name + strlen(builtins[i].name));
		type_system_add_internal(name, builtins[i].type);
	}
	g_type_void = type_system_get(sintern("void"));
	g_type_bool = type_system_get(sintern("bool"));
	g_type_int = type_system_get(sintern("int"));
	g_type_uint = type_system_get(sintern("uint"));
	g_type_int64 = type_system_get(sintern("int64_t"));
	g_type_uint64 = type_system_get(sintern("uint64_t"));
	g_type_float = type_system_get(sintern("float"));
	g_type_double = type_system_get(sintern("double"));
}

void type_system_free()
{
	for (int i = 0; i < acount(ts->types); ++i)
	{
		Type* type = &ts->types[i];
		if (type->tag == T_STRUCT && type->user)
		{
			StructInfo* info = (StructInfo*)type->user;
			if (info)
			{
				if (info->members)
				{
					for (int j = 0; j < acount(info->members); ++j)
					{
						StructMember* member = &info->members[j];
						if (member->array_dims)
							afree(member->array_dims);
					}
					afree(info->members);
				}
				if (info->layout_identifiers)
					afree(info->layout_identifiers);
				free(info);
			}
			type->user = NULL;
		}
	}
	map_free(ts->map);
	ts->map = (Map){ 0 };
	afree(ts->types);
}

const char* type_display(const Type* type)
{
	if (!type)
		return "<unknown>";
	if (type->name)
		return type->name;
	return type_tag_name(type->tag);
}

TypeTag type_base_type(const Type* type)
{
	if (!type)
		return T_VOID;
	switch (type->tag)
	{
	case T_VEC:
	case T_MAT:
	case T_ARRAY:
		return (TypeTag)type->base;
	default:
		return type->tag;
	}
}

int type_is_scalar(const Type* type)
{
	if (!type)
		return 0;
	if (type->tag == T_VEC || type->tag == T_MAT || type->tag == T_ARRAY)
		return 0;
	return type->cols == 1 && type->rows == 1 && type->tag != T_VOID;
}

int type_is_vector(const Type* type)
{
	return type && type->tag == T_VEC;
}

int type_is_matrix(const Type* type)
{
	return type && type->tag == T_MAT;
}

int type_base_is_numeric(TypeTag tag)
{
	switch (tag)
	{
	case T_INT:
	case T_UINT:
	case T_INT64:
	case T_UINT64:
	case T_FLOAT:
	case T_DOUBLE:
		return 1;
	default:
		return 0;
	}
}

int type_base_is_integer(TypeTag tag)
{
	return tag == T_INT || tag == T_UINT || tag == T_INT64 || tag == T_UINT64;
}

int type_is_numeric(const Type* type)
{
	return type_base_is_numeric(type_base_type(type));
}

int type_is_integer(const Type* type)
{
	return type_base_is_integer(type_base_type(type));
}

int type_is_bool_like(const Type* type)
{
	return type_base_type(type) == T_BOOL;
}

int type_equal(const Type* a, const Type* b)
{
	if (a == b)
		return 1;
	if (!a || !b)
		return 0;
	if (a->tag != b->tag)
		return 0;
	if (a->cols != b->cols || a->rows != b->rows)
		return 0;
	return type_base_type(a) == type_base_type(b);
}

int type_base_can_convert(TypeTag from, TypeTag to)
{
	if (from == to)
		return 1;
	if (to == T_BOOL)
		return from == T_BOOL || type_base_is_numeric(from);
	if (from == T_BOOL)
		return to == T_BOOL;
	if (type_base_is_numeric(from) && type_base_is_numeric(to))
		return 1;
	return 0;
}

int type_scalar_can_convert(const Type* from, const Type* to)
{
	return type_base_can_convert(type_base_type(from), type_base_type(to));
}

const char* type_vector_name(TypeTag base, int cols)
{
	const char* prefix = NULL;
	switch (base)
	{
	case T_FLOAT:
		prefix = "vec";
		break;
	case T_INT:
		prefix = "ivec";
		break;
	case T_UINT:
		prefix = "uvec";
		break;
	case T_DOUBLE:
		prefix = "dvec";
		break;
	case T_INT64:
		prefix = "i64vec";
		break;
	case T_UINT64:
		prefix = "u64vec";
		break;
	case T_BOOL:
		prefix = "bvec";
		break;
	default:
		return NULL;
	}
	char buf[16];
	int n = snprintf(buf, sizeof(buf), "%s%d", prefix, cols);
	if (n <= 0 || n >= (int)sizeof(buf))
		return NULL;
	return sintern_range(buf, buf + n);
}

const char* type_matrix_name(TypeTag base, int cols, int rows)
{
	if (base != T_FLOAT && base != T_DOUBLE)
		return NULL;
	char buf[16];
	int n = 0;
	if (cols == rows)
	{
		n = snprintf(buf, sizeof(buf), "%s%d", base == T_FLOAT ? "mat" : "dmat", cols);
	}
	else
	{
		n = snprintf(buf, sizeof(buf), "%s%dx%d", base == T_FLOAT ? "mat" : "dmat", cols, rows);
	}
	if (n <= 0 || n >= (int)sizeof(buf))
		return NULL;
	return sintern_range(buf, buf + n);
}

Type* type_get_scalar(TypeTag base)
{
	switch (base)
	{
	case T_BOOL:
		return g_type_bool;
	case T_INT:
		return g_type_int;
	case T_UINT:
		return g_type_uint;
	case T_INT64:
		return g_type_int64;
	case T_UINT64:
		return g_type_uint64;
	case T_FLOAT:
		return g_type_float;
	case T_DOUBLE:
		return g_type_double;
	default:
		return NULL;
	}
}

Type* type_get_vector(TypeTag base, int cols)
{
	const char* name = type_vector_name(base, cols);
	if (!name)
		return NULL;
	return type_system_get(name);
}

Type* type_get_matrix(TypeTag base, int cols, int rows)
{
	const char* name = type_matrix_name(base, cols, rows);
	if (!name)
		return NULL;
	return type_system_get(name);
}

static const char* builtin_func_name(const Symbol* sym)
{
	return (sym && sym->name) ? sym->name : "<builtin>";
}

static int type_is_scalar_or_vector(const Type* type)
{
	if (!type)
		return 0;
	return type_is_scalar(type) || type_is_vector(type);
}

static int type_is_floating_point(const Type* type)
{
	if (!type)
		return 0;
	TypeTag base = type_base_type(type);
	return base == T_FLOAT || base == T_DOUBLE;
}

static Type* builtin_result_same(Type** args, int argc, int index)
{
	if (index < 0 || index >= argc)
		return NULL;
	return args[index];
}

typedef enum BuiltinBaseSet
{
	BUILTIN_BASE_ANY,
	BUILTIN_BASE_FLOATING,
	BUILTIN_BASE_FLOAT_ONLY,
	BUILTIN_BASE_NUMERIC,
	BUILTIN_BASE_BOOL,
	BUILTIN_BASE_SAME_AS_REF,
	BUILTIN_BASE_FLOAT_OR_BOOL,
} BuiltinBaseSet;

typedef enum BuiltinShapeRule
{
	BUILTIN_SHAPE_ANY,
	BUILTIN_SHAPE_SCALAR,
	BUILTIN_SHAPE_VECTOR,
	BUILTIN_SHAPE_SCALAR_OR_VECTOR,
	BUILTIN_SHAPE_MATCH_REF,
	BUILTIN_SHAPE_SCALAR_OR_MATCH,
	BUILTIN_SHAPE_MATRIX,
	BUILTIN_SHAPE_SQUARE_MATRIX,
} BuiltinShapeRule;

typedef struct BuiltinArgSig
{
	const char* role;
	BuiltinBaseSet base;
	BuiltinShapeRule shape;
	int ref_index;
} BuiltinArgSig;

#define ARRAY_COUNT(arr) ((int)(sizeof(arr) / sizeof((arr)[0])))

typedef enum BuiltinSignatureResultKind
{
	BUILTIN_SIGNATURE_RESULT_NONE,
	BUILTIN_SIGNATURE_RESULT_SAME,
	BUILTIN_SIGNATURE_RESULT_SCALAR,
	BUILTIN_SIGNATURE_RESULT_VECTOR,
	BUILTIN_SIGNATURE_RESULT_CUSTOM,
} BuiltinSignatureResultKind;

typedef Type* (*BuiltinSignatureCustomResult)(const Symbol* sym, Type** args, int argc);

typedef struct BuiltinSignature
{
	BuiltinFuncKind kind;
	const BuiltinArgSig* args;
	int arg_count;
	BuiltinSignatureResultKind result_kind;
	int result_index;
	int result_components;
	BuiltinSignatureCustomResult custom_result;
} BuiltinSignature;

static Type* builtin_result_mix(const Symbol* sym, Type** args, int argc);
static Type* builtin_result_determinant(const Symbol* sym, Type** args, int argc);
static Type* builtin_result_outer_product(const Symbol* sym, Type** args, int argc);
static int builtin_check_args(const Symbol* sym, Type** args, int argc, const BuiltinArgSig* sigs, int sig_count);
static Type* builtin_result_scalar(Type** args, int argc, int index);
static Type* builtin_result_vector(Type** args, int argc, int index, int components);

static const BuiltinArgSig builtin_length_sig[] = {
	{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
};

static const BuiltinArgSig builtin_distance_sig[] = {
	{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
	{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_MATCH_REF, 0 },
};

static const BuiltinArgSig builtin_derivative_sig[] = {
	{ "first", BUILTIN_BASE_FLOAT_ONLY, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
};

static const BuiltinArgSig builtin_normalize_sig[] = {
	{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
};

static const BuiltinArgSig builtin_reflect_sig[] = {
	{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_VECTOR, -1 },
	{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_MATCH_REF, 0 },
};

static const BuiltinArgSig builtin_refract_sig[] = {
	{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_VECTOR, -1 },
	{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_MATCH_REF, 0 },
	{ "third", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR, 0 },
};

static const BuiltinArgSig builtin_mix_base_sig[] = {
	{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
	{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
	{ "third", BUILTIN_BASE_ANY, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
};

static const BuiltinArgSig builtin_mix_float_sig[] = {
	{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
	{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
	{ "third", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
};

static const BuiltinArgSig builtin_mix_mask_sig[] = {
	{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
	{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
	{ "third", BUILTIN_BASE_BOOL, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
};

static const BuiltinArgSig builtin_determinant_sig[] = {
	{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SQUARE_MATRIX, -1 },
};

static const BuiltinArgSig builtin_outer_product_sig[] = {
	{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_VECTOR, -1 },
	{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_VECTOR, 0 },
};

static const BuiltinSignature builtin_signatures[] = {
	{ BUILTIN_LENGTH, builtin_length_sig, ARRAY_COUNT(builtin_length_sig), BUILTIN_SIGNATURE_RESULT_SCALAR, 0, 0, NULL },
	{ BUILTIN_DISTANCE, builtin_distance_sig, ARRAY_COUNT(builtin_distance_sig), BUILTIN_SIGNATURE_RESULT_SCALAR, 0, 0, NULL },
	{ BUILTIN_DOT, builtin_distance_sig, ARRAY_COUNT(builtin_distance_sig), BUILTIN_SIGNATURE_RESULT_SCALAR, 0, 0, NULL },
	{ BUILTIN_NORMALIZE, builtin_normalize_sig, ARRAY_COUNT(builtin_normalize_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_REFLECT, builtin_reflect_sig, ARRAY_COUNT(builtin_reflect_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_REFRACT, builtin_refract_sig, ARRAY_COUNT(builtin_refract_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_MIX, builtin_mix_base_sig, ARRAY_COUNT(builtin_mix_base_sig), BUILTIN_SIGNATURE_RESULT_CUSTOM, 0, 0, builtin_result_mix },
	{ BUILTIN_DETERMINANT, builtin_determinant_sig, ARRAY_COUNT(builtin_determinant_sig), BUILTIN_SIGNATURE_RESULT_CUSTOM, 0, 0, builtin_result_determinant },
	{ BUILTIN_OUTER_PRODUCT, builtin_outer_product_sig, ARRAY_COUNT(builtin_outer_product_sig), BUILTIN_SIGNATURE_RESULT_CUSTOM, 0, 0, builtin_result_outer_product },
	{ BUILTIN_FWIDTH, builtin_derivative_sig, ARRAY_COUNT(builtin_derivative_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_FWIDTH_FINE, builtin_derivative_sig, ARRAY_COUNT(builtin_derivative_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_FWIDTH_COARSE, builtin_derivative_sig, ARRAY_COUNT(builtin_derivative_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_DFDX, builtin_derivative_sig, ARRAY_COUNT(builtin_derivative_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_DFDX_FINE, builtin_derivative_sig, ARRAY_COUNT(builtin_derivative_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_DFDX_COARSE, builtin_derivative_sig, ARRAY_COUNT(builtin_derivative_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_DFDY, builtin_derivative_sig, ARRAY_COUNT(builtin_derivative_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_DFDY_FINE, builtin_derivative_sig, ARRAY_COUNT(builtin_derivative_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
	{ BUILTIN_DFDY_COARSE, builtin_derivative_sig, ARRAY_COUNT(builtin_derivative_sig), BUILTIN_SIGNATURE_RESULT_SAME, 0, 0, NULL },
};

static const BuiltinSignature* builtin_find_signature(BuiltinFuncKind kind)
{
	for (int i = 0; i < ARRAY_COUNT(builtin_signatures); ++i)
	{
		const BuiltinSignature* signature = &builtin_signatures[i];
		if (signature->kind == kind)
			return signature;
	}
	return NULL;
}

static Type* builtin_apply_signature(const BuiltinSignature* signature, const Symbol* sym, Type** args, int argc)
{
	if (!signature)
		return NULL;
	builtin_check_args(sym, args, argc, signature->args, signature->arg_count);
	switch (signature->result_kind)
	{
	case BUILTIN_SIGNATURE_RESULT_NONE:
		return NULL;
	case BUILTIN_SIGNATURE_RESULT_SAME:
		return builtin_result_same(args, argc, signature->result_index);
	case BUILTIN_SIGNATURE_RESULT_SCALAR:
		return builtin_result_scalar(args, argc, signature->result_index);
	case BUILTIN_SIGNATURE_RESULT_VECTOR:
		return builtin_result_vector(args, argc, signature->result_index, signature->result_components);
	case BUILTIN_SIGNATURE_RESULT_CUSTOM:
		if (signature->custom_result)
			return signature->custom_result(sym, args, argc);
		return NULL;
	default:
		return NULL;
	}
}

// Validate a builtin invocation against per-argument rules before resolving it.
// ...error: distance requires second argument to match first argument shape, got vec3 and vec2
static int builtin_arg_in_base_set(const Type* type, BuiltinBaseSet base, const Type* ref)
{
	switch (base)
	{
	case BUILTIN_BASE_ANY:
		return 1;
	case BUILTIN_BASE_FLOATING:
		return type_is_floating_point(type);
	case BUILTIN_BASE_FLOAT_ONLY:
		return type && type_base_type(type) == T_FLOAT;
	case BUILTIN_BASE_NUMERIC:
		return type_is_numeric(type);
	case BUILTIN_BASE_BOOL:
		return type_is_bool_like(type);
	case BUILTIN_BASE_SAME_AS_REF:
		return ref && type_base_type(type) == type_base_type(ref);
	case BUILTIN_BASE_FLOAT_OR_BOOL:
		return type_is_floating_point(type) || type_is_bool_like(type);
	default:
		return 0;
	}
}

static int builtin_shape_matches(const Type* type, BuiltinShapeRule rule, const Type* ref)
{
	switch (rule)
	{
	case BUILTIN_SHAPE_ANY:
		return 1;
	case BUILTIN_SHAPE_SCALAR:
		return type_is_scalar(type);
	case BUILTIN_SHAPE_VECTOR:
		return type_is_vector(type);
	case BUILTIN_SHAPE_SCALAR_OR_VECTOR:
		return type_is_scalar_or_vector(type);
	case BUILTIN_SHAPE_MATRIX:
		return type_is_matrix(type);
	case BUILTIN_SHAPE_SQUARE_MATRIX:
		return type_is_matrix(type) && type->cols == type->rows;
	case BUILTIN_SHAPE_MATCH_REF:
		if (!ref)
			return 0;
		if (type_is_vector(ref))
			return type_is_vector(type) && type->cols == ref->cols;
		if (type_is_scalar(ref))
			return type_is_scalar(type);
		if (type_is_matrix(ref))
			return type_is_matrix(type) && type->cols == ref->cols && type->rows == ref->rows;
		return 0;
	case BUILTIN_SHAPE_SCALAR_OR_MATCH:
		if (type_is_scalar(type))
			return 1;
		if (!ref)
			return 0;
		if (type_is_vector(ref))
			return type_is_vector(type) && type->cols == ref->cols;
		if (type_is_scalar(ref))
			return type_is_scalar(type);
		if (type_is_matrix(ref))
			return type_is_matrix(type) && type->cols == ref->cols && type->rows == ref->rows;
		return 0;
	default:
		return 0;
	}
}

static void builtin_require_base(const Symbol* sym, const Type* arg, const BuiltinArgSig* sig, const Type* ref, const char* ref_role)
{
	if (builtin_arg_in_base_set(arg, sig->base, ref))
		return;
	const char* name = builtin_func_name(sym);
	switch (sig->base)
	{
	case BUILTIN_BASE_FLOATING:
		type_check_error("%s requires floating-point %s argument, got %s", name, sig->role, type_display(arg));
		break;
	case BUILTIN_BASE_FLOAT_ONLY:
		type_check_error("%s requires %s argument to be float-based, got %s", name, sig->role, type_display(arg));
		break;
	case BUILTIN_BASE_NUMERIC:
		type_check_error("%s requires numeric %s argument, got %s", name, sig->role, type_display(arg));
		break;
	case BUILTIN_BASE_BOOL:
		type_check_error("%s requires boolean %s argument, got %s", name, sig->role, type_display(arg));
		break;
	case BUILTIN_BASE_SAME_AS_REF:
		if (ref)
		{
			type_check_error("%s requires %s argument to match %s argument base type, got %s and %s", name, sig->role, ref_role, type_display(arg), type_display(ref));
		}
		else
		{
			type_check_error("%s requires %s argument to match %s argument base type", name, sig->role, ref_role);
		}
		break;
	case BUILTIN_BASE_FLOAT_OR_BOOL:
		type_check_error("%s requires floating-point or boolean %s argument, got %s", name, sig->role, type_display(arg));
		break;
	default:
		break;
	}
}

static void builtin_require_shape(const Symbol* sym, const Type* arg, const BuiltinArgSig* sig, const Type* ref, const char* ref_role)
{
	if (builtin_shape_matches(arg, sig->shape, ref))
		return;
	const char* name = builtin_func_name(sym);
	switch (sig->shape)
	{
	case BUILTIN_SHAPE_SCALAR:
		type_check_error("%s requires scalar %s argument, got %s", name, sig->role, type_display(arg));
		break;
	case BUILTIN_SHAPE_VECTOR:
		type_check_error("%s requires vector %s argument, got %s", name, sig->role, type_display(arg));
		break;
	case BUILTIN_SHAPE_SCALAR_OR_VECTOR:
		type_check_error("%s requires scalar or vector %s argument, got %s", name, sig->role, type_display(arg));
		break;
	case BUILTIN_SHAPE_MATRIX:
		type_check_error("%s requires matrix %s argument, got %s", name, sig->role, type_display(arg));
		break;
	case BUILTIN_SHAPE_SQUARE_MATRIX:
		if (arg && type_is_matrix(arg))
		{
			type_check_error("%s requires square matrix %s argument, got %s", name, sig->role, type_display(arg));
		}
		else
		{
			type_check_error("%s requires matrix %s argument, got %s", name, sig->role, type_display(arg));
		}
		break;
	case BUILTIN_SHAPE_MATCH_REF:
		if (ref && type_is_vector(ref))
		{
			type_check_error("%s requires %s argument to match %s argument shape, got %s and %s", name, sig->role, ref_role, type_display(arg), type_display(ref));
		}
		else if (ref && type_is_scalar(ref))
		{
			type_check_error("%s requires %s argument to match %s argument shape, got %s and %s", name, sig->role, ref_role, type_display(arg), type_display(ref));
		}
		else if (ref && type_is_matrix(ref))
		{
			type_check_error("%s requires %s argument to match %s argument shape, got %s and %s", name, sig->role, ref_role, type_display(arg), type_display(ref));
		}
		else
		{
			type_check_error("%s requires %s argument to match %s argument shape", name, sig->role, ref_role);
		}
		break;
	case BUILTIN_SHAPE_SCALAR_OR_MATCH:
		if (ref && type_is_vector(ref))
		{
			type_check_error("%s requires %s argument scalar or %d components, got %s", name, sig->role, ref->cols, type_display(arg));
		}
		else if (ref && type_is_matrix(ref))
		{
			type_check_error("%s requires %s argument to match %s argument shape, got %s and %s", name, sig->role, ref_role, type_display(arg), type_display(ref));
		}
		else
		{
			type_check_error("%s requires scalar %s argument, got %s", name, sig->role, type_display(arg));
		}
		break;
	default:
		break;
	}
}

static int builtin_check_args(const Symbol* sym, Type** args, int argc, const BuiltinArgSig* sigs, int sig_count)
{
	int all_ok = 1;
	const char* name = builtin_func_name(sym);
	if (argc != sig_count)
	{
		type_check_error("%s expects %d arguments but received %d", name, sig_count, argc);
	}
	for (int i = 0; i < sig_count; ++i)
	{
		const BuiltinArgSig* sig = &sigs[i];
		Type* arg = (i < argc) ? args[i] : NULL;
		if (!arg)
		{
			type_check_error("%s requires %s argument", name, sig->role);
			all_ok = 0;
			continue;
		}
		const Type* ref = NULL;
		const char* ref_role = "referenced";
		if (sig->ref_index >= 0 && sig->ref_index < argc)
		{
			ref = args[sig->ref_index];
		}
		if (sig->ref_index >= 0 && sig->ref_index < sig_count)
		{
			ref_role = sigs[sig->ref_index].role;
		}
		if (sig->base != BUILTIN_BASE_ANY)
		{
			builtin_require_base(sym, arg, sig, ref, ref_role);
		}
		if (sig->shape != BUILTIN_SHAPE_ANY)
		{
			builtin_require_shape(sym, arg, sig, ref, ref_role);
		}
	}
	return all_ok;
}

static Type* builtin_result_scalar(Type** args, int argc, int index)
{
	Type* source = builtin_result_same(args, argc, index);
	if (!source)
		return NULL;
	return type_get_scalar(type_base_type(source));
}

static Type* builtin_result_vector(Type** args, int argc, int index, int components)
{
	Type* source = builtin_result_same(args, argc, index);
	if (!source)
		return NULL;
	return type_get_vector(type_base_type(source), components);
}

// Enforce numeric scalar/vector arguments so min/max style builtins report clear errors.
// ...error: min requires numeric second argument, got mat2
static Type* builtin_result_min_max(const Symbol* sym, Type** args, int argc)
{
	const BuiltinArgSig sig[] = {
		{ "first", BUILTIN_BASE_NUMERIC, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
		{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
	};
	builtin_check_args(sym, args, argc, sig, ARRAY_COUNT(sig));
	return (argc > 0) ? args[0] : NULL;
}

// Validate clamp(x, min, max) and return the clamped value type.
// ...vec3 lit = clamp(raw, vec3(0.0), vec3(1.0));
static Type* builtin_result_clamp(const Symbol* sym, Type** args, int argc)
{
	const BuiltinArgSig sig[] = {
		{ "first", BUILTIN_BASE_NUMERIC, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
		{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
		{ "third", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
	};
	builtin_check_args(sym, args, argc, sig, ARRAY_COUNT(sig));
	return (argc > 0) ? args[0] : NULL;
}

// Resolve abs/similar functions that mirror their argument type.
// ...vec3 mag = abs(force);
static Type* builtin_result_abs_like(const Symbol* sym, Type** args, int argc)
{
	const BuiltinArgSig sig[] = {
		{ "first", BUILTIN_BASE_NUMERIC, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
	};
	builtin_check_args(sym, args, argc, sig, ARRAY_COUNT(sig));
	return (argc > 0) ? args[0] : NULL;
}

static Type* builtin_result_unary_float(const Symbol* sym, Type** args, int argc)
{
	const BuiltinArgSig sig[] = {
		{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
	};
	builtin_check_args(sym, args, argc, sig, ARRAY_COUNT(sig));
	return (argc > 0) ? args[0] : NULL;
}

static Type* builtin_result_pow(const Symbol* sym, Type** args, int argc)
{
	const BuiltinArgSig sig[] = {
		{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
		{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
	};
	builtin_check_args(sym, args, argc, sig, ARRAY_COUNT(sig));
	return (argc > 0) ? args[0] : NULL;
}

static Type* builtin_result_mod(const Symbol* sym, Type** args, int argc)
{
	const BuiltinArgSig sig[] = {
		{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
		{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_MATCH, 0 },
	};
	builtin_check_args(sym, args, argc, sig, ARRAY_COUNT(sig));
	return (argc > 0) ? args[0] : NULL;
}

static Type* builtin_result_trig(const Symbol* sym, Type** args, int argc)
{
	return builtin_result_unary_float(sym, args, argc);
}

static Type* builtin_result_atan(const Symbol* sym, Type** args, int argc)
{
	if (argc == 1)
		return builtin_result_unary_float(sym, args, argc);
	const BuiltinArgSig sig[] = {
		{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_VECTOR, -1 },
		{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_MATCH_REF, 0 },
	};
	builtin_check_args(sym, args, argc, sig, ARRAY_COUNT(sig));
	return (argc > 0) ? args[0] : NULL;
}

static Type* builtin_result_mix(const Symbol* sym, Type** args, int argc)
{
	if (argc < 3)
	{
		type_check_error("%s requires third argument", builtin_func_name(sym));
		return (argc > 0) ? args[0] : NULL;
	}
	Type* third = args[2];
	if (third && type_is_bool_like(third))
	{
		builtin_check_args(sym, args, argc, builtin_mix_mask_sig, ARRAY_COUNT(builtin_mix_mask_sig));
		return (argc > 0) ? args[0] : NULL;
	}
	builtin_check_args(sym, args, argc, builtin_mix_float_sig, ARRAY_COUNT(builtin_mix_float_sig));
	return (argc > 0) ? args[0] : NULL;
}

static Type* builtin_result_step(const Symbol* sym, Type** args, int argc)
{
	const BuiltinArgSig sig[] = {
		{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_MATCH, 1 },
		{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_VECTOR, 0 },
	};
	builtin_check_args(sym, args, argc, sig, ARRAY_COUNT(sig));
	if (argc > 1)
		return args[1];
	return (argc > 0) ? args[0] : NULL;
}

static Type* builtin_result_smoothstep(const Symbol* sym, Type** args, int argc)
{
	const BuiltinArgSig sig[] = {
		{ "first", BUILTIN_BASE_FLOATING, BUILTIN_SHAPE_SCALAR_OR_MATCH, 2 },
		{ "second", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_MATCH, 2 },
		{ "third", BUILTIN_BASE_SAME_AS_REF, BUILTIN_SHAPE_SCALAR_OR_VECTOR, 0 },
	};
	builtin_check_args(sym, args, argc, sig, ARRAY_COUNT(sig));
	if (argc > 2)
		return args[2];
	return (argc > 0) ? args[0] : NULL;
}

// Pick the return type for sampling helpers like texture().
// ...vec4 color = texture(u_image, v_uv);
static Type* builtin_result_texture(Type** args, int argc)
{
	Type* sampler = (args && argc > 0) ? args[0] : NULL;
	if (!sampler || sampler->tag != T_SAMPLER)
		return type_system_get(sintern("vec4"));
	TypeTag component = sampler->base ? (TypeTag)sampler->base : T_FLOAT;
	if (sampler->dim & TYPE_DIM_FLAG_SHADOW)
	{
		Type* scalar = type_get_scalar(component);
		return scalar ? scalar : type_system_get(sintern("float"));
	}
	Type* vec = type_get_vector(component, 4);
	if (vec)
		return vec;
	return type_system_get(sintern("vec4"));
}

static int sampler_base_dimension(const Type* sampler)
{
	if (!sampler || (sampler->tag != T_SAMPLER && sampler->tag != T_IMAGE))
		return TYPE_DIM_UNKNOWN;
	return sampler->dim & 0xF;
}

static int sampler_coord_components(const Type* sampler)
{
	int base_dim = sampler_base_dimension(sampler);
	int components = 0;
	switch (base_dim)
	{
	case TYPE_DIM_1D:
		components = 1;
		break;
	case TYPE_DIM_2D:
	case TYPE_DIM_RECT:
	case TYPE_DIM_CUBE:
	case TYPE_DIM_2D_MS:
		components = 2;
		break;
	case TYPE_DIM_3D:
		components = 3;
		break;
	case TYPE_DIM_BUFFER:
		components = 1;
		break;
	default:
		return 0;
	}
	if (sampler->dim & TYPE_DIM_FLAG_ARRAY)
		components += 1;
	return components;
}

// Match the textureSize() return shape to the sampler argument.
// ...ivec2 size = textureSize(u_image, 0);
static Type* builtin_result_texture_size(Type** args, int argc)
{
	Type* sampler = (args && argc > 0) ? args[0] : NULL;
	if (!sampler || sampler->tag != T_SAMPLER)
	{
		type_check_error("textureSize requires sampler argument, got %s", sampler ? type_display(sampler) : "<null>");
		return type_get_scalar(T_INT);
	}
	int base_dim = sampler_base_dimension(sampler);
	int expected_args = 1;
	switch (base_dim)
	{
	case TYPE_DIM_RECT:
	case TYPE_DIM_BUFFER:
	case TYPE_DIM_2D_MS:
		expected_args = 1;
		break;
	case TYPE_DIM_UNKNOWN:
		type_check_error("textureSize unsupported sampler type %s", type_display(sampler));
		break;
	default:
		expected_args = 2;
		break;
	}
	if (argc != expected_args)
	{
		type_check_error("textureSize expects %d arguments but received %d", expected_args, argc);
	}
	if (expected_args == 2)
	{
		Type* lod = (argc > 1) ? args[1] : NULL;
		if (!lod || !type_is_scalar(lod) || !type_is_integer(lod))
		{
			type_check_error("textureSize LOD must be integer scalar, got %s", lod ? type_display(lod) : "<null>");
		}
	}
	int components = sampler_coord_components(sampler);
	if (!components)
	{
		type_check_error("textureSize cannot determine dimensions for sampler %s", type_display(sampler));
		return type_get_scalar(T_INT);
	}
	if (components <= 1)
		return type_get_scalar(T_INT);
	Type* result = type_get_vector(T_INT, components);
	return result ? result : type_get_scalar(T_INT);
}

// Check texelFetch() coordinates, lod/sample, and sampler compatibility.
// ...ivec4 texel = texelFetch(u_image, ivec2(gl_FragCoord.xy), 0);
static Type* builtin_result_texel_fetch(Type** args, int argc, int has_offset)
{
	const char* func_name = has_offset ? "texelFetchOffset" : "texelFetch";
	Type* sampler = (args && argc > 0) ? args[0] : NULL;
	if (!sampler || sampler->tag != T_SAMPLER)
	{
		type_check_error("%s requires sampler argument, got %s", func_name, sampler ? type_display(sampler) : "<null>");
		return builtin_result_texture(args, argc);
	}
	if (sampler->dim & TYPE_DIM_FLAG_SHADOW)
	{
		type_check_error("%s does not support shadow samplers (%s)", func_name, type_display(sampler));
	}
	int base_dim = sampler_base_dimension(sampler);
	int coord_components = sampler_coord_components(sampler);
	int expected_args = 0;
	int level_index = -1;
	int sample_index = -1;
	switch (base_dim)
	{
	case TYPE_DIM_BUFFER:
		expected_args = 2;
		coord_components = 1;
		break;
	case TYPE_DIM_1D:
	case TYPE_DIM_2D:
	case TYPE_DIM_3D:
		expected_args = 3;
		level_index = 2;
		break;
	case TYPE_DIM_RECT:
		expected_args = 2;
		break;
	case TYPE_DIM_2D_MS:
		expected_args = 3;
		sample_index = 2;
		break;
	default:
		type_check_error("%s unsupported sampler type %s", func_name, type_display(sampler));
		return builtin_result_texture(args, argc);
	}
	if (has_offset)
	{
		expected_args += 1;
		if (base_dim == TYPE_DIM_BUFFER || base_dim == TYPE_DIM_2D_MS)
		{
			type_check_error("%s does not support sampler type %s", func_name, type_display(sampler));
		}
	}
	if (argc != expected_args)
	{
		type_check_error("%s expects %d arguments but received %d", func_name, expected_args, argc);
	}
	Type* coord = (argc > 1) ? args[1] : NULL;
	if (coord_components <= 1)
	{
		if (!coord || !type_is_scalar(coord) || !type_is_integer(coord))
		{
			type_check_error("%s coordinate must be integer scalar, got %s", func_name, coord ? type_display(coord) : "<null>");
		}
	}
	else
	{
		if (!coord || !type_is_vector(coord) || coord->cols != coord_components || !type_is_integer(coord))
		{
			type_check_error("%s coordinate must be ivec%d, got %s", func_name, coord_components, coord ? type_display(coord) : "<null>");
		}
	}
	if (level_index >= 0)
	{
		Type* level = args[level_index];
		if (!level || !type_is_scalar(level) || !type_is_integer(level))
		{
			type_check_error("%s LOD must be integer scalar, got %s", func_name, level ? type_display(level) : "<null>");
		}
	}
	else if (sample_index >= 0)
	{
		Type* sample = args[sample_index];
		if (!sample || !type_is_scalar(sample) || !type_is_integer(sample))
		{
			type_check_error("%s sample index must be integer scalar, got %s", func_name, sample ? type_display(sample) : "<null>");
		}
	}
	if (has_offset)
	{
		Type* offset = args[expected_args - 1];
		if (coord_components <= 1)
		{
			if (!offset || !type_is_scalar(offset) || !type_is_integer(offset))
			{
				type_check_error("%s offset must be integer scalar, got %s", func_name, offset ? type_display(offset) : "<null>");
			}
		}
		else
		{
			if (!offset || !type_is_vector(offset) || offset->cols != coord_components || !type_is_integer(offset))
			{
				type_check_error("%s offset must be ivec%d, got %s", func_name, coord_components, offset ? type_display(offset) : "<null>");
			}
		}
	}
	return builtin_result_texture(args, argc);
}

// Keep inverse() limited to square matrix inputs.
// ...mat4 inv_model = inverse(model);
static Type* builtin_result_inverse(Type** args, int argc)
{
	Type* mat = (args && argc > 0) ? args[0] : NULL;
	if (!mat || !type_is_matrix(mat))
	{
		type_check_error("inverse requires matrix argument, got %s", mat ? type_display(mat) : "<null>");
		return mat;
	}
	if (mat->cols != mat->rows)
	{
		type_check_error("inverse requires square matrix, got %s", type_display(mat));
	}
	return mat;
}

// Flip the rows and columns for transpose() return types.
// ...mat3 normal_mat = transpose(inverse(mat3(model)));
static Type* builtin_result_transpose(Type** args, int argc)
{
	Type* mat = (args && argc > 0) ? args[0] : NULL;
	if (!mat || !type_is_matrix(mat))
	{
		type_check_error("transpose requires matrix argument, got %s", mat ? type_display(mat) : "<null>");
		return mat;
	}
	Type* result = type_get_matrix(type_base_type(mat), mat->rows, mat->cols);
	return result ? result : mat;
}

static Type* builtin_result_determinant(const Symbol* sym, Type** args, int argc)
{
	Type* mat = (args && argc > 0) ? args[0] : NULL;
	const char* name = builtin_func_name(sym);
	if (!mat || !type_is_matrix(mat))
	{
		type_check_error("%s requires matrix argument, got %s", name, mat ? type_display(mat) : "<null>");
		return type_get_scalar(T_FLOAT);
	}
	if (mat->cols != mat->rows)
	{
		type_check_error("%s requires square matrix, got %s", name, type_display(mat));
	}
	TypeTag base = type_base_type(mat);
	if (base != T_FLOAT && base != T_DOUBLE)
	{
		type_check_error("%s requires floating-point matrix, got %s", name, type_display(mat));
		base = T_FLOAT;
	}
	return type_get_scalar(base);
}

static Type* builtin_result_outer_product(const Symbol* sym, Type** args, int argc)
{
	Type* lhs = (args && argc > 0) ? args[0] : NULL;
	Type* rhs = (args && argc > 1) ? args[1] : NULL;
	const char* name = builtin_func_name(sym);
	if (!lhs || !rhs || !type_is_vector(lhs) || !type_is_vector(rhs))
	{
		type_check_error("%s requires vector arguments, got %s and %s", name, lhs ? type_display(lhs) : "<null>", rhs ? type_display(rhs) : "<null>");
		return type_get_matrix(T_FLOAT, 2, 2);
	}
	TypeTag lhs_base = type_base_type(lhs);
	TypeTag rhs_base = type_base_type(rhs);
	if (lhs_base != rhs_base)
	{
		type_check_error("%s requires matching base types, got %s and %s", name, type_display(lhs), type_display(rhs));
	}
	if ((lhs_base != T_FLOAT && lhs_base != T_DOUBLE) || (rhs_base != T_FLOAT && rhs_base != T_DOUBLE))
	{
		type_check_error("%s requires floating-point vectors, got %s and %s", name, type_display(lhs), type_display(rhs));
		lhs_base = T_FLOAT;
	}
	int cols = lhs->cols;
	int rows = rhs->cols;
	Type* result = type_get_matrix(lhs_base, cols, rows);
	if (!result)
		result = type_get_matrix(T_FLOAT, cols, rows);
	return result;
}

// Enforce matching shapes for relational helpers like lessThan().
// ...bvec3 mask = lessThan(a.xyz, b.xyz);
static Type* builtin_result_relational(Type** args, int argc, int allow_bool)
{
	Type* lhs = (args && argc > 0) ? args[0] : NULL;
	Type* rhs = (args && argc > 1) ? args[1] : NULL;
	if (!lhs || !rhs)
		return type_get_scalar(T_BOOL);
	if ((!type_is_scalar(lhs) && !type_is_vector(lhs)) || (!type_is_scalar(rhs) && !type_is_vector(rhs)))
	{
		type_check_error("relational functions require scalar or vector arguments, got %s and %s", type_display(lhs), type_display(rhs));
		return type_get_scalar(T_BOOL);
	}
	if ((type_is_vector(lhs) && !type_is_vector(rhs)) || (type_is_scalar(lhs) && !type_is_scalar(rhs)))
	{
		type_check_error("relational functions require matching argument shapes, got %s and %s", type_display(lhs), type_display(rhs));
	}
	if (type_is_vector(lhs) && type_is_vector(rhs) && lhs->cols != rhs->cols)
	{
		type_check_error("relational functions require matching vector sizes, got %s and %s", type_display(lhs), type_display(rhs));
	}
	TypeTag lhs_base = type_base_type(lhs);
	TypeTag rhs_base = type_base_type(rhs);
	if (lhs_base != rhs_base)
	{
		type_check_error("relational functions require matching base types, got %s and %s", type_display(lhs), type_display(rhs));
	}
	if (!allow_bool && lhs_base == T_BOOL)
	{
		type_check_error("ordering relational functions do not support boolean arguments");
	}
	int components = type_is_vector(lhs) ? lhs->cols : 1;
	if (components <= 1)
		return type_get_scalar(T_BOOL);
	return type_get_vector(T_BOOL, components);
}

static Type* builtin_result_matrix_comp_mult(Type** args, int argc)
{
	Type* lhs = (args && argc > 0) ? args[0] : NULL;
	Type* rhs = (args && argc > 1) ? args[1] : NULL;
	if (!lhs || !rhs || !type_is_matrix(lhs) || !type_is_matrix(rhs))
	{
		type_check_error("matrixCompMult requires matrix arguments, got %s and %s", lhs ? type_display(lhs) : "<null>", rhs ? type_display(rhs) : "<null>");
		return lhs ? lhs : rhs;
	}
	if (lhs->cols != rhs->cols || lhs->rows != rhs->rows)
	{
		type_check_error("matrixCompMult requires matching matrix dimensions, got %s and %s", type_display(lhs), type_display(rhs));
	}
	TypeTag lhs_base = type_base_type(lhs);
	TypeTag rhs_base = type_base_type(rhs);
	if (lhs_base != rhs_base)
	{
		type_check_error("matrixCompMult requires matching base types, got %s and %s", type_display(lhs), type_display(rhs));
	}
	if (lhs_base != T_FLOAT && lhs_base != T_DOUBLE)
	{
		type_check_error("matrixCompMult requires floating-point matrices, got %s", type_display(lhs));
	}
	return lhs;
}

static Type* builtin_result_texture_query_lod(Type** args, int argc)
{
	Type* sampler = (args && argc > 0) ? args[0] : NULL;
	if (!sampler || sampler->tag != T_SAMPLER)
	{
		type_check_error("textureQueryLod requires sampler argument, got %s", sampler ? type_display(sampler) : "<null>");
		return type_get_vector(T_FLOAT, 2);
	}
	if (sampler->dim & TYPE_DIM_FLAG_SHADOW)
	{
		type_check_error("textureQueryLod does not support shadow samplers (%s)", type_display(sampler));
	}
	return type_get_vector(T_FLOAT, 2);
}

static Type* builtin_result_texture_query_levels(Type** args, int argc)
{
	Type* sampler = (args && argc > 0) ? args[0] : NULL;
	if (!sampler || sampler->tag != T_SAMPLER)
	{
		type_check_error("textureQueryLevels requires sampler argument, got %s", sampler ? type_display(sampler) : "<null>");
	}
	return type_get_scalar(T_INT);
}

static const char* image_atomic_name(BuiltinFuncKind kind)
{
	switch (kind)
	{
	case BUILTIN_IMAGE_ATOMIC_ADD:
		return "imageAtomicAdd";
	case BUILTIN_IMAGE_ATOMIC_MIN:
		return "imageAtomicMin";
	case BUILTIN_IMAGE_ATOMIC_MAX:
		return "imageAtomicMax";
	case BUILTIN_IMAGE_ATOMIC_AND:
		return "imageAtomicAnd";
	case BUILTIN_IMAGE_ATOMIC_OR:
		return "imageAtomicOr";
	case BUILTIN_IMAGE_ATOMIC_XOR:
		return "imageAtomicXor";
	case BUILTIN_IMAGE_ATOMIC_EXCHANGE:
		return "imageAtomicExchange";
	case BUILTIN_IMAGE_ATOMIC_COMP_SWAP:
		return "imageAtomicCompSwap";
	default:
		return "imageAtomic";
	}
}

static Type* builtin_result_image_atomic(BuiltinFuncKind kind, Type** args, int argc)
{
	const char* func_name = image_atomic_name(kind);
	Type* image = (args && argc > 0) ? args[0] : NULL;
	if (!image || image->tag != T_IMAGE)
	{
		type_check_error("%s requires image argument, got %s", func_name, image ? type_display(image) : "<null>");
		return type_get_scalar(T_INT);
	}
	TypeTag base = (TypeTag)image->base;
	if (base != T_INT && base != T_UINT)
	{
		type_check_error("%s requires integer image types, got %s", func_name, type_display(image));
		base = T_INT;
	}
	int coord_components = sampler_coord_components(image);
	if (!coord_components)
	{
		type_check_error("%s cannot determine coordinate size for %s", func_name, type_display(image));
		coord_components = 1;
	}
	int base_dim = sampler_base_dimension(image);
	int is_ms = (base_dim == TYPE_DIM_2D_MS);
	int value_params = (kind == BUILTIN_IMAGE_ATOMIC_COMP_SWAP) ? 2 : 1;
	int expected_args = 2 + value_params + (is_ms ? 1 : 0);
	if (argc != expected_args)
	{
		type_check_error("%s expects %d arguments but received %d", func_name, expected_args, argc);
	}
	Type* coord = (argc > 1) ? args[1] : NULL;
	if (coord_components <= 1)
	{
		if (!coord || !type_is_scalar(coord) || !type_is_integer(coord))
		{
			type_check_error("%s coordinate must be integer scalar, got %s", func_name, coord ? type_display(coord) : "<null>");
		}
	}
	else
	{
		if (!coord || !type_is_vector(coord) || coord->cols != coord_components || !type_is_integer(coord))
		{
			type_check_error("%s coordinate must be ivec%d, got %s", func_name, coord_components, coord ? type_display(coord) : "<null>");
		}
	}
	int index = 2;
	if (is_ms)
	{
		Type* sample = (argc > index) ? args[index] : NULL;
		if (!sample || !type_is_scalar(sample) || !type_is_integer(sample))
		{
			type_check_error("%s sample index must be integer scalar, got %s", func_name, sample ? type_display(sample) : "<null>");
		}
		index += 1;
	}
	Type* compare = NULL;
	if (kind == BUILTIN_IMAGE_ATOMIC_COMP_SWAP)
	{
		compare = (argc > index) ? args[index] : NULL;
		if (!compare || !type_is_scalar(compare) || type_base_type(compare) != base)
		{
			const char* expected = (base == T_UINT) ? "uint" : "int";
			type_check_error("%s compare value must be %s scalar, got %s", func_name, expected, compare ? type_display(compare) : "<null>");
		}
		index += 1;
	}
	Type* value = (argc > index) ? args[index] : NULL;
	if (!value || !type_is_scalar(value) || type_base_type(value) != base)
	{
		const char* expected = (base == T_UINT) ? "uint" : "int";
		type_check_error("%s value must be %s scalar, got %s", func_name, expected, value ? type_display(value) : "<null>");
	}
	return type_get_scalar(base);
}

// Collapse boolean vectors for any()/all() reductions.
// ...bool is_visible = all(greaterThanEqual(alpha.rgb, vec3(0.0)));
static Type* builtin_result_any_all(Type** args, int argc)
{
	Type* arg = (args && argc > 0) ? args[0] : NULL;
	if (!arg)
		return type_get_scalar(T_BOOL);
	if (!type_is_vector(arg) || type_base_type(arg) != T_BOOL)
	{
		type_check_error("any/all require boolean vector argument, got %s", type_display(arg));
	}
	return type_get_scalar(T_BOOL);
}

// Route builtin calls to the helper that checks their arguments.
// ...float lighting = dot(normal, light_dir);
Type* type_infer_builtin_call(const Symbol* sym, Type** args, int argc)
{
	if (!sym)
		return NULL;
	const BuiltinSignature* signature = builtin_find_signature(sym->builtin_kind);
	if (signature)
		return builtin_apply_signature(signature, sym, args, argc);
	switch (sym->builtin_kind)
	{
	case BUILTIN_TEXTURE:
	case BUILTIN_TEXTURE_LOD:
	case BUILTIN_TEXTURE_PROJ:
	case BUILTIN_TEXTURE_GRAD:
	case BUILTIN_TEXTURE_OFFSET:
	case BUILTIN_TEXTURE_LOD_OFFSET:
	case BUILTIN_TEXTURE_PROJ_OFFSET:
	case BUILTIN_TEXTURE_PROJ_LOD:
	case BUILTIN_TEXTURE_PROJ_LOD_OFFSET:
	case BUILTIN_TEXTURE_GRAD_OFFSET:
	case BUILTIN_TEXTURE_PROJ_GRAD:
	case BUILTIN_TEXTURE_PROJ_GRAD_OFFSET:
	case BUILTIN_TEXTURE_GATHER:
	case BUILTIN_TEXTURE_GATHER_OFFSET:
	case BUILTIN_TEXTURE_GATHER_OFFSETS:
		return builtin_result_texture(args, argc);
	case BUILTIN_TEXTURE_SIZE:
		return builtin_result_texture_size(args, argc);
	case BUILTIN_TEXEL_FETCH:
		return builtin_result_texel_fetch(args, argc, 0);
	case BUILTIN_TEXEL_FETCH_OFFSET:
		return builtin_result_texel_fetch(args, argc, 1);
	case BUILTIN_TEXTURE_QUERY_LOD:
		return builtin_result_texture_query_lod(args, argc);
	case BUILTIN_TEXTURE_QUERY_LEVELS:
		return builtin_result_texture_query_levels(args, argc);
	case BUILTIN_MATRIX_COMP_MULT:
		return builtin_result_matrix_comp_mult(args, argc);
	case BUILTIN_MIN:
	case BUILTIN_MAX:
		return builtin_result_min_max(sym, args, argc);
	case BUILTIN_CLAMP:
		return builtin_result_clamp(sym, args, argc);
	case BUILTIN_ABS:
	case BUILTIN_SIGN:
		return builtin_result_abs_like(sym, args, argc);
	case BUILTIN_FLOOR:
	case BUILTIN_CEIL:
	case BUILTIN_FRACT:
	case BUILTIN_TRUNC:
	case BUILTIN_ROUND:
	case BUILTIN_ROUND_EVEN:
	case BUILTIN_EXP:
	case BUILTIN_EXP2:
	case BUILTIN_LOG:
	case BUILTIN_LOG2:
	case BUILTIN_SQRT:
	case BUILTIN_INVERSE_SQRT:
		return builtin_result_unary_float(sym, args, argc);
	case BUILTIN_POW:
		return builtin_result_pow(sym, args, argc);
	case BUILTIN_MOD:
		return builtin_result_mod(sym, args, argc);
	case BUILTIN_SIN:
	case BUILTIN_COS:
	case BUILTIN_TAN:
	case BUILTIN_ASIN:
	case BUILTIN_ACOS:
		return builtin_result_trig(sym, args, argc);
	case BUILTIN_ATAN:
		return builtin_result_atan(sym, args, argc);
	case BUILTIN_INVERSE:
		return builtin_result_inverse(args, argc);
	case BUILTIN_TRANSPOSE:
		return builtin_result_transpose(args, argc);
	case BUILTIN_STEP:
		return builtin_result_step(sym, args, argc);
	case BUILTIN_SMOOTHSTEP:
		return builtin_result_smoothstep(sym, args, argc);
	case BUILTIN_CROSS:
		return builtin_result_vector(args, argc, 0, 3);
	case BUILTIN_IMAGE_ATOMIC_ADD:
	case BUILTIN_IMAGE_ATOMIC_MIN:
	case BUILTIN_IMAGE_ATOMIC_MAX:
	case BUILTIN_IMAGE_ATOMIC_AND:
	case BUILTIN_IMAGE_ATOMIC_OR:
	case BUILTIN_IMAGE_ATOMIC_XOR:
	case BUILTIN_IMAGE_ATOMIC_EXCHANGE:
	case BUILTIN_IMAGE_ATOMIC_COMP_SWAP:
		return builtin_result_image_atomic(sym->builtin_kind, args, argc);
	case BUILTIN_LESS_THAN:
	case BUILTIN_LESS_THAN_EQUAL:
	case BUILTIN_GREATER_THAN:
	case BUILTIN_GREATER_THAN_EQUAL:
		return builtin_result_relational(args, argc, 0);
	case BUILTIN_EQUAL:
	case BUILTIN_NOT_EQUAL:
		return builtin_result_relational(args, argc, 1);
	case BUILTIN_ANY:
	case BUILTIN_ALL:
		return builtin_result_any_all(args, argc);
	default:
		break;
	}
	return NULL;
}
int type_can_assign(const Type* dst, const Type* src)
{
	if (!dst || !src)
		return 0;
	if (type_equal(dst, src))
		return 1;
	if (type_is_scalar(dst) && type_is_scalar(src))
	{
		return type_scalar_can_convert(src, dst);
	}
	if (type_is_vector(dst) && type_is_vector(src))
	{
		if (dst->cols != src->cols)
			return 0;
		if (type_base_type(dst) != type_base_type(src))
			return 0;
		return 1;
	}
	if (type_is_matrix(dst) && type_is_matrix(src))
	{
		if (dst->cols != src->cols || dst->rows != src->rows)
			return 0;
		if (type_base_type(dst) != type_base_type(src))
			return 0;
		return 1;
	}
	return 0;
}

int type_component_count(const Type* type)
{
	if (!type)
		return 0;
	if (type_is_scalar(type))
		return 1;
	if (type_is_vector(type))
		return type->cols;
	if (type_is_matrix(type))
		return type->cols * type->rows;
	return 0;
}

Type* type_bool_type(int components)
{
	if (components <= 1)
		return type_get_scalar(T_BOOL);
	return type_get_vector(T_BOOL, components);
}

static Type* type_promote_scalar_to_components(Type* type, int components)
{
	if (!type || components <= 1)
		return type;
	if (!type_is_scalar(type))
		return type;
	TypeTag base = type_base_type(type);
	Type* vector = type_get_vector(base, components);
	return vector ? vector : type;
}

static int type_align_component_counts(Type** a, Type** b)
{
	if (!a || !b || !*a || !*b)
		return 0;
	Type* lhs = *a;
	Type* rhs = *b;
	if (type_is_matrix(lhs) || type_is_matrix(rhs))
	{
		if (type_is_matrix(lhs) && type_is_matrix(rhs) && lhs->cols == rhs->cols && lhs->rows == rhs->rows)
			return 1;
		return 0;
	}
	if (type_is_vector(lhs) && type_is_vector(rhs))
		return lhs->cols == rhs->cols;
	if (type_is_vector(lhs) && type_is_scalar(rhs))
	{
		Type* promoted = type_promote_scalar_to_components(rhs, lhs->cols);
		if (!promoted || !type_is_vector(promoted) || promoted->cols != lhs->cols)
			return 0;
		*b = promoted;
		return 1;
	}
	if (type_is_scalar(lhs) && type_is_vector(rhs))
	{
		Type* promoted = type_promote_scalar_to_components(lhs, rhs->cols);
		if (!promoted || !type_is_vector(promoted) || promoted->cols != rhs->cols)
			return 0;
		*a = promoted;
		return 1;
	}
	if (type_is_scalar(lhs) && type_is_scalar(rhs))
		return 1;
	return 0;
}

static int type_numeric_rank(TypeTag tag)
{
	switch (tag)
	{
	case T_DOUBLE:
		return 6;
	case T_FLOAT:
		return 5;
	case T_UINT64:
		return 4;
	case T_INT64:
		return 3;
	case T_UINT:
		return 2;
	case T_INT:
		return 1;
	default:
		return 0;
	}
}

static TypeTag type_shared_numeric_base(TypeTag a, TypeTag b)
{
	if (!type_base_is_numeric(a) || !type_base_is_numeric(b))
		return T_VOID;
	int rank_a = type_numeric_rank(a);
	int rank_b = type_numeric_rank(b);
	TypeTag preferred = rank_a >= rank_b ? a : b;
	if (type_base_can_convert(a, preferred) && type_base_can_convert(b, preferred))
		return preferred;
	preferred = rank_a < rank_b ? a : b;
	if (type_base_can_convert(a, preferred) && type_base_can_convert(b, preferred))
		return preferred;
	const TypeTag fallback[] = { T_DOUBLE, T_FLOAT, T_UINT64, T_INT64, T_UINT, T_INT };
	for (int i = 0; i < (int)(sizeof(fallback) / sizeof(fallback[0])); ++i)
	{
		TypeTag candidate = fallback[i];
		if (type_base_can_convert(a, candidate) && type_base_can_convert(b, candidate))
			return candidate;
	}
	return T_VOID;
}
void type_check_error(const char* fmt, ...);

Type* type_check_unary(const IR_Cmd* inst, Type* operand)
{
	if (!inst)
		return operand;
	Tok tok = inst->tok;
	switch (tok)
	{
	case TOK_PLUS_PLUS:
	case TOK_MINUS_MINUS:
	{
		if (!operand)
			return NULL;
		int idx = inst->arg0;
		if (idx < 0 || idx >= acount(g_ir) || !g_ir[idx].is_lvalue)
		{
			type_check_error("operator %s requires l-value operand", tok_name[tok]);
		}
		if (!type_is_scalar(operand) && !type_is_vector(operand))
		{
			type_check_error("operator %s requires scalar or vector operand, got %s", tok_name[tok], type_display(operand));
		}
		TypeTag base = type_base_type(operand);
		if (!type_base_is_numeric(base))
		{
			type_check_error("operator %s requires integer or float operand, got %s", tok_name[tok], type_display(operand));
		}
		return operand;
	}
	case TOK_MINUS:
	case TOK_PLUS:
		if (!type_is_numeric(operand) && !type_is_matrix(operand))
		{
			type_check_error("operator %s requires numeric operand, got %s", tok_name[tok], type_display(operand));
		}
		return operand;
	case TOK_NOT:
		if (!type_is_bool_like(operand))
		{
			type_check_error("operator ! requires boolean operand, got %s", type_display(operand));
		}
		if (type_is_vector(operand))
		{
			return type_get_vector(T_BOOL, operand->cols);
		}
		return type_get_scalar(T_BOOL);
	case TOK_TILDE:
		if (!type_is_integer(operand))
		{
			type_check_error("operator %s requires integer operand, got %s", tok_name[tok], type_display(operand));
		}
		return operand;
	default:
		type_check_error("unsupported unary operator %s", tok_name[tok]);
	}
	return operand;
}

Type* type_binary_add_sub(Tok tok, Type* lhs, Type* rhs)
{
	if (!lhs || !rhs)
		return lhs ? lhs : rhs;
	if (!type_is_numeric(lhs) || !type_is_numeric(rhs))
	{
		type_check_error("operator %s requires numeric operands, got %s and %s", tok_name[tok], type_display(lhs), type_display(rhs));
	}
	if (type_base_type(lhs) != type_base_type(rhs))
	{
		type_check_error("operator %s requires matching base types, got %s and %s", tok_name[tok], type_display(lhs), type_display(rhs));
	}
	if (type_is_scalar(lhs) && type_is_scalar(rhs))
		return lhs;
	if (type_is_scalar(lhs) && type_is_vector(rhs))
		return rhs;
	if (type_is_vector(lhs) && type_is_scalar(rhs))
		return lhs;
	if (type_is_vector(lhs) && type_is_vector(rhs))
	{
		if (lhs->cols != rhs->cols)
		{
			type_check_error("operator %s requires matching vector sizes, got %d and %d", tok_name[tok], lhs->cols, rhs->cols);
		}
		return lhs;
	}
	if (type_is_matrix(lhs) && type_is_matrix(rhs))
	{
		if (lhs->cols != rhs->cols || lhs->rows != rhs->rows)
		{
			type_check_error("operator %s requires matching matrix sizes", tok_name[tok]);
		}
		return lhs;
	}
	if (type_is_matrix(lhs) && type_is_scalar(rhs))
		return lhs;
	if (type_is_scalar(lhs) && type_is_matrix(rhs))
		return rhs;
	type_check_error("operator %s unsupported for %s and %s", tok_name[tok], type_display(lhs), type_display(rhs));
	return NULL;
}

Type* type_binary_mul(Type* lhs, Type* rhs)
{
	if (!lhs || !rhs)
		return lhs ? lhs : rhs;
	if (!type_is_numeric(lhs) || !type_is_numeric(rhs))
	{
		type_check_error("operator * requires numeric operands, got %s and %s", type_display(lhs), type_display(rhs));
	}
	if (type_base_type(lhs) != type_base_type(rhs))
	{
		type_check_error("operator * requires matching base types, got %s and %s", type_display(lhs), type_display(rhs));
	}
	if (type_is_scalar(lhs) && type_is_scalar(rhs))
		return lhs;
	if (type_is_scalar(lhs) && (type_is_vector(rhs) || type_is_matrix(rhs)))
		return rhs;
	if (type_is_scalar(rhs) && (type_is_vector(lhs) || type_is_matrix(lhs)))
		return lhs;
	if (type_is_vector(lhs) && type_is_vector(rhs))
	{
		if (lhs->cols != rhs->cols)
		{
			type_check_error("operator * requires matching vector sizes, got %d and %d", lhs->cols, rhs->cols);
		}
		return lhs;
	}
	if (type_is_matrix(lhs) && type_is_vector(rhs))
	{
		if (lhs->cols != rhs->cols)
		{
			type_check_error("matrix-vector multiplication requires matrix columns to match vector size, got %d and %d", lhs->cols, rhs->cols);
		}
		Type* result = type_get_vector(type_base_type(lhs), lhs->rows);
		if (!result)
		{
			type_check_error("unsupported matrix-vector multiplication result size %dx1", lhs->rows);
		}
		return result;
	}
	if (type_is_vector(lhs) && type_is_matrix(rhs))
	{
		if (lhs->cols != rhs->rows)
		{
			type_check_error("vector-matrix multiplication requires vector size to match matrix rows, got %d and %d", lhs->cols, rhs->rows);
		}
		Type* result = type_get_vector(type_base_type(lhs), rhs->cols);
		if (!result)
		{
			type_check_error("unsupported vector-matrix multiplication result size %dx1", rhs->cols);
		}
		return result;
	}
	if (type_is_matrix(lhs) && type_is_matrix(rhs))
	{
		if (lhs->cols != rhs->rows)
		{
			type_check_error("matrix multiplication requires the left matrix columns to match the right matrix rows, got %dx%d and %dx%d", lhs->cols, lhs->rows, rhs->cols, rhs->rows);
		}
		Type* result = type_get_matrix(type_base_type(lhs), rhs->cols, lhs->rows);
		if (!result)
		{
			type_check_error("unsupported matrix multiplication result size %dx%d", rhs->cols, lhs->rows);
		}
		return result;
	}
	type_check_error("operator * unsupported for %s and %s", type_display(lhs), type_display(rhs));
	return NULL;
}

Type* type_binary_div(Type* lhs, Type* rhs)
{
	if (!lhs || !rhs)
		return lhs ? lhs : rhs;
	if (!type_is_numeric(lhs) || !type_is_numeric(rhs))
	{
		type_check_error("operator / requires numeric operands, got %s and %s", type_display(lhs), type_display(rhs));
	}
	if (type_base_type(lhs) != type_base_type(rhs))
	{
		type_check_error("operator / requires matching base types, got %s and %s", type_display(lhs), type_display(rhs));
	}
	if (type_is_scalar(lhs) && type_is_scalar(rhs))
		return lhs;
	if (type_is_vector(lhs) && type_is_scalar(rhs))
		return lhs;
	if (type_is_scalar(lhs) && type_is_vector(rhs))
		return rhs;
	if (type_is_matrix(lhs) && type_is_scalar(rhs))
		return lhs;
	if (type_is_scalar(lhs) && type_is_matrix(rhs))
		return rhs;
	if (type_is_vector(lhs) && type_is_vector(rhs))
	{
		if (lhs->cols != rhs->cols)
		{
			type_check_error("operator / requires matching vector sizes, got %d and %d", lhs->cols, rhs->cols);
		}
		return lhs;
	}
	type_check_error("operator / unsupported for %s and %s", type_display(lhs), type_display(rhs));
	return NULL;
}

Type* type_binary_mod(Type* lhs, Type* rhs)
{
	if (!lhs || !rhs)
		return lhs ? lhs : rhs;
	if (!type_is_integer(lhs) || !type_is_integer(rhs))
	{
		type_check_error("operator % requires integer operands, got %s and %s", type_display(lhs), type_display(rhs));
	}
	if (type_is_scalar(lhs) && type_is_scalar(rhs))
		return lhs;
	if (type_is_vector(lhs) && type_is_vector(rhs))
	{
		if (lhs->cols != rhs->cols)
		{
			type_check_error("operator % requires matching vector sizes, got %d and %d", lhs->cols, rhs->cols);
		}
		return lhs;
	}
	if (type_is_vector(lhs) && type_is_scalar(rhs))
		return lhs;
	if (type_is_scalar(lhs) && type_is_vector(rhs))
		return rhs;
	type_check_error("operator % unsupported for %s and %s", type_display(lhs), type_display(rhs));
	return NULL;
}

Type* type_binary_bitwise(Tok tok, Type* lhs, Type* rhs)
{
	if (!lhs || !rhs)
		return lhs ? lhs : rhs;
	if (!type_is_integer(lhs) || !type_is_integer(rhs))
	{
		type_check_error("operator %s requires integer operands, got %s and %s", tok_name[tok], type_display(lhs), type_display(rhs));
	}
	if (type_is_vector(lhs) && type_is_vector(rhs))
	{
		if (lhs->cols != rhs->cols)
		{
			type_check_error("operator %s requires matching vector sizes, got %d and %d", tok_name[tok], lhs->cols, rhs->cols);
		}
		if (type_base_type(lhs) != type_base_type(rhs))
		{
			type_check_error("operator %s requires matching integer vector types", tok_name[tok]);
		}
		return lhs;
	}
	if (type_is_vector(lhs) && type_is_scalar(rhs))
	{
		if (type_base_type(lhs) != type_base_type(rhs))
		{
			type_check_error("operator %s requires matching integer types", tok_name[tok]);
		}
		return lhs;
	}
	if (type_is_scalar(lhs) && type_is_vector(rhs))
	{
		if (type_base_type(lhs) != type_base_type(rhs))
		{
			type_check_error("operator %s requires matching integer types", tok_name[tok]);
		}
		return rhs;
	}
	if (type_is_scalar(lhs) && type_is_scalar(rhs))
		return lhs;
	type_check_error("operator %s unsupported for %s and %s", tok_name[tok], type_display(lhs), type_display(rhs));
	return NULL;
}

Type* type_binary_shift(Tok tok, Type* lhs, Type* rhs)
{
	if (!lhs || !rhs)
		return lhs ? lhs : rhs;
	if (!type_is_integer(lhs) || !type_is_integer(rhs))
	{
		type_check_error("operator %s requires integer operands, got %s and %s", tok_name[tok], type_display(lhs), type_display(rhs));
	}
	if (type_is_scalar(lhs))
	{
		if (!type_is_scalar(rhs))
		{
			type_check_error("operator %s requires integer scalar shift amounts", tok_name[tok]);
		}
		return lhs;
	}
	if (type_is_vector(lhs))
	{
		if (type_is_scalar(rhs))
			return lhs;
		if (type_is_vector(rhs))
		{
			if (lhs->cols != rhs->cols)
			{
				type_check_error("operator %s requires matching vector sizes, got %d and %d", tok_name[tok], lhs->cols, rhs->cols);
			}
			if (type_base_type(lhs) != type_base_type(rhs))
			{
				type_check_error("operator %s requires matching integer vector types", tok_name[tok]);
			}
			return lhs;
		}
	}
	type_check_error("operator %s unsupported for %s and %s", tok_name[tok], type_display(lhs), type_display(rhs));
	return NULL;
}

Type* type_binary_rel(Tok tok, Type* lhs, Type* rhs)
{
	if (!lhs || !rhs)
		return type_get_scalar(T_BOOL);
	if (!type_is_numeric(lhs) || !type_is_numeric(rhs))
	{
		type_check_error("operator %s requires numeric operands, got %s and %s", tok_name[tok], type_display(lhs), type_display(rhs));
	}
	if (!type_is_scalar(lhs) || !type_is_scalar(rhs))
	{
		type_check_error("operator %s currently supports scalar operands only", tok_name[tok]);
	}
	if (type_base_type(lhs) != type_base_type(rhs))
	{
		type_check_error("operator %s requires matching base types", tok_name[tok]);
	}
	return type_get_scalar(T_BOOL);
}

Type* type_binary_eq(Tok tok, Type* lhs, Type* rhs)
{
	if (!lhs || !rhs)
		return type_get_scalar(T_BOOL);
	if (type_is_vector(lhs) && type_is_vector(rhs))
	{
		if (lhs->cols != rhs->cols || type_base_type(lhs) != type_base_type(rhs))
		{
			type_check_error("operator %s requires matching vector types", tok_name[tok]);
		}
		return type_get_vector(T_BOOL, lhs->cols);
	}
	if (type_is_scalar(lhs) && type_is_scalar(rhs))
	{
		if (type_base_type(lhs) != type_base_type(rhs))
		{
			type_check_error("operator %s requires matching scalar types", tok_name[tok]);
		}
		return type_get_scalar(T_BOOL);
	}
	type_check_error("operator %s unsupported for %s and %s", tok_name[tok], type_display(lhs), type_display(rhs));
	return type_get_scalar(T_BOOL);
}

Type* type_binary_logical(Tok tok, Type* lhs, Type* rhs)
{
	if (!lhs || !rhs)
		return type_get_scalar(T_BOOL);
	if (!type_is_bool_like(lhs) || !type_is_bool_like(rhs) || !type_is_scalar(lhs) || !type_is_scalar(rhs))
	{
		type_check_error("operator %s requires boolean scalars", tok_name[tok]);
	}
	return type_get_scalar(T_BOOL);
}

Type* type_binary_assign(Type* lhs, Type* rhs)
{
	if (!lhs)
		return rhs;
	if (!rhs)
		return lhs;
	if (!type_can_assign(lhs, rhs))
	{
		type_check_error("cannot assign value of type %s to %s", type_display(rhs), type_display(lhs));
	}
	return lhs;
}

Type* type_check_binary(Tok tok, Type* lhs, Type* rhs)
{
	switch (tok)
	{
	case TOK_PLUS:
	case TOK_MINUS:
		return type_binary_add_sub(tok, lhs, rhs);
	case TOK_STAR:
		return type_binary_mul(lhs, rhs);
	case TOK_SLASH:
		return type_binary_div(lhs, rhs);
	case TOK_PERCENT:
		return type_binary_mod(lhs, rhs);
	case TOK_AMP:
	case TOK_PIPE:
	case TOK_CARET:
		return type_binary_bitwise(tok, lhs, rhs);
	case TOK_LSHIFT:
	case TOK_RSHIFT:
		return type_binary_shift(tok, lhs, rhs);
	case TOK_LT:
	case TOK_LE:
	case TOK_GT:
	case TOK_GE:
		return type_binary_rel(tok, lhs, rhs);
	case TOK_EQ:
	case TOK_NE:
		return type_binary_eq(tok, lhs, rhs);
	case TOK_AND_AND:
	case TOK_OR_OR:
		return type_binary_logical(tok, lhs, rhs);
	case TOK_ASSIGN:
		return type_binary_assign(lhs, rhs);
	case TOK_PLUS_ASSIGN:
	{
		Type* sum = type_binary_add_sub(TOK_PLUS, lhs, rhs);
		return type_binary_assign(lhs, sum);
	}
	case TOK_MINUS_ASSIGN:
	{
		Type* diff = type_binary_add_sub(TOK_MINUS, lhs, rhs);
		return type_binary_assign(lhs, diff);
	}
	case TOK_STAR_ASSIGN:
	{
		Type* value = type_binary_mul(lhs, rhs);
		return type_binary_assign(lhs, value);
	}
	case TOK_SLASH_ASSIGN:
	{
		Type* value = type_binary_div(lhs, rhs);
		return type_binary_assign(lhs, value);
	}
	case TOK_PERCENT_ASSIGN:
	{
		Type* value = type_binary_mod(lhs, rhs);
		return type_binary_assign(lhs, value);
	}
	case TOK_AND_ASSIGN:
	{
		Type* value = type_binary_bitwise(TOK_AMP, lhs, rhs);
		return type_binary_assign(lhs, value);
	}
	case TOK_OR_ASSIGN:
	{
		Type* value = type_binary_bitwise(TOK_PIPE, lhs, rhs);
		return type_binary_assign(lhs, value);
	}
	case TOK_XOR_ASSIGN:
	{
		Type* value = type_binary_bitwise(TOK_CARET, lhs, rhs);
		return type_binary_assign(lhs, value);
	}
	case TOK_LSHIFT_ASSIGN:
	{
		Type* value = type_binary_shift(TOK_LSHIFT, lhs, rhs);
		return type_binary_assign(lhs, value);
	}
	case TOK_RSHIFT_ASSIGN:
	{
		Type* value = type_binary_shift(TOK_RSHIFT, lhs, rhs);
		return type_binary_assign(lhs, value);
	}
	default:
		type_check_error("unsupported binary operator %s", tok_name[tok]);
	}
	return NULL;
}

Type* type_select_result(Type* cond, Type* true_type, Type* false_type)
{
	if (cond && (!type_is_bool_like(cond) || !type_is_scalar(cond)))
	{
		type_check_error("ternary condition must be boolean scalar, got %s", type_display(cond));
	}
	if (!true_type || !false_type)
		return true_type ? true_type : false_type;
	if (type_equal(true_type, false_type))
		return true_type;
	if (type_base_type(true_type) == T_BOOL && type_base_type(false_type) == T_BOOL)
	{
		Type* aligned_true = true_type;
		Type* aligned_false = false_type;
		if (type_align_component_counts(&aligned_true, &aligned_false))
			return aligned_true;
	}
	Type* aligned_true = true_type;
	Type* aligned_false = false_type;
	if (type_align_component_counts(&aligned_true, &aligned_false))
	{
		TypeTag shared_base = type_shared_numeric_base(type_base_type(true_type), type_base_type(false_type));
		if (shared_base != T_VOID)
		{
			if (type_is_scalar(aligned_true) && type_is_scalar(aligned_false))
				return type_get_scalar(shared_base);
			if (type_is_vector(aligned_true) && type_is_vector(aligned_false))
				return type_get_vector(shared_base, aligned_true->cols);
			if (type_is_matrix(aligned_true) && type_is_matrix(aligned_false))
				return type_get_matrix(shared_base, aligned_true->cols, aligned_true->rows);
		}
	}
	type_check_error("ternary branches must match types, got %s and %s", type_display(true_type), type_display(false_type));
	return true_type;
}
static void type_constructor_error(const Type* target, const char* owner, const char* member, const char* fmt, ...)
{
	const char* type_name = type_display(target);
	char prefix[256];
	if (owner && member)
	{
		snprintf(prefix, sizeof(prefix), "constructor for %s member %s (%s)", owner, member, type_name);
	}
	else if (owner)
	{
		snprintf(prefix, sizeof(prefix), "constructor for %s (%s)", owner, type_name);
	}
	else
	{
		snprintf(prefix, sizeof(prefix), "constructor %s", type_name);
	}
	va_list args_list;
	va_start(args_list, fmt);
	char suffix[256];
	vsnprintf(suffix, sizeof(suffix), fmt, args_list);
	va_end(args_list);
	char message[512];
	snprintf(message, sizeof(message), "%s %s", prefix, suffix);
	type_check_error("%s", message);
}

static int type_constructor_validate(Type* target, Type** args, int argc, int start, const char* owner, const char* member)
{
	if (!target)
		return 0;
	int remaining = argc - start;
	if (remaining < 0)
		remaining = 0;
	switch (target->tag)
	{
	case T_VEC:
	{
		int needed = target->cols;
		TypeTag base = type_base_type(target);
		if (needed > 1 && remaining == 1)
		{
			Type* arg = args[start];
			if (arg && type_is_scalar(arg))
			{
				if (!type_base_can_convert(type_base_type(arg), base))
					type_constructor_error(target, owner, member, "cannot pass %s to constructor", type_display(arg));
				return 1;
			}
		}
		int count = 0;
		int consumed = 0;
		while (count < needed)
		{
			if (start + consumed >= argc)
				type_constructor_error(target, owner, member, "expected %d components but received %d", needed, count);
			Type* arg = args[start + consumed];
			if (!type_base_can_convert(type_base_type(arg), base))
				type_constructor_error(target, owner, member, "cannot pass %s to constructor", type_display(arg));
			if (type_is_scalar(arg))
			{
				count += 1;
			}
			else if (type_is_vector(arg))
			{
				count += arg->cols;
			}
			else
			{
				type_constructor_error(target, owner, member, "arguments must be scalar or vector, got %s", type_display(arg));
			}
			if (count > needed)
				type_constructor_error(target, owner, member, "expected %d components but received %d", needed, count);
			consumed++;
		}
		return consumed;
	}
	case T_MAT:
	{
		int needed = target->cols * target->rows;
		TypeTag base = type_base_type(target);
		if (needed > 1 && remaining == 1)
		{
			Type* arg = args[start];
			if (arg && type_is_scalar(arg))
			{
				if (!type_base_can_convert(type_base_type(arg), base))
					type_constructor_error(target, owner, member, "cannot pass %s to constructor", type_display(arg));
				return 1;
			}
		}
		int count = 0;
		int consumed = 0;
		while (count < needed)
		{
			if (start + consumed >= argc)
				type_constructor_error(target, owner, member, "expected %d components but received %d", needed, count);
			Type* arg = args[start + consumed];
			if (!type_base_can_convert(type_base_type(arg), base))
				type_constructor_error(target, owner, member, "cannot pass %s to constructor", type_display(arg));
			if (type_is_scalar(arg))
			{
				count += 1;
			}
			else if (type_is_vector(arg))
			{
				if (arg->cols != target->rows)
					type_constructor_error(target, owner, member, "column argument expected %d components, got %d", target->rows, arg->cols);
				count += arg->cols;
			}
			else
			{
				type_constructor_error(target, owner, member, "arguments must be scalars or column vectors, got %s", type_display(arg));
			}
			if (count > needed)
				type_constructor_error(target, owner, member, "expected %d components but received %d", needed, count);
			consumed++;
		}
		return consumed;
	}
	case T_ARRAY:
	{
		if (target->array_len < 0)
			type_constructor_error(target, owner, member, "requires known array size");
		Type* element = target->user ? (Type*)target->user : NULL;
		if (!element)
			type_constructor_error(target, owner, member, "has unknown element type");
		if (remaining > 0)
		{
			Type* arg0 = args[start];
			if (arg0 && arg0 == target)
				return 1;
		}
		int consumed = 0;
		for (int i = 0; i < target->array_len; ++i)
		{
			consumed += type_constructor_validate(element, args, argc, start + consumed, owner, member);
		}
		return consumed;
	}
	case T_STRUCT:
	{
		StructInfo* info = type_struct_info(target);
		if (!info)
			type_constructor_error(target, owner, member, "is incomplete");
		const char* struct_name = target->name ? target->name : type_display(target);
		if (remaining > 0)
		{
			Type* arg0 = args[start];
			if (arg0 && arg0 == target)
				return 1;
		}
		int consumed = 0;
		int member_count = type_struct_member_count(target);
		for (int i = 0; i < member_count; ++i)
		{
			StructMember* field = type_struct_member_at(target, i);
			if (!field)
				continue;
			Type* field_type = field->type ? field->type : field->declared_type;
			if (!field_type)
				type_constructor_error(target, struct_name, field->name, "has unknown type");
			consumed += type_constructor_validate(field_type, args, argc, start + consumed, struct_name, field->name);
		}
		return consumed;
	}
	default:
	{
		if (!type_is_scalar(target))
			type_constructor_error(target, owner, member, "unsupported target");
		if (remaining <= 0)
			type_constructor_error(target, owner, member, "expects 1 argument but received 0");
		Type* arg = args[start];
		if (!type_scalar_can_convert(arg, target))
			type_constructor_error(target, owner, member, "cannot convert %s to %s", type_display(arg), type_display(target));
		return 1;
	}
	}
}

void type_check_constructor(Type* target, Type** args, int argc)
{
	if (!target)
		return;
	int has_unknown = 0;
	for (int i = 0; i < argc; ++i)
	{
		if (!args[i])
		{
			has_unknown = 1;
			break;
		}
	}
	if (has_unknown)
		return;
	int consumed = type_constructor_validate(target, args, argc, 0, NULL, NULL);
	if (consumed != argc)
	{
		type_check_error("constructor %s expected %d arguments but received %d", type_display(target), consumed, argc);
	}
}

void type_check_error(const char* fmt, ...)
{
	va_list args;
	va_start(args, fmt);
	fprintf(stderr, "Type error: ");
	vfprintf(stderr, fmt, args);
	fprintf(stderr, "\n");
	va_end(args);
	exit(1);
}

#define type_stack_pop(stack, context) (((acount(stack) == 0) ? type_check_error("missing operand for %s", context) : (void)0), apop(stack))

typedef struct TypeCheckSwitchCase
{
	int value;
	int flags;
	int has_value;
} TypeCheckSwitchCase;

typedef struct TypeCheckSwitch
{
	dyna TypeCheckSwitchCase* cases;
} TypeCheckSwitch;

void type_check_ir()
{
	dyna Type** stack = NULL;
	dyna Type** func_stack = NULL;
	dyna TypeCheckSwitch* switch_stack = NULL;
	dyna unsigned* qualifier_stack = NULL;
	dyna unsigned* storage_stack = NULL;
	dyna Symbol** symbol_stack = NULL;
	Type* current_decl_type = NULL;
	const char* current_decl_name = NULL;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		switch (inst->op)
		{
		case IR_PUSH_INT:
			inst->type = inst->is_unsigned_literal ? g_type_uint : g_type_int;
			apush(stack, inst->type);
			inst->qualifier_flags = 0;
			apush(qualifier_stack, 0);
			apush(storage_stack, 0);
			apush(symbol_stack, NULL);
			break;
		case IR_PUSH_FLOAT:
			inst->type = g_type_float;
			apush(stack, inst->type);
			inst->qualifier_flags = 0;
			apush(qualifier_stack, 0);
			apush(storage_stack, 0);
			apush(symbol_stack, NULL);
			break;
		case IR_PUSH_BOOL:
			inst->type = g_type_bool;
			apush(stack, inst->type);
			inst->qualifier_flags = 0;
			apush(qualifier_stack, 0);
			apush(storage_stack, 0);
			apush(symbol_stack, NULL);
			break;
		case IR_PUSH_IDENT:
		{
			Type* type = NULL;
			Symbol* sym = NULL;
			if (inst->str0)
			{
				sym = symbol_table_find(inst->str0);
				if (!sym)
				{
					for (int j = acount(st->symbols) - 1; j >= 0; --j)
					{
						Symbol* candidate = &st->symbols[j];
						if (candidate->name == inst->str0)
						{
							sym = candidate;
							break;
						}
					}
				}
				if (sym && sym->type)
				{
					type = sym->type;
				}
				else
				{
					type = type_system_get(inst->str0);
				}
			}
			unsigned qualifier_flags = sym ? sym->qualifier_flags : 0;
			unsigned storage_flags = sym ? sym->storage_flags : 0;
			inst->qualifier_flags = qualifier_flags;
			inst->storage_flags = storage_flags;
			inst->type = type;
			apush(stack, type);
			apush(qualifier_stack, qualifier_flags);
			apush(storage_stack, storage_flags);
			apush(symbol_stack, sym);
			break;
		}
		case IR_UNARY:
		{
			Type* operand = type_stack_pop(stack, "unary expression");
			unsigned operand_qualifier = acount(qualifier_stack) ? apop(qualifier_stack) : 0;
			if (acount(storage_stack))
				(void)apop(storage_stack);
			if (acount(symbol_stack))
				(void)apop(symbol_stack);
			Type* result = type_check_unary(inst, operand);
			if ((inst->tok == TOK_PLUS_PLUS || inst->tok == TOK_MINUS_MINUS) && (operand_qualifier & SYM_QUAL_CONST))
			{
				type_check_error("cannot modify const-qualified value");
			}
			if (!result)
				result = operand;
			inst->type = result;
			inst->qualifier_flags = 0;
			apush(stack, result);
			apush(qualifier_stack, 0);
			apush(storage_stack, 0);
			apush(symbol_stack, NULL);
			break;
		}
		case IR_BINARY:
		{
			Type* rhs = type_stack_pop(stack, "binary rhs");
			if (acount(storage_stack))
				(void)apop(storage_stack);
			if (acount(symbol_stack))
				(void)apop(symbol_stack);
			Type* lhs = type_stack_pop(stack, "binary lhs");
			unsigned lhs_storage = acount(storage_stack) ? apop(storage_stack) : 0;
			Symbol* lhs_symbol = acount(symbol_stack) ? apop(symbol_stack) : NULL;
			if (acount(qualifier_stack))
				(void)apop(qualifier_stack);
			unsigned lhs_qualifier = acount(qualifier_stack) ? apop(qualifier_stack) : 0;
			int is_assignment = 0;
			switch (inst->tok)
			{
			case TOK_ASSIGN:
			case TOK_PLUS_ASSIGN:
			case TOK_MINUS_ASSIGN:
			case TOK_STAR_ASSIGN:
			case TOK_SLASH_ASSIGN:
			case TOK_PERCENT_ASSIGN:
			case TOK_AND_ASSIGN:
			case TOK_OR_ASSIGN:
			case TOK_XOR_ASSIGN:
			case TOK_LSHIFT_ASSIGN:
			case TOK_RSHIFT_ASSIGN:
				is_assignment = 1;
				break;
			default:
				break;
			}
			if (is_assignment)
			{
				if (lhs_qualifier & SYM_QUAL_CONST)
				{
					type_check_error("cannot assign to const-qualified value");
				}
				if ((lhs_storage & SYM_STORAGE_IN) && !(lhs_storage & SYM_STORAGE_OUT))
				{
					const char* target = lhs_symbol ? lhs_symbol->name : "value";
					type_check_error("cannot assign to input %s", target);
				}
			}
			Type* result = type_check_binary(inst->tok, lhs, rhs);
			if (!result)
				result = lhs ? lhs : rhs;
			inst->type = result;
			unsigned result_qualifier = is_assignment ? lhs_qualifier : 0;
			unsigned result_storage = is_assignment ? lhs_storage : 0;
			inst->qualifier_flags = result_qualifier;
			inst->storage_flags = result_storage;
			apush(stack, result);
			apush(qualifier_stack, result_qualifier);
			apush(storage_stack, result_storage);
			apush(symbol_stack, is_assignment ? lhs_symbol : NULL);
			break;
		}
		case IR_CALL:
		{
			Type** args = NULL;
			for (int arg = 0; arg < inst->arg0; ++arg)
			{
				Type* arg_type = type_stack_pop(stack, "call argument");
				if (acount(qualifier_stack))
					(void)apop(qualifier_stack);
				if (acount(storage_stack))
					(void)apop(storage_stack);
				if (acount(symbol_stack))
					(void)apop(symbol_stack);
				apush(args, arg_type);
			}
			int argc = acount(args);
			for (int l = 0, r = argc - 1; l < r; ++l, --r)
			{
				Type* tmp = args[l];
				args[l] = args[r];
				args[r] = tmp;
			}
			Type* callee = type_stack_pop(stack, "call target");
			if (acount(qualifier_stack))
				(void)apop(qualifier_stack);
			if (acount(storage_stack))
				(void)apop(storage_stack);
			if (acount(symbol_stack))
				(void)apop(symbol_stack);
			Type* result = callee;
			Symbol* sym = NULL;
		if (inst->str0)
		{
			sym = symbol_table_find(inst->str0);
		}
			if (sym && sym->kind == SYM_FUNC)
			{
				if (sym->builtin_param_count >= 0 && sym->builtin_param_count != argc)
				{
					type_check_error("function %s expects %d arguments but received %d", inst->str0, sym->builtin_param_count, argc);
				}
				if (sym->param_signature_set)
				{
					if (sym->param_count != argc)
					{
						type_check_error("function %s expects %d arguments but received %d", inst->str0, sym->param_count, argc);
					}
					for (int i = 0; i < sym->param_count; ++i)
					{
						Type* param_type = (sym->params && i < acount(sym->params)) ? sym->params[i] : NULL;
						Type* arg_type = args ? args[i] : NULL;
						if (param_type && arg_type)
						{
							if (!type_can_assign(param_type, arg_type))
							{
								type_check_error("argument %d to %s expects %s but got %s", i + 1, inst->str0, type_display(param_type), type_display(arg_type));
							}
						}
						else if (param_type != arg_type)
						{
							type_check_error("argument %d to %s has incompatible type", i + 1, inst->str0);
						}
					}
				}
				if (sym->builtin_kind != BUILTIN_NONE)
				{
					Type* builtin_result = type_infer_builtin_call(sym, args, argc);
					if (builtin_result)
					{
						result = builtin_result;
					}
					else if (sym->type)
					{
						result = sym->type;
					}
				}
				else if (sym->type)
				{
					result = sym->type;
				}
			}
			inst->type = result;
			afree(args);
			inst->qualifier_flags = 0;
			apush(stack, result);
			apush(qualifier_stack, 0);
			apush(storage_stack, 0);
			apush(symbol_stack, NULL);
			break;
		}
		case IR_CONSTRUCT:
		{
			Type** args = NULL;
			for (int arg = 0; arg < inst->arg0; ++arg)
			{
				Type* arg_type = type_stack_pop(stack, "constructor argument");
				if (acount(qualifier_stack))
					(void)apop(qualifier_stack);
				if (acount(storage_stack))
					(void)apop(storage_stack);
				if (acount(symbol_stack))
					(void)apop(symbol_stack);
				apush(args, arg_type);
			}
			int argc = acount(args);
			for (int l = 0, r = argc - 1; l < r; ++l, --r)
			{
				Type* tmp = args[l];
				args[l] = args[r];
				args[r] = tmp;
			}
			Type* target = type_stack_pop(stack, "constructor target");
			Type* result = inst->type ? inst->type : target;
			if (acount(storage_stack))
				(void)apop(storage_stack);
			if (acount(symbol_stack))
				(void)apop(symbol_stack);
			inst->type = result;
			if (result)
				type_check_constructor(result, args, argc);
			afree(args);
			inst->qualifier_flags = 0;
			apush(stack, result);
			apush(qualifier_stack, 0);
			apush(storage_stack, 0);
			apush(symbol_stack, NULL);
			break;
		}
		case IR_INDEX:
		{
			Type* index = type_stack_pop(stack, "index expression");
			if (acount(qualifier_stack))
				(void)apop(qualifier_stack);
			if (acount(storage_stack))
				(void)apop(storage_stack);
			if (acount(symbol_stack))
				(void)apop(symbol_stack);
			Type* base = type_stack_pop(stack, "index base");
			unsigned base_qualifier = acount(qualifier_stack) ? apop(qualifier_stack) : 0;
			unsigned base_storage = acount(storage_stack) ? apop(storage_stack) : 0;
			Symbol* base_symbol = acount(symbol_stack) ? apop(symbol_stack) : NULL;
			if (index && (!type_is_scalar(index) || !type_is_integer(index)))
			{
				type_check_error("index expression must be integer scalar, got %s", type_display(index));
			}
			Type* result = NULL;
			if (base)
			{
				if (type_is_vector(base))
				{
					result = type_get_scalar(type_base_type(base));
				}
				else if (type_is_matrix(base))
				{
					result = type_get_vector(type_base_type(base), base->rows);
				}
				else if (base->tag == T_ARRAY)
				{
					result = base->user ? (Type*)base->user : NULL;
				}
				else
				{
					type_check_error("type %s is not indexable", type_display(base));
				}
			}
			inst->type = result;
			inst->qualifier_flags = base_qualifier;
			apush(stack, result);
			apush(qualifier_stack, base_qualifier);
			inst->storage_flags = base_storage;
			apush(storage_stack, base_storage);
			apush(symbol_stack, base_symbol);
			break;
		}
		case IR_SWIZZLE:
		{
			Type* base = type_stack_pop(stack, "swizzle base");
			unsigned base_qualifier = acount(qualifier_stack) ? apop(qualifier_stack) : 0;
			unsigned base_storage = acount(storage_stack) ? apop(storage_stack) : 0;
			Symbol* base_symbol = acount(symbol_stack) ? apop(symbol_stack) : NULL;
			if (base && !type_is_vector(base))
			{
				type_check_error("cannot swizzle non-vector type %s", type_display(base));
			}
			Type* result = NULL;
			if (base)
			{
				if (inst->arg0 == 1)
				{
					result = type_get_scalar(type_base_type(base));
				}
				else
				{
					result = type_get_vector(type_base_type(base), inst->arg0);
				}
			}
			if (!result)
			{
				type_check_error("unsupported swizzle size %d on %s", inst->arg0, type_display(base));
			}
			inst->type = result;
			inst->qualifier_flags = base_qualifier;
			apush(stack, result);
			apush(qualifier_stack, base_qualifier);
			inst->storage_flags = base_storage;
			apush(storage_stack, base_storage);
			apush(symbol_stack, base_symbol);
			break;
		}
		case IR_MEMBER:
		{
			Type* base = type_stack_pop(stack, "member access");
			unsigned base_qualifier = acount(qualifier_stack) ? apop(qualifier_stack) : 0;
			unsigned base_storage = acount(storage_stack) ? apop(storage_stack) : 0;
			Symbol* base_symbol = acount(symbol_stack) ? apop(symbol_stack) : NULL;
			Type* result = base;
			if (base && base->tag == T_STRUCT)
			{
				StructMember* member = type_struct_find_member(base, inst->str0);
				if (!member)
				{
					type_check_error("struct %s has no member %s", type_display(base), inst->str0 ? inst->str0 : "<anon>");
				}
				result = member->type;
			}
			inst->type = result;
			inst->qualifier_flags = base_qualifier;
			apush(stack, result);
			apush(qualifier_stack, base_qualifier);
			inst->storage_flags = base_storage;
			apush(storage_stack, base_storage);
			apush(symbol_stack, base_symbol);
			break;
		}
		case IR_SELECT:
		{
			Type* false_type = type_stack_pop(stack, "ternary false branch");
			if (acount(qualifier_stack))
				(void)apop(qualifier_stack);
			if (acount(storage_stack))
				(void)apop(storage_stack);
			if (acount(symbol_stack))
				(void)apop(symbol_stack);
			Type* true_type = type_stack_pop(stack, "ternary true branch");
			if (acount(qualifier_stack))
				(void)apop(qualifier_stack);
			if (acount(storage_stack))
				(void)apop(storage_stack);
			if (acount(symbol_stack))
				(void)apop(symbol_stack);
			Type* cond_type = type_stack_pop(stack, "ternary condition");
			if (acount(qualifier_stack))
				(void)apop(qualifier_stack);
			if (acount(storage_stack))
				(void)apop(storage_stack);
			if (acount(symbol_stack))
				(void)apop(symbol_stack);
			Type* result = type_select_result(cond_type, true_type, false_type);
			if (!result)
				result = true_type ? true_type : false_type;
			inst->type = result;
			inst->qualifier_flags = 0;
			apush(stack, result);
			apush(qualifier_stack, 0);
			apush(storage_stack, 0);
			apush(symbol_stack, NULL);
			break;
		}
		case IR_DECL_TYPE:
			current_decl_type = type_system_get(inst->str0);
			inst->type = current_decl_type;
			current_decl_name = NULL;
			break;
		case IR_DECL_VAR:
			current_decl_name = inst->str0;
			break;
		case IR_DECL_END:
			current_decl_type = NULL;
			current_decl_name = NULL;
			break;
		case IR_DECL_INIT_END:
			if (acount(stack) > 0)
			{
				Type* value = type_stack_pop(stack, "initializer");
				if (acount(qualifier_stack))
					(void)apop(qualifier_stack);
				if (current_decl_type && value && !type_can_assign(current_decl_type, value))
				{
					type_check_error("initializer type %s cannot initialize %s %s", type_display(value), current_decl_name ? current_decl_name : "value", type_display(current_decl_type));
				}
			}
			if (acount(stack) > 0)
				aclear(stack);
			if (acount(qualifier_stack) > 0)
				aclear(qualifier_stack);
			break;
		case IR_DECL_ARRAY_SIZE_END:
		case IR_FUNC_PARAM_ARRAY_SIZE_END:
			if (acount(stack) > 0)
			{
				Type* size = type_stack_pop(stack, "array size");
				if (acount(qualifier_stack))
					(void)apop(qualifier_stack);
				if (size && (!type_is_scalar(size) || !type_is_integer(size)))
				{
					type_check_error("array size must be integer scalar, got %s", type_display(size));
				}
			}
			if (acount(stack) > 0)
				aclear(stack);
			if (acount(qualifier_stack) > 0)
				aclear(qualifier_stack);
			break;
		case IR_SWITCH_BEGIN:
			apush(switch_stack, (TypeCheckSwitch){ 0 });
			break;
		case IR_SWITCH_SELECTOR_BEGIN:
			break;
		case IR_SWITCH_SELECTOR_END:
		{
			Type* selector = type_stack_pop(stack, "switch selector");
			if (acount(qualifier_stack))
				(void)apop(qualifier_stack);
			if (selector && (!type_is_scalar(selector) || !type_is_integer(selector)))
			{
				type_check_error("switch selector must be integer scalar, got %s", type_display(selector));
			}
			break;
		}
		case IR_SWITCH_CASE:
		{
			if (!acount(switch_stack))
			{
				type_check_error("switch case outside of switch");
			}
			TypeCheckSwitch* ctx = &switch_stack[acount(switch_stack) - 1];
			TypeCheckSwitchCase label = (TypeCheckSwitchCase){ 0 };
			label.flags = inst->arg1;
			if (!(inst->arg1 & SWITCH_CASE_FLAG_DEFAULT))
			{
				label.value = inst->arg0;
				label.has_value = 1;
			}
			apush(ctx->cases, label);
			break;
		}
		case IR_SWITCH_END:
		{
			if (!acount(switch_stack))
			{
				type_check_error("mismatched switch end");
			}
			TypeCheckSwitch* ctx = &switch_stack[acount(switch_stack) - 1];
			int default_count = 0;
			for (int idx = 0; idx < acount(ctx->cases); ++idx)
			{
				TypeCheckSwitchCase* label = &ctx->cases[idx];
				if (label->flags & SWITCH_CASE_FLAG_DEFAULT)
				{
					default_count++;
				}
				if ((label->flags & SWITCH_CASE_FLAG_HAS_BODY) == 0)
				{
					if (!(label->flags & SWITCH_CASE_FLAG_FALLTHROUGH))
					{
						type_check_error("case label with no statements must fall through to another label");
					}
				}
				if ((label->flags & SWITCH_CASE_FLAG_HAS_BODY) && (label->flags & SWITCH_CASE_FLAG_FALLTHROUGH))
				{
					type_check_error("case label with statements cannot be marked as fallthrough");
				}
			}
			if (default_count > 1)
			{
				type_check_error("multiple default labels in switch");
			}
			for (int i = 0; i < acount(ctx->cases); ++i)
			{
				TypeCheckSwitchCase* a = &ctx->cases[i];
				if (!a->has_value)
					continue;
				for (int j = i + 1; j < acount(ctx->cases); ++j)
				{
					TypeCheckSwitchCase* b = &ctx->cases[j];
					if (b->has_value && a->value == b->value)
					{
						type_check_error("duplicate case label value %d in switch", a->value);
					}
				}
			}
			afree(ctx->cases);
			(void)apop(switch_stack);
			break;
		}
		case IR_STMT_EXPR:
			if (acount(stack) > 0)
			{
				type_stack_pop(stack, "expression result");
				if (acount(qualifier_stack))
					(void)apop(qualifier_stack);
				if (acount(storage_stack))
					(void)apop(storage_stack);
				if (acount(symbol_stack))
					(void)apop(symbol_stack);
			}
			if (acount(stack) > 0)
				aclear(stack);
			if (acount(qualifier_stack) > 0)
				aclear(qualifier_stack);
			if (acount(storage_stack) > 0)
				aclear(storage_stack);
			if (acount(symbol_stack) > 0)
				aclear(symbol_stack);
			break;
		case IR_IF_THEN:
		{
			Type* cond = type_stack_pop(stack, "if condition");
			if (acount(qualifier_stack))
				(void)apop(qualifier_stack);
			if (acount(storage_stack))
				(void)apop(storage_stack);
			if (acount(symbol_stack))
				(void)apop(symbol_stack);
			if (cond && (!type_is_bool_like(cond) || !type_is_scalar(cond)))
			{
				type_check_error("if condition must be boolean scalar, got %s", type_display(cond));
			}
			break;
		}
		case IR_RETURN:
		{
			Type* ret_type = acount(func_stack) ? func_stack[acount(func_stack) - 1] : NULL;
			if (inst->arg0)
			{
				Type* value = type_stack_pop(stack, "return value");
				if (acount(qualifier_stack))
					(void)apop(qualifier_stack);
				if (acount(storage_stack))
					(void)apop(storage_stack);
				if (acount(symbol_stack))
					(void)apop(symbol_stack);
				if (ret_type && ret_type != g_type_void && value && !type_can_assign(ret_type, value))
				{
					type_check_error("return type mismatch: expected %s got %s", type_display(ret_type), type_display(value));
				}
			}
			else if (ret_type && ret_type != g_type_void)
			{
				type_check_error("return statement missing value for function returning %s", type_display(ret_type));
			}
			break;
		}
		case IR_FUNC_BEGIN:
		{
			Type* ret = type_system_get(inst->str0);
			inst->type = ret;
			apush(func_stack, ret);
			break;
		}
		case IR_FUNC_PROTOTYPE_END:
		case IR_FUNC_DEFINITION_END:
			if (acount(func_stack) > 0)
				(void)apop(func_stack);
			break;
		default:
			break;
		}
	}
	for (int i = 0; i < acount(switch_stack); ++i)
	{
		afree(switch_stack[i].cases);
	}
	afree(switch_stack);
	afree(storage_stack);
	afree(symbol_stack);
	afree(stack);
	afree(func_stack);
	afree(qualifier_stack);
}

//--------------------------------------------------------------------------------------------------
// Diagnostics, dumping helpers, and unit tests.

#define SPINDLE_SNIPPET(...) #__VA_ARGS__
const char* snippet_basic_io = SPINDLE_SNIPPET(
		layout(location = 0) in vec3 in_pos;
		layout(location = 1) in vec2 in_uv;
		layout(location = 0) out vec4 out_color;
		layout(set = 0, binding = 0) uniform sampler2D u_texture;
		layout(set = 1, binding = 0) uniform vec4 u_tint;
		void main() {
			vec4 sampled = texture(u_texture, in_uv);
			out_color = sampled * u_tint;
		});

const char* snippet_stage_builtins_vertex = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		void main() {
			float base = float(gl_VertexIndex + gl_BaseInstance);
			gl_Position = vec4(base, 0.0, 0.0, 1.0);
			gl_PointSize = 1.0;
			gl_ClipDistance[0] = base;
			out_color = vec4(base);
		});

const char* snippet_control_flow = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		void main() {
			float accum = 0.0;
			int counter = 0;
			accum += float(counter++);
			accum += float(++counter);
			vec2 adjustments = vec2(0.25, 0.25);
			adjustments++;
			--adjustments;
			accum += adjustments.x;
			for (int i = 0; i < 4; ++i)
			{
				accum += float(i) * 0.25;
			}
			counter--;
			--counter;
			if (accum > 0.5)
			{
				out_color = vec4(accum, 1.0 - accum, accum * 0.5, 1.0);
			}
			else
			{
				out_color = vec4(1.0 - accum);
			}
		});

const char* snippet_ternary_vectors = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		void main() {
			bool flag = true;
			vec3 vector_false = flag ? vec3(1.0, 0.0, 0.0) : 0.5;
			vec4 vector_true = flag ? 0.25 : vec4(0.0, 1.0, 0.0, 1.0);
			out_color = vec4(vector_false, vector_true.x);
		});

const char* snippet_array_indexing = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		void main() {
			float scalars[4];
			scalars[0] = 1.0;
			int ints[3];
			ints[1] = 2;
			uint uints[3];
			uints[2] = 3u;
			bool flags[2];
			flags[0] = false;
			flags[1] = ints[1] > 0;
			vec4 vectors[2];
			vec4 v = vectors[1];
			mat3 matrices[2];
			mat3 m = matrices[0];
			vec3 column = m[1];
			float element = column[2];
			out_color = vec4(scalars[0], float(ints[1]), v.x, element);
			bool flag = flags[1];
			bool literal_true = true;
			uint value = uints[2];
			if (literal_true && flag)
			{
				out_color.xy += vec2(float(value));
			}
		});

const char* snippet_swizzle = SPINDLE_SNIPPET(
		layout(location = 0) in vec4 input_vec;
		layout(location = 0) out vec4 out_vec;
		void main() {
			float single = input_vec.x;
			vec3 rgb = input_vec.rgb;
			vec2 ba = input_vec.ba;
			vec4 assembled = vec4(rgb, 1.0);
			vec4 full = input_vec.xyzw;
			out_vec = vec4(ba, assembled.gr);
			out_vec += vec4(single, full.wzy);
		});

const char* snippet_function_calls = SPINDLE_SNIPPET(
		layout(location = 0) in vec2 in_uv;
		layout(location = 0) out vec4 out_color;
		float saturate(float value) {
			if (value < 0.0)
				return 0.0;
			if (value > 1.0)
				return 1.0;
			return value;
		} vec4 apply_gain(vec4 color, float gain) {
			return vec4(color.rgb * gain, color.a);
		} void main() {
			vec4 base = vec4(in_uv, 0.5, 1.0);
			float gain = saturate(base.x + base.y);
			out_color = apply_gain(base, gain);
		});

const char* snippet_matrix_ops = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		void main() {
			mat3 rotation = mat3(1.0);
			vec3 column = rotation[1];
			float diagonal = rotation[2][2];
			vec3 axis = vec3(1.0, 0.0, 0.0);
			vec3 rotated = rotation * axis;
			vec3 row_combo = axis * rotation;
			mat3 scale = mat3(2.0);
			mat3 combined = rotation * scale;
			mat2x3 rect_a = mat2x3(1.0);
			mat3x2 rect_b = mat3x2(1.0);
			mat3 rect_product = rect_a * rect_b;
			vec2 weights = vec2(0.5, 2.0);
			vec3 rect_vec = rect_a * weights;
			vec2 rect_row = row_combo * rect_a;
			out_color = vec4(column + rotated + rect_vec, diagonal + rect_row.x + combined[0][0] + rect_product[0][0]);
		});

const char* snippet_struct_block = SPINDLE_SNIPPET(
		struct Light {
			vec3 position;
			float intensity;
		};
		layout(std140, set = 1, binding = 0) uniform LightBlock {
			Light lights[2];
		} u_light_data;
		layout(location = 0) out vec4 out_color;
		void main() {
			out_color = vec4(u_light_data.lights[0].position, u_light_data.lights[0].intensity);
		});

const char* snippet_looping = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		void main() {
			int counter = 0;
			float total = 0.0;
			while (counter < 4)
			{
				total += float(counter);
				counter = counter + 1;
			}
			do
			{
				total = total + 0.5;
				counter = counter - 1;
				if (counter == 1)
				{
					continue;
				}
			} while (counter > 0);
			out_color = vec4(total);
		});

const char* snippet_bitwise = SPINDLE_SNIPPET(
		layout(location = 0) out ivec2 out_bits;
		void main() {
			int a = 5;
			int b = 3;
			int mask = a & b;
			int mix = (a | b) ^ 1;
			int shifted = (a << 2) >> 1;
			ivec2 vec_mask = ivec2(1, 2);
			ivec2 values = ivec2(4, 8);
			values |= vec_mask;
			values &= ivec2(7, 7);
			values ^= ivec2(1, 2);
			values <<= ivec2(1, 0);
			values >>= 1;
			out_bits = values + ivec2(mask, mix + shifted);
		});

const char* snippet_numeric_literals = SPINDLE_SNIPPET(
		layout(location = 0) out ivec3 out_values;
		layout(location = 1) out uvec3 out_uvalues;
		void main() {
			int hex_val = 0x1f;
			int bin_val = 0b1010;
			int oct_val = 075;
			uint uhex_val = 0x1fu;
			uint ubin_val = 0b1010u;
			uint uoct_val = 075u;
			uint udec_val = 42u;
			out_values = ivec3(hex_val, bin_val, oct_val);
			out_uvalues = uvec3(uhex_val, ubin_val, uoct_val);
		});

const char* snippet_switch_stmt = SPINDLE_SNIPPET(
		layout(location = 0) in int in_mode;
		layout(location = 0) out vec4 out_color;
		void main() {
			int mode = in_mode;
			vec4 color = vec4(0.0);
			switch (mode)
			{
			case 0:
				color = vec4(1.0, 0.0, 0.0, 1.0);
				break;
			case 1:
			case 2:
				color = vec4(float(mode));
				break;
			default:
				color = vec4(0.0, 0.0, 1.0, 1.0);
				break;
			}
			out_color = color;
		});

const char* snippet_discard = SPINDLE_SNIPPET(
		layout(location = 0) in vec4 in_color;
		layout(location = 0) out vec4 out_color;
		void main() {
			vec4 color = in_color;
			if (color.a == 0.0)
			{
				discard;
			}
			out_color = color;
		});

const char* snippet_stage_builtins_fragment = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		void main() {
			vec4 coord = gl_FragCoord;
			int sample_id = gl_SampleID;
			int sample_mask = gl_SampleMaskIn[0];
			if (gl_HelperInvocation)
			{
				gl_SampleMask[0] = 0;
			}
			out_color = coord + vec4(float(sample_id + sample_mask));
		});

const char* snippet_stage_builtins_compute = SPINDLE_SNIPPET(
		layout(local_size_x = 8, local_size_y = 8, local_size_z = 1) in;
		readonly layout(std430, set = 0, binding = 0) buffer Globals { uvec4 data[]; } u_globals;
		shared float tile[64];
		void main() {
			uvec3 gid = gl_GlobalInvocationID;
			uvec3 lid = gl_LocalInvocationID;
			uint index = gl_LocalInvocationIndex;
			tile[index] = float(gid.x + lid.y);
		});

const char* snippet_builtin_funcs = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		layout(set = 0, binding = 0) uniform sampler2D u_tex;
		void main() {
			vec2 uv = clamp(vec2(0.2, 0.8), vec2(0.0), vec2(1.0));
			vec4 sampled = texture(u_tex, uv);
			float m = max(sampled.x, sampled.y);
			float f = frac(m);
			float shade = dot(sampled.rgb, vec3(0.299, 0.587, 0.114));
			vec3 unit = normalize(sampled.rgb);
			float len_val = length(sampled.rgb);
			float dist_val = distance(sampled.xy, vec2(0.25, 0.75));
			float dot_val = dot(sampled.rgb, unit);
			float grad_x = dFdx(m);
			float grad_y = dFdy(m);
			float grad_w = fwidth(m);
			float soft = smoothstep(0.1, 0.9, m);
			float stepped = step(0.5, m);
			vec3 mixed = mix(sampled.rgb, vec3(1.0), 0.25);
			vec3 boolean_mix = mix(sampled.rgb, vec3(0.0), bvec3(true, false, true));
			float min_component = min(m, shade);
			float pow_val = pow(shade, 2.0);
			vec3 reflected = reflect(unit, vec3(0.0, 0.0, 1.0));
			vec3 refracted = refract(unit, vec3(0.0, 0.0, 1.0), 1.33);
			out_color = vec4(mixed * (f + soft + stepped + min_component + pow_val), sampled.a);
		});

const char* snippet_texture_queries = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		layout(set = 0, binding = 0) uniform sampler2D u_tex;
		void main() {
			ivec2 tex_size = textureSize(u_tex, 0);
			vec4 texel = texelFetch(u_tex, ivec2(1, 1), 0);
			mat3 identity = mat3(1.0);
			mat3 inv_identity = inverse(identity);
			mat3 trans_identity = transpose(identity);
			bvec2 less_mask = lessThan(vec2(0.0, 1.0), vec2(1.0, 1.0));
			bvec2 ge_mask = greaterThanEqual(vec2(1.0, 2.0), vec2(1.0, 1.0));
			bvec2 eq_mask = equal(less_mask, ge_mask);
			bvec2 ne_mask = notEqual(less_mask, bvec2(false, false));
			bool all_true = all(eq_mask);
			bool any_true = any(ne_mask);
			float texel_extent = float(tex_size.x + tex_size.y);
			vec3 basis = (all_true && any_true && inv_identity[0][0] == trans_identity[0][0]) ? vec3(texel_extent, 1.0, 1.0) : vec3(0.0, 0.0, 1.0);
			out_color = texel + vec4(basis, 0.0);
		});

const char* snippet_extended_intrinsics = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		layout(set = 2, binding = 0) uniform sampler2D u_tex;
		layout(set = 2, binding = 1) uniform iimage2D u_image;
		void main() {
			vec2 uv = vec2(0.25, 0.75);
			vec4 offset_sample = textureOffset(u_tex, uv, ivec2(1, -1));
			vec4 lod_offset = textureLodOffset(u_tex, uv, 0.0, ivec2(0, 1));
			vec4 grad_offset = textureGradOffset(u_tex, uv, vec2(1.0, 0.0), vec2(0.0, 1.0), ivec2(1, 0));
			vec4 gather0 = textureGather(u_tex, uv, 0);
			vec4 gather_offset = textureGatherOffset(u_tex, uv, ivec2(1, 0), 0);
			vec2 lod_info = textureQueryLod(u_tex, uv);
			int level_count = textureQueryLevels(u_tex);
			vec4 fetch_offset = texelFetchOffset(u_tex, ivec2(0, 0), 0, ivec2(1, 0));
			vec2 fine_width = fwidthFine(uv);
			vec2 coarse_width = fwidthCoarse(uv);
			vec2 deriv_mix = dFdxFine(uv) + dFdyCoarse(uv);
			mat3 base = mat3(1.0);
			float det = determinant(base);
			mat3 comp = matrixCompMult(base, transpose(base));
			mat3 outer = outerProduct(vec3(1.0, 0.0, 1.0), vec3(0.5, 0.25, 0.75));
			ivec2 pixel = ivec2(0, 0);
			int previous = imageAtomicAdd(u_image, pixel, 1);
			int swapped = imageAtomicCompSwap(u_image, pixel, previous, previous + 1);
			float derivative_sum = fine_width.x + coarse_width.y + deriv_mix.x;
			float matrix_sum = comp[0][0] + outer[0][0];
			vec4 accum = offset_sample + lod_offset + grad_offset;
			accum += gather0 + gather_offset + fetch_offset;
			out_color = accum + vec4(vec3(det + matrix_sum + derivative_sum + float(level_count) + float(swapped), lod_info), 1.0);
		});

const char* snippet_preprocessor_passthrough =
		"#define UNUSED_CONSTANT 1\n"
		"#define MULTI_LINE_MACRO(x) \\\n"
		"\tdo { \\\n"
		"\t\tint temp = 0; \\\n"
		"\t\ttemp += (x); \\\n"
		"\t} while (0)\n"
		"\n"
		"layout(location = 0) out vec4 out_color;\n"
		"void main() {\n"
		"\tout_color = vec4(0.25, 0.5, 0.75, 1.0);\n"
		"}\n";

const char* snippet_const_qualifier = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		void main() {
			const float factor = 0.5;
			float mutable_val = 1.0;
			mutable_val = mutable_val + factor;
			out_color = vec4(mutable_val);
		});

const char* snippet_resource_types = SPINDLE_SNIPPET(
		layout(location = 0) out vec4 out_color;
		layout(set = 2, binding = 0) uniform sampler1D u_sampler1d;
		layout(set = 2, binding = 1) uniform sampler3D u_sampler3d;
		layout(set = 2, binding = 2) uniform sampler2DShadow u_sampler_shadow;
		layout(set = 2, binding = 3) uniform isampler2D u_sampler_int;
		layout(set = 2, binding = 4) uniform usamplerCubeArray u_sampler_uint_array;
		layout(set = 2, binding = 5) uniform image2D u_image2d;
		layout(set = 2, binding = 6) uniform iimage3D u_image3d;
		layout(set = 2, binding = 7) uniform uimageBuffer u_image_buffer;
		void main() {
			vec4 base = texture(u_sampler1d, 0.5);
			vec4 volume = texture(u_sampler3d, vec3(0.0));
			float depth = texture(u_sampler_shadow, vec3(0.0, 0.0, 1.0));
			ivec4 ints = texture(u_sampler_int, vec2(0.0, 0.0));
			uvec4 uints = texture(u_sampler_uint_array, vec4(0.0, 0.0, 1.0, 0.0));
			out_color = vec4(base.rgb + volume.rgb, depth);
		});

const char* snippet_struct_constructor = SPINDLE_SNIPPET(
		struct Inner {
			vec2 coords[2][2];
		};
		struct Outer {
			float weight;
			Inner segments[2];
			float thresholds[2][2];
		};
		layout(location = 0) out vec4 out_color;
		void main() {
			Inner first = Inner(vec2(0.0, 1.0), vec2(2.0, 3.0),
					vec2(4.0, 5.0), vec2(6.0, 7.0));
			Outer combo = Outer(1.0,
					Inner(vec2(0.5, 0.5), vec2(0.75, 0.25),
							vec2(0.125, 0.875), vec2(0.625, 0.375)),
					Inner(vec2(0.25, 0.75), vec2(0.5, 0.5),
							vec2(0.9, 0.1), vec2(0.2, 0.8)),
					0.0, 1.0, 2.0, 3.0);
			out_color = vec4(combo.segments[0].coords[1][1], combo.thresholds[1][0], combo.weight);
		});

const char* snippet_extended_types = SPINDLE_SNIPPET(
		layout(location = 0) out dvec4 out_color;
		void main() {
			dvec2 dv = dvec2(1.0, 2.0);
			dmat2 dm = dmat2(1.0);
			dvec2 transformed = dm * dv;
			dmat2x3 dm2 = dmat2x3(1.0);
			dvec3 projected = dvec3(dm2 * transformed);
			dmat3x2 dm3 = dmat3x2(1.0);
			dvec3 expanded = dvec3(transformed * dm3);
			int64_t big = int64_t(1);
			i64vec2 ivec = i64vec2(big);
			uint64_t ubig = uint64_t(2u);
			u64vec3 uvec = u64vec3(ubig);
			atomic_uint counter = atomic_uint(0u);
			double compare = double(ivec.x < int64_t(uvec.x) ? 1 : 0);
			out_color = dvec4(expanded.xy, projected.x, compare + double(counter == atomic_uint(0u) ? 0 : 1));
		});
#undef SPINDLE_SNIPPET

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
	if (flags & SYM_STORAGE_BUFFER)
	{
		printf("%sbuffer", first ? "" : "|");
		first = 0;
	}
	if (flags & SYM_STORAGE_SHARED)
	{
		printf("%sshared", first ? "" : "|");
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
	if (flags & SYM_QUAL_VOLATILE)
	{
		printf("%svolatile", first ? "" : "|");
		first = 0;
	}
	if (flags & SYM_QUAL_RESTRICT)
	{
		printf("%srestrict", first ? "" : "|");
		first = 0;
	}
	if (flags & SYM_QUAL_READONLY)
	{
		printf("%sreadonly", first ? "" : "|");
		first = 0;
	}
	if (flags & SYM_QUAL_WRITEONLY)
	{
		printf("%swriteonly", first ? "" : "|");
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
			if (inst->is_unsigned_literal)
				printf("u");
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
		case IR_STAGE_LAYOUT_BEGIN:
			dump_storage_flags(inst->storage_flags);
			dump_qualifier_flags(inst->qualifier_flags);
			dump_layout_info(inst->layout_flags, inst->layout_set, inst->layout_binding, inst->layout_location);
			break;
		case IR_STAGE_LAYOUT_IDENTIFIER:
			printf(" %s", inst->str0 ? inst->str0 : "<anon>");
			break;
		case IR_STAGE_LAYOUT_VALUE:
			printf(" %s=%d", inst->str0 ? inst->str0 : "<anon>", inst->arg0);
			break;
		case IR_STAGE_LAYOUT_END:
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
		{ sintern("textureQueryLod"), BUILTIN_TEXTURE_QUERY_LOD, 2 },
		{ sintern("texelFetchOffset"), BUILTIN_TEXEL_FETCH_OFFSET, -1 },
		{ sintern("determinant"), BUILTIN_DETERMINANT, 1 },
		{ sintern("imageAtomicAdd"), BUILTIN_IMAGE_ATOMIC_ADD, -1 },
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

DEFINE_TEST(test_ternary_vector_promotions)
{
	compiler_setup(snippet_ternary_vectors);
	int saw_vec3_select = 0;
	int saw_vec4_select = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op != IR_SELECT)
			continue;
		assert(inst->type);
		assert(type_is_vector(inst->type));
		assert(type_base_type(inst->type) == T_FLOAT);
		if (inst->type->cols == 3)
			saw_vec3_select = 1;
		if (inst->type->cols == 4)
			saw_vec4_select = 1;
	}
	compiler_teardown();
	assert(saw_vec3_select);
	assert(saw_vec4_select);
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

DEFINE_TEST(test_function_redeclaration_without_overloads)
{
	type_system_init_builtins();
	symbol_table_init();
	const char* func_name = sintern("foo");
	const char* float_name = sintern("float");
	Type* float_type = type_get_scalar(T_FLOAT);
	Symbol* first_decl = symbol_table_add(func_name, float_name, float_type, SYM_FUNC);
	assert(first_decl && first_decl->kind == SYM_FUNC);
	dyna Type** params = NULL;
	apush(params, float_type);
	symbol_set_function_signature(first_decl, params, 1);
	Symbol* second_decl = symbol_table_add(func_name, float_name, float_type, SYM_FUNC);
	assert(second_decl == first_decl);
	assert(second_decl->param_signature_set);
	assert(second_decl->param_count == 1);
	if (params)
		afree(params);
	symbol_table_free();
	type_system_free();
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
	int saw_hex_int = 0;
	int saw_bin_int = 0;
	int saw_oct_int = 0;
	int saw_hex_uint = 0;
	int saw_bin_uint = 0;
	int saw_oct_uint = 0;
	int saw_dec_uint = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op != IR_PUSH_INT)
			continue;
		if (inst->is_unsigned_literal)
		{
			if (inst->arg0 == 31)
			{
				saw_hex_uint = inst->type == g_type_uint;
			}
			else if (inst->arg0 == 10)
			{
				saw_bin_uint = inst->type == g_type_uint;
			}
			else if (inst->arg0 == 61)
			{
				saw_oct_uint = inst->type == g_type_uint;
			}
			else if (inst->arg0 == 42)
			{
				saw_dec_uint = inst->type == g_type_uint;
			}
		}
		else
		{
			if (inst->arg0 == 31)
			{
				saw_hex_int = inst->type == g_type_int;
			}
			else if (inst->arg0 == 10)
			{
				saw_bin_int = inst->type == g_type_int;
			}
			else if (inst->arg0 == 61)
			{
				saw_oct_int = inst->type == g_type_int;
			}
		}
	}
	compiler_teardown();
	assert(saw_hex_int);
	assert(saw_bin_int);
	assert(saw_oct_int);
	assert(saw_hex_uint);
	assert(saw_bin_uint);
	assert(saw_oct_uint);
	assert(saw_dec_uint);
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

DEFINE_TEST(test_struct_constructor_ir)
{
	const char* inner_name = sintern("Inner");
	const char* outer_name = sintern("Outer");
	compiler_setup(snippet_struct_constructor);
	int saw_inner_ctor = 0;
	int saw_outer_ctor = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op != IR_CONSTRUCT)
			continue;
		if (!inst->type || inst->type->tag != T_STRUCT)
			continue;
		if (inst->str0 == inner_name && inst->arg0 == 4)
			saw_inner_ctor = 1;
		if (inst->str0 == outer_name && inst->arg0 == 7)
			saw_outer_ctor = 1;
	}
	Type* inner_type = type_system_get(inner_name);
	const char* coords_name = sintern("coords");
	StructMember* coords_member = type_struct_find_member(inner_type, coords_name);
	assert(coords_member);
	assert(coords_member->has_array);
	assert(acount(coords_member->array_dims) == 2);
	assert(coords_member->type && coords_member->type->tag == T_ARRAY);
	Type* coords_inner = coords_member->type->user ? (Type*)coords_member->type->user : NULL;
	assert(coords_inner && coords_inner->tag == T_ARRAY);
	Type* coords_element = coords_inner->user ? (Type*)coords_inner->user : NULL;
	assert(coords_element && coords_element->tag == T_VEC && coords_element->cols == 2);
	Type* outer_type = type_system_get(outer_name);
	const char* thresholds_name = sintern("thresholds");
	StructMember* thresholds_member = type_struct_find_member(outer_type, thresholds_name);
	assert(thresholds_member);
	assert(thresholds_member->has_array);
	assert(acount(thresholds_member->array_dims) == 2);
	assert(thresholds_member->type && thresholds_member->type->tag == T_ARRAY);
	Type* thresholds_inner = thresholds_member->type->user ? (Type*)thresholds_member->type->user : NULL;
	assert(thresholds_inner && thresholds_inner->tag == T_ARRAY);
	Type* thresholds_element = thresholds_inner->user ? (Type*)thresholds_inner->user : NULL;
	assert(thresholds_element && type_base_type(thresholds_element) == T_FLOAT);
	compiler_teardown();
	assert(saw_inner_ctor);
	assert(saw_outer_ctor);
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

DEFINE_TEST(test_texture_query_builtins)
{
	const char* texture_size_name = sintern("textureSize");
	const char* texel_fetch_name = sintern("texelFetch");
	const char* inverse_name = sintern("inverse");
	const char* transpose_name = sintern("transpose");
	const char* less_than_name = sintern("lessThan");
	const char* greater_equal_name = sintern("greaterThanEqual");
	const char* equal_name = sintern("equal");
	const char* not_equal_name = sintern("notEqual");
	const char* any_name = sintern("any");
	const char* all_name = sintern("all");
	compiler_setup(snippet_texture_queries);
	int saw_texture_size = 0;
	int saw_texel_fetch = 0;
	int saw_inverse = 0;
	int saw_transpose = 0;
	int saw_less_than = 0;
	int saw_greater_equal = 0;
	int saw_equal = 0;
	int saw_not_equal = 0;
	int saw_any = 0;
	int saw_all = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op != IR_CALL)
			continue;
		if (inst->str0 == texture_size_name)
			saw_texture_size = 1;
		if (inst->str0 == texel_fetch_name)
			saw_texel_fetch = 1;
		if (inst->str0 == inverse_name)
			saw_inverse = 1;
		if (inst->str0 == transpose_name)
			saw_transpose = 1;
		if (inst->str0 == less_than_name)
			saw_less_than = 1;
		if (inst->str0 == greater_equal_name)
			saw_greater_equal = 1;
		if (inst->str0 == equal_name)
			saw_equal = 1;
		if (inst->str0 == not_equal_name)
			saw_not_equal = 1;
		if (inst->str0 == any_name)
			saw_any = 1;
		if (inst->str0 == all_name)
			saw_all = 1;
	}
	compiler_teardown();
	assert(saw_texture_size);
	assert(saw_texel_fetch);
	assert(saw_inverse);
	assert(saw_transpose);
	assert(saw_less_than);
	assert(saw_greater_equal);
	assert(saw_equal);
	assert(saw_not_equal);
	assert(saw_any);
	assert(saw_all);
}

DEFINE_TEST(test_extended_intrinsic_calls)
{
	const char* determinant_name = sintern("determinant");
	const char* texture_query_lod_name = sintern("textureQueryLod");
	const char* texel_fetch_offset_name = sintern("texelFetchOffset");
	const char* image_atomic_add_name = sintern("imageAtomicAdd");
	const char* image_atomic_comp_swap_name = sintern("imageAtomicCompSwap");
	const char* outer_product_name = sintern("outerProduct");
	const char* fwidth_fine_name = sintern("fwidthFine");
	compiler_setup(snippet_extended_intrinsics);
	int saw_determinant = 0;
	int saw_texture_query_lod = 0;
	int saw_texel_fetch_offset = 0;
	int saw_image_atomic_add = 0;
	int saw_image_atomic_comp_swap = 0;
	int saw_outer_product = 0;
	int saw_fwidth_fine = 0;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op != IR_CALL)
			continue;
		if (inst->str0 == determinant_name)
			saw_determinant = 1;
		if (inst->str0 == texture_query_lod_name)
			saw_texture_query_lod = 1;
		if (inst->str0 == texel_fetch_offset_name)
			saw_texel_fetch_offset = 1;
		if (inst->str0 == image_atomic_add_name)
			saw_image_atomic_add = 1;
		if (inst->str0 == image_atomic_comp_swap_name)
			saw_image_atomic_comp_swap = 1;
		if (inst->str0 == outer_product_name)
			saw_outer_product = 1;
		if (inst->str0 == fwidth_fine_name)
			saw_fwidth_fine = 1;
	}
	compiler_teardown();
	assert(saw_determinant);
	assert(saw_texture_query_lod);
	assert(saw_texel_fetch_offset);
	assert(saw_image_atomic_add);
	assert(saw_image_atomic_comp_swap);
	assert(saw_outer_product);
	assert(saw_fwidth_fine);
}

DEFINE_TEST(test_builtin_variables_vertex_stage)
{
	compiler_set_shader_stage(SHADER_STAGE_VERTEX);
	compiler_setup("void main() { gl_Position = vec4(0.0); }");
	Symbol* gl_position = symbol_table_find(sintern("gl_Position"));
	assert(gl_position);
	assert(gl_position->is_builtin);
	assert(gl_position->builtin_stage == SHADER_STAGE_VERTEX);
	assert(symbol_has_storage(gl_position, SYM_STORAGE_OUT));
	assert(!(gl_position->qualifier_flags & SYM_QUAL_CONST));
	Symbol* clip_distance = symbol_table_find(sintern("gl_ClipDistance"));
	assert(clip_distance);
	assert(symbol_has_storage(clip_distance, SYM_STORAGE_OUT));
	assert(!(clip_distance->qualifier_flags & SYM_QUAL_CONST));
	assert(clip_distance->array_dimensions == 1);
	Symbol* draw_id = symbol_table_find(sintern("gl_DrawID"));
	assert(draw_id);
	assert(symbol_has_storage(draw_id, SYM_STORAGE_IN));
	assert(draw_id->qualifier_flags & SYM_QUAL_CONST);
	Symbol* view_index = symbol_table_find(sintern("gl_ViewIndex"));
	assert(view_index);
	assert(symbol_has_storage(view_index, SYM_STORAGE_IN));
	assert(view_index->qualifier_flags & SYM_QUAL_CONST);
	Symbol* frag_coord = symbol_table_find(sintern("gl_FragCoord"));
	assert(!frag_coord);
	Symbol* sample_id = symbol_table_find(sintern("gl_SampleID"));
	assert(!sample_id);
	Symbol* sample_mask = symbol_table_find(sintern("gl_SampleMask"));
	assert(!sample_mask);
	compiler_teardown();
}

DEFINE_TEST(test_builtin_variables_fragment_stage)
{
	compiler_set_shader_stage(SHADER_STAGE_FRAGMENT);
	compiler_setup("void main() { vec4 coord = gl_FragCoord; gl_FragDepth = coord.x; }");
	Symbol* frag_coord = symbol_table_find(sintern("gl_FragCoord"));
	assert(frag_coord);
	assert(frag_coord->is_builtin);
	assert(frag_coord->builtin_stage == SHADER_STAGE_FRAGMENT);
	assert(symbol_has_storage(frag_coord, SYM_STORAGE_IN));
	assert(frag_coord->qualifier_flags & SYM_QUAL_CONST);
	Symbol* frag_depth = symbol_table_find(sintern("gl_FragDepth"));
	assert(frag_depth);
	assert(symbol_has_storage(frag_depth, SYM_STORAGE_OUT));
	assert(!(frag_depth->qualifier_flags & SYM_QUAL_CONST));
	Symbol* sample_id = symbol_table_find(sintern("gl_SampleID"));
	assert(sample_id);
	assert(symbol_has_storage(sample_id, SYM_STORAGE_IN));
	assert(sample_id->qualifier_flags & SYM_QUAL_CONST);
	Symbol* sample_mask_in = symbol_table_find(sintern("gl_SampleMaskIn"));
	assert(sample_mask_in);
	assert(symbol_has_storage(sample_mask_in, SYM_STORAGE_IN));
	assert(sample_mask_in->qualifier_flags & SYM_QUAL_CONST);
	assert(sample_mask_in->array_dimensions == 1);
	Symbol* sample_mask = symbol_table_find(sintern("gl_SampleMask"));
	assert(sample_mask);
	assert(symbol_has_storage(sample_mask, SYM_STORAGE_OUT));
	assert(!(sample_mask->qualifier_flags & SYM_QUAL_CONST));
	assert(sample_mask->array_dimensions == 1);
	Symbol* clip_distance = symbol_table_find(sintern("gl_ClipDistance"));
	assert(clip_distance);
	assert(symbol_has_storage(clip_distance, SYM_STORAGE_IN));
	assert(clip_distance->qualifier_flags & SYM_QUAL_CONST);
	assert(clip_distance->array_dimensions == 1);
	Symbol* gl_position = symbol_table_find(sintern("gl_Position"));
	assert(!gl_position);
	compiler_teardown();
	compiler_set_shader_stage(SHADER_STAGE_VERTEX);
}

DEFINE_TEST(test_builtin_variables_compute_stage)
{
	compiler_set_shader_stage(SHADER_STAGE_COMPUTE);
	compiler_setup("layout(local_size_x = 1) in; void main() { uvec3 id = gl_GlobalInvocationID; }");
	Symbol* global_id = symbol_table_find(sintern("gl_GlobalInvocationID"));
	assert(global_id);
	assert(symbol_has_storage(global_id, SYM_STORAGE_IN));
	assert(global_id->qualifier_flags & SYM_QUAL_CONST);
	Symbol* local_index = symbol_table_find(sintern("gl_LocalInvocationIndex"));
	assert(local_index);
	assert(symbol_has_storage(local_index, SYM_STORAGE_IN));
	assert(local_index->qualifier_flags & SYM_QUAL_CONST);
	Symbol* workgroup_size = symbol_table_find(sintern("gl_WorkGroupSize"));
	assert(workgroup_size);
	assert(symbol_has_storage(workgroup_size, SYM_STORAGE_IN));
	assert(workgroup_size->qualifier_flags & SYM_QUAL_CONST);
	compiler_teardown();
	compiler_set_shader_stage(SHADER_STAGE_VERTEX);
}

DEFINE_TEST(test_compute_layout_and_storage)
{
	compiler_set_shader_stage(SHADER_STAGE_COMPUTE);
	const char* source =
			"layout(local_size_x = 8, local_size_y = 4, local_size_z = 1) in;"
			"readonly layout(std430, set = 0, binding = 1) buffer Data { float values[]; } data;"
			"shared float tile[64];"
			"void main() { tile[gl_LocalInvocationIndex] = float(gl_GlobalInvocationID.x); }";
	compiler_setup(source);
	int saw_stage_layout = 0;
	int saw_x = 0;
	int saw_y = 0;
	int saw_z = 0;
	const char* local_size_x = sintern("local_size_x");
	const char* local_size_y = sintern("local_size_y");
	const char* local_size_z = sintern("local_size_z");
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		if (inst->op == IR_STAGE_LAYOUT_BEGIN)
		{
			saw_stage_layout = 1;
			continue;
		}
		if (inst->op == IR_STAGE_LAYOUT_VALUE)
		{
			if (inst->str0 == local_size_x && inst->arg0 == 8)
				saw_x = 1;
			if (inst->str0 == local_size_y && inst->arg0 == 4)
				saw_y = 1;
			if (inst->str0 == local_size_z && inst->arg0 == 1)
				saw_z = 1;
		}
	}
	assert(saw_stage_layout);
	assert(saw_x && saw_y && saw_z);
	Symbol* data_block = symbol_table_find(sintern("data"));
	assert(data_block);
	assert(symbol_has_storage(data_block, SYM_STORAGE_BUFFER));
	assert(data_block->qualifier_flags & SYM_QUAL_READONLY);
	Symbol* tile = symbol_table_find(sintern("tile"));
	assert(tile);
	assert(symbol_has_storage(tile, SYM_STORAGE_SHARED));
	assert(tile->qualifier_flags == 0);
	compiler_teardown();
	compiler_set_shader_stage(SHADER_STAGE_VERTEX);
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
		TEST_ENTRY(test_resource_texture_inference),
		TEST_ENTRY(test_ir_emit_push_int),
		TEST_ENTRY(test_basic_io_symbols),
		TEST_ENTRY(test_array_indexing_ir),
		TEST_ENTRY(test_swizzle_ir),
		TEST_ENTRY(test_control_flow_unary_ops),
		TEST_ENTRY(test_ternary_vector_promotions),
		TEST_ENTRY(test_function_call_symbols),
		TEST_ENTRY(test_function_redeclaration_without_overloads),
		TEST_ENTRY(test_matrix_operations_ir),
		TEST_ENTRY(test_looping_constructs),
		TEST_ENTRY(test_bitwise_operations),
		TEST_ENTRY(test_numeric_literal_bases),
		TEST_ENTRY(test_discard_instruction),
		TEST_ENTRY(test_struct_block_layout),
		TEST_ENTRY(test_struct_constructor_ir),
		TEST_ENTRY(test_switch_statement_cases),
		TEST_ENTRY(test_builtin_function_calls),
		TEST_ENTRY(test_texture_query_builtins),
		TEST_ENTRY(test_extended_intrinsic_calls),
		TEST_ENTRY(test_builtin_variables_vertex_stage),
		TEST_ENTRY(test_builtin_variables_fragment_stage),
		TEST_ENTRY(test_builtin_variables_compute_stage),
		TEST_ENTRY(test_compute_layout_and_storage),
		TEST_ENTRY(test_preprocessor_passthrough),
		TEST_ENTRY(test_const_qualifier_metadata),
	};
	run_tests(tests, (int)(sizeof(tests) / sizeof(tests[0])));
}

#endif /* SPINDLE_IMPLEMENTATION */
