#define _CRT_SECURE_NO_WARNINGS
#define _CRT_SECURE_NO_DEPRECATE

#include <stddef.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdarg.h>

#define CKIT_IMPLEMENTATION
#include "ckit.h"

#define STR(...) #__VA_ARGS__

typedef enum Tok
{
	TOK_EOF,
	TOK_IDENTIFIER,
	TOK_INT,
	TOK_FLOAT,
	TOK_BOOL,

	TOK_LPAREN,
	TOK_RPAREN,
	TOK_LBRACK,
	TOK_RBRACK,
	TOK_LBRACE,
	TOK_RBRACE,
	TOK_DOT,
	TOK_COMMA,
	TOK_SEMI,
	TOK_QUESTION,
	TOK_COLON,

	TOK_IF,
	TOK_ELSE,
	TOK_FOR,
	TOK_WHILE,
	TOK_DO,
	TOK_RETURN,
	TOK_BREAK,
	TOK_CONTINUE,
	TOK_DISCARD,
	TOK_SWITCH,
	TOK_CASE,
	TOK_DEFAULT,

	TOK_PLUS,
	TOK_MINUS,
	TOK_STAR,
	TOK_SLASH,
	TOK_PERCENT,
	TOK_PLUS_PLUS,
	TOK_MINUS_MINUS,
	TOK_NOT,
	TOK_TILDE,
	TOK_LT,
	TOK_LE,
	TOK_GT,
	TOK_GE,
	TOK_EQ,
	TOK_NE,
	TOK_AND_AND,
	TOK_OR_OR,
	TOK_AMP,
	TOK_PIPE,
	TOK_CARET,
	TOK_LSHIFT,
	TOK_RSHIFT,
	TOK_ASSIGN,
	TOK_PLUS_ASSIGN,
	TOK_MINUS_ASSIGN,
	TOK_STAR_ASSIGN,
	TOK_SLASH_ASSIGN,
	TOK_PERCENT_ASSIGN,
	TOK_AND_ASSIGN,
	TOK_OR_ASSIGN,
	TOK_XOR_ASSIGN,
	TOK_LSHIFT_ASSIGN,
	TOK_RSHIFT_ASSIGN,

	TOK_COUNT
} Tok;

const char* tok_name[TOK_COUNT] = {
	[TOK_EOF] = "EOF",
	[TOK_IDENTIFIER] = "IDENT",
	[TOK_INT] = "INT",
	[TOK_FLOAT] = "FLOAT",
	[TOK_BOOL] = "BOOL",

	[TOK_LPAREN] = "(",
	[TOK_RPAREN] = ")",
	[TOK_LBRACK] = "[",
	[TOK_RBRACK] = "]",
	[TOK_LBRACE] = "{",
	[TOK_RBRACE] = "}",
	[TOK_DOT] = ".",
	[TOK_COMMA] = ",",
	[TOK_SEMI] = ";",
	[TOK_QUESTION] = "?",
	[TOK_COLON] = ":",

	[TOK_IF] = "if",
	[TOK_ELSE] = "else",
	[TOK_FOR] = "for",
	[TOK_WHILE] = "while",
	[TOK_DO] = "do",
	[TOK_RETURN] = "return",
	[TOK_BREAK] = "break",
	[TOK_CONTINUE] = "continue",
	[TOK_DISCARD] = "discard",
	[TOK_SWITCH] = "switch",
	[TOK_CASE] = "case",
	[TOK_DEFAULT] = "default",

	[TOK_PLUS] = "+",
	[TOK_MINUS] = "-",
	[TOK_STAR] = "*",
	[TOK_SLASH] = "/",
	[TOK_PERCENT] = "%",

	[TOK_PLUS_PLUS] = "++",
	[TOK_MINUS_MINUS] = "--",

	[TOK_NOT] = "!",
	[TOK_TILDE] = "~",

	[TOK_LT] = "<",
	[TOK_LE] = "<=",
	[TOK_GT] = ">",
	[TOK_GE] = ">=",
	[TOK_EQ] = "==",
	[TOK_NE] = "!=",

	[TOK_AND_AND] = "&&",
	[TOK_OR_OR] = "||",
	[TOK_AMP] = "&",
	[TOK_PIPE] = "|",
	[TOK_CARET] = "^",
	[TOK_LSHIFT] = "<<",
	[TOK_RSHIFT] = ">>",

	[TOK_ASSIGN] = "=",
	[TOK_PLUS_ASSIGN] = "+=",
	[TOK_MINUS_ASSIGN] = "-=",
	[TOK_STAR_ASSIGN] = "*=",
	[TOK_SLASH_ASSIGN] = "/=",
	[TOK_PERCENT_ASSIGN] = "%=",
	[TOK_AND_ASSIGN] = "&=",
	[TOK_OR_ASSIGN] = "|=",
	[TOK_XOR_ASSIGN] = "^=",
	[TOK_LSHIFT_ASSIGN] = "<<=",
	[TOK_RSHIFT_ASSIGN] = ">>=",
};

typedef enum SymbolKind
{
	SYM_VAR,
	SYM_FUNC,
	SYM_PARAM,
	SYM_BLOCK,
	SYM_KIND_COUNT
} SymbolKind;

const char* symbol_kind_name[SYM_KIND_COUNT] = {
	[SYM_VAR] = "var",
	[SYM_FUNC] = "func",
	[SYM_PARAM] = "param",
	[SYM_BLOCK] = "block",
};

typedef enum ShaderStage
{
	SHADER_STAGE_VERTEX,
	SHADER_STAGE_FRAGMENT,
	SHADER_STAGE_COUNT
} ShaderStage;

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

typedef struct StructMember
{
	const char* name;
	Type* declared_type;
	Type* type;
	Type array_type;
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
void type_struct_member_mark_array(StructMember* member, Type* element_type, int size, int unsized);
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
	BUILTIN_DFDY,
	BUILTIN_FWIDTH,
	BUILTIN_TEXTURE_SIZE,
	BUILTIN_TEXEL_FETCH,
	BUILTIN_INVERSE,
	BUILTIN_TRANSPOSE,
	BUILTIN_LESS_THAN,
	BUILTIN_LESS_THAN_EQUAL,
	BUILTIN_GREATER_THAN,
	BUILTIN_GREATER_THAN_EQUAL,
	BUILTIN_EQUAL,
	BUILTIN_NOT_EQUAL,
	BUILTIN_ANY,
	BUILTIN_ALL
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
	StructInfo* struct_info;
	int has_struct_definition;
	int is_interface_block;
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
Type* g_type_float;
Type* g_type_double;

// Any forward declarations we need tend to just get placed here as a simple way to
// maintain a unity build.
void type_check_error(const char* fmt, ...);
int type_equal(const Type* a, const Type* b);
const char* type_display(const Type* type);
Type* type_system_get(const char* name);
void type_system_free();
void type_system_init_builtins();
void type_system_init_builtins();
Type* type_get_scalar(TypeTag base);
Type* type_get_vector(TypeTag base, int cols);
Type* type_get_matrix(TypeTag base, int cols, int rows);
Type* type_check_unary(const IR_Cmd* inst, Type* operand);
void type_check_ir();
Type* type_infer_builtin_call(const Symbol* sym, Type** args, int argc);
void dump_ir();
void dump_symbols();

const char* snippet_basic_io = STR(
		layout(location = 0) in vec3 in_pos;
		layout(location = 1) in vec2 in_uv;
		layout(location = 0) out vec4 out_color;
		layout(set = 0, binding = 0) uniform sampler2D u_texture;
		layout(set = 1, binding = 0) uniform vec4 u_tint;
		void main() {
			vec4 sampled = texture(u_texture, in_uv);
			out_color = sampled * u_tint;
		});

const char* snippet_control_flow = STR(
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

const char* snippet_array_indexing = STR(
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

const char* snippet_swizzle = STR(
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

const char* snippet_function_calls = STR(
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

const char* snippet_matrix_ops = STR(
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

const char* snippet_struct_block = STR(
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

const char* snippet_looping = STR(
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

const char* snippet_bitwise = STR(
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

const char* snippet_numeric_literals = STR(
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

const char* snippet_switch_stmt = STR(
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

const char* snippet_discard = STR(
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

const char* snippet_builtin_funcs = STR(
		layout(location = 0) out vec4 out_color;
		layout(set = 0, binding = 0) uniform sampler2D u_tex;
		void main() {
			vec2 uv = clamp(vec2(0.2, 0.8), vec2(0.0), vec2(1.0));
			vec4 sampled = texture(u_tex, uv);
			float m = max(sampled.x, sampled.y);
			float f = frac(m);
			float shade = dot(sampled.rgb, vec3(0.299, 0.587, 0.114));
			vec3 unit = normalize(sampled.rgb);
			out_color = vec4(unit * (f + shade), sampled.a);
		});

const char* snippet_texture_queries = STR(
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

const char* snippet_const_qualifier = STR(
		layout(location = 0) out vec4 out_color;
		void main() {
			const float factor = 0.5;
			float mutable_val = 1.0;
			mutable_val = mutable_val + factor;
			out_color = vec4(mutable_val);
		});

const char* snippet_resource_types = STR(
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

const char* snippet_struct_constructor = STR(
		struct Inner {
			vec2 coords[2];
		};
		struct Outer {
			float weight;
			Inner segments[2];
			float thresholds[4];
		};
		layout(location = 0) out vec4 out_color;
		void main() {
			Inner first = Inner(vec2(0.0, 1.0), vec2(2.0, 3.0));
			Outer combo = Outer(1.0,
					Inner(vec2(0.5, 0.5), vec2(0.75, 0.25)),
					Inner(vec2(0.25, 0.75), vec2(0.5, 0.5)),
					0.0, 1.0, 2.0, 3.0);
			out_color = vec4(combo.segments[0].coords[1], combo.thresholds[3], combo.weight);
		});

// Directly include all of our source for a unity build.
#include "lex_parse.c"
#include "type.c"
#include "testing.c"

void transpile(const char* source)
{
	printf("Input : %s\n\n", source);
	compiler_set_shader_stage(SHADER_STAGE_VERTEX);
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
		const char* source;
	} ShaderSnippet;

	const ShaderSnippet snippets[] = {
		{ "basic_io", snippet_basic_io },
		{ "control_flow", snippet_control_flow },
		{ "array_indexing", snippet_array_indexing },
		{ "swizzle_usage", snippet_swizzle },
		{ "function_calls", snippet_function_calls },
		{ "matrix_ops", snippet_matrix_ops },
		{ "looping", snippet_looping },
		{ "bitwise", snippet_bitwise },
		{ "numeric_literals", snippet_numeric_literals },
		{ "discard", snippet_discard },
		{ "switch", snippet_switch_stmt },
		{ "builtin_funcs", snippet_builtin_funcs },
		{ "texture_queries", snippet_texture_queries },
		{ "const_qualifier", snippet_const_qualifier },
		{ "resource_types", snippet_resource_types },
		{ "struct_block", snippet_struct_block },
		{ "struct_constructor", snippet_struct_constructor },
		{ "preprocessor_passthrough", snippet_preprocessor_passthrough },
	};

	for (int i = 0; i < (int)(sizeof(snippets) / sizeof(snippets[0])); ++i)
	{
		printf("=== %s ===\n", snippets[i].name);
		transpile(snippets[i].source);
		printf("\n");
	}
	return 0;
}
