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

void type_struct_clear(Type* type)
{
	StructInfo* info = type_struct_info_ensure(type);
	if (!info)
		return;
	info->name = type ? type->name : NULL;
	if (info->members)
		aclear(info->members);
	if (info->layout_identifiers)
		aclear(info->layout_identifiers);
}

StructMember* type_struct_add_member(Type* type, const char* name, Type* member_type)
{
	StructInfo* info = type_struct_info_ensure(type);
	if (!info)
		return NULL;
	StructMember member = (StructMember){ 0 };
	member.name = name;
	member.declared_type = member_type;
	member.type = member_type;
	member.array_type = (Type){ 0 };
	member.has_array = 0;
	member.array_len = 0;
	member.array_unsized = 0;
	apush(info->members, member);
	return &alast(info->members);
}

void type_struct_member_set_layout(StructMember* member, unsigned layout_flags, int set, int binding, int location)
{
	if (!member)
		return;
	member->layout_flags = layout_flags;
	member->layout_set = set;
	member->layout_binding = binding;
	member->layout_location = location;
}

void type_struct_member_mark_array(StructMember* member, Type* element_type, int size, int unsized)
{
	if (!member)
		return;
	member->has_array = 1;
	member->array_len = size;
	member->array_unsized = unsized;
	member->array_type = (Type){ 0 };
	member->array_type.tag = T_ARRAY;
	member->array_type.base = element_type ? element_type->tag : T_VOID;
	member->array_type.cols = element_type ? element_type->cols : 1;
	member->array_type.rows = element_type ? element_type->rows : 1;
	member->array_type.array_len = unsized ? -1 : size;
	member->array_type.user = element_type;
	member->array_type.name = NULL;
	member->type = &member->array_type;
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

void type_system_init_builtins()
{
	typedef struct TypeInit
	{
		const char* name;
		Type type;
	} TypeInit;
#define SAMPLER(name, base_tag, dim_value) \
	{ \
		name, { .tag = T_SAMPLER, .cols = 1, .rows = 1, .base = base_tag, .dim = dim_value, .array_len = 0 } \
	}
#define IMAGE(name, base_tag, dim_value) \
	{ \
		name, { .tag = T_IMAGE, .cols = 1, .rows = 1, .base = base_tag, .dim = dim_value, .array_len = 0 } \
	}
	const TypeInit builtins[] = {
		{ "void", { .tag = T_VOID, .cols = 1, .rows = 1, .base = T_VOID, .array_len = 0 } },
		{ "bool", { .tag = T_BOOL, .cols = 1, .rows = 1, .base = T_BOOL, .array_len = 0 } },
		{ "int", { .tag = T_INT, .cols = 1, .rows = 1, .base = T_INT, .array_len = 0 } },
		{ "uint", { .tag = T_UINT, .cols = 1, .rows = 1, .base = T_UINT, .array_len = 0 } },
		{ "float", { .tag = T_FLOAT, .cols = 1, .rows = 1, .base = T_FLOAT, .array_len = 0 } },
		{ "double", { .tag = T_DOUBLE, .cols = 1, .rows = 1, .base = T_DOUBLE, .array_len = 0 } },
		{ "vec2", { .tag = T_VEC, .cols = 2, .rows = 1, .base = T_FLOAT, .array_len = 0 } },
		{ "vec3", { .tag = T_VEC, .cols = 3, .rows = 1, .base = T_FLOAT, .array_len = 0 } },
		{ "vec4", { .tag = T_VEC, .cols = 4, .rows = 1, .base = T_FLOAT, .array_len = 0 } },
		{ "ivec2", { .tag = T_VEC, .cols = 2, .rows = 1, .base = T_INT, .array_len = 0 } },
		{ "ivec3", { .tag = T_VEC, .cols = 3, .rows = 1, .base = T_INT, .array_len = 0 } },
		{ "ivec4", { .tag = T_VEC, .cols = 4, .rows = 1, .base = T_INT, .array_len = 0 } },
		{ "uvec2", { .tag = T_VEC, .cols = 2, .rows = 1, .base = T_UINT, .array_len = 0 } },
		{ "uvec3", { .tag = T_VEC, .cols = 3, .rows = 1, .base = T_UINT, .array_len = 0 } },
		{ "uvec4", { .tag = T_VEC, .cols = 4, .rows = 1, .base = T_UINT, .array_len = 0 } },
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
					afree(info->members);
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
	case T_FLOAT:
	case T_DOUBLE:
		return 1;
	default:
		return 0;
	}
}

int type_base_is_integer(TypeTag tag)
{
	return tag == T_INT || tag == T_UINT;
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

static Type* builtin_result_same(Type** args, int argc, int index)
{
	if (index < 0 || index >= argc)
		return NULL;
	return args[index];
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

Type* type_infer_builtin_call(const Symbol* sym, Type** args, int argc)
{
	if (!sym)
		return NULL;
	switch (sym->builtin_kind)
	{
	case BUILTIN_TEXTURE:
	case BUILTIN_TEXTURE_LOD:
	case BUILTIN_TEXTURE_PROJ:
	case BUILTIN_TEXTURE_GRAD:
		return builtin_result_texture(args, argc);
	case BUILTIN_MIN:
	case BUILTIN_MAX:
	case BUILTIN_CLAMP:
	case BUILTIN_ABS:
	case BUILTIN_FLOOR:
	case BUILTIN_CEIL:
	case BUILTIN_FRACT:
	case BUILTIN_MIX:
	case BUILTIN_SIGN:
	case BUILTIN_TRUNC:
	case BUILTIN_ROUND:
	case BUILTIN_ROUND_EVEN:
	case BUILTIN_POW:
	case BUILTIN_EXP:
	case BUILTIN_EXP2:
	case BUILTIN_LOG:
	case BUILTIN_LOG2:
	case BUILTIN_SQRT:
	case BUILTIN_INVERSE_SQRT:
	case BUILTIN_MOD:
	case BUILTIN_SIN:
	case BUILTIN_COS:
	case BUILTIN_TAN:
	case BUILTIN_ASIN:
	case BUILTIN_ACOS:
	case BUILTIN_ATAN:
	case BUILTIN_NORMALIZE:
	case BUILTIN_REFLECT:
	case BUILTIN_REFRACT:
		return builtin_result_same(args, argc, 0);
	case BUILTIN_STEP:
		return builtin_result_same(args, argc, 1);
	case BUILTIN_SMOOTHSTEP:
		return builtin_result_same(args, argc, 2);
	case BUILTIN_LENGTH:
	case BUILTIN_DISTANCE:
	case BUILTIN_DOT:
	case BUILTIN_FWIDTH:
	case BUILTIN_DFDX:
	case BUILTIN_DFDY:
		return builtin_result_scalar(args, argc, 0);
	case BUILTIN_CROSS:
		return builtin_result_vector(args, argc, 0, 3);
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
	if (type_is_matrix(lhs) && type_is_matrix(rhs))
	{
		if (lhs->cols != rhs->cols || lhs->rows != rhs->rows)
		{
			type_check_error("matrix multiplication currently requires matching dimensions");
		}
		return lhs;
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
	if (!type_equal(true_type, false_type))
	{
		type_check_error("ternary branches must match types, got %s and %s", type_display(true_type), type_display(false_type));
	}
	return true_type;
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
	TypeTag base = type_base_type(target);
	switch (target->tag)
	{
	case T_VEC:
	{
		int needed = target->cols;
		int count = 0;
		for (int i = 0; i < argc; ++i)
		{
			Type* arg = args[i];
			if (!type_base_can_convert(type_base_type(arg), base))
			{
				type_check_error("cannot pass %s to constructor %s", type_display(arg), type_display(target));
			}
			if (type_is_scalar(arg))
				count += 1;
			else if (type_is_vector(arg))
				count += arg->cols;
			else
				type_check_error("vector constructor arguments must be scalar or vector, got %s", type_display(arg));
		}
		if (count != needed)
		{
			int allow_scalar_replication = (needed > 1 && count == 1 && argc == 1 && type_is_scalar(args[0]));
			if (!allow_scalar_replication)
			{
				type_check_error("constructor %s expected %d components but received %d", type_display(target), needed, count);
			}
		}
		break;
	}
	case T_MAT:
	{
		int needed = target->cols * target->rows;
		int count = 0;
		for (int i = 0; i < argc; ++i)
		{
			Type* arg = args[i];
			if (!type_base_can_convert(type_base_type(arg), base))
			{
				type_check_error("cannot pass %s to constructor %s", type_display(arg), type_display(target));
			}
			if (type_is_scalar(arg))
			{
				count += 1;
			}
			else if (type_is_vector(arg))
			{
				if (arg->cols != target->rows)
				{
					type_check_error("matrix constructor column argument expected %d components, got %d", target->rows, arg->cols);
				}
				count += arg->cols;
			}
			else
			{
				type_check_error("matrix constructor arguments must be scalars or column vectors, got %s", type_display(arg));
			}
		}
		if (count != needed)
		{
			int allow_scalar_replication = (needed > 1 && count == 1 && argc == 1 && type_is_scalar(args[0]));
			if (!allow_scalar_replication)
			{
				type_check_error("constructor %s expected %d components but received %d", type_display(target), needed, count);
			}
		}
		break;
	}
	default:
		if (!type_is_scalar(target))
		{
			type_check_error("unsupported constructor target %s", type_display(target));
		}
		if (argc != 1)
		{
			type_check_error("scalar constructor %s expects 1 argument, got %d", type_display(target), argc);
		}
		if (!type_scalar_can_convert(args[0], target))
		{
			type_check_error("cannot convert %s to %s", type_display(args[0]), type_display(target));
		}
		break;
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

static Type* type_stack_pop_checked(dyna Type*** stack_ref, dyna int** index_ref, const char* context, int* out_index)
{
	dyna Type** stack = stack_ref ? *stack_ref : NULL;
	if (!stack || acount(stack) == 0)
		type_check_error("missing operand for %s", context);
	Type* value = apop(stack);
	if (stack_ref)
		*stack_ref = stack;
	if (index_ref)
	{
		dyna int* indices = *index_ref;
		if (!indices || acount(indices) == 0)
			type_check_error("internal error: missing expression index for %s", context);
		int idx = apop(indices);
		*index_ref = indices;
		if (out_index)
			*out_index = idx;
	}
	else if (out_index)
	{
		*out_index = -1;
	}
	return value;
}

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
	dyna int* stack_indices = NULL;
	dyna Type** func_stack = NULL;
	dyna TypeCheckSwitch* switch_stack = NULL;
	Type* current_decl_type = NULL;
	for (int i = 0; i < acount(g_ir); ++i)
	{
		IR_Cmd* inst = &g_ir[i];
		switch (inst->op)
		{
		case IR_PUSH_INT:
			inst->type = g_type_int;
			apush(stack, inst->type);
			apush(stack_indices, i);
			break;
		case IR_PUSH_FLOAT:
			inst->type = g_type_float;
			apush(stack, inst->type);
			apush(stack_indices, i);
			break;
		case IR_PUSH_BOOL:
			inst->type = g_type_bool;
			apush(stack, inst->type);
			apush(stack_indices, i);
			break;
		case IR_PUSH_IDENT:
		{
			Type* type = NULL;
			if (inst->str0)
			{
				Symbol* sym = symbol_table_find(inst->str0);
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
			inst->type = type;
			apush(stack, type);
			apush(stack_indices, i);
			break;
		}
		case IR_UNARY:
		{
			Type* operand = type_stack_pop_checked(&stack, &stack_indices, "unary expression", NULL);
			Type* result = type_check_unary(inst, operand);
			if (!result)
				result = operand;
			inst->type = result;
			apush(stack, result);
			apush(stack_indices, i);
			break;
		}
		case IR_BINARY:
		{
			Type* rhs = type_stack_pop_checked(&stack, &stack_indices, "binary rhs", NULL);
			Type* lhs = type_stack_pop_checked(&stack, &stack_indices, "binary lhs", NULL);
			Type* result = type_check_binary(inst->tok, lhs, rhs);
			if (!result)
				result = lhs ? lhs : rhs;
			inst->type = result;
			apush(stack, result);
			apush(stack_indices, i);
			break;
		}
		case IR_CALL:
		{
			Type** args = NULL;
			dyna int* arg_indices = NULL;
			for (int arg = 0; arg < inst->arg0; ++arg)
			{
				int arg_value_index = -1;
				Type* arg_type = type_stack_pop_checked(&stack, &stack_indices, "call argument", &arg_value_index);
				apush(args, arg_type);
				apush(arg_indices, arg_value_index);
			}
			int argc = acount(args);
			for (int l = 0, r = argc - 1; l < r; ++l, --r)
			{
				Type* tmp = args[l];
				args[l] = args[r];
				args[r] = tmp;
				int idx_tmp = arg_indices[l];
				arg_indices[l] = arg_indices[r];
				arg_indices[r] = idx_tmp;
			}
			Type* callee = type_stack_pop_checked(&stack, &stack_indices, "call target", NULL);
			Type* result = callee;
			if (inst->str0)
			{
				Symbol* sym = symbol_table_find(inst->str0);
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
							unsigned param_flags = (sym->param_storage_flags && i < acount(sym->param_storage_flags)) ? sym->param_storage_flags[i] : 0;
							param_flags = storage_flags_default_in(param_flags);
							int arg_value_index = (arg_indices && i < acount(arg_indices)) ? arg_indices[i] : -1;
							if ((param_flags & SYM_STORAGE_IN) && param_type && arg_type)
							{
								if (!type_can_assign(param_type, arg_type))
								{
									type_check_error("argument %d to %s expects %s but got %s", i + 1, inst->str0, type_display(param_type), type_display(arg_type));
								}
							}
							if (param_flags & SYM_STORAGE_OUT)
							{
								if (arg_value_index < 0 || arg_value_index >= acount(g_ir) || !g_ir[arg_value_index].is_lvalue)
									type_check_error("argument %d to %s must be an l-value", i + 1, inst->str0);
								if (param_type && arg_type && !type_can_assign(arg_type, param_type))
								{
									type_check_error("argument %d to %s expects assignable storage of %s but got %s", i + 1, inst->str0, type_display(param_type), type_display(arg_type));
								}
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
			}
			inst->type = result;
			afree(args);
			afree(arg_indices);
			apush(stack, result);
			apush(stack_indices, i);
			break;
		}
		case IR_CONSTRUCT:
		{
			Type** args = NULL;
			for (int arg = 0; arg < inst->arg0; ++arg)
			{
				Type* arg_type = type_stack_pop_checked(&stack, &stack_indices, "constructor argument", NULL);
				apush(args, arg_type);
			}
			int argc = acount(args);
			for (int l = 0, r = argc - 1; l < r; ++l, --r)
			{
				Type* tmp = args[l];
				args[l] = args[r];
				args[r] = tmp;
			}
			Type* target = type_stack_pop_checked(&stack, &stack_indices, "constructor target", NULL);
			Type* result = inst->type ? inst->type : target;
			inst->type = result;
			if (result)
				type_check_constructor(result, args, argc);
			afree(args);
			apush(stack, result);
			apush(stack_indices, i);
			break;
		}
		case IR_INDEX:
		{
			Type* index = type_stack_pop_checked(&stack, &stack_indices, "index expression", NULL);
			Type* base = type_stack_pop_checked(&stack, &stack_indices, "index base", NULL);
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
			apush(stack, result);
			apush(stack_indices, i);
			break;
		}
		case IR_SWIZZLE:
		{
			Type* base = type_stack_pop_checked(&stack, &stack_indices, "swizzle base", NULL);
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
			apush(stack, result);
			apush(stack_indices, i);
			break;
		}
		case IR_MEMBER:
		{
			Type* base = type_stack_pop_checked(&stack, &stack_indices, "member access", NULL);
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
			apush(stack, result);
			apush(stack_indices, i);
			break;
		}
		case IR_SELECT:
		{
			Type* false_type = type_stack_pop_checked(&stack, &stack_indices, "ternary false branch", NULL);
			Type* true_type = type_stack_pop_checked(&stack, &stack_indices, "ternary true branch", NULL);
			Type* cond_type = type_stack_pop_checked(&stack, &stack_indices, "ternary condition", NULL);
			Type* result = type_select_result(cond_type, true_type, false_type);
			if (!result)
				result = true_type ? true_type : false_type;
			inst->type = result;
			apush(stack, result);
			apush(stack_indices, i);
			break;
		}
		case IR_DECL_TYPE:
			current_decl_type = type_system_get(inst->str0);
			inst->type = current_decl_type;
			break;
		case IR_DECL_END:
			current_decl_type = NULL;
			break;
		case IR_DECL_INIT_END:
			if (acount(stack) > 0)
			{
				Type* value = type_stack_pop_checked(&stack, &stack_indices, "initializer", NULL);
				if (current_decl_type && value && !type_can_assign(current_decl_type, value))
				{
					type_check_error("initializer type %s cannot initialize %s", type_display(value), type_display(current_decl_type));
				}
			}
			if (acount(stack) > 0)
				aclear(stack);
			if (acount(stack_indices) > 0)
				aclear(stack_indices);
			break;
		case IR_DECL_ARRAY_SIZE_END:
		case IR_FUNC_PARAM_ARRAY_SIZE_END:
			if (acount(stack) > 0)
			{
				Type* size = type_stack_pop_checked(&stack, &stack_indices, "array size", NULL);
				if (size && (!type_is_scalar(size) || !type_is_integer(size)))
				{
					type_check_error("array size must be integer scalar, got %s", type_display(size));
				}
			}
			if (acount(stack) > 0)
				aclear(stack);
			if (acount(stack_indices) > 0)
				aclear(stack_indices);
			break;
		case IR_SWITCH_BEGIN:
			apush(switch_stack, (TypeCheckSwitch){ 0 });
			break;
		case IR_SWITCH_SELECTOR_BEGIN:
			break;
		case IR_SWITCH_SELECTOR_END:
		{
			Type* selector = type_stack_pop_checked(&stack, &stack_indices, "switch selector", NULL);
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
				type_stack_pop_checked(&stack, &stack_indices, "expression result", NULL);
			if (acount(stack) > 0)
				aclear(stack);
			if (acount(stack_indices) > 0)
				aclear(stack_indices);
			break;
		case IR_IF_THEN:
		{
			Type* cond = type_stack_pop_checked(&stack, &stack_indices, "if condition", NULL);
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
				Type* value = type_stack_pop_checked(&stack, &stack_indices, "return value", NULL);
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
	afree(stack_indices);
	afree(stack);
	afree(func_stack);
}
