/*
    ckit — A tiny, single-header kit of high-performance C essentials.

    In exactly one .c/.cpp file:
           #define CKIT_IMPLEMENTATION
           #include "ckit.h"
       In all other cases include as-expected:
           #include "ckit.h"

    QUICK START GUIDE

        1) Dynamic arrays:
            int* a = NULL;
            for (int i = 0; i < 10; ++i) {
                apush(a, i);
            }
            for (int i = 0; i < 10; ++i) {
                printf("%d\n", a[i]);
            }
            printf("len=%d cap=%d\n", acount(a), acap(a));
            afree(a);

        2) Map (hashtable):
            Map m = (Map){ 0 };
            for (int i = 0; i < 10; ++i)
                map_add(m, i, i * 10);
            for (int i = 0; i < 10; ++i)
                printf("k : %d, v %d\n", (int)i, (int)map_get(m, i));
            map_free(m);

        3) String interning:
           const char *a = sintern("hello");
           const char *b = sintern("he" "llo");
           assert(a == b);

    Public domain - Do whatever you want with this code.
*/

#ifndef CKIT_H
#define CKIT_H

#include <stdint.h>

//--------------------------------------------------------------------------------------------------
// Dynamic arrays.
//
// Example:
// 
//    int* a = NULL;
//    for (int i = 0; i < 10; ++i) {
//        apush(a, i);
//    }
//    for (int i = 0; i < 10; ++i) {
//        printf("%d\n", a[i]);
//    }
//    printf("len=%d cap=%d\n", acount(a), acap(a));
//    afree(a);

#define asize(a)      (a ? AHDR(a)->size : 0)
#define acount(a)     asize(a)
#define acap(a)       ((a) ? AHDR(a)->capacity : 0)
#define afit(a, n)    ((n) <= acap(a) ? 0 : (*(void**)&(a) = agrow((a), (n), sizeof(*a))))
#define apush(a, ...) (ACANARY(a), afit((a), 1 + ((a) ? asize(a) : 0)), (a)[AHDR(a)->size++] = (__VA_ARGS__))
#define apop(a)       (a[--AHDR(a)->size])
#define aend(a)       (a + asize(a))
#define alast(a)      (a[asize(a) - 1])
#define aclear(a)     (ACANARY(a), (a) ? AHDR(a)->size = 0 : 0)
#define adel(a, i)    (a[i] = a[--AHDR(a)->size])
#define afree(a)      do { ACANARY(a); free(AHDR(a)); (a) = NULL; } while (0)

//--------------------------------------------------------------------------------------------------
// Map (hashtable) for uint64_t key <-> val mapping.
// Keys and values are *only* 64-bit values to keep things simple.
// You may pass in map by value or pointer semantics.
//
// Note: You can iterate over keys/vals as dense arrays.
//
// Example:
// 
//     Map m = (Map){ 0 };
//     for (int i = 0; i < 10; ++i)
//         map_add(m, i, i * 10);
//     for (int i = 0; i < 10; ++i)
//         printf("k : %d, v %d\n", (int)i, (int)map_get(m, i));
//     map_free(m);

typedef struct MapSlot
{
	uint64_t h;
	int item_index;
	int base_count;
} MapSlot;

typedef struct Map
{
	uint64_t* keys;
	uint64_t* vals;
	int size;
	int capacity;

	// Private members.
	int *islot;
	MapSlot* slots;
	int slot_count;
	int slot_capacity;
} Map;

#define map_get(m, k)     map_get_impl(map_ref(m), (uint64_t)(k))
#define map_add(m, k, v)  map_add_impl(map_ref(m), (uint64_t)(k), (uint64_t)(v))
#define map_del(m, k)     map_del_impl(map_ref(m), (uint64_t)(k))
#define map_clear(m)      map_clear_impl(map_ref(m))
#define map_free(m)       map_free_impl(map_ref(m))

// Sorts m->keys as an array of strings lexicographically.
#define map_ssort(m, ignore_case) map_ssort_impl(map_ref(m), ignore_case)

// You can iterate over keys/vals as dense arrays. You may also swap elements in keys/vals
// arrays freely using `map_swap(m, i, j)`. This is nice for implementing your own sorting on
// these arrays, such as sorting the keys as lexicographic strings.
#define map_swap(m, key_index, val_index) map_swap_impl(map_ref(m), key_index, val_index)

//--------------------------------------------------------------------------------------------------
// String interning.
//
// What:
// - Create a unique, stable pointer for each unique string contents.
// 
// Why:
// - For intern'd strings you may compare by pointer directly, instead of by string contents.
// - You can put these directly into hash tables as keys by cast to uint64_t.
// - Reduce RAM consumption by compressing redundant strings down to a single copy.
//
// Example:
// 
//    const char* a = sintern("hello");
//    const char* b = sintern("he" "llo");
//    assert(a == b);
//    const char* c = sintern("world");
//    assert(a != c);

#define sintern(s) str_intern_range(s, s + strlen(s))
#define sintern_range(start, end) str_intern_range(start, end)

// Frees all memory used by string interning so far. All prior strings are now invalid.
void sintern_nuke();

//--------------------------------------------------------------------------------------------------
// "Private" implementation details (not intended as public API).

#define AHDR(a) ((ArrayHeader*)a - 1)
#define ACOOKIE 0xE6F7E359

// If you see this macro asserting it likely means you didn't pass in a proper dynamic array.
#define ACANARY(a) ((a) ? assert(AHDR(a)->cookie == ACOOKIE) : (void)0)

// *Hidden* array header behind the user pointer.
typedef struct ArrayHeader
{
	int size;
	int capacity;
	char* data;
	uint32_t cookie;
} ArrayHeader;

void* agrow(const void* a, int new_size, size_t element_size);

#ifdef __cplusplus
	inline Map* map_ref(Map& m)  { return &m; }
	inline Map* map_ref(Map* mp) { return mp; }
#else
#	define map_ref(m) _Generic((m), Map*: (m), Map: &(m))
#endif

uint64_t map_get_impl(Map* m, uint64_t key);
uint64_t map_add_impl(Map* m, uint64_t key, uint64_t val);
int map_del_impl(Map* m, uint64_t key);
void map_clear_impl(Map* m);
void map_free_impl(Map* m);
void map_swap_impl(Map* m, int i, int j);
void map_ssort_impl(Map* m, int ignore_case);

const char* str_intern_range(const char* start, const char* end);

#endif // CKIT_H

//--------------------------------------------------------------------------------------------------

#ifdef CKIT_IMPLEMENTATION

#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include <string.h>
#include <stddef.h>

#ifndef _MSC_VER
#	include <strings.h>
#	ifndef _stricmp
#		define _stricmp strcasecmp
#	endif
#endif

void* agrow(const void* a, int new_size, size_t element_size)
{
	ACANARY(a);
	assert((size_t)acap(a) <= (SIZE_MAX - 1) / 2);
	int new_capacity = ((2 * acap(a)) > ((new_size > 16) ? new_size : 16) ? (2 * acap(a)) : ((new_size > 16) ? new_size : 16));
	assert(new_size <= new_capacity);
	assert((size_t)new_capacity <= (SIZE_MAX - sizeof(ArrayHeader)) / element_size);
	size_t total_size = sizeof(ArrayHeader) + new_capacity * element_size;
	ArrayHeader* hdr;
	if (a) {
		hdr = (ArrayHeader*)realloc(AHDR(a), total_size);
	} else {
		hdr = (ArrayHeader*)malloc(total_size);
		hdr->size = 0;
		hdr->cookie = ACOOKIE;
	}
	hdr->capacity = new_capacity;
	hdr->data = (char*)(hdr + 1); // For debugging convenience.
	return (void*)(hdr + 1);
}

// Map implementation originally by Mattias Gustavsson: https://github.com/mattiasgustavsson/libs

uint64_t map_hash(uint64_t x)
{
	x += 0x9e3779b97f4a7c15ull;
	x = (x ^ (x >> 30)) * 0xbf58476d1ce4e5b9ull;
	x = (x ^ (x >> 27)) * 0x94d049bb133111ebull;
	x ^= (x >> 31);
	return x ? x : 1ull; // Maintain `0` as valid sentinel value.
}

void map_zero_slots(Map* m)
{
	for (int i = 0; i < m->slot_capacity; ++i) {
		m->slots[i].h = 0;
		m->slots[i].item_index = -1;
		m->slots[i].base_count = 0;
	}
	m->slot_count = 0;
}

void map_alloc_slots(Map* m, int min_want)
{
	int want = 16;
	while (want < min_want) want <<= 1;
	MapSlot* s = (MapSlot*)realloc(m->slots, (size_t)want * sizeof(MapSlot));
	assert(s || want == 0);
	m->slots = s;
	m->slot_capacity = want;
	if (want) {
		map_zero_slots(m);
	} else {
		m->slot_count = 0;
	}
}

void map_ensure_items(Map* m, int want)
{
	if (want <= m->capacity) return;
	int nc = m->capacity ? m->capacity * 2 : 16;
	while (nc < want) nc <<= 1;

	// Realloc dense arrays.
	uint64_t* nk = (uint64_t*)realloc(m->keys, (size_t)nc * sizeof(uint64_t));
	uint64_t* nv = (uint64_t*)realloc(m->vals, (size_t)nc * sizeof(uint64_t));
	int* ns = (int*)realloc(m->islot, (size_t)nc * sizeof(int));

	// Init new islot tail to -1.
	for (int i = m->capacity; i < nc; ++i) ns[i] = -1;

	m->keys = nk;
	m->vals = nv;
	m->islot = ns;
	m->capacity = nc;
}

int map_find_insertion_slot(Map* m, uint64_t h)
{
	int mask = m->slot_capacity - 1;
	int base = (int)(h & (uint64_t)mask);
	int slot = base, first_free = base;
	int remaining = m->slots[base].base_count;

	while (remaining > 0) {
		if (m->slots[slot].item_index < 0 && m->slots[first_free].item_index >= 0)
			first_free = slot;

		uint64_t sh = m->slots[slot].h;
		if (sh) {
			if (((int)(sh & (uint64_t)mask)) == base) --remaining;
		}
		slot = (slot + 1) & mask;
	}

	slot = first_free;
	while (m->slots[slot].item_index >= 0) {
		slot = (slot + 1) & mask;
	}

	return slot;
}

int map_find_slot(const Map* m, uint64_t key, uint64_t h)
{
	if (m->slot_capacity == 0) return -1;
	int mask = m->slot_capacity - 1;
	int base = (int)(h & (uint64_t)mask);
	int slot = base;
	int remaining = m->slots[base].base_count;
	while (remaining > 0) {
		int item = m->slots[slot].item_index;
		if (item >= 0) {
			uint64_t sh = m->slots[slot].h;
			if (((int)(sh & (uint64_t)mask)) == base) {
				--remaining;
				if (sh == h && m->keys[item] == key) return slot;
			}
		}
		slot = (slot + 1) & mask;
	}
	return -1;
}

void map_rebuild_slots(Map* m, int new_cap)
{
	map_alloc_slots(m, new_cap);
	if (m->slot_capacity == 0) return;

	// Reinsert all dense items.
	for (int idx = 0; idx < m->size; ++idx) {
		uint64_t h = map_hash(m->keys[idx]);
		int slot = map_find_insertion_slot(m, h);
		int base = (int)(h & (uint64_t)(m->slot_capacity - 1));

		m->slots[slot].h = h;
		m->slots[slot].item_index = idx;
		++m->slots[base].base_count;
		m->islot[idx] = slot;
		++m->slot_count;
	}
}

void map_grow(Map* m)
{
	if (m->slot_capacity == 0) {
		map_rebuild_slots(m, 16);
		return;
	}

	// Grow around ~75% full.
	int thresh = m->slot_capacity - (m->slot_capacity >> 2);
	if (m->slot_count >= thresh) {
		int target = m->slot_capacity << 1;
		map_rebuild_slots(m, target);
	}
}

uint64_t map_get_impl(Map* m, uint64_t key)
{
	if (m->size == 0 || m->slot_capacity == 0) return 0;
	uint64_t h = map_hash(key);
	int s = map_find_slot(m, key, h);
	if (s < 0) return 0;
	return m->vals[m->slots[s].item_index];
}

uint64_t map_add_impl(Map* m, uint64_t key, uint64_t val)
{
	map_ensure_items(m, m->size + 1);

	uint64_t h = map_hash(key);

	// Update if entry already exists.
	if (m->slot_capacity) {
		int s = map_find_slot(m, key, h);
		if (s >= 0) {
			m->vals[m->slots[s].item_index] = val;
			return val;
		}
	}

	map_grow(m);
	if (m->slot_capacity == 0) map_rebuild_slots(m, 16);

	// Append to dense set.
	int idx = m->size++;
	m->keys[idx] = key;
	m->vals[idx] = val;

	// Insert into slots.
	int slot = map_find_insertion_slot(m, h);
	int base = (int)(h & (uint64_t)(m->slot_capacity - 1));
	m->slots[slot].h = h;
	m->slots[slot].item_index = idx;
	++m->slots[base].base_count;
	m->islot[idx] = slot;
	++m->slot_count;

	return val;
}

int map_del_impl(Map* m, uint64_t key)
{
	if (m->size == 0 || m->slot_capacity == 0) return 0;
	uint64_t h = map_hash(key);
	int s = map_find_slot(m, key, h);
	if (s < 0) return 0;

	int mask = m->slot_capacity - 1;
	int base = (int)(h & (uint64_t)mask);
	int idx  = m->slots[s].item_index;
	int last = m->size - 1;

	// Unlink slot (leave a hole).
	--m->slots[base].base_count;
	m->slots[s].item_index = -1;
	--m->slot_count;

	// Move last dense item down (if needed).
	if (idx != last) {
		m->keys[idx] = m->keys[last];
		m->vals[idx] = m->vals[last];

		int ms = m->islot[last];       // Slot of moved item.
		m->slots[ms].item_index = idx; // Patch slot -> new dense index.
		m->islot[idx] = ms;            // Patch backpointer.
	}

	// Clear tail islot to -1 for debug cleanliness.
	m->islot[last] = -1;

	--m->size;
	return 1;
}

void map_swap_impl(Map* m, int i, int j)
{
	if (i == j) return;
	assert(i >= 0 && i < m->size);
	assert(j >= 0 && j < m->size);

	uint64_t tk = m->keys[i];
	uint64_t tv = m->vals[i];
	m->keys[i] = m->keys[j];
	m->vals[i] = m->vals[j];
	m->keys[j] = tk;
	m->vals[j] = tv;

	int si = m->islot[i];
	int sj = m->islot[j];
	if (si >= 0) m->slots[si].item_index = j;
	if (sj >= 0) m->slots[sj].item_index = i;
	m->islot[i] = sj;
	m->islot[j] = si;
}

// Standard qsort style.
void map_ssort_range(Map* m, int offset, int count, int ignore_case)
{
	if (count <= 1) return;

	#define CKIT_KEY(i) (m->keys[(offset) + (i)])

	uint64_t pivot = CKIT_KEY(count - 1);
	int lo = 0;
	for (int hi = 0; hi < count - 1; ++hi) {
		const char* hi_key = (const char*)CKIT_KEY(hi);
		const char* pivot_key = (const char*)pivot;
		int swap = 0;
		if (ignore_case) swap = _stricmp(hi_key, pivot_key);
		else swap = strcmp(hi_key, pivot_key);
		if (swap < 0) {
			map_swap_impl(m, offset + lo, offset + hi);
			++lo;
		}
	}
	map_swap_impl(m, offset + (count - 1), offset + lo);

	map_ssort_range(m, offset, lo, ignore_case);
	map_ssort_range(m, offset + lo + 1, count - 1 - lo, ignore_case);

	#undef CKIT_KEY
}

void map_ssort_impl(Map* m, int ignore_case)
{
	map_ssort_range(m, 0, m->size, ignore_case);
}

void map_free_impl(Map* m)
{
	free(m->keys);
	free(m->vals);
	free(m->slots);
	free(m->islot);
	memset(m, 0, sizeof(*m));
}

void map_clear_impl(Map* m)
{
	m->size = 0;
	if (m->slot_capacity) map_zero_slots(m);
}

// Interning method originally from Per Vognsen: https://github.com/pervognsen/bitwise

uint64_t hash_fnv1a(const void* ptr, size_t sz)
{
	uint64_t x = 0xcbf29ce484222325ull;
	char* buf = (char*)ptr;
	for (size_t i = 0; i < sz; i++) {
		x ^= (uint64_t)buf[i];
		x *= 0x100000001b3ull;
		x ^= x >> 32;
	}
	return x;
}

typedef struct UniqueString
{
	size_t len;
	struct UniqueString* next;
	char* str;
} UniqueString;

Map g_interns;

const char* str_intern_range(const char* start, const char* end)
{
	size_t len = (size_t)(end - start);
	uint64_t key = hash_fnv1a((void*)start, len);

	UniqueString* head = (UniqueString*)map_get(g_interns, key);
	for (UniqueString* it = head; it; it = it->next) {
		if (it->len == len && memcmp(it->str, start, len) == 0) {
			return it->str;
		}
	}

	size_t bytes = sizeof(UniqueString) + len + 1;
	UniqueString* node = (UniqueString*)malloc(bytes);
	node->len = len;
	node->next = head;
	node->str = (char*)(node + 1);
	memcpy(node->str, start, len);
	node->str[len] = '\0';
	map_add(g_interns, key, (uintptr_t)node);

	return node->str;
}

void sintern_nuke()
{
	for (int i = 0; i < g_interns.size; ++i) {
		UniqueString* it = (UniqueString*)(uintptr_t)g_interns.vals[i];
		while (it) {
			UniqueString* next = it->next;
			free(it);
			it = next;
		}
	}

	map_free(g_interns);
}

#endif // CKIT_IMPL