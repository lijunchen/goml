package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
    _goml_strings "strings"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_string_to_bytes(s string) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: []byte(s),
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_concat(values *_goml_vec_string) string {
    return _goml_strings.Join(values.items, "")
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_hash(s string) uint64 {
    var h uint64 = 14695981039346656037
    var i int = 0
    for {
        if i >= int(len(s)) {
            break
        }
        h = h * 1099511628211 + uint64(s[i])
        i = i + 1
    }
    return h
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_new__Vec_5uint8() *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: nil,
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_with_capacity__Vec_6string(capacity int) *_goml_vec_string {
    return &_goml_vec_string{
        items: _goml_slices.Grow([]string{}, int(capacity)),
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_capacity__Vec_6string(vec *_goml_vec_string) int {
    return int(cap(vec.items))
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_with_capacity__Vec_5int32(capacity int) *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: _goml_slices.Grow([]int32{}, int(capacity)),
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

func vec_capacity__Vec_5int32(vec *_goml_vec_int32) int {
    return int(cap(vec.items))
}

func vec_reserve__Vec_5int32(vec *_goml_vec_int32, additional int) struct{} {
    vec.items = _goml_slices.Grow(vec.items, int(additional))
    return struct{}{}
}

func vec_truncate__Vec_5int32(vec *_goml_vec_int32, new_len int) struct{} {
    if new_len < 0 {
        panic("negative vector length")
    }
    if new_len < int(len(vec.items)) {
        clear(vec.items[new_len:int(len(vec.items))])
        vec.items = vec.items[0:new_len]
    }
    return struct{}{}
}

type _goml_vec_Tuple2_6string_5int32 struct {
    items []Tuple2_6string_5int32
}

func vec_get__Vec_21Tuple2_6string_5int32(vec *_goml_vec_Tuple2_6string_5int32, index int) Tuple2_6string_5int32 {
    return vec.items[index]
}

func vec_len__Vec_21Tuple2_6string_5int32(vec *_goml_vec_Tuple2_6string_5int32) int {
    return int(len(vec.items))
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type hashmap_string_int32_x_entry struct {
    active bool
    key string
    value int32
}

type hashmap_string_int32_x struct {
    buckets map[uint64][]hashmap_string_int32_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_6string_5int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        buckets: make(map[uint64][]hashmap_string_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_set__HashMap_6string_5int32(m *hashmap_string_int32_x, key string, value int32) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_string_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_string_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_string_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_string_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) struct{} {
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_string_int32_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_string_i_eq(entry.key, key) {
            var zero hashmap_string_int32_x_entry
            bucket[i] = zero
            m.len = m.len - 1
            return struct{}{}
        }
        i = i + 1
    }
    return struct{}{}
}

func hashmap_entries__HashMap_6string_5int32(m *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var result []Tuple2_6string_5int32
    if m == nil {
        return &_goml_vec_Tuple2_6string_5int32{
            items: result,
        }
    }
    for _, h := range m.hashes {
        var bucket []hashmap_string_int32_x_entry = m.buckets[h]
        var i int = 0
        for {
            if i >= int(len(bucket)) {
                break
            }
            var entry hashmap_string_int32_x_entry = bucket[i]
            if entry.active {
                result = append(result, Tuple2_6string_5int32{
                    _0: entry.key,
                    _1: entry.value,
                })
            }
            i = i + 1
        }
    }
    return &_goml_vec_Tuple2_6string_5int32{
        items: result,
    }
}

type Tuple2_3int_4char struct {
    _0 int
    _1 rune
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_6string_5int32 struct {
    _0 string
    _1 int32
}

type Tuple2_4char_3int struct {
    _0 rune
    _1 int
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type FnIterator__char struct {
    next_fn func() Option__char
}

type _goml_m_FnIterator_____o_int_c_char_q_ struct {
    next_fn func() _goml_m_Option_____o_int_c_char_q_
}

type closure_env_inherent_string_string_chars_0 struct {
    self_0 string
    index_1 *ref_int_x
}

type closure_env_inherent_string_string_char_indices_1 struct {
    index_0 *ref_int_x
    self_1 string
}

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

type _goml_m_Option_____o_int_c_char_q_ interface {
    is_goml_m_Option_____o_int_c_char_q_()
}

type _goml_m_Option_____o_int_c_char_q__None struct {}

func (_ _goml_m_Option_____o_int_c_char_q__None) is_goml_m_Option_____o_int_c_char_q_() {}

type _goml_m_Option_____o_int_c_char_q__Some struct {
    _0 Tuple2_3int_4char
}

func (_ _goml_m_Option_____o_int_c_char_q__Some) is_goml_m_Option_____o_int_c_char_q_() {}

type _goml_m_Option_____o_char_c_int_q_ interface {
    is_goml_m_Option_____o_char_c_int_q_()
}

type _goml_m_Option_____o_char_c_int_q__None struct {}

func (_ _goml_m_Option_____o_char_c_int_q__None) is_goml_m_Option_____o_char_c_int_q_() {}

type _goml_m_Option_____o_char_c_int_q__Some struct {
    _0 Tuple2_4char_3int
}

func (_ _goml_m_Option_____o_char_c_int_q__Some) is_goml_m_Option_____o_char_c_int_q_() {}

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

func print_chars(value__0 string) struct{} {
    var t276 FnIterator__char
    var inline816 *ref_int_x = ref__Ref_3int(0)
    var inline817 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: value__0,
        index_1: inline816,
    }
    var inline818 func() Option__char = func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(inline817)
    }
    var inline819 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(inline818)
    t276 = inline819
    var for_iter182 FnIterator__char
    for_iter182 = t276
    Loop_loop278:
    for {
        var for_next183 Option__char
        var inline812 func() Option__char = for_iter182.next_fn
        var inline813 Option__char = inline812()
        for_next183 = inline813
        switch for_next183.(type) {
        case Option__char_None:
            break Loop_loop278
        case Option__char_Some:
            var x184 rune = for_next183.(Option__char_Some)._0
            var inline809 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x184)
            _goml_runtime_core_string_println(inline809)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t282 _goml_m_FnIterator_____o_int_c_char_q_
    var inline832 *ref_int_x = ref__Ref_3int(0)
    var inline833 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: inline832,
        self_1: value__2,
    }
    var inline834 func() _goml_m_Option_____o_int_c_char_q_ = func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(inline833)
    }
    var inline835 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(inline834)
    t282 = inline835
    var for_iter185 _goml_m_FnIterator_____o_int_c_char_q_
    for_iter185 = t282
    Loop_loop284:
    for {
        var for_next186 _goml_m_Option_____o_int_c_char_q_
        var inline828 func() _goml_m_Option_____o_int_c_char_q_ = for_iter185.next_fn
        var inline829 _goml_m_Option_____o_int_c_char_q_ = inline828()
        for_next186 = inline829
        switch for_next186.(type) {
        case _goml_m_Option_____o_int_c_char_q__None:
            break Loop_loop284
        case _goml_m_Option_____o_int_c_char_q__Some:
            var x187 Tuple2_3int_4char = for_next186.(_goml_m_Option_____o_int_c_char_q__Some)._0
            var x189 int = x187._0
            var x190 rune = x187._1
            var t286 string
            var inline826 string = _goml_runtime_core_int_to_string(x189)
            t286 = inline826
            var t287 string = t286 + ":"
            var t288 string
            var inline824 string = char_to_string(x190)
            t288 = inline824
            var t289 string = t287 + t288
            var inline821 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t289)
            _goml_runtime_core_string_println(inline821)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t292 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t292)
    var t293 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t293)
    var t294 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t294)
    var t295 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t295)
    var t296 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t296)
    var t297 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t297)
    var t298 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t298)
    var t299 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t299)
    var t300 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t300)
    var t301 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t301)
    var t302 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t302)
    var t303 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t303)
    var mtmp203 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp203.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        var inline837 string = "missing"
        var inline838 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline837)
        _goml_runtime_core_string_println(inline838)
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x204 Tuple2_4char_3int = mtmp203.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var x206 rune = x204._0
        var x207 int = x204._1
        var inline844 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x206)
        _goml_runtime_core_string_println(inline844)
        var inline841 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x207)
        _goml_runtime_core_string_println(inline841)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t305 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t305)
    var t306 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t306)
    var t307 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t307)
    var mtmp215 Tuple2_4bool_6string = string_from_utf8(bytes__10)
    var x216 bool = mtmp215._0
    var x217 string = mtmp215._1
    println__T_bool(x216)
    println__T_string(x217)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp222 Tuple2_4bool_6string = string_from_utf8(invalid__13)
    var x223 bool = mtmp222._0
    var x224 string = mtmp222._1
    println__T_bool(x223)
    var t308 bool = x224 == ""
    println__T_bool(t308)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t309 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t310 bool = t309 >= 3
    println__T_bool(t310)
    var t311 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t311)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t312 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t312)
    var t313 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t314 bool = t313 >= 1
    println__T_bool(t314)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t315 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t316 bool = t315 >= 100
    println__T_bool(t316)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t317 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t317)
    var t318 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t318)
    var t319 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t319)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t320 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t320)
    var t321 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t321)
    var mtmp246 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp246.(type) {
    case Option__int32_None:
        var inline847 int = -1
        var inline848 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline847)
        _goml_runtime_core_string_println(inline848)
    case Option__int32_Some:
        var x247 int32 = mtmp246.(Option__int32_Some)._0
        var inline851 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x247)
        _goml_runtime_core_string_println(inline851)
    default:
        panic("non-exhaustive match")
    }
    var mtmp249 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp249.(type) {
    case Option__int32_None:
        var inline854 int = -1
        var inline855 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline854)
        _goml_runtime_core_string_println(inline855)
    case Option__int32_Some:
        var x250 int32 = mtmp249.(Option__int32_Some)._0
        var inline858 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x250)
        _goml_runtime_core_string_println(inline858)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t324 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t324)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t325 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t325)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t326 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t326)
    var inline891 string = "c"
    var inline892 int32 = 3
    hashmap_set__HashMap_6string_5int32(map__20, inline891, inline892)
    var inline888 string = "a"
    hashmap_remove__HashMap_6string_5int32(map__20, inline888)
    var t327 int
    var inline886 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    t327 = inline886
    var inline883 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t327)
    _goml_runtime_core_string_println(inline883)
    var seen_a__22 *ref_bool_x
    var inline880 bool = false
    var inline881 *ref_bool_x = ref__Ref_4bool(inline880)
    seen_a__22 = inline881
    var seen_b__23 *ref_bool_x
    var inline877 bool = false
    var inline878 *ref_bool_x = ref__Ref_4bool(inline877)
    seen_b__23 = inline878
    var for_limit264 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    var for_index265 int = 0
    Loop_loop332:
    for {
        var t333 bool = for_index265 < for_limit264
        if t333 {
            var for_item266 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(entries__21, for_index265)
            var t334 int = for_index265 + 1
            for_index265 = t334
            var x269 string = for_item266._0
            var x270 int32 = for_item266._1
            var t345 bool = x269 == "a"
            var jp337 bool
            if t345 {
                var t346 bool = x270 == 1
                jp337 = t346
            } else {
                jp337 = false
            }
            if jp337 {
                var inline861 bool = true
                ref_set__Ref_4bool(seen_a__22, inline861)
                continue
            } else {
                var t343 bool = x269 == "b"
                var jp341 bool
                if t343 {
                    var t344 bool = x270 == 2
                    jp341 = t344
                } else {
                    jp341 = false
                }
                if jp341 {
                    var inline864 bool = true
                    ref_set__Ref_4bool(seen_b__23, inline864)
                    continue
                } else {
                    continue
                }
            }
        } else {
            break Loop_loop332
        }
    }
    var t329 bool
    var inline875 bool = ref_get__Ref_4bool(seen_a__22)
    t329 = inline875
    var inline872 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t329)
    _goml_runtime_core_string_println(inline872)
    var t330 bool
    var inline870 bool = ref_get__Ref_4bool(seen_b__23)
    t330 = inline870
    var inline867 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t330)
    _goml_runtime_core_string_println(inline867)
    return struct{}{}
}

func println__T_char(value__1 rune) struct{} {
    var t365 string
    var inline897 string = char_to_string(value__1)
    t365 = inline897
    _goml_runtime_core_string_println(t365)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t379 string
    t379 = value__1
    _goml_runtime_core_string_println(t379)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t388 string
    var inline907 string = _goml_runtime_core_int_to_string(value__1)
    t388 = inline907
    _goml_runtime_core_string_println(t388)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t392 int = _goml_runtime_core_string_len(self__36)
    return t392
}

func _goml_m_inherent_i_string_i_string_i_len(self__35 string) int {
    var t395 int = _goml_runtime_core_string_len(self__35)
    return t395
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline909 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline910 bool = inline909._0
    var inline911 rune = inline909._1
    if inline910 {
        return inline911
    } else {
        var inline914 rune = _goml_runtime_core_string_get("", -1)
        return inline914
    }
}

func println__T_bool(value__1 bool) struct{} {
    var t400 string
    var inline916 string = _goml_runtime_core_bool_to_string(value__1)
    t400 = inline916
    _goml_runtime_core_string_println(t400)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__44 string, index__45 int) bool {
    var t404 bool = string_is_char_boundary(self__44, index__45)
    return t404
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline918 bool = string_is_char_boundary(self__41, start__42)
    var inline920 bool
    if inline918 {
        var inline923 bool = string_is_char_boundary(self__41, end__43)
        inline920 = inline923
    } else {
        inline920 = false
    }
    if inline920 {
        var inline921 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline921
    } else {
        var inline922 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline922
    }
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__46 string, index__47 int) _goml_m_Option_____o_char_c_int_q_ {
    var mtmp25 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__46, index__47)
    var x26 bool = mtmp25._0
    var x27 rune = mtmp25._1
    var x28 int = mtmp25._2
    if x26 {
        var t412 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: x27,
            _1: x28,
        }
        var t413 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t412,
        }
        return t413
    } else {
        return _goml_m_Option_____o_char_c_int_q__None{}
    }
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__51 string) *_goml_vec_uint8 {
    var t416 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__51)
    return t416
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__189 *_goml_vec_uint8) int {
    var t419 int = vec_len__Vec_5uint8(self__189)
    return t419
}

func println__T_uint8(value__1 uint8) struct{} {
    var t421 string
    var inline925 string = _goml_runtime_core_uint8_to_string(value__1)
    t421 = inline925
    _goml_runtime_core_string_println(t421)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__184 *_goml_vec_uint8, index__185 int) uint8 {
    var t425 uint8 = vec_get__Vec_5uint8(self__184, index__185)
    return t425
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop430:
    for {
        var t431 int
        var inline927 int = _goml_runtime_core_string_len(x12)
        t431 = inline927
        var t432 bool = index__26 < t431
        if t432 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t434 int = compound_old17 + x16
                index__26 = t434
                continue
            } else {
                var t436 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t436
            }
        } else {
            break Loop_loop430
        }
    }
    var t429 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t429
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t439 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t439
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__174 *_goml_vec_uint8, elem__175 uint8) struct{} {
    vec_push__Vec_5uint8(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__173 int) *_goml_vec_string {
    var t444 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__173)
    return t444
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__174 *_goml_vec_string, elem__175 string) struct{} {
    vec_push__Vec_6string(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__190 *_goml_vec_string) int {
    var t449 int = vec_capacity__Vec_6string(self__190)
    return t449
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__173 int) *_goml_vec_int32 {
    var t452 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__173)
    return t452
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__189 *_goml_vec_int32) int {
    var t455 int = vec_len__Vec_5int32(self__189)
    return t455
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__190 *_goml_vec_int32) int {
    var t458 int = vec_capacity__Vec_5int32(self__190)
    return t458
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__192 *_goml_vec_int32, additional__193 int) struct{} {
    vec_reserve__Vec_5int32(self__192, additional__193)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__174 *_goml_vec_int32, elem__175 int32) struct{} {
    vec_push__Vec_5int32(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(self__210 *_goml_vec_int32, index__211 int, value__212 int32) struct{} {
    var len__213 int
    var inline933 int = vec_len__Vec_5int32(self__210)
    len__213 = inline933
    var t465 bool = index__211 == len__213
    if t465 {
        vec_push__Vec_5int32(self__210, value__212)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__210, index__211)
        var t467 int = len__213 - 1
        var t468 int32 = vec_get__Vec_5int32(self__210, t467)
        vec_push__Vec_5int32(self__210, t468)
        var current__214 int = len__213 - 1
        Loop_loop471:
        for {
            var t472 bool = current__214 > index__211
            if t472 {
                var index111 int = current__214
                vec_get__Vec_5int32(self__210, index111)
                var t473 int = current__214 - 1
                var value113 int32 = vec_get__Vec_5int32(self__210, t473)
                vec_set__Vec_5int32(self__210, index111, value113)
                var compound_old115 int = current__214
                var compound_value116 int = 1
                var t475 int = compound_old115 - compound_value116
                current__214 = t475
                continue
            } else {
                break Loop_loop471
            }
        }
        vec_get__Vec_5int32(self__210, index__211)
        vec_set__Vec_5int32(self__210, index__211, value__212)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t478 string
    var inline935 string = _goml_runtime_core_int32_to_string(value__1)
    t478 = inline935
    _goml_runtime_core_string_println(t478)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__215 *_goml_vec_int32, index__216 int) int32 {
    var len__217 int
    var inline939 int = vec_len__Vec_5int32(self__215)
    len__217 = inline939
    var value__218 int32 = vec_get__Vec_5int32(self__215, index__216)
    var current__219 int = index__216
    Loop_loop484:
    for {
        var t485 int = current__219 + 1
        var t486 bool = t485 < len__217
        if t486 {
            var index125 int = current__219
            vec_get__Vec_5int32(self__215, index125)
            var t487 int = current__219 + 1
            var value127 int32 = vec_get__Vec_5int32(self__215, t487)
            vec_set__Vec_5int32(self__215, index125, value127)
            var compound_old129 int = current__219
            var compound_value130 int = 1
            var t489 int = compound_old129 + compound_value130
            current__219 = t489
            continue
        } else {
            break Loop_loop484
        }
    }
    var t483 int = len__217 - 1
    vec_truncate__Vec_5int32(self__215, t483)
    return value__218
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__206 *_goml_vec_int32, index__207 int) int32 {
    var len__208 int
    var inline943 int = vec_len__Vec_5int32(self__206)
    len__208 = inline943
    var value__209 int32 = vec_get__Vec_5int32(self__206, index__207)
    var t495 int = index__207 + 1
    var t496 bool = t495 < len__208
    if t496 {
        vec_get__Vec_5int32(self__206, index__207)
        var t497 int = len__208 - 1
        var value104 int32 = vec_get__Vec_5int32(self__206, t497)
        vec_set__Vec_5int32(self__206, index__207, value104)
    } else {}
    var t494 int = len__208 - 1
    vec_truncate__Vec_5int32(self__206, t494)
    return value__209
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__220 *_goml_vec_int32) struct{} {
    var left__221 int = 0
    var t500 int
    var inline959 int = vec_len__Vec_5int32(self__220)
    t500 = inline959
    var right__222 int = t500 - 1
    Loop_loop502:
    for {
        var t503 bool = left__221 < right__222
        if t503 {
            var inline945 int32 = vec_get__Vec_5int32(self__220, left__221)
            vec_get__Vec_5int32(self__220, left__221)
            var inline949 int32 = vec_get__Vec_5int32(self__220, right__222)
            vec_set__Vec_5int32(self__220, left__221, inline949)
            vec_get__Vec_5int32(self__220, right__222)
            vec_set__Vec_5int32(self__220, right__222, inline945)
            var compound_old135 int = left__221
            var compound_value136 int = 1
            var t504 int = compound_old135 + compound_value136
            left__221 = t504
            var compound_old138 int = right__222
            var compound_value139 int = 1
            var t506 int = compound_old138 - compound_value139
            right__222 = t506
            continue
        } else {
            break Loop_loop502
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__184 *_goml_vec_int32, index__185 int) int32 {
    var t510 int32 = vec_get__Vec_5int32(self__184, index__185)
    return t510
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__197 *_goml_vec_int32) Option__int32 {
    var len__198 int
    var inline961 int = vec_len__Vec_5int32(self__197)
    len__198 = inline961
    var t515 bool = len__198 == 0
    if t515 {
        return Option__int32_None{}
    } else {
        var t516 int = len__198 - 1
        var t517 int32 = vec_get__Vec_5int32(self__197, t516)
        var t518 Option__int32 = Option__int32_Some{
            _0: t517,
        }
        return t518
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__199 *_goml_vec_int32) Option__int32 {
    var len__200 int
    var inline965 int = vec_len__Vec_5int32(self__199)
    len__200 = inline965
    var t523 bool = len__200 == 0
    if t523 {
        return Option__int32_None{}
    } else {
        var t524 int = len__200 - 1
        var value__201 int32 = vec_get__Vec_5int32(self__199, t524)
        var t525 int = len__200 - 1
        vec_truncate__Vec_5int32(self__199, t525)
        var t526 Option__int32 = Option__int32_Some{
            _0: value__201,
        }
        return t526
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__194 *_goml_vec_int32, len__195 int) struct{} {
    vec_truncate__Vec_5int32(self__194, len__195)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__191 *_goml_vec_int32) bool {
    var t531 int = vec_len__Vec_5int32(self__191)
    var t532 bool = t531 == 0
    return t532
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__196 *_goml_vec_int32) struct{} {
    var inline967 int = 0
    vec_truncate__Vec_5int32(self__196, inline967)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t537 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t537
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__261 *hashmap_string_int32_x, key__262 string, value__263 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__261, key__262, value__263)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__269 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t542 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__269)
    return t542
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__189 *_goml_vec_Tuple2_6string_5int32) int {
    var t545 int = vec_len__Vec_21Tuple2_6string_5int32(self__189)
    return t545
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__170 func() Option__char) FnIterator__char {
    var t558 FnIterator__char = FnIterator__char{
        next_fn: next_fn__170,
    }
    return t558
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__66 rune) string {
    var inline970 uint32 = uint32(rune(self__66))
    var inline971 bool = utf8_valid_scalar(inline970)
    if inline971 {
        var inline972 string = _goml_runtime_core_char_to_string(self__66)
        return inline972
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__170 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t564 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__170,
    }
    return t564
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func char_to_string(value__29 rune) string {
    var t571 uint32 = uint32(rune(value__29))
    var t572 bool
    var inline975 bool = t571 <= 1114111
    if inline975 {
        var inline976 bool = t571 >= 55296
        var inline978 bool
        if inline976 {
            var inline980 bool = t571 <= 57343
            inline978 = inline980
        } else {
            inline978 = false
        }
        var inline979 bool = !inline978
        t572 = inline979
    } else {
        t572 = false
    }
    if t572 {
        var t573 string = _goml_runtime_core_char_to_string(value__29)
        return t573
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t576 string = _goml_runtime_core_int_to_string(self__67)
    return t576
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t584 string = _goml_runtime_core_bool_to_string(self__64)
    return t584
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t598 bool = index__16 < 0
    var jp590 bool
    if t598 {
        jp590 = true
    } else {
        var t599 int
        var inline982 int = _goml_runtime_core_string_len(value__15)
        t599 = inline982
        var t600 bool = index__16 > t599
        jp590 = t600
    }
    if jp590 {
        return false
    } else {
        var t593 int
        var inline986 int = _goml_runtime_core_string_len(value__15)
        t593 = inline986
        var t594 bool = index__16 == t593
        if t594 {
            return true
        } else {
            var t595 uint8
            var inline984 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t595 = inline984
            var t596_rhs uint8 = 192
            var t596 uint8 = t595 & t596_rhs
            var t597 bool = t596 != 128
            return t597
        }
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t729 bool = index__6 < 0
    var jp727 bool
    if t729 {
        jp727 = true
    } else {
        var t730 bool = index__6 >= length__7
        jp727 = t730
    }
    if jp727 {
        var inline988 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline988
    } else {
        var t614 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t614))
        var t617 bool = first__8 < 128
        if t617 {
            var inline990 int = 1
            var inline991 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline991.(type) {
            case Option__char_None:
                var inline992 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline992
            case Option__char_Some:
                var inline993 rune = inline991.(Option__char_Some)._0
                var inline995 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline993,
                    _2: inline990,
                }
                return inline995
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t621 bool = first__8 < 194
            if t621 {
                var inline997 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline997
            } else {
                var t625 bool = first__8 < 224
                if t625 {
                    var t638 int = length__7 - index__6
                    var t639 bool = t638 < 2
                    if t639 {
                        var inline999 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline999
                    } else {
                        var t627 int = index__6 + 1
                        var t628 uint8
                        var inline1013 uint8 = _goml_runtime_core_string_byte_get(value__5, t627)
                        t628 = inline1013
                        var second__9 uint32 = uint32(uint8(t628))
                        var t631 bool
                        var inline1010 bool = second__9 < 128
                        if inline1010 {
                            t631 = true
                        } else {
                            var inline1011 bool = second__9 > 191
                            t631 = inline1011
                        }
                        if t631 {
                            var inline1001 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1001
                        } else {
                            var t633_rhs uint32 = 31
                            var t633 uint32 = first__8 & t633_rhs
                            var t634_rhs int = 6
                            var t634 uint32 = t633 << t634_rhs
                            var t635_rhs uint32 = 63
                            var t635 uint32 = second__9 & t635_rhs
                            var t636 uint32 = t634 | t635
                            var inline1003 int = 2
                            var inline1004 Option__char = __goml_builtin_char_from_uint32(t636)
                            switch inline1004.(type) {
                            case Option__char_None:
                                var inline1005 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1005
                            case Option__char_Some:
                                var inline1006 rune = inline1004.(Option__char_Some)._0
                                var inline1008 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1006,
                                    _2: inline1003,
                                }
                                return inline1008
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t643 bool = first__8 < 240
                    if t643 {
                        var t676 int = length__7 - index__6
                        var t677 bool = t676 < 3
                        if t677 {
                            var inline1015 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1015
                        } else {
                            var t645 int = index__6 + 1
                            var t646 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t645)
                            var second__10 uint32 = uint32(uint8(t646))
                            var t647 int = index__6 + 2
                            var t648 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t647)
                            var third__11 uint32 = uint32(uint8(t648))
                            var t674 bool = utf8_invalid_continuation(second__10)
                            var jp669 bool
                            if t674 {
                                jp669 = true
                            } else {
                                var inline1017 bool = third__11 < 128
                                if inline1017 {
                                    jp669 = true
                                } else {
                                    var inline1018 bool = third__11 > 191
                                    jp669 = inline1018
                                }
                            }
                            var jp663 bool
                            if jp669 {
                                jp663 = true
                            } else {
                                var t672 bool = first__8 == 224
                                if t672 {
                                    var t673 bool = second__10 < 160
                                    jp663 = t673
                                } else {
                                    jp663 = false
                                }
                            }
                            var jp652 bool
                            if jp663 {
                                jp652 = true
                            } else {
                                var t666 bool = first__8 == 237
                                if t666 {
                                    var t667 bool = second__10 >= 160
                                    jp652 = t667
                                } else {
                                    jp652 = false
                                }
                            }
                            if jp652 {
                                var inline1020 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1020
                            } else {
                                var t654_rhs uint32 = 15
                                var t654 uint32 = first__8 & t654_rhs
                                var t655_rhs int = 12
                                var t655 uint32 = t654 << t655_rhs
                                var t656_rhs uint32 = 63
                                var t656 uint32 = second__10 & t656_rhs
                                var t657_rhs int = 6
                                var t657 uint32 = t656 << t657_rhs
                                var t658 uint32 = t655 | t657
                                var t659_rhs uint32 = 63
                                var t659 uint32 = third__11 & t659_rhs
                                var t660 uint32 = t658 | t659
                                var inline1022 int = 3
                                var inline1023 Option__char = __goml_builtin_char_from_uint32(t660)
                                switch inline1023.(type) {
                                case Option__char_None:
                                    var inline1024 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1024
                                case Option__char_Some:
                                    var inline1025 rune = inline1023.(Option__char_Some)._0
                                    var inline1027 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1025,
                                        _2: inline1022,
                                    }
                                    return inline1027
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t681 bool = first__8 < 245
                        if t681 {
                            var t722 int = length__7 - index__6
                            var t723 bool = t722 < 4
                            if t723 {
                                var t724 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t724
                            } else {
                                var t683 int = index__6 + 1
                                var t684 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t683)
                                var second__12 uint32 = uint32(uint8(t684))
                                var t685 int = index__6 + 2
                                var t686 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t685)
                                var third__13 uint32 = uint32(uint8(t686))
                                var t687 int = index__6 + 3
                                var t688 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t687)
                                var fourth__14 uint32 = uint32(uint8(t688))
                                var t720 bool = utf8_invalid_continuation(second__12)
                                var jp718 bool
                                if t720 {
                                    jp718 = true
                                } else {
                                    var t721 bool = utf8_invalid_continuation(third__13)
                                    jp718 = t721
                                }
                                var jp712 bool
                                if jp718 {
                                    jp712 = true
                                } else {
                                    var t719 bool = utf8_invalid_continuation(fourth__14)
                                    jp712 = t719
                                }
                                var jp706 bool
                                if jp712 {
                                    jp706 = true
                                } else {
                                    var t715 bool = first__8 == 240
                                    if t715 {
                                        var t716 bool = second__12 < 144
                                        jp706 = t716
                                    } else {
                                        jp706 = false
                                    }
                                }
                                var jp692 bool
                                if jp706 {
                                    jp692 = true
                                } else {
                                    var t709 bool = first__8 == 244
                                    if t709 {
                                        var t710 bool = second__12 > 143
                                        jp692 = t710
                                    } else {
                                        jp692 = false
                                    }
                                }
                                if jp692 {
                                    var t693 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t693
                                } else {
                                    var t694_rhs uint32 = 7
                                    var t694 uint32 = first__8 & t694_rhs
                                    var t695_rhs int = 18
                                    var t695 uint32 = t694 << t695_rhs
                                    var t696_rhs uint32 = 63
                                    var t696 uint32 = second__12 & t696_rhs
                                    var t697_rhs int = 12
                                    var t697 uint32 = t696 << t697_rhs
                                    var t698 uint32 = t695 | t697
                                    var t699_rhs uint32 = 63
                                    var t699 uint32 = third__13 & t699_rhs
                                    var t700_rhs int = 6
                                    var t700 uint32 = t699 << t700_rhs
                                    var t701 uint32 = t698 | t700
                                    var t702_rhs uint32 = 63
                                    var t702 uint32 = fourth__14 & t702_rhs
                                    var t703 uint32 = t701 | t702
                                    var t704 Tuple3_4bool_4char_3int = utf8_valid_decode(t703, 4)
                                    return t704
                                }
                            }
                        } else {
                            var t725 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t725
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t736 string = _goml_runtime_core_int32_to_string(self__70)
    return t736
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t744 bool = value__4 <= 1114111
    if t744 {
        var t748 bool = value__4 >= 55296
        var jp746 bool
        if t748 {
            var t749 bool = value__4 <= 57343
            jp746 = t749
        } else {
            jp746 = false
        }
        var t747 bool = !jp746
        return t747
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t752 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t752
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t755 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t755
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1070 rune
    var inline1031 bool = utf8_valid_scalar(value__0)
    if inline1031 {
        var inline1032 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1033 rune = inline1032._1
        commute_field1070 = inline1033
        var t761 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1070,
            _2: width__1,
        }
        return t761
    } else {
        var inline1029 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1029
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t766 bool = value__3 < 128
    if t766 {
        return true
    } else {
        var t767 bool = value__3 > 191
        return t767
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t772 bool
    var inline1037 bool = value__30 <= 1114111
    if inline1037 {
        var inline1038 bool = value__30 >= 55296
        var inline1040 bool
        if inline1038 {
            var inline1042 bool = value__30 <= 57343
            inline1040 = inline1042
        } else {
            inline1040 = false
        }
        var inline1041 bool = !inline1040
        t772 = inline1041
    } else {
        t772 = false
    }
    if t772 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t773 Option__char = Option__char_Some{
            _0: x24,
        }
        return t773
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__97 string, other__98 string) bool {
    var t776 bool = self__97 == other__98
    return t776
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__125 string) uint64 {
    var t779 uint64 = _goml_runtime_core_string_hash(self__125)
    return t779
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env273 closure_env_inherent_string_string_chars_0) Option__char {
    var self__52 string = env273.self_0
    var index__53 *ref_int_x = env273.index_1
    var t795 int = ref_get__Ref_3int(index__53)
    var commute_field1073 Tuple2_4char_3int
    var inline1044 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__52, t795)
    var inline1045 bool = inline1044._0
    var inline1046 rune = inline1044._1
    var inline1047 int = inline1044._2
    if inline1045 {
        var inline1051 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1046,
            _1: inline1047,
        }
        commute_field1073 = inline1051
        var x32 rune = commute_field1073._0
        var x33 int = commute_field1073._1
        var compound_old34 int = ref_get__Ref_3int(index__53)
        var t798 int = compound_old34 + x33
        ref_set__Ref_3int(index__53, t798)
        var t800 Option__char = Option__char_Some{
            _0: x32,
        }
        return t800
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env274 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var index__58 *ref_int_x = env274.index_0
    var self__57 string = env274.self_1
    var current__59 int = ref_get__Ref_3int(index__58)
    var commute_field1076 Tuple2_4char_3int
    var inline1054 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__57, current__59)
    var inline1055 bool = inline1054._0
    var inline1056 rune = inline1054._1
    var inline1057 int = inline1054._2
    if inline1055 {
        var inline1061 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1056,
            _1: inline1057,
        }
        commute_field1076 = inline1061
        var x40 rune = commute_field1076._0
        var x41 int = commute_field1076._1
        var t805 int = current__59 + x41
        ref_set__Ref_3int(index__58, t805)
        var t806 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__59,
            _1: x40,
        }
        var t807 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t806,
        }
        return t807
    } else {
        return _goml_m_Option_____o_int_c_char_q__None{}
    }
}

func main() {
    main0()
}
