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
    var t281 FnIterator__char
    var inline821 *ref_int_x = ref__Ref_3int(0)
    var inline822 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: value__0,
        index_1: inline821,
    }
    var inline823 func() Option__char = func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(inline822)
    }
    var inline824 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(inline823)
    t281 = inline824
    var for_iter187 FnIterator__char
    for_iter187 = t281
    Loop_loop283:
    for {
        var for_next188 Option__char
        var inline817 func() Option__char = for_iter187.next_fn
        var inline818 Option__char = inline817()
        for_next188 = inline818
        switch for_next188.(type) {
        case Option__char_None:
            break Loop_loop283
        case Option__char_Some:
            var x189 rune = for_next188.(Option__char_Some)._0
            var inline814 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x189)
            _goml_runtime_core_string_println(inline814)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t287 _goml_m_FnIterator_____o_int_c_char_q_
    var inline837 *ref_int_x = ref__Ref_3int(0)
    var inline838 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: inline837,
        self_1: value__2,
    }
    var inline839 func() _goml_m_Option_____o_int_c_char_q_ = func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(inline838)
    }
    var inline840 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(inline839)
    t287 = inline840
    var for_iter190 _goml_m_FnIterator_____o_int_c_char_q_
    for_iter190 = t287
    Loop_loop289:
    for {
        var for_next191 _goml_m_Option_____o_int_c_char_q_
        var inline833 func() _goml_m_Option_____o_int_c_char_q_ = for_iter190.next_fn
        var inline834 _goml_m_Option_____o_int_c_char_q_ = inline833()
        for_next191 = inline834
        switch for_next191.(type) {
        case _goml_m_Option_____o_int_c_char_q__None:
            break Loop_loop289
        case _goml_m_Option_____o_int_c_char_q__Some:
            var x192 Tuple2_3int_4char = for_next191.(_goml_m_Option_____o_int_c_char_q__Some)._0
            var x194 int = x192._0
            var x195 rune = x192._1
            var t291 string
            var inline831 string = _goml_runtime_core_int_to_string(x194)
            t291 = inline831
            var t292 string = t291 + ":"
            var t293 string
            var inline829 string = char_to_string(x195)
            t293 = inline829
            var t294 string = t292 + t293
            var inline826 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t294)
            _goml_runtime_core_string_println(inline826)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t297 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t297)
    var t298 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t298)
    var t299 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t299)
    var t300 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t300)
    var t301 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t301)
    var t302 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t302)
    var t303 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t303)
    var t304 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t304)
    var t305 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t305)
    var t306 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t306)
    var t307 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t307)
    var t308 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t308)
    var mtmp208 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp208.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        var inline842 string = "missing"
        var inline843 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline842)
        _goml_runtime_core_string_println(inline843)
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x209 Tuple2_4char_3int = mtmp208.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var x211 rune = x209._0
        var x212 int = x209._1
        var inline849 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x211)
        _goml_runtime_core_string_println(inline849)
        var inline846 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x212)
        _goml_runtime_core_string_println(inline846)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t310 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t310)
    var t311 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t311)
    var t312 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t312)
    var mtmp220 Tuple2_4bool_6string = string_from_utf8(bytes__10)
    var x221 bool = mtmp220._0
    var x222 string = mtmp220._1
    println__T_bool(x221)
    println__T_string(x222)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp227 Tuple2_4bool_6string = string_from_utf8(invalid__13)
    var x228 bool = mtmp227._0
    var x229 string = mtmp227._1
    println__T_bool(x228)
    var t313 bool = x229 == ""
    println__T_bool(t313)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t314 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t315 bool = t314 >= 3
    println__T_bool(t315)
    var t316 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t316)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t317 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t317)
    var t318 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t319 bool = t318 >= 1
    println__T_bool(t319)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t320 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t321 bool = t320 >= 100
    println__T_bool(t321)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t322 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t322)
    var t323 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t323)
    var t324 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t324)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t325 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t325)
    var t326 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t326)
    var mtmp251 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp251.(type) {
    case Option__int32_None:
        var inline852 int = -1
        var inline853 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline852)
        _goml_runtime_core_string_println(inline853)
    case Option__int32_Some:
        var x252 int32 = mtmp251.(Option__int32_Some)._0
        var inline856 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x252)
        _goml_runtime_core_string_println(inline856)
    default:
        panic("non-exhaustive match")
    }
    var mtmp254 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp254.(type) {
    case Option__int32_None:
        var inline859 int = -1
        var inline860 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline859)
        _goml_runtime_core_string_println(inline860)
    case Option__int32_Some:
        var x255 int32 = mtmp254.(Option__int32_Some)._0
        var inline863 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x255)
        _goml_runtime_core_string_println(inline863)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t329 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t329)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t330 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t330)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t331 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t331)
    var inline896 string = "c"
    var inline897 int32 = 3
    hashmap_set__HashMap_6string_5int32(map__20, inline896, inline897)
    var inline893 string = "a"
    hashmap_remove__HashMap_6string_5int32(map__20, inline893)
    var t332 int
    var inline891 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    t332 = inline891
    var inline888 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t332)
    _goml_runtime_core_string_println(inline888)
    var seen_a__22 *ref_bool_x
    var inline885 bool = false
    var inline886 *ref_bool_x = ref__Ref_4bool(inline885)
    seen_a__22 = inline886
    var seen_b__23 *ref_bool_x
    var inline882 bool = false
    var inline883 *ref_bool_x = ref__Ref_4bool(inline882)
    seen_b__23 = inline883
    var for_limit269 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    var for_index270 int = 0
    Loop_loop337:
    for {
        var t338 bool = for_index270 < for_limit269
        if t338 {
            var for_item271 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(entries__21, for_index270)
            var t339 int = for_index270 + 1
            for_index270 = t339
            var x274 string = for_item271._0
            var x275 int32 = for_item271._1
            var t350 bool = x274 == "a"
            var jp342 bool
            if t350 {
                var t351 bool = x275 == 1
                jp342 = t351
            } else {
                jp342 = false
            }
            if jp342 {
                var inline866 bool = true
                ref_set__Ref_4bool(seen_a__22, inline866)
                continue
            } else {
                var t348 bool = x274 == "b"
                var jp346 bool
                if t348 {
                    var t349 bool = x275 == 2
                    jp346 = t349
                } else {
                    jp346 = false
                }
                if jp346 {
                    var inline869 bool = true
                    ref_set__Ref_4bool(seen_b__23, inline869)
                    continue
                } else {
                    continue
                }
            }
        } else {
            break Loop_loop337
        }
    }
    var t334 bool
    var inline880 bool = ref_get__Ref_4bool(seen_a__22)
    t334 = inline880
    var inline877 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t334)
    _goml_runtime_core_string_println(inline877)
    var t335 bool
    var inline875 bool = ref_get__Ref_4bool(seen_b__23)
    t335 = inline875
    var inline872 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t335)
    _goml_runtime_core_string_println(inline872)
    return struct{}{}
}

func println__T_char(value__1 rune) struct{} {
    var t370 string
    var inline902 string = char_to_string(value__1)
    t370 = inline902
    _goml_runtime_core_string_println(t370)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t384 string
    t384 = value__1
    _goml_runtime_core_string_println(t384)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t393 string
    var inline912 string = _goml_runtime_core_int_to_string(value__1)
    t393 = inline912
    _goml_runtime_core_string_println(t393)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t397 int = _goml_runtime_core_string_len(self__36)
    return t397
}

func _goml_m_inherent_i_string_i_string_i_len(self__35 string) int {
    var t400 int = _goml_runtime_core_string_len(self__35)
    return t400
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline914 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline915 bool = inline914._0
    var inline916 rune = inline914._1
    if inline915 {
        return inline916
    } else {
        var inline919 rune = _goml_runtime_core_string_get("", -1)
        return inline919
    }
}

func println__T_bool(value__1 bool) struct{} {
    var t405 string
    var inline921 string = _goml_runtime_core_bool_to_string(value__1)
    t405 = inline921
    _goml_runtime_core_string_println(t405)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__44 string, index__45 int) bool {
    var t409 bool = string_is_char_boundary(self__44, index__45)
    return t409
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline923 bool = string_is_char_boundary(self__41, start__42)
    var inline925 bool
    if inline923 {
        var inline928 bool = string_is_char_boundary(self__41, end__43)
        inline925 = inline928
    } else {
        inline925 = false
    }
    if inline925 {
        var inline926 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline926
    } else {
        var inline927 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline927
    }
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__46 string, index__47 int) _goml_m_Option_____o_char_c_int_q_ {
    var mtmp25 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__46, index__47)
    var x26 bool = mtmp25._0
    var x27 rune = mtmp25._1
    var x28 int = mtmp25._2
    if x26 {
        var t417 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: x27,
            _1: x28,
        }
        var t418 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t417,
        }
        return t418
    } else {
        return _goml_m_Option_____o_char_c_int_q__None{}
    }
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__51 string) *_goml_vec_uint8 {
    var t421 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__51)
    return t421
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__189 *_goml_vec_uint8) int {
    var t424 int = vec_len__Vec_5uint8(self__189)
    return t424
}

func println__T_uint8(value__1 uint8) struct{} {
    var t426 string
    var inline930 string = _goml_runtime_core_uint8_to_string(value__1)
    t426 = inline930
    _goml_runtime_core_string_println(t426)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__184 *_goml_vec_uint8, index__185 int) uint8 {
    var t430 uint8 = vec_get__Vec_5uint8(self__184, index__185)
    return t430
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop435:
    for {
        var t436 int
        var inline932 int = _goml_runtime_core_string_len(x12)
        t436 = inline932
        var t437 bool = index__26 < t436
        if t437 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t439 int = compound_old17 + x16
                index__26 = t439
                continue
            } else {
                var t441 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t441
            }
        } else {
            break Loop_loop435
        }
    }
    var t434 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t434
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t444 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t444
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__174 *_goml_vec_uint8, elem__175 uint8) struct{} {
    vec_push__Vec_5uint8(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__173 int) *_goml_vec_string {
    var t449 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__173)
    return t449
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__174 *_goml_vec_string, elem__175 string) struct{} {
    vec_push__Vec_6string(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__190 *_goml_vec_string) int {
    var t454 int = vec_capacity__Vec_6string(self__190)
    return t454
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__173 int) *_goml_vec_int32 {
    var t457 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__173)
    return t457
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__189 *_goml_vec_int32) int {
    var t460 int = vec_len__Vec_5int32(self__189)
    return t460
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__190 *_goml_vec_int32) int {
    var t463 int = vec_capacity__Vec_5int32(self__190)
    return t463
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
    var inline938 int = vec_len__Vec_5int32(self__210)
    len__213 = inline938
    var t470 bool = index__211 == len__213
    if t470 {
        vec_push__Vec_5int32(self__210, value__212)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__210, index__211)
        var t472 int = len__213 - 1
        var t473 int32 = vec_get__Vec_5int32(self__210, t472)
        vec_push__Vec_5int32(self__210, t473)
        var current__214 int = len__213 - 1
        Loop_loop476:
        for {
            var t477 bool = current__214 > index__211
            if t477 {
                var index111 int = current__214
                vec_get__Vec_5int32(self__210, index111)
                var t478 int = current__214 - 1
                var value113 int32 = vec_get__Vec_5int32(self__210, t478)
                vec_set__Vec_5int32(self__210, index111, value113)
                var compound_old115 int = current__214
                var compound_value116 int = 1
                var t480 int = compound_old115 - compound_value116
                current__214 = t480
                continue
            } else {
                break Loop_loop476
            }
        }
        vec_get__Vec_5int32(self__210, index__211)
        vec_set__Vec_5int32(self__210, index__211, value__212)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t483 string
    var inline940 string = _goml_runtime_core_int32_to_string(value__1)
    t483 = inline940
    _goml_runtime_core_string_println(t483)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__215 *_goml_vec_int32, index__216 int) int32 {
    var len__217 int
    var inline944 int = vec_len__Vec_5int32(self__215)
    len__217 = inline944
    var value__218 int32 = vec_get__Vec_5int32(self__215, index__216)
    var current__219 int = index__216
    Loop_loop489:
    for {
        var t490 int = current__219 + 1
        var t491 bool = t490 < len__217
        if t491 {
            var index125 int = current__219
            vec_get__Vec_5int32(self__215, index125)
            var t492 int = current__219 + 1
            var value127 int32 = vec_get__Vec_5int32(self__215, t492)
            vec_set__Vec_5int32(self__215, index125, value127)
            var compound_old129 int = current__219
            var compound_value130 int = 1
            var t494 int = compound_old129 + compound_value130
            current__219 = t494
            continue
        } else {
            break Loop_loop489
        }
    }
    var t488 int = len__217 - 1
    vec_truncate__Vec_5int32(self__215, t488)
    return value__218
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__206 *_goml_vec_int32, index__207 int) int32 {
    var len__208 int
    var inline948 int = vec_len__Vec_5int32(self__206)
    len__208 = inline948
    var value__209 int32 = vec_get__Vec_5int32(self__206, index__207)
    var t500 int = index__207 + 1
    var t501 bool = t500 < len__208
    if t501 {
        vec_get__Vec_5int32(self__206, index__207)
        var t502 int = len__208 - 1
        var value104 int32 = vec_get__Vec_5int32(self__206, t502)
        vec_set__Vec_5int32(self__206, index__207, value104)
    } else {}
    var t499 int = len__208 - 1
    vec_truncate__Vec_5int32(self__206, t499)
    return value__209
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__220 *_goml_vec_int32) struct{} {
    var left__221 int = 0
    var t505 int
    var inline964 int = vec_len__Vec_5int32(self__220)
    t505 = inline964
    var right__222 int = t505 - 1
    Loop_loop507:
    for {
        var t508 bool = left__221 < right__222
        if t508 {
            var inline950 int32 = vec_get__Vec_5int32(self__220, left__221)
            vec_get__Vec_5int32(self__220, left__221)
            var inline954 int32 = vec_get__Vec_5int32(self__220, right__222)
            vec_set__Vec_5int32(self__220, left__221, inline954)
            vec_get__Vec_5int32(self__220, right__222)
            vec_set__Vec_5int32(self__220, right__222, inline950)
            var compound_old135 int = left__221
            var compound_value136 int = 1
            var t509 int = compound_old135 + compound_value136
            left__221 = t509
            var compound_old138 int = right__222
            var compound_value139 int = 1
            var t511 int = compound_old138 - compound_value139
            right__222 = t511
            continue
        } else {
            break Loop_loop507
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__184 *_goml_vec_int32, index__185 int) int32 {
    var t515 int32 = vec_get__Vec_5int32(self__184, index__185)
    return t515
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__197 *_goml_vec_int32) Option__int32 {
    var len__198 int
    var inline966 int = vec_len__Vec_5int32(self__197)
    len__198 = inline966
    var t520 bool = len__198 == 0
    if t520 {
        return Option__int32_None{}
    } else {
        var t521 int = len__198 - 1
        var t522 int32 = vec_get__Vec_5int32(self__197, t521)
        var t523 Option__int32 = Option__int32_Some{
            _0: t522,
        }
        return t523
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__199 *_goml_vec_int32) Option__int32 {
    var len__200 int
    var inline970 int = vec_len__Vec_5int32(self__199)
    len__200 = inline970
    var t528 bool = len__200 == 0
    if t528 {
        return Option__int32_None{}
    } else {
        var t529 int = len__200 - 1
        var value__201 int32 = vec_get__Vec_5int32(self__199, t529)
        var t530 int = len__200 - 1
        vec_truncate__Vec_5int32(self__199, t530)
        var t531 Option__int32 = Option__int32_Some{
            _0: value__201,
        }
        return t531
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__194 *_goml_vec_int32, len__195 int) struct{} {
    vec_truncate__Vec_5int32(self__194, len__195)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__191 *_goml_vec_int32) bool {
    var t536 int = vec_len__Vec_5int32(self__191)
    var t537 bool = t536 == 0
    return t537
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__196 *_goml_vec_int32) struct{} {
    var inline972 int = 0
    vec_truncate__Vec_5int32(self__196, inline972)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t542 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t542
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__264 *hashmap_string_int32_x, key__265 string, value__266 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__264, key__265, value__266)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__272 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t547 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__272)
    return t547
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__189 *_goml_vec_Tuple2_6string_5int32) int {
    var t550 int = vec_len__Vec_21Tuple2_6string_5int32(self__189)
    return t550
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__170 func() Option__char) FnIterator__char {
    var t563 FnIterator__char = FnIterator__char{
        next_fn: next_fn__170,
    }
    return t563
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__66 rune) string {
    var inline975 uint32 = uint32(rune(self__66))
    var inline976 bool = utf8_valid_scalar(inline975)
    if inline976 {
        var inline977 string = _goml_runtime_core_char_to_string(self__66)
        return inline977
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__170 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t569 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__170,
    }
    return t569
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func char_to_string(value__29 rune) string {
    var t576 uint32 = uint32(rune(value__29))
    var t577 bool
    var inline980 bool = t576 <= 1114111
    if inline980 {
        var inline981 bool = t576 >= 55296
        var inline983 bool
        if inline981 {
            var inline985 bool = t576 <= 57343
            inline983 = inline985
        } else {
            inline983 = false
        }
        var inline984 bool = !inline983
        t577 = inline984
    } else {
        t577 = false
    }
    if t577 {
        var t578 string = _goml_runtime_core_char_to_string(value__29)
        return t578
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t581 string = _goml_runtime_core_int_to_string(self__67)
    return t581
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t589 string = _goml_runtime_core_bool_to_string(self__64)
    return t589
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t603 bool = index__16 < 0
    var jp595 bool
    if t603 {
        jp595 = true
    } else {
        var t604 int
        var inline987 int = _goml_runtime_core_string_len(value__15)
        t604 = inline987
        var t605 bool = index__16 > t604
        jp595 = t605
    }
    if jp595 {
        return false
    } else {
        var t598 int
        var inline991 int = _goml_runtime_core_string_len(value__15)
        t598 = inline991
        var t599 bool = index__16 == t598
        if t599 {
            return true
        } else {
            var t600 uint8
            var inline989 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t600 = inline989
            var t601_rhs uint8 = 192
            var t601 uint8 = t600 & t601_rhs
            var t602 bool = t601 != 128
            return t602
        }
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t734 bool = index__6 < 0
    var jp732 bool
    if t734 {
        jp732 = true
    } else {
        var t735 bool = index__6 >= length__7
        jp732 = t735
    }
    if jp732 {
        var inline993 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline993
    } else {
        var t619 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t619))
        var t622 bool = first__8 < 128
        if t622 {
            var inline995 int = 1
            var inline996 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline996.(type) {
            case Option__char_None:
                var inline997 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline997
            case Option__char_Some:
                var inline998 rune = inline996.(Option__char_Some)._0
                var inline1000 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline998,
                    _2: inline995,
                }
                return inline1000
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t626 bool = first__8 < 194
            if t626 {
                var inline1002 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1002
            } else {
                var t630 bool = first__8 < 224
                if t630 {
                    var t643 int = length__7 - index__6
                    var t644 bool = t643 < 2
                    if t644 {
                        var inline1004 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1004
                    } else {
                        var t632 int = index__6 + 1
                        var t633 uint8
                        var inline1018 uint8 = _goml_runtime_core_string_byte_get(value__5, t632)
                        t633 = inline1018
                        var second__9 uint32 = uint32(uint8(t633))
                        var t636 bool
                        var inline1015 bool = second__9 < 128
                        if inline1015 {
                            t636 = true
                        } else {
                            var inline1016 bool = second__9 > 191
                            t636 = inline1016
                        }
                        if t636 {
                            var inline1006 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1006
                        } else {
                            var t638_rhs uint32 = 31
                            var t638 uint32 = first__8 & t638_rhs
                            var t639_rhs int = 6
                            var t639 uint32 = t638 << t639_rhs
                            var t640_rhs uint32 = 63
                            var t640 uint32 = second__9 & t640_rhs
                            var t641 uint32 = t639 | t640
                            var inline1008 int = 2
                            var inline1009 Option__char = __goml_builtin_char_from_uint32(t641)
                            switch inline1009.(type) {
                            case Option__char_None:
                                var inline1010 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1010
                            case Option__char_Some:
                                var inline1011 rune = inline1009.(Option__char_Some)._0
                                var inline1013 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1011,
                                    _2: inline1008,
                                }
                                return inline1013
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t648 bool = first__8 < 240
                    if t648 {
                        var t681 int = length__7 - index__6
                        var t682 bool = t681 < 3
                        if t682 {
                            var inline1020 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1020
                        } else {
                            var t650 int = index__6 + 1
                            var t651 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t650)
                            var second__10 uint32 = uint32(uint8(t651))
                            var t652 int = index__6 + 2
                            var t653 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t652)
                            var third__11 uint32 = uint32(uint8(t653))
                            var t679 bool = utf8_invalid_continuation(second__10)
                            var jp674 bool
                            if t679 {
                                jp674 = true
                            } else {
                                var inline1022 bool = third__11 < 128
                                if inline1022 {
                                    jp674 = true
                                } else {
                                    var inline1023 bool = third__11 > 191
                                    jp674 = inline1023
                                }
                            }
                            var jp668 bool
                            if jp674 {
                                jp668 = true
                            } else {
                                var t677 bool = first__8 == 224
                                if t677 {
                                    var t678 bool = second__10 < 160
                                    jp668 = t678
                                } else {
                                    jp668 = false
                                }
                            }
                            var jp657 bool
                            if jp668 {
                                jp657 = true
                            } else {
                                var t671 bool = first__8 == 237
                                if t671 {
                                    var t672 bool = second__10 >= 160
                                    jp657 = t672
                                } else {
                                    jp657 = false
                                }
                            }
                            if jp657 {
                                var inline1025 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1025
                            } else {
                                var t659_rhs uint32 = 15
                                var t659 uint32 = first__8 & t659_rhs
                                var t660_rhs int = 12
                                var t660 uint32 = t659 << t660_rhs
                                var t661_rhs uint32 = 63
                                var t661 uint32 = second__10 & t661_rhs
                                var t662_rhs int = 6
                                var t662 uint32 = t661 << t662_rhs
                                var t663 uint32 = t660 | t662
                                var t664_rhs uint32 = 63
                                var t664 uint32 = third__11 & t664_rhs
                                var t665 uint32 = t663 | t664
                                var inline1027 int = 3
                                var inline1028 Option__char = __goml_builtin_char_from_uint32(t665)
                                switch inline1028.(type) {
                                case Option__char_None:
                                    var inline1029 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1029
                                case Option__char_Some:
                                    var inline1030 rune = inline1028.(Option__char_Some)._0
                                    var inline1032 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1030,
                                        _2: inline1027,
                                    }
                                    return inline1032
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t686 bool = first__8 < 245
                        if t686 {
                            var t727 int = length__7 - index__6
                            var t728 bool = t727 < 4
                            if t728 {
                                var t729 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t729
                            } else {
                                var t688 int = index__6 + 1
                                var t689 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t688)
                                var second__12 uint32 = uint32(uint8(t689))
                                var t690 int = index__6 + 2
                                var t691 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t690)
                                var third__13 uint32 = uint32(uint8(t691))
                                var t692 int = index__6 + 3
                                var t693 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t692)
                                var fourth__14 uint32 = uint32(uint8(t693))
                                var t725 bool = utf8_invalid_continuation(second__12)
                                var jp723 bool
                                if t725 {
                                    jp723 = true
                                } else {
                                    var t726 bool = utf8_invalid_continuation(third__13)
                                    jp723 = t726
                                }
                                var jp717 bool
                                if jp723 {
                                    jp717 = true
                                } else {
                                    var t724 bool = utf8_invalid_continuation(fourth__14)
                                    jp717 = t724
                                }
                                var jp711 bool
                                if jp717 {
                                    jp711 = true
                                } else {
                                    var t720 bool = first__8 == 240
                                    if t720 {
                                        var t721 bool = second__12 < 144
                                        jp711 = t721
                                    } else {
                                        jp711 = false
                                    }
                                }
                                var jp697 bool
                                if jp711 {
                                    jp697 = true
                                } else {
                                    var t714 bool = first__8 == 244
                                    if t714 {
                                        var t715 bool = second__12 > 143
                                        jp697 = t715
                                    } else {
                                        jp697 = false
                                    }
                                }
                                if jp697 {
                                    var t698 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t698
                                } else {
                                    var t699_rhs uint32 = 7
                                    var t699 uint32 = first__8 & t699_rhs
                                    var t700_rhs int = 18
                                    var t700 uint32 = t699 << t700_rhs
                                    var t701_rhs uint32 = 63
                                    var t701 uint32 = second__12 & t701_rhs
                                    var t702_rhs int = 12
                                    var t702 uint32 = t701 << t702_rhs
                                    var t703 uint32 = t700 | t702
                                    var t704_rhs uint32 = 63
                                    var t704 uint32 = third__13 & t704_rhs
                                    var t705_rhs int = 6
                                    var t705 uint32 = t704 << t705_rhs
                                    var t706 uint32 = t703 | t705
                                    var t707_rhs uint32 = 63
                                    var t707 uint32 = fourth__14 & t707_rhs
                                    var t708 uint32 = t706 | t707
                                    var t709 Tuple3_4bool_4char_3int = utf8_valid_decode(t708, 4)
                                    return t709
                                }
                            }
                        } else {
                            var t730 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t730
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t741 string = _goml_runtime_core_int32_to_string(self__70)
    return t741
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t749 bool = value__4 <= 1114111
    if t749 {
        var t753 bool = value__4 >= 55296
        var jp751 bool
        if t753 {
            var t754 bool = value__4 <= 57343
            jp751 = t754
        } else {
            jp751 = false
        }
        var t752 bool = !jp751
        return t752
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t757 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t757
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t760 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t760
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1075 rune
    var inline1036 bool = utf8_valid_scalar(value__0)
    if inline1036 {
        var inline1037 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1038 rune = inline1037._1
        commute_field1075 = inline1038
        var t766 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1075,
            _2: width__1,
        }
        return t766
    } else {
        var inline1034 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1034
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t771 bool = value__3 < 128
    if t771 {
        return true
    } else {
        var t772 bool = value__3 > 191
        return t772
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t777 bool
    var inline1042 bool = value__30 <= 1114111
    if inline1042 {
        var inline1043 bool = value__30 >= 55296
        var inline1045 bool
        if inline1043 {
            var inline1047 bool = value__30 <= 57343
            inline1045 = inline1047
        } else {
            inline1045 = false
        }
        var inline1046 bool = !inline1045
        t777 = inline1046
    } else {
        t777 = false
    }
    if t777 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t778 Option__char = Option__char_Some{
            _0: x24,
        }
        return t778
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__97 string, other__98 string) bool {
    var t781 bool = self__97 == other__98
    return t781
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__125 string) uint64 {
    var t784 uint64 = _goml_runtime_core_string_hash(self__125)
    return t784
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env278 closure_env_inherent_string_string_chars_0) Option__char {
    var self__52 string = env278.self_0
    var index__53 *ref_int_x = env278.index_1
    var t800 int = ref_get__Ref_3int(index__53)
    var commute_field1078 Tuple2_4char_3int
    var inline1049 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__52, t800)
    var inline1050 bool = inline1049._0
    var inline1051 rune = inline1049._1
    var inline1052 int = inline1049._2
    if inline1050 {
        var inline1056 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1051,
            _1: inline1052,
        }
        commute_field1078 = inline1056
        var x32 rune = commute_field1078._0
        var x33 int = commute_field1078._1
        var compound_old34 int = ref_get__Ref_3int(index__53)
        var t803 int = compound_old34 + x33
        ref_set__Ref_3int(index__53, t803)
        var t805 Option__char = Option__char_Some{
            _0: x32,
        }
        return t805
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env279 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var index__58 *ref_int_x = env279.index_0
    var self__57 string = env279.self_1
    var current__59 int = ref_get__Ref_3int(index__58)
    var commute_field1081 Tuple2_4char_3int
    var inline1059 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__57, current__59)
    var inline1060 bool = inline1059._0
    var inline1061 rune = inline1059._1
    var inline1062 int = inline1059._2
    if inline1060 {
        var inline1066 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1061,
            _1: inline1062,
        }
        commute_field1081 = inline1066
        var x40 rune = commute_field1081._0
        var x41 int = commute_field1081._1
        var t810 int = current__59 + x41
        ref_set__Ref_3int(index__58, t810)
        var t811 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__59,
            _1: x40,
        }
        var t812 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t811,
        }
        return t812
    } else {
        return _goml_m_Option_____o_int_c_char_q__None{}
    }
}

func main() {
    main0()
}
