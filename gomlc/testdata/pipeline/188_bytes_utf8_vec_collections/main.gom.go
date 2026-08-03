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
        if entry.active && _goml_m_trait__impl_i_Eq_i_string_i_eq(entry.key, key) {
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
        if entry.active && _goml_m_trait__impl_i_Eq_i_string_i_eq(entry.key, key) {
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
    var t271 FnIterator__char
    var inline819 *ref_int_x = ref__Ref_3int(0)
    var inline820 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: value__0,
        index_1: inline819,
    }
    var inline821 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(inline820)
    })
    t271 = inline821
    var for_iter177 FnIterator__char
    for_iter177 = t271
    Loop_loop273:
    for {
        var for_next178 Option__char
        var inline815 func() Option__char = for_iter177.next_fn
        var inline816 Option__char = inline815()
        for_next178 = inline816
        switch for_next178.(type) {
        case Option__char_None:
            break Loop_loop273
        case Option__char_Some:
            var x179 rune = for_next178.(Option__char_Some)._0
            var inline812 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x179)
            _goml_runtime_core_string_println(inline812)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t277 _goml_m_FnIterator_____o_int_c_char_q_
    var inline834 *ref_int_x = ref__Ref_3int(0)
    var inline835 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: inline834,
        self_1: value__2,
    }
    var inline836 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(inline835)
    })
    t277 = inline836
    var for_iter180 _goml_m_FnIterator_____o_int_c_char_q_
    for_iter180 = t277
    Loop_loop279:
    for {
        var for_next181 _goml_m_Option_____o_int_c_char_q_
        var inline830 func() _goml_m_Option_____o_int_c_char_q_ = for_iter180.next_fn
        var inline831 _goml_m_Option_____o_int_c_char_q_ = inline830()
        for_next181 = inline831
        switch for_next181.(type) {
        case _goml_m_Option_____o_int_c_char_q__None:
            break Loop_loop279
        case _goml_m_Option_____o_int_c_char_q__Some:
            var x182 Tuple2_3int_4char = for_next181.(_goml_m_Option_____o_int_c_char_q__Some)._0
            var x184 int = x182._0
            var x185 rune = x182._1
            var t281 string
            var inline828 string = _goml_runtime_core_int_to_string(x184)
            t281 = inline828
            var t282 string = t281 + ":"
            var t283 string
            var inline826 string = char_to_string(x185)
            t283 = inline826
            var t284 string = t282 + t283
            var inline823 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t284)
            _goml_runtime_core_string_println(inline823)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t287 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t287)
    var t288 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t288)
    var t289 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t289)
    var t290 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t290)
    var t291 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t291)
    var t292 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t292)
    var t293 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t293)
    var t294 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t294)
    var t295 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t295)
    var t296 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t296)
    var t297 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t297)
    var t298 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t298)
    var mtmp198 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp198.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        var inline838 string = "missing"
        var inline839 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline838)
        _goml_runtime_core_string_println(inline839)
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x199 Tuple2_4char_3int = mtmp198.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var x201 rune = x199._0
        var x202 int = x199._1
        var inline845 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x201)
        _goml_runtime_core_string_println(inline845)
        var inline842 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x202)
        _goml_runtime_core_string_println(inline842)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t300 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t300)
    var t301 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t301)
    var t302 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t302)
    var mtmp210 Tuple2_4bool_6string = string_from_utf8(bytes__10)
    var x211 bool = mtmp210._0
    var x212 string = mtmp210._1
    println__T_bool(x211)
    println__T_string(x212)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp217 Tuple2_4bool_6string = string_from_utf8(invalid__13)
    var x218 bool = mtmp217._0
    var x219 string = mtmp217._1
    println__T_bool(x218)
    var t303 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(x219, "")
    println__T_bool(t303)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t304 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t305 bool = t304 >= 3
    println__T_bool(t305)
    var t306 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t306)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t307 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t307)
    var t308 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t309 bool = t308 >= 1
    println__T_bool(t309)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t310 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t311 bool = t310 >= 100
    println__T_bool(t311)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t312 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t312)
    var t313 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t313)
    var t314 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t314)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t315 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t315)
    var t316 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t316)
    var mtmp241 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp241.(type) {
    case Option__int32_None:
        var inline848 int = -1
        var inline849 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline848)
        _goml_runtime_core_string_println(inline849)
    case Option__int32_Some:
        var x242 int32 = mtmp241.(Option__int32_Some)._0
        var inline852 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x242)
        _goml_runtime_core_string_println(inline852)
    default:
        panic("non-exhaustive match")
    }
    var mtmp244 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp244.(type) {
    case Option__int32_None:
        var inline855 int = -1
        var inline856 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline855)
        _goml_runtime_core_string_println(inline856)
    case Option__int32_Some:
        var x245 int32 = mtmp244.(Option__int32_Some)._0
        var inline859 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x245)
        _goml_runtime_core_string_println(inline859)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t319 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t319)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t320 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t320)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t321 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t321)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t322 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t322)
    var seen_a__22 *ref_bool_x
    var inline893 bool = false
    var inline894 *ref_bool_x = ref__Ref_4bool(inline893)
    seen_a__22 = inline894
    var seen_b__23 *ref_bool_x
    var inline890 bool = false
    var inline891 *ref_bool_x = ref__Ref_4bool(inline890)
    seen_b__23 = inline891
    var for_limit259 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    var for_index260 int = 0
    Loop_loop327:
    for {
        var t328 bool = for_index260 < for_limit259
        if t328 {
            var for_item261 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(entries__21, for_index260)
            var t329 int = for_index260 + 1
            for_index260 = t329
            var x264 string = for_item261._0
            var x265 int32 = for_item261._1
            var t340 bool
            var inline877 string = "a"
            var inline878 bool = x264 == inline877
            t340 = inline878
            var jp332 bool
            if t340 {
                var inline862 int32 = 1
                var inline863 bool = x265 == inline862
                jp332 = inline863
            } else {
                jp332 = false
            }
            if jp332 {
                var inline865 bool = true
                ref_set__Ref_4bool(seen_a__22, inline865)
                continue
            } else {
                var t338 bool
                var inline874 string = "b"
                var inline875 bool = x264 == inline874
                t338 = inline875
                var jp336 bool
                if t338 {
                    var inline868 int32 = 2
                    var inline869 bool = x265 == inline868
                    jp336 = inline869
                } else {
                    jp336 = false
                }
                if jp336 {
                    var inline871 bool = true
                    ref_set__Ref_4bool(seen_b__23, inline871)
                    continue
                } else {
                    continue
                }
            }
        } else {
            break Loop_loop327
        }
    }
    var t324 bool
    var inline888 bool = ref_get__Ref_4bool(seen_a__22)
    t324 = inline888
    var inline885 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t324)
    _goml_runtime_core_string_println(inline885)
    var t325 bool
    var inline883 bool = ref_get__Ref_4bool(seen_b__23)
    t325 = inline883
    var inline880 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t325)
    _goml_runtime_core_string_println(inline880)
    return struct{}{}
}

func println__T_char(value__31 rune) struct{} {
    var t359 string
    var inline896 string = char_to_string(value__31)
    t359 = inline896
    _goml_runtime_core_string_println(t359)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t372 string
    t372 = value__31
    _goml_runtime_core_string_println(t372)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t381 string
    var inline904 string = _goml_runtime_core_int_to_string(value__31)
    t381 = inline904
    _goml_runtime_core_string_println(t381)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t385 int = _goml_runtime_core_string_len(self__38)
    return t385
}

func _goml_m_inherent_i_string_i_string_i_len(self__37 string) int {
    var t388 int = _goml_runtime_core_string_len(self__37)
    return t388
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline906 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline907 bool = inline906._0
    var inline908 rune = inline906._1
    if inline907 {
        return inline908
    } else {
        var inline912 rune = _goml_runtime_core_string_get("", -1)
        return inline912
    }
}

func println__T_bool(value__31 bool) struct{} {
    var t393 string
    var inline914 string = _goml_runtime_core_bool_to_string(value__31)
    t393 = inline914
    _goml_runtime_core_string_println(t393)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__46 string, index__47 int) bool {
    var t397 bool = string_is_char_boundary(self__46, index__47)
    return t397
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline916 bool = string_is_char_boundary(self__43, start__44)
    var inline918 bool
    if inline916 {
        var inline921 bool = string_is_char_boundary(self__43, end__45)
        inline918 = inline921
    } else {
        inline918 = false
    }
    if inline918 {
        var inline919 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline919
    } else {
        var inline920 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline920
    }
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__48 string, index__49 int) _goml_m_Option_____o_char_c_int_q_ {
    var mtmp25 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__48, index__49)
    var x26 bool = mtmp25._0
    var x27 rune = mtmp25._1
    var x28 int = mtmp25._2
    if x26 {
        var t405 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: x27,
            _1: x28,
        }
        var t406 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t405,
        }
        return t406
    } else {
        return _goml_m_Option_____o_char_c_int_q__None{}
    }
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__53 string) *_goml_vec_uint8 {
    var t409 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__53)
    return t409
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__166 *_goml_vec_uint8) int {
    var t412 int = vec_len__Vec_5uint8(self__166)
    return t412
}

func println__T_uint8(value__31 uint8) struct{} {
    var t414 string
    var inline923 string = _goml_runtime_core_uint8_to_string(value__31)
    t414 = inline923
    _goml_runtime_core_string_println(t414)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__161 *_goml_vec_uint8, index__162 int) uint8 {
    var t418 uint8 = vec_get__Vec_5uint8(self__161, index__162)
    return t418
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop423:
    for {
        var t424 int
        var inline925 int = _goml_runtime_core_string_len(x12)
        t424 = inline925
        var t425 bool = index__26 < t424
        if t425 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t427 int = compound_old17 + x16
                index__26 = t427
                continue
            } else {
                var t429 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t429
            }
        } else {
            break Loop_loop423
        }
    }
    var t422 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t422
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t432 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t432
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__155 *_goml_vec_uint8, elem__156 uint8) struct{} {
    vec_push__Vec_5uint8(self__155, elem__156)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__84 string, other__85 string) bool {
    var t437 bool = self__84 == other__85
    return t437
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__154 int) *_goml_vec_string {
    var t440 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__154)
    return t440
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__155 *_goml_vec_string, elem__156 string) struct{} {
    vec_push__Vec_6string(self__155, elem__156)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__167 *_goml_vec_string) int {
    var t445 int = vec_capacity__Vec_6string(self__167)
    return t445
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__154 int) *_goml_vec_int32 {
    var t448 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__154)
    return t448
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__166 *_goml_vec_int32) int {
    var t451 int = vec_len__Vec_5int32(self__166)
    return t451
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__167 *_goml_vec_int32) int {
    var t454 int = vec_capacity__Vec_5int32(self__167)
    return t454
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__169 *_goml_vec_int32, additional__170 int) struct{} {
    vec_reserve__Vec_5int32(self__169, additional__170)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__155 *_goml_vec_int32, elem__156 int32) struct{} {
    vec_push__Vec_5int32(self__155, elem__156)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(self__187 *_goml_vec_int32, index__188 int, value__189 int32) struct{} {
    var len__190 int
    var inline933 int = vec_len__Vec_5int32(self__187)
    len__190 = inline933
    var t461 bool
    var inline931 bool = index__188 == len__190
    t461 = inline931
    if t461 {
        vec_push__Vec_5int32(self__187, value__189)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__187, index__188)
        var t463 int = len__190 - 1
        var t464 int32 = vec_get__Vec_5int32(self__187, t463)
        vec_push__Vec_5int32(self__187, t464)
        var current__191 int = len__190 - 1
        Loop_loop467:
        for {
            var t468 bool = current__191 > index__188
            if t468 {
                var index86 int = current__191
                vec_get__Vec_5int32(self__187, index86)
                var t469 int = current__191 - 1
                var value88 int32 = vec_get__Vec_5int32(self__187, t469)
                vec_set__Vec_5int32(self__187, index86, value88)
                var compound_old90 int = current__191
                var compound_value91 int = 1
                var t471 int = compound_old90 - compound_value91
                current__191 = t471
                continue
            } else {
                break Loop_loop467
            }
        }
        vec_get__Vec_5int32(self__187, index__188)
        vec_set__Vec_5int32(self__187, index__188, value__189)
        return struct{}{}
    }
}

func println__T_int32(value__31 int32) struct{} {
    var t474 string
    var inline935 string = _goml_runtime_core_int32_to_string(value__31)
    t474 = inline935
    _goml_runtime_core_string_println(t474)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__192 *_goml_vec_int32, index__193 int) int32 {
    var len__194 int
    var inline939 int = vec_len__Vec_5int32(self__192)
    len__194 = inline939
    var value__195 int32 = vec_get__Vec_5int32(self__192, index__193)
    var current__196 int = index__193
    Loop_loop480:
    for {
        var t481 int = current__196 + 1
        var t482 bool = t481 < len__194
        if t482 {
            var index100 int = current__196
            vec_get__Vec_5int32(self__192, index100)
            var t483 int = current__196 + 1
            var value102 int32 = vec_get__Vec_5int32(self__192, t483)
            vec_set__Vec_5int32(self__192, index100, value102)
            var compound_old104 int = current__196
            var compound_value105 int = 1
            var t485 int = compound_old104 + compound_value105
            current__196 = t485
            continue
        } else {
            break Loop_loop480
        }
    }
    var t479 int = len__194 - 1
    vec_truncate__Vec_5int32(self__192, t479)
    return value__195
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__183 *_goml_vec_int32, index__184 int) int32 {
    var len__185 int
    var inline943 int = vec_len__Vec_5int32(self__183)
    len__185 = inline943
    var value__186 int32 = vec_get__Vec_5int32(self__183, index__184)
    var t491 int = index__184 + 1
    var t492 bool = t491 < len__185
    if t492 {
        vec_get__Vec_5int32(self__183, index__184)
        var t493 int = len__185 - 1
        var value79 int32 = vec_get__Vec_5int32(self__183, t493)
        vec_set__Vec_5int32(self__183, index__184, value79)
    } else {}
    var t490 int = len__185 - 1
    vec_truncate__Vec_5int32(self__183, t490)
    return value__186
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__197 *_goml_vec_int32) struct{} {
    var left__198 int = 0
    var t496 int
    var inline959 int = vec_len__Vec_5int32(self__197)
    t496 = inline959
    var right__199 int = t496 - 1
    Loop_loop498:
    for {
        var t499 bool = left__198 < right__199
        if t499 {
            var inline945 int32 = vec_get__Vec_5int32(self__197, left__198)
            vec_get__Vec_5int32(self__197, left__198)
            var inline949 int32 = vec_get__Vec_5int32(self__197, right__199)
            vec_set__Vec_5int32(self__197, left__198, inline949)
            vec_get__Vec_5int32(self__197, right__199)
            vec_set__Vec_5int32(self__197, right__199, inline945)
            var compound_old110 int = left__198
            var compound_value111 int = 1
            var t500 int = compound_old110 + compound_value111
            left__198 = t500
            var compound_old113 int = right__199
            var compound_value114 int = 1
            var t502 int = compound_old113 - compound_value114
            right__199 = t502
            continue
        } else {
            break Loop_loop498
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__161 *_goml_vec_int32, index__162 int) int32 {
    var t506 int32 = vec_get__Vec_5int32(self__161, index__162)
    return t506
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__174 *_goml_vec_int32) Option__int32 {
    var len__175 int
    var inline964 int = vec_len__Vec_5int32(self__174)
    len__175 = inline964
    var t511 bool
    var inline961 int = 0
    var inline962 bool = len__175 == inline961
    t511 = inline962
    if t511 {
        return Option__int32_None{}
    } else {
        var t512 int = len__175 - 1
        var t513 int32 = vec_get__Vec_5int32(self__174, t512)
        var t514 Option__int32 = Option__int32_Some{
            _0: t513,
        }
        return t514
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__176 *_goml_vec_int32) Option__int32 {
    var len__177 int
    var inline971 int = vec_len__Vec_5int32(self__176)
    len__177 = inline971
    var t519 bool
    var inline968 int = 0
    var inline969 bool = len__177 == inline968
    t519 = inline969
    if t519 {
        return Option__int32_None{}
    } else {
        var t520 int = len__177 - 1
        var value__178 int32 = vec_get__Vec_5int32(self__176, t520)
        var t521 int = len__177 - 1
        vec_truncate__Vec_5int32(self__176, t521)
        var t522 Option__int32 = Option__int32_Some{
            _0: value__178,
        }
        return t522
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__171 *_goml_vec_int32, len__172 int) struct{} {
    vec_truncate__Vec_5int32(self__171, len__172)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__168 *_goml_vec_int32) bool {
    var t527 int = vec_len__Vec_5int32(self__168)
    var inline973 int = 0
    var inline974 bool = t527 == inline973
    return inline974
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__173 *_goml_vec_int32) struct{} {
    var inline976 int = 0
    vec_truncate__Vec_5int32(self__173, inline976)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t533 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t533
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__227 *hashmap_string_int32_x, key__228 string, value__229 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__227, key__228, value__229)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__235 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t538 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__235)
    return t538
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__166 *_goml_vec_Tuple2_6string_5int32) int {
    var t541 int = vec_len__Vec_21Tuple2_6string_5int32(self__166)
    return t541
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__230 *hashmap_string_int32_x, key__231 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__230, key__231)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__130 func() Option__char) FnIterator__char {
    var t557 FnIterator__char = FnIterator__char{
        next_fn: next_fn__130,
    }
    return t557
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__68 rune) string {
    var inline979 uint32 = uint32(rune(self__68))
    var inline980 bool = utf8_valid_scalar(inline979)
    if inline980 {
        var inline981 string = _goml_runtime_core_char_to_string(self__68)
        return inline981
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__130 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t563 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__130,
    }
    return t563
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func char_to_string(value__29 rune) string {
    var t570 uint32 = uint32(rune(value__29))
    var t571 bool
    var inline984 bool = t570 <= 1114111
    if inline984 {
        var inline985 bool = t570 >= 55296
        var inline987 bool
        if inline985 {
            var inline989 bool = t570 <= 57343
            inline987 = inline989
        } else {
            inline987 = false
        }
        var inline988 bool = !inline987
        t571 = inline988
    } else {
        t571 = false
    }
    if t571 {
        var t572 string = _goml_runtime_core_char_to_string(value__29)
        return t572
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t575 string = _goml_runtime_core_int_to_string(self__69)
    return t575
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t583 string = _goml_runtime_core_bool_to_string(self__66)
    return t583
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t598 bool = index__16 < 0
    var jp589 bool
    if t598 {
        jp589 = true
    } else {
        var t599 int
        var inline991 int = _goml_runtime_core_string_len(value__15)
        t599 = inline991
        var t600 bool = index__16 > t599
        jp589 = t600
    }
    if jp589 {
        return false
    } else {
        var t592 int
        var inline1000 int = _goml_runtime_core_string_len(value__15)
        t592 = inline1000
        var t593 bool
        var inline998 bool = index__16 == t592
        t593 = inline998
        if t593 {
            return true
        } else {
            var t594 uint8
            var inline996 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t594 = inline996
            var t595_rhs uint8 = 192
            var t595 uint8 = t594 & t595_rhs
            var t596 bool
            var inline993 uint8 = 128
            var inline994 bool = t595 == inline993
            t596 = inline994
            var t597 bool = !t596
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
        var inline1002 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1002
    } else {
        var t614 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t614))
        var t617 bool = first__8 < 128
        if t617 {
            var inline1004 int = 1
            var inline1005 Option__char = char_from_uint32(first__8)
            switch inline1005.(type) {
            case Option__char_None:
                var inline1006 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1006
            case Option__char_Some:
                var inline1007 rune = inline1005.(Option__char_Some)._0
                var inline1009 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1007,
                    _2: inline1004,
                }
                return inline1009
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t621 bool = first__8 < 194
            if t621 {
                var inline1011 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1011
            } else {
                var t625 bool = first__8 < 224
                if t625 {
                    var t638 int = length__7 - index__6
                    var t639 bool = t638 < 2
                    if t639 {
                        var inline1013 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1013
                    } else {
                        var t627 int = index__6 + 1
                        var t628 uint8
                        var inline1027 uint8 = _goml_runtime_core_string_byte_get(value__5, t627)
                        t628 = inline1027
                        var second__9 uint32 = uint32(uint8(t628))
                        var t631 bool
                        var inline1024 bool = second__9 < 128
                        if inline1024 {
                            t631 = true
                        } else {
                            var inline1025 bool = second__9 > 191
                            t631 = inline1025
                        }
                        if t631 {
                            var inline1015 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1015
                        } else {
                            var t633_rhs uint32 = 31
                            var t633 uint32 = first__8 & t633_rhs
                            var t634_rhs int = 6
                            var t634 uint32 = t633 << t634_rhs
                            var t635_rhs uint32 = 63
                            var t635 uint32 = second__9 & t635_rhs
                            var t636 uint32 = t634 | t635
                            var inline1017 int = 2
                            var inline1018 Option__char = char_from_uint32(t636)
                            switch inline1018.(type) {
                            case Option__char_None:
                                var inline1019 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1019
                            case Option__char_Some:
                                var inline1020 rune = inline1018.(Option__char_Some)._0
                                var inline1022 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1020,
                                    _2: inline1017,
                                }
                                return inline1022
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
                            var inline1029 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1029
                        } else {
                            var t645 int = index__6 + 1
                            var t646 uint8
                            var inline1044 uint8 = _goml_runtime_core_string_byte_get(value__5, t645)
                            t646 = inline1044
                            var second__10 uint32 = uint32(uint8(t646))
                            var t647 int = index__6 + 2
                            var t648 uint8
                            var inline1042 uint8 = _goml_runtime_core_string_byte_get(value__5, t647)
                            t648 = inline1042
                            var third__11 uint32 = uint32(uint8(t648))
                            var t674 bool = utf8_invalid_continuation(second__10)
                            var jp669 bool
                            if t674 {
                                jp669 = true
                            } else {
                                var inline1031 bool = third__11 < 128
                                if inline1031 {
                                    jp669 = true
                                } else {
                                    var inline1032 bool = third__11 > 191
                                    jp669 = inline1032
                                }
                            }
                            var jp663 bool
                            if jp669 {
                                jp663 = true
                            } else {
                                var t672 bool
                                var inline1034 uint32 = 224
                                var inline1035 bool = first__8 == inline1034
                                t672 = inline1035
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
                                var t666 bool
                                var inline1037 uint32 = 237
                                var inline1038 bool = first__8 == inline1037
                                t666 = inline1038
                                if t666 {
                                    var t667 bool = second__10 >= 160
                                    jp652 = t667
                                } else {
                                    jp652 = false
                                }
                            }
                            if jp652 {
                                var inline1040 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1040
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
                                var t661 Tuple3_4bool_4char_3int = utf8_valid_decode(t660, 3)
                                return t661
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
                                    var t715 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
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
                                    var t709 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
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

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t739 string = _goml_runtime_core_int32_to_string(self__72)
    return t739
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t747 bool = value__4 <= 1114111
    if t747 {
        var t751 bool = value__4 >= 55296
        var jp749 bool
        if t751 {
            var t752 bool = value__4 <= 57343
            jp749 = t752
        } else {
            jp749 = false
        }
        var t750 bool = !jp749
        return t750
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t755 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t755
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t761 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t761
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1088 rune
    var inline1048 bool = utf8_valid_scalar(value__0)
    if inline1048 {
        var inline1049 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1051 rune = inline1049._1
        commute_field1088 = inline1051
        var t767 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1088,
            _2: width__1,
        }
        return t767
    } else {
        var inline1046 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1046
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t772 bool = value__3 < 128
    if t772 {
        return true
    } else {
        var t773 bool = value__3 > 191
        return t773
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t776 bool = self__102 == other__103
    return t776
}

func char_from_uint32(value__32 uint32) Option__char {
    var t781 bool
    var inline1055 bool = value__32 <= 1114111
    if inline1055 {
        var inline1056 bool = value__32 >= 55296
        var inline1058 bool
        if inline1056 {
            var inline1060 bool = value__32 <= 57343
            inline1058 = inline1060
        } else {
            inline1058 = false
        }
        var inline1059 bool = !inline1058
        t781 = inline1059
    } else {
        t781 = false
    }
    if t781 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t782 Option__char = Option__char_Some{
            _0: x24,
        }
        return t782
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__112 string) uint64 {
    var t785 uint64 = _goml_runtime_core_string_hash(self__112)
    return t785
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env268 closure_env_inherent_string_string_chars_0) Option__char {
    var self__54 string = env268.self_0
    var index__55 *ref_int_x = env268.index_1
    var t798 int = ref_get__Ref_3int(index__55)
    var commute_field1091 Tuple2_4char_3int
    var inline1062 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__54, t798)
    var inline1063 bool = inline1062._0
    var inline1064 rune = inline1062._1
    var inline1065 int = inline1062._2
    if inline1063 {
        var inline1069 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1064,
            _1: inline1065,
        }
        commute_field1091 = inline1069
        var x32 rune = commute_field1091._0
        var x33 int = commute_field1091._1
        var compound_old34 int = ref_get__Ref_3int(index__55)
        var t801 int = compound_old34 + x33
        ref_set__Ref_3int(index__55, t801)
        var t803 Option__char = Option__char_Some{
            _0: x32,
        }
        return t803
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env269 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var index__60 *ref_int_x = env269.index_0
    var self__59 string = env269.self_1
    var current__61 int = ref_get__Ref_3int(index__60)
    var commute_field1094 Tuple2_4char_3int
    var inline1072 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__59, current__61)
    var inline1073 bool = inline1072._0
    var inline1074 rune = inline1072._1
    var inline1075 int = inline1072._2
    if inline1073 {
        var inline1079 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1074,
            _1: inline1075,
        }
        commute_field1094 = inline1079
        var x40 rune = commute_field1094._0
        var x41 int = commute_field1094._1
        var t808 int = current__61 + x41
        ref_set__Ref_3int(index__60, t808)
        var t809 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__61,
            _1: x40,
        }
        var t810 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t809,
        }
        return t810
    } else {
        return _goml_m_Option_____o_int_c_char_q__None{}
    }
}

func main() {
    main0()
}
