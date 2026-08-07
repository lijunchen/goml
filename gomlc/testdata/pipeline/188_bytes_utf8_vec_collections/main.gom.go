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
    var t266 FnIterator__char
    var inline814 *ref_int_x = ref__Ref_3int(0)
    var inline815 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: value__0,
        index_1: inline814,
    }
    var inline816 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(inline815)
    })
    t266 = inline816
    var for_iter172 FnIterator__char
    for_iter172 = t266
    Loop_loop268:
    for {
        var for_next173 Option__char
        var inline810 func() Option__char = for_iter172.next_fn
        var inline811 Option__char = inline810()
        for_next173 = inline811
        switch for_next173.(type) {
        case Option__char_None:
            break Loop_loop268
        case Option__char_Some:
            var x174 rune = for_next173.(Option__char_Some)._0
            var inline807 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x174)
            _goml_runtime_core_string_println(inline807)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t272 _goml_m_FnIterator_____o_int_c_char_q_
    var inline829 *ref_int_x = ref__Ref_3int(0)
    var inline830 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: inline829,
        self_1: value__2,
    }
    var inline831 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(inline830)
    })
    t272 = inline831
    var for_iter175 _goml_m_FnIterator_____o_int_c_char_q_
    for_iter175 = t272
    Loop_loop274:
    for {
        var for_next176 _goml_m_Option_____o_int_c_char_q_
        var inline825 func() _goml_m_Option_____o_int_c_char_q_ = for_iter175.next_fn
        var inline826 _goml_m_Option_____o_int_c_char_q_ = inline825()
        for_next176 = inline826
        switch for_next176.(type) {
        case _goml_m_Option_____o_int_c_char_q__None:
            break Loop_loop274
        case _goml_m_Option_____o_int_c_char_q__Some:
            var x177 Tuple2_3int_4char = for_next176.(_goml_m_Option_____o_int_c_char_q__Some)._0
            var x179 int = x177._0
            var x180 rune = x177._1
            var t276 string
            var inline823 string = _goml_runtime_core_int_to_string(x179)
            t276 = inline823
            var t277 string = t276 + ":"
            var t278 string
            var inline821 string = char_to_string(x180)
            t278 = inline821
            var t279 string = t277 + t278
            var inline818 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t279)
            _goml_runtime_core_string_println(inline818)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t282 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t282)
    var t283 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t283)
    var t284 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t284)
    var t285 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t285)
    var t286 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t286)
    var t287 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t287)
    var t288 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t288)
    var t289 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t289)
    var t290 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t290)
    var t291 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t291)
    var t292 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t292)
    var t293 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t293)
    var mtmp193 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp193.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        var inline833 string = "missing"
        var inline834 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline833)
        _goml_runtime_core_string_println(inline834)
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x194 Tuple2_4char_3int = mtmp193.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var x196 rune = x194._0
        var x197 int = x194._1
        var inline840 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x196)
        _goml_runtime_core_string_println(inline840)
        var inline837 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x197)
        _goml_runtime_core_string_println(inline837)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t295 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t295)
    var t296 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t296)
    var t297 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t297)
    var mtmp205 Tuple2_4bool_6string = string_from_utf8(bytes__10)
    var x206 bool = mtmp205._0
    var x207 string = mtmp205._1
    println__T_bool(x206)
    println__T_string(x207)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp212 Tuple2_4bool_6string = string_from_utf8(invalid__13)
    var x213 bool = mtmp212._0
    var x214 string = mtmp212._1
    println__T_bool(x213)
    var t298 bool = _goml_m_trait__impl_i_PartialEq_i_string_i_eq(x214, "")
    println__T_bool(t298)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t299 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t300 bool = t299 >= 3
    println__T_bool(t300)
    var t301 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t301)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t302 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t302)
    var t303 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t304 bool = t303 >= 1
    println__T_bool(t304)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t305 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t306 bool = t305 >= 100
    println__T_bool(t306)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t307 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t307)
    var t308 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t308)
    var t309 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t309)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t310 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t310)
    var t311 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t311)
    var mtmp236 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp236.(type) {
    case Option__int32_None:
        var inline843 int = -1
        var inline844 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline843)
        _goml_runtime_core_string_println(inline844)
    case Option__int32_Some:
        var x237 int32 = mtmp236.(Option__int32_Some)._0
        var inline847 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x237)
        _goml_runtime_core_string_println(inline847)
    default:
        panic("non-exhaustive match")
    }
    var mtmp239 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp239.(type) {
    case Option__int32_None:
        var inline850 int = -1
        var inline851 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline850)
        _goml_runtime_core_string_println(inline851)
    case Option__int32_Some:
        var x240 int32 = mtmp239.(Option__int32_Some)._0
        var inline854 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x240)
        _goml_runtime_core_string_println(inline854)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t314 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t314)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t315 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t315)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t316 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t316)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t317 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t317)
    var seen_a__22 *ref_bool_x
    var inline888 bool = false
    var inline889 *ref_bool_x = ref__Ref_4bool(inline888)
    seen_a__22 = inline889
    var seen_b__23 *ref_bool_x
    var inline885 bool = false
    var inline886 *ref_bool_x = ref__Ref_4bool(inline885)
    seen_b__23 = inline886
    var for_limit254 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    var for_index255 int = 0
    Loop_loop322:
    for {
        var t323 bool = for_index255 < for_limit254
        if t323 {
            var for_item256 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(entries__21, for_index255)
            var t324 int = for_index255 + 1
            for_index255 = t324
            var x259 string = for_item256._0
            var x260 int32 = for_item256._1
            var t335 bool
            var inline872 string = "a"
            var inline873 bool = x259 == inline872
            t335 = inline873
            var jp327 bool
            if t335 {
                var inline857 int32 = 1
                var inline858 bool = x260 == inline857
                jp327 = inline858
            } else {
                jp327 = false
            }
            if jp327 {
                var inline860 bool = true
                ref_set__Ref_4bool(seen_a__22, inline860)
                continue
            } else {
                var t333 bool
                var inline869 string = "b"
                var inline870 bool = x259 == inline869
                t333 = inline870
                var jp331 bool
                if t333 {
                    var inline863 int32 = 2
                    var inline864 bool = x260 == inline863
                    jp331 = inline864
                } else {
                    jp331 = false
                }
                if jp331 {
                    var inline866 bool = true
                    ref_set__Ref_4bool(seen_b__23, inline866)
                    continue
                } else {
                    continue
                }
            }
        } else {
            break Loop_loop322
        }
    }
    var t319 bool
    var inline883 bool = ref_get__Ref_4bool(seen_a__22)
    t319 = inline883
    var inline880 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t319)
    _goml_runtime_core_string_println(inline880)
    var t320 bool
    var inline878 bool = ref_get__Ref_4bool(seen_b__23)
    t320 = inline878
    var inline875 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t320)
    _goml_runtime_core_string_println(inline875)
    return struct{}{}
}

func println__T_char(value__31 rune) struct{} {
    var t354 string
    var inline891 string = char_to_string(value__31)
    t354 = inline891
    _goml_runtime_core_string_println(t354)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t367 string
    t367 = value__31
    _goml_runtime_core_string_println(t367)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t376 string
    var inline899 string = _goml_runtime_core_int_to_string(value__31)
    t376 = inline899
    _goml_runtime_core_string_println(t376)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t380 int = _goml_runtime_core_string_len(self__38)
    return t380
}

func _goml_m_inherent_i_string_i_string_i_len(self__37 string) int {
    var t383 int = _goml_runtime_core_string_len(self__37)
    return t383
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline901 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline902 bool = inline901._0
    var inline903 rune = inline901._1
    if inline902 {
        return inline903
    } else {
        var inline907 rune = _goml_runtime_core_string_get("", -1)
        return inline907
    }
}

func println__T_bool(value__31 bool) struct{} {
    var t388 string
    var inline909 string = _goml_runtime_core_bool_to_string(value__31)
    t388 = inline909
    _goml_runtime_core_string_println(t388)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__46 string, index__47 int) bool {
    var t392 bool = string_is_char_boundary(self__46, index__47)
    return t392
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline911 bool = string_is_char_boundary(self__43, start__44)
    var inline913 bool
    if inline911 {
        var inline916 bool = string_is_char_boundary(self__43, end__45)
        inline913 = inline916
    } else {
        inline913 = false
    }
    if inline913 {
        var inline914 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline914
    } else {
        var inline915 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline915
    }
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__48 string, index__49 int) _goml_m_Option_____o_char_c_int_q_ {
    var mtmp25 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__48, index__49)
    var x26 bool = mtmp25._0
    var x27 rune = mtmp25._1
    var x28 int = mtmp25._2
    if x26 {
        var t400 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: x27,
            _1: x28,
        }
        var t401 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t400,
        }
        return t401
    } else {
        return _goml_m_Option_____o_char_c_int_q__None{}
    }
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__53 string) *_goml_vec_uint8 {
    var t404 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__53)
    return t404
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__187 *_goml_vec_uint8) int {
    var t407 int = vec_len__Vec_5uint8(self__187)
    return t407
}

func println__T_uint8(value__31 uint8) struct{} {
    var t409 string
    var inline918 string = _goml_runtime_core_uint8_to_string(value__31)
    t409 = inline918
    _goml_runtime_core_string_println(t409)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__182 *_goml_vec_uint8, index__183 int) uint8 {
    var t413 uint8 = vec_get__Vec_5uint8(self__182, index__183)
    return t413
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop418:
    for {
        var t419 int
        var inline920 int = _goml_runtime_core_string_len(x12)
        t419 = inline920
        var t420 bool = index__26 < t419
        if t420 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t422 int = compound_old17 + x16
                index__26 = t422
                continue
            } else {
                var t424 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t424
            }
        } else {
            break Loop_loop418
        }
    }
    var t417 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t417
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t427 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t427
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__176 *_goml_vec_uint8, elem__177 uint8) struct{} {
    vec_push__Vec_5uint8(self__176, elem__177)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__99 string, other__100 string) bool {
    var t432 bool = self__99 == other__100
    return t432
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__175 int) *_goml_vec_string {
    var t435 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__175)
    return t435
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__176 *_goml_vec_string, elem__177 string) struct{} {
    vec_push__Vec_6string(self__176, elem__177)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__188 *_goml_vec_string) int {
    var t440 int = vec_capacity__Vec_6string(self__188)
    return t440
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__175 int) *_goml_vec_int32 {
    var t443 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__175)
    return t443
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__187 *_goml_vec_int32) int {
    var t446 int = vec_len__Vec_5int32(self__187)
    return t446
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__188 *_goml_vec_int32) int {
    var t449 int = vec_capacity__Vec_5int32(self__188)
    return t449
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__190 *_goml_vec_int32, additional__191 int) struct{} {
    vec_reserve__Vec_5int32(self__190, additional__191)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__176 *_goml_vec_int32, elem__177 int32) struct{} {
    vec_push__Vec_5int32(self__176, elem__177)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(self__208 *_goml_vec_int32, index__209 int, value__210 int32) struct{} {
    var len__211 int
    var inline928 int = vec_len__Vec_5int32(self__208)
    len__211 = inline928
    var t456 bool
    var inline926 bool = index__209 == len__211
    t456 = inline926
    if t456 {
        vec_push__Vec_5int32(self__208, value__210)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__208, index__209)
        var t458 int = len__211 - 1
        var t459 int32 = vec_get__Vec_5int32(self__208, t458)
        vec_push__Vec_5int32(self__208, t459)
        var current__212 int = len__211 - 1
        Loop_loop462:
        for {
            var t463 bool = current__212 > index__209
            if t463 {
                var index106 int = current__212
                vec_get__Vec_5int32(self__208, index106)
                var t464 int = current__212 - 1
                var value108 int32 = vec_get__Vec_5int32(self__208, t464)
                vec_set__Vec_5int32(self__208, index106, value108)
                var compound_old110 int = current__212
                var compound_value111 int = 1
                var t466 int = compound_old110 - compound_value111
                current__212 = t466
                continue
            } else {
                break Loop_loop462
            }
        }
        vec_get__Vec_5int32(self__208, index__209)
        vec_set__Vec_5int32(self__208, index__209, value__210)
        return struct{}{}
    }
}

func println__T_int32(value__31 int32) struct{} {
    var t469 string
    var inline930 string = _goml_runtime_core_int32_to_string(value__31)
    t469 = inline930
    _goml_runtime_core_string_println(t469)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__213 *_goml_vec_int32, index__214 int) int32 {
    var len__215 int
    var inline934 int = vec_len__Vec_5int32(self__213)
    len__215 = inline934
    var value__216 int32 = vec_get__Vec_5int32(self__213, index__214)
    var current__217 int = index__214
    Loop_loop475:
    for {
        var t476 int = current__217 + 1
        var t477 bool = t476 < len__215
        if t477 {
            var index120 int = current__217
            vec_get__Vec_5int32(self__213, index120)
            var t478 int = current__217 + 1
            var value122 int32 = vec_get__Vec_5int32(self__213, t478)
            vec_set__Vec_5int32(self__213, index120, value122)
            var compound_old124 int = current__217
            var compound_value125 int = 1
            var t480 int = compound_old124 + compound_value125
            current__217 = t480
            continue
        } else {
            break Loop_loop475
        }
    }
    var t474 int = len__215 - 1
    vec_truncate__Vec_5int32(self__213, t474)
    return value__216
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__204 *_goml_vec_int32, index__205 int) int32 {
    var len__206 int
    var inline938 int = vec_len__Vec_5int32(self__204)
    len__206 = inline938
    var value__207 int32 = vec_get__Vec_5int32(self__204, index__205)
    var t486 int = index__205 + 1
    var t487 bool = t486 < len__206
    if t487 {
        vec_get__Vec_5int32(self__204, index__205)
        var t488 int = len__206 - 1
        var value99 int32 = vec_get__Vec_5int32(self__204, t488)
        vec_set__Vec_5int32(self__204, index__205, value99)
    } else {}
    var t485 int = len__206 - 1
    vec_truncate__Vec_5int32(self__204, t485)
    return value__207
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__218 *_goml_vec_int32) struct{} {
    var left__219 int = 0
    var t491 int
    var inline954 int = vec_len__Vec_5int32(self__218)
    t491 = inline954
    var right__220 int = t491 - 1
    Loop_loop493:
    for {
        var t494 bool = left__219 < right__220
        if t494 {
            var inline940 int32 = vec_get__Vec_5int32(self__218, left__219)
            vec_get__Vec_5int32(self__218, left__219)
            var inline944 int32 = vec_get__Vec_5int32(self__218, right__220)
            vec_set__Vec_5int32(self__218, left__219, inline944)
            vec_get__Vec_5int32(self__218, right__220)
            vec_set__Vec_5int32(self__218, right__220, inline940)
            var compound_old130 int = left__219
            var compound_value131 int = 1
            var t495 int = compound_old130 + compound_value131
            left__219 = t495
            var compound_old133 int = right__220
            var compound_value134 int = 1
            var t497 int = compound_old133 - compound_value134
            right__220 = t497
            continue
        } else {
            break Loop_loop493
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__182 *_goml_vec_int32, index__183 int) int32 {
    var t501 int32 = vec_get__Vec_5int32(self__182, index__183)
    return t501
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__195 *_goml_vec_int32) Option__int32 {
    var len__196 int
    var inline959 int = vec_len__Vec_5int32(self__195)
    len__196 = inline959
    var t506 bool
    var inline956 int = 0
    var inline957 bool = len__196 == inline956
    t506 = inline957
    if t506 {
        return Option__int32_None{}
    } else {
        var t507 int = len__196 - 1
        var t508 int32 = vec_get__Vec_5int32(self__195, t507)
        var t509 Option__int32 = Option__int32_Some{
            _0: t508,
        }
        return t509
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__197 *_goml_vec_int32) Option__int32 {
    var len__198 int
    var inline966 int = vec_len__Vec_5int32(self__197)
    len__198 = inline966
    var t514 bool
    var inline963 int = 0
    var inline964 bool = len__198 == inline963
    t514 = inline964
    if t514 {
        return Option__int32_None{}
    } else {
        var t515 int = len__198 - 1
        var value__199 int32 = vec_get__Vec_5int32(self__197, t515)
        var t516 int = len__198 - 1
        vec_truncate__Vec_5int32(self__197, t516)
        var t517 Option__int32 = Option__int32_Some{
            _0: value__199,
        }
        return t517
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__192 *_goml_vec_int32, len__193 int) struct{} {
    vec_truncate__Vec_5int32(self__192, len__193)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__189 *_goml_vec_int32) bool {
    var t522 int = vec_len__Vec_5int32(self__189)
    var inline968 int = 0
    var inline969 bool = t522 == inline968
    return inline969
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__194 *_goml_vec_int32) struct{} {
    var inline971 int = 0
    vec_truncate__Vec_5int32(self__194, inline971)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t528 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t528
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__248 *hashmap_string_int32_x, key__249 string, value__250 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__248, key__249, value__250)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__256 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t533 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__256)
    return t533
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__187 *_goml_vec_Tuple2_6string_5int32) int {
    var t536 int = vec_len__Vec_21Tuple2_6string_5int32(self__187)
    return t536
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__251 *hashmap_string_int32_x, key__252 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__251, key__252)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__172 func() Option__char) FnIterator__char {
    var t552 FnIterator__char = FnIterator__char{
        next_fn: next_fn__172,
    }
    return t552
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__68 rune) string {
    var inline974 uint32 = uint32(rune(self__68))
    var inline975 bool = utf8_valid_scalar(inline974)
    if inline975 {
        var inline976 string = _goml_runtime_core_char_to_string(self__68)
        return inline976
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__172 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t558 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__172,
    }
    return t558
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func char_to_string(value__29 rune) string {
    var t565 uint32 = uint32(rune(value__29))
    var t566 bool
    var inline979 bool = t565 <= 1114111
    if inline979 {
        var inline980 bool = t565 >= 55296
        var inline982 bool
        if inline980 {
            var inline984 bool = t565 <= 57343
            inline982 = inline984
        } else {
            inline982 = false
        }
        var inline983 bool = !inline982
        t566 = inline983
    } else {
        t566 = false
    }
    if t566 {
        var t567 string = _goml_runtime_core_char_to_string(value__29)
        return t567
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t570 string = _goml_runtime_core_int_to_string(self__69)
    return t570
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t578 string = _goml_runtime_core_bool_to_string(self__66)
    return t578
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t593 bool = index__16 < 0
    var jp584 bool
    if t593 {
        jp584 = true
    } else {
        var t594 int
        var inline986 int = _goml_runtime_core_string_len(value__15)
        t594 = inline986
        var t595 bool = index__16 > t594
        jp584 = t595
    }
    if jp584 {
        return false
    } else {
        var t587 int
        var inline995 int = _goml_runtime_core_string_len(value__15)
        t587 = inline995
        var t588 bool
        var inline993 bool = index__16 == t587
        t588 = inline993
        if t588 {
            return true
        } else {
            var t589 uint8
            var inline991 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t589 = inline991
            var t590_rhs uint8 = 192
            var t590 uint8 = t589 & t590_rhs
            var t591 bool
            var inline988 uint8 = 128
            var inline989 bool = t590 == inline988
            t591 = inline989
            var t592 bool = !t591
            return t592
        }
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t724 bool = index__6 < 0
    var jp722 bool
    if t724 {
        jp722 = true
    } else {
        var t725 bool = index__6 >= length__7
        jp722 = t725
    }
    if jp722 {
        var inline997 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline997
    } else {
        var t609 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t609))
        var t612 bool = first__8 < 128
        if t612 {
            var inline999 int = 1
            var inline1000 Option__char = char_from_uint32(first__8)
            switch inline1000.(type) {
            case Option__char_None:
                var inline1001 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1001
            case Option__char_Some:
                var inline1002 rune = inline1000.(Option__char_Some)._0
                var inline1004 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1002,
                    _2: inline999,
                }
                return inline1004
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t616 bool = first__8 < 194
            if t616 {
                var inline1006 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1006
            } else {
                var t620 bool = first__8 < 224
                if t620 {
                    var t633 int = length__7 - index__6
                    var t634 bool = t633 < 2
                    if t634 {
                        var inline1008 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1008
                    } else {
                        var t622 int = index__6 + 1
                        var t623 uint8
                        var inline1022 uint8 = _goml_runtime_core_string_byte_get(value__5, t622)
                        t623 = inline1022
                        var second__9 uint32 = uint32(uint8(t623))
                        var t626 bool
                        var inline1019 bool = second__9 < 128
                        if inline1019 {
                            t626 = true
                        } else {
                            var inline1020 bool = second__9 > 191
                            t626 = inline1020
                        }
                        if t626 {
                            var inline1010 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1010
                        } else {
                            var t628_rhs uint32 = 31
                            var t628 uint32 = first__8 & t628_rhs
                            var t629_rhs int = 6
                            var t629 uint32 = t628 << t629_rhs
                            var t630_rhs uint32 = 63
                            var t630 uint32 = second__9 & t630_rhs
                            var t631 uint32 = t629 | t630
                            var inline1012 int = 2
                            var inline1013 Option__char = char_from_uint32(t631)
                            switch inline1013.(type) {
                            case Option__char_None:
                                var inline1014 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1014
                            case Option__char_Some:
                                var inline1015 rune = inline1013.(Option__char_Some)._0
                                var inline1017 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1015,
                                    _2: inline1012,
                                }
                                return inline1017
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t638 bool = first__8 < 240
                    if t638 {
                        var t671 int = length__7 - index__6
                        var t672 bool = t671 < 3
                        if t672 {
                            var inline1024 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1024
                        } else {
                            var t640 int = index__6 + 1
                            var t641 uint8
                            var inline1039 uint8 = _goml_runtime_core_string_byte_get(value__5, t640)
                            t641 = inline1039
                            var second__10 uint32 = uint32(uint8(t641))
                            var t642 int = index__6 + 2
                            var t643 uint8
                            var inline1037 uint8 = _goml_runtime_core_string_byte_get(value__5, t642)
                            t643 = inline1037
                            var third__11 uint32 = uint32(uint8(t643))
                            var t669 bool = utf8_invalid_continuation(second__10)
                            var jp664 bool
                            if t669 {
                                jp664 = true
                            } else {
                                var inline1026 bool = third__11 < 128
                                if inline1026 {
                                    jp664 = true
                                } else {
                                    var inline1027 bool = third__11 > 191
                                    jp664 = inline1027
                                }
                            }
                            var jp658 bool
                            if jp664 {
                                jp658 = true
                            } else {
                                var t667 bool
                                var inline1029 uint32 = 224
                                var inline1030 bool = first__8 == inline1029
                                t667 = inline1030
                                if t667 {
                                    var t668 bool = second__10 < 160
                                    jp658 = t668
                                } else {
                                    jp658 = false
                                }
                            }
                            var jp647 bool
                            if jp658 {
                                jp647 = true
                            } else {
                                var t661 bool
                                var inline1032 uint32 = 237
                                var inline1033 bool = first__8 == inline1032
                                t661 = inline1033
                                if t661 {
                                    var t662 bool = second__10 >= 160
                                    jp647 = t662
                                } else {
                                    jp647 = false
                                }
                            }
                            if jp647 {
                                var inline1035 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1035
                            } else {
                                var t649_rhs uint32 = 15
                                var t649 uint32 = first__8 & t649_rhs
                                var t650_rhs int = 12
                                var t650 uint32 = t649 << t650_rhs
                                var t651_rhs uint32 = 63
                                var t651 uint32 = second__10 & t651_rhs
                                var t652_rhs int = 6
                                var t652 uint32 = t651 << t652_rhs
                                var t653 uint32 = t650 | t652
                                var t654_rhs uint32 = 63
                                var t654 uint32 = third__11 & t654_rhs
                                var t655 uint32 = t653 | t654
                                var t656 Tuple3_4bool_4char_3int = utf8_valid_decode(t655, 3)
                                return t656
                            }
                        }
                    } else {
                        var t676 bool = first__8 < 245
                        if t676 {
                            var t717 int = length__7 - index__6
                            var t718 bool = t717 < 4
                            if t718 {
                                var t719 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t719
                            } else {
                                var t678 int = index__6 + 1
                                var t679 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t678)
                                var second__12 uint32 = uint32(uint8(t679))
                                var t680 int = index__6 + 2
                                var t681 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t680)
                                var third__13 uint32 = uint32(uint8(t681))
                                var t682 int = index__6 + 3
                                var t683 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t682)
                                var fourth__14 uint32 = uint32(uint8(t683))
                                var t715 bool = utf8_invalid_continuation(second__12)
                                var jp713 bool
                                if t715 {
                                    jp713 = true
                                } else {
                                    var t716 bool = utf8_invalid_continuation(third__13)
                                    jp713 = t716
                                }
                                var jp707 bool
                                if jp713 {
                                    jp707 = true
                                } else {
                                    var t714 bool = utf8_invalid_continuation(fourth__14)
                                    jp707 = t714
                                }
                                var jp701 bool
                                if jp707 {
                                    jp701 = true
                                } else {
                                    var t710 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t710 {
                                        var t711 bool = second__12 < 144
                                        jp701 = t711
                                    } else {
                                        jp701 = false
                                    }
                                }
                                var jp687 bool
                                if jp701 {
                                    jp687 = true
                                } else {
                                    var t704 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t704 {
                                        var t705 bool = second__12 > 143
                                        jp687 = t705
                                    } else {
                                        jp687 = false
                                    }
                                }
                                if jp687 {
                                    var t688 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t688
                                } else {
                                    var t689_rhs uint32 = 7
                                    var t689 uint32 = first__8 & t689_rhs
                                    var t690_rhs int = 18
                                    var t690 uint32 = t689 << t690_rhs
                                    var t691_rhs uint32 = 63
                                    var t691 uint32 = second__12 & t691_rhs
                                    var t692_rhs int = 12
                                    var t692 uint32 = t691 << t692_rhs
                                    var t693 uint32 = t690 | t692
                                    var t694_rhs uint32 = 63
                                    var t694 uint32 = third__13 & t694_rhs
                                    var t695_rhs int = 6
                                    var t695 uint32 = t694 << t695_rhs
                                    var t696 uint32 = t693 | t695
                                    var t697_rhs uint32 = 63
                                    var t697 uint32 = fourth__14 & t697_rhs
                                    var t698 uint32 = t696 | t697
                                    var t699 Tuple3_4bool_4char_3int = utf8_valid_decode(t698, 4)
                                    return t699
                                }
                            }
                        } else {
                            var t720 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t720
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t734 string = _goml_runtime_core_int32_to_string(self__72)
    return t734
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t742 bool = value__4 <= 1114111
    if t742 {
        var t746 bool = value__4 >= 55296
        var jp744 bool
        if t746 {
            var t747 bool = value__4 <= 57343
            jp744 = t747
        } else {
            jp744 = false
        }
        var t745 bool = !jp744
        return t745
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t750 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t750
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t756 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t756
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1083 rune
    var inline1043 bool = utf8_valid_scalar(value__0)
    if inline1043 {
        var inline1044 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1046 rune = inline1044._1
        commute_field1083 = inline1046
        var t762 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1083,
            _2: width__1,
        }
        return t762
    } else {
        var inline1041 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1041
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t767 bool = value__3 < 128
    if t767 {
        return true
    } else {
        var t768 bool = value__3 > 191
        return t768
    }
}

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t771 bool = self__117 == other__118
    return t771
}

func char_from_uint32(value__32 uint32) Option__char {
    var t776 bool
    var inline1050 bool = value__32 <= 1114111
    if inline1050 {
        var inline1051 bool = value__32 >= 55296
        var inline1053 bool
        if inline1051 {
            var inline1055 bool = value__32 <= 57343
            inline1053 = inline1055
        } else {
            inline1053 = false
        }
        var inline1054 bool = !inline1053
        t776 = inline1054
    } else {
        t776 = false
    }
    if t776 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t777 Option__char = Option__char_Some{
            _0: x24,
        }
        return t777
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__127 string) uint64 {
    var t780 uint64 = _goml_runtime_core_string_hash(self__127)
    return t780
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env263 closure_env_inherent_string_string_chars_0) Option__char {
    var self__54 string = env263.self_0
    var index__55 *ref_int_x = env263.index_1
    var t793 int = ref_get__Ref_3int(index__55)
    var commute_field1086 Tuple2_4char_3int
    var inline1057 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__54, t793)
    var inline1058 bool = inline1057._0
    var inline1059 rune = inline1057._1
    var inline1060 int = inline1057._2
    if inline1058 {
        var inline1064 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1059,
            _1: inline1060,
        }
        commute_field1086 = inline1064
        var x32 rune = commute_field1086._0
        var x33 int = commute_field1086._1
        var compound_old34 int = ref_get__Ref_3int(index__55)
        var t796 int = compound_old34 + x33
        ref_set__Ref_3int(index__55, t796)
        var t798 Option__char = Option__char_Some{
            _0: x32,
        }
        return t798
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env264 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var index__60 *ref_int_x = env264.index_0
    var self__59 string = env264.self_1
    var current__61 int = ref_get__Ref_3int(index__60)
    var commute_field1089 Tuple2_4char_3int
    var inline1067 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__59, current__61)
    var inline1068 bool = inline1067._0
    var inline1069 rune = inline1067._1
    var inline1070 int = inline1067._2
    if inline1068 {
        var inline1074 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1069,
            _1: inline1070,
        }
        commute_field1089 = inline1074
        var x40 rune = commute_field1089._0
        var x41 int = commute_field1089._1
        var t803 int = current__61 + x41
        ref_set__Ref_3int(index__60, t803)
        var t804 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__61,
            _1: x40,
        }
        var t805 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t804,
        }
        return t805
    } else {
        return _goml_m_Option_____o_int_c_char_q__None{}
    }
}

func main() {
    main0()
}
