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
    var inline806 *ref_int_x = ref__Ref_3int(0)
    var inline807 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: value__0,
        index_1: inline806,
    }
    var inline808 func() Option__char = func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(inline807)
    }
    var inline809 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(inline808)
    t266 = inline809
    var for_iter172 FnIterator__char
    for_iter172 = t266
    Loop_loop268:
    for {
        var for_next173 Option__char
        var inline802 func() Option__char = for_iter172.next_fn
        var inline803 Option__char = inline802()
        for_next173 = inline803
        switch for_next173.(type) {
        case Option__char_None:
            break Loop_loop268
        case Option__char_Some:
            var x174 rune = for_next173.(Option__char_Some)._0
            var inline799 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x174)
            _goml_runtime_core_string_println(inline799)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t272 _goml_m_FnIterator_____o_int_c_char_q_
    var inline822 *ref_int_x = ref__Ref_3int(0)
    var inline823 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: inline822,
        self_1: value__2,
    }
    var inline824 func() _goml_m_Option_____o_int_c_char_q_ = func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(inline823)
    }
    var inline825 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(inline824)
    t272 = inline825
    var for_iter175 _goml_m_FnIterator_____o_int_c_char_q_
    for_iter175 = t272
    Loop_loop274:
    for {
        var for_next176 _goml_m_Option_____o_int_c_char_q_
        var inline818 func() _goml_m_Option_____o_int_c_char_q_ = for_iter175.next_fn
        var inline819 _goml_m_Option_____o_int_c_char_q_ = inline818()
        for_next176 = inline819
        switch for_next176.(type) {
        case _goml_m_Option_____o_int_c_char_q__None:
            break Loop_loop274
        case _goml_m_Option_____o_int_c_char_q__Some:
            var x177 Tuple2_3int_4char = for_next176.(_goml_m_Option_____o_int_c_char_q__Some)._0
            var x179 int = x177._0
            var x180 rune = x177._1
            var t276 string
            var inline816 string = _goml_runtime_core_int_to_string(x179)
            t276 = inline816
            var t277 string = t276 + ":"
            var t278 string
            var inline814 string = char_to_string(x180)
            t278 = inline814
            var t279 string = t277 + t278
            var inline811 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t279)
            _goml_runtime_core_string_println(inline811)
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
        var inline827 string = "missing"
        var inline828 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline827)
        _goml_runtime_core_string_println(inline828)
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x194 Tuple2_4char_3int = mtmp193.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var x196 rune = x194._0
        var x197 int = x194._1
        var inline834 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x196)
        _goml_runtime_core_string_println(inline834)
        var inline831 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x197)
        _goml_runtime_core_string_println(inline831)
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
    var t298 bool = x214 == ""
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
        var inline837 int = -1
        var inline838 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline837)
        _goml_runtime_core_string_println(inline838)
    case Option__int32_Some:
        var x237 int32 = mtmp236.(Option__int32_Some)._0
        var inline841 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x237)
        _goml_runtime_core_string_println(inline841)
    default:
        panic("non-exhaustive match")
    }
    var mtmp239 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp239.(type) {
    case Option__int32_None:
        var inline844 int = -1
        var inline845 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline844)
        _goml_runtime_core_string_println(inline845)
    case Option__int32_Some:
        var x240 int32 = mtmp239.(Option__int32_Some)._0
        var inline848 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x240)
        _goml_runtime_core_string_println(inline848)
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
    var inline881 string = "c"
    var inline882 int32 = 3
    hashmap_set__HashMap_6string_5int32(map__20, inline881, inline882)
    var inline878 string = "a"
    hashmap_remove__HashMap_6string_5int32(map__20, inline878)
    var t317 int
    var inline876 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    t317 = inline876
    var inline873 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t317)
    _goml_runtime_core_string_println(inline873)
    var seen_a__22 *ref_bool_x
    var inline870 bool = false
    var inline871 *ref_bool_x = ref__Ref_4bool(inline870)
    seen_a__22 = inline871
    var seen_b__23 *ref_bool_x
    var inline867 bool = false
    var inline868 *ref_bool_x = ref__Ref_4bool(inline867)
    seen_b__23 = inline868
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
            var t335 bool = x259 == "a"
            var jp327 bool
            if t335 {
                var t336 bool = x260 == 1
                jp327 = t336
            } else {
                jp327 = false
            }
            if jp327 {
                var inline851 bool = true
                ref_set__Ref_4bool(seen_a__22, inline851)
                continue
            } else {
                var t333 bool = x259 == "b"
                var jp331 bool
                if t333 {
                    var t334 bool = x260 == 2
                    jp331 = t334
                } else {
                    jp331 = false
                }
                if jp331 {
                    var inline854 bool = true
                    ref_set__Ref_4bool(seen_b__23, inline854)
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
    var inline865 bool = ref_get__Ref_4bool(seen_a__22)
    t319 = inline865
    var inline862 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t319)
    _goml_runtime_core_string_println(inline862)
    var t320 bool
    var inline860 bool = ref_get__Ref_4bool(seen_b__23)
    t320 = inline860
    var inline857 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t320)
    _goml_runtime_core_string_println(inline857)
    return struct{}{}
}

func println__T_char(value__1 rune) struct{} {
    var t355 string
    var inline887 string = char_to_string(value__1)
    t355 = inline887
    _goml_runtime_core_string_println(t355)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t369 string
    t369 = value__1
    _goml_runtime_core_string_println(t369)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t378 string
    var inline897 string = _goml_runtime_core_int_to_string(value__1)
    t378 = inline897
    _goml_runtime_core_string_println(t378)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t382 int = _goml_runtime_core_string_len(self__36)
    return t382
}

func _goml_m_inherent_i_string_i_string_i_len(self__35 string) int {
    var t385 int = _goml_runtime_core_string_len(self__35)
    return t385
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline899 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline900 bool = inline899._0
    var inline901 rune = inline899._1
    if inline900 {
        return inline901
    } else {
        var inline904 rune = _goml_runtime_core_string_get("", -1)
        return inline904
    }
}

func println__T_bool(value__1 bool) struct{} {
    var t390 string
    var inline906 string = _goml_runtime_core_bool_to_string(value__1)
    t390 = inline906
    _goml_runtime_core_string_println(t390)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__44 string, index__45 int) bool {
    var t394 bool = string_is_char_boundary(self__44, index__45)
    return t394
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline908 bool = string_is_char_boundary(self__41, start__42)
    var inline910 bool
    if inline908 {
        var inline913 bool = string_is_char_boundary(self__41, end__43)
        inline910 = inline913
    } else {
        inline910 = false
    }
    if inline910 {
        var inline911 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline911
    } else {
        var inline912 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline912
    }
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__46 string, index__47 int) _goml_m_Option_____o_char_c_int_q_ {
    var mtmp25 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__46, index__47)
    var x26 bool = mtmp25._0
    var x27 rune = mtmp25._1
    var x28 int = mtmp25._2
    if x26 {
        var t402 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: x27,
            _1: x28,
        }
        var t403 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t402,
        }
        return t403
    } else {
        return _goml_m_Option_____o_char_c_int_q__None{}
    }
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__51 string) *_goml_vec_uint8 {
    var t406 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__51)
    return t406
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__185 *_goml_vec_uint8) int {
    var t409 int = vec_len__Vec_5uint8(self__185)
    return t409
}

func println__T_uint8(value__1 uint8) struct{} {
    var t411 string
    var inline915 string = _goml_runtime_core_uint8_to_string(value__1)
    t411 = inline915
    _goml_runtime_core_string_println(t411)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__180 *_goml_vec_uint8, index__181 int) uint8 {
    var t415 uint8 = vec_get__Vec_5uint8(self__180, index__181)
    return t415
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop420:
    for {
        var t421 int
        var inline917 int = _goml_runtime_core_string_len(x12)
        t421 = inline917
        var t422 bool = index__26 < t421
        if t422 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t424 int = compound_old17 + x16
                index__26 = t424
                continue
            } else {
                var t426 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t426
            }
        } else {
            break Loop_loop420
        }
    }
    var t419 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t419
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t429 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t429
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__174 *_goml_vec_uint8, elem__175 uint8) struct{} {
    vec_push__Vec_5uint8(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__173 int) *_goml_vec_string {
    var t434 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__173)
    return t434
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__174 *_goml_vec_string, elem__175 string) struct{} {
    vec_push__Vec_6string(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__186 *_goml_vec_string) int {
    var t439 int = vec_capacity__Vec_6string(self__186)
    return t439
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__173 int) *_goml_vec_int32 {
    var t442 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__173)
    return t442
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__185 *_goml_vec_int32) int {
    var t445 int = vec_len__Vec_5int32(self__185)
    return t445
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__186 *_goml_vec_int32) int {
    var t448 int = vec_capacity__Vec_5int32(self__186)
    return t448
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__188 *_goml_vec_int32, additional__189 int) struct{} {
    vec_reserve__Vec_5int32(self__188, additional__189)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__174 *_goml_vec_int32, elem__175 int32) struct{} {
    vec_push__Vec_5int32(self__174, elem__175)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(self__206 *_goml_vec_int32, index__207 int, value__208 int32) struct{} {
    var len__209 int
    var inline923 int = vec_len__Vec_5int32(self__206)
    len__209 = inline923
    var t455 bool = index__207 == len__209
    if t455 {
        vec_push__Vec_5int32(self__206, value__208)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__206, index__207)
        var t457 int = len__209 - 1
        var t458 int32 = vec_get__Vec_5int32(self__206, t457)
        vec_push__Vec_5int32(self__206, t458)
        var current__210 int = len__209 - 1
        Loop_loop461:
        for {
            var t462 bool = current__210 > index__207
            if t462 {
                var index106 int = current__210
                vec_get__Vec_5int32(self__206, index106)
                var t463 int = current__210 - 1
                var value108 int32 = vec_get__Vec_5int32(self__206, t463)
                vec_set__Vec_5int32(self__206, index106, value108)
                var compound_old110 int = current__210
                var compound_value111 int = 1
                var t465 int = compound_old110 - compound_value111
                current__210 = t465
                continue
            } else {
                break Loop_loop461
            }
        }
        vec_get__Vec_5int32(self__206, index__207)
        vec_set__Vec_5int32(self__206, index__207, value__208)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t468 string
    var inline925 string = _goml_runtime_core_int32_to_string(value__1)
    t468 = inline925
    _goml_runtime_core_string_println(t468)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__211 *_goml_vec_int32, index__212 int) int32 {
    var len__213 int
    var inline929 int = vec_len__Vec_5int32(self__211)
    len__213 = inline929
    var value__214 int32 = vec_get__Vec_5int32(self__211, index__212)
    var current__215 int = index__212
    Loop_loop474:
    for {
        var t475 int = current__215 + 1
        var t476 bool = t475 < len__213
        if t476 {
            var index120 int = current__215
            vec_get__Vec_5int32(self__211, index120)
            var t477 int = current__215 + 1
            var value122 int32 = vec_get__Vec_5int32(self__211, t477)
            vec_set__Vec_5int32(self__211, index120, value122)
            var compound_old124 int = current__215
            var compound_value125 int = 1
            var t479 int = compound_old124 + compound_value125
            current__215 = t479
            continue
        } else {
            break Loop_loop474
        }
    }
    var t473 int = len__213 - 1
    vec_truncate__Vec_5int32(self__211, t473)
    return value__214
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__202 *_goml_vec_int32, index__203 int) int32 {
    var len__204 int
    var inline933 int = vec_len__Vec_5int32(self__202)
    len__204 = inline933
    var value__205 int32 = vec_get__Vec_5int32(self__202, index__203)
    var t485 int = index__203 + 1
    var t486 bool = t485 < len__204
    if t486 {
        vec_get__Vec_5int32(self__202, index__203)
        var t487 int = len__204 - 1
        var value99 int32 = vec_get__Vec_5int32(self__202, t487)
        vec_set__Vec_5int32(self__202, index__203, value99)
    } else {}
    var t484 int = len__204 - 1
    vec_truncate__Vec_5int32(self__202, t484)
    return value__205
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__216 *_goml_vec_int32) struct{} {
    var left__217 int = 0
    var t490 int
    var inline949 int = vec_len__Vec_5int32(self__216)
    t490 = inline949
    var right__218 int = t490 - 1
    Loop_loop492:
    for {
        var t493 bool = left__217 < right__218
        if t493 {
            var inline935 int32 = vec_get__Vec_5int32(self__216, left__217)
            vec_get__Vec_5int32(self__216, left__217)
            var inline939 int32 = vec_get__Vec_5int32(self__216, right__218)
            vec_set__Vec_5int32(self__216, left__217, inline939)
            vec_get__Vec_5int32(self__216, right__218)
            vec_set__Vec_5int32(self__216, right__218, inline935)
            var compound_old130 int = left__217
            var compound_value131 int = 1
            var t494 int = compound_old130 + compound_value131
            left__217 = t494
            var compound_old133 int = right__218
            var compound_value134 int = 1
            var t496 int = compound_old133 - compound_value134
            right__218 = t496
            continue
        } else {
            break Loop_loop492
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__180 *_goml_vec_int32, index__181 int) int32 {
    var t500 int32 = vec_get__Vec_5int32(self__180, index__181)
    return t500
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__193 *_goml_vec_int32) Option__int32 {
    var len__194 int
    var inline951 int = vec_len__Vec_5int32(self__193)
    len__194 = inline951
    var t505 bool = len__194 == 0
    if t505 {
        return Option__int32_None{}
    } else {
        var t506 int = len__194 - 1
        var t507 int32 = vec_get__Vec_5int32(self__193, t506)
        var t508 Option__int32 = Option__int32_Some{
            _0: t507,
        }
        return t508
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__195 *_goml_vec_int32) Option__int32 {
    var len__196 int
    var inline955 int = vec_len__Vec_5int32(self__195)
    len__196 = inline955
    var t513 bool = len__196 == 0
    if t513 {
        return Option__int32_None{}
    } else {
        var t514 int = len__196 - 1
        var value__197 int32 = vec_get__Vec_5int32(self__195, t514)
        var t515 int = len__196 - 1
        vec_truncate__Vec_5int32(self__195, t515)
        var t516 Option__int32 = Option__int32_Some{
            _0: value__197,
        }
        return t516
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__190 *_goml_vec_int32, len__191 int) struct{} {
    vec_truncate__Vec_5int32(self__190, len__191)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__187 *_goml_vec_int32) bool {
    var t521 int = vec_len__Vec_5int32(self__187)
    var t522 bool = t521 == 0
    return t522
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__192 *_goml_vec_int32) struct{} {
    var inline957 int = 0
    vec_truncate__Vec_5int32(self__192, inline957)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t527 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t527
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__246 *hashmap_string_int32_x, key__247 string, value__248 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__246, key__247, value__248)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__254 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t532 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__254)
    return t532
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__185 *_goml_vec_Tuple2_6string_5int32) int {
    var t535 int = vec_len__Vec_21Tuple2_6string_5int32(self__185)
    return t535
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__170 func() Option__char) FnIterator__char {
    var t548 FnIterator__char = FnIterator__char{
        next_fn: next_fn__170,
    }
    return t548
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__66 rune) string {
    var inline960 uint32 = uint32(rune(self__66))
    var inline961 bool = utf8_valid_scalar(inline960)
    if inline961 {
        var inline962 string = _goml_runtime_core_char_to_string(self__66)
        return inline962
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__170 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t554 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__170,
    }
    return t554
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func char_to_string(value__29 rune) string {
    var t561 uint32 = uint32(rune(value__29))
    var t562 bool
    var inline965 bool = t561 <= 1114111
    if inline965 {
        var inline966 bool = t561 >= 55296
        var inline968 bool
        if inline966 {
            var inline970 bool = t561 <= 57343
            inline968 = inline970
        } else {
            inline968 = false
        }
        var inline969 bool = !inline968
        t562 = inline969
    } else {
        t562 = false
    }
    if t562 {
        var t563 string = _goml_runtime_core_char_to_string(value__29)
        return t563
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t566 string = _goml_runtime_core_int_to_string(self__67)
    return t566
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t574 string = _goml_runtime_core_bool_to_string(self__64)
    return t574
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t588 bool = index__16 < 0
    var jp580 bool
    if t588 {
        jp580 = true
    } else {
        var t589 int
        var inline972 int = _goml_runtime_core_string_len(value__15)
        t589 = inline972
        var t590 bool = index__16 > t589
        jp580 = t590
    }
    if jp580 {
        return false
    } else {
        var t583 int
        var inline976 int = _goml_runtime_core_string_len(value__15)
        t583 = inline976
        var t584 bool = index__16 == t583
        if t584 {
            return true
        } else {
            var t585 uint8
            var inline974 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t585 = inline974
            var t586_rhs uint8 = 192
            var t586 uint8 = t585 & t586_rhs
            var t587 bool = t586 != 128
            return t587
        }
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t719 bool = index__6 < 0
    var jp717 bool
    if t719 {
        jp717 = true
    } else {
        var t720 bool = index__6 >= length__7
        jp717 = t720
    }
    if jp717 {
        var inline978 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline978
    } else {
        var t604 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t604))
        var t607 bool = first__8 < 128
        if t607 {
            var inline980 int = 1
            var inline981 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline981.(type) {
            case Option__char_None:
                var inline982 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline982
            case Option__char_Some:
                var inline983 rune = inline981.(Option__char_Some)._0
                var inline985 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline983,
                    _2: inline980,
                }
                return inline985
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t611 bool = first__8 < 194
            if t611 {
                var inline987 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline987
            } else {
                var t615 bool = first__8 < 224
                if t615 {
                    var t628 int = length__7 - index__6
                    var t629 bool = t628 < 2
                    if t629 {
                        var inline989 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline989
                    } else {
                        var t617 int = index__6 + 1
                        var t618 uint8
                        var inline1003 uint8 = _goml_runtime_core_string_byte_get(value__5, t617)
                        t618 = inline1003
                        var second__9 uint32 = uint32(uint8(t618))
                        var t621 bool
                        var inline1000 bool = second__9 < 128
                        if inline1000 {
                            t621 = true
                        } else {
                            var inline1001 bool = second__9 > 191
                            t621 = inline1001
                        }
                        if t621 {
                            var inline991 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline991
                        } else {
                            var t623_rhs uint32 = 31
                            var t623 uint32 = first__8 & t623_rhs
                            var t624_rhs int = 6
                            var t624 uint32 = t623 << t624_rhs
                            var t625_rhs uint32 = 63
                            var t625 uint32 = second__9 & t625_rhs
                            var t626 uint32 = t624 | t625
                            var inline993 int = 2
                            var inline994 Option__char = __goml_builtin_char_from_uint32(t626)
                            switch inline994.(type) {
                            case Option__char_None:
                                var inline995 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline995
                            case Option__char_Some:
                                var inline996 rune = inline994.(Option__char_Some)._0
                                var inline998 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline996,
                                    _2: inline993,
                                }
                                return inline998
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t633 bool = first__8 < 240
                    if t633 {
                        var t666 int = length__7 - index__6
                        var t667 bool = t666 < 3
                        if t667 {
                            var inline1005 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1005
                        } else {
                            var t635 int = index__6 + 1
                            var t636 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t635)
                            var second__10 uint32 = uint32(uint8(t636))
                            var t637 int = index__6 + 2
                            var t638 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t637)
                            var third__11 uint32 = uint32(uint8(t638))
                            var t664 bool = utf8_invalid_continuation(second__10)
                            var jp659 bool
                            if t664 {
                                jp659 = true
                            } else {
                                var inline1007 bool = third__11 < 128
                                if inline1007 {
                                    jp659 = true
                                } else {
                                    var inline1008 bool = third__11 > 191
                                    jp659 = inline1008
                                }
                            }
                            var jp653 bool
                            if jp659 {
                                jp653 = true
                            } else {
                                var t662 bool = first__8 == 224
                                if t662 {
                                    var t663 bool = second__10 < 160
                                    jp653 = t663
                                } else {
                                    jp653 = false
                                }
                            }
                            var jp642 bool
                            if jp653 {
                                jp642 = true
                            } else {
                                var t656 bool = first__8 == 237
                                if t656 {
                                    var t657 bool = second__10 >= 160
                                    jp642 = t657
                                } else {
                                    jp642 = false
                                }
                            }
                            if jp642 {
                                var inline1010 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1010
                            } else {
                                var t644_rhs uint32 = 15
                                var t644 uint32 = first__8 & t644_rhs
                                var t645_rhs int = 12
                                var t645 uint32 = t644 << t645_rhs
                                var t646_rhs uint32 = 63
                                var t646 uint32 = second__10 & t646_rhs
                                var t647_rhs int = 6
                                var t647 uint32 = t646 << t647_rhs
                                var t648 uint32 = t645 | t647
                                var t649_rhs uint32 = 63
                                var t649 uint32 = third__11 & t649_rhs
                                var t650 uint32 = t648 | t649
                                var inline1012 int = 3
                                var inline1013 Option__char = __goml_builtin_char_from_uint32(t650)
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
                        var t671 bool = first__8 < 245
                        if t671 {
                            var t712 int = length__7 - index__6
                            var t713 bool = t712 < 4
                            if t713 {
                                var t714 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t714
                            } else {
                                var t673 int = index__6 + 1
                                var t674 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t673)
                                var second__12 uint32 = uint32(uint8(t674))
                                var t675 int = index__6 + 2
                                var t676 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t675)
                                var third__13 uint32 = uint32(uint8(t676))
                                var t677 int = index__6 + 3
                                var t678 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t677)
                                var fourth__14 uint32 = uint32(uint8(t678))
                                var t710 bool = utf8_invalid_continuation(second__12)
                                var jp708 bool
                                if t710 {
                                    jp708 = true
                                } else {
                                    var t711 bool = utf8_invalid_continuation(third__13)
                                    jp708 = t711
                                }
                                var jp702 bool
                                if jp708 {
                                    jp702 = true
                                } else {
                                    var t709 bool = utf8_invalid_continuation(fourth__14)
                                    jp702 = t709
                                }
                                var jp696 bool
                                if jp702 {
                                    jp696 = true
                                } else {
                                    var t705 bool = first__8 == 240
                                    if t705 {
                                        var t706 bool = second__12 < 144
                                        jp696 = t706
                                    } else {
                                        jp696 = false
                                    }
                                }
                                var jp682 bool
                                if jp696 {
                                    jp682 = true
                                } else {
                                    var t699 bool = first__8 == 244
                                    if t699 {
                                        var t700 bool = second__12 > 143
                                        jp682 = t700
                                    } else {
                                        jp682 = false
                                    }
                                }
                                if jp682 {
                                    var t683 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t683
                                } else {
                                    var t684_rhs uint32 = 7
                                    var t684 uint32 = first__8 & t684_rhs
                                    var t685_rhs int = 18
                                    var t685 uint32 = t684 << t685_rhs
                                    var t686_rhs uint32 = 63
                                    var t686 uint32 = second__12 & t686_rhs
                                    var t687_rhs int = 12
                                    var t687 uint32 = t686 << t687_rhs
                                    var t688 uint32 = t685 | t687
                                    var t689_rhs uint32 = 63
                                    var t689 uint32 = third__13 & t689_rhs
                                    var t690_rhs int = 6
                                    var t690 uint32 = t689 << t690_rhs
                                    var t691 uint32 = t688 | t690
                                    var t692_rhs uint32 = 63
                                    var t692 uint32 = fourth__14 & t692_rhs
                                    var t693 uint32 = t691 | t692
                                    var t694 Tuple3_4bool_4char_3int = utf8_valid_decode(t693, 4)
                                    return t694
                                }
                            }
                        } else {
                            var t715 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t715
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t726 string = _goml_runtime_core_int32_to_string(self__70)
    return t726
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t734 bool = value__4 <= 1114111
    if t734 {
        var t738 bool = value__4 >= 55296
        var jp736 bool
        if t738 {
            var t739 bool = value__4 <= 57343
            jp736 = t739
        } else {
            jp736 = false
        }
        var t737 bool = !jp736
        return t737
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t742 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t742
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t745 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t745
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1060 rune
    var inline1021 bool = utf8_valid_scalar(value__0)
    if inline1021 {
        var inline1022 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1023 rune = inline1022._1
        commute_field1060 = inline1023
        var t751 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1060,
            _2: width__1,
        }
        return t751
    } else {
        var inline1019 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1019
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t756 bool = value__3 < 128
    if t756 {
        return true
    } else {
        var t757 bool = value__3 > 191
        return t757
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t762 bool
    var inline1027 bool = value__30 <= 1114111
    if inline1027 {
        var inline1028 bool = value__30 >= 55296
        var inline1030 bool
        if inline1028 {
            var inline1032 bool = value__30 <= 57343
            inline1030 = inline1032
        } else {
            inline1030 = false
        }
        var inline1031 bool = !inline1030
        t762 = inline1031
    } else {
        t762 = false
    }
    if t762 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t763 Option__char = Option__char_Some{
            _0: x24,
        }
        return t763
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__97 string, other__98 string) bool {
    var t766 bool = self__97 == other__98
    return t766
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__125 string) uint64 {
    var t769 uint64 = _goml_runtime_core_string_hash(self__125)
    return t769
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env263 closure_env_inherent_string_string_chars_0) Option__char {
    var self__52 string = env263.self_0
    var index__53 *ref_int_x = env263.index_1
    var t785 int = ref_get__Ref_3int(index__53)
    var commute_field1063 Tuple2_4char_3int
    var inline1034 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__52, t785)
    var inline1035 bool = inline1034._0
    var inline1036 rune = inline1034._1
    var inline1037 int = inline1034._2
    if inline1035 {
        var inline1041 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1036,
            _1: inline1037,
        }
        commute_field1063 = inline1041
        var x32 rune = commute_field1063._0
        var x33 int = commute_field1063._1
        var compound_old34 int = ref_get__Ref_3int(index__53)
        var t788 int = compound_old34 + x33
        ref_set__Ref_3int(index__53, t788)
        var t790 Option__char = Option__char_Some{
            _0: x32,
        }
        return t790
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env264 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var index__58 *ref_int_x = env264.index_0
    var self__57 string = env264.self_1
    var current__59 int = ref_get__Ref_3int(index__58)
    var commute_field1066 Tuple2_4char_3int
    var inline1044 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__57, current__59)
    var inline1045 bool = inline1044._0
    var inline1046 rune = inline1044._1
    var inline1047 int = inline1044._2
    if inline1045 {
        var inline1051 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1046,
            _1: inline1047,
        }
        commute_field1066 = inline1051
        var x40 rune = commute_field1066._0
        var x41 int = commute_field1066._1
        var t795 int = current__59 + x41
        ref_set__Ref_3int(index__58, t795)
        var t796 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__59,
            _1: x40,
        }
        var t797 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t796,
        }
        return t797
    } else {
        return _goml_m_Option_____o_int_c_char_q__None{}
    }
}

func main() {
    main0()
}
