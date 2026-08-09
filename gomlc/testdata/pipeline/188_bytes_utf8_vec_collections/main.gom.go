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
    var inline804 *ref_int_x = ref__Ref_3int(0)
    var inline805 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: value__0,
        index_1: inline804,
    }
    var inline806 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(inline805)
    })
    t266 = inline806
    var for_iter172 FnIterator__char
    for_iter172 = t266
    Loop_loop268:
    for {
        var for_next173 Option__char
        var inline800 func() Option__char = for_iter172.next_fn
        var inline801 Option__char = inline800()
        for_next173 = inline801
        switch for_next173.(type) {
        case Option__char_None:
            break Loop_loop268
        case Option__char_Some:
            var x174 rune = for_next173.(Option__char_Some)._0
            var inline797 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x174)
            _goml_runtime_core_string_println(inline797)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t272 _goml_m_FnIterator_____o_int_c_char_q_
    var inline819 *ref_int_x = ref__Ref_3int(0)
    var inline820 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: inline819,
        self_1: value__2,
    }
    var inline821 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(inline820)
    })
    t272 = inline821
    var for_iter175 _goml_m_FnIterator_____o_int_c_char_q_
    for_iter175 = t272
    Loop_loop274:
    for {
        var for_next176 _goml_m_Option_____o_int_c_char_q_
        var inline815 func() _goml_m_Option_____o_int_c_char_q_ = for_iter175.next_fn
        var inline816 _goml_m_Option_____o_int_c_char_q_ = inline815()
        for_next176 = inline816
        switch for_next176.(type) {
        case _goml_m_Option_____o_int_c_char_q__None:
            break Loop_loop274
        case _goml_m_Option_____o_int_c_char_q__Some:
            var x177 Tuple2_3int_4char = for_next176.(_goml_m_Option_____o_int_c_char_q__Some)._0
            var x179 int = x177._0
            var x180 rune = x177._1
            var t276 string
            var inline813 string = _goml_runtime_core_int_to_string(x179)
            t276 = inline813
            var t277 string = t276 + ":"
            var t278 string
            var inline811 string = char_to_string(x180)
            t278 = inline811
            var t279 string = t277 + t278
            var inline808 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t279)
            _goml_runtime_core_string_println(inline808)
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
        var inline823 string = "missing"
        var inline824 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline823)
        _goml_runtime_core_string_println(inline824)
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x194 Tuple2_4char_3int = mtmp193.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var x196 rune = x194._0
        var x197 int = x194._1
        var inline830 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x196)
        _goml_runtime_core_string_println(inline830)
        var inline827 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x197)
        _goml_runtime_core_string_println(inline827)
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
        var inline833 int = -1
        var inline834 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline833)
        _goml_runtime_core_string_println(inline834)
    case Option__int32_Some:
        var x237 int32 = mtmp236.(Option__int32_Some)._0
        var inline837 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x237)
        _goml_runtime_core_string_println(inline837)
    default:
        panic("non-exhaustive match")
    }
    var mtmp239 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp239.(type) {
    case Option__int32_None:
        var inline840 int = -1
        var inline841 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline840)
        _goml_runtime_core_string_println(inline841)
    case Option__int32_Some:
        var x240 int32 = mtmp239.(Option__int32_Some)._0
        var inline844 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x240)
        _goml_runtime_core_string_println(inline844)
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
    var inline877 string = "c"
    var inline878 int32 = 3
    hashmap_set__HashMap_6string_5int32(map__20, inline877, inline878)
    var inline874 string = "a"
    hashmap_remove__HashMap_6string_5int32(map__20, inline874)
    var t317 int
    var inline872 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    t317 = inline872
    var inline869 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t317)
    _goml_runtime_core_string_println(inline869)
    var seen_a__22 *ref_bool_x
    var inline866 bool = false
    var inline867 *ref_bool_x = ref__Ref_4bool(inline866)
    seen_a__22 = inline867
    var seen_b__23 *ref_bool_x
    var inline863 bool = false
    var inline864 *ref_bool_x = ref__Ref_4bool(inline863)
    seen_b__23 = inline864
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
                var inline847 bool = true
                ref_set__Ref_4bool(seen_a__22, inline847)
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
                    var inline850 bool = true
                    ref_set__Ref_4bool(seen_b__23, inline850)
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
    var inline861 bool = ref_get__Ref_4bool(seen_a__22)
    t319 = inline861
    var inline858 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t319)
    _goml_runtime_core_string_println(inline858)
    var t320 bool
    var inline856 bool = ref_get__Ref_4bool(seen_b__23)
    t320 = inline856
    var inline853 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t320)
    _goml_runtime_core_string_println(inline853)
    return struct{}{}
}

func println__T_char(value__31 rune) struct{} {
    var t354 string
    var inline881 string = char_to_string(value__31)
    t354 = inline881
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
    var inline889 string = _goml_runtime_core_int_to_string(value__31)
    t376 = inline889
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
    var inline891 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline892 bool = inline891._0
    var inline893 rune = inline891._1
    if inline892 {
        return inline893
    } else {
        var inline896 rune = _goml_runtime_core_string_get("", -1)
        return inline896
    }
}

func println__T_bool(value__31 bool) struct{} {
    var t388 string
    var inline898 string = _goml_runtime_core_bool_to_string(value__31)
    t388 = inline898
    _goml_runtime_core_string_println(t388)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__46 string, index__47 int) bool {
    var t392 bool = string_is_char_boundary(self__46, index__47)
    return t392
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline900 bool = string_is_char_boundary(self__43, start__44)
    var inline902 bool
    if inline900 {
        var inline905 bool = string_is_char_boundary(self__43, end__45)
        inline902 = inline905
    } else {
        inline902 = false
    }
    if inline902 {
        var inline903 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline903
    } else {
        var inline904 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline904
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
    var inline907 string = _goml_runtime_core_uint8_to_string(value__31)
    t409 = inline907
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
        var inline909 int = _goml_runtime_core_string_len(x12)
        t419 = inline909
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__175 int) *_goml_vec_string {
    var t432 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__175)
    return t432
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__176 *_goml_vec_string, elem__177 string) struct{} {
    vec_push__Vec_6string(self__176, elem__177)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__188 *_goml_vec_string) int {
    var t437 int = vec_capacity__Vec_6string(self__188)
    return t437
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__175 int) *_goml_vec_int32 {
    var t440 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__175)
    return t440
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__187 *_goml_vec_int32) int {
    var t443 int = vec_len__Vec_5int32(self__187)
    return t443
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__188 *_goml_vec_int32) int {
    var t446 int = vec_capacity__Vec_5int32(self__188)
    return t446
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
    var inline915 int = vec_len__Vec_5int32(self__208)
    len__211 = inline915
    var t453 bool = index__209 == len__211
    if t453 {
        vec_push__Vec_5int32(self__208, value__210)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__208, index__209)
        var t455 int = len__211 - 1
        var t456 int32 = vec_get__Vec_5int32(self__208, t455)
        vec_push__Vec_5int32(self__208, t456)
        var current__212 int = len__211 - 1
        Loop_loop459:
        for {
            var t460 bool = current__212 > index__209
            if t460 {
                var index106 int = current__212
                vec_get__Vec_5int32(self__208, index106)
                var t461 int = current__212 - 1
                var value108 int32 = vec_get__Vec_5int32(self__208, t461)
                vec_set__Vec_5int32(self__208, index106, value108)
                var compound_old110 int = current__212
                var compound_value111 int = 1
                var t463 int = compound_old110 - compound_value111
                current__212 = t463
                continue
            } else {
                break Loop_loop459
            }
        }
        vec_get__Vec_5int32(self__208, index__209)
        vec_set__Vec_5int32(self__208, index__209, value__210)
        return struct{}{}
    }
}

func println__T_int32(value__31 int32) struct{} {
    var t466 string
    var inline917 string = _goml_runtime_core_int32_to_string(value__31)
    t466 = inline917
    _goml_runtime_core_string_println(t466)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__213 *_goml_vec_int32, index__214 int) int32 {
    var len__215 int
    var inline921 int = vec_len__Vec_5int32(self__213)
    len__215 = inline921
    var value__216 int32 = vec_get__Vec_5int32(self__213, index__214)
    var current__217 int = index__214
    Loop_loop472:
    for {
        var t473 int = current__217 + 1
        var t474 bool = t473 < len__215
        if t474 {
            var index120 int = current__217
            vec_get__Vec_5int32(self__213, index120)
            var t475 int = current__217 + 1
            var value122 int32 = vec_get__Vec_5int32(self__213, t475)
            vec_set__Vec_5int32(self__213, index120, value122)
            var compound_old124 int = current__217
            var compound_value125 int = 1
            var t477 int = compound_old124 + compound_value125
            current__217 = t477
            continue
        } else {
            break Loop_loop472
        }
    }
    var t471 int = len__215 - 1
    vec_truncate__Vec_5int32(self__213, t471)
    return value__216
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__204 *_goml_vec_int32, index__205 int) int32 {
    var len__206 int
    var inline925 int = vec_len__Vec_5int32(self__204)
    len__206 = inline925
    var value__207 int32 = vec_get__Vec_5int32(self__204, index__205)
    var t483 int = index__205 + 1
    var t484 bool = t483 < len__206
    if t484 {
        vec_get__Vec_5int32(self__204, index__205)
        var t485 int = len__206 - 1
        var value99 int32 = vec_get__Vec_5int32(self__204, t485)
        vec_set__Vec_5int32(self__204, index__205, value99)
    } else {}
    var t482 int = len__206 - 1
    vec_truncate__Vec_5int32(self__204, t482)
    return value__207
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__218 *_goml_vec_int32) struct{} {
    var left__219 int = 0
    var t488 int
    var inline941 int = vec_len__Vec_5int32(self__218)
    t488 = inline941
    var right__220 int = t488 - 1
    Loop_loop490:
    for {
        var t491 bool = left__219 < right__220
        if t491 {
            var inline927 int32 = vec_get__Vec_5int32(self__218, left__219)
            vec_get__Vec_5int32(self__218, left__219)
            var inline931 int32 = vec_get__Vec_5int32(self__218, right__220)
            vec_set__Vec_5int32(self__218, left__219, inline931)
            vec_get__Vec_5int32(self__218, right__220)
            vec_set__Vec_5int32(self__218, right__220, inline927)
            var compound_old130 int = left__219
            var compound_value131 int = 1
            var t492 int = compound_old130 + compound_value131
            left__219 = t492
            var compound_old133 int = right__220
            var compound_value134 int = 1
            var t494 int = compound_old133 - compound_value134
            right__220 = t494
            continue
        } else {
            break Loop_loop490
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__182 *_goml_vec_int32, index__183 int) int32 {
    var t498 int32 = vec_get__Vec_5int32(self__182, index__183)
    return t498
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__195 *_goml_vec_int32) Option__int32 {
    var len__196 int
    var inline943 int = vec_len__Vec_5int32(self__195)
    len__196 = inline943
    var t503 bool = len__196 == 0
    if t503 {
        return Option__int32_None{}
    } else {
        var t504 int = len__196 - 1
        var t505 int32 = vec_get__Vec_5int32(self__195, t504)
        var t506 Option__int32 = Option__int32_Some{
            _0: t505,
        }
        return t506
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__197 *_goml_vec_int32) Option__int32 {
    var len__198 int
    var inline947 int = vec_len__Vec_5int32(self__197)
    len__198 = inline947
    var t511 bool = len__198 == 0
    if t511 {
        return Option__int32_None{}
    } else {
        var t512 int = len__198 - 1
        var value__199 int32 = vec_get__Vec_5int32(self__197, t512)
        var t513 int = len__198 - 1
        vec_truncate__Vec_5int32(self__197, t513)
        var t514 Option__int32 = Option__int32_Some{
            _0: value__199,
        }
        return t514
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__192 *_goml_vec_int32, len__193 int) struct{} {
    vec_truncate__Vec_5int32(self__192, len__193)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__189 *_goml_vec_int32) bool {
    var t519 int = vec_len__Vec_5int32(self__189)
    var t520 bool = t519 == 0
    return t520
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__194 *_goml_vec_int32) struct{} {
    var inline949 int = 0
    vec_truncate__Vec_5int32(self__194, inline949)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t525 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t525
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__248 *hashmap_string_int32_x, key__249 string, value__250 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__248, key__249, value__250)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__256 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t530 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__256)
    return t530
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__187 *_goml_vec_Tuple2_6string_5int32) int {
    var t533 int = vec_len__Vec_21Tuple2_6string_5int32(self__187)
    return t533
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__172 func() Option__char) FnIterator__char {
    var t546 FnIterator__char = FnIterator__char{
        next_fn: next_fn__172,
    }
    return t546
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__68 rune) string {
    var inline952 uint32 = uint32(rune(self__68))
    var inline953 bool = utf8_valid_scalar(inline952)
    if inline953 {
        var inline954 string = _goml_runtime_core_char_to_string(self__68)
        return inline954
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__172 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t552 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__172,
    }
    return t552
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func char_to_string(value__29 rune) string {
    var t559 uint32 = uint32(rune(value__29))
    var t560 bool
    var inline957 bool = t559 <= 1114111
    if inline957 {
        var inline958 bool = t559 >= 55296
        var inline960 bool
        if inline958 {
            var inline962 bool = t559 <= 57343
            inline960 = inline962
        } else {
            inline960 = false
        }
        var inline961 bool = !inline960
        t560 = inline961
    } else {
        t560 = false
    }
    if t560 {
        var t561 string = _goml_runtime_core_char_to_string(value__29)
        return t561
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t564 string = _goml_runtime_core_int_to_string(self__69)
    return t564
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t572 string = _goml_runtime_core_bool_to_string(self__66)
    return t572
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t586 bool = index__16 < 0
    var jp578 bool
    if t586 {
        jp578 = true
    } else {
        var t587 int
        var inline964 int = _goml_runtime_core_string_len(value__15)
        t587 = inline964
        var t588 bool = index__16 > t587
        jp578 = t588
    }
    if jp578 {
        return false
    } else {
        var t581 int
        var inline968 int = _goml_runtime_core_string_len(value__15)
        t581 = inline968
        var t582 bool = index__16 == t581
        if t582 {
            return true
        } else {
            var t583 uint8
            var inline966 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t583 = inline966
            var t584_rhs uint8 = 192
            var t584 uint8 = t583 & t584_rhs
            var t585 bool = t584 != 128
            return t585
        }
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t717 bool = index__6 < 0
    var jp715 bool
    if t717 {
        jp715 = true
    } else {
        var t718 bool = index__6 >= length__7
        jp715 = t718
    }
    if jp715 {
        var inline970 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline970
    } else {
        var t602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t602))
        var t605 bool = first__8 < 128
        if t605 {
            var inline972 int = 1
            var inline973 Option__char = char_from_uint32(first__8)
            switch inline973.(type) {
            case Option__char_None:
                var inline974 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline974
            case Option__char_Some:
                var inline975 rune = inline973.(Option__char_Some)._0
                var inline977 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline975,
                    _2: inline972,
                }
                return inline977
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t609 bool = first__8 < 194
            if t609 {
                var inline979 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline979
            } else {
                var t613 bool = first__8 < 224
                if t613 {
                    var t626 int = length__7 - index__6
                    var t627 bool = t626 < 2
                    if t627 {
                        var inline981 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline981
                    } else {
                        var t615 int = index__6 + 1
                        var t616 uint8
                        var inline995 uint8 = _goml_runtime_core_string_byte_get(value__5, t615)
                        t616 = inline995
                        var second__9 uint32 = uint32(uint8(t616))
                        var t619 bool
                        var inline992 bool = second__9 < 128
                        if inline992 {
                            t619 = true
                        } else {
                            var inline993 bool = second__9 > 191
                            t619 = inline993
                        }
                        if t619 {
                            var inline983 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline983
                        } else {
                            var t621_rhs uint32 = 31
                            var t621 uint32 = first__8 & t621_rhs
                            var t622_rhs int = 6
                            var t622 uint32 = t621 << t622_rhs
                            var t623_rhs uint32 = 63
                            var t623 uint32 = second__9 & t623_rhs
                            var t624 uint32 = t622 | t623
                            var inline985 int = 2
                            var inline986 Option__char = char_from_uint32(t624)
                            switch inline986.(type) {
                            case Option__char_None:
                                var inline987 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline987
                            case Option__char_Some:
                                var inline988 rune = inline986.(Option__char_Some)._0
                                var inline990 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline988,
                                    _2: inline985,
                                }
                                return inline990
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t631 bool = first__8 < 240
                    if t631 {
                        var t664 int = length__7 - index__6
                        var t665 bool = t664 < 3
                        if t665 {
                            var inline997 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline997
                        } else {
                            var t633 int = index__6 + 1
                            var t634 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t633)
                            var second__10 uint32 = uint32(uint8(t634))
                            var t635 int = index__6 + 2
                            var t636 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t635)
                            var third__11 uint32 = uint32(uint8(t636))
                            var t662 bool = utf8_invalid_continuation(second__10)
                            var jp657 bool
                            if t662 {
                                jp657 = true
                            } else {
                                var inline999 bool = third__11 < 128
                                if inline999 {
                                    jp657 = true
                                } else {
                                    var inline1000 bool = third__11 > 191
                                    jp657 = inline1000
                                }
                            }
                            var jp651 bool
                            if jp657 {
                                jp651 = true
                            } else {
                                var t660 bool = first__8 == 224
                                if t660 {
                                    var t661 bool = second__10 < 160
                                    jp651 = t661
                                } else {
                                    jp651 = false
                                }
                            }
                            var jp640 bool
                            if jp651 {
                                jp640 = true
                            } else {
                                var t654 bool = first__8 == 237
                                if t654 {
                                    var t655 bool = second__10 >= 160
                                    jp640 = t655
                                } else {
                                    jp640 = false
                                }
                            }
                            if jp640 {
                                var inline1002 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1002
                            } else {
                                var t642_rhs uint32 = 15
                                var t642 uint32 = first__8 & t642_rhs
                                var t643_rhs int = 12
                                var t643 uint32 = t642 << t643_rhs
                                var t644_rhs uint32 = 63
                                var t644 uint32 = second__10 & t644_rhs
                                var t645_rhs int = 6
                                var t645 uint32 = t644 << t645_rhs
                                var t646 uint32 = t643 | t645
                                var t647_rhs uint32 = 63
                                var t647 uint32 = third__11 & t647_rhs
                                var t648 uint32 = t646 | t647
                                var inline1004 int = 3
                                var inline1005 Option__char = char_from_uint32(t648)
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
                            }
                        }
                    } else {
                        var t669 bool = first__8 < 245
                        if t669 {
                            var t710 int = length__7 - index__6
                            var t711 bool = t710 < 4
                            if t711 {
                                var t712 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t712
                            } else {
                                var t671 int = index__6 + 1
                                var t672 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t671)
                                var second__12 uint32 = uint32(uint8(t672))
                                var t673 int = index__6 + 2
                                var t674 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t673)
                                var third__13 uint32 = uint32(uint8(t674))
                                var t675 int = index__6 + 3
                                var t676 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t675)
                                var fourth__14 uint32 = uint32(uint8(t676))
                                var t708 bool = utf8_invalid_continuation(second__12)
                                var jp706 bool
                                if t708 {
                                    jp706 = true
                                } else {
                                    var t709 bool = utf8_invalid_continuation(third__13)
                                    jp706 = t709
                                }
                                var jp700 bool
                                if jp706 {
                                    jp700 = true
                                } else {
                                    var t707 bool = utf8_invalid_continuation(fourth__14)
                                    jp700 = t707
                                }
                                var jp694 bool
                                if jp700 {
                                    jp694 = true
                                } else {
                                    var t703 bool = first__8 == 240
                                    if t703 {
                                        var t704 bool = second__12 < 144
                                        jp694 = t704
                                    } else {
                                        jp694 = false
                                    }
                                }
                                var jp680 bool
                                if jp694 {
                                    jp680 = true
                                } else {
                                    var t697 bool = first__8 == 244
                                    if t697 {
                                        var t698 bool = second__12 > 143
                                        jp680 = t698
                                    } else {
                                        jp680 = false
                                    }
                                }
                                if jp680 {
                                    var t681 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t681
                                } else {
                                    var t682_rhs uint32 = 7
                                    var t682 uint32 = first__8 & t682_rhs
                                    var t683_rhs int = 18
                                    var t683 uint32 = t682 << t683_rhs
                                    var t684_rhs uint32 = 63
                                    var t684 uint32 = second__12 & t684_rhs
                                    var t685_rhs int = 12
                                    var t685 uint32 = t684 << t685_rhs
                                    var t686 uint32 = t683 | t685
                                    var t687_rhs uint32 = 63
                                    var t687 uint32 = third__13 & t687_rhs
                                    var t688_rhs int = 6
                                    var t688 uint32 = t687 << t688_rhs
                                    var t689 uint32 = t686 | t688
                                    var t690_rhs uint32 = 63
                                    var t690 uint32 = fourth__14 & t690_rhs
                                    var t691 uint32 = t689 | t690
                                    var t692 Tuple3_4bool_4char_3int = utf8_valid_decode(t691, 4)
                                    return t692
                                }
                            }
                        } else {
                            var t713 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t713
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t724 string = _goml_runtime_core_int32_to_string(self__72)
    return t724
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t732 bool = value__4 <= 1114111
    if t732 {
        var t736 bool = value__4 >= 55296
        var jp734 bool
        if t736 {
            var t737 bool = value__4 <= 57343
            jp734 = t737
        } else {
            jp734 = false
        }
        var t735 bool = !jp734
        return t735
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t740 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t740
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t743 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t743
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1052 rune
    var inline1013 bool = utf8_valid_scalar(value__0)
    if inline1013 {
        var inline1014 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1015 rune = inline1014._1
        commute_field1052 = inline1015
        var t749 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1052,
            _2: width__1,
        }
        return t749
    } else {
        var inline1011 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1011
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t754 bool = value__3 < 128
    if t754 {
        return true
    } else {
        var t755 bool = value__3 > 191
        return t755
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t760 bool
    var inline1019 bool = value__32 <= 1114111
    if inline1019 {
        var inline1020 bool = value__32 >= 55296
        var inline1022 bool
        if inline1020 {
            var inline1024 bool = value__32 <= 57343
            inline1022 = inline1024
        } else {
            inline1022 = false
        }
        var inline1023 bool = !inline1022
        t760 = inline1023
    } else {
        t760 = false
    }
    if t760 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t761 Option__char = Option__char_Some{
            _0: x24,
        }
        return t761
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__99 string, other__100 string) bool {
    var t764 bool = self__99 == other__100
    return t764
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__127 string) uint64 {
    var t767 uint64 = _goml_runtime_core_string_hash(self__127)
    return t767
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env263 closure_env_inherent_string_string_chars_0) Option__char {
    var self__54 string = env263.self_0
    var index__55 *ref_int_x = env263.index_1
    var t783 int = ref_get__Ref_3int(index__55)
    var commute_field1055 Tuple2_4char_3int
    var inline1026 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__54, t783)
    var inline1027 bool = inline1026._0
    var inline1028 rune = inline1026._1
    var inline1029 int = inline1026._2
    if inline1027 {
        var inline1033 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1028,
            _1: inline1029,
        }
        commute_field1055 = inline1033
        var x32 rune = commute_field1055._0
        var x33 int = commute_field1055._1
        var compound_old34 int = ref_get__Ref_3int(index__55)
        var t786 int = compound_old34 + x33
        ref_set__Ref_3int(index__55, t786)
        var t788 Option__char = Option__char_Some{
            _0: x32,
        }
        return t788
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env264 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var index__60 *ref_int_x = env264.index_0
    var self__59 string = env264.self_1
    var current__61 int = ref_get__Ref_3int(index__60)
    var commute_field1058 Tuple2_4char_3int
    var inline1036 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__59, current__61)
    var inline1037 bool = inline1036._0
    var inline1038 rune = inline1036._1
    var inline1039 int = inline1036._2
    if inline1037 {
        var inline1043 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1038,
            _1: inline1039,
        }
        commute_field1058 = inline1043
        var x40 rune = commute_field1058._0
        var x41 int = commute_field1058._1
        var t793 int = current__61 + x41
        ref_set__Ref_3int(index__60, t793)
        var t794 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__61,
            _1: x40,
        }
        var t795 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t794,
        }
        return t795
    } else {
        return _goml_m_Option_____o_int_c_char_q__None{}
    }
}

func main() {
    main0()
}
