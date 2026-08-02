package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
    _goml_strings "strings"
    _goml_utf8 "unicode/utf8"
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

func _goml_runtime_string_decode_utf8_at_native(s string, i int) (bool, rune, int) {
    if i < 0 || i >= int(len(s)) {
        return false, 0, 0
    }
    var value rune
    var width int
    value, width = _goml_utf8.DecodeRuneInString(s[i:int(len(s))])
    if value == _goml_utf8.RuneError && width == 1 {
        return false, 0, 0
    }
    return true, value, int(width)
}

func _goml_runtime_core_string_get(s string, i int) rune {
    var valid bool
    var value rune
    valid, value, _ = _goml_runtime_string_decode_utf8_at_native(s, i)
    if !valid {
        panic("invalid string byte index")
    }
    return value
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    if !_goml_runtime_core_string_is_char_boundary(s, start) && _goml_runtime_core_string_is_char_boundary(s, end) {
        panic("invalid string byte slice")
    }
    return s[start:end]
}

func _goml_runtime_core_string_is_char_boundary(s string, i int) bool {
    if i < 0 || i > int(len(s)) {
        return false
    }
    if i == int(len(s)) {
        return true
    }
    return _goml_utf8.RuneStart(s[i])
}

func _goml_runtime_core_string_decode_utf8_at(s string, i int) Tuple3_4bool_4char_3int {
    var valid bool
    var value rune
    var width int
    valid, value, width = _goml_runtime_string_decode_utf8_at_native(s, i)
    return Tuple3_4bool_4char_3int{
        _0: valid,
        _1: value,
        _2: width,
    }
}

func _goml_runtime_core_string_to_bytes(s string) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: []byte(s),
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    if !_goml_utf8.Valid(bytes.items) {
        return Tuple2_4bool_6string{
            _0: false,
            _1: "",
        }
    }
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_string_concat(values *_goml_vec_string) string {
    return _goml_strings.Join(values.items, "")
}

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
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
    var t249 FnIterator__char = _goml_m_inherent_i_string_i_string_i_chars(value__0)
    var for_iter155 FnIterator__char = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(t249)
    Loop_loop251:
    for {
        var for_next156 Option__char = _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(for_iter155)
        switch for_next156.(type) {
        case Option__char_None:
            break Loop_loop251
        case Option__char_Some:
            var x157 rune = for_next156.(Option__char_Some)._0
            println__T_char(x157)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t255 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_string_i_string_i_char__indices(value__2)
    var for_iter158 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(t255)
    Loop_loop257:
    for {
        var for_next159 _goml_m_Option_____o_int_c_char_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(for_iter158)
        switch for_next159.(type) {
        case _goml_m_Option_____o_int_c_char_q__None:
            break Loop_loop257
        case _goml_m_Option_____o_int_c_char_q__Some:
            var x160 Tuple2_3int_4char = for_next159.(_goml_m_Option_____o_int_c_char_q__Some)._0
            var x162 int = x160._0
            var x163 rune = x160._1
            var t259 string = _goml_m_inherent_i_int_i_int_i_to__string(x162)
            var t260 string = t259 + ":"
            var t261 string = _goml_m_inherent_i_char_i_char_i_to__string(x163)
            var t262 string = t260 + t261
            println__T_string(t262)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t265 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t265)
    var t266 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t266)
    var t267 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t267)
    var t268 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t268)
    var t269 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t269)
    var t270 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t270)
    var t271 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t271)
    var t272 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t272)
    var t273 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t273)
    var t274 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t274)
    var t275 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t275)
    var t276 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t276)
    var mtmp176 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp176.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        println__T_string("missing")
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x177 Tuple2_4char_3int = mtmp176.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var x179 rune = x177._0
        var x180 int = x177._1
        println__T_char(x179)
        println__T_int(x180)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t278 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t278)
    var t279 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t279)
    var t280 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t280)
    var mtmp188 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__10)
    var x189 bool = mtmp188._0
    var x190 string = mtmp188._1
    println__T_bool(x189)
    println__T_string(x190)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp195 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(invalid__13)
    var x196 bool = mtmp195._0
    var x197 string = mtmp195._1
    println__T_bool(x196)
    var t281 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(x197, "")
    println__T_bool(t281)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t282 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t283 bool = t282 >= 3
    println__T_bool(t283)
    var t284 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t284)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t285 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t285)
    var t286 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t287 bool = t286 >= 1
    println__T_bool(t287)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t288 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t289 bool = t288 >= 100
    println__T_bool(t289)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t290 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t290)
    var t291 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t291)
    var t292 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t292)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t293 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t293)
    var t294 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t294)
    var mtmp219 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp219.(type) {
    case Option__int32_None:
        println__T_int(-1)
    case Option__int32_Some:
        var x220 int32 = mtmp219.(Option__int32_Some)._0
        println__T_int32(x220)
    default:
        panic("non-exhaustive match")
    }
    var mtmp222 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp222.(type) {
    case Option__int32_None:
        println__T_int(-1)
    case Option__int32_Some:
        var x223 int32 = mtmp222.(Option__int32_Some)._0
        println__T_int32(x223)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t297 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t297)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t298 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t298)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t299 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t299)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t300 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t300)
    var seen_a__22 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var seen_b__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var for_limit237 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    var for_index238 int = 0
    Loop_loop305:
    for {
        var t306 bool = for_index238 < for_limit237
        if t306 {
            var for_item239 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(entries__21, for_index238)
            var t307 int = for_index238 + 1
            for_index238 = t307
            var x242 string = for_item239._0
            var x243 int32 = for_item239._1
            var t318 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(x242, "a")
            var jp310 bool
            if t318 {
                var t319 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(x243, 1)
                jp310 = t319
            } else {
                jp310 = false
            }
            if jp310 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_a__22, true)
                continue
            } else {
                var t316 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(x242, "b")
                var jp314 bool
                if t316 {
                    var t317 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(x243, 2)
                    jp314 = t317
                } else {
                    jp314 = false
                }
                if jp314 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_b__23, true)
                    continue
                } else {
                    continue
                }
            }
        } else {
            break Loop_loop305
        }
    }
    var t302 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_a__22)
    println__T_bool(t302)
    var t303 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_b__23)
    println__T_bool(t303)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_chars(self__25 string) FnIterator__char {
    var index__26 *ref_int_x = ref__Ref_3int(0)
    var t328 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: self__25,
        index_1: index__26,
    }
    var t329 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(t328)
    })
    return t329
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(self__109 FnIterator__char) FnIterator__char {
    return self__109
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(self__102 FnIterator__char) Option__char {
    var t334 func() Option__char = self__102.next_fn
    var t335 Option__char = t334()
    return t335
}

func println__T_char(value__1 rune) struct{} {
    var t337 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(value__1)
    _goml_runtime_core_string_println(t337)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_char__indices(self__30 string) _goml_m_FnIterator_____o_int_c_char_q_ {
    var index__31 *ref_int_x = ref__Ref_3int(0)
    var t341 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: index__31,
        self_1: self__30,
    }
    var t342 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(t341)
    })
    return t342
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(self__109 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    return self__109
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(self__102 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_Option_____o_int_c_char_q_ {
    var t347 func() _goml_m_Option_____o_int_c_char_q_ = self__102.next_fn
    var t348 _goml_m_Option_____o_int_c_char_q_ = t347()
    return t348
}

func println__T_string(value__1 string) struct{} {
    var t350 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t350)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t354 string = _goml_runtime_core_int_to_string(self__5)
    return t354
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var t357 string = _goml_runtime_core_char_to_string(self__7)
    return t357
}

func println__T_int(value__1 int) struct{} {
    var t359 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t359)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var t363 int = _goml_runtime_core_string_len(self__9)
    return t363
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var t366 int = _goml_runtime_core_string_len(self__8)
    return t366
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var t369 rune = _goml_runtime_core_string_get(self__10, index__11)
    return t369
}

func println__T_bool(value__1 bool) struct{} {
    var t371 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t371)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__17 string, index__18 int) bool {
    var t375 bool = _goml_runtime_core_string_is_char_boundary(self__17, index__18)
    return t375
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var t378 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    return t378
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__19 string, index__20 int) _goml_m_Option_____o_char_c_int_q_ {
    var mtmp3 Tuple3_4bool_4char_3int = _goml_runtime_core_string_decode_utf8_at(self__19, index__20)
    var x4 bool = mtmp3._0
    var x5 rune = mtmp3._1
    var x6 int = mtmp3._2
    if x4 {
        var t383 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: x5,
            _1: x6,
        }
        var t384 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t383,
        }
        return t384
    } else {
        return _goml_m_Option_____o_char_c_int_q__None{}
    }
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var t387 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    return t387
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__137 *_goml_vec_uint8) int {
    var t390 int = vec_len__Vec_5uint8(self__137)
    return t390
}

func println__T_uint8(value__1 uint8) struct{} {
    var t392 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t392)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__132 *_goml_vec_uint8, index__133 int) uint8 {
    var t396 uint8 = vec_get__Vec_5uint8(self__132, index__133)
    return t396
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t399 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t399
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__126 *_goml_vec_uint8, elem__127 uint8) struct{} {
    vec_push__Vec_5uint8(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var t404 bool = self__55 == other__56
    return t404
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__125 int) *_goml_vec_string {
    var t407 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__125)
    return t407
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__138 *_goml_vec_string) int {
    var t412 int = vec_capacity__Vec_6string(self__138)
    return t412
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__125 int) *_goml_vec_int32 {
    var t415 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__125)
    return t415
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var t418 int = vec_len__Vec_5int32(self__137)
    return t418
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__138 *_goml_vec_int32) int {
    var t421 int = vec_capacity__Vec_5int32(self__138)
    return t421
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__140 *_goml_vec_int32, additional__141 int) struct{} {
    vec_reserve__Vec_5int32(self__140, additional__141)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__126 *_goml_vec_int32, elem__127 int32) struct{} {
    vec_push__Vec_5int32(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(self__158 *_goml_vec_int32, index__159 int, value__160 int32) struct{} {
    var len__161 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__158)
    var t428 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(index__159, len__161)
    if t428 {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__158, value__160)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__158, index__159)
        var t430 int = len__161 - 1
        var t431 int32 = vec_get__Vec_5int32(self__158, t430)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__158, t431)
        var current__162 int = len__161 - 1
        Loop_loop434:
        for {
            var t435 bool = current__162 > index__159
            if t435 {
                var index64 int = current__162
                vec_get__Vec_5int32(self__158, index64)
                var t436 int = current__162 - 1
                var value66 int32 = vec_get__Vec_5int32(self__158, t436)
                vec_set__Vec_5int32(self__158, index64, value66)
                var compound_old68 int = current__162
                var compound_value69 int = 1
                var t438 int = compound_old68 - compound_value69
                current__162 = t438
                continue
            } else {
                break Loop_loop434
            }
        }
        vec_get__Vec_5int32(self__158, index__159)
        vec_set__Vec_5int32(self__158, index__159, value__160)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t441 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t441)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__163 *_goml_vec_int32, index__164 int) int32 {
    var len__165 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__163)
    var value__166 int32 = vec_get__Vec_5int32(self__163, index__164)
    var current__167 int = index__164
    Loop_loop447:
    for {
        var t448 int = current__167 + 1
        var t449 bool = t448 < len__165
        if t449 {
            var index78 int = current__167
            vec_get__Vec_5int32(self__163, index78)
            var t450 int = current__167 + 1
            var value80 int32 = vec_get__Vec_5int32(self__163, t450)
            vec_set__Vec_5int32(self__163, index78, value80)
            var compound_old82 int = current__167
            var compound_value83 int = 1
            var t452 int = compound_old82 + compound_value83
            current__167 = t452
            continue
        } else {
            break Loop_loop447
        }
    }
    var t446 int = len__165 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__163, t446)
    return value__166
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__154 *_goml_vec_int32, index__155 int) int32 {
    var len__156 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__154)
    var value__157 int32 = vec_get__Vec_5int32(self__154, index__155)
    var t458 int = index__155 + 1
    var t459 bool = t458 < len__156
    if t459 {
        vec_get__Vec_5int32(self__154, index__155)
        var t460 int = len__156 - 1
        var value57 int32 = vec_get__Vec_5int32(self__154, t460)
        vec_set__Vec_5int32(self__154, index__155, value57)
    } else {}
    var t457 int = len__156 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__154, t457)
    return value__157
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__168 *_goml_vec_int32) struct{} {
    var left__169 int = 0
    var t463 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__168)
    var right__170 int = t463 - 1
    Loop_loop465:
    for {
        var t466 bool = left__169 < right__170
        if t466 {
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__168, left__169, right__170)
            var compound_old88 int = left__169
            var compound_value89 int = 1
            var t467 int = compound_old88 + compound_value89
            left__169 = t467
            var compound_old91 int = right__170
            var compound_value92 int = 1
            var t469 int = compound_old91 - compound_value92
            right__170 = t469
            continue
        } else {
            break Loop_loop465
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var t473 int32 = vec_get__Vec_5int32(self__132, index__133)
    return t473
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__145 *_goml_vec_int32) Option__int32 {
    var len__146 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__145)
    var t478 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__146, 0)
    if t478 {
        return Option__int32_None{}
    } else {
        var t479 int = len__146 - 1
        var t480 int32 = vec_get__Vec_5int32(self__145, t479)
        var t481 Option__int32 = Option__int32_Some{
            _0: t480,
        }
        return t481
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__147 *_goml_vec_int32) Option__int32 {
    var len__148 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__147)
    var t486 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__148, 0)
    if t486 {
        return Option__int32_None{}
    } else {
        var t487 int = len__148 - 1
        var value__149 int32 = vec_get__Vec_5int32(self__147, t487)
        var t488 int = len__148 - 1
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__147, t488)
        var t489 Option__int32 = Option__int32_Some{
            _0: value__149,
        }
        return t489
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__142 *_goml_vec_int32, len__143 int) struct{} {
    vec_truncate__Vec_5int32(self__142, len__143)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__139 *_goml_vec_int32) bool {
    var t494 int = vec_len__Vec_5int32(self__139)
    var t495 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t494, 0)
    return t495
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__144 *_goml_vec_int32) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__144, 0)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t500 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t500
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__198 *hashmap_string_int32_x, key__199 string, value__200 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__206 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t505 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__206)
    return t505
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__137 *_goml_vec_Tuple2_6string_5int32) int {
    var t508 int = vec_len__Vec_21Tuple2_6string_5int32(self__137)
    return t508
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__201 *hashmap_string_int32_x, key__202 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__201, key__202)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var t513 *ref_bool_x = ref__Ref_4bool(value__207)
    return t513
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var t516 bool = self__65 == other__66
    return t516
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var t521 bool = ref_get__Ref_4bool(self__208)
    return t521
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__101 func() Option__char) FnIterator__char {
    var t524 FnIterator__char = FnIterator__char{
        next_fn: next_fn__101,
    }
    return t524
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__39 rune) string {
    var t527 string = _goml_runtime_core_char_to_string(self__39)
    return t527
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__101 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t530 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__101,
    }
    return t530
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t535 string = _goml_runtime_core_int_to_string(self__40)
    return t535
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t538 string = _goml_runtime_core_bool_to_string(self__37)
    return t538
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var t541 string = _goml_runtime_core_uint8_to_string(self__45)
    return t541
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var t544 bool = self__59 == other__60
    return t544
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t547 string = _goml_runtime_core_int32_to_string(self__43)
    return t547
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__150 *_goml_vec_int32, left__151 int, right__152 int) struct{} {
    var value__153 int32 = vec_get__Vec_5int32(self__150, left__151)
    vec_get__Vec_5int32(self__150, left__151)
    var value47 int32 = vec_get__Vec_5int32(self__150, right__152)
    vec_set__Vec_5int32(self__150, left__151, value47)
    vec_get__Vec_5int32(self__150, right__152)
    vec_set__Vec_5int32(self__150, right__152, value__153)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var t553 uint64 = _goml_runtime_core_string_hash(self__83)
    return t553
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env246 closure_env_inherent_string_string_chars_0) Option__char {
    var self__25 string = env246.self_0
    var index__26 *ref_int_x = env246.index_1
    var t566 int = ref_get__Ref_3int(index__26)
    var mtmp7 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__25, t566)
    switch mtmp7.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        return Option__char_None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x8 Tuple2_4char_3int = mtmp7.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var x10 rune = x8._0
        var x11 int = x8._1
        var compound_old12 int = ref_get__Ref_3int(index__26)
        var t569 int = compound_old12 + x11
        ref_set__Ref_3int(index__26, t569)
        var t571 Option__char = Option__char_Some{
            _0: x10,
        }
        return t571
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env247 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var index__31 *ref_int_x = env247.index_0
    var self__30 string = env247.self_1
    var current__32 int = ref_get__Ref_3int(index__31)
    var mtmp15 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__30, current__32)
    switch mtmp15.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        return _goml_m_Option_____o_int_c_char_q__None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x16 Tuple2_4char_3int = mtmp15.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var x18 rune = x16._0
        var x19 int = x16._1
        var t576 int = current__32 + x19
        ref_set__Ref_3int(index__31, t576)
        var t577 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__32,
            _1: x18,
        }
        var t578 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t577,
        }
        return t578
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
