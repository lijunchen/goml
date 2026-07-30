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
    var t162 FnIterator__char = _goml_m_inherent_i_string_i_string_i_chars(value__0)
    var for_iter68 FnIterator__char = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(t162)
    Loop_loop164:
    for {
        if true {
            var for_next69 Option__char = _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(for_iter68)
            switch for_next69.(type) {
            case Option__char_None:
                break Loop_loop164
            case Option__char_Some:
                var x70 rune = for_next69.(Option__char_Some)._0
                var character__1 rune = x70
                println__T_char(character__1)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop164
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t168 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_string_i_string_i_char__indices(value__2)
    var for_iter71 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(t168)
    Loop_loop170:
    for {
        if true {
            var for_next72 _goml_m_Option_____o_int_c_char_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(for_iter71)
            switch for_next72.(type) {
            case _goml_m_Option_____o_int_c_char_q__None:
                break Loop_loop170
            case _goml_m_Option_____o_int_c_char_q__Some:
                var x73 Tuple2_3int_4char = for_next72.(_goml_m_Option_____o_int_c_char_q__Some)._0
                var item__3 Tuple2_3int_4char = x73
                var mtmp74 Tuple2_3int_4char = item__3
                var x75 int = mtmp74._0
                var x76 rune = mtmp74._1
                var character__5 rune = x76
                var index__4 int = x75
                var t172 string = _goml_m_inherent_i_int_i_int_i_to__string(index__4)
                var t173 string = t172 + ":"
                var t174 string = _goml_m_inherent_i_char_i_char_i_to__string(character__5)
                var t175 string = t173 + t174
                println__T_string(t175)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop170
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t178 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t178)
    var t179 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t179)
    var t180 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t180)
    var t181 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t181)
    var t182 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t182)
    var t183 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t183)
    var t184 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t184)
    var t185 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t185)
    var t186 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t186)
    var t187 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t187)
    var t188 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t188)
    var t189 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t189)
    var mtmp89 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp89.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        println__T_string("missing")
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x90 Tuple2_4char_3int = mtmp89.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__7 Tuple2_4char_3int = x90
        var mtmp91 Tuple2_4char_3int = decoded__7
        var x92 rune = mtmp91._0
        var x93 int = mtmp91._1
        var width__9 int = x93
        var character__8 rune = x92
        println__T_char(character__8)
        println__T_int(width__9)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t191 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t191)
    var t192 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t192)
    var t193 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t193)
    var mtmp101 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__10)
    var x102 bool = mtmp101._0
    var x103 string = mtmp101._1
    var roundtrip__12 string = x103
    var roundtrip_valid__11 bool = x102
    println__T_bool(roundtrip_valid__11)
    println__T_string(roundtrip__12)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp108 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(invalid__13)
    var x109 bool = mtmp108._0
    var x110 string = mtmp108._1
    var invalid_text__15 string = x110
    var invalid_valid__14 bool = x109
    println__T_bool(invalid_valid__14)
    var t194 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(invalid_text__15, "")
    println__T_bool(t194)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t195 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t196 bool = t195 >= 3
    println__T_bool(t196)
    var t197 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t197)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t198 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t198)
    var t199 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t200 bool = t199 >= 1
    println__T_bool(t200)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t201 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t202 bool = t201 >= 100
    println__T_bool(t202)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t203 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t203)
    var t204 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t204)
    var t205 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t205)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t206 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t206)
    var t207 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t207)
    var mtmp132 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp132.(type) {
    case Option__int32_None:
        println__T_int(-1)
    case Option__int32_Some:
        var x133 int32 = mtmp132.(Option__int32_Some)._0
        var value__18 int32 = x133
        println__T_int32(value__18)
    default:
        panic("non-exhaustive match")
    }
    var mtmp135 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp135.(type) {
    case Option__int32_None:
        println__T_int(-1)
    case Option__int32_Some:
        var x136 int32 = mtmp135.(Option__int32_Some)._0
        var value__19 int32 = x136
        println__T_int32(value__19)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t210 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t210)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t211 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t211)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t212 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t212)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t213 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t213)
    var seen_a__22 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var seen_b__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var for_source149 *_goml_vec_Tuple2_6string_5int32 = entries__21
    var for_limit150 int = vec_len__Vec_21Tuple2_6string_5int32(for_source149)
    var for_index151 int = 0
    Loop_loop218:
    for {
        var t219 bool = for_index151 < for_limit150
        if t219 {
            var for_item152 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(for_source149, for_index151)
            var t220 int = for_index151 + 1
            for_index151 = t220
            var entry__24 Tuple2_6string_5int32 = for_item152
            var mtmp154 Tuple2_6string_5int32 = entry__24
            var x155 string = mtmp154._0
            var x156 int32 = mtmp154._1
            var item_value__26 int32 = x156
            var key__25 string = x155
            var t231 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "a")
            var jp223 bool
            if t231 {
                var t232 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 1)
                jp223 = t232
            } else {
                jp223 = false
            }
            if jp223 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_a__22, true)
            } else {
                var t229 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "b")
                var jp227 bool
                if t229 {
                    var t230 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 2)
                    jp227 = t230
                } else {
                    jp227 = false
                }
                if jp227 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_b__23, true)
                } else {}
            }
            continue
        } else {
            break Loop_loop218
        }
    }
    var t215 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_a__22)
    println__T_bool(t215)
    var t216 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_b__23)
    println__T_bool(t216)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_chars(self__25 string) FnIterator__char {
    var retv240 FnIterator__char
    var index__26 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t241 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: self__25,
        index_1: index__26,
    }
    var t242 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(t241)
    })
    retv240 = t242
    return retv240
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(self__109 FnIterator__char) FnIterator__char {
    var retv244 FnIterator__char
    retv244 = self__109
    return retv244
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(self__102 FnIterator__char) Option__char {
    var retv246 Option__char
    var t247 func() Option__char = self__102.next_fn
    var t248 Option__char = t247()
    retv246 = t248
    return retv246
}

func println__T_char(value__1 rune) struct{} {
    var t250 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(value__1)
    _goml_runtime_core_string_println(t250)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_char__indices(self__30 string) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv253 _goml_m_FnIterator_____o_int_c_char_q_
    var index__31 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t254 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: index__31,
        self_1: self__30,
    }
    var t255 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(t254)
    })
    retv253 = t255
    return retv253
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(self__109 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv257 _goml_m_FnIterator_____o_int_c_char_q_
    retv257 = self__109
    return retv257
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(self__102 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_Option_____o_int_c_char_q_ {
    var retv259 _goml_m_Option_____o_int_c_char_q_
    var t260 func() _goml_m_Option_____o_int_c_char_q_ = self__102.next_fn
    var t261 _goml_m_Option_____o_int_c_char_q_ = t260()
    retv259 = t261
    return retv259
}

func println__T_string(value__1 string) struct{} {
    var t263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t263)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv266 string
    var t267 string = _goml_runtime_core_int_to_string(self__5)
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv269 string
    var t270 string = _goml_runtime_core_char_to_string(self__7)
    retv269 = t270
    return retv269
}

func println__T_int(value__1 int) struct{} {
    var t272 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t272)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv275 int
    var t276 int = _goml_runtime_core_string_len(self__9)
    retv275 = t276
    return retv275
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv278 int
    var t279 int = _goml_runtime_core_string_len(self__8)
    retv278 = t279
    return retv278
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv281 rune
    var t282 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv281 = t282
    return retv281
}

func println__T_bool(value__1 bool) struct{} {
    var t284 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t284)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__17 string, index__18 int) bool {
    var retv287 bool
    var t288 bool = _goml_runtime_core_string_is_char_boundary(self__17, index__18)
    retv287 = t288
    return retv287
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv290 string
    var t291 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv290 = t291
    return retv290
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__19 string, index__20 int) _goml_m_Option_____o_char_c_int_q_ {
    var retv293 _goml_m_Option_____o_char_c_int_q_
    var mtmp3 Tuple3_4bool_4char_3int = _goml_runtime_core_string_decode_utf8_at(self__19, index__20)
    var x4 bool = mtmp3._0
    var x5 rune = mtmp3._1
    var x6 int = mtmp3._2
    var width__23 int = x6
    var value__22 rune = x5
    var valid__21 bool = x4
    var jp295 _goml_m_Option_____o_char_c_int_q_
    if valid__21 {
        var t296 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: value__22,
            _1: width__23,
        }
        var t297 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t296,
        }
        jp295 = t297
    } else {
        jp295 = _goml_m_Option_____o_char_c_int_q__None{}
    }
    retv293 = jp295
    return retv293
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var retv299 *_goml_vec_uint8
    var t300 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    retv299 = t300
    return retv299
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__137 *_goml_vec_uint8) int {
    var retv302 int
    var t303 int = vec_len__Vec_5uint8(self__137)
    retv302 = t303
    return retv302
}

func println__T_uint8(value__1 uint8) struct{} {
    var t305 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t305)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__132 *_goml_vec_uint8, index__133 int) uint8 {
    var retv308 uint8
    var t309 uint8 = vec_get__Vec_5uint8(self__132, index__133)
    retv308 = t309
    return retv308
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv311 *_goml_vec_uint8
    var t312 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv311 = t312
    return retv311
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__126 *_goml_vec_uint8, elem__127 uint8) struct{} {
    vec_push__Vec_5uint8(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv316 bool
    var t317 bool = self__55 == other__56
    retv316 = t317
    return retv316
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__125 int) *_goml_vec_string {
    var retv319 *_goml_vec_string
    var t320 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__125)
    retv319 = t320
    return retv319
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__138 *_goml_vec_string) int {
    var retv324 int
    var t325 int = vec_capacity__Vec_6string(self__138)
    retv324 = t325
    return retv324
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__125 int) *_goml_vec_int32 {
    var retv327 *_goml_vec_int32
    var t328 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__125)
    retv327 = t328
    return retv327
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv330 int
    var t331 int = vec_len__Vec_5int32(self__137)
    retv330 = t331
    return retv330
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__138 *_goml_vec_int32) int {
    var retv333 int
    var t334 int = vec_capacity__Vec_5int32(self__138)
    retv333 = t334
    return retv333
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
    var t341 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(index__159, len__161)
    if t341 {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__158, value__160)
        return struct{}{}
    } else {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__158, index__159)
        var t343 int = len__161 - 1
        var t344 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__158, t343)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__158, t344)
        var t345 int = len__161 - 1
        var current__162 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t345)
        Loop_loop348:
        for {
            var t349 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__162)
            var t350 bool = t349 > index__159
            if t350 {
                var t351 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__162)
                var t352 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__162)
                var t353 int = t352 - 1
                var t354 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__158, t353)
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__158, t351, t354)
                var t355 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__162)
                var t356 int = t355 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__162, t356)
                continue
            } else {
                break Loop_loop348
            }
        }
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__158, index__159, value__160)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t358 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t358)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__163 *_goml_vec_int32, index__164 int) int32 {
    var retv361 int32
    var len__165 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__163)
    var value__166 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__163, index__164)
    var current__167 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(index__164)
    Loop_loop364:
    for {
        var t365 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__167)
        var t366 int = t365 + 1
        var t367 bool = t366 < len__165
        if t367 {
            var t368 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__167)
            var t369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__167)
            var t370 int = t369 + 1
            var t371 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__163, t370)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__163, t368, t371)
            var t372 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__167)
            var t373 int = t372 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__167, t373)
            continue
        } else {
            break Loop_loop364
        }
    }
    var t363 int = len__165 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__163, t363)
    retv361 = value__166
    return retv361
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__154 *_goml_vec_int32, index__155 int) int32 {
    var retv375 int32
    var len__156 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__154)
    var value__157 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__154, index__155)
    var t378 int = index__155 + 1
    var t379 bool = t378 < len__156
    if t379 {
        var t380 int = len__156 - 1
        var t381 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__154, t380)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__154, index__155, t381)
    } else {}
    var t377 int = len__156 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__154, t377)
    retv375 = value__157
    return retv375
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__168 *_goml_vec_int32) struct{} {
    var left__169 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t383 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__168)
    var t384 int = t383 - 1
    var right__170 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t384)
    Loop_loop386:
    for {
        var t387 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__169)
        var t388 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__170)
        var t389 bool = t387 < t388
        if t389 {
            var t390 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__169)
            var t391 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__170)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__168, t390, t391)
            var t392 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__169)
            var t393 int = t392 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(left__169, t393)
            var t394 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__170)
            var t395 int = t394 - 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(right__170, t395)
            continue
        } else {
            break Loop_loop386
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv397 int32
    var t398 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv397 = t398
    return retv397
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__145 *_goml_vec_int32) Option__int32 {
    var retv400 Option__int32
    var len__146 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__145)
    var t403 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__146, 0)
    var jp402 Option__int32
    if t403 {
        jp402 = Option__int32_None{}
    } else {
        var t404 int = len__146 - 1
        var t405 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__145, t404)
        var t406 Option__int32 = Option__int32_Some{
            _0: t405,
        }
        jp402 = t406
    }
    retv400 = jp402
    return retv400
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__147 *_goml_vec_int32) Option__int32 {
    var retv408 Option__int32
    var len__148 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__147)
    var t411 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__148, 0)
    var jp410 Option__int32
    if t411 {
        jp410 = Option__int32_None{}
    } else {
        var t412 int = len__148 - 1
        var value__149 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__147, t412)
        var t413 int = len__148 - 1
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__147, t413)
        var t414 Option__int32 = Option__int32_Some{
            _0: value__149,
        }
        jp410 = t414
    }
    retv408 = jp410
    return retv408
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__142 *_goml_vec_int32, len__143 int) struct{} {
    vec_truncate__Vec_5int32(self__142, len__143)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__139 *_goml_vec_int32) bool {
    var retv418 bool
    var t419 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139)
    var t420 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t419, 0)
    retv418 = t420
    return retv418
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__144 *_goml_vec_int32) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__144, 0)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv424 *hashmap_string_int32_x
    var t425 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv424 = t425
    return retv424
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__198 *hashmap_string_int32_x, key__199 string, value__200 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__206 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var retv429 *_goml_vec_Tuple2_6string_5int32
    var t430 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__206)
    retv429 = t430
    return retv429
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__137 *_goml_vec_Tuple2_6string_5int32) int {
    var retv432 int
    var t433 int = vec_len__Vec_21Tuple2_6string_5int32(self__137)
    retv432 = t433
    return retv432
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__201 *hashmap_string_int32_x, key__202 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__201, key__202)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv437 *ref_bool_x
    var t438 *ref_bool_x = ref__Ref_4bool(value__207)
    retv437 = t438
    return retv437
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv440 bool
    var t441 bool = self__65 == other__66
    retv440 = t441
    return retv440
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv445 bool
    var t446 bool = ref_get__Ref_4bool(self__208)
    retv445 = t446
    return retv445
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv448 *ref_int_x
    var t449 *ref_int_x = ref__Ref_3int(value__207)
    retv448 = t449
    return retv448
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv451 int
    var t452 int = ref_get__Ref_3int(self__208)
    retv451 = t452
    return retv451
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__101 func() Option__char) FnIterator__char {
    var retv456 FnIterator__char
    var t457 FnIterator__char = FnIterator__char{
        next_fn: next_fn__101,
    }
    retv456 = t457
    return retv456
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__39 rune) string {
    var retv459 string
    var t460 string = _goml_runtime_core_char_to_string(self__39)
    retv459 = t460
    return retv459
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__101 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv462 _goml_m_FnIterator_____o_int_c_char_q_
    var t463 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__101,
    }
    retv462 = t463
    return retv462
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv465 string
    retv465 = self__38
    return retv465
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv467 string
    var t468 string = _goml_runtime_core_int_to_string(self__40)
    retv467 = t468
    return retv467
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv470 string
    var t471 string = _goml_runtime_core_bool_to_string(self__37)
    retv470 = t471
    return retv470
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv473 string
    var t474 string = _goml_runtime_core_uint8_to_string(self__45)
    retv473 = t474
    return retv473
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv476 bool
    var t477 bool = self__59 == other__60
    retv476 = t477
    return retv476
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__134 *_goml_vec_int32, index__135 int, elem__136 int32) struct{} {
    vec_set__Vec_5int32(self__134, index__135, elem__136)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv481 string
    var t482 string = _goml_runtime_core_int32_to_string(self__43)
    retv481 = t482
    return retv481
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__150 *_goml_vec_int32, left__151 int, right__152 int) struct{} {
    var value__153 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__150, left__151)
    var t484 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__150, right__152)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__150, left__151, t484)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__150, right__152, value__153)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv487 uint64
    var t488 uint64 = _goml_runtime_core_string_hash(self__83)
    retv487 = t488
    return retv487
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env159 closure_env_inherent_string_string_chars_0) Option__char {
    var retv500 Option__char
    var self__25 string = env159.self_0
    var index__26 *ref_int_x = env159.index_1
    var t501 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__26)
    var mtmp7 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__25, t501)
    var jp503 Option__char
    switch mtmp7.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        jp503 = Option__char_None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x8 Tuple2_4char_3int = mtmp7.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__27 Tuple2_4char_3int = x8
        var mtmp9 Tuple2_4char_3int = decoded__27
        var x10 rune = mtmp9._0
        var x11 int = mtmp9._1
        var width__29 int = x11
        var value__28 rune = x10
        var t504 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__26)
        var t505 int = t504 + width__29
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__26, t505)
        var t506 Option__char = Option__char_Some{
            _0: value__28,
        }
        jp503 = t506
    default:
        panic("non-exhaustive match")
    }
    retv500 = jp503
    return retv500
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env160 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var retv508 _goml_m_Option_____o_int_c_char_q_
    var index__31 *ref_int_x = env160.index_0
    var self__30 string = env160.self_1
    var current__32 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__31)
    var mtmp13 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__30, current__32)
    var jp510 _goml_m_Option_____o_int_c_char_q_
    switch mtmp13.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        jp510 = _goml_m_Option_____o_int_c_char_q__None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x14 Tuple2_4char_3int = mtmp13.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__33 Tuple2_4char_3int = x14
        var mtmp15 Tuple2_4char_3int = decoded__33
        var x16 rune = mtmp15._0
        var x17 int = mtmp15._1
        var width__35 int = x17
        var value__34 rune = x16
        var t511 int = current__32 + width__35
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__31, t511)
        var t512 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__32,
            _1: value__34,
        }
        var t513 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t512,
        }
        jp510 = t513
    default:
        panic("non-exhaustive match")
    }
    retv508 = jp510
    return retv508
}

func main() {
    main0()
}
