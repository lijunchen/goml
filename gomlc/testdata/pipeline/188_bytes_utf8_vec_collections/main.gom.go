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
    var t158 FnIterator__char = _goml_m_inherent_i_string_i_string_i_chars(value__0)
    var for_iter64 FnIterator__char = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(t158)
    Loop_loop160:
    for {
        if true {
            var for_next65 Option__char = _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(for_iter64)
            switch for_next65.(type) {
            case Option__char_None:
                break Loop_loop160
            case Option__char_Some:
                var x66 rune = for_next65.(Option__char_Some)._0
                var character__1 rune = x66
                println__T_char(character__1)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop160
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t164 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_string_i_string_i_char__indices(value__2)
    var for_iter67 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(t164)
    Loop_loop166:
    for {
        if true {
            var for_next68 _goml_m_Option_____o_int_c_char_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(for_iter67)
            switch for_next68.(type) {
            case _goml_m_Option_____o_int_c_char_q__None:
                break Loop_loop166
            case _goml_m_Option_____o_int_c_char_q__Some:
                var x69 Tuple2_3int_4char = for_next68.(_goml_m_Option_____o_int_c_char_q__Some)._0
                var item__3 Tuple2_3int_4char = x69
                var mtmp70 Tuple2_3int_4char = item__3
                var x71 int = mtmp70._0
                var x72 rune = mtmp70._1
                var character__5 rune = x72
                var index__4 int = x71
                var t168 string = _goml_m_inherent_i_int_i_int_i_to__string(index__4)
                var t169 string = t168 + ":"
                var t170 string = _goml_m_inherent_i_char_i_char_i_to__string(character__5)
                var t171 string = t169 + t170
                println__T_string(t171)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop166
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t174 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t174)
    var t175 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t175)
    var t176 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t176)
    var t177 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t177)
    var t178 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t178)
    var t179 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t179)
    var t180 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t180)
    var t181 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t181)
    var t182 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t182)
    var t183 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t183)
    var t184 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t184)
    var t185 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t185)
    var mtmp85 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp85.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        println__T_string("missing")
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x86 Tuple2_4char_3int = mtmp85.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__7 Tuple2_4char_3int = x86
        var mtmp87 Tuple2_4char_3int = decoded__7
        var x88 rune = mtmp87._0
        var x89 int = mtmp87._1
        var width__9 int = x89
        var character__8 rune = x88
        println__T_char(character__8)
        println__T_int(width__9)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t187 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t187)
    var t188 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t188)
    var t189 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t189)
    var mtmp97 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__10)
    var x98 bool = mtmp97._0
    var x99 string = mtmp97._1
    var roundtrip__12 string = x99
    var roundtrip_valid__11 bool = x98
    println__T_bool(roundtrip_valid__11)
    println__T_string(roundtrip__12)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp104 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(invalid__13)
    var x105 bool = mtmp104._0
    var x106 string = mtmp104._1
    var invalid_text__15 string = x106
    var invalid_valid__14 bool = x105
    println__T_bool(invalid_valid__14)
    var t190 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(invalid_text__15, "")
    println__T_bool(t190)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t191 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t192 bool = t191 >= 3
    println__T_bool(t192)
    var t193 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t193)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t194 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t194)
    var t195 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t196 bool = t195 >= 1
    println__T_bool(t196)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t197 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t198 bool = t197 >= 100
    println__T_bool(t198)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t199 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t199)
    var t200 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t200)
    var t201 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t201)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t202 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t202)
    var t203 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t203)
    var mtmp128 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp128.(type) {
    case Option__int32_None:
        println__T_int(-1)
    case Option__int32_Some:
        var x129 int32 = mtmp128.(Option__int32_Some)._0
        var value__18 int32 = x129
        println__T_int32(value__18)
    default:
        panic("non-exhaustive match")
    }
    var mtmp131 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp131.(type) {
    case Option__int32_None:
        println__T_int(-1)
    case Option__int32_Some:
        var x132 int32 = mtmp131.(Option__int32_Some)._0
        var value__19 int32 = x132
        println__T_int32(value__19)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t206 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t206)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t207 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t207)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t208 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t208)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t209 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t209)
    var seen_a__22 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var seen_b__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var for_source145 *_goml_vec_Tuple2_6string_5int32 = entries__21
    var for_limit146 int = vec_len__Vec_21Tuple2_6string_5int32(for_source145)
    var for_index147 int = 0
    Loop_loop214:
    for {
        var t215 bool = for_index147 < for_limit146
        if t215 {
            var for_item148 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(for_source145, for_index147)
            var t216 int = for_index147 + 1
            for_index147 = t216
            var entry__24 Tuple2_6string_5int32 = for_item148
            var mtmp150 Tuple2_6string_5int32 = entry__24
            var x151 string = mtmp150._0
            var x152 int32 = mtmp150._1
            var item_value__26 int32 = x152
            var key__25 string = x151
            var t227 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "a")
            var jp219 bool
            if t227 {
                var t228 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 1)
                jp219 = t228
            } else {
                jp219 = false
            }
            if jp219 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_a__22, true)
            } else {
                var t225 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "b")
                var jp223 bool
                if t225 {
                    var t226 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 2)
                    jp223 = t226
                } else {
                    jp223 = false
                }
                if jp223 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_b__23, true)
                } else {}
            }
            continue
        } else {
            break Loop_loop214
        }
    }
    var t211 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_a__22)
    println__T_bool(t211)
    var t212 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_b__23)
    println__T_bool(t212)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_chars(self__25 string) FnIterator__char {
    var retv236 FnIterator__char
    var index__26 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t237 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: self__25,
        index_1: index__26,
    }
    var t238 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(t237)
    })
    retv236 = t238
    return retv236
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(self__109 FnIterator__char) FnIterator__char {
    var retv240 FnIterator__char
    retv240 = self__109
    return retv240
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(self__102 FnIterator__char) Option__char {
    var retv242 Option__char
    var t243 func() Option__char = self__102.next_fn
    var t244 Option__char = t243()
    retv242 = t244
    return retv242
}

func println__T_char(value__1 rune) struct{} {
    var t246 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(value__1)
    _goml_runtime_core_string_println(t246)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_char__indices(self__30 string) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv249 _goml_m_FnIterator_____o_int_c_char_q_
    var index__31 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t250 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: index__31,
        self_1: self__30,
    }
    var t251 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(t250)
    })
    retv249 = t251
    return retv249
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(self__109 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv253 _goml_m_FnIterator_____o_int_c_char_q_
    retv253 = self__109
    return retv253
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(self__102 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_Option_____o_int_c_char_q_ {
    var retv255 _goml_m_Option_____o_int_c_char_q_
    var t256 func() _goml_m_Option_____o_int_c_char_q_ = self__102.next_fn
    var t257 _goml_m_Option_____o_int_c_char_q_ = t256()
    retv255 = t257
    return retv255
}

func println__T_string(value__1 string) struct{} {
    var t259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t259)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv262 string
    var t263 string = _goml_runtime_core_int_to_string(self__5)
    retv262 = t263
    return retv262
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv265 string
    var t266 string = _goml_runtime_core_char_to_string(self__7)
    retv265 = t266
    return retv265
}

func println__T_int(value__1 int) struct{} {
    var t268 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t268)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv271 int
    var t272 int = _goml_runtime_core_string_len(self__9)
    retv271 = t272
    return retv271
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv274 int
    var t275 int = _goml_runtime_core_string_len(self__8)
    retv274 = t275
    return retv274
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv277 rune
    var t278 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv277 = t278
    return retv277
}

func println__T_bool(value__1 bool) struct{} {
    var t280 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t280)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__17 string, index__18 int) bool {
    var retv283 bool
    var t284 bool = _goml_runtime_core_string_is_char_boundary(self__17, index__18)
    retv283 = t284
    return retv283
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv286 string
    var t287 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv286 = t287
    return retv286
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__19 string, index__20 int) _goml_m_Option_____o_char_c_int_q_ {
    var retv289 _goml_m_Option_____o_char_c_int_q_
    var mtmp3 Tuple3_4bool_4char_3int = _goml_runtime_core_string_decode_utf8_at(self__19, index__20)
    var x4 bool = mtmp3._0
    var x5 rune = mtmp3._1
    var x6 int = mtmp3._2
    var width__23 int = x6
    var value__22 rune = x5
    var valid__21 bool = x4
    var jp291 _goml_m_Option_____o_char_c_int_q_
    if valid__21 {
        var t292 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: value__22,
            _1: width__23,
        }
        var t293 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t292,
        }
        jp291 = t293
    } else {
        jp291 = _goml_m_Option_____o_char_c_int_q__None{}
    }
    retv289 = jp291
    return retv289
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var retv295 *_goml_vec_uint8
    var t296 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    retv295 = t296
    return retv295
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__139 *_goml_vec_uint8) int {
    var retv298 int
    var t299 int = vec_len__Vec_5uint8(self__139)
    retv298 = t299
    return retv298
}

func println__T_uint8(value__1 uint8) struct{} {
    var t301 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t301)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__134 *_goml_vec_uint8, index__135 int) uint8 {
    var retv304 uint8
    var t305 uint8 = vec_get__Vec_5uint8(self__134, index__135)
    retv304 = t305
    return retv304
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv307 *_goml_vec_uint8
    var t308 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv307 = t308
    return retv307
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__128 *_goml_vec_uint8, elem__129 uint8) struct{} {
    vec_push__Vec_5uint8(self__128, elem__129)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv312 bool
    var t313 bool = self__55 == other__56
    retv312 = t313
    return retv312
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__127 int) *_goml_vec_string {
    var retv315 *_goml_vec_string
    var t316 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__127)
    retv315 = t316
    return retv315
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__128 *_goml_vec_string, elem__129 string) struct{} {
    vec_push__Vec_6string(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__140 *_goml_vec_string) int {
    var retv320 int
    var t321 int = vec_capacity__Vec_6string(self__140)
    retv320 = t321
    return retv320
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__127 int) *_goml_vec_int32 {
    var retv323 *_goml_vec_int32
    var t324 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__127)
    retv323 = t324
    return retv323
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv326 int
    var t327 int = vec_len__Vec_5int32(self__139)
    retv326 = t327
    return retv326
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__140 *_goml_vec_int32) int {
    var retv329 int
    var t330 int = vec_capacity__Vec_5int32(self__140)
    retv329 = t330
    return retv329
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__142 *_goml_vec_int32, additional__143 int) struct{} {
    vec_reserve__Vec_5int32(self__142, additional__143)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__128 *_goml_vec_int32, elem__129 int32) struct{} {
    vec_push__Vec_5int32(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(self__160 *_goml_vec_int32, index__161 int, value__162 int32) struct{} {
    var len__163 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__160)
    var t337 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(index__161, len__163)
    if t337 {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__160, value__162)
        return struct{}{}
    } else {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__160, index__161)
        var t339 int = len__163 - 1
        var t340 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__160, t339)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__160, t340)
        var t341 int = len__163 - 1
        var current__164 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t341)
        Loop_loop344:
        for {
            var t345 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__164)
            var t346 bool = t345 > index__161
            if t346 {
                var t347 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__164)
                var t348 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__164)
                var t349 int = t348 - 1
                var t350 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__160, t349)
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__160, t347, t350)
                var t351 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__164)
                var t352 int = t351 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__164, t352)
                continue
            } else {
                break Loop_loop344
            }
        }
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__160, index__161, value__162)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t354 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t354)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__165 *_goml_vec_int32, index__166 int) int32 {
    var retv357 int32
    var len__167 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__165)
    var value__168 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__165, index__166)
    var current__169 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(index__166)
    Loop_loop360:
    for {
        var t361 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__169)
        var t362 int = t361 + 1
        var t363 bool = t362 < len__167
        if t363 {
            var t364 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__169)
            var t365 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__169)
            var t366 int = t365 + 1
            var t367 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__165, t366)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__165, t364, t367)
            var t368 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__169)
            var t369 int = t368 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__169, t369)
            continue
        } else {
            break Loop_loop360
        }
    }
    var t359 int = len__167 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__165, t359)
    retv357 = value__168
    return retv357
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__156 *_goml_vec_int32, index__157 int) int32 {
    var retv371 int32
    var len__158 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__156)
    var value__159 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__156, index__157)
    var t374 int = index__157 + 1
    var t375 bool = t374 < len__158
    if t375 {
        var t376 int = len__158 - 1
        var t377 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__156, t376)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__156, index__157, t377)
    } else {}
    var t373 int = len__158 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__156, t373)
    retv371 = value__159
    return retv371
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__170 *_goml_vec_int32) struct{} {
    var left__171 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t379 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__170)
    var t380 int = t379 - 1
    var right__172 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t380)
    Loop_loop382:
    for {
        var t383 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__171)
        var t384 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__172)
        var t385 bool = t383 < t384
        if t385 {
            var t386 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__171)
            var t387 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__172)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__170, t386, t387)
            var t388 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__171)
            var t389 int = t388 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(left__171, t389)
            var t390 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__172)
            var t391 int = t390 - 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(right__172, t391)
            continue
        } else {
            break Loop_loop382
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__134 *_goml_vec_int32, index__135 int) int32 {
    var retv393 int32
    var t394 int32 = vec_get__Vec_5int32(self__134, index__135)
    retv393 = t394
    return retv393
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__147 *_goml_vec_int32) Option__int32 {
    var retv396 Option__int32
    var len__148 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__147)
    var t399 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__148, 0)
    var jp398 Option__int32
    if t399 {
        jp398 = Option__int32_None{}
    } else {
        var t400 int = len__148 - 1
        var t401 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__147, t400)
        var t402 Option__int32 = Option__int32_Some{
            _0: t401,
        }
        jp398 = t402
    }
    retv396 = jp398
    return retv396
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__149 *_goml_vec_int32) Option__int32 {
    var retv404 Option__int32
    var len__150 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__149)
    var t407 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__150, 0)
    var jp406 Option__int32
    if t407 {
        jp406 = Option__int32_None{}
    } else {
        var t408 int = len__150 - 1
        var value__151 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__149, t408)
        var t409 int = len__150 - 1
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__149, t409)
        var t410 Option__int32 = Option__int32_Some{
            _0: value__151,
        }
        jp406 = t410
    }
    retv404 = jp406
    return retv404
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__144 *_goml_vec_int32, len__145 int) struct{} {
    vec_truncate__Vec_5int32(self__144, len__145)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__141 *_goml_vec_int32) bool {
    var retv414 bool
    var t415 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__141)
    var t416 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t415, 0)
    retv414 = t416
    return retv414
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__146 *_goml_vec_int32) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__146, 0)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv420 *hashmap_string_int32_x
    var t421 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv420 = t421
    return retv420
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__200 *hashmap_string_int32_x, key__201 string, value__202 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__200, key__201, value__202)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__208 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var retv425 *_goml_vec_Tuple2_6string_5int32
    var t426 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__208)
    retv425 = t426
    return retv425
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__139 *_goml_vec_Tuple2_6string_5int32) int {
    var retv428 int
    var t429 int = vec_len__Vec_21Tuple2_6string_5int32(self__139)
    retv428 = t429
    return retv428
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__203 *hashmap_string_int32_x, key__204 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__203, key__204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__209 bool) *ref_bool_x {
    var retv433 *ref_bool_x
    var t434 *ref_bool_x = ref__Ref_4bool(value__209)
    retv433 = t434
    return retv433
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv436 bool
    var t437 bool = self__65 == other__66
    retv436 = t437
    return retv436
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__211 *ref_bool_x, value__212 bool) struct{} {
    ref_set__Ref_4bool(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__210 *ref_bool_x) bool {
    var retv441 bool
    var t442 bool = ref_get__Ref_4bool(self__210)
    retv441 = t442
    return retv441
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv444 *ref_int_x
    var t445 *ref_int_x = ref__Ref_3int(value__209)
    retv444 = t445
    return retv444
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv447 int
    var t448 int = ref_get__Ref_3int(self__210)
    retv447 = t448
    return retv447
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__101 func() Option__char) FnIterator__char {
    var retv452 FnIterator__char
    var t453 FnIterator__char = FnIterator__char{
        next_fn: next_fn__101,
    }
    retv452 = t453
    return retv452
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__39 rune) string {
    var retv455 string
    var t456 string = _goml_runtime_core_char_to_string(self__39)
    retv455 = t456
    return retv455
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__101 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv458 _goml_m_FnIterator_____o_int_c_char_q_
    var t459 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__101,
    }
    retv458 = t459
    return retv458
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv461 string
    retv461 = self__38
    return retv461
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv463 string
    var t464 string = _goml_runtime_core_int_to_string(self__40)
    retv463 = t464
    return retv463
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv466 string
    var t467 string = _goml_runtime_core_bool_to_string(self__37)
    retv466 = t467
    return retv466
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv469 string
    var t470 string = _goml_runtime_core_uint8_to_string(self__45)
    retv469 = t470
    return retv469
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv472 bool
    var t473 bool = self__59 == other__60
    retv472 = t473
    return retv472
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__136 *_goml_vec_int32, index__137 int, elem__138 int32) struct{} {
    vec_set__Vec_5int32(self__136, index__137, elem__138)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv477 string
    var t478 string = _goml_runtime_core_int32_to_string(self__43)
    retv477 = t478
    return retv477
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__152 *_goml_vec_int32, left__153 int, right__154 int) struct{} {
    var value__155 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__152, left__153)
    var t480 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__152, right__154)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__152, left__153, t480)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__152, right__154, value__155)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv483 uint64
    var t484 uint64 = _goml_runtime_core_string_hash(self__83)
    retv483 = t484
    return retv483
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env155 closure_env_inherent_string_string_chars_0) Option__char {
    var retv496 Option__char
    var self__25 string = env155.self_0
    var index__26 *ref_int_x = env155.index_1
    var t497 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__26)
    var mtmp7 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__25, t497)
    var jp499 Option__char
    switch mtmp7.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        jp499 = Option__char_None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x8 Tuple2_4char_3int = mtmp7.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__27 Tuple2_4char_3int = x8
        var mtmp9 Tuple2_4char_3int = decoded__27
        var x10 rune = mtmp9._0
        var x11 int = mtmp9._1
        var width__29 int = x11
        var value__28 rune = x10
        var t500 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__26)
        var t501 int = t500 + width__29
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__26, t501)
        var t502 Option__char = Option__char_Some{
            _0: value__28,
        }
        jp499 = t502
    default:
        panic("non-exhaustive match")
    }
    retv496 = jp499
    return retv496
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env156 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var retv504 _goml_m_Option_____o_int_c_char_q_
    var index__31 *ref_int_x = env156.index_0
    var self__30 string = env156.self_1
    var current__32 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__31)
    var mtmp13 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__30, current__32)
    var jp506 _goml_m_Option_____o_int_c_char_q_
    switch mtmp13.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        jp506 = _goml_m_Option_____o_int_c_char_q__None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x14 Tuple2_4char_3int = mtmp13.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__33 Tuple2_4char_3int = x14
        var mtmp15 Tuple2_4char_3int = decoded__33
        var x16 rune = mtmp15._0
        var x17 int = mtmp15._1
        var width__35 int = x17
        var value__34 rune = x16
        var t507 int = current__32 + width__35
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__31, t507)
        var t508 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__32,
            _1: value__34,
        }
        var t509 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t508,
        }
        jp506 = t509
    default:
        panic("non-exhaustive match")
    }
    retv504 = jp506
    return retv504
}

func main() {
    main0()
}
