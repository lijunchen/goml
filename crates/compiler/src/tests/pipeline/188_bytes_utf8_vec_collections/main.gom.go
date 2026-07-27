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

type _goml_m_FnIterator_____o_string_c_int32_q_ struct {
    next_fn func() _goml_m_Option_____o_string_c_int32_q_
}

type closure_env_inherent_string_string_chars_0 struct {
    self_0 string
    index_1 *ref_int_x
}

type closure_env_inherent_string_string_char_indices_1 struct {
    index_0 *ref_int_x
    self_1 string
}

type closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec_Tuple2_6string_5int32
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

type _goml_m_Option_____o_string_c_int32_q_ interface {
    is_goml_m_Option_____o_string_c_int32_q_()
}

type _goml_m_Option_____o_string_c_int32_q__None struct {}

func (_ _goml_m_Option_____o_string_c_int32_q__None) is_goml_m_Option_____o_string_c_int32_q_() {}

type _goml_m_Option_____o_string_c_int32_q__Some struct {
    _0 Tuple2_6string_5int32
}

func (_ _goml_m_Option_____o_string_c_int32_q__Some) is_goml_m_Option_____o_string_c_int32_q_() {}

func print_chars(value__0 string) struct{} {
    var t157 FnIterator__char = _goml_m_inherent_i_string_i_string_i_chars(value__0)
    var for_iter64 FnIterator__char = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(t157)
    Loop_loop159:
    for {
        if true {
            var for_next65 Option__char = _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(for_iter64)
            switch for_next65.(type) {
            case Option__char_None:
                break Loop_loop159
            case Option__char_Some:
                var x66 rune = for_next65.(Option__char_Some)._0
                var character__1 rune = x66
                println__T_char(character__1)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop159
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t163 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_string_i_string_i_char__indices(value__2)
    var for_iter67 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(t163)
    Loop_loop165:
    for {
        if true {
            var for_next68 _goml_m_Option_____o_int_c_char_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(for_iter67)
            switch for_next68.(type) {
            case _goml_m_Option_____o_int_c_char_q__None:
                break Loop_loop165
            case _goml_m_Option_____o_int_c_char_q__Some:
                var x69 Tuple2_3int_4char = for_next68.(_goml_m_Option_____o_int_c_char_q__Some)._0
                var item__3 Tuple2_3int_4char = x69
                var mtmp70 Tuple2_3int_4char = item__3
                var x71 int = mtmp70._0
                var x72 rune = mtmp70._1
                var character__5 rune = x72
                var index__4 int = x71
                var t167 string = _goml_m_inherent_i_int_i_int_i_to__string(index__4)
                var t168 string = t167 + ":"
                var t169 string = _goml_m_inherent_i_char_i_char_i_to__string(character__5)
                var t170 string = t168 + t169
                println__T_string(t170)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop165
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t173 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t173)
    var t174 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t174)
    var t175 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t175)
    var t176 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t176)
    var t177 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t177)
    var t178 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t178)
    var t179 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t179)
    var t180 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t180)
    var t181 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t181)
    var t182 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t182)
    var t183 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t183)
    var t184 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t184)
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
    var t186 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t186)
    var t187 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t187)
    var t188 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t188)
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
    var t189 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(invalid_text__15, "")
    println__T_bool(t189)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t190 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t191 bool = t190 >= 3
    println__T_bool(t191)
    var t192 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t192)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t193 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t193)
    var t194 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t195 bool = t194 >= 1
    println__T_bool(t195)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t196 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t197 bool = t196 >= 100
    println__T_bool(t197)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t198 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t198)
    var t199 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t199)
    var t200 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t200)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t201 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t201)
    var t202 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t202)
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
    var t205 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t205)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t206 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t206)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t207 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t207)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t208 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t208)
    var seen_a__22 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var seen_b__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var for_iter145 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_string_c_int32_q__r__i_into__iter(entries__21)
    Loop_loop213:
    for {
        if true {
            var for_next146 _goml_m_Option_____o_string_c_int32_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_string_c_int32_q__i_next(for_iter145)
            switch for_next146.(type) {
            case _goml_m_Option_____o_string_c_int32_q__None:
                break Loop_loop213
            case _goml_m_Option_____o_string_c_int32_q__Some:
                var x147 Tuple2_6string_5int32 = for_next146.(_goml_m_Option_____o_string_c_int32_q__Some)._0
                var entry__24 Tuple2_6string_5int32 = x147
                var mtmp148 Tuple2_6string_5int32 = entry__24
                var x149 string = mtmp148._0
                var x150 int32 = mtmp148._1
                var item_value__26 int32 = x150
                var key__25 string = x149
                var t225 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "a")
                var jp217 bool
                if t225 {
                    var t226 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 1)
                    jp217 = t226
                } else {
                    jp217 = false
                }
                if jp217 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_a__22, true)
                } else {
                    var t223 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "b")
                    var jp221 bool
                    if t223 {
                        var t224 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 2)
                        jp221 = t224
                    } else {
                        jp221 = false
                    }
                    if jp221 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_b__23, true)
                    } else {}
                }
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop213
        }
    }
    var t210 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_a__22)
    println__T_bool(t210)
    var t211 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_b__23)
    println__T_bool(t211)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_chars(self__25 string) FnIterator__char {
    var retv234 FnIterator__char
    var index__26 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t235 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: self__25,
        index_1: index__26,
    }
    var t236 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(t235)
    })
    retv234 = t236
    return retv234
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(self__109 FnIterator__char) FnIterator__char {
    var retv238 FnIterator__char
    retv238 = self__109
    return retv238
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(self__102 FnIterator__char) Option__char {
    var retv240 Option__char
    var t241 func() Option__char = self__102.next_fn
    var t242 Option__char = t241()
    retv240 = t242
    return retv240
}

func println__T_char(value__1 rune) struct{} {
    var t244 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(value__1)
    _goml_runtime_core_string_println(t244)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_char__indices(self__30 string) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv247 _goml_m_FnIterator_____o_int_c_char_q_
    var index__31 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t248 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: index__31,
        self_1: self__30,
    }
    var t249 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(t248)
    })
    retv247 = t249
    return retv247
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(self__109 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv251 _goml_m_FnIterator_____o_int_c_char_q_
    retv251 = self__109
    return retv251
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(self__102 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_Option_____o_int_c_char_q_ {
    var retv253 _goml_m_Option_____o_int_c_char_q_
    var t254 func() _goml_m_Option_____o_int_c_char_q_ = self__102.next_fn
    var t255 _goml_m_Option_____o_int_c_char_q_ = t254()
    retv253 = t255
    return retv253
}

func println__T_string(value__1 string) struct{} {
    var t257 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t257)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv260 string
    var t261 string = _goml_runtime_core_int_to_string(self__5)
    retv260 = t261
    return retv260
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv263 string
    var t264 string = _goml_runtime_core_char_to_string(self__7)
    retv263 = t264
    return retv263
}

func println__T_int(value__1 int) struct{} {
    var t266 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t266)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv269 int
    var t270 int = _goml_runtime_core_string_len(self__9)
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv272 int
    var t273 int = _goml_runtime_core_string_len(self__8)
    retv272 = t273
    return retv272
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv275 rune
    var t276 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv275 = t276
    return retv275
}

func println__T_bool(value__1 bool) struct{} {
    var t278 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t278)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__17 string, index__18 int) bool {
    var retv281 bool
    var t282 bool = _goml_runtime_core_string_is_char_boundary(self__17, index__18)
    retv281 = t282
    return retv281
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv284 string
    var t285 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv284 = t285
    return retv284
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__19 string, index__20 int) _goml_m_Option_____o_char_c_int_q_ {
    var retv287 _goml_m_Option_____o_char_c_int_q_
    var mtmp3 Tuple3_4bool_4char_3int = _goml_runtime_core_string_decode_utf8_at(self__19, index__20)
    var x4 bool = mtmp3._0
    var x5 rune = mtmp3._1
    var x6 int = mtmp3._2
    var width__23 int = x6
    var value__22 rune = x5
    var valid__21 bool = x4
    var jp289 _goml_m_Option_____o_char_c_int_q_
    if valid__21 {
        var t290 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: value__22,
            _1: width__23,
        }
        var t291 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t290,
        }
        jp289 = t291
    } else {
        jp289 = _goml_m_Option_____o_char_c_int_q__None{}
    }
    retv287 = jp289
    return retv287
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var retv293 *_goml_vec_uint8
    var t294 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    retv293 = t294
    return retv293
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__139 *_goml_vec_uint8) int {
    var retv296 int
    var t297 int = vec_len__Vec_5uint8(self__139)
    retv296 = t297
    return retv296
}

func println__T_uint8(value__1 uint8) struct{} {
    var t299 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t299)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__134 *_goml_vec_uint8, index__135 int) uint8 {
    var retv302 uint8
    var t303 uint8 = vec_get__Vec_5uint8(self__134, index__135)
    retv302 = t303
    return retv302
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv305 *_goml_vec_uint8
    var t306 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv305 = t306
    return retv305
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__128 *_goml_vec_uint8, elem__129 uint8) struct{} {
    vec_push__Vec_5uint8(self__128, elem__129)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv310 bool
    var t311 bool = self__55 == other__56
    retv310 = t311
    return retv310
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__127 int) *_goml_vec_string {
    var retv313 *_goml_vec_string
    var t314 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__127)
    retv313 = t314
    return retv313
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__128 *_goml_vec_string, elem__129 string) struct{} {
    vec_push__Vec_6string(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__140 *_goml_vec_string) int {
    var retv318 int
    var t319 int = vec_capacity__Vec_6string(self__140)
    retv318 = t319
    return retv318
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__127 int) *_goml_vec_int32 {
    var retv321 *_goml_vec_int32
    var t322 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__127)
    retv321 = t322
    return retv321
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv324 int
    var t325 int = vec_len__Vec_5int32(self__139)
    retv324 = t325
    return retv324
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__140 *_goml_vec_int32) int {
    var retv327 int
    var t328 int = vec_capacity__Vec_5int32(self__140)
    retv327 = t328
    return retv327
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
    var t335 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(index__161, len__163)
    if t335 {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__160, value__162)
        return struct{}{}
    } else {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__160, index__161)
        var t337 int = len__163 - 1
        var t338 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__160, t337)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__160, t338)
        var t339 int = len__163 - 1
        var current__164 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t339)
        Loop_loop342:
        for {
            var t343 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__164)
            var t344 bool = t343 > index__161
            if t344 {
                var t345 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__164)
                var t346 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__164)
                var t347 int = t346 - 1
                var t348 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__160, t347)
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__160, t345, t348)
                var t349 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__164)
                var t350 int = t349 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__164, t350)
                continue
            } else {
                break Loop_loop342
            }
        }
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__160, index__161, value__162)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t352 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t352)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__165 *_goml_vec_int32, index__166 int) int32 {
    var retv355 int32
    var len__167 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__165)
    var value__168 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__165, index__166)
    var current__169 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(index__166)
    Loop_loop358:
    for {
        var t359 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__169)
        var t360 int = t359 + 1
        var t361 bool = t360 < len__167
        if t361 {
            var t362 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__169)
            var t363 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__169)
            var t364 int = t363 + 1
            var t365 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__165, t364)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__165, t362, t365)
            var t366 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__169)
            var t367 int = t366 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__169, t367)
            continue
        } else {
            break Loop_loop358
        }
    }
    var t357 int = len__167 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__165, t357)
    retv355 = value__168
    return retv355
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__156 *_goml_vec_int32, index__157 int) int32 {
    var retv369 int32
    var len__158 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__156)
    var value__159 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__156, index__157)
    var t372 int = index__157 + 1
    var t373 bool = t372 < len__158
    if t373 {
        var t374 int = len__158 - 1
        var t375 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__156, t374)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__156, index__157, t375)
    } else {}
    var t371 int = len__158 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__156, t371)
    retv369 = value__159
    return retv369
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__170 *_goml_vec_int32) struct{} {
    var left__171 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t377 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__170)
    var t378 int = t377 - 1
    var right__172 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t378)
    Loop_loop380:
    for {
        var t381 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__171)
        var t382 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__172)
        var t383 bool = t381 < t382
        if t383 {
            var t384 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__171)
            var t385 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__172)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__170, t384, t385)
            var t386 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__171)
            var t387 int = t386 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(left__171, t387)
            var t388 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__172)
            var t389 int = t388 - 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(right__172, t389)
            continue
        } else {
            break Loop_loop380
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__134 *_goml_vec_int32, index__135 int) int32 {
    var retv391 int32
    var t392 int32 = vec_get__Vec_5int32(self__134, index__135)
    retv391 = t392
    return retv391
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__147 *_goml_vec_int32) Option__int32 {
    var retv394 Option__int32
    var len__148 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__147)
    var t397 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__148, 0)
    var jp396 Option__int32
    if t397 {
        jp396 = Option__int32_None{}
    } else {
        var t398 int = len__148 - 1
        var t399 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__147, t398)
        var t400 Option__int32 = Option__int32_Some{
            _0: t399,
        }
        jp396 = t400
    }
    retv394 = jp396
    return retv394
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__149 *_goml_vec_int32) Option__int32 {
    var retv402 Option__int32
    var len__150 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__149)
    var t405 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__150, 0)
    var jp404 Option__int32
    if t405 {
        jp404 = Option__int32_None{}
    } else {
        var t406 int = len__150 - 1
        var value__151 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__149, t406)
        var t407 int = len__150 - 1
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__149, t407)
        var t408 Option__int32 = Option__int32_Some{
            _0: value__151,
        }
        jp404 = t408
    }
    retv402 = jp404
    return retv402
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__144 *_goml_vec_int32, len__145 int) struct{} {
    vec_truncate__Vec_5int32(self__144, len__145)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__141 *_goml_vec_int32) bool {
    var retv412 bool
    var t413 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__141)
    var t414 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t413, 0)
    retv412 = t414
    return retv412
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__146 *_goml_vec_int32) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__146, 0)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv418 *hashmap_string_int32_x
    var t419 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv418 = t419
    return retv418
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__200 *hashmap_string_int32_x, key__201 string, value__202 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__200, key__201, value__202)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__208 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var retv423 *_goml_vec_Tuple2_6string_5int32
    var t424 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__208)
    retv423 = t424
    return retv423
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__139 *_goml_vec_Tuple2_6string_5int32) int {
    var retv426 int
    var t427 int = vec_len__Vec_21Tuple2_6string_5int32(self__139)
    retv426 = t427
    return retv426
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__203 *hashmap_string_int32_x, key__204 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__203, key__204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__209 bool) *ref_bool_x {
    var retv431 *ref_bool_x
    var t432 *ref_bool_x = ref__Ref_4bool(value__209)
    retv431 = t432
    return retv431
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_string_c_int32_q__r__i_into__iter(self__185 *_goml_vec_Tuple2_6string_5int32) _goml_m_FnIterator_____o_string_c_int32_q_ {
    var retv434 _goml_m_FnIterator_____o_string_c_int32_q_
    var t435 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_string_c_int32_q_(self__185)
    retv434 = t435
    return retv434
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_string_c_int32_q__i_next(self__102 _goml_m_FnIterator_____o_string_c_int32_q_) _goml_m_Option_____o_string_c_int32_q_ {
    var retv437 _goml_m_Option_____o_string_c_int32_q_
    var t438 func() _goml_m_Option_____o_string_c_int32_q_ = self__102.next_fn
    var t439 _goml_m_Option_____o_string_c_int32_q_ = t438()
    retv437 = t439
    return retv437
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv441 bool
    var t442 bool = self__65 == other__66
    retv441 = t442
    return retv441
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__211 *ref_bool_x, value__212 bool) struct{} {
    ref_set__Ref_4bool(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__210 *ref_bool_x) bool {
    var retv446 bool
    var t447 bool = ref_get__Ref_4bool(self__210)
    retv446 = t447
    return retv446
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv449 *ref_int_x
    var t450 *ref_int_x = ref__Ref_3int(value__209)
    retv449 = t450
    return retv449
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv452 int
    var t453 int = ref_get__Ref_3int(self__210)
    retv452 = t453
    return retv452
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__101 func() Option__char) FnIterator__char {
    var retv457 FnIterator__char
    var t458 FnIterator__char = FnIterator__char{
        next_fn: next_fn__101,
    }
    retv457 = t458
    return retv457
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__39 rune) string {
    var retv460 string
    var t461 string = _goml_runtime_core_char_to_string(self__39)
    retv460 = t461
    return retv460
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__101 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv463 _goml_m_FnIterator_____o_int_c_char_q_
    var t464 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__101,
    }
    retv463 = t464
    return retv463
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv466 string
    retv466 = self__38
    return retv466
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv468 string
    var t469 string = _goml_runtime_core_int_to_string(self__40)
    retv468 = t469
    return retv468
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv471 string
    var t472 string = _goml_runtime_core_bool_to_string(self__37)
    retv471 = t472
    return retv471
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv474 string
    var t475 string = _goml_runtime_core_uint8_to_string(self__45)
    retv474 = t475
    return retv474
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv477 bool
    var t478 bool = self__59 == other__60
    retv477 = t478
    return retv477
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__136 *_goml_vec_int32, index__137 int, elem__138 int32) struct{} {
    vec_set__Vec_5int32(self__136, index__137, elem__138)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv482 string
    var t483 string = _goml_runtime_core_int32_to_string(self__43)
    retv482 = t483
    return retv482
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__152 *_goml_vec_int32, left__153 int, right__154 int) struct{} {
    var value__155 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__152, left__153)
    var t485 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__152, right__154)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__152, left__153, t485)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__152, right__154, value__155)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_string_c_int32_q_(self__180 *_goml_vec_Tuple2_6string_5int32) _goml_m_FnIterator_____o_string_c_int32_q_ {
    var retv488 _goml_m_FnIterator_____o_string_c_int32_q_
    var index__181 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__182 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__180)
    var t489 closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2 = closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2{
        index_0: index__181,
        len_1: len__182,
        self_2: self__180,
    }
    var t490 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_inherent_i_FnIterator__h4aab6f157cede91513543003d0ae8727_ring_c_int32_q_(func() _goml_m_Option_____o_string_c_int32_q_ {
        return _goml_m_inherent_i_closure__en_hb124241ac92f068ce7e0775da15a1ab4_nt32__2_i_apply(t489)
    })
    retv488 = t490
    return retv488
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_string_c_int32_q_(self__134 *_goml_vec_Tuple2_6string_5int32, index__135 int) Tuple2_6string_5int32 {
    var retv492 Tuple2_6string_5int32
    var t493 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(self__134, index__135)
    retv492 = t493
    return retv492
}

func _goml_m_inherent_i_FnIterator__h4aab6f157cede91513543003d0ae8727_ring_c_int32_q_(next_fn__101 func() _goml_m_Option_____o_string_c_int32_q_) _goml_m_FnIterator_____o_string_c_int32_q_ {
    var retv495 _goml_m_FnIterator_____o_string_c_int32_q_
    var t496 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_FnIterator_____o_string_c_int32_q_{
        next_fn: next_fn__101,
    }
    retv495 = t496
    return retv495
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv498 uint64
    var t499 uint64 = _goml_runtime_core_string_hash(self__83)
    retv498 = t499
    return retv498
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env153 closure_env_inherent_string_string_chars_0) Option__char {
    var retv511 Option__char
    var self__25 string = env153.self_0
    var index__26 *ref_int_x = env153.index_1
    var t512 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__26)
    var mtmp7 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__25, t512)
    var jp514 Option__char
    switch mtmp7.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        jp514 = Option__char_None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x8 Tuple2_4char_3int = mtmp7.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__27 Tuple2_4char_3int = x8
        var mtmp9 Tuple2_4char_3int = decoded__27
        var x10 rune = mtmp9._0
        var x11 int = mtmp9._1
        var width__29 int = x11
        var value__28 rune = x10
        var t515 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__26)
        var t516 int = t515 + width__29
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__26, t516)
        var t517 Option__char = Option__char_Some{
            _0: value__28,
        }
        jp514 = t517
    default:
        panic("non-exhaustive match")
    }
    retv511 = jp514
    return retv511
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env154 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var retv519 _goml_m_Option_____o_int_c_char_q_
    var index__31 *ref_int_x = env154.index_0
    var self__30 string = env154.self_1
    var current__32 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__31)
    var mtmp13 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__30, current__32)
    var jp521 _goml_m_Option_____o_int_c_char_q_
    switch mtmp13.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        jp521 = _goml_m_Option_____o_int_c_char_q__None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x14 Tuple2_4char_3int = mtmp13.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__33 Tuple2_4char_3int = x14
        var mtmp15 Tuple2_4char_3int = decoded__33
        var x16 rune = mtmp15._0
        var x17 int = mtmp15._1
        var width__35 int = x17
        var value__34 rune = x16
        var t522 int = current__32 + width__35
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__31, t522)
        var t523 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__32,
            _1: value__34,
        }
        var t524 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t523,
        }
        jp521 = t524
    default:
        panic("non-exhaustive match")
    }
    retv519 = jp521
    return retv519
}

func _goml_m_inherent_i_closure__en_hb124241ac92f068ce7e0775da15a1ab4_nt32__2_i_apply(env155 closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2) _goml_m_Option_____o_string_c_int32_q_ {
    var retv526 _goml_m_Option_____o_string_c_int32_q_
    var index__181 *ref_int_x = env155.index_0
    var len__182 int = env155.len_1
    var self__180 *_goml_vec_Tuple2_6string_5int32 = env155.self_2
    var current__183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__181)
    var t529 bool = current__183 < len__182
    var jp528 _goml_m_Option_____o_string_c_int32_q_
    if t529 {
        var value__184 Tuple2_6string_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_string_c_int32_q_(self__180, current__183)
        var t530 int = current__183 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__181, t530)
        var t531 _goml_m_Option_____o_string_c_int32_q_ = _goml_m_Option_____o_string_c_int32_q__Some{
            _0: value__184,
        }
        jp528 = t531
    } else {
        jp528 = _goml_m_Option_____o_string_c_int32_q__None{}
    }
    retv526 = jp528
    return retv526
}

func main() {
    main0()
}
