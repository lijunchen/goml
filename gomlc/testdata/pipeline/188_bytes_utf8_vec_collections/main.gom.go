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
    var t202 FnIterator__char = _goml_m_inherent_i_string_i_string_i_chars(value__0)
    var for_iter108 FnIterator__char = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(t202)
    Loop_loop204:
    for {
        if true {
            var for_next109 Option__char = _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(for_iter108)
            switch for_next109.(type) {
            case Option__char_None:
                break Loop_loop204
            case Option__char_Some:
                var x110 rune = for_next109.(Option__char_Some)._0
                var character__1 rune = x110
                println__T_char(character__1)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop204
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t208 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_string_i_string_i_char__indices(value__2)
    var for_iter111 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(t208)
    Loop_loop210:
    for {
        if true {
            var for_next112 _goml_m_Option_____o_int_c_char_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(for_iter111)
            switch for_next112.(type) {
            case _goml_m_Option_____o_int_c_char_q__None:
                break Loop_loop210
            case _goml_m_Option_____o_int_c_char_q__Some:
                var x113 Tuple2_3int_4char = for_next112.(_goml_m_Option_____o_int_c_char_q__Some)._0
                var item__3 Tuple2_3int_4char = x113
                var mtmp114 Tuple2_3int_4char = item__3
                var x115 int = mtmp114._0
                var x116 rune = mtmp114._1
                var character__5 rune = x116
                var index__4 int = x115
                var t212 string = _goml_m_inherent_i_int_i_int_i_to__string(index__4)
                var t213 string = t212 + ":"
                var t214 string = _goml_m_inherent_i_char_i_char_i_to__string(character__5)
                var t215 string = t213 + t214
                println__T_string(t215)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop210
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t218 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t218)
    var t219 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t219)
    var t220 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t220)
    var t221 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t221)
    var t222 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t222)
    var t223 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t223)
    var t224 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t224)
    var t225 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t225)
    var t226 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t226)
    var t227 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t227)
    var t228 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t228)
    var t229 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t229)
    var mtmp129 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp129.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        println__T_string("missing")
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x130 Tuple2_4char_3int = mtmp129.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__7 Tuple2_4char_3int = x130
        var mtmp131 Tuple2_4char_3int = decoded__7
        var x132 rune = mtmp131._0
        var x133 int = mtmp131._1
        var width__9 int = x133
        var character__8 rune = x132
        println__T_char(character__8)
        println__T_int(width__9)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t231 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t231)
    var t232 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t232)
    var t233 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t233)
    var mtmp141 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__10)
    var x142 bool = mtmp141._0
    var x143 string = mtmp141._1
    var roundtrip__12 string = x143
    var roundtrip_valid__11 bool = x142
    println__T_bool(roundtrip_valid__11)
    println__T_string(roundtrip__12)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp148 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(invalid__13)
    var x149 bool = mtmp148._0
    var x150 string = mtmp148._1
    var invalid_text__15 string = x150
    var invalid_valid__14 bool = x149
    println__T_bool(invalid_valid__14)
    var t234 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(invalid_text__15, "")
    println__T_bool(t234)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t235 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t236 bool = t235 >= 3
    println__T_bool(t236)
    var t237 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t237)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t238 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t238)
    var t239 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t240 bool = t239 >= 1
    println__T_bool(t240)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t241 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t242 bool = t241 >= 100
    println__T_bool(t242)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t243 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t243)
    var t244 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t244)
    var t245 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t245)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t246 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t246)
    var t247 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t247)
    var mtmp172 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp172.(type) {
    case Option__int32_None:
        println__T_int(-1)
    case Option__int32_Some:
        var x173 int32 = mtmp172.(Option__int32_Some)._0
        var value__18 int32 = x173
        println__T_int32(value__18)
    default:
        panic("non-exhaustive match")
    }
    var mtmp175 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp175.(type) {
    case Option__int32_None:
        println__T_int(-1)
    case Option__int32_Some:
        var x176 int32 = mtmp175.(Option__int32_Some)._0
        var value__19 int32 = x176
        println__T_int32(value__19)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t250 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t250)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t251 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t251)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t252 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t252)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t253 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t253)
    var seen_a__22 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var seen_b__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var for_source189 *_goml_vec_Tuple2_6string_5int32 = entries__21
    var for_limit190 int = vec_len__Vec_21Tuple2_6string_5int32(for_source189)
    var for_index191 int = 0
    Loop_loop258:
    for {
        var t259 bool = for_index191 < for_limit190
        if t259 {
            var for_item192 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(for_source189, for_index191)
            var t260 int = for_index191 + 1
            for_index191 = t260
            var entry__24 Tuple2_6string_5int32 = for_item192
            var mtmp194 Tuple2_6string_5int32 = entry__24
            var x195 string = mtmp194._0
            var x196 int32 = mtmp194._1
            var item_value__26 int32 = x196
            var key__25 string = x195
            var t271 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "a")
            var jp263 bool
            if t271 {
                var t272 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 1)
                jp263 = t272
            } else {
                jp263 = false
            }
            if jp263 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_a__22, true)
            } else {
                var t269 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "b")
                var jp267 bool
                if t269 {
                    var t270 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 2)
                    jp267 = t270
                } else {
                    jp267 = false
                }
                if jp267 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_b__23, true)
                } else {}
            }
            continue
        } else {
            break Loop_loop258
        }
    }
    var t255 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_a__22)
    println__T_bool(t255)
    var t256 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_b__23)
    println__T_bool(t256)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_chars(self__25 string) FnIterator__char {
    var retv280 FnIterator__char
    var index__26 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t281 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: self__25,
        index_1: index__26,
    }
    var t282 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(t281)
    })
    retv280 = t282
    return retv280
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(self__109 FnIterator__char) FnIterator__char {
    var retv284 FnIterator__char
    retv284 = self__109
    return retv284
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(self__102 FnIterator__char) Option__char {
    var retv286 Option__char
    var t287 func() Option__char = self__102.next_fn
    var t288 Option__char = t287()
    retv286 = t288
    return retv286
}

func println__T_char(value__1 rune) struct{} {
    var t290 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(value__1)
    _goml_runtime_core_string_println(t290)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_char__indices(self__30 string) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv293 _goml_m_FnIterator_____o_int_c_char_q_
    var index__31 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t294 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: index__31,
        self_1: self__30,
    }
    var t295 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(t294)
    })
    retv293 = t295
    return retv293
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(self__109 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv297 _goml_m_FnIterator_____o_int_c_char_q_
    retv297 = self__109
    return retv297
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(self__102 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_Option_____o_int_c_char_q_ {
    var retv299 _goml_m_Option_____o_int_c_char_q_
    var t300 func() _goml_m_Option_____o_int_c_char_q_ = self__102.next_fn
    var t301 _goml_m_Option_____o_int_c_char_q_ = t300()
    retv299 = t301
    return retv299
}

func println__T_string(value__1 string) struct{} {
    var t303 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t303)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv306 string
    var t307 string = _goml_runtime_core_int_to_string(self__5)
    retv306 = t307
    return retv306
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv309 string
    var t310 string = _goml_runtime_core_char_to_string(self__7)
    retv309 = t310
    return retv309
}

func println__T_int(value__1 int) struct{} {
    var t312 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t312)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv315 int
    var t316 int = _goml_runtime_core_string_len(self__9)
    retv315 = t316
    return retv315
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv318 int
    var t319 int = _goml_runtime_core_string_len(self__8)
    retv318 = t319
    return retv318
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv321 rune
    var t322 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv321 = t322
    return retv321
}

func println__T_bool(value__1 bool) struct{} {
    var t324 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t324)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__17 string, index__18 int) bool {
    var retv327 bool
    var t328 bool = _goml_runtime_core_string_is_char_boundary(self__17, index__18)
    retv327 = t328
    return retv327
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv330 string
    var t331 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv330 = t331
    return retv330
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__19 string, index__20 int) _goml_m_Option_____o_char_c_int_q_ {
    var retv333 _goml_m_Option_____o_char_c_int_q_
    var mtmp3 Tuple3_4bool_4char_3int = _goml_runtime_core_string_decode_utf8_at(self__19, index__20)
    var x4 bool = mtmp3._0
    var x5 rune = mtmp3._1
    var x6 int = mtmp3._2
    var width__23 int = x6
    var value__22 rune = x5
    var valid__21 bool = x4
    var jp335 _goml_m_Option_____o_char_c_int_q_
    if valid__21 {
        var t336 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: value__22,
            _1: width__23,
        }
        var t337 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t336,
        }
        jp335 = t337
    } else {
        jp335 = _goml_m_Option_____o_char_c_int_q__None{}
    }
    retv333 = jp335
    return retv333
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var retv339 *_goml_vec_uint8
    var t340 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    retv339 = t340
    return retv339
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__137 *_goml_vec_uint8) int {
    var retv342 int
    var t343 int = vec_len__Vec_5uint8(self__137)
    retv342 = t343
    return retv342
}

func println__T_uint8(value__1 uint8) struct{} {
    var t345 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t345)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__132 *_goml_vec_uint8, index__133 int) uint8 {
    var retv348 uint8
    var t349 uint8 = vec_get__Vec_5uint8(self__132, index__133)
    retv348 = t349
    return retv348
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv351 *_goml_vec_uint8
    var t352 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv351 = t352
    return retv351
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__126 *_goml_vec_uint8, elem__127 uint8) struct{} {
    vec_push__Vec_5uint8(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv356 bool
    var t357 bool = self__55 == other__56
    retv356 = t357
    return retv356
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__125 int) *_goml_vec_string {
    var retv359 *_goml_vec_string
    var t360 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__125)
    retv359 = t360
    return retv359
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__138 *_goml_vec_string) int {
    var retv364 int
    var t365 int = vec_capacity__Vec_6string(self__138)
    retv364 = t365
    return retv364
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__125 int) *_goml_vec_int32 {
    var retv367 *_goml_vec_int32
    var t368 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__125)
    retv367 = t368
    return retv367
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv370 int
    var t371 int = vec_len__Vec_5int32(self__137)
    retv370 = t371
    return retv370
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__138 *_goml_vec_int32) int {
    var retv373 int
    var t374 int = vec_capacity__Vec_5int32(self__138)
    retv373 = t374
    return retv373
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
    var t381 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(index__159, len__161)
    if t381 {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__158, value__160)
        return struct{}{}
    } else {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__158, index__159)
        var t383 int = len__161 - 1
        var t384 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__158, t383)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__158, t384)
        var t385 int = len__161 - 1
        var current__162 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t385)
        Loop_loop388:
        for {
            var t389 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__162)
            var t390 bool = t389 > index__159
            if t390 {
                var t391 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__162)
                var t392 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__162)
                var t393 int = t392 - 1
                var t394 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__158, t393)
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__158, t391, t394)
                var t395 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__162)
                var t396 int = t395 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__162, t396)
                continue
            } else {
                break Loop_loop388
            }
        }
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__158, index__159, value__160)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t398 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t398)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__163 *_goml_vec_int32, index__164 int) int32 {
    var retv401 int32
    var len__165 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__163)
    var value__166 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__163, index__164)
    var current__167 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(index__164)
    Loop_loop404:
    for {
        var t405 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__167)
        var t406 int = t405 + 1
        var t407 bool = t406 < len__165
        if t407 {
            var t408 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__167)
            var t409 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__167)
            var t410 int = t409 + 1
            var t411 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__163, t410)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__163, t408, t411)
            var t412 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__167)
            var t413 int = t412 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__167, t413)
            continue
        } else {
            break Loop_loop404
        }
    }
    var t403 int = len__165 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__163, t403)
    retv401 = value__166
    return retv401
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__154 *_goml_vec_int32, index__155 int) int32 {
    var retv415 int32
    var len__156 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__154)
    var value__157 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__154, index__155)
    var t418 int = index__155 + 1
    var t419 bool = t418 < len__156
    if t419 {
        var t420 int = len__156 - 1
        var t421 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__154, t420)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__154, index__155, t421)
    } else {}
    var t417 int = len__156 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__154, t417)
    retv415 = value__157
    return retv415
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__168 *_goml_vec_int32) struct{} {
    var left__169 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t423 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__168)
    var t424 int = t423 - 1
    var right__170 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t424)
    Loop_loop426:
    for {
        var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__169)
        var t428 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__170)
        var t429 bool = t427 < t428
        if t429 {
            var t430 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__169)
            var t431 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__170)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__168, t430, t431)
            var t432 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(left__169)
            var t433 int = t432 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(left__169, t433)
            var t434 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(right__170)
            var t435 int = t434 - 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(right__170, t435)
            continue
        } else {
            break Loop_loop426
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv437 int32
    var t438 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv437 = t438
    return retv437
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__145 *_goml_vec_int32) Option__int32 {
    var retv440 Option__int32
    var len__146 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__145)
    var t443 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__146, 0)
    var jp442 Option__int32
    if t443 {
        jp442 = Option__int32_None{}
    } else {
        var t444 int = len__146 - 1
        var t445 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__145, t444)
        var t446 Option__int32 = Option__int32_Some{
            _0: t445,
        }
        jp442 = t446
    }
    retv440 = jp442
    return retv440
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__147 *_goml_vec_int32) Option__int32 {
    var retv448 Option__int32
    var len__148 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__147)
    var t451 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__148, 0)
    var jp450 Option__int32
    if t451 {
        jp450 = Option__int32_None{}
    } else {
        var t452 int = len__148 - 1
        var value__149 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__147, t452)
        var t453 int = len__148 - 1
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__147, t453)
        var t454 Option__int32 = Option__int32_Some{
            _0: value__149,
        }
        jp450 = t454
    }
    retv448 = jp450
    return retv448
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__142 *_goml_vec_int32, len__143 int) struct{} {
    vec_truncate__Vec_5int32(self__142, len__143)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__139 *_goml_vec_int32) bool {
    var retv458 bool
    var t459 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139)
    var t460 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t459, 0)
    retv458 = t460
    return retv458
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__144 *_goml_vec_int32) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__144, 0)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv464 *hashmap_string_int32_x
    var t465 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv464 = t465
    return retv464
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__198 *hashmap_string_int32_x, key__199 string, value__200 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__206 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var retv469 *_goml_vec_Tuple2_6string_5int32
    var t470 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__206)
    retv469 = t470
    return retv469
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__137 *_goml_vec_Tuple2_6string_5int32) int {
    var retv472 int
    var t473 int = vec_len__Vec_21Tuple2_6string_5int32(self__137)
    retv472 = t473
    return retv472
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__201 *hashmap_string_int32_x, key__202 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__201, key__202)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv477 *ref_bool_x
    var t478 *ref_bool_x = ref__Ref_4bool(value__207)
    retv477 = t478
    return retv477
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv480 bool
    var t481 bool = self__65 == other__66
    retv480 = t481
    return retv480
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv485 bool
    var t486 bool = ref_get__Ref_4bool(self__208)
    retv485 = t486
    return retv485
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv488 *ref_int_x
    var t489 *ref_int_x = ref__Ref_3int(value__207)
    retv488 = t489
    return retv488
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv491 int
    var t492 int = ref_get__Ref_3int(self__208)
    retv491 = t492
    return retv491
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__101 func() Option__char) FnIterator__char {
    var retv496 FnIterator__char
    var t497 FnIterator__char = FnIterator__char{
        next_fn: next_fn__101,
    }
    retv496 = t497
    return retv496
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__39 rune) string {
    var retv499 string
    var t500 string = _goml_runtime_core_char_to_string(self__39)
    retv499 = t500
    return retv499
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__101 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv502 _goml_m_FnIterator_____o_int_c_char_q_
    var t503 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__101,
    }
    retv502 = t503
    return retv502
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv505 string
    retv505 = self__38
    return retv505
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv507 string
    var t508 string = _goml_runtime_core_int_to_string(self__40)
    retv507 = t508
    return retv507
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv510 string
    var t511 string = _goml_runtime_core_bool_to_string(self__37)
    retv510 = t511
    return retv510
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv513 string
    var t514 string = _goml_runtime_core_uint8_to_string(self__45)
    retv513 = t514
    return retv513
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv516 bool
    var t517 bool = self__59 == other__60
    retv516 = t517
    return retv516
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__134 *_goml_vec_int32, index__135 int, elem__136 int32) struct{} {
    vec_set__Vec_5int32(self__134, index__135, elem__136)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv521 string
    var t522 string = _goml_runtime_core_int32_to_string(self__43)
    retv521 = t522
    return retv521
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__150 *_goml_vec_int32, left__151 int, right__152 int) struct{} {
    var value__153 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__150, left__151)
    var t524 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__150, right__152)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__150, left__151, t524)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__150, right__152, value__153)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv527 uint64
    var t528 uint64 = _goml_runtime_core_string_hash(self__83)
    retv527 = t528
    return retv527
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env199 closure_env_inherent_string_string_chars_0) Option__char {
    var retv540 Option__char
    var self__25 string = env199.self_0
    var index__26 *ref_int_x = env199.index_1
    var t541 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__26)
    var mtmp7 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__25, t541)
    var jp543 Option__char
    switch mtmp7.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        jp543 = Option__char_None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x8 Tuple2_4char_3int = mtmp7.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__27 Tuple2_4char_3int = x8
        var mtmp9 Tuple2_4char_3int = decoded__27
        var x10 rune = mtmp9._0
        var x11 int = mtmp9._1
        var width__29 int = x11
        var value__28 rune = x10
        var t544 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__26)
        var t545 int = t544 + width__29
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__26, t545)
        var t546 Option__char = Option__char_Some{
            _0: value__28,
        }
        jp543 = t546
    default:
        panic("non-exhaustive match")
    }
    retv540 = jp543
    return retv540
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env200 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var retv548 _goml_m_Option_____o_int_c_char_q_
    var index__31 *ref_int_x = env200.index_0
    var self__30 string = env200.self_1
    var current__32 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__31)
    var mtmp13 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__30, current__32)
    var jp550 _goml_m_Option_____o_int_c_char_q_
    switch mtmp13.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        jp550 = _goml_m_Option_____o_int_c_char_q__None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x14 Tuple2_4char_3int = mtmp13.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__33 Tuple2_4char_3int = x14
        var mtmp15 Tuple2_4char_3int = decoded__33
        var x16 rune = mtmp15._0
        var x17 int = mtmp15._1
        var width__35 int = x17
        var value__34 rune = x16
        var t551 int = current__32 + width__35
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__31, t551)
        var t552 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__32,
            _1: value__34,
        }
        var t553 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t552,
        }
        jp550 = t553
    default:
        panic("non-exhaustive match")
    }
    retv548 = jp550
    return retv548
}

func main() {
    main0()
}
