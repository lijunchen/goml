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

func _goml_runtime_core_string_len(s string) int32 {
    return int32(len(s))
}

func _goml_runtime_string_decode_utf8_at_native(s string, i int32) (bool, rune, int32) {
    if i < 0 || i >= int32(len(s)) {
        return false, 0, 0
    }
    var value rune
    var width int
    value, width = _goml_utf8.DecodeRuneInString(s[i:int32(len(s))])
    if value == _goml_utf8.RuneError && width == 1 {
        return false, 0, 0
    }
    return true, value, int32(width)
}

func _goml_runtime_core_string_get(s string, i int32) rune {
    var valid bool
    var value rune
    valid, value, _ = _goml_runtime_string_decode_utf8_at_native(s, i)
    if !valid {
        panic("invalid string byte index")
    }
    return value
}

func _goml_runtime_core_string_byte_slice(s string, start int32, end int32) string {
    if !_goml_runtime_core_string_is_char_boundary(s, start) && _goml_runtime_core_string_is_char_boundary(s, end) {
        panic("invalid string byte slice")
    }
    return s[start:end]
}

func _goml_runtime_core_string_is_char_boundary(s string, i int32) bool {
    if i < 0 || i > int32(len(s)) {
        return false
    }
    if i == int32(len(s)) {
        return true
    }
    return _goml_utf8.RuneStart(s[i])
}

func _goml_runtime_core_string_decode_utf8_at(s string, i int32) Tuple3_4bool_4char_5int32 {
    var valid bool
    var value rune
    var width int32
    valid, value, width = _goml_runtime_string_decode_utf8_at_native(s, i)
    return Tuple3_4bool_4char_5int32{
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

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_hash(s string) uint64 {
    var h uint64 = 14695981039346656037
    var i int32 = 0
    for {
        if i >= int32(len(s)) {
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

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int32) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int32 {
    return int32(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_with_capacity__Vec_6string(capacity int32) *_goml_vec_string {
    return &_goml_vec_string{
        items: _goml_slices.Grow([]string{}, int(capacity)),
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_capacity__Vec_6string(vec *_goml_vec_string) int32 {
    return int32(cap(vec.items))
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_with_capacity__Vec_5int32(capacity int32) *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: _goml_slices.Grow([]int32{}, int(capacity)),
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int32) int32 {
    return vec.items[index]
}

func vec_set__Vec_5int32(vec *_goml_vec_int32, index int32, value int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
}

func vec_capacity__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(cap(vec.items))
}

func vec_reserve__Vec_5int32(vec *_goml_vec_int32, additional int32) struct{} {
    vec.items = _goml_slices.Grow(vec.items, int(additional))
    return struct{}{}
}

func vec_truncate__Vec_5int32(vec *_goml_vec_int32, new_len int32) struct{} {
    if new_len < 0 {
        panic("negative vector length")
    }
    if new_len < int32(len(vec.items)) {
        clear(vec.items[new_len:int32(len(vec.items))])
        vec.items = vec.items[0:new_len]
    }
    return struct{}{}
}

type _goml_vec_Tuple2_6string_5int32 struct {
    items []Tuple2_6string_5int32
}

func vec_get__Vec_21Tuple2_6string_5int32(vec *_goml_vec_Tuple2_6string_5int32, index int32) Tuple2_6string_5int32 {
    return vec.items[index]
}

func vec_len__Vec_21Tuple2_6string_5int32(vec *_goml_vec_Tuple2_6string_5int32) int32 {
    return int32(len(vec.items))
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

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
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
    len int32
}

func hashmap_new__HashMap_6string_5int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        buckets: make(map[uint64][]hashmap_string_int32_x_entry),
        len: 0,
        hashes: nil,
    }
}

func hashmap_set__HashMap_6string_5int32(m *hashmap_string_int32_x, key string, value int32) struct{} {
    var reuse_index int32 = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int32_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
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
    var i int32 = 0
    for {
        if i >= int32(len(bucket)) {
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
        var i int32 = 0
        for {
            if i >= int32(len(bucket)) {
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

type Tuple2_5int32_4char struct {
    _0 int32
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

type Tuple2_4char_5int32 struct {
    _0 rune
    _1 int32
}

type Tuple3_4bool_4char_5int32 struct {
    _0 bool
    _1 rune
    _2 int32
}

type FnIterator__char struct {
    next_fn func() Option__char
}

type _goml_m_FnIterator_____o_int32_c_char_q_ struct {
    next_fn func() _goml_m_Option_____o_int32_c_char_q_
}

type _goml_m_FnIterator_____o_string_c_int32_q_ struct {
    next_fn func() _goml_m_Option_____o_string_c_int32_q_
}

type closure_env_inherent_string_string_chars_0 struct {
    self_0 string
    index_1 *ref_int32_x
}

type closure_env_inherent_string_string_char_indices_1 struct {
    index_0 *ref_int32_x
    self_1 string
}

type closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2 struct {
    index_0 *ref_int32_x
    len_1 int32
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

type _goml_m_Option_____o_int32_c_char_q_ interface {
    is_goml_m_Option_____o_int32_c_char_q_()
}

type _goml_m_Option_____o_int32_c_char_q__None struct {}

func (_ _goml_m_Option_____o_int32_c_char_q__None) is_goml_m_Option_____o_int32_c_char_q_() {}

type _goml_m_Option_____o_int32_c_char_q__Some struct {
    _0 Tuple2_5int32_4char
}

func (_ _goml_m_Option_____o_int32_c_char_q__Some) is_goml_m_Option_____o_int32_c_char_q_() {}

type _goml_m_Option_____o_char_c_int32_q_ interface {
    is_goml_m_Option_____o_char_c_int32_q_()
}

type _goml_m_Option_____o_char_c_int32_q__None struct {}

func (_ _goml_m_Option_____o_char_c_int32_q__None) is_goml_m_Option_____o_char_c_int32_q_() {}

type _goml_m_Option_____o_char_c_int32_q__Some struct {
    _0 Tuple2_4char_5int32
}

func (_ _goml_m_Option_____o_char_c_int32_q__Some) is_goml_m_Option_____o_char_c_int32_q_() {}

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
    var t154 FnIterator__char = _goml_m_inherent_i_string_i_string_i_chars(value__0)
    var for_iter61 FnIterator__char = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(t154)
    Loop_loop156:
    for {
        if true {
            var for_next62 Option__char = _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(for_iter61)
            switch for_next62.(type) {
            case Option__char_None:
                break Loop_loop156
            case Option__char_Some:
                var x63 rune = for_next62.(Option__char_Some)._0
                var character__1 rune = x63
                println__T_char(character__1)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop156
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t160 _goml_m_FnIterator_____o_int32_c_char_q_ = _goml_m_inherent_i_string_i_string_i_char__indices(value__2)
    var for_iter64 _goml_m_FnIterator_____o_int32_c_char_q_ = _goml_m_trait__impl_i_IntoIter_hd465d08633f2d8f7a43ba12dc5f21517_q__i_into__iter(t160)
    Loop_loop162:
    for {
        if true {
            var for_next65 _goml_m_Option_____o_int32_c_char_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_char_q__i_next(for_iter64)
            switch for_next65.(type) {
            case _goml_m_Option_____o_int32_c_char_q__None:
                break Loop_loop162
            case _goml_m_Option_____o_int32_c_char_q__Some:
                var x66 Tuple2_5int32_4char = for_next65.(_goml_m_Option_____o_int32_c_char_q__Some)._0
                var item__3 Tuple2_5int32_4char = x66
                var mtmp67 Tuple2_5int32_4char = item__3
                var x68 int32 = mtmp67._0
                var x69 rune = mtmp67._1
                var character__5 rune = x69
                var index__4 int32 = x68
                var t164 string = _goml_m_inherent_i_int32_i_int32_i_to__string(index__4)
                var t165 string = t164 + ":"
                var t166 string = _goml_m_inherent_i_char_i_char_i_to__string(character__5)
                var t167 string = t165 + t166
                println__T_string(t167)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop162
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t170 int32 = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int32(t170)
    var t171 int32 = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int32(t171)
    var t172 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t172)
    var t173 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t173)
    var t174 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t174)
    var t175 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t175)
    var t176 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t176)
    var t177 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t177)
    var t178 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t178)
    var t179 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t179)
    var t180 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t180)
    var t181 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t181)
    var mtmp82 _goml_m_Option_____o_char_c_int32_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp82.(type) {
    case _goml_m_Option_____o_char_c_int32_q__None:
        println__T_string("missing")
    case _goml_m_Option_____o_char_c_int32_q__Some:
        var x83 Tuple2_4char_5int32 = mtmp82.(_goml_m_Option_____o_char_c_int32_q__Some)._0
        var decoded__7 Tuple2_4char_5int32 = x83
        var mtmp84 Tuple2_4char_5int32 = decoded__7
        var x85 rune = mtmp84._0
        var x86 int32 = mtmp84._1
        var width__9 int32 = x86
        var character__8 rune = x85
        println__T_char(character__8)
        println__T_int32(width__9)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t183 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int32(t183)
    var t184 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t184)
    var t185 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t185)
    var mtmp94 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__10)
    var x95 bool = mtmp94._0
    var x96 string = mtmp94._1
    var roundtrip__12 string = x96
    var roundtrip_valid__11 bool = x95
    println__T_bool(roundtrip_valid__11)
    println__T_string(roundtrip__12)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp101 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(invalid__13)
    var x102 bool = mtmp101._0
    var x103 string = mtmp101._1
    var invalid_text__15 string = x103
    var invalid_valid__14 bool = x102
    println__T_bool(invalid_valid__14)
    var t186 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(invalid_text__15, "")
    println__T_bool(t186)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t187 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t188 bool = t187 >= 3
    println__T_bool(t188)
    var t189 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t189)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t190 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int32(t190)
    var t191 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t192 bool = t191 >= 1
    println__T_bool(t192)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t193 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t194 bool = t193 >= 100
    println__T_bool(t194)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t195 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int32(t195)
    var t196 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t196)
    var t197 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t197)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t198 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t198)
    var t199 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t199)
    var mtmp125 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp125.(type) {
    case Option__int32_None:
        println__T_int32(-1)
    case Option__int32_Some:
        var x126 int32 = mtmp125.(Option__int32_Some)._0
        var value__18 int32 = x126
        println__T_int32(value__18)
    default:
        panic("non-exhaustive match")
    }
    var mtmp128 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp128.(type) {
    case Option__int32_None:
        println__T_int32(-1)
    case Option__int32_Some:
        var x129 int32 = mtmp128.(Option__int32_Some)._0
        var value__19 int32 = x129
        println__T_int32(value__19)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t202 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t202)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t203 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int32(t203)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t204 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int32(t204)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t205 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int32(t205)
    var seen_a__22 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var seen_b__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var for_iter142 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_string_c_int32_q__r__i_into__iter(entries__21)
    Loop_loop210:
    for {
        if true {
            var for_next143 _goml_m_Option_____o_string_c_int32_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_string_c_int32_q__i_next(for_iter142)
            switch for_next143.(type) {
            case _goml_m_Option_____o_string_c_int32_q__None:
                break Loop_loop210
            case _goml_m_Option_____o_string_c_int32_q__Some:
                var x144 Tuple2_6string_5int32 = for_next143.(_goml_m_Option_____o_string_c_int32_q__Some)._0
                var entry__24 Tuple2_6string_5int32 = x144
                var mtmp145 Tuple2_6string_5int32 = entry__24
                var x146 string = mtmp145._0
                var x147 int32 = mtmp145._1
                var item_value__26 int32 = x147
                var key__25 string = x146
                var t222 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "a")
                var jp214 bool
                if t222 {
                    var t223 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 1)
                    jp214 = t223
                } else {
                    jp214 = false
                }
                if jp214 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_a__22, true)
                } else {
                    var t220 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "b")
                    var jp218 bool
                    if t220 {
                        var t221 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 2)
                        jp218 = t221
                    } else {
                        jp218 = false
                    }
                    if jp218 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_b__23, true)
                    } else {}
                }
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop210
        }
    }
    var t207 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_a__22)
    println__T_bool(t207)
    var t208 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_b__23)
    println__T_bool(t208)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_chars(self__24 string) FnIterator__char {
    var retv231 FnIterator__char
    var index__25 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t232 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: self__24,
        index_1: index__25,
    }
    var t233 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(t232)
    })
    retv231 = t233
    return retv231
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(self__104 FnIterator__char) FnIterator__char {
    var retv235 FnIterator__char
    retv235 = self__104
    return retv235
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(self__97 FnIterator__char) Option__char {
    var retv237 Option__char
    var t238 func() Option__char = self__97.next_fn
    var t239 Option__char = t238()
    retv237 = t239
    return retv237
}

func println__T_char(value__1 rune) struct{} {
    var t241 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(value__1)
    _goml_runtime_core_string_println(t241)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_char__indices(self__29 string) _goml_m_FnIterator_____o_int32_c_char_q_ {
    var retv244 _goml_m_FnIterator_____o_int32_c_char_q_
    var index__30 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t245 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: index__30,
        self_1: self__29,
    }
    var t246 _goml_m_FnIterator_____o_int32_c_char_q_ = _goml_m_inherent_i_FnIterator__h81b975155429c11a603dd605befbfb23_int32_c_char_q_(func() _goml_m_Option_____o_int32_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(t245)
    })
    retv244 = t246
    return retv244
}

func _goml_m_trait__impl_i_IntoIter_hd465d08633f2d8f7a43ba12dc5f21517_q__i_into__iter(self__104 _goml_m_FnIterator_____o_int32_c_char_q_) _goml_m_FnIterator_____o_int32_c_char_q_ {
    var retv248 _goml_m_FnIterator_____o_int32_c_char_q_
    retv248 = self__104
    return retv248
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_char_q__i_next(self__97 _goml_m_FnIterator_____o_int32_c_char_q_) _goml_m_Option_____o_int32_c_char_q_ {
    var retv250 _goml_m_Option_____o_int32_c_char_q_
    var t251 func() _goml_m_Option_____o_int32_c_char_q_ = self__97.next_fn
    var t252 _goml_m_Option_____o_int32_c_char_q_ = t251()
    retv250 = t252
    return retv250
}

func println__T_string(value__1 string) struct{} {
    var t254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t254)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv257 string
    var t258 string = _goml_runtime_core_int32_to_string(self__5)
    retv257 = t258
    return retv257
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__6 rune) string {
    var retv260 string
    var t261 string = _goml_runtime_core_char_to_string(self__6)
    retv260 = t261
    return retv260
}

func println__T_int32(value__1 int32) struct{} {
    var t263 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t263)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__8 string) int32 {
    var retv266 int32
    var t267 int32 = _goml_runtime_core_string_len(self__8)
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_string_i_string_i_len(self__7 string) int32 {
    var retv269 int32
    var t270 int32 = _goml_runtime_core_string_len(self__7)
    retv269 = t270
    return retv269
}

func _goml_m_inherent_i_string_i_string_i_get(self__9 string, index__10 int32) rune {
    var retv272 rune
    var t273 rune = _goml_runtime_core_string_get(self__9, index__10)
    retv272 = t273
    return retv272
}

func println__T_bool(value__1 bool) struct{} {
    var t275 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t275)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__16 string, index__17 int32) bool {
    var retv278 bool
    var t279 bool = _goml_runtime_core_string_is_char_boundary(self__16, index__17)
    retv278 = t279
    return retv278
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__13 string, start__14 int32, end__15 int32) string {
    var retv281 string
    var t282 string = _goml_runtime_core_string_byte_slice(self__13, start__14, end__15)
    retv281 = t282
    return retv281
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__18 string, index__19 int32) _goml_m_Option_____o_char_c_int32_q_ {
    var retv284 _goml_m_Option_____o_char_c_int32_q_
    var mtmp3 Tuple3_4bool_4char_5int32 = _goml_runtime_core_string_decode_utf8_at(self__18, index__19)
    var x4 bool = mtmp3._0
    var x5 rune = mtmp3._1
    var x6 int32 = mtmp3._2
    var width__22 int32 = x6
    var value__21 rune = x5
    var valid__20 bool = x4
    var jp286 _goml_m_Option_____o_char_c_int32_q_
    if valid__20 {
        var t287 Tuple2_4char_5int32 = Tuple2_4char_5int32{
            _0: value__21,
            _1: width__22,
        }
        var t288 _goml_m_Option_____o_char_c_int32_q_ = _goml_m_Option_____o_char_c_int32_q__Some{
            _0: t287,
        }
        jp286 = t288
    } else {
        jp286 = _goml_m_Option_____o_char_c_int32_q__None{}
    }
    retv284 = jp286
    return retv284
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__23 string) *_goml_vec_uint8 {
    var retv290 *_goml_vec_uint8
    var t291 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__23)
    retv290 = t291
    return retv290
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__134 *_goml_vec_uint8) int32 {
    var retv293 int32
    var t294 int32 = vec_len__Vec_5uint8(self__134)
    retv293 = t294
    return retv293
}

func println__T_uint8(value__1 uint8) struct{} {
    var t296 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t296)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__129 *_goml_vec_uint8, index__130 int32) uint8 {
    var retv299 uint8
    var t300 uint8 = vec_get__Vec_5uint8(self__129, index__130)
    retv299 = t300
    return retv299
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv302 *_goml_vec_uint8
    var t303 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv302 = t303
    return retv302
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__123 *_goml_vec_uint8, elem__124 uint8) struct{} {
    vec_push__Vec_5uint8(self__123, elem__124)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__53 string, other__54 string) bool {
    var retv307 bool
    var t308 bool = self__53 == other__54
    retv307 = t308
    return retv307
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__122 int32) *_goml_vec_string {
    var retv310 *_goml_vec_string
    var t311 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__122)
    retv310 = t311
    return retv310
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__123 *_goml_vec_string, elem__124 string) struct{} {
    vec_push__Vec_6string(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__135 *_goml_vec_string) int32 {
    var retv315 int32
    var t316 int32 = vec_capacity__Vec_6string(self__135)
    retv315 = t316
    return retv315
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__122 int32) *_goml_vec_int32 {
    var retv318 *_goml_vec_int32
    var t319 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__122)
    retv318 = t319
    return retv318
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv321 int32
    var t322 int32 = vec_len__Vec_5int32(self__134)
    retv321 = t322
    return retv321
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__135 *_goml_vec_int32) int32 {
    var retv324 int32
    var t325 int32 = vec_capacity__Vec_5int32(self__135)
    retv324 = t325
    return retv324
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__137 *_goml_vec_int32, additional__138 int32) struct{} {
    vec_reserve__Vec_5int32(self__137, additional__138)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__123 *_goml_vec_int32, elem__124 int32) struct{} {
    vec_push__Vec_5int32(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(self__155 *_goml_vec_int32, index__156 int32, value__157 int32) struct{} {
    var len__158 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__155)
    var t332 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(index__156, len__158)
    if t332 {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__155, value__157)
        return struct{}{}
    } else {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__155, index__156)
        var t334 int32 = len__158 - 1
        var t335 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__155, t334)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__155, t335)
        var t336 int32 = len__158 - 1
        var current__159 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t336)
        Loop_loop339:
        for {
            var t340 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__159)
            var t341 bool = t340 > index__156
            if t341 {
                var t342 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__159)
                var t343 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__159)
                var t344 int32 = t343 - 1
                var t345 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__155, t344)
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__155, t342, t345)
                var t346 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__159)
                var t347 int32 = t346 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__159, t347)
                continue
            } else {
                break Loop_loop339
            }
        }
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__155, index__156, value__157)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__160 *_goml_vec_int32, index__161 int32) int32 {
    var retv349 int32
    var len__162 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__160)
    var value__163 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__160, index__161)
    var current__164 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(index__161)
    Loop_loop352:
    for {
        var t353 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__164)
        var t354 int32 = t353 + 1
        var t355 bool = t354 < len__162
        if t355 {
            var t356 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__164)
            var t357 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__164)
            var t358 int32 = t357 + 1
            var t359 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__160, t358)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__160, t356, t359)
            var t360 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__164)
            var t361 int32 = t360 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__164, t361)
            continue
        } else {
            break Loop_loop352
        }
    }
    var t351 int32 = len__162 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__160, t351)
    retv349 = value__163
    return retv349
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__151 *_goml_vec_int32, index__152 int32) int32 {
    var retv363 int32
    var len__153 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__151)
    var value__154 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__151, index__152)
    var t366 int32 = index__152 + 1
    var t367 bool = t366 < len__153
    if t367 {
        var t368 int32 = len__153 - 1
        var t369 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__151, t368)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__151, index__152, t369)
    } else {}
    var t365 int32 = len__153 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__151, t365)
    retv363 = value__154
    return retv363
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__165 *_goml_vec_int32) struct{} {
    var left__166 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t371 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__165)
    var t372 int32 = t371 - 1
    var right__167 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t372)
    Loop_loop374:
    for {
        var t375 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(left__166)
        var t376 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(right__167)
        var t377 bool = t375 < t376
        if t377 {
            var t378 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(left__166)
            var t379 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(right__167)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__165, t378, t379)
            var t380 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(left__166)
            var t381 int32 = t380 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(left__166, t381)
            var t382 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(right__167)
            var t383 int32 = t382 - 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(right__167, t383)
            continue
        } else {
            break Loop_loop374
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__129 *_goml_vec_int32, index__130 int32) int32 {
    var retv385 int32
    var t386 int32 = vec_get__Vec_5int32(self__129, index__130)
    retv385 = t386
    return retv385
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__142 *_goml_vec_int32) Option__int32 {
    var retv388 Option__int32
    var len__143 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__142)
    var t391 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(len__143, 0)
    var jp390 Option__int32
    if t391 {
        jp390 = Option__int32_None{}
    } else {
        var t392 int32 = len__143 - 1
        var t393 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__142, t392)
        var t394 Option__int32 = Option__int32_Some{
            _0: t393,
        }
        jp390 = t394
    }
    retv388 = jp390
    return retv388
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__144 *_goml_vec_int32) Option__int32 {
    var retv396 Option__int32
    var len__145 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__144)
    var t399 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(len__145, 0)
    var jp398 Option__int32
    if t399 {
        jp398 = Option__int32_None{}
    } else {
        var t400 int32 = len__145 - 1
        var value__146 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__144, t400)
        var t401 int32 = len__145 - 1
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__144, t401)
        var t402 Option__int32 = Option__int32_Some{
            _0: value__146,
        }
        jp398 = t402
    }
    retv396 = jp398
    return retv396
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__139 *_goml_vec_int32, len__140 int32) struct{} {
    vec_truncate__Vec_5int32(self__139, len__140)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__136 *_goml_vec_int32) bool {
    var retv406 bool
    var t407 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__136)
    var t408 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t407, 0)
    retv406 = t408
    return retv406
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__141 *_goml_vec_int32) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__141, 0)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv412 *hashmap_string_int32_x
    var t413 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv412 = t413
    return retv412
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__195 *hashmap_string_int32_x, key__196 string, value__197 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__195, key__196, value__197)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__203 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var retv417 *_goml_vec_Tuple2_6string_5int32
    var t418 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__203)
    retv417 = t418
    return retv417
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__134 *_goml_vec_Tuple2_6string_5int32) int32 {
    var retv420 int32
    var t421 int32 = vec_len__Vec_21Tuple2_6string_5int32(self__134)
    retv420 = t421
    return retv420
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__198 *hashmap_string_int32_x, key__199 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__198, key__199)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__204 bool) *ref_bool_x {
    var retv425 *ref_bool_x
    var t426 *ref_bool_x = ref__Ref_4bool(value__204)
    retv425 = t426
    return retv425
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_string_c_int32_q__r__i_into__iter(self__180 *_goml_vec_Tuple2_6string_5int32) _goml_m_FnIterator_____o_string_c_int32_q_ {
    var retv428 _goml_m_FnIterator_____o_string_c_int32_q_
    var t429 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_string_c_int32_q_(self__180)
    retv428 = t429
    return retv428
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_string_c_int32_q__i_next(self__97 _goml_m_FnIterator_____o_string_c_int32_q_) _goml_m_Option_____o_string_c_int32_q_ {
    var retv431 _goml_m_Option_____o_string_c_int32_q_
    var t432 func() _goml_m_Option_____o_string_c_int32_q_ = self__97.next_fn
    var t433 _goml_m_Option_____o_string_c_int32_q_ = t432()
    retv431 = t433
    return retv431
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv435 bool
    var t436 bool = self__61 == other__62
    retv435 = t436
    return retv435
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__206 *ref_bool_x, value__207 bool) struct{} {
    ref_set__Ref_4bool(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__205 *ref_bool_x) bool {
    var retv440 bool
    var t441 bool = ref_get__Ref_4bool(self__205)
    retv440 = t441
    return retv440
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv443 *ref_int32_x
    var t444 *ref_int32_x = ref__Ref_5int32(value__204)
    retv443 = t444
    return retv443
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv446 int32
    var t447 int32 = ref_get__Ref_5int32(self__205)
    retv446 = t447
    return retv446
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__96 func() Option__char) FnIterator__char {
    var retv451 FnIterator__char
    var t452 FnIterator__char = FnIterator__char{
        next_fn: next_fn__96,
    }
    retv451 = t452
    return retv451
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__38 rune) string {
    var retv454 string
    var t455 string = _goml_runtime_core_char_to_string(self__38)
    retv454 = t455
    return retv454
}

func _goml_m_inherent_i_FnIterator__h81b975155429c11a603dd605befbfb23_int32_c_char_q_(next_fn__96 func() _goml_m_Option_____o_int32_c_char_q_) _goml_m_FnIterator_____o_int32_c_char_q_ {
    var retv457 _goml_m_FnIterator_____o_int32_c_char_q_
    var t458 _goml_m_FnIterator_____o_int32_c_char_q_ = _goml_m_FnIterator_____o_int32_c_char_q_{
        next_fn: next_fn__96,
    }
    retv457 = t458
    return retv457
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv460 string
    retv460 = self__37
    return retv460
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv462 string
    var t463 string = _goml_runtime_core_int32_to_string(self__41)
    retv462 = t463
    return retv462
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv465 string
    var t466 string = _goml_runtime_core_bool_to_string(self__36)
    retv465 = t466
    return retv465
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__43 uint8) string {
    var retv468 string
    var t469 string = _goml_runtime_core_uint8_to_string(self__43)
    retv468 = t469
    return retv468
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__131 *_goml_vec_int32, index__132 int32, elem__133 int32) struct{} {
    vec_set__Vec_5int32(self__131, index__132, elem__133)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__147 *_goml_vec_int32, left__148 int32, right__149 int32) struct{} {
    var value__150 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__147, left__148)
    var t473 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__147, right__149)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__147, left__148, t473)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__147, right__149, value__150)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_string_c_int32_q_(self__175 *_goml_vec_Tuple2_6string_5int32) _goml_m_FnIterator_____o_string_c_int32_q_ {
    var retv476 _goml_m_FnIterator_____o_string_c_int32_q_
    var index__176 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__177 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__175)
    var t477 closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2 = closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2{
        index_0: index__176,
        len_1: len__177,
        self_2: self__175,
    }
    var t478 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_inherent_i_FnIterator__h4aab6f157cede91513543003d0ae8727_ring_c_int32_q_(func() _goml_m_Option_____o_string_c_int32_q_ {
        return _goml_m_inherent_i_closure__en_hb124241ac92f068ce7e0775da15a1ab4_nt32__2_i_apply(t477)
    })
    retv476 = t478
    return retv476
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_string_c_int32_q_(self__129 *_goml_vec_Tuple2_6string_5int32, index__130 int32) Tuple2_6string_5int32 {
    var retv480 Tuple2_6string_5int32
    var t481 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(self__129, index__130)
    retv480 = t481
    return retv480
}

func _goml_m_inherent_i_FnIterator__h4aab6f157cede91513543003d0ae8727_ring_c_int32_q_(next_fn__96 func() _goml_m_Option_____o_string_c_int32_q_) _goml_m_FnIterator_____o_string_c_int32_q_ {
    var retv483 _goml_m_FnIterator_____o_string_c_int32_q_
    var t484 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_FnIterator_____o_string_c_int32_q_{
        next_fn: next_fn__96,
    }
    retv483 = t484
    return retv483
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__79 string) uint64 {
    var retv486 uint64
    var t487 uint64 = _goml_runtime_core_string_hash(self__79)
    retv486 = t487
    return retv486
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env150 closure_env_inherent_string_string_chars_0) Option__char {
    var retv499 Option__char
    var self__24 string = env150.self_0
    var index__25 *ref_int32_x = env150.index_1
    var t500 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__25)
    var mtmp7 _goml_m_Option_____o_char_c_int32_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__24, t500)
    var jp502 Option__char
    switch mtmp7.(type) {
    case _goml_m_Option_____o_char_c_int32_q__None:
        jp502 = Option__char_None{}
    case _goml_m_Option_____o_char_c_int32_q__Some:
        var x8 Tuple2_4char_5int32 = mtmp7.(_goml_m_Option_____o_char_c_int32_q__Some)._0
        var decoded__26 Tuple2_4char_5int32 = x8
        var mtmp9 Tuple2_4char_5int32 = decoded__26
        var x10 rune = mtmp9._0
        var x11 int32 = mtmp9._1
        var width__28 int32 = x11
        var value__27 rune = x10
        var t503 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__25)
        var t504 int32 = t503 + width__28
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__25, t504)
        var t505 Option__char = Option__char_Some{
            _0: value__27,
        }
        jp502 = t505
    default:
        panic("non-exhaustive match")
    }
    retv499 = jp502
    return retv499
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env151 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int32_c_char_q_ {
    var retv507 _goml_m_Option_____o_int32_c_char_q_
    var index__30 *ref_int32_x = env151.index_0
    var self__29 string = env151.self_1
    var current__31 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__30)
    var mtmp13 _goml_m_Option_____o_char_c_int32_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__29, current__31)
    var jp509 _goml_m_Option_____o_int32_c_char_q_
    switch mtmp13.(type) {
    case _goml_m_Option_____o_char_c_int32_q__None:
        jp509 = _goml_m_Option_____o_int32_c_char_q__None{}
    case _goml_m_Option_____o_char_c_int32_q__Some:
        var x14 Tuple2_4char_5int32 = mtmp13.(_goml_m_Option_____o_char_c_int32_q__Some)._0
        var decoded__32 Tuple2_4char_5int32 = x14
        var mtmp15 Tuple2_4char_5int32 = decoded__32
        var x16 rune = mtmp15._0
        var x17 int32 = mtmp15._1
        var width__34 int32 = x17
        var value__33 rune = x16
        var t510 int32 = current__31 + width__34
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__30, t510)
        var t511 Tuple2_5int32_4char = Tuple2_5int32_4char{
            _0: current__31,
            _1: value__33,
        }
        var t512 _goml_m_Option_____o_int32_c_char_q_ = _goml_m_Option_____o_int32_c_char_q__Some{
            _0: t511,
        }
        jp509 = t512
    default:
        panic("non-exhaustive match")
    }
    retv507 = jp509
    return retv507
}

func _goml_m_inherent_i_closure__en_hb124241ac92f068ce7e0775da15a1ab4_nt32__2_i_apply(env152 closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2) _goml_m_Option_____o_string_c_int32_q_ {
    var retv514 _goml_m_Option_____o_string_c_int32_q_
    var index__176 *ref_int32_x = env152.index_0
    var len__177 int32 = env152.len_1
    var self__175 *_goml_vec_Tuple2_6string_5int32 = env152.self_2
    var current__178 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__176)
    var t517 bool = current__178 < len__177
    var jp516 _goml_m_Option_____o_string_c_int32_q_
    if t517 {
        var value__179 Tuple2_6string_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_string_c_int32_q_(self__175, current__178)
        var t518 int32 = current__178 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__176, t518)
        var t519 _goml_m_Option_____o_string_c_int32_q_ = _goml_m_Option_____o_string_c_int32_q__Some{
            _0: value__179,
        }
        jp516 = t519
    } else {
        jp516 = _goml_m_Option_____o_string_c_int32_q__None{}
    }
    retv514 = jp516
    return retv514
}

func main() {
    main0()
}
