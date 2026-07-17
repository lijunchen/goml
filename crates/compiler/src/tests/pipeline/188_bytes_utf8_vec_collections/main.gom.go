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
    var t151 FnIterator__char = _goml_m_inherent_i_string_i_string_i_chars(value__0)
    var for_iter58 FnIterator__char = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(t151)
    Loop_loop153:
    for {
        if true {
            var for_next59 Option__char = _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(for_iter58)
            switch for_next59.(type) {
            case Option__char_None:
                break Loop_loop153
            case Option__char_Some:
                var x60 rune = for_next59.(Option__char_Some)._0
                var character__1 rune = x60
                println__T_char(character__1)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop153
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t157 _goml_m_FnIterator_____o_int32_c_char_q_ = _goml_m_inherent_i_string_i_string_i_char__indices(value__2)
    var for_iter61 _goml_m_FnIterator_____o_int32_c_char_q_ = _goml_m_trait__impl_i_IntoIter_hd465d08633f2d8f7a43ba12dc5f21517_q__i_into__iter(t157)
    Loop_loop159:
    for {
        if true {
            var for_next62 _goml_m_Option_____o_int32_c_char_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_char_q__i_next(for_iter61)
            switch for_next62.(type) {
            case _goml_m_Option_____o_int32_c_char_q__None:
                break Loop_loop159
            case _goml_m_Option_____o_int32_c_char_q__Some:
                var x63 Tuple2_5int32_4char = for_next62.(_goml_m_Option_____o_int32_c_char_q__Some)._0
                var item__3 Tuple2_5int32_4char = x63
                var mtmp64 Tuple2_5int32_4char = item__3
                var x65 int32 = mtmp64._0
                var x66 rune = mtmp64._1
                var character__5 rune = x66
                var index__4 int32 = x65
                var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(index__4)
                var t162 string = t161 + ":"
                var t163 string = _goml_m_inherent_i_char_i_char_i_to__string(character__5)
                var t164 string = t162 + t163
                println__T_string(t164)
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

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t167 int32 = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int32(t167)
    var t168 int32 = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int32(t168)
    var t169 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t169)
    var t170 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t170)
    var t171 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t171)
    var t172 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t172)
    var t173 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t173)
    var t174 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t174)
    var t175 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t175)
    var t176 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t176)
    var t177 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t177)
    var t178 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t178)
    var mtmp79 _goml_m_Option_____o_char_c_int32_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp79.(type) {
    case _goml_m_Option_____o_char_c_int32_q__None:
        println__T_string("missing")
    case _goml_m_Option_____o_char_c_int32_q__Some:
        var x80 Tuple2_4char_5int32 = mtmp79.(_goml_m_Option_____o_char_c_int32_q__Some)._0
        var decoded__7 Tuple2_4char_5int32 = x80
        var mtmp81 Tuple2_4char_5int32 = decoded__7
        var x82 rune = mtmp81._0
        var x83 int32 = mtmp81._1
        var width__9 int32 = x83
        var character__8 rune = x82
        println__T_char(character__8)
        println__T_int32(width__9)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t180 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int32(t180)
    var t181 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t181)
    var t182 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t182)
    var mtmp91 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__10)
    var x92 bool = mtmp91._0
    var x93 string = mtmp91._1
    var roundtrip__12 string = x93
    var roundtrip_valid__11 bool = x92
    println__T_bool(roundtrip_valid__11)
    println__T_string(roundtrip__12)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp98 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(invalid__13)
    var x99 bool = mtmp98._0
    var x100 string = mtmp98._1
    var invalid_text__15 string = x100
    var invalid_valid__14 bool = x99
    println__T_bool(invalid_valid__14)
    var t183 bool = invalid_text__15 == ""
    println__T_bool(t183)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t184 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t185 bool = t184 >= 3
    println__T_bool(t185)
    var t186 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t186)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t187 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int32(t187)
    var t188 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t189 bool = t188 >= 1
    println__T_bool(t189)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t190 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t191 bool = t190 >= 100
    println__T_bool(t191)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t192 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int32(t192)
    var t193 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t193)
    var t194 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t194)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t195 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t195)
    var t196 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t196)
    var mtmp122 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp122.(type) {
    case Option__int32_None:
        println__T_int32(-1)
    case Option__int32_Some:
        var x123 int32 = mtmp122.(Option__int32_Some)._0
        var value__18 int32 = x123
        println__T_int32(value__18)
    default:
        panic("non-exhaustive match")
    }
    var mtmp125 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp125.(type) {
    case Option__int32_None:
        println__T_int32(-1)
    case Option__int32_Some:
        var x126 int32 = mtmp125.(Option__int32_Some)._0
        var value__19 int32 = x126
        println__T_int32(value__19)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t199 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t199)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t200 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int32(t200)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t201 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int32(t201)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t202 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int32(t202)
    var seen_a__22 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var seen_b__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var for_iter139 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_string_c_int32_q__r__i_into__iter(entries__21)
    Loop_loop207:
    for {
        if true {
            var for_next140 _goml_m_Option_____o_string_c_int32_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_string_c_int32_q__i_next(for_iter139)
            switch for_next140.(type) {
            case _goml_m_Option_____o_string_c_int32_q__None:
                break Loop_loop207
            case _goml_m_Option_____o_string_c_int32_q__Some:
                var x141 Tuple2_6string_5int32 = for_next140.(_goml_m_Option_____o_string_c_int32_q__Some)._0
                var entry__24 Tuple2_6string_5int32 = x141
                var mtmp142 Tuple2_6string_5int32 = entry__24
                var x143 string = mtmp142._0
                var x144 int32 = mtmp142._1
                var item_value__26 int32 = x144
                var key__25 string = x143
                var t219 bool = key__25 == "a"
                var jp211 bool
                if t219 {
                    var t220 bool = item_value__26 == 1
                    jp211 = t220
                } else {
                    jp211 = false
                }
                if jp211 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_a__22, true)
                } else {
                    var t217 bool = key__25 == "b"
                    var jp215 bool
                    if t217 {
                        var t218 bool = item_value__26 == 2
                        jp215 = t218
                    } else {
                        jp215 = false
                    }
                    if jp215 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_b__23, true)
                    } else {}
                }
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop207
        }
    }
    var t204 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_a__22)
    println__T_bool(t204)
    var t205 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_b__23)
    println__T_bool(t205)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_chars(self__21 string) FnIterator__char {
    var retv228 FnIterator__char
    var index__22 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t229 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: self__21,
        index_1: index__22,
    }
    var t230 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(t229)
    })
    retv228 = t230
    return retv228
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(self__101 FnIterator__char) FnIterator__char {
    var retv232 FnIterator__char
    retv232 = self__101
    return retv232
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(self__94 FnIterator__char) Option__char {
    var retv234 Option__char
    var t235 func() Option__char = self__94.next_fn
    var t236 Option__char = t235()
    retv234 = t236
    return retv234
}

func println__T_char(value__1 rune) struct{} {
    var t238 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(value__1)
    _goml_runtime_core_string_println(t238)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_char__indices(self__26 string) _goml_m_FnIterator_____o_int32_c_char_q_ {
    var retv241 _goml_m_FnIterator_____o_int32_c_char_q_
    var index__27 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t242 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: index__27,
        self_1: self__26,
    }
    var t243 _goml_m_FnIterator_____o_int32_c_char_q_ = _goml_m_inherent_i_FnIterator__h81b975155429c11a603dd605befbfb23_int32_c_char_q_(func() _goml_m_Option_____o_int32_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(t242)
    })
    retv241 = t243
    return retv241
}

func _goml_m_trait__impl_i_IntoIter_hd465d08633f2d8f7a43ba12dc5f21517_q__i_into__iter(self__101 _goml_m_FnIterator_____o_int32_c_char_q_) _goml_m_FnIterator_____o_int32_c_char_q_ {
    var retv245 _goml_m_FnIterator_____o_int32_c_char_q_
    retv245 = self__101
    return retv245
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int32_c_char_q__i_next(self__94 _goml_m_FnIterator_____o_int32_c_char_q_) _goml_m_Option_____o_int32_c_char_q_ {
    var retv247 _goml_m_Option_____o_int32_c_char_q_
    var t248 func() _goml_m_Option_____o_int32_c_char_q_ = self__94.next_fn
    var t249 _goml_m_Option_____o_int32_c_char_q_ = t248()
    retv247 = t249
    return retv247
}

func println__T_string(value__1 string) struct{} {
    var t251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t251)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv254 string
    var t255 string = _goml_runtime_core_int32_to_string(self__2)
    retv254 = t255
    return retv254
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv257 string
    var t258 string = _goml_runtime_core_char_to_string(self__3)
    retv257 = t258
    return retv257
}

func println__T_int32(value__1 int32) struct{} {
    var t260 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t260)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__5 string) int32 {
    var retv263 int32
    var t264 int32 = _goml_runtime_core_string_len(self__5)
    retv263 = t264
    return retv263
}

func _goml_m_inherent_i_string_i_string_i_len(self__4 string) int32 {
    var retv266 int32
    var t267 int32 = _goml_runtime_core_string_len(self__4)
    retv266 = t267
    return retv266
}

func _goml_m_inherent_i_string_i_string_i_get(self__6 string, index__7 int32) rune {
    var retv269 rune
    var t270 rune = _goml_runtime_core_string_get(self__6, index__7)
    retv269 = t270
    return retv269
}

func println__T_bool(value__1 bool) struct{} {
    var t272 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t272)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__13 string, index__14 int32) bool {
    var retv275 bool
    var t276 bool = _goml_runtime_core_string_is_char_boundary(self__13, index__14)
    retv275 = t276
    return retv275
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__10 string, start__11 int32, end__12 int32) string {
    var retv278 string
    var t279 string = _goml_runtime_core_string_byte_slice(self__10, start__11, end__12)
    retv278 = t279
    return retv278
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__15 string, index__16 int32) _goml_m_Option_____o_char_c_int32_q_ {
    var retv281 _goml_m_Option_____o_char_c_int32_q_
    var mtmp0 Tuple3_4bool_4char_5int32 = _goml_runtime_core_string_decode_utf8_at(self__15, index__16)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var x3 int32 = mtmp0._2
    var width__19 int32 = x3
    var value__18 rune = x2
    var valid__17 bool = x1
    var jp283 _goml_m_Option_____o_char_c_int32_q_
    if valid__17 {
        var t284 Tuple2_4char_5int32 = Tuple2_4char_5int32{
            _0: value__18,
            _1: width__19,
        }
        var t285 _goml_m_Option_____o_char_c_int32_q_ = _goml_m_Option_____o_char_c_int32_q__Some{
            _0: t284,
        }
        jp283 = t285
    } else {
        jp283 = _goml_m_Option_____o_char_c_int32_q__None{}
    }
    retv281 = jp283
    return retv281
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__20 string) *_goml_vec_uint8 {
    var retv287 *_goml_vec_uint8
    var t288 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__20)
    retv287 = t288
    return retv287
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__131 *_goml_vec_uint8) int32 {
    var retv290 int32
    var t291 int32 = vec_len__Vec_5uint8(self__131)
    retv290 = t291
    return retv290
}

func println__T_uint8(value__1 uint8) struct{} {
    var t293 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t293)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__126 *_goml_vec_uint8, index__127 int32) uint8 {
    var retv296 uint8
    var t297 uint8 = vec_get__Vec_5uint8(self__126, index__127)
    retv296 = t297
    return retv296
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv299 *_goml_vec_uint8
    var t300 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv299 = t300
    return retv299
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__120 *_goml_vec_uint8, elem__121 uint8) struct{} {
    vec_push__Vec_5uint8(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__119 int32) *_goml_vec_string {
    var retv304 *_goml_vec_string
    var t305 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__119)
    retv304 = t305
    return retv304
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__120 *_goml_vec_string, elem__121 string) struct{} {
    vec_push__Vec_6string(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__132 *_goml_vec_string) int32 {
    var retv309 int32
    var t310 int32 = vec_capacity__Vec_6string(self__132)
    retv309 = t310
    return retv309
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__119 int32) *_goml_vec_int32 {
    var retv312 *_goml_vec_int32
    var t313 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__119)
    retv312 = t313
    return retv312
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__131 *_goml_vec_int32) int32 {
    var retv315 int32
    var t316 int32 = vec_len__Vec_5int32(self__131)
    retv315 = t316
    return retv315
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__132 *_goml_vec_int32) int32 {
    var retv318 int32
    var t319 int32 = vec_capacity__Vec_5int32(self__132)
    retv318 = t319
    return retv318
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__134 *_goml_vec_int32, additional__135 int32) struct{} {
    vec_reserve__Vec_5int32(self__134, additional__135)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__120 *_goml_vec_int32, elem__121 int32) struct{} {
    vec_push__Vec_5int32(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(self__152 *_goml_vec_int32, index__153 int32, value__154 int32) struct{} {
    var len__155 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__152)
    var t326 bool = index__153 == len__155
    if t326 {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__152, value__154)
        return struct{}{}
    } else {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__152, index__153)
        var t328 int32 = len__155 - 1
        var t329 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__152, t328)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__152, t329)
        var t330 int32 = len__155 - 1
        var current__156 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t330)
        Loop_loop333:
        for {
            var t334 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__156)
            var t335 bool = t334 > index__153
            if t335 {
                var t336 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__156)
                var t337 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__156)
                var t338 int32 = t337 - 1
                var t339 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__152, t338)
                _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__152, t336, t339)
                var t340 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__156)
                var t341 int32 = t340 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__156, t341)
                continue
            } else {
                break Loop_loop333
            }
        }
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__152, index__153, value__154)
        return struct{}{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__157 *_goml_vec_int32, index__158 int32) int32 {
    var retv343 int32
    var len__159 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__157)
    var value__160 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__157, index__158)
    var current__161 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(index__158)
    Loop_loop346:
    for {
        var t347 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__161)
        var t348 int32 = t347 + 1
        var t349 bool = t348 < len__159
        if t349 {
            var t350 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__161)
            var t351 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__161)
            var t352 int32 = t351 + 1
            var t353 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__157, t352)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__157, t350, t353)
            var t354 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(current__161)
            var t355 int32 = t354 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(current__161, t355)
            continue
        } else {
            break Loop_loop346
        }
    }
    var t345 int32 = len__159 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__157, t345)
    retv343 = value__160
    return retv343
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__148 *_goml_vec_int32, index__149 int32) int32 {
    var retv357 int32
    var len__150 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__148)
    var value__151 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__148, index__149)
    var t360 int32 = index__149 + 1
    var t361 bool = t360 < len__150
    if t361 {
        var t362 int32 = len__150 - 1
        var t363 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__148, t362)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__148, index__149, t363)
    } else {}
    var t359 int32 = len__150 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__148, t359)
    retv357 = value__151
    return retv357
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__162 *_goml_vec_int32) struct{} {
    var left__163 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var t365 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__162)
    var t366 int32 = t365 - 1
    var right__164 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t366)
    Loop_loop368:
    for {
        var t369 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(left__163)
        var t370 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(right__164)
        var t371 bool = t369 < t370
        if t371 {
            var t372 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(left__163)
            var t373 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(right__164)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__162, t372, t373)
            var t374 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(left__163)
            var t375 int32 = t374 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(left__163, t375)
            var t376 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(right__164)
            var t377 int32 = t376 - 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(right__164, t377)
            continue
        } else {
            break Loop_loop368
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__126 *_goml_vec_int32, index__127 int32) int32 {
    var retv379 int32
    var t380 int32 = vec_get__Vec_5int32(self__126, index__127)
    retv379 = t380
    return retv379
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__139 *_goml_vec_int32) Option__int32 {
    var retv382 Option__int32
    var len__140 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139)
    var t385 bool = len__140 == 0
    var jp384 Option__int32
    if t385 {
        jp384 = Option__int32_None{}
    } else {
        var t386 int32 = len__140 - 1
        var t387 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__139, t386)
        var t388 Option__int32 = Option__int32_Some{
            _0: t387,
        }
        jp384 = t388
    }
    retv382 = jp384
    return retv382
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__141 *_goml_vec_int32) Option__int32 {
    var retv390 Option__int32
    var len__142 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__141)
    var t393 bool = len__142 == 0
    var jp392 Option__int32
    if t393 {
        jp392 = Option__int32_None{}
    } else {
        var t394 int32 = len__142 - 1
        var value__143 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__141, t394)
        var t395 int32 = len__142 - 1
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__141, t395)
        var t396 Option__int32 = Option__int32_Some{
            _0: value__143,
        }
        jp392 = t396
    }
    retv390 = jp392
    return retv390
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__136 *_goml_vec_int32, len__137 int32) struct{} {
    vec_truncate__Vec_5int32(self__136, len__137)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__133 *_goml_vec_int32) bool {
    var retv400 bool
    var t401 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__133)
    var t402 bool = t401 == 0
    retv400 = t402
    return retv400
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__138 *_goml_vec_int32) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__138, 0)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv406 *hashmap_string_int32_x
    var t407 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv406 = t407
    return retv406
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__191 *hashmap_string_int32_x, key__192 string, value__193 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__191, key__192, value__193)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__199 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var retv411 *_goml_vec_Tuple2_6string_5int32
    var t412 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__199)
    retv411 = t412
    return retv411
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__131 *_goml_vec_Tuple2_6string_5int32) int32 {
    var retv414 int32
    var t415 int32 = vec_len__Vec_21Tuple2_6string_5int32(self__131)
    retv414 = t415
    return retv414
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__194 *hashmap_string_int32_x, key__195 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__194, key__195)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__200 bool) *ref_bool_x {
    var retv419 *ref_bool_x
    var t420 *ref_bool_x = ref__Ref_4bool(value__200)
    retv419 = t420
    return retv419
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l__o_string_c_int32_q__r__i_into__iter(self__176 *_goml_vec_Tuple2_6string_5int32) _goml_m_FnIterator_____o_string_c_int32_q_ {
    var retv422 _goml_m_FnIterator_____o_string_c_int32_q_
    var t423 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_string_c_int32_q_(self__176)
    retv422 = t423
    return retv422
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_string_c_int32_q__i_next(self__94 _goml_m_FnIterator_____o_string_c_int32_q_) _goml_m_Option_____o_string_c_int32_q_ {
    var retv425 _goml_m_Option_____o_string_c_int32_q_
    var t426 func() _goml_m_Option_____o_string_c_int32_q_ = self__94.next_fn
    var t427 _goml_m_Option_____o_string_c_int32_q_ = t426()
    retv425 = t427
    return retv425
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__202 *ref_bool_x, value__203 bool) struct{} {
    ref_set__Ref_4bool(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__201 *ref_bool_x) bool {
    var retv431 bool
    var t432 bool = ref_get__Ref_4bool(self__201)
    retv431 = t432
    return retv431
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__200 int32) *ref_int32_x {
    var retv434 *ref_int32_x
    var t435 *ref_int32_x = ref__Ref_5int32(value__200)
    retv434 = t435
    return retv434
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__201 *ref_int32_x) int32 {
    var retv437 int32
    var t438 int32 = ref_get__Ref_5int32(self__201)
    retv437 = t438
    return retv437
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__202 *ref_int32_x, value__203 int32) struct{} {
    ref_set__Ref_5int32(self__202, value__203)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__93 func() Option__char) FnIterator__char {
    var retv442 FnIterator__char
    var t443 FnIterator__char = FnIterator__char{
        next_fn: next_fn__93,
    }
    retv442 = t443
    return retv442
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__35 rune) string {
    var retv445 string
    var t446 string = _goml_runtime_core_char_to_string(self__35)
    retv445 = t446
    return retv445
}

func _goml_m_inherent_i_FnIterator__h81b975155429c11a603dd605befbfb23_int32_c_char_q_(next_fn__93 func() _goml_m_Option_____o_int32_c_char_q_) _goml_m_FnIterator_____o_int32_c_char_q_ {
    var retv448 _goml_m_FnIterator_____o_int32_c_char_q_
    var t449 _goml_m_FnIterator_____o_int32_c_char_q_ = _goml_m_FnIterator_____o_int32_c_char_q_{
        next_fn: next_fn__93,
    }
    retv448 = t449
    return retv448
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv451 string
    retv451 = self__34
    return retv451
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv453 string
    var t454 string = _goml_runtime_core_int32_to_string(self__38)
    retv453 = t454
    return retv453
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv456 string
    var t457 string = _goml_runtime_core_bool_to_string(self__33)
    retv456 = t457
    return retv456
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__40 uint8) string {
    var retv459 string
    var t460 string = _goml_runtime_core_uint8_to_string(self__40)
    retv459 = t460
    return retv459
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__128 *_goml_vec_int32, index__129 int32, elem__130 int32) struct{} {
    vec_set__Vec_5int32(self__128, index__129, elem__130)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__144 *_goml_vec_int32, left__145 int32, right__146 int32) struct{} {
    var value__147 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__144, left__145)
    var t464 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__144, right__146)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__144, left__145, t464)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_set____T__int32(self__144, right__146, value__147)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T___o_string_c_int32_q_(self__171 *_goml_vec_Tuple2_6string_5int32) _goml_m_FnIterator_____o_string_c_int32_q_ {
    var retv467 _goml_m_FnIterator_____o_string_c_int32_q_
    var index__172 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var len__173 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__171)
    var t468 closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2 = closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2{
        index_0: index__172,
        len_1: len__173,
        self_2: self__171,
    }
    var t469 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_inherent_i_FnIterator__h4aab6f157cede91513543003d0ae8727_ring_c_int32_q_(func() _goml_m_Option_____o_string_c_int32_q_ {
        return _goml_m_inherent_i_closure__en_hb124241ac92f068ce7e0775da15a1ab4_nt32__2_i_apply(t468)
    })
    retv467 = t469
    return retv467
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_string_c_int32_q_(self__126 *_goml_vec_Tuple2_6string_5int32, index__127 int32) Tuple2_6string_5int32 {
    var retv471 Tuple2_6string_5int32
    var t472 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(self__126, index__127)
    retv471 = t472
    return retv471
}

func _goml_m_inherent_i_FnIterator__h4aab6f157cede91513543003d0ae8727_ring_c_int32_q_(next_fn__93 func() _goml_m_Option_____o_string_c_int32_q_) _goml_m_FnIterator_____o_string_c_int32_q_ {
    var retv474 _goml_m_FnIterator_____o_string_c_int32_q_
    var t475 _goml_m_FnIterator_____o_string_c_int32_q_ = _goml_m_FnIterator_____o_string_c_int32_q_{
        next_fn: next_fn__93,
    }
    retv474 = t475
    return retv474
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__50 string, other__51 string) bool {
    var retv477 bool
    var t478 bool = self__50 == other__51
    retv477 = t478
    return retv477
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__76 string) uint64 {
    var retv480 uint64
    var t481 uint64 = _goml_runtime_core_string_hash(self__76)
    retv480 = t481
    return retv480
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env147 closure_env_inherent_string_string_chars_0) Option__char {
    var retv496 Option__char
    var self__21 string = env147.self_0
    var index__22 *ref_int32_x = env147.index_1
    var t497 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__22)
    var mtmp4 _goml_m_Option_____o_char_c_int32_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__21, t497)
    var jp499 Option__char
    switch mtmp4.(type) {
    case _goml_m_Option_____o_char_c_int32_q__None:
        jp499 = Option__char_None{}
    case _goml_m_Option_____o_char_c_int32_q__Some:
        var x5 Tuple2_4char_5int32 = mtmp4.(_goml_m_Option_____o_char_c_int32_q__Some)._0
        var decoded__23 Tuple2_4char_5int32 = x5
        var mtmp6 Tuple2_4char_5int32 = decoded__23
        var x7 rune = mtmp6._0
        var x8 int32 = mtmp6._1
        var width__25 int32 = x8
        var value__24 rune = x7
        var t500 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__22)
        var t501 int32 = t500 + width__25
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__22, t501)
        var t502 Option__char = Option__char_Some{
            _0: value__24,
        }
        jp499 = t502
    default:
        panic("non-exhaustive match")
    }
    retv496 = jp499
    return retv496
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env148 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int32_c_char_q_ {
    var retv504 _goml_m_Option_____o_int32_c_char_q_
    var index__27 *ref_int32_x = env148.index_0
    var self__26 string = env148.self_1
    var current__28 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__27)
    var mtmp10 _goml_m_Option_____o_char_c_int32_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__26, current__28)
    var jp506 _goml_m_Option_____o_int32_c_char_q_
    switch mtmp10.(type) {
    case _goml_m_Option_____o_char_c_int32_q__None:
        jp506 = _goml_m_Option_____o_int32_c_char_q__None{}
    case _goml_m_Option_____o_char_c_int32_q__Some:
        var x11 Tuple2_4char_5int32 = mtmp10.(_goml_m_Option_____o_char_c_int32_q__Some)._0
        var decoded__29 Tuple2_4char_5int32 = x11
        var mtmp12 Tuple2_4char_5int32 = decoded__29
        var x13 rune = mtmp12._0
        var x14 int32 = mtmp12._1
        var width__31 int32 = x14
        var value__30 rune = x13
        var t507 int32 = current__28 + width__31
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__27, t507)
        var t508 Tuple2_5int32_4char = Tuple2_5int32_4char{
            _0: current__28,
            _1: value__30,
        }
        var t509 _goml_m_Option_____o_int32_c_char_q_ = _goml_m_Option_____o_int32_c_char_q__Some{
            _0: t508,
        }
        jp506 = t509
    default:
        panic("non-exhaustive match")
    }
    retv504 = jp506
    return retv504
}

func _goml_m_inherent_i_closure__en_hb124241ac92f068ce7e0775da15a1ab4_nt32__2_i_apply(env149 closure_env_inherent_Vec_Vec_T_iter_T_string_int32_2) _goml_m_Option_____o_string_c_int32_q_ {
    var retv511 _goml_m_Option_____o_string_c_int32_q_
    var index__172 *ref_int32_x = env149.index_0
    var len__173 int32 = env149.len_1
    var self__171 *_goml_vec_Tuple2_6string_5int32 = env149.self_2
    var current__174 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__172)
    var t514 bool = current__174 < len__173
    var jp513 _goml_m_Option_____o_string_c_int32_q_
    if t514 {
        var value__175 Tuple2_6string_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T___o_string_c_int32_q_(self__171, current__174)
        var t515 int32 = current__174 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__172, t515)
        var t516 _goml_m_Option_____o_string_c_int32_q_ = _goml_m_Option_____o_string_c_int32_q__Some{
            _0: value__175,
        }
        jp513 = t516
    } else {
        jp513 = _goml_m_Option_____o_string_c_int32_q__None{}
    }
    retv511 = jp513
    return retv511
}

func main() {
    main0()
}
