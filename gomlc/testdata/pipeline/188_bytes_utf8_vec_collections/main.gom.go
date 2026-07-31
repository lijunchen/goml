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
    var t246 FnIterator__char = _goml_m_inherent_i_string_i_string_i_chars(value__0)
    var for_iter152 FnIterator__char = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(t246)
    Loop_loop248:
    for {
        if true {
            var for_next153 Option__char = _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(for_iter152)
            switch for_next153.(type) {
            case Option__char_None:
                break Loop_loop248
            case Option__char_Some:
                var x154 rune = for_next153.(Option__char_Some)._0
                var character__1 rune = x154
                println__T_char(character__1)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop248
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t252 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_string_i_string_i_char__indices(value__2)
    var for_iter155 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(t252)
    Loop_loop254:
    for {
        if true {
            var for_next156 _goml_m_Option_____o_int_c_char_q_ = _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(for_iter155)
            switch for_next156.(type) {
            case _goml_m_Option_____o_int_c_char_q__None:
                break Loop_loop254
            case _goml_m_Option_____o_int_c_char_q__Some:
                var x157 Tuple2_3int_4char = for_next156.(_goml_m_Option_____o_int_c_char_q__Some)._0
                var item__3 Tuple2_3int_4char = x157
                var mtmp158 Tuple2_3int_4char = item__3
                var x159 int = mtmp158._0
                var x160 rune = mtmp158._1
                var character__5 rune = x160
                var index__4 int = x159
                var t256 string = _goml_m_inherent_i_int_i_int_i_to__string(index__4)
                var t257 string = t256 + ":"
                var t258 string = _goml_m_inherent_i_char_i_char_i_to__string(character__5)
                var t259 string = t257 + t258
                println__T_string(t259)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop254
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t262 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t262)
    var t263 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t263)
    var t264 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t264)
    var t265 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t265)
    var t266 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t266)
    var t267 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t267)
    var t268 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t268)
    var t269 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t269)
    var t270 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t270)
    var t271 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t271)
    var t272 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t272)
    var t273 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t273)
    var mtmp173 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp173.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        println__T_string("missing")
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x174 Tuple2_4char_3int = mtmp173.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__7 Tuple2_4char_3int = x174
        var mtmp175 Tuple2_4char_3int = decoded__7
        var x176 rune = mtmp175._0
        var x177 int = mtmp175._1
        var width__9 int = x177
        var character__8 rune = x176
        println__T_char(character__8)
        println__T_int(width__9)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t275 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t275)
    var t276 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t276)
    var t277 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t277)
    var mtmp185 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__10)
    var x186 bool = mtmp185._0
    var x187 string = mtmp185._1
    var roundtrip__12 string = x187
    var roundtrip_valid__11 bool = x186
    println__T_bool(roundtrip_valid__11)
    println__T_string(roundtrip__12)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp192 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(invalid__13)
    var x193 bool = mtmp192._0
    var x194 string = mtmp192._1
    var invalid_text__15 string = x194
    var invalid_valid__14 bool = x193
    println__T_bool(invalid_valid__14)
    var t278 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(invalid_text__15, "")
    println__T_bool(t278)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t279 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t280 bool = t279 >= 3
    println__T_bool(t280)
    var t281 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t281)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t282 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t282)
    var t283 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t284 bool = t283 >= 1
    println__T_bool(t284)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t285 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t286 bool = t285 >= 100
    println__T_bool(t286)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t287 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t287)
    var t288 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t288)
    var t289 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t289)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t290 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t290)
    var t291 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t291)
    var mtmp216 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp216.(type) {
    case Option__int32_None:
        println__T_int(-1)
    case Option__int32_Some:
        var x217 int32 = mtmp216.(Option__int32_Some)._0
        var value__18 int32 = x217
        println__T_int32(value__18)
    default:
        panic("non-exhaustive match")
    }
    var mtmp219 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp219.(type) {
    case Option__int32_None:
        println__T_int(-1)
    case Option__int32_Some:
        var x220 int32 = mtmp219.(Option__int32_Some)._0
        var value__19 int32 = x220
        println__T_int32(value__19)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t294 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t294)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t295 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t295)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t296 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t296)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t297 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t297)
    var seen_a__22 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var seen_b__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var for_source233 *_goml_vec_Tuple2_6string_5int32 = entries__21
    var for_limit234 int = vec_len__Vec_21Tuple2_6string_5int32(for_source233)
    var for_index235 int = 0
    Loop_loop302:
    for {
        var t303 bool = for_index235 < for_limit234
        if t303 {
            var for_item236 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(for_source233, for_index235)
            var t304 int = for_index235 + 1
            for_index235 = t304
            var entry__24 Tuple2_6string_5int32 = for_item236
            var mtmp238 Tuple2_6string_5int32 = entry__24
            var x239 string = mtmp238._0
            var x240 int32 = mtmp238._1
            var item_value__26 int32 = x240
            var key__25 string = x239
            var t315 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "a")
            var jp307 bool
            if t315 {
                var t316 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 1)
                jp307 = t316
            } else {
                jp307 = false
            }
            if jp307 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_a__22, true)
            } else {
                var t313 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(key__25, "b")
                var jp311 bool
                if t313 {
                    var t314 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(item_value__26, 2)
                    jp311 = t314
                } else {
                    jp311 = false
                }
                if jp311 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(seen_b__23, true)
                } else {}
            }
            continue
        } else {
            break Loop_loop302
        }
    }
    var t299 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_a__22)
    println__T_bool(t299)
    var t300 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(seen_b__23)
    println__T_bool(t300)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_chars(self__25 string) FnIterator__char {
    var retv324 FnIterator__char
    var index__26 *ref_int_x = ref__Ref_3int(0)
    var t325 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: self__25,
        index_1: index__26,
    }
    var t326 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(t325)
    })
    retv324 = t326
    return retv324
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____char_i_into__iter(self__109 FnIterator__char) FnIterator__char {
    var retv328 FnIterator__char
    retv328 = self__109
    return retv328
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____char_i_next(self__102 FnIterator__char) Option__char {
    var retv330 Option__char
    var t331 func() Option__char = self__102.next_fn
    var t332 Option__char = t331()
    retv330 = t332
    return retv330
}

func println__T_char(value__1 rune) struct{} {
    var t334 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(value__1)
    _goml_runtime_core_string_println(t334)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_char__indices(self__30 string) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv337 _goml_m_FnIterator_____o_int_c_char_q_
    var index__31 *ref_int_x = ref__Ref_3int(0)
    var t338 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: index__31,
        self_1: self__30,
    }
    var t339 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(t338)
    })
    retv337 = t339
    return retv337
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator_____o_int_c_char_q__i_into__iter(self__109 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv341 _goml_m_FnIterator_____o_int_c_char_q_
    retv341 = self__109
    return retv341
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator_____o_int_c_char_q__i_next(self__102 _goml_m_FnIterator_____o_int_c_char_q_) _goml_m_Option_____o_int_c_char_q_ {
    var retv343 _goml_m_Option_____o_int_c_char_q_
    var t344 func() _goml_m_Option_____o_int_c_char_q_ = self__102.next_fn
    var t345 _goml_m_Option_____o_int_c_char_q_ = t344()
    retv343 = t345
    return retv343
}

func println__T_string(value__1 string) struct{} {
    var t347 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t347)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv350 string
    var t351 string = _goml_runtime_core_int_to_string(self__5)
    retv350 = t351
    return retv350
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv353 string
    var t354 string = _goml_runtime_core_char_to_string(self__7)
    retv353 = t354
    return retv353
}

func println__T_int(value__1 int) struct{} {
    var t356 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t356)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv359 int
    var t360 int = _goml_runtime_core_string_len(self__9)
    retv359 = t360
    return retv359
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv362 int
    var t363 int = _goml_runtime_core_string_len(self__8)
    retv362 = t363
    return retv362
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv365 rune
    var t366 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv365 = t366
    return retv365
}

func println__T_bool(value__1 bool) struct{} {
    var t368 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t368)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__17 string, index__18 int) bool {
    var retv371 bool
    var t372 bool = _goml_runtime_core_string_is_char_boundary(self__17, index__18)
    retv371 = t372
    return retv371
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv374 string
    var t375 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv374 = t375
    return retv374
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__19 string, index__20 int) _goml_m_Option_____o_char_c_int_q_ {
    var retv377 _goml_m_Option_____o_char_c_int_q_
    var mtmp3 Tuple3_4bool_4char_3int = _goml_runtime_core_string_decode_utf8_at(self__19, index__20)
    var x4 bool = mtmp3._0
    var x5 rune = mtmp3._1
    var x6 int = mtmp3._2
    var width__23 int = x6
    var value__22 rune = x5
    var valid__21 bool = x4
    var jp379 _goml_m_Option_____o_char_c_int_q_
    if valid__21 {
        var t380 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: value__22,
            _1: width__23,
        }
        var t381 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t380,
        }
        jp379 = t381
    } else {
        jp379 = _goml_m_Option_____o_char_c_int_q__None{}
    }
    retv377 = jp379
    return retv377
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var retv383 *_goml_vec_uint8
    var t384 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    retv383 = t384
    return retv383
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__137 *_goml_vec_uint8) int {
    var retv386 int
    var t387 int = vec_len__Vec_5uint8(self__137)
    retv386 = t387
    return retv386
}

func println__T_uint8(value__1 uint8) struct{} {
    var t389 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(value__1)
    _goml_runtime_core_string_println(t389)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__132 *_goml_vec_uint8, index__133 int) uint8 {
    var retv392 uint8
    var t393 uint8 = vec_get__Vec_5uint8(self__132, index__133)
    retv392 = t393
    return retv392
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv395 *_goml_vec_uint8
    var t396 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv395 = t396
    return retv395
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__126 *_goml_vec_uint8, elem__127 uint8) struct{} {
    vec_push__Vec_5uint8(self__126, elem__127)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv400 bool
    var t401 bool = self__55 == other__56
    retv400 = t401
    return retv400
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__125 int) *_goml_vec_string {
    var retv403 *_goml_vec_string
    var t404 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__125)
    retv403 = t404
    return retv403
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__126 *_goml_vec_string, elem__127 string) struct{} {
    vec_push__Vec_6string(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__138 *_goml_vec_string) int {
    var retv408 int
    var t409 int = vec_capacity__Vec_6string(self__138)
    retv408 = t409
    return retv408
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__125 int) *_goml_vec_int32 {
    var retv411 *_goml_vec_int32
    var t412 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__125)
    retv411 = t412
    return retv411
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv414 int
    var t415 int = vec_len__Vec_5int32(self__137)
    retv414 = t415
    return retv414
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__138 *_goml_vec_int32) int {
    var retv417 int
    var t418 int = vec_capacity__Vec_5int32(self__138)
    retv417 = t418
    return retv417
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
    var t425 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(index__159, len__161)
    if t425 {
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__158, value__160)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__158, index__159)
        var t427 int = len__161 - 1
        var t428 int32 = vec_get__Vec_5int32(self__158, t427)
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__158, t428)
        var current__162 int = len__161 - 1
        Loop_loop431:
        for {
            var t432 bool = current__162 > index__159
            if t432 {
                var place_root63 *_goml_vec_int32 = self__158
                var index64 int = current__162
                vec_get__Vec_5int32(place_root63, index64)
                var t433 int = current__162 - 1
                var value66 int32 = vec_get__Vec_5int32(self__158, t433)
                vec_set__Vec_5int32(place_root63, index64, value66)
                var compound_old68 int = current__162
                var compound_value69 int = 1
                var t435 int = compound_old68 - compound_value69
                current__162 = t435
                continue
            } else {
                break Loop_loop431
            }
        }
        var place_root72 *_goml_vec_int32 = self__158
        var index73 int = index__159
        vec_get__Vec_5int32(place_root72, index73)
        var value75 int32 = value__160
        vec_set__Vec_5int32(place_root72, index73, value75)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t438 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t438)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__163 *_goml_vec_int32, index__164 int) int32 {
    var retv441 int32
    var len__165 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__163)
    var value__166 int32 = vec_get__Vec_5int32(self__163, index__164)
    var current__167 int = index__164
    Loop_loop444:
    for {
        var t445 int = current__167 + 1
        var t446 bool = t445 < len__165
        if t446 {
            var place_root77 *_goml_vec_int32 = self__163
            var index78 int = current__167
            vec_get__Vec_5int32(place_root77, index78)
            var t447 int = current__167 + 1
            var value80 int32 = vec_get__Vec_5int32(self__163, t447)
            vec_set__Vec_5int32(place_root77, index78, value80)
            var compound_old82 int = current__167
            var compound_value83 int = 1
            var t449 int = compound_old82 + compound_value83
            current__167 = t449
            continue
        } else {
            break Loop_loop444
        }
    }
    var t443 int = len__165 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__163, t443)
    retv441 = value__166
    return retv441
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__154 *_goml_vec_int32, index__155 int) int32 {
    var retv452 int32
    var len__156 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__154)
    var value__157 int32 = vec_get__Vec_5int32(self__154, index__155)
    var t455 int = index__155 + 1
    var t456 bool = t455 < len__156
    if t456 {
        var place_root54 *_goml_vec_int32 = self__154
        var index55 int = index__155
        vec_get__Vec_5int32(place_root54, index55)
        var t457 int = len__156 - 1
        var value57 int32 = vec_get__Vec_5int32(self__154, t457)
        vec_set__Vec_5int32(place_root54, index55, value57)
    } else {}
    var t454 int = len__156 - 1
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__154, t454)
    retv452 = value__157
    return retv452
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__168 *_goml_vec_int32) struct{} {
    var left__169 int = 0
    var t460 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__168)
    var right__170 int = t460 - 1
    Loop_loop462:
    for {
        var t463 bool = left__169 < right__170
        if t463 {
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__168, left__169, right__170)
            var compound_old88 int = left__169
            var compound_value89 int = 1
            var t464 int = compound_old88 + compound_value89
            left__169 = t464
            var compound_old91 int = right__170
            var compound_value92 int = 1
            var t466 int = compound_old91 - compound_value92
            right__170 = t466
            continue
        } else {
            break Loop_loop462
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__132 *_goml_vec_int32, index__133 int) int32 {
    var retv469 int32
    var t470 int32 = vec_get__Vec_5int32(self__132, index__133)
    retv469 = t470
    return retv469
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__145 *_goml_vec_int32) Option__int32 {
    var retv472 Option__int32
    var len__146 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__145)
    var t475 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__146, 0)
    var jp474 Option__int32
    if t475 {
        jp474 = Option__int32_None{}
    } else {
        var t476 int = len__146 - 1
        var t477 int32 = vec_get__Vec_5int32(self__145, t476)
        var t478 Option__int32 = Option__int32_Some{
            _0: t477,
        }
        jp474 = t478
    }
    retv472 = jp474
    return retv472
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__147 *_goml_vec_int32) Option__int32 {
    var retv480 Option__int32
    var len__148 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__147)
    var t483 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(len__148, 0)
    var jp482 Option__int32
    if t483 {
        jp482 = Option__int32_None{}
    } else {
        var t484 int = len__148 - 1
        var value__149 int32 = vec_get__Vec_5int32(self__147, t484)
        var t485 int = len__148 - 1
        _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__147, t485)
        var t486 Option__int32 = Option__int32_Some{
            _0: value__149,
        }
        jp482 = t486
    }
    retv480 = jp482
    return retv480
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__142 *_goml_vec_int32, len__143 int) struct{} {
    vec_truncate__Vec_5int32(self__142, len__143)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__139 *_goml_vec_int32) bool {
    var retv490 bool
    var t491 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139)
    var t492 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t491, 0)
    retv490 = t492
    return retv490
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__144 *_goml_vec_int32) struct{} {
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__144, 0)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var retv496 *hashmap_string_int32_x
    var t497 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    retv496 = t497
    return retv496
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__198 *hashmap_string_int32_x, key__199 string, value__200 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__198, key__199, value__200)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__206 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var retv501 *_goml_vec_Tuple2_6string_5int32
    var t502 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__206)
    retv501 = t502
    return retv501
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__137 *_goml_vec_Tuple2_6string_5int32) int {
    var retv504 int
    var t505 int = vec_len__Vec_21Tuple2_6string_5int32(self__137)
    retv504 = t505
    return retv504
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__201 *hashmap_string_int32_x, key__202 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__201, key__202)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var retv509 *ref_bool_x
    var t510 *ref_bool_x = ref__Ref_4bool(value__207)
    retv509 = t510
    return retv509
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv512 bool
    var t513 bool = self__65 == other__66
    retv512 = t513
    return retv512
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__209 *ref_bool_x, value__210 bool) struct{} {
    ref_set__Ref_4bool(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__208 *ref_bool_x) bool {
    var retv517 bool
    var t518 bool = ref_get__Ref_4bool(self__208)
    retv517 = t518
    return retv517
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__101 func() Option__char) FnIterator__char {
    var retv520 FnIterator__char
    var t521 FnIterator__char = FnIterator__char{
        next_fn: next_fn__101,
    }
    retv520 = t521
    return retv520
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__39 rune) string {
    var retv523 string
    var t524 string = _goml_runtime_core_char_to_string(self__39)
    retv523 = t524
    return retv523
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__101 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var retv526 _goml_m_FnIterator_____o_int_c_char_q_
    var t527 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__101,
    }
    retv526 = t527
    return retv526
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv529 string
    retv529 = self__38
    return retv529
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv531 string
    var t532 string = _goml_runtime_core_int_to_string(self__40)
    retv531 = t532
    return retv531
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv534 string
    var t535 string = _goml_runtime_core_bool_to_string(self__37)
    retv534 = t535
    return retv534
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__45 uint8) string {
    var retv537 string
    var t538 string = _goml_runtime_core_uint8_to_string(self__45)
    retv537 = t538
    return retv537
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv540 bool
    var t541 bool = self__59 == other__60
    retv540 = t541
    return retv540
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv543 string
    var t544 string = _goml_runtime_core_int32_to_string(self__43)
    retv543 = t544
    return retv543
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap____T__int32(self__150 *_goml_vec_int32, left__151 int, right__152 int) struct{} {
    var value__153 int32 = vec_get__Vec_5int32(self__150, left__151)
    var place_root44 *_goml_vec_int32 = self__150
    var index45 int = left__151
    vec_get__Vec_5int32(place_root44, index45)
    var value47 int32 = vec_get__Vec_5int32(self__150, right__152)
    vec_set__Vec_5int32(place_root44, index45, value47)
    var place_root49 *_goml_vec_int32 = self__150
    var index50 int = right__152
    vec_get__Vec_5int32(place_root49, index50)
    var value52 int32 = value__153
    vec_set__Vec_5int32(place_root49, index50, value52)
    return struct{}{}
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__83 string) uint64 {
    var retv549 uint64
    var t550 uint64 = _goml_runtime_core_string_hash(self__83)
    retv549 = t550
    return retv549
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env243 closure_env_inherent_string_string_chars_0) Option__char {
    var retv562 Option__char
    var self__25 string = env243.self_0
    var index__26 *ref_int_x = env243.index_1
    var t563 int = ref_get__Ref_3int(index__26)
    var mtmp7 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__25, t563)
    var jp565 Option__char
    switch mtmp7.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        jp565 = Option__char_None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x8 Tuple2_4char_3int = mtmp7.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__27 Tuple2_4char_3int = x8
        var mtmp9 Tuple2_4char_3int = decoded__27
        var x10 rune = mtmp9._0
        var x11 int = mtmp9._1
        var width__29 int = x11
        var value__28 rune = x10
        var compound_old12 int = ref_get__Ref_3int(index__26)
        var compound_value13 int = width__29
        var t566 int = compound_old12 + compound_value13
        ref_set__Ref_3int(index__26, t566)
        var t568 Option__char = Option__char_Some{
            _0: value__28,
        }
        jp565 = t568
    default:
        panic("non-exhaustive match")
    }
    retv562 = jp565
    return retv562
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env244 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var retv570 _goml_m_Option_____o_int_c_char_q_
    var index__31 *ref_int_x = env244.index_0
    var self__30 string = env244.self_1
    var current__32 int = ref_get__Ref_3int(index__31)
    var mtmp15 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(self__30, current__32)
    var jp572 _goml_m_Option_____o_int_c_char_q_
    switch mtmp15.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        jp572 = _goml_m_Option_____o_int_c_char_q__None{}
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x16 Tuple2_4char_3int = mtmp15.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var decoded__33 Tuple2_4char_3int = x16
        var mtmp17 Tuple2_4char_3int = decoded__33
        var x18 rune = mtmp17._0
        var x19 int = mtmp17._1
        var width__35 int = x19
        var value__34 rune = x18
        var t573 int = current__32 + width__35
        ref_set__Ref_3int(index__31, t573)
        var t574 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__32,
            _1: value__34,
        }
        var t575 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t574,
        }
        jp572 = t575
    default:
        panic("non-exhaustive match")
    }
    retv570 = jp572
    return retv570
}

func main() {
    main0()
}
