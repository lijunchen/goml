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
    var t230 FnIterator__char
    var inline778 *ref_int_x = ref__Ref_3int(0)
    var inline779 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: value__0,
        index_1: inline778,
    }
    var inline780 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(inline779)
    })
    t230 = inline780
    var for_iter136 FnIterator__char
    for_iter136 = t230
    Loop_loop232:
    for {
        var for_next137 Option__char
        var inline774 func() Option__char = for_iter136.next_fn
        var inline775 Option__char = inline774()
        for_next137 = inline775
        switch for_next137.(type) {
        case Option__char_None:
            break Loop_loop232
        case Option__char_Some:
            var x138 rune = for_next137.(Option__char_Some)._0
            var inline771 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x138)
            _goml_runtime_core_string_println(inline771)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t236 _goml_m_FnIterator_____o_int_c_char_q_
    var inline793 *ref_int_x = ref__Ref_3int(0)
    var inline794 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: inline793,
        self_1: value__2,
    }
    var inline795 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(inline794)
    })
    t236 = inline795
    var for_iter139 _goml_m_FnIterator_____o_int_c_char_q_
    for_iter139 = t236
    Loop_loop238:
    for {
        var for_next140 _goml_m_Option_____o_int_c_char_q_
        var inline789 func() _goml_m_Option_____o_int_c_char_q_ = for_iter139.next_fn
        var inline790 _goml_m_Option_____o_int_c_char_q_ = inline789()
        for_next140 = inline790
        switch for_next140.(type) {
        case _goml_m_Option_____o_int_c_char_q__None:
            break Loop_loop238
        case _goml_m_Option_____o_int_c_char_q__Some:
            var x141 Tuple2_3int_4char = for_next140.(_goml_m_Option_____o_int_c_char_q__Some)._0
            var x143 int = x141._0
            var x144 rune = x141._1
            var t240 string
            var inline787 string = _goml_runtime_core_int_to_string(x143)
            t240 = inline787
            var t241 string = t240 + ":"
            var t242 string
            var inline785 string = char_to_string(x144)
            t242 = inline785
            var t243 string = t241 + t242
            var inline782 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t243)
            _goml_runtime_core_string_println(inline782)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t246 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t246)
    var t247 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t247)
    var t248 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t248)
    var t249 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t249)
    var t250 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t250)
    var t251 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t251)
    var t252 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t252)
    var t253 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t253)
    var t254 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t254)
    var t255 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t255)
    var t256 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t256)
    var t257 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t257)
    var mtmp157 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp157.(type) {
    case _goml_m_Option_____o_char_c_int_q__None:
        var inline797 string = "missing"
        var inline798 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline797)
        _goml_runtime_core_string_println(inline798)
    case _goml_m_Option_____o_char_c_int_q__Some:
        var x158 Tuple2_4char_3int = mtmp157.(_goml_m_Option_____o_char_c_int_q__Some)._0
        var x160 rune = x158._0
        var x161 int = x158._1
        var inline804 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x160)
        _goml_runtime_core_string_println(inline804)
        var inline801 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x161)
        _goml_runtime_core_string_println(inline801)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t259 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t259)
    var t260 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t260)
    var t261 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t261)
    var mtmp169 Tuple2_4bool_6string = string_from_utf8(bytes__10)
    var x170 bool = mtmp169._0
    var x171 string = mtmp169._1
    println__T_bool(x170)
    println__T_string(x171)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp176 Tuple2_4bool_6string = string_from_utf8(invalid__13)
    var x177 bool = mtmp176._0
    var x178 string = mtmp176._1
    println__T_bool(x177)
    var t262 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(x178, "")
    println__T_bool(t262)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t263 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t264 bool = t263 >= 3
    println__T_bool(t264)
    var t265 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t265)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t266 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t266)
    var t267 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t268 bool = t267 >= 1
    println__T_bool(t268)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t269 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t270 bool = t269 >= 100
    println__T_bool(t270)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t271 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t271)
    var t272 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t272)
    var t273 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t273)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t274 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t274)
    var t275 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t275)
    var mtmp200 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp200.(type) {
    case Option__int32_None:
        var inline807 int = -1
        var inline808 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline807)
        _goml_runtime_core_string_println(inline808)
    case Option__int32_Some:
        var x201 int32 = mtmp200.(Option__int32_Some)._0
        var inline811 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x201)
        _goml_runtime_core_string_println(inline811)
    default:
        panic("non-exhaustive match")
    }
    var mtmp203 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp203.(type) {
    case Option__int32_None:
        var inline814 int = -1
        var inline815 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline814)
        _goml_runtime_core_string_println(inline815)
    case Option__int32_Some:
        var x204 int32 = mtmp203.(Option__int32_Some)._0
        var inline818 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x204)
        _goml_runtime_core_string_println(inline818)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t278 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t278)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t279 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t279)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t280 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t280)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "c", 3)
    _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(map__20, "a")
    var t281 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t281)
    var seen_a__22 *ref_bool_x
    var inline852 bool = false
    var inline853 *ref_bool_x = ref__Ref_4bool(inline852)
    seen_a__22 = inline853
    var seen_b__23 *ref_bool_x
    var inline849 bool = false
    var inline850 *ref_bool_x = ref__Ref_4bool(inline849)
    seen_b__23 = inline850
    var for_limit218 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    var for_index219 int = 0
    Loop_loop286:
    for {
        var t287 bool = for_index219 < for_limit218
        if t287 {
            var for_item220 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(entries__21, for_index219)
            var t288 int = for_index219 + 1
            for_index219 = t288
            var x223 string = for_item220._0
            var x224 int32 = for_item220._1
            var t299 bool
            var inline836 string = "a"
            var inline837 bool = x223 == inline836
            t299 = inline837
            var jp291 bool
            if t299 {
                var inline821 int32 = 1
                var inline822 bool = x224 == inline821
                jp291 = inline822
            } else {
                jp291 = false
            }
            if jp291 {
                var inline824 bool = true
                ref_set__Ref_4bool(seen_a__22, inline824)
                continue
            } else {
                var t297 bool
                var inline833 string = "b"
                var inline834 bool = x223 == inline833
                t297 = inline834
                var jp295 bool
                if t297 {
                    var inline827 int32 = 2
                    var inline828 bool = x224 == inline827
                    jp295 = inline828
                } else {
                    jp295 = false
                }
                if jp295 {
                    var inline830 bool = true
                    ref_set__Ref_4bool(seen_b__23, inline830)
                    continue
                } else {
                    continue
                }
            }
        } else {
            break Loop_loop286
        }
    }
    var t283 bool
    var inline847 bool = ref_get__Ref_4bool(seen_a__22)
    t283 = inline847
    var inline844 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t283)
    _goml_runtime_core_string_println(inline844)
    var t284 bool
    var inline842 bool = ref_get__Ref_4bool(seen_b__23)
    t284 = inline842
    var inline839 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t284)
    _goml_runtime_core_string_println(inline839)
    return struct{}{}
}

func println__T_char(value__31 rune) struct{} {
    var t318 string
    var inline855 string = char_to_string(value__31)
    t318 = inline855
    _goml_runtime_core_string_println(t318)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t331 string
    t331 = value__31
    _goml_runtime_core_string_println(t331)
    return struct{}{}
}

func println__T_int(value__31 int) struct{} {
    var t340 string
    var inline863 string = _goml_runtime_core_int_to_string(value__31)
    t340 = inline863
    _goml_runtime_core_string_println(t340)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t344 int = _goml_runtime_core_string_len(self__38)
    return t344
}

func _goml_m_inherent_i_string_i_string_i_len(self__37 string) int {
    var t347 int = _goml_runtime_core_string_len(self__37)
    return t347
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline865 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline866 bool = inline865._0
    var inline867 rune = inline865._1
    if inline866 {
        return inline867
    } else {
        var inline871 rune = _goml_runtime_core_string_get("", -1)
        return inline871
    }
}

func println__T_bool(value__31 bool) struct{} {
    var t352 string
    var inline873 string = _goml_runtime_core_bool_to_string(value__31)
    t352 = inline873
    _goml_runtime_core_string_println(t352)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__46 string, index__47 int) bool {
    var t356 bool = string_is_char_boundary(self__46, index__47)
    return t356
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline875 bool = string_is_char_boundary(self__43, start__44)
    var inline877 bool
    if inline875 {
        var inline880 bool = string_is_char_boundary(self__43, end__45)
        inline877 = inline880
    } else {
        inline877 = false
    }
    if inline877 {
        var inline878 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline878
    } else {
        var inline879 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline879
    }
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__48 string, index__49 int) _goml_m_Option_____o_char_c_int_q_ {
    var mtmp25 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__48, index__49)
    var x26 bool = mtmp25._0
    var x27 rune = mtmp25._1
    var x28 int = mtmp25._2
    if x26 {
        var t364 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: x27,
            _1: x28,
        }
        var t365 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q__Some{
            _0: t364,
        }
        return t365
    } else {
        return _goml_m_Option_____o_char_c_int_q__None{}
    }
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__53 string) *_goml_vec_uint8 {
    var t368 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__53)
    return t368
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__162 *_goml_vec_uint8) int {
    var t371 int = vec_len__Vec_5uint8(self__162)
    return t371
}

func println__T_uint8(value__31 uint8) struct{} {
    var t373 string
    var inline882 string = _goml_runtime_core_uint8_to_string(value__31)
    t373 = inline882
    _goml_runtime_core_string_println(t373)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__157 *_goml_vec_uint8, index__158 int) uint8 {
    var t377 uint8 = vec_get__Vec_5uint8(self__157, index__158)
    return t377
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop382:
    for {
        var t383 int
        var inline884 int = _goml_runtime_core_string_len(x12)
        t383 = inline884
        var t384 bool = index__26 < t383
        if t384 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t386 int = compound_old17 + x16
                index__26 = t386
                continue
            } else {
                var t388 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t388
            }
        } else {
            break Loop_loop382
        }
    }
    var t381 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t381
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t391 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t391
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__151 *_goml_vec_uint8, elem__152 uint8) struct{} {
    vec_push__Vec_5uint8(self__151, elem__152)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__99 string, other__100 string) bool {
    var t396 bool = self__99 == other__100
    return t396
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__150 int) *_goml_vec_string {
    var t399 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__150)
    return t399
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__151 *_goml_vec_string, elem__152 string) struct{} {
    vec_push__Vec_6string(self__151, elem__152)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__163 *_goml_vec_string) int {
    var t404 int = vec_capacity__Vec_6string(self__163)
    return t404
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__150 int) *_goml_vec_int32 {
    var t407 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__150)
    return t407
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__162 *_goml_vec_int32) int {
    var t410 int = vec_len__Vec_5int32(self__162)
    return t410
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__163 *_goml_vec_int32) int {
    var t413 int = vec_capacity__Vec_5int32(self__163)
    return t413
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__165 *_goml_vec_int32, additional__166 int) struct{} {
    vec_reserve__Vec_5int32(self__165, additional__166)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__151 *_goml_vec_int32, elem__152 int32) struct{} {
    vec_push__Vec_5int32(self__151, elem__152)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(self__183 *_goml_vec_int32, index__184 int, value__185 int32) struct{} {
    var len__186 int
    var inline892 int = vec_len__Vec_5int32(self__183)
    len__186 = inline892
    var t420 bool
    var inline890 bool = index__184 == len__186
    t420 = inline890
    if t420 {
        vec_push__Vec_5int32(self__183, value__185)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__183, index__184)
        var t422 int = len__186 - 1
        var t423 int32 = vec_get__Vec_5int32(self__183, t422)
        vec_push__Vec_5int32(self__183, t423)
        var current__187 int = len__186 - 1
        Loop_loop426:
        for {
            var t427 bool = current__187 > index__184
            if t427 {
                var index70 int = current__187
                vec_get__Vec_5int32(self__183, index70)
                var t428 int = current__187 - 1
                var value72 int32 = vec_get__Vec_5int32(self__183, t428)
                vec_set__Vec_5int32(self__183, index70, value72)
                var compound_old74 int = current__187
                var compound_value75 int = 1
                var t430 int = compound_old74 - compound_value75
                current__187 = t430
                continue
            } else {
                break Loop_loop426
            }
        }
        vec_get__Vec_5int32(self__183, index__184)
        vec_set__Vec_5int32(self__183, index__184, value__185)
        return struct{}{}
    }
}

func println__T_int32(value__31 int32) struct{} {
    var t433 string
    var inline894 string = _goml_runtime_core_int32_to_string(value__31)
    t433 = inline894
    _goml_runtime_core_string_println(t433)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__188 *_goml_vec_int32, index__189 int) int32 {
    var len__190 int
    var inline898 int = vec_len__Vec_5int32(self__188)
    len__190 = inline898
    var value__191 int32 = vec_get__Vec_5int32(self__188, index__189)
    var current__192 int = index__189
    Loop_loop439:
    for {
        var t440 int = current__192 + 1
        var t441 bool = t440 < len__190
        if t441 {
            var index84 int = current__192
            vec_get__Vec_5int32(self__188, index84)
            var t442 int = current__192 + 1
            var value86 int32 = vec_get__Vec_5int32(self__188, t442)
            vec_set__Vec_5int32(self__188, index84, value86)
            var compound_old88 int = current__192
            var compound_value89 int = 1
            var t444 int = compound_old88 + compound_value89
            current__192 = t444
            continue
        } else {
            break Loop_loop439
        }
    }
    var t438 int = len__190 - 1
    vec_truncate__Vec_5int32(self__188, t438)
    return value__191
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__179 *_goml_vec_int32, index__180 int) int32 {
    var len__181 int
    var inline902 int = vec_len__Vec_5int32(self__179)
    len__181 = inline902
    var value__182 int32 = vec_get__Vec_5int32(self__179, index__180)
    var t450 int = index__180 + 1
    var t451 bool = t450 < len__181
    if t451 {
        vec_get__Vec_5int32(self__179, index__180)
        var t452 int = len__181 - 1
        var value63 int32 = vec_get__Vec_5int32(self__179, t452)
        vec_set__Vec_5int32(self__179, index__180, value63)
    } else {}
    var t449 int = len__181 - 1
    vec_truncate__Vec_5int32(self__179, t449)
    return value__182
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__193 *_goml_vec_int32) struct{} {
    var left__194 int = 0
    var t455 int
    var inline918 int = vec_len__Vec_5int32(self__193)
    t455 = inline918
    var right__195 int = t455 - 1
    Loop_loop457:
    for {
        var t458 bool = left__194 < right__195
        if t458 {
            var inline904 int32 = vec_get__Vec_5int32(self__193, left__194)
            vec_get__Vec_5int32(self__193, left__194)
            var inline908 int32 = vec_get__Vec_5int32(self__193, right__195)
            vec_set__Vec_5int32(self__193, left__194, inline908)
            vec_get__Vec_5int32(self__193, right__195)
            vec_set__Vec_5int32(self__193, right__195, inline904)
            var compound_old94 int = left__194
            var compound_value95 int = 1
            var t459 int = compound_old94 + compound_value95
            left__194 = t459
            var compound_old97 int = right__195
            var compound_value98 int = 1
            var t461 int = compound_old97 - compound_value98
            right__195 = t461
            continue
        } else {
            break Loop_loop457
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__157 *_goml_vec_int32, index__158 int) int32 {
    var t465 int32 = vec_get__Vec_5int32(self__157, index__158)
    return t465
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__170 *_goml_vec_int32) Option__int32 {
    var len__171 int
    var inline923 int = vec_len__Vec_5int32(self__170)
    len__171 = inline923
    var t470 bool
    var inline920 int = 0
    var inline921 bool = len__171 == inline920
    t470 = inline921
    if t470 {
        return Option__int32_None{}
    } else {
        var t471 int = len__171 - 1
        var t472 int32 = vec_get__Vec_5int32(self__170, t471)
        var t473 Option__int32 = Option__int32_Some{
            _0: t472,
        }
        return t473
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__172 *_goml_vec_int32) Option__int32 {
    var len__173 int
    var inline930 int = vec_len__Vec_5int32(self__172)
    len__173 = inline930
    var t478 bool
    var inline927 int = 0
    var inline928 bool = len__173 == inline927
    t478 = inline928
    if t478 {
        return Option__int32_None{}
    } else {
        var t479 int = len__173 - 1
        var value__174 int32 = vec_get__Vec_5int32(self__172, t479)
        var t480 int = len__173 - 1
        vec_truncate__Vec_5int32(self__172, t480)
        var t481 Option__int32 = Option__int32_Some{
            _0: value__174,
        }
        return t481
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__167 *_goml_vec_int32, len__168 int) struct{} {
    vec_truncate__Vec_5int32(self__167, len__168)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__164 *_goml_vec_int32) bool {
    var t486 int = vec_len__Vec_5int32(self__164)
    var inline932 int = 0
    var inline933 bool = t486 == inline932
    return inline933
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__169 *_goml_vec_int32) struct{} {
    var inline935 int = 0
    vec_truncate__Vec_5int32(self__169, inline935)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t492 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t492
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__223 *hashmap_string_int32_x, key__224 string, value__225 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__223, key__224, value__225)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__231 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t497 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__231)
    return t497
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__162 *_goml_vec_Tuple2_6string_5int32) int {
    var t500 int = vec_len__Vec_21Tuple2_6string_5int32(self__162)
    return t500
}

func _goml_m_inherent_i_HashMap_i_H_hb7143c95ea85fb1078f35c7fdf7ce7f2_ing____V__int32(self__226 *hashmap_string_int32_x, key__227 string) struct{} {
    hashmap_remove__HashMap_6string_5int32(self__226, key__227)
    return struct{}{}
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__147 func() Option__char) FnIterator__char {
    var t516 FnIterator__char = FnIterator__char{
        next_fn: next_fn__147,
    }
    return t516
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__68 rune) string {
    var inline938 uint32 = uint32(rune(self__68))
    var inline939 bool = utf8_valid_scalar(inline938)
    if inline939 {
        var inline940 string = _goml_runtime_core_char_to_string(self__68)
        return inline940
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__147 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t522 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__147,
    }
    return t522
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func char_to_string(value__29 rune) string {
    var t529 uint32 = uint32(rune(value__29))
    var t530 bool
    var inline943 bool = t529 <= 1114111
    if inline943 {
        var inline944 bool = t529 >= 55296
        var inline946 bool
        if inline944 {
            var inline948 bool = t529 <= 57343
            inline946 = inline948
        } else {
            inline946 = false
        }
        var inline947 bool = !inline946
        t530 = inline947
    } else {
        t530 = false
    }
    if t530 {
        var t531 string = _goml_runtime_core_char_to_string(value__29)
        return t531
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t534 string = _goml_runtime_core_int_to_string(self__69)
    return t534
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t542 string = _goml_runtime_core_bool_to_string(self__66)
    return t542
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t557 bool = index__16 < 0
    var jp548 bool
    if t557 {
        jp548 = true
    } else {
        var t558 int
        var inline950 int = _goml_runtime_core_string_len(value__15)
        t558 = inline950
        var t559 bool = index__16 > t558
        jp548 = t559
    }
    if jp548 {
        return false
    } else {
        var t551 int
        var inline959 int = _goml_runtime_core_string_len(value__15)
        t551 = inline959
        var t552 bool
        var inline957 bool = index__16 == t551
        t552 = inline957
        if t552 {
            return true
        } else {
            var t553 uint8
            var inline955 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t553 = inline955
            var t554_rhs uint8 = 192
            var t554 uint8 = t553 & t554_rhs
            var t555 bool
            var inline952 uint8 = 128
            var inline953 bool = t554 == inline952
            t555 = inline953
            var t556 bool = !t555
            return t556
        }
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t688 bool = index__6 < 0
    var jp686 bool
    if t688 {
        jp686 = true
    } else {
        var t689 bool = index__6 >= length__7
        jp686 = t689
    }
    if jp686 {
        var inline961 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline961
    } else {
        var t573 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t573))
        var t576 bool = first__8 < 128
        if t576 {
            var inline963 int = 1
            var inline964 Option__char = char_from_uint32(first__8)
            switch inline964.(type) {
            case Option__char_None:
                var inline965 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline965
            case Option__char_Some:
                var inline966 rune = inline964.(Option__char_Some)._0
                var inline968 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline966,
                    _2: inline963,
                }
                return inline968
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t580 bool = first__8 < 194
            if t580 {
                var inline970 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline970
            } else {
                var t584 bool = first__8 < 224
                if t584 {
                    var t597 int = length__7 - index__6
                    var t598 bool = t597 < 2
                    if t598 {
                        var inline972 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline972
                    } else {
                        var t586 int = index__6 + 1
                        var t587 uint8
                        var inline986 uint8 = _goml_runtime_core_string_byte_get(value__5, t586)
                        t587 = inline986
                        var second__9 uint32 = uint32(uint8(t587))
                        var t590 bool
                        var inline983 bool = second__9 < 128
                        if inline983 {
                            t590 = true
                        } else {
                            var inline984 bool = second__9 > 191
                            t590 = inline984
                        }
                        if t590 {
                            var inline974 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline974
                        } else {
                            var t592_rhs uint32 = 31
                            var t592 uint32 = first__8 & t592_rhs
                            var t593_rhs int = 6
                            var t593 uint32 = t592 << t593_rhs
                            var t594_rhs uint32 = 63
                            var t594 uint32 = second__9 & t594_rhs
                            var t595 uint32 = t593 | t594
                            var inline976 int = 2
                            var inline977 Option__char = char_from_uint32(t595)
                            switch inline977.(type) {
                            case Option__char_None:
                                var inline978 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline978
                            case Option__char_Some:
                                var inline979 rune = inline977.(Option__char_Some)._0
                                var inline981 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline979,
                                    _2: inline976,
                                }
                                return inline981
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t602 bool = first__8 < 240
                    if t602 {
                        var t635 int = length__7 - index__6
                        var t636 bool = t635 < 3
                        if t636 {
                            var inline988 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline988
                        } else {
                            var t604 int = index__6 + 1
                            var t605 uint8
                            var inline1003 uint8 = _goml_runtime_core_string_byte_get(value__5, t604)
                            t605 = inline1003
                            var second__10 uint32 = uint32(uint8(t605))
                            var t606 int = index__6 + 2
                            var t607 uint8
                            var inline1001 uint8 = _goml_runtime_core_string_byte_get(value__5, t606)
                            t607 = inline1001
                            var third__11 uint32 = uint32(uint8(t607))
                            var t633 bool = utf8_invalid_continuation(second__10)
                            var jp628 bool
                            if t633 {
                                jp628 = true
                            } else {
                                var inline990 bool = third__11 < 128
                                if inline990 {
                                    jp628 = true
                                } else {
                                    var inline991 bool = third__11 > 191
                                    jp628 = inline991
                                }
                            }
                            var jp622 bool
                            if jp628 {
                                jp622 = true
                            } else {
                                var t631 bool
                                var inline993 uint32 = 224
                                var inline994 bool = first__8 == inline993
                                t631 = inline994
                                if t631 {
                                    var t632 bool = second__10 < 160
                                    jp622 = t632
                                } else {
                                    jp622 = false
                                }
                            }
                            var jp611 bool
                            if jp622 {
                                jp611 = true
                            } else {
                                var t625 bool
                                var inline996 uint32 = 237
                                var inline997 bool = first__8 == inline996
                                t625 = inline997
                                if t625 {
                                    var t626 bool = second__10 >= 160
                                    jp611 = t626
                                } else {
                                    jp611 = false
                                }
                            }
                            if jp611 {
                                var inline999 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline999
                            } else {
                                var t613_rhs uint32 = 15
                                var t613 uint32 = first__8 & t613_rhs
                                var t614_rhs int = 12
                                var t614 uint32 = t613 << t614_rhs
                                var t615_rhs uint32 = 63
                                var t615 uint32 = second__10 & t615_rhs
                                var t616_rhs int = 6
                                var t616 uint32 = t615 << t616_rhs
                                var t617 uint32 = t614 | t616
                                var t618_rhs uint32 = 63
                                var t618 uint32 = third__11 & t618_rhs
                                var t619 uint32 = t617 | t618
                                var t620 Tuple3_4bool_4char_3int = utf8_valid_decode(t619, 3)
                                return t620
                            }
                        }
                    } else {
                        var t640 bool = first__8 < 245
                        if t640 {
                            var t681 int = length__7 - index__6
                            var t682 bool = t681 < 4
                            if t682 {
                                var t683 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t683
                            } else {
                                var t642 int = index__6 + 1
                                var t643 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t642)
                                var second__12 uint32 = uint32(uint8(t643))
                                var t644 int = index__6 + 2
                                var t645 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t644)
                                var third__13 uint32 = uint32(uint8(t645))
                                var t646 int = index__6 + 3
                                var t647 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t646)
                                var fourth__14 uint32 = uint32(uint8(t647))
                                var t679 bool = utf8_invalid_continuation(second__12)
                                var jp677 bool
                                if t679 {
                                    jp677 = true
                                } else {
                                    var t680 bool = utf8_invalid_continuation(third__13)
                                    jp677 = t680
                                }
                                var jp671 bool
                                if jp677 {
                                    jp671 = true
                                } else {
                                    var t678 bool = utf8_invalid_continuation(fourth__14)
                                    jp671 = t678
                                }
                                var jp665 bool
                                if jp671 {
                                    jp665 = true
                                } else {
                                    var t674 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t674 {
                                        var t675 bool = second__12 < 144
                                        jp665 = t675
                                    } else {
                                        jp665 = false
                                    }
                                }
                                var jp651 bool
                                if jp665 {
                                    jp651 = true
                                } else {
                                    var t668 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t668 {
                                        var t669 bool = second__12 > 143
                                        jp651 = t669
                                    } else {
                                        jp651 = false
                                    }
                                }
                                if jp651 {
                                    var t652 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t652
                                } else {
                                    var t653_rhs uint32 = 7
                                    var t653 uint32 = first__8 & t653_rhs
                                    var t654_rhs int = 18
                                    var t654 uint32 = t653 << t654_rhs
                                    var t655_rhs uint32 = 63
                                    var t655 uint32 = second__12 & t655_rhs
                                    var t656_rhs int = 12
                                    var t656 uint32 = t655 << t656_rhs
                                    var t657 uint32 = t654 | t656
                                    var t658_rhs uint32 = 63
                                    var t658 uint32 = third__13 & t658_rhs
                                    var t659_rhs int = 6
                                    var t659 uint32 = t658 << t659_rhs
                                    var t660 uint32 = t657 | t659
                                    var t661_rhs uint32 = 63
                                    var t661 uint32 = fourth__14 & t661_rhs
                                    var t662 uint32 = t660 | t661
                                    var t663 Tuple3_4bool_4char_3int = utf8_valid_decode(t662, 4)
                                    return t663
                                }
                            }
                        } else {
                            var t684 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t684
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t698 string = _goml_runtime_core_int32_to_string(self__72)
    return t698
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t706 bool = value__4 <= 1114111
    if t706 {
        var t710 bool = value__4 >= 55296
        var jp708 bool
        if t710 {
            var t711 bool = value__4 <= 57343
            jp708 = t711
        } else {
            jp708 = false
        }
        var t709 bool = !jp708
        return t709
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t714 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t714
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t720 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t720
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1047 rune
    var inline1007 bool = utf8_valid_scalar(value__0)
    if inline1007 {
        var inline1008 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1010 rune = inline1008._1
        commute_field1047 = inline1010
        var t726 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1047,
            _2: width__1,
        }
        return t726
    } else {
        var inline1005 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1005
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t731 bool = value__3 < 128
    if t731 {
        return true
    } else {
        var t732 bool = value__3 > 191
        return t732
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t735 bool = self__117 == other__118
    return t735
}

func char_from_uint32(value__32 uint32) Option__char {
    var t740 bool
    var inline1014 bool = value__32 <= 1114111
    if inline1014 {
        var inline1015 bool = value__32 >= 55296
        var inline1017 bool
        if inline1015 {
            var inline1019 bool = value__32 <= 57343
            inline1017 = inline1019
        } else {
            inline1017 = false
        }
        var inline1018 bool = !inline1017
        t740 = inline1018
    } else {
        t740 = false
    }
    if t740 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t741 Option__char = Option__char_Some{
            _0: x24,
        }
        return t741
    } else {
        return Option__char_None{}
    }
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__127 string) uint64 {
    var t744 uint64 = _goml_runtime_core_string_hash(self__127)
    return t744
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env227 closure_env_inherent_string_string_chars_0) Option__char {
    var self__54 string = env227.self_0
    var index__55 *ref_int_x = env227.index_1
    var t757 int = ref_get__Ref_3int(index__55)
    var commute_field1050 Tuple2_4char_3int
    var inline1021 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__54, t757)
    var inline1022 bool = inline1021._0
    var inline1023 rune = inline1021._1
    var inline1024 int = inline1021._2
    if inline1022 {
        var inline1028 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1023,
            _1: inline1024,
        }
        commute_field1050 = inline1028
        var x32 rune = commute_field1050._0
        var x33 int = commute_field1050._1
        var compound_old34 int = ref_get__Ref_3int(index__55)
        var t760 int = compound_old34 + x33
        ref_set__Ref_3int(index__55, t760)
        var t762 Option__char = Option__char_Some{
            _0: x32,
        }
        return t762
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env228 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var index__60 *ref_int_x = env228.index_0
    var self__59 string = env228.self_1
    var current__61 int = ref_get__Ref_3int(index__60)
    var commute_field1053 Tuple2_4char_3int
    var inline1031 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__59, current__61)
    var inline1032 bool = inline1031._0
    var inline1033 rune = inline1031._1
    var inline1034 int = inline1031._2
    if inline1032 {
        var inline1038 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1033,
            _1: inline1034,
        }
        commute_field1053 = inline1038
        var x40 rune = commute_field1053._0
        var x41 int = commute_field1053._1
        var t767 int = current__61 + x41
        ref_set__Ref_3int(index__60, t767)
        var t768 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__61,
            _1: x40,
        }
        var t769 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q__Some{
            _0: t768,
        }
        return t769
    } else {
        return _goml_m_Option_____o_int_c_char_q__None{}
    }
}

func main() {
    main0()
}
