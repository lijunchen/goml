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
    indices map[string]int
    entries []hashmap_string_int32_x_entry
    len int
}

func hashmap_new__HashMap_6string_5int32() *hashmap_string_int32_x {
    return &hashmap_string_int32_x{
        indices: make(map[string]int),
        entries: nil,
        len: 0,
    }
}

func hashmap_set__HashMap_6string_5int32(m *hashmap_string_int32_x, key string, value int32) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if found {
        var entry hashmap_string_int32_x_entry = m.entries[index]
        if entry.active {
            m.entries[index].value = value
            return struct{}{}
        }
        m.entries[index] = hashmap_string_int32_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    index = len(m.entries)
    m.indices[key] = index
    m.entries = append(m.entries, hashmap_string_int32_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.len = m.len + 1
    return struct{}{}
}

func hashmap_remove__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if !found {
        return struct{}{}
    }
    var entry hashmap_string_int32_x_entry = m.entries[index]
    if !entry.active {
        return struct{}{}
    }
    var zero hashmap_string_int32_x_entry
    m.entries[index] = zero
    m.len = m.len - 1
    return struct{}{}
}

func hashmap_entries__HashMap_6string_5int32(m *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var result []Tuple2_6string_5int32
    if m == nil {
        return &_goml_vec_Tuple2_6string_5int32{
            items: result,
        }
    }
    for _, entry := range m.entries {
        if entry.active {
            result = append(result, Tuple2_6string_5int32{
                _0: entry.key,
                _1: entry.value,
            })
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

type Ordering int32

type Option__char struct {
    _tag int32
    _v1_0 rune
}

type _goml_m_Option_____o_int_c_char_q_ struct {
    _tag int32
    _v1_0 Tuple2_3int_4char
}

type _goml_m_Option_____o_char_c_int_q_ struct {
    _tag int32
    _v1_0 Tuple2_4char_3int
}

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func print_chars(value__0 string) struct{} {
    var t502 FnIterator__char
    var inline1042 *ref_int_x = ref__Ref_3int(0)
    var inline1043 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: value__0,
        index_1: inline1042,
    }
    var inline1044 func() Option__char = func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(inline1043)
    }
    var inline1045 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(inline1044)
    t502 = inline1045
    var for_iter408 FnIterator__char
    for_iter408 = t502
    Loop_loop504:
    for {
        var for_next409 Option__char
        var inline1038 func() Option__char = for_iter408.next_fn
        var inline1039 Option__char = inline1038()
        for_next409 = inline1039
        switch for_next409._tag {
        case 0:
            break Loop_loop504
        case 1:
            var x410 rune = for_next409._v1_0
            var inline1035 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x410)
            _goml_runtime_core_string_println(inline1035)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t508 _goml_m_FnIterator_____o_int_c_char_q_
    var inline1058 *ref_int_x = ref__Ref_3int(0)
    var inline1059 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: inline1058,
        self_1: value__2,
    }
    var inline1060 func() _goml_m_Option_____o_int_c_char_q_ = func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(inline1059)
    }
    var inline1061 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(inline1060)
    t508 = inline1061
    var for_iter411 _goml_m_FnIterator_____o_int_c_char_q_
    for_iter411 = t508
    Loop_loop510:
    for {
        var for_next412 _goml_m_Option_____o_int_c_char_q_
        var inline1054 func() _goml_m_Option_____o_int_c_char_q_ = for_iter411.next_fn
        var inline1055 _goml_m_Option_____o_int_c_char_q_ = inline1054()
        for_next412 = inline1055
        switch for_next412._tag {
        case 0:
            break Loop_loop510
        case 1:
            var x413 Tuple2_3int_4char = for_next412._v1_0
            var x415 int = x413._0
            var x416 rune = x413._1
            var t512 string
            var inline1052 string = _goml_runtime_core_int_to_string(x415)
            t512 = inline1052
            var t513 string = t512 + ":"
            var t514 string
            var inline1050 string = char_to_string(x416)
            t514 = inline1050
            var t515 string = t513 + t514
            var inline1047 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t515)
            _goml_runtime_core_string_println(inline1047)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t518 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t518)
    var t519 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t519)
    var t520 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t520)
    var t521 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t521)
    var t522 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t522)
    var t523 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t523)
    var t524 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t524)
    var t525 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t525)
    var t526 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t526)
    var t527 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t527)
    var t528 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t528)
    var t529 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t529)
    var mtmp429 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp429._tag {
    case 0:
        var inline1063 string = "missing"
        var inline1064 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1063)
        _goml_runtime_core_string_println(inline1064)
    case 1:
        var x430 Tuple2_4char_3int = mtmp429._v1_0
        var x432 rune = x430._0
        var x433 int = x430._1
        var inline1070 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x432)
        _goml_runtime_core_string_println(inline1070)
        var inline1067 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x433)
        _goml_runtime_core_string_println(inline1067)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t531 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t531)
    var t532 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t532)
    var t533 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t533)
    var mtmp441 Tuple2_4bool_6string = string_from_utf8(bytes__10)
    var x442 bool = mtmp441._0
    var x443 string = mtmp441._1
    println__T_bool(x442)
    println__T_string(x443)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp448 Tuple2_4bool_6string = string_from_utf8(invalid__13)
    var x449 bool = mtmp448._0
    var x450 string = mtmp448._1
    println__T_bool(x449)
    var t534 bool = x450 == ""
    println__T_bool(t534)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t535 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t536 bool = t535 >= 3
    println__T_bool(t536)
    var t537 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t537)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t538 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t538)
    var t539 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t540 bool = t539 >= 1
    println__T_bool(t540)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t541 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t542 bool = t541 >= 100
    println__T_bool(t542)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t543 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t543)
    var t544 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t544)
    var t545 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t545)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t546 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t546)
    var t547 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t547)
    var mtmp472 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp472._tag {
    case 0:
        var inline1073 int = -1
        var inline1074 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline1073)
        _goml_runtime_core_string_println(inline1074)
    case 1:
        var x473 int32 = mtmp472._v1_0
        var inline1077 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x473)
        _goml_runtime_core_string_println(inline1077)
    default:
        panic("non-exhaustive match")
    }
    var mtmp475 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp475._tag {
    case 0:
        var inline1080 int = -1
        var inline1081 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline1080)
        _goml_runtime_core_string_println(inline1081)
    case 1:
        var x476 int32 = mtmp475._v1_0
        var inline1084 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x476)
        _goml_runtime_core_string_println(inline1084)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t550 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t550)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t551 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t551)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t552 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t552)
    var inline1117 string = "c"
    var inline1118 int32 = 3
    hashmap_set__HashMap_6string_5int32(map__20, inline1117, inline1118)
    var inline1114 string = "a"
    hashmap_remove__HashMap_6string_5int32(map__20, inline1114)
    var t553 int
    var inline1112 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    t553 = inline1112
    var inline1109 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t553)
    _goml_runtime_core_string_println(inline1109)
    var seen_a__22 *ref_bool_x
    var inline1106 bool = false
    var inline1107 *ref_bool_x = ref__Ref_4bool(inline1106)
    seen_a__22 = inline1107
    var seen_b__23 *ref_bool_x
    var inline1103 bool = false
    var inline1104 *ref_bool_x = ref__Ref_4bool(inline1103)
    seen_b__23 = inline1104
    var for_limit490 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    var for_index491 int = 0
    Loop_loop558:
    for {
        var t559 bool = for_index491 < for_limit490
        if t559 {
            var for_item492 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(entries__21, for_index491)
            var t560 int = for_index491 + 1
            for_index491 = t560
            var x495 string = for_item492._0
            var x496 int32 = for_item492._1
            var t571 bool = x495 == "a"
            var jp563 bool
            if t571 {
                var t572 bool = x496 == 1
                jp563 = t572
            } else {
                jp563 = false
            }
            if jp563 {
                var inline1087 bool = true
                ref_set__Ref_4bool(seen_a__22, inline1087)
                continue
            } else {
                var t569 bool = x495 == "b"
                var jp567 bool
                if t569 {
                    var t570 bool = x496 == 2
                    jp567 = t570
                } else {
                    jp567 = false
                }
                if jp567 {
                    var inline1090 bool = true
                    ref_set__Ref_4bool(seen_b__23, inline1090)
                    continue
                } else {
                    continue
                }
            }
        } else {
            break Loop_loop558
        }
    }
    var t555 bool
    var inline1101 bool = ref_get__Ref_4bool(seen_a__22)
    t555 = inline1101
    var inline1098 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t555)
    _goml_runtime_core_string_println(inline1098)
    var t556 bool
    var inline1096 bool = ref_get__Ref_4bool(seen_b__23)
    t556 = inline1096
    var inline1093 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t556)
    _goml_runtime_core_string_println(inline1093)
    return struct{}{}
}

func println__T_char(value__1 rune) struct{} {
    var t591 string
    var inline1123 string = char_to_string(value__1)
    t591 = inline1123
    _goml_runtime_core_string_println(t591)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t605 string
    t605 = value__1
    _goml_runtime_core_string_println(t605)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t614 string
    var inline1133 string = _goml_runtime_core_int_to_string(value__1)
    t614 = inline1133
    _goml_runtime_core_string_println(t614)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t618 int = _goml_runtime_core_string_len(self__36)
    return t618
}

func _goml_m_inherent_i_string_i_string_i_len(self__35 string) int {
    var t621 int = _goml_runtime_core_string_len(self__35)
    return t621
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline1135 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline1136 bool = inline1135._0
    var inline1137 rune = inline1135._1
    if inline1136 {
        return inline1137
    } else {
        var inline1140 rune = _goml_runtime_core_string_get("", -1)
        return inline1140
    }
}

func println__T_bool(value__1 bool) struct{} {
    var t626 string
    var inline1142 string = _goml_runtime_core_bool_to_string(value__1)
    t626 = inline1142
    _goml_runtime_core_string_println(t626)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__44 string, index__45 int) bool {
    var t630 bool = string_is_char_boundary(self__44, index__45)
    return t630
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline1144 bool = string_is_char_boundary(self__41, start__42)
    var inline1146 bool
    if inline1144 {
        var inline1149 bool = string_is_char_boundary(self__41, end__43)
        inline1146 = inline1149
    } else {
        inline1146 = false
    }
    if inline1146 {
        var inline1147 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline1147
    } else {
        var inline1148 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline1148
    }
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__46 string, index__47 int) _goml_m_Option_____o_char_c_int_q_ {
    var mtmp25 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__46, index__47)
    var x26 bool = mtmp25._0
    var x27 rune = mtmp25._1
    var x28 int = mtmp25._2
    if x26 {
        var t638 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: x27,
            _1: x28,
        }
        var t639 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q_{
            _tag: 1,
            _v1_0: t638,
        }
        return t639
    } else {
        return _goml_m_Option_____o_char_c_int_q_{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__51 string) *_goml_vec_uint8 {
    var t642 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__51)
    return t642
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__273 *_goml_vec_uint8) int {
    var t645 int = vec_len__Vec_5uint8(self__273)
    return t645
}

func println__T_uint8(value__1 uint8) struct{} {
    var t647 string
    var inline1151 string = _goml_runtime_core_uint8_to_string(value__1)
    t647 = inline1151
    _goml_runtime_core_string_println(t647)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__268 *_goml_vec_uint8, index__269 int) uint8 {
    var t651 uint8 = vec_get__Vec_5uint8(self__268, index__269)
    return t651
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop656:
    for {
        var t657 int
        var inline1153 int = _goml_runtime_core_string_len(x12)
        t657 = inline1153
        var t658 bool = index__26 < t657
        if t658 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t660 int = compound_old17 + x16
                index__26 = t660
                continue
            } else {
                var t662 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t662
            }
        } else {
            break Loop_loop656
        }
    }
    var t655 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t655
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t665 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t665
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__258 *_goml_vec_uint8, elem__259 uint8) struct{} {
    vec_push__Vec_5uint8(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__257 int) *_goml_vec_string {
    var t670 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__257)
    return t670
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__258 *_goml_vec_string, elem__259 string) struct{} {
    vec_push__Vec_6string(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__274 *_goml_vec_string) int {
    var t675 int = vec_capacity__Vec_6string(self__274)
    return t675
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__257 int) *_goml_vec_int32 {
    var t678 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__257)
    return t678
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__273 *_goml_vec_int32) int {
    var t681 int = vec_len__Vec_5int32(self__273)
    return t681
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__274 *_goml_vec_int32) int {
    var t684 int = vec_capacity__Vec_5int32(self__274)
    return t684
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(self__276 *_goml_vec_int32, additional__277 int) struct{} {
    vec_reserve__Vec_5int32(self__276, additional__277)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(self__258 *_goml_vec_int32, elem__259 int32) struct{} {
    vec_push__Vec_5int32(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(self__294 *_goml_vec_int32, index__295 int, value__296 int32) struct{} {
    var len__297 int
    var inline1159 int = vec_len__Vec_5int32(self__294)
    len__297 = inline1159
    var t691 bool = index__295 == len__297
    if t691 {
        vec_push__Vec_5int32(self__294, value__296)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__294, index__295)
        var t693 int = len__297 - 1
        var t694 int32 = vec_get__Vec_5int32(self__294, t693)
        vec_push__Vec_5int32(self__294, t694)
        var current__298 int = len__297 - 1
        Loop_loop697:
        for {
            var t698 bool = current__298 > index__295
            if t698 {
                var index221 int = current__298
                vec_get__Vec_5int32(self__294, index221)
                var t699 int = current__298 - 1
                var value223 int32 = vec_get__Vec_5int32(self__294, t699)
                vec_set__Vec_5int32(self__294, index221, value223)
                var compound_old225 int = current__298
                var compound_value226 int = 1
                var t701 int = compound_old225 - compound_value226
                current__298 = t701
                continue
            } else {
                break Loop_loop697
            }
        }
        vec_get__Vec_5int32(self__294, index__295)
        vec_set__Vec_5int32(self__294, index__295, value__296)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t704 string
    var inline1161 string = _goml_runtime_core_int32_to_string(value__1)
    t704 = inline1161
    _goml_runtime_core_string_println(t704)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__299 *_goml_vec_int32, index__300 int) int32 {
    var len__301 int
    var inline1165 int = vec_len__Vec_5int32(self__299)
    len__301 = inline1165
    var value__302 int32 = vec_get__Vec_5int32(self__299, index__300)
    var current__303 int = index__300
    Loop_loop710:
    for {
        var t711 int = current__303 + 1
        var t712 bool = t711 < len__301
        if t712 {
            var index235 int = current__303
            vec_get__Vec_5int32(self__299, index235)
            var t713 int = current__303 + 1
            var value237 int32 = vec_get__Vec_5int32(self__299, t713)
            vec_set__Vec_5int32(self__299, index235, value237)
            var compound_old239 int = current__303
            var compound_value240 int = 1
            var t715 int = compound_old239 + compound_value240
            current__303 = t715
            continue
        } else {
            break Loop_loop710
        }
    }
    var t709 int = len__301 - 1
    vec_truncate__Vec_5int32(self__299, t709)
    return value__302
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__290 *_goml_vec_int32, index__291 int) int32 {
    var len__292 int
    var inline1169 int = vec_len__Vec_5int32(self__290)
    len__292 = inline1169
    var value__293 int32 = vec_get__Vec_5int32(self__290, index__291)
    var t721 int = index__291 + 1
    var t722 bool = t721 < len__292
    if t722 {
        vec_get__Vec_5int32(self__290, index__291)
        var t723 int = len__292 - 1
        var value214 int32 = vec_get__Vec_5int32(self__290, t723)
        vec_set__Vec_5int32(self__290, index__291, value214)
    } else {}
    var t720 int = len__292 - 1
    vec_truncate__Vec_5int32(self__290, t720)
    return value__293
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__304 *_goml_vec_int32) struct{} {
    var left__305 int = 0
    var t726 int
    var inline1185 int = vec_len__Vec_5int32(self__304)
    t726 = inline1185
    var right__306 int = t726 - 1
    Loop_loop728:
    for {
        var t729 bool = left__305 < right__306
        if t729 {
            var inline1171 int32 = vec_get__Vec_5int32(self__304, left__305)
            vec_get__Vec_5int32(self__304, left__305)
            var inline1175 int32 = vec_get__Vec_5int32(self__304, right__306)
            vec_set__Vec_5int32(self__304, left__305, inline1175)
            vec_get__Vec_5int32(self__304, right__306)
            vec_set__Vec_5int32(self__304, right__306, inline1171)
            var compound_old245 int = left__305
            var compound_value246 int = 1
            var t730 int = compound_old245 + compound_value246
            left__305 = t730
            var compound_old248 int = right__306
            var compound_value249 int = 1
            var t732 int = compound_old248 - compound_value249
            right__306 = t732
            continue
        } else {
            break Loop_loop728
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__268 *_goml_vec_int32, index__269 int) int32 {
    var t736 int32 = vec_get__Vec_5int32(self__268, index__269)
    return t736
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__281 *_goml_vec_int32) Option__int32 {
    var len__282 int
    var inline1187 int = vec_len__Vec_5int32(self__281)
    len__282 = inline1187
    var t741 bool = len__282 == 0
    if t741 {
        return Option__int32{
            _tag: 0,
        }
    } else {
        var t742 int = len__282 - 1
        var t743 int32 = vec_get__Vec_5int32(self__281, t742)
        var t744 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: t743,
        }
        return t744
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__283 *_goml_vec_int32) Option__int32 {
    var len__284 int
    var inline1191 int = vec_len__Vec_5int32(self__283)
    len__284 = inline1191
    var t749 bool = len__284 == 0
    if t749 {
        return Option__int32{
            _tag: 0,
        }
    } else {
        var t750 int = len__284 - 1
        var value__285 int32 = vec_get__Vec_5int32(self__283, t750)
        var t751 int = len__284 - 1
        vec_truncate__Vec_5int32(self__283, t751)
        var t752 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: value__285,
        }
        return t752
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__278 *_goml_vec_int32, len__279 int) struct{} {
    vec_truncate__Vec_5int32(self__278, len__279)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__275 *_goml_vec_int32) bool {
    var t757 int = vec_len__Vec_5int32(self__275)
    var t758 bool = t757 == 0
    return t758
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__280 *_goml_vec_int32) struct{} {
    var inline1193 int = 0
    vec_truncate__Vec_5int32(self__280, inline1193)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t763 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t763
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__422 *hashmap_string_int32_x, key__423 string, value__424 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__422, key__423, value__424)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__430 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t768 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__430)
    return t768
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__273 *_goml_vec_Tuple2_6string_5int32) int {
    var t771 int = vec_len__Vec_21Tuple2_6string_5int32(self__273)
    return t771
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__254 func() Option__char) FnIterator__char {
    var t784 FnIterator__char = FnIterator__char{
        next_fn: next_fn__254,
    }
    return t784
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__150 rune) string {
    var inline1196 uint32 = uint32(rune(self__150))
    var inline1197 bool = utf8_valid_scalar(inline1196)
    if inline1197 {
        var inline1198 string = _goml_runtime_core_char_to_string(self__150)
        return inline1198
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__254 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t790 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__254,
    }
    return t790
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func char_to_string(value__29 rune) string {
    var t797 uint32 = uint32(rune(value__29))
    var t798 bool
    var inline1201 bool = t797 <= 1114111
    if inline1201 {
        var inline1202 bool = t797 >= 55296
        var inline1204 bool
        if inline1202 {
            var inline1206 bool = t797 <= 57343
            inline1204 = inline1206
        } else {
            inline1204 = false
        }
        var inline1205 bool = !inline1204
        t798 = inline1205
    } else {
        t798 = false
    }
    if t798 {
        var t799 string = _goml_runtime_core_char_to_string(value__29)
        return t799
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t802 string = _goml_runtime_core_int_to_string(self__151)
    return t802
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t810 string = _goml_runtime_core_bool_to_string(self__148)
    return t810
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t824 bool = index__16 < 0
    var jp816 bool
    if t824 {
        jp816 = true
    } else {
        var t825 int
        var inline1208 int = _goml_runtime_core_string_len(value__15)
        t825 = inline1208
        var t826 bool = index__16 > t825
        jp816 = t826
    }
    if jp816 {
        return false
    } else {
        var t819 int
        var inline1212 int = _goml_runtime_core_string_len(value__15)
        t819 = inline1212
        var t820 bool = index__16 == t819
        if t820 {
            return true
        } else {
            var t821 uint8
            var inline1210 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t821 = inline1210
            var t822_rhs uint8 = 192
            var t822 uint8 = t821 & t822_rhs
            var t823 bool = t822 != 128
            return t823
        }
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t955 bool = index__6 < 0
    var jp953 bool
    if t955 {
        jp953 = true
    } else {
        var t956 bool = index__6 >= length__7
        jp953 = t956
    }
    if jp953 {
        var inline1214 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1214
    } else {
        var t840 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t840))
        var t843 bool = first__8 < 128
        if t843 {
            var inline1216 int = 1
            var inline1217 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1217._tag {
            case 0:
                var inline1218 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1218
            case 1:
                var inline1219 rune = inline1217._v1_0
                var inline1221 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1219,
                    _2: inline1216,
                }
                return inline1221
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t847 bool = first__8 < 194
            if t847 {
                var inline1223 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1223
            } else {
                var t851 bool = first__8 < 224
                if t851 {
                    var t864 int = length__7 - index__6
                    var t865 bool = t864 < 2
                    if t865 {
                        var inline1225 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1225
                    } else {
                        var t853 int = index__6 + 1
                        var t854 uint8
                        var inline1239 uint8 = _goml_runtime_core_string_byte_get(value__5, t853)
                        t854 = inline1239
                        var second__9 uint32 = uint32(uint8(t854))
                        var t857 bool
                        var inline1236 bool = second__9 < 128
                        if inline1236 {
                            t857 = true
                        } else {
                            var inline1237 bool = second__9 > 191
                            t857 = inline1237
                        }
                        if t857 {
                            var inline1227 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1227
                        } else {
                            var t859_rhs uint32 = 31
                            var t859 uint32 = first__8 & t859_rhs
                            var t860_rhs int = 6
                            var t860 uint32 = t859 << t860_rhs
                            var t861_rhs uint32 = 63
                            var t861 uint32 = second__9 & t861_rhs
                            var t862 uint32 = t860 | t861
                            var inline1229 int = 2
                            var inline1230 Option__char = __goml_builtin_char_from_uint32(t862)
                            switch inline1230._tag {
                            case 0:
                                var inline1231 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1231
                            case 1:
                                var inline1232 rune = inline1230._v1_0
                                var inline1234 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1232,
                                    _2: inline1229,
                                }
                                return inline1234
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t869 bool = first__8 < 240
                    if t869 {
                        var t902 int = length__7 - index__6
                        var t903 bool = t902 < 3
                        if t903 {
                            var inline1241 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1241
                        } else {
                            var t871 int = index__6 + 1
                            var t872 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t871)
                            var second__10 uint32 = uint32(uint8(t872))
                            var t873 int = index__6 + 2
                            var t874 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t873)
                            var third__11 uint32 = uint32(uint8(t874))
                            var t900 bool = utf8_invalid_continuation(second__10)
                            var jp895 bool
                            if t900 {
                                jp895 = true
                            } else {
                                var inline1243 bool = third__11 < 128
                                if inline1243 {
                                    jp895 = true
                                } else {
                                    var inline1244 bool = third__11 > 191
                                    jp895 = inline1244
                                }
                            }
                            var jp889 bool
                            if jp895 {
                                jp889 = true
                            } else {
                                var t898 bool = first__8 == 224
                                if t898 {
                                    var t899 bool = second__10 < 160
                                    jp889 = t899
                                } else {
                                    jp889 = false
                                }
                            }
                            var jp878 bool
                            if jp889 {
                                jp878 = true
                            } else {
                                var t892 bool = first__8 == 237
                                if t892 {
                                    var t893 bool = second__10 >= 160
                                    jp878 = t893
                                } else {
                                    jp878 = false
                                }
                            }
                            if jp878 {
                                var inline1246 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1246
                            } else {
                                var t880_rhs uint32 = 15
                                var t880 uint32 = first__8 & t880_rhs
                                var t881_rhs int = 12
                                var t881 uint32 = t880 << t881_rhs
                                var t882_rhs uint32 = 63
                                var t882 uint32 = second__10 & t882_rhs
                                var t883_rhs int = 6
                                var t883 uint32 = t882 << t883_rhs
                                var t884 uint32 = t881 | t883
                                var t885_rhs uint32 = 63
                                var t885 uint32 = third__11 & t885_rhs
                                var t886 uint32 = t884 | t885
                                var inline1248 int = 3
                                var inline1249 Option__char = __goml_builtin_char_from_uint32(t886)
                                switch inline1249._tag {
                                case 0:
                                    var inline1250 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1250
                                case 1:
                                    var inline1251 rune = inline1249._v1_0
                                    var inline1253 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1251,
                                        _2: inline1248,
                                    }
                                    return inline1253
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t907 bool = first__8 < 245
                        if t907 {
                            var t948 int = length__7 - index__6
                            var t949 bool = t948 < 4
                            if t949 {
                                var t950 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t950
                            } else {
                                var t909 int = index__6 + 1
                                var t910 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t909)
                                var second__12 uint32 = uint32(uint8(t910))
                                var t911 int = index__6 + 2
                                var t912 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t911)
                                var third__13 uint32 = uint32(uint8(t912))
                                var t913 int = index__6 + 3
                                var t914 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t913)
                                var fourth__14 uint32 = uint32(uint8(t914))
                                var t946 bool = utf8_invalid_continuation(second__12)
                                var jp944 bool
                                if t946 {
                                    jp944 = true
                                } else {
                                    var t947 bool = utf8_invalid_continuation(third__13)
                                    jp944 = t947
                                }
                                var jp938 bool
                                if jp944 {
                                    jp938 = true
                                } else {
                                    var t945 bool = utf8_invalid_continuation(fourth__14)
                                    jp938 = t945
                                }
                                var jp932 bool
                                if jp938 {
                                    jp932 = true
                                } else {
                                    var t941 bool = first__8 == 240
                                    if t941 {
                                        var t942 bool = second__12 < 144
                                        jp932 = t942
                                    } else {
                                        jp932 = false
                                    }
                                }
                                var jp918 bool
                                if jp932 {
                                    jp918 = true
                                } else {
                                    var t935 bool = first__8 == 244
                                    if t935 {
                                        var t936 bool = second__12 > 143
                                        jp918 = t936
                                    } else {
                                        jp918 = false
                                    }
                                }
                                if jp918 {
                                    var t919 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t919
                                } else {
                                    var t920_rhs uint32 = 7
                                    var t920 uint32 = first__8 & t920_rhs
                                    var t921_rhs int = 18
                                    var t921 uint32 = t920 << t921_rhs
                                    var t922_rhs uint32 = 63
                                    var t922 uint32 = second__12 & t922_rhs
                                    var t923_rhs int = 12
                                    var t923 uint32 = t922 << t923_rhs
                                    var t924 uint32 = t921 | t923
                                    var t925_rhs uint32 = 63
                                    var t925 uint32 = third__13 & t925_rhs
                                    var t926_rhs int = 6
                                    var t926 uint32 = t925 << t926_rhs
                                    var t927 uint32 = t924 | t926
                                    var t928_rhs uint32 = 63
                                    var t928 uint32 = fourth__14 & t928_rhs
                                    var t929 uint32 = t927 | t928
                                    var t930 Tuple3_4bool_4char_3int = utf8_valid_decode(t929, 4)
                                    return t930
                                }
                            }
                        } else {
                            var t951 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t951
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t962 string = _goml_runtime_core_int32_to_string(self__154)
    return t962
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t970 bool = value__4 <= 1114111
    if t970 {
        var t974 bool = value__4 >= 55296
        var jp972 bool
        if t974 {
            var t975 bool = value__4 <= 57343
            jp972 = t975
        } else {
            jp972 = false
        }
        var t973 bool = !jp972
        return t973
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t978 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t978
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t981 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t981
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1296 rune
    var inline1257 bool = utf8_valid_scalar(value__0)
    if inline1257 {
        var inline1258 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1259 rune = inline1258._1
        commute_field1296 = inline1259
        var t987 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1296,
            _2: width__1,
        }
        return t987
    } else {
        var inline1255 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1255
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t992 bool = value__3 < 128
    if t992 {
        return true
    } else {
        var t993 bool = value__3 > 191
        return t993
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t998 bool
    var inline1263 bool = value__30 <= 1114111
    if inline1263 {
        var inline1264 bool = value__30 >= 55296
        var inline1266 bool
        if inline1264 {
            var inline1268 bool = value__30 <= 57343
            inline1266 = inline1268
        } else {
            inline1266 = false
        }
        var inline1267 bool = !inline1266
        t998 = inline1267
    } else {
        t998 = false
    }
    if t998 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t999 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t999
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env499 closure_env_inherent_string_string_chars_0) Option__char {
    var self__52 string = env499.self_0
    var index__53 *ref_int_x = env499.index_1
    var t1021 int = ref_get__Ref_3int(index__53)
    var commute_field1299 Tuple2_4char_3int
    var inline1270 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__52, t1021)
    var inline1271 bool = inline1270._0
    var inline1272 rune = inline1270._1
    var inline1273 int = inline1270._2
    if inline1271 {
        var inline1277 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1272,
            _1: inline1273,
        }
        commute_field1299 = inline1277
        var x32 rune = commute_field1299._0
        var x33 int = commute_field1299._1
        var compound_old34 int = ref_get__Ref_3int(index__53)
        var t1024 int = compound_old34 + x33
        ref_set__Ref_3int(index__53, t1024)
        var t1026 Option__char = Option__char{
            _tag: 1,
            _v1_0: x32,
        }
        return t1026
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env500 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var index__58 *ref_int_x = env500.index_0
    var self__57 string = env500.self_1
    var current__59 int = ref_get__Ref_3int(index__58)
    var commute_field1302 Tuple2_4char_3int
    var inline1280 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__57, current__59)
    var inline1281 bool = inline1280._0
    var inline1282 rune = inline1280._1
    var inline1283 int = inline1280._2
    if inline1281 {
        var inline1287 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1282,
            _1: inline1283,
        }
        commute_field1302 = inline1287
        var x40 rune = commute_field1302._0
        var x41 int = commute_field1302._1
        var t1031 int = current__59 + x41
        ref_set__Ref_3int(index__58, t1031)
        var t1032 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__59,
            _1: x40,
        }
        var t1033 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q_{
            _tag: 1,
            _v1_0: t1032,
        }
        return t1033
    } else {
        return _goml_m_Option_____o_int_c_char_q_{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
