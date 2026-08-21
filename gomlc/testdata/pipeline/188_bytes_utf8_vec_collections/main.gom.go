package main

import (
    _goml_fmt "fmt"
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
        items: make([]string, 0, capacity),
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
        items: make([]int32, 0, capacity),
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
    if additional < 0 {
        panic("negative vector capacity")
    }
    var length int = len(vec.items)
    var required int = length + additional
    if required < length {
        panic("vector capacity overflow")
    }
    if required > cap(vec.items) {
        var next_capacity int = cap(vec.items) * 2
        if next_capacity < required {
            next_capacity = required
        }
        var next_items []int32 = make([]int32, length, next_capacity)
        copy(next_items, vec.items)
        vec.items = next_items
    }
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
    var t505 FnIterator__char
    var inline1045 *ref_int_x = ref__Ref_3int(0)
    var inline1046 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: value__0,
        index_1: inline1045,
    }
    var inline1047 func() Option__char = func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(inline1046)
    }
    var inline1048 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(inline1047)
    t505 = inline1048
    var for_iter411 FnIterator__char
    for_iter411 = t505
    Loop_loop507:
    for {
        var for_next412 Option__char
        var inline1041 func() Option__char = for_iter411.next_fn
        var inline1042 Option__char = inline1041()
        for_next412 = inline1042
        switch for_next412._tag {
        case 0:
            break Loop_loop507
        case 1:
            var x413 rune = for_next412._v1_0
            var inline1038 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x413)
            _goml_runtime_core_string_println(inline1038)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t511 _goml_m_FnIterator_____o_int_c_char_q_
    var inline1061 *ref_int_x = ref__Ref_3int(0)
    var inline1062 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: inline1061,
        self_1: value__2,
    }
    var inline1063 func() _goml_m_Option_____o_int_c_char_q_ = func() _goml_m_Option_____o_int_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(inline1062)
    }
    var inline1064 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(inline1063)
    t511 = inline1064
    var for_iter414 _goml_m_FnIterator_____o_int_c_char_q_
    for_iter414 = t511
    Loop_loop513:
    for {
        var for_next415 _goml_m_Option_____o_int_c_char_q_
        var inline1057 func() _goml_m_Option_____o_int_c_char_q_ = for_iter414.next_fn
        var inline1058 _goml_m_Option_____o_int_c_char_q_ = inline1057()
        for_next415 = inline1058
        switch for_next415._tag {
        case 0:
            break Loop_loop513
        case 1:
            var x416 Tuple2_3int_4char = for_next415._v1_0
            var x418 int = x416._0
            var x419 rune = x416._1
            var t515 string
            var inline1055 string = _goml_runtime_core_int_to_string(x418)
            t515 = inline1055
            var t516 string = t515 + ":"
            var t517 string
            var inline1053 string = char_to_string(x419)
            t517 = inline1053
            var t518 string = t516 + t517
            var inline1050 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t518)
            _goml_runtime_core_string_println(inline1050)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t521 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_int(t521)
    var t522 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_int(t522)
    var t523 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t523)
    var t524 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t524)
    var t525 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t525)
    var t526 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t526)
    var t527 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t527)
    var t528 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t528)
    var t529 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t529)
    var t530 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t530)
    var t531 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t531)
    var t532 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t532)
    var mtmp432 _goml_m_Option_____o_char_c_int_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp432._tag {
    case 0:
        var inline1066 string = "missing"
        var inline1067 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1066)
        _goml_runtime_core_string_println(inline1067)
    case 1:
        var x433 Tuple2_4char_3int = mtmp432._v1_0
        var x435 rune = x433._0
        var x436 int = x433._1
        var inline1073 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x435)
        _goml_runtime_core_string_println(inline1073)
        var inline1070 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x436)
        _goml_runtime_core_string_println(inline1070)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t534 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(bytes__10)
    println__T_int(t534)
    var t535 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 0)
    println__T_uint8(t535)
    var t536 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(bytes__10, 1)
    println__T_uint8(t536)
    var mtmp444 Tuple2_4bool_6string = string_from_utf8(bytes__10)
    var x445 bool = mtmp444._0
    var x446 string = mtmp444._1
    println__T_bool(x445)
    println__T_string(x446)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(invalid__13, 254)
    var mtmp451 Tuple2_4bool_6string = string_from_utf8(invalid__13)
    var x452 bool = mtmp451._0
    var x453 string = mtmp451._1
    println__T_bool(x452)
    var t537 bool = x453 == ""
    println__T_bool(t537)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t538 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t539 bool = t538 >= 3
    println__T_bool(t539)
    var t540 string = _goml_runtime_core_string_concat(parts__16)
    println__T_string(t540)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(1)
    var t541 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t541)
    var t542 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t543 bool = t542 >= 1
    println__T_bool(t543)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__int32(values__17, 100)
    var t544 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(values__17)
    var t545 bool = t544 >= 100
    println__T_bool(t545)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__int32(values__17, 1, 9)
    var t546 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t546)
    var t547 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(values__17, 2)
    println__T_int32(t547)
    var t548 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(values__17, 0)
    println__T_int32(t548)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(values__17)
    var t549 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 0)
    println__T_int32(t549)
    var t550 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(values__17, 1)
    println__T_int32(t550)
    var mtmp475 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(values__17)
    switch mtmp475._tag {
    case 0:
        var inline1076 int = -1
        var inline1077 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline1076)
        _goml_runtime_core_string_println(inline1077)
    case 1:
        var x476 int32 = mtmp475._v1_0
        var inline1080 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x476)
        _goml_runtime_core_string_println(inline1080)
    default:
        panic("non-exhaustive match")
    }
    var mtmp478 Option__int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(values__17)
    switch mtmp478._tag {
    case 0:
        var inline1083 int = -1
        var inline1084 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline1083)
        _goml_runtime_core_string_println(inline1084)
    case 1:
        var x479 int32 = mtmp478._v1_0
        var inline1087 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x479)
        _goml_runtime_core_string_println(inline1087)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(values__17, 0)
    var t553 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(values__17)
    println__T_bool(t553)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(values__17)
    var t554 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(values__17)
    println__T_int(t554)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(map__20)
    var t555 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(entries__21)
    println__T_int(t555)
    var inline1120 string = "c"
    var inline1121 int32 = 3
    hashmap_set__HashMap_6string_5int32(map__20, inline1120, inline1121)
    var inline1117 string = "a"
    hashmap_remove__HashMap_6string_5int32(map__20, inline1117)
    var t556 int
    var inline1115 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    t556 = inline1115
    var inline1112 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t556)
    _goml_runtime_core_string_println(inline1112)
    var seen_a__22 *ref_bool_x
    var inline1109 bool = false
    var inline1110 *ref_bool_x = ref__Ref_4bool(inline1109)
    seen_a__22 = inline1110
    var seen_b__23 *ref_bool_x
    var inline1106 bool = false
    var inline1107 *ref_bool_x = ref__Ref_4bool(inline1106)
    seen_b__23 = inline1107
    var for_limit493 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    var for_index494 int = 0
    Loop_loop561:
    for {
        var t562 bool = for_index494 < for_limit493
        if t562 {
            var for_item495 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(entries__21, for_index494)
            var t563 int = for_index494 + 1
            for_index494 = t563
            var x498 string = for_item495._0
            var x499 int32 = for_item495._1
            var t574 bool = x498 == "a"
            var jp566 bool
            if t574 {
                var t575 bool = x499 == 1
                jp566 = t575
            } else {
                jp566 = false
            }
            if jp566 {
                var inline1090 bool = true
                ref_set__Ref_4bool(seen_a__22, inline1090)
                continue
            } else {
                var t572 bool = x498 == "b"
                var jp570 bool
                if t572 {
                    var t573 bool = x499 == 2
                    jp570 = t573
                } else {
                    jp570 = false
                }
                if jp570 {
                    var inline1093 bool = true
                    ref_set__Ref_4bool(seen_b__23, inline1093)
                    continue
                } else {
                    continue
                }
            }
        } else {
            break Loop_loop561
        }
    }
    var t558 bool
    var inline1104 bool = ref_get__Ref_4bool(seen_a__22)
    t558 = inline1104
    var inline1101 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t558)
    _goml_runtime_core_string_println(inline1101)
    var t559 bool
    var inline1099 bool = ref_get__Ref_4bool(seen_b__23)
    t559 = inline1099
    var inline1096 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t559)
    _goml_runtime_core_string_println(inline1096)
    return struct{}{}
}

func println__T_char(value__1 rune) struct{} {
    var t594 string
    var inline1126 string = char_to_string(value__1)
    t594 = inline1126
    _goml_runtime_core_string_println(t594)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t608 string
    t608 = value__1
    _goml_runtime_core_string_println(t608)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t617 string
    var inline1136 string = _goml_runtime_core_int_to_string(value__1)
    t617 = inline1136
    _goml_runtime_core_string_println(t617)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t621 int = _goml_runtime_core_string_len(self__36)
    return t621
}

func _goml_m_inherent_i_string_i_string_i_len(self__35 string) int {
    var t624 int = _goml_runtime_core_string_len(self__35)
    return t624
}

func _goml_m_inherent_i_string_i_string_i_get(self__37 string, index__38 int) rune {
    var inline1138 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__37, index__38)
    var inline1139 bool = inline1138._0
    var inline1140 rune = inline1138._1
    if inline1139 {
        return inline1140
    } else {
        var inline1143 rune = _goml_runtime_core_string_get("", -1)
        return inline1143
    }
}

func println__T_bool(value__1 bool) struct{} {
    var t629 string
    var inline1145 string = _goml_runtime_core_bool_to_string(value__1)
    t629 = inline1145
    _goml_runtime_core_string_println(t629)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__44 string, index__45 int) bool {
    var t633 bool = string_is_char_boundary(self__44, index__45)
    return t633
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__41 string, start__42 int, end__43 int) string {
    var inline1147 bool = string_is_char_boundary(self__41, start__42)
    var inline1149 bool
    if inline1147 {
        var inline1152 bool = string_is_char_boundary(self__41, end__43)
        inline1149 = inline1152
    } else {
        inline1149 = false
    }
    if inline1149 {
        var inline1150 string = _goml_runtime_core_string_byte_slice(self__41, start__42, end__43)
        return inline1150
    } else {
        var inline1151 string = _goml_runtime_core_string_byte_slice(self__41, -1, -1)
        return inline1151
    }
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__46 string, index__47 int) _goml_m_Option_____o_char_c_int_q_ {
    var mtmp25 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__46, index__47)
    var x26 bool = mtmp25._0
    var x27 rune = mtmp25._1
    var x28 int = mtmp25._2
    if x26 {
        var t641 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: x27,
            _1: x28,
        }
        var t642 _goml_m_Option_____o_char_c_int_q_ = _goml_m_Option_____o_char_c_int_q_{
            _tag: 1,
            _v1_0: t641,
        }
        return t642
    } else {
        return _goml_m_Option_____o_char_c_int_q_{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__51 string) *_goml_vec_uint8 {
    var t645 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__51)
    return t645
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__uint8(self__273 *_goml_vec_uint8) int {
    var t648 int = vec_len__Vec_5uint8(self__273)
    return t648
}

func println__T_uint8(value__1 uint8) struct{} {
    var t650 string
    var inline1154 string = _goml_runtime_core_uint8_to_string(value__1)
    t650 = inline1154
    _goml_runtime_core_string_println(t650)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__uint8(self__268 *_goml_vec_uint8, index__269 int) uint8 {
    var t654 uint8 = vec_get__Vec_5uint8(self__268, index__269)
    return t654
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop659:
    for {
        var t660 int
        var inline1156 int = _goml_runtime_core_string_len(x12)
        t660 = inline1156
        var t661 bool = index__26 < t660
        if t661 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t663 int = compound_old17 + x16
                index__26 = t663
                continue
            } else {
                var t665 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t665
            }
        } else {
            break Loop_loop659
        }
    }
    var t658 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t658
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t668 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t668
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__258 *_goml_vec_uint8, elem__259 uint8) struct{} {
    vec_push__Vec_5uint8(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__257 int) *_goml_vec_string {
    var t673 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__257)
    return t673
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__258 *_goml_vec_string, elem__259 string) struct{} {
    vec_push__Vec_6string(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__274 *_goml_vec_string) int {
    var t678 int = vec_capacity__Vec_6string(self__274)
    return t678
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__int32(capacity__257 int) *_goml_vec_int32 {
    var t681 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__257)
    return t681
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__273 *_goml_vec_int32) int {
    var t684 int = vec_len__Vec_5int32(self__273)
    return t684
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__int32(self__274 *_goml_vec_int32) int {
    var t687 int = vec_capacity__Vec_5int32(self__274)
    return t687
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
    var inline1162 int = vec_len__Vec_5int32(self__294)
    len__297 = inline1162
    var t694 bool = index__295 == len__297
    if t694 {
        vec_push__Vec_5int32(self__294, value__296)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__294, index__295)
        var t696 int = len__297 - 1
        var t697 int32 = vec_get__Vec_5int32(self__294, t696)
        vec_push__Vec_5int32(self__294, t697)
        var current__298 int = len__297 - 1
        Loop_loop700:
        for {
            var t701 bool = current__298 > index__295
            if t701 {
                var index221 int = current__298
                vec_get__Vec_5int32(self__294, index221)
                var t702 int = current__298 - 1
                var value223 int32 = vec_get__Vec_5int32(self__294, t702)
                vec_set__Vec_5int32(self__294, index221, value223)
                var compound_old225 int = current__298
                var compound_value226 int = 1
                var t704 int = compound_old225 - compound_value226
                current__298 = t704
                continue
            } else {
                break Loop_loop700
            }
        }
        vec_get__Vec_5int32(self__294, index__295)
        vec_set__Vec_5int32(self__294, index__295, value__296)
        return struct{}{}
    }
}

func println__T_int32(value__1 int32) struct{} {
    var t707 string
    var inline1164 string = _goml_runtime_core_int32_to_string(value__1)
    t707 = inline1164
    _goml_runtime_core_string_println(t707)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__int32(self__299 *_goml_vec_int32, index__300 int) int32 {
    var len__301 int
    var inline1168 int = vec_len__Vec_5int32(self__299)
    len__301 = inline1168
    var value__302 int32 = vec_get__Vec_5int32(self__299, index__300)
    var current__303 int = index__300
    Loop_loop713:
    for {
        var t714 int = current__303 + 1
        var t715 bool = t714 < len__301
        if t715 {
            var index235 int = current__303
            vec_get__Vec_5int32(self__299, index235)
            var t716 int = current__303 + 1
            var value237 int32 = vec_get__Vec_5int32(self__299, t716)
            vec_set__Vec_5int32(self__299, index235, value237)
            var compound_old239 int = current__303
            var compound_value240 int = 1
            var t718 int = compound_old239 + compound_value240
            current__303 = t718
            continue
        } else {
            break Loop_loop713
        }
    }
    var t712 int = len__301 - 1
    vec_truncate__Vec_5int32(self__299, t712)
    return value__302
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__int32(self__290 *_goml_vec_int32, index__291 int) int32 {
    var len__292 int
    var inline1172 int = vec_len__Vec_5int32(self__290)
    len__292 = inline1172
    var value__293 int32 = vec_get__Vec_5int32(self__290, index__291)
    var t724 int = index__291 + 1
    var t725 bool = t724 < len__292
    if t725 {
        vec_get__Vec_5int32(self__290, index__291)
        var t726 int = len__292 - 1
        var value214 int32 = vec_get__Vec_5int32(self__290, t726)
        vec_set__Vec_5int32(self__290, index__291, value214)
    } else {}
    var t723 int = len__292 - 1
    vec_truncate__Vec_5int32(self__290, t723)
    return value__293
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__int32(self__304 *_goml_vec_int32) struct{} {
    var left__305 int = 0
    var t729 int
    var inline1188 int = vec_len__Vec_5int32(self__304)
    t729 = inline1188
    var right__306 int = t729 - 1
    Loop_loop731:
    for {
        var t732 bool = left__305 < right__306
        if t732 {
            var inline1174 int32 = vec_get__Vec_5int32(self__304, left__305)
            vec_get__Vec_5int32(self__304, left__305)
            var inline1178 int32 = vec_get__Vec_5int32(self__304, right__306)
            vec_set__Vec_5int32(self__304, left__305, inline1178)
            vec_get__Vec_5int32(self__304, right__306)
            vec_set__Vec_5int32(self__304, right__306, inline1174)
            var compound_old245 int = left__305
            var compound_value246 int = 1
            var t733 int = compound_old245 + compound_value246
            left__305 = t733
            var compound_old248 int = right__306
            var compound_value249 int = 1
            var t735 int = compound_old248 - compound_value249
            right__306 = t735
            continue
        } else {
            break Loop_loop731
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int32(self__268 *_goml_vec_int32, index__269 int) int32 {
    var t739 int32 = vec_get__Vec_5int32(self__268, index__269)
    return t739
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__int32(self__281 *_goml_vec_int32) Option__int32 {
    var len__282 int
    var inline1190 int = vec_len__Vec_5int32(self__281)
    len__282 = inline1190
    var t744 bool = len__282 == 0
    if t744 {
        return Option__int32{
            _tag: 0,
        }
    } else {
        var t745 int = len__282 - 1
        var t746 int32 = vec_get__Vec_5int32(self__281, t745)
        var t747 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: t746,
        }
        return t747
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__int32(self__283 *_goml_vec_int32) Option__int32 {
    var len__284 int
    var inline1194 int = vec_len__Vec_5int32(self__283)
    len__284 = inline1194
    var t752 bool = len__284 == 0
    if t752 {
        return Option__int32{
            _tag: 0,
        }
    } else {
        var t753 int = len__284 - 1
        var value__285 int32 = vec_get__Vec_5int32(self__283, t753)
        var t754 int = len__284 - 1
        vec_truncate__Vec_5int32(self__283, t754)
        var t755 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: value__285,
        }
        return t755
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__int32(self__278 *_goml_vec_int32, len__279 int) struct{} {
    vec_truncate__Vec_5int32(self__278, len__279)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__int32(self__275 *_goml_vec_int32) bool {
    var t760 int = vec_len__Vec_5int32(self__275)
    var t761 bool = t760 == 0
    return t761
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__int32(self__280 *_goml_vec_int32) struct{} {
    var inline1196 int = 0
    vec_truncate__Vec_5int32(self__280, inline1196)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__int32() *hashmap_string_int32_x {
    var t766 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t766
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__int32(self__422 *hashmap_string_int32_x, key__423 string, value__424 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__422, key__423, value__424)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_H_h5d53e9f4fcd36ccc04716112b7466571_ing____V__int32(self__430 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t771 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__430)
    return t771
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_int32_q_(self__273 *_goml_vec_Tuple2_6string_5int32) int {
    var t774 int = vec_len__Vec_21Tuple2_6string_5int32(self__273)
    return t774
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__254 func() Option__char) FnIterator__char {
    var t787 FnIterator__char = FnIterator__char{
        next_fn: next_fn__254,
    }
    return t787
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__150 rune) string {
    var inline1199 uint32 = uint32(rune(self__150))
    var inline1200 bool = utf8_valid_scalar(inline1199)
    if inline1200 {
        var inline1201 string = _goml_runtime_core_char_to_string(self__150)
        return inline1201
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_FnIterator__h559d57d5b56469d45e6c7f16383d3a50_o_int_c_char_q_(next_fn__254 func() _goml_m_Option_____o_int_c_char_q_) _goml_m_FnIterator_____o_int_c_char_q_ {
    var t793 _goml_m_FnIterator_____o_int_c_char_q_ = _goml_m_FnIterator_____o_int_c_char_q_{
        next_fn: next_fn__254,
    }
    return t793
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func char_to_string(value__29 rune) string {
    var t800 uint32 = uint32(rune(value__29))
    var t801 bool
    var inline1204 bool = t800 <= 1114111
    if inline1204 {
        var inline1205 bool = t800 >= 55296
        var inline1207 bool
        if inline1205 {
            var inline1209 bool = t800 <= 57343
            inline1207 = inline1209
        } else {
            inline1207 = false
        }
        var inline1208 bool = !inline1207
        t801 = inline1208
    } else {
        t801 = false
    }
    if t801 {
        var t802 string = _goml_runtime_core_char_to_string(value__29)
        return t802
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t805 string = _goml_runtime_core_int_to_string(self__151)
    return t805
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t813 string = _goml_runtime_core_bool_to_string(self__148)
    return t813
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t827 bool = index__16 < 0
    var jp819 bool
    if t827 {
        jp819 = true
    } else {
        var t828 int
        var inline1211 int = _goml_runtime_core_string_len(value__15)
        t828 = inline1211
        var t829 bool = index__16 > t828
        jp819 = t829
    }
    if jp819 {
        return false
    } else {
        var t822 int
        var inline1215 int = _goml_runtime_core_string_len(value__15)
        t822 = inline1215
        var t823 bool = index__16 == t822
        if t823 {
            return true
        } else {
            var t824 uint8
            var inline1213 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t824 = inline1213
            var t825_rhs uint8 = 192
            var t825 uint8 = t824 & t825_rhs
            var t826 bool = t825 != 128
            return t826
        }
    }
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t958 bool = index__6 < 0
    var jp956 bool
    if t958 {
        jp956 = true
    } else {
        var t959 bool = index__6 >= length__7
        jp956 = t959
    }
    if jp956 {
        var inline1217 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1217
    } else {
        var t843 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t843))
        var t846 bool = first__8 < 128
        if t846 {
            var inline1219 int = 1
            var inline1220 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1220._tag {
            case 0:
                var inline1221 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1221
            case 1:
                var inline1222 rune = inline1220._v1_0
                var inline1224 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1222,
                    _2: inline1219,
                }
                return inline1224
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t850 bool = first__8 < 194
            if t850 {
                var inline1226 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1226
            } else {
                var t854 bool = first__8 < 224
                if t854 {
                    var t867 int = length__7 - index__6
                    var t868 bool = t867 < 2
                    if t868 {
                        var inline1228 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1228
                    } else {
                        var t856 int = index__6 + 1
                        var t857 uint8
                        var inline1242 uint8 = _goml_runtime_core_string_byte_get(value__5, t856)
                        t857 = inline1242
                        var second__9 uint32 = uint32(uint8(t857))
                        var t860 bool
                        var inline1239 bool = second__9 < 128
                        if inline1239 {
                            t860 = true
                        } else {
                            var inline1240 bool = second__9 > 191
                            t860 = inline1240
                        }
                        if t860 {
                            var inline1230 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1230
                        } else {
                            var t862_rhs uint32 = 31
                            var t862 uint32 = first__8 & t862_rhs
                            var t863_rhs int = 6
                            var t863 uint32 = t862 << t863_rhs
                            var t864_rhs uint32 = 63
                            var t864 uint32 = second__9 & t864_rhs
                            var t865 uint32 = t863 | t864
                            var inline1232 int = 2
                            var inline1233 Option__char = __goml_builtin_char_from_uint32(t865)
                            switch inline1233._tag {
                            case 0:
                                var inline1234 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1234
                            case 1:
                                var inline1235 rune = inline1233._v1_0
                                var inline1237 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1235,
                                    _2: inline1232,
                                }
                                return inline1237
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t872 bool = first__8 < 240
                    if t872 {
                        var t905 int = length__7 - index__6
                        var t906 bool = t905 < 3
                        if t906 {
                            var inline1244 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1244
                        } else {
                            var t874 int = index__6 + 1
                            var t875 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t874)
                            var second__10 uint32 = uint32(uint8(t875))
                            var t876 int = index__6 + 2
                            var t877 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t876)
                            var third__11 uint32 = uint32(uint8(t877))
                            var t903 bool = utf8_invalid_continuation(second__10)
                            var jp898 bool
                            if t903 {
                                jp898 = true
                            } else {
                                var inline1246 bool = third__11 < 128
                                if inline1246 {
                                    jp898 = true
                                } else {
                                    var inline1247 bool = third__11 > 191
                                    jp898 = inline1247
                                }
                            }
                            var jp892 bool
                            if jp898 {
                                jp892 = true
                            } else {
                                var t901 bool = first__8 == 224
                                if t901 {
                                    var t902 bool = second__10 < 160
                                    jp892 = t902
                                } else {
                                    jp892 = false
                                }
                            }
                            var jp881 bool
                            if jp892 {
                                jp881 = true
                            } else {
                                var t895 bool = first__8 == 237
                                if t895 {
                                    var t896 bool = second__10 >= 160
                                    jp881 = t896
                                } else {
                                    jp881 = false
                                }
                            }
                            if jp881 {
                                var inline1249 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1249
                            } else {
                                var t883_rhs uint32 = 15
                                var t883 uint32 = first__8 & t883_rhs
                                var t884_rhs int = 12
                                var t884 uint32 = t883 << t884_rhs
                                var t885_rhs uint32 = 63
                                var t885 uint32 = second__10 & t885_rhs
                                var t886_rhs int = 6
                                var t886 uint32 = t885 << t886_rhs
                                var t887 uint32 = t884 | t886
                                var t888_rhs uint32 = 63
                                var t888 uint32 = third__11 & t888_rhs
                                var t889 uint32 = t887 | t888
                                var inline1251 int = 3
                                var inline1252 Option__char = __goml_builtin_char_from_uint32(t889)
                                switch inline1252._tag {
                                case 0:
                                    var inline1253 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1253
                                case 1:
                                    var inline1254 rune = inline1252._v1_0
                                    var inline1256 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1254,
                                        _2: inline1251,
                                    }
                                    return inline1256
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t910 bool = first__8 < 245
                        if t910 {
                            var t951 int = length__7 - index__6
                            var t952 bool = t951 < 4
                            if t952 {
                                var t953 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t953
                            } else {
                                var t912 int = index__6 + 1
                                var t913 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t912)
                                var second__12 uint32 = uint32(uint8(t913))
                                var t914 int = index__6 + 2
                                var t915 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t914)
                                var third__13 uint32 = uint32(uint8(t915))
                                var t916 int = index__6 + 3
                                var t917 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t916)
                                var fourth__14 uint32 = uint32(uint8(t917))
                                var t949 bool = utf8_invalid_continuation(second__12)
                                var jp947 bool
                                if t949 {
                                    jp947 = true
                                } else {
                                    var t950 bool = utf8_invalid_continuation(third__13)
                                    jp947 = t950
                                }
                                var jp941 bool
                                if jp947 {
                                    jp941 = true
                                } else {
                                    var t948 bool = utf8_invalid_continuation(fourth__14)
                                    jp941 = t948
                                }
                                var jp935 bool
                                if jp941 {
                                    jp935 = true
                                } else {
                                    var t944 bool = first__8 == 240
                                    if t944 {
                                        var t945 bool = second__12 < 144
                                        jp935 = t945
                                    } else {
                                        jp935 = false
                                    }
                                }
                                var jp921 bool
                                if jp935 {
                                    jp921 = true
                                } else {
                                    var t938 bool = first__8 == 244
                                    if t938 {
                                        var t939 bool = second__12 > 143
                                        jp921 = t939
                                    } else {
                                        jp921 = false
                                    }
                                }
                                if jp921 {
                                    var t922 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t922
                                } else {
                                    var t923_rhs uint32 = 7
                                    var t923 uint32 = first__8 & t923_rhs
                                    var t924_rhs int = 18
                                    var t924 uint32 = t923 << t924_rhs
                                    var t925_rhs uint32 = 63
                                    var t925 uint32 = second__12 & t925_rhs
                                    var t926_rhs int = 12
                                    var t926 uint32 = t925 << t926_rhs
                                    var t927 uint32 = t924 | t926
                                    var t928_rhs uint32 = 63
                                    var t928 uint32 = third__13 & t928_rhs
                                    var t929_rhs int = 6
                                    var t929 uint32 = t928 << t929_rhs
                                    var t930 uint32 = t927 | t929
                                    var t931_rhs uint32 = 63
                                    var t931 uint32 = fourth__14 & t931_rhs
                                    var t932 uint32 = t930 | t931
                                    var t933 Tuple3_4bool_4char_3int = utf8_valid_decode(t932, 4)
                                    return t933
                                }
                            }
                        } else {
                            var t954 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t954
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t965 string = _goml_runtime_core_int32_to_string(self__154)
    return t965
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t973 bool = value__4 <= 1114111
    if t973 {
        var t977 bool = value__4 >= 55296
        var jp975 bool
        if t977 {
            var t978 bool = value__4 <= 57343
            jp975 = t978
        } else {
            jp975 = false
        }
        var t976 bool = !jp975
        return t976
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t981 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t981
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t984 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t984
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1299 rune
    var inline1260 bool = utf8_valid_scalar(value__0)
    if inline1260 {
        var inline1261 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1262 rune = inline1261._1
        commute_field1299 = inline1262
        var t990 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1299,
            _2: width__1,
        }
        return t990
    } else {
        var inline1258 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1258
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t995 bool = value__3 < 128
    if t995 {
        return true
    } else {
        var t996 bool = value__3 > 191
        return t996
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1001 bool
    var inline1266 bool = value__30 <= 1114111
    if inline1266 {
        var inline1267 bool = value__30 >= 55296
        var inline1269 bool
        if inline1267 {
            var inline1271 bool = value__30 <= 57343
            inline1269 = inline1271
        } else {
            inline1269 = false
        }
        var inline1270 bool = !inline1269
        t1001 = inline1270
    } else {
        t1001 = false
    }
    if t1001 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1002 Option__char = Option__char{
            _tag: 1,
            _v1_0: x24,
        }
        return t1002
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env502 closure_env_inherent_string_string_chars_0) Option__char {
    var self__52 string = env502.self_0
    var index__53 *ref_int_x = env502.index_1
    var t1024 int = ref_get__Ref_3int(index__53)
    var commute_field1302 Tuple2_4char_3int
    var inline1273 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__52, t1024)
    var inline1274 bool = inline1273._0
    var inline1275 rune = inline1273._1
    var inline1276 int = inline1273._2
    if inline1274 {
        var inline1280 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1275,
            _1: inline1276,
        }
        commute_field1302 = inline1280
        var x32 rune = commute_field1302._0
        var x33 int = commute_field1302._1
        var compound_old34 int = ref_get__Ref_3int(index__53)
        var t1027 int = compound_old34 + x33
        ref_set__Ref_3int(index__53, t1027)
        var t1029 Option__char = Option__char{
            _tag: 1,
            _v1_0: x32,
        }
        return t1029
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env503 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_int_c_char_q_ {
    var index__58 *ref_int_x = env503.index_0
    var self__57 string = env503.self_1
    var current__59 int = ref_get__Ref_3int(index__58)
    var commute_field1305 Tuple2_4char_3int
    var inline1283 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__57, current__59)
    var inline1284 bool = inline1283._0
    var inline1285 rune = inline1283._1
    var inline1286 int = inline1283._2
    if inline1284 {
        var inline1290 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1285,
            _1: inline1286,
        }
        commute_field1305 = inline1290
        var x40 rune = commute_field1305._0
        var x41 int = commute_field1305._1
        var t1034 int = current__59 + x41
        ref_set__Ref_3int(index__58, t1034)
        var t1035 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__59,
            _1: x40,
        }
        var t1036 _goml_m_Option_____o_int_c_char_q_ = _goml_m_Option_____o_int_c_char_q_{
            _tag: 1,
            _v1_0: t1035,
        }
        return t1036
    } else {
        return _goml_m_Option_____o_int_c_char_q_{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
