package main

import (
    _goml_os "os"
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

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
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

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
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

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
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

type _goml_vec_uint32 struct {
    items []uint32
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

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type FnIterator__char struct {
    next_fn func() Option__char
}

type _goml_m_FnIterator_____o_isize_c_char_q_ struct {
    next_fn func() _goml_m_Option_____o_isize_c_char_q_
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

type _goml_m_Option_____o_isize_c_char_q_ struct {
    _tag int32
    _v1_0 Tuple2_3int_4char
}

type _goml_m_Option_____o_char_c_isize_q_ struct {
    _tag int32
    _v1_0 Tuple2_4char_3int
}

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func print_chars(value__0 string) struct{} {
    var t890 FnIterator__char
    var inline1506 *ref_int_x = ref__Ref_3int(0)
    var inline1507 closure_env_inherent_string_string_chars_0 = closure_env_inherent_string_string_chars_0{
        self_0: value__0,
        index_1: inline1506,
    }
    var inline1508 func() Option__char = func() Option__char {
        return _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(inline1507)
    }
    var inline1509 FnIterator__char = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(inline1508)
    t890 = inline1509
    var for_iter796 FnIterator__char
    for_iter796 = t890
    Loop_loop892:
    for {
        var for_next797 Option__char
        var inline1502 func() Option__char = for_iter796.next_fn
        var inline1503 Option__char = inline1502()
        for_next797 = inline1503
        switch for_next797._tag {
        case 0:
            break Loop_loop892
        case 1:
            var x798 rune = for_next797._v1_0
            var inline1499 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x798)
            _goml_runtime_core_string_println(inline1499)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func print_char_indices(value__2 string) struct{} {
    var t896 _goml_m_FnIterator_____o_isize_c_char_q_
    var inline1522 *ref_int_x = ref__Ref_3int(0)
    var inline1523 closure_env_inherent_string_string_char_indices_1 = closure_env_inherent_string_string_char_indices_1{
        index_0: inline1522,
        self_1: value__2,
    }
    var inline1524 func() _goml_m_Option_____o_isize_c_char_q_ = func() _goml_m_Option_____o_isize_c_char_q_ {
        return _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(inline1523)
    }
    var inline1525 _goml_m_FnIterator_____o_isize_c_char_q_ = _goml_m_inherent_i_FnIterator__hae8fa7fd40ba2a3681525fbecfca8da7_isize_c_char_q_(inline1524)
    t896 = inline1525
    var for_iter799 _goml_m_FnIterator_____o_isize_c_char_q_
    for_iter799 = t896
    Loop_loop898:
    for {
        var for_next800 _goml_m_Option_____o_isize_c_char_q_
        var inline1518 func() _goml_m_Option_____o_isize_c_char_q_ = for_iter799.next_fn
        var inline1519 _goml_m_Option_____o_isize_c_char_q_ = inline1518()
        for_next800 = inline1519
        switch for_next800._tag {
        case 0:
            break Loop_loop898
        case 1:
            var x801 Tuple2_3int_4char = for_next800._v1_0
            var x803 int = x801._0
            var x804 rune = x801._1
            var t900 string
            var inline1516 string = __goml_builtin_int_to_string(x803)
            t900 = inline1516
            var t901 string = t900 + ":"
            var t902 string
            var inline1514 string = char_to_string(x804)
            t902 = inline1514
            var t903 string = t901 + t902
            var inline1511 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t903)
            _goml_runtime_core_string_println(inline1511)
            continue
        default:
            panic("non-exhaustive match")
        }
    }
    return struct{}{}
}

func main0() struct{} {
    var value__6 string = "a你好😀z"
    var t906 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__6)
    println__T_isize(t906)
    var t907 int = _goml_m_inherent_i_string_i_string_i_len(value__6)
    println__T_isize(t907)
    var t908 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 0)
    println__T_char(t908)
    var t909 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 1)
    println__T_char(t909)
    var t910 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 4)
    println__T_char(t910)
    var t911 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 7)
    println__T_char(t911)
    var t912 rune = _goml_m_inherent_i_string_i_string_i_get(value__6, 11)
    println__T_char(t912)
    var t913 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 0)
    println__T_bool(t913)
    var t914 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 2)
    println__T_bool(t914)
    var t915 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 7)
    println__T_bool(t915)
    var t916 bool = _goml_m_inherent_i_string_i_string_i_is__char__boundary(value__6, 12)
    println__T_bool(t916)
    var t917 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__6, 1, 7)
    println__T_string(t917)
    var mtmp817 _goml_m_Option_____o_char_c_isize_q_ = _goml_m_inherent_i_string_i_string_i_decode__at(value__6, 7)
    switch mtmp817._tag {
    case 0:
        var inline1527 string = "missing"
        var inline1528 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1527)
        _goml_runtime_core_string_println(inline1528)
    case 1:
        var x818 Tuple2_4char_3int = mtmp817._v1_0
        var x820 rune = x818._0
        var x821 int = x818._1
        var inline1534 string = _goml_m_trait__impl_i_ToString_i_char_i_to__string(x820)
        _goml_runtime_core_string_println(inline1534)
        var inline1531 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x821)
        _goml_runtime_core_string_println(inline1531)
    default:
        panic("non-exhaustive match")
    }
    print_chars(value__6)
    print_char_indices(value__6)
    var bytes__10 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__6)
    var t919 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(bytes__10)
    println__T_isize(t919)
    var t920 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__u8(bytes__10, 0)
    println__T_u8(t920)
    var t921 uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__u8(bytes__10, 1)
    println__T_u8(t921)
    var mtmp829 Tuple2_4bool_6string = string_from_utf8(bytes__10)
    var x830 bool = mtmp829._0
    var x831 string = mtmp829._1
    println__T_bool(x830)
    println__T_string(x831)
    var invalid__13 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__u8()
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__u8(invalid__13, 255)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__u8(invalid__13, 254)
    var mtmp836 Tuple2_4bool_6string = string_from_utf8(invalid__13)
    var x837 bool = mtmp836._0
    var x838 string = mtmp836._1
    println__T_bool(x837)
    var t922 bool = x838 == ""
    println__T_bool(t922)
    var parts__16 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "hello")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "，")
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(parts__16, "世界")
    var t923 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(parts__16)
    var t924 bool = t923 >= 3
    println__T_bool(t924)
    var t925 string = string_concat(parts__16)
    println__T_string(t925)
    var values__17 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__i32(1)
    var t926 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__i32(values__17)
    println__T_isize(t926)
    var t927 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__i32(values__17)
    var t928 bool = t927 >= 1
    println__T_bool(t928)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__i32(values__17, 100)
    var t929 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__i32(values__17)
    var t930 bool = t929 >= 100
    println__T_bool(t930)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(values__17, 1)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(values__17, 2)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(values__17, 3)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__i32(values__17, 1, 9)
    var t931 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__i32(values__17)
    println__T_isize(t931)
    var t932 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__i32(values__17, 2)
    println__T_i32(t932)
    var t933 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__i32(values__17, 0)
    println__T_i32(t933)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__i32(values__17)
    var t934 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__i32(values__17, 0)
    println__T_i32(t934)
    var t935 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__i32(values__17, 1)
    println__T_i32(t935)
    var mtmp860 Option__i32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__i32(values__17)
    switch mtmp860._tag {
    case 0:
        var inline1537 int = -1
        var inline1538 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline1537)
        _goml_runtime_core_string_println(inline1538)
    case 1:
        var x861 int32 = mtmp860._v1_0
        var inline1541 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x861)
        _goml_runtime_core_string_println(inline1541)
    default:
        panic("non-exhaustive match")
    }
    var mtmp863 Option__i32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__i32(values__17)
    switch mtmp863._tag {
    case 0:
        var inline1544 int = -1
        var inline1545 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(inline1544)
        _goml_runtime_core_string_println(inline1545)
    case 1:
        var x864 int32 = mtmp863._v1_0
        var inline1548 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x864)
        _goml_runtime_core_string_println(inline1548)
    default:
        panic("non-exhaustive match")
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__i32(values__17, 0)
    var t938 bool = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__i32(values__17)
    println__T_bool(t938)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(values__17, 42)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__i32(values__17)
    var t939 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__i32(values__17)
    println__T_isize(t939)
    var map__20 *hashmap_string_int32_x = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__i32()
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__i32(map__20, "a", 1)
    _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__i32(map__20, "b", 2)
    var entries__21 *_goml_vec_Tuple2_6string_5int32 = _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_entries____K__string____V__i32(map__20)
    var t940 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_i32_q_(entries__21)
    println__T_isize(t940)
    var inline1581 string = "c"
    var inline1582 int32 = 3
    hashmap_set__HashMap_6string_5int32(map__20, inline1581, inline1582)
    var inline1578 string = "a"
    hashmap_remove__HashMap_6string_5int32(map__20, inline1578)
    var t941 int
    var inline1576 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    t941 = inline1576
    var inline1573 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t941)
    _goml_runtime_core_string_println(inline1573)
    var seen_a__22 *ref_bool_x
    var inline1570 bool = false
    var inline1571 *ref_bool_x = ref__Ref_4bool(inline1570)
    seen_a__22 = inline1571
    var seen_b__23 *ref_bool_x
    var inline1567 bool = false
    var inline1568 *ref_bool_x = ref__Ref_4bool(inline1567)
    seen_b__23 = inline1568
    var for_limit878 int = vec_len__Vec_21Tuple2_6string_5int32(entries__21)
    var for_index879 int = 0
    Loop_loop946:
    for {
        var t947 bool = for_index879 < for_limit878
        if t947 {
            var for_item880 Tuple2_6string_5int32 = vec_get__Vec_21Tuple2_6string_5int32(entries__21, for_index879)
            var t948 int = for_index879 + 1
            for_index879 = t948
            var x883 string = for_item880._0
            var x884 int32 = for_item880._1
            var t959 bool = x883 == "a"
            var jp951 bool
            if t959 {
                var t960 bool = x884 == 1
                jp951 = t960
            } else {
                jp951 = false
            }
            if jp951 {
                var inline1551 bool = true
                ref_set__Ref_4bool(seen_a__22, inline1551)
                continue
            } else {
                var t957 bool = x883 == "b"
                var jp955 bool
                if t957 {
                    var t958 bool = x884 == 2
                    jp955 = t958
                } else {
                    jp955 = false
                }
                if jp955 {
                    var inline1554 bool = true
                    ref_set__Ref_4bool(seen_b__23, inline1554)
                    continue
                } else {
                    continue
                }
            }
        } else {
            break Loop_loop946
        }
    }
    var t943 bool
    var inline1565 bool = ref_get__Ref_4bool(seen_a__22)
    t943 = inline1565
    var inline1562 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t943)
    _goml_runtime_core_string_println(inline1562)
    var t944 bool
    var inline1560 bool = ref_get__Ref_4bool(seen_b__23)
    t944 = inline1560
    var inline1557 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t944)
    _goml_runtime_core_string_println(inline1557)
    return struct{}{}
}

func println__T_char(value__1 rune) struct{} {
    var t979 string
    var inline1587 string = char_to_string(value__1)
    t979 = inline1587
    _goml_runtime_core_string_println(t979)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t993 string
    t993 = value__1
    _goml_runtime_core_string_println(t993)
    return struct{}{}
}

func println__T_isize(value__1 int) struct{} {
    var t1002 string
    var inline1600 string = __goml_builtin_int_to_string(value__1)
    t1002 = inline1600
    _goml_runtime_core_string_println(t1002)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1006 int = _goml_runtime_core_string_len(self__289)
    return t1006
}

func _goml_m_inherent_i_string_i_string_i_len(self__288 string) int {
    var t1009 int = _goml_runtime_core_string_len(self__288)
    return t1009
}

func _goml_m_inherent_i_string_i_string_i_get(self__290 string, index__291 int) rune {
    var inline1602 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__290, index__291)
    var inline1603 bool = inline1602._0
    var inline1604 rune = inline1602._1
    if inline1603 {
        return inline1604
    } else {
        var inline1607 rune = _goml_runtime_core_string_get("", -1)
        return inline1607
    }
}

func println__T_bool(value__1 bool) struct{} {
    var t1014 string
    var inline1609 string = _goml_runtime_core_bool_to_string(value__1)
    t1014 = inline1609
    _goml_runtime_core_string_println(t1014)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_is__char__boundary(self__297 string, index__298 int) bool {
    var t1018 bool = string_is_char_boundary(self__297, index__298)
    return t1018
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline1611 bool = string_is_char_boundary(self__294, start__295)
    var inline1613 bool
    if inline1611 {
        var inline1616 bool = string_is_char_boundary(self__294, end__296)
        inline1613 = inline1616
    } else {
        inline1613 = false
    }
    if inline1613 {
        var inline1614 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline1614
    } else {
        var inline1615 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline1615
    }
}

func _goml_m_inherent_i_string_i_string_i_decode__at(self__299 string, index__300 int) _goml_m_Option_____o_char_c_isize_q_ {
    var mtmp410 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__299, index__300)
    var x411 bool = mtmp410._0
    var x412 rune = mtmp410._1
    var x413 int = mtmp410._2
    if x411 {
        var t1026 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: x412,
            _1: x413,
        }
        var t1027 _goml_m_Option_____o_char_c_isize_q_ = _goml_m_Option_____o_char_c_isize_q_{
            _tag: 1,
            _v1_0: t1026,
        }
        return t1027
    } else {
        return _goml_m_Option_____o_char_c_isize_q_{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__304 string) *_goml_vec_uint8 {
    var t1030 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__304)
    return t1030
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__u8(self__526 *_goml_vec_uint8) int {
    var t1033 int = vec_len__Vec_5uint8(self__526)
    return t1033
}

func println__T_u8(value__1 uint8) struct{} {
    var t1035 string
    var inline1618 string = __goml_builtin_uint8_to_string(value__1)
    t1035 = inline1618
    _goml_runtime_core_string_println(t1035)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__u8(self__521 *_goml_vec_uint8, index__522 int) uint8 {
    var t1039 uint8 = vec_get__Vec_5uint8(self__521, index__522)
    return t1039
}

func string_from_utf8(bytes__277 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp395 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__277)
    var x397 string = mtmp395._1
    var index__279 int = 0
    Loop_loop1044:
    for {
        var t1045 int
        var inline1620 int = _goml_runtime_core_string_len(x397)
        t1045 = inline1620
        var t1046 bool = index__279 < t1045
        if t1046 {
            var mtmp398 Tuple3_4bool_4char_3int = string_decode_utf8_at(x397, index__279)
            var x399 bool = mtmp398._0
            var x401 int = mtmp398._2
            if x399 {
                var compound_old402 int = index__279
                var t1048 int = compound_old402 + x401
                index__279 = t1048
                continue
            } else {
                var t1050 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1050
            }
        } else {
            break Loop_loop1044
        }
    }
    var t1043 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x397,
    }
    return t1043
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__u8() *_goml_vec_uint8 {
    var t1053 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t1053
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__u8(self__511 *_goml_vec_uint8, elem__512 uint8) struct{} {
    vec_push__Vec_5uint8(self__511, elem__512)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__string(capacity__510 int) *_goml_vec_string {
    var t1058 *_goml_vec_string = vec_with_capacity__Vec_6string(capacity__510)
    return t1058
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__511 *_goml_vec_string, elem__512 string) struct{} {
    vec_push__Vec_6string(self__511, elem__512)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__string(self__527 *_goml_vec_string) int {
    var t1063 int = vec_capacity__Vec_6string(self__527)
    return t1063
}

func string_concat(values__242 *_goml_vec_string) string {
    var t1066 string = __goml_builtin_string_concat(values__242)
    return t1066
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_with__capacity____T__i32(capacity__510 int) *_goml_vec_int32 {
    var t1069 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(capacity__510)
    return t1069
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__i32(self__526 *_goml_vec_int32) int {
    var t1072 int = vec_len__Vec_5int32(self__526)
    return t1072
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_capacity____T__i32(self__527 *_goml_vec_int32) int {
    var t1075 int = vec_capacity__Vec_5int32(self__527)
    return t1075
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__i32(self__529 *_goml_vec_int32, additional__530 int) struct{} {
    vec_reserve__Vec_5int32(self__529, additional__530)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__i32(self__511 *_goml_vec_int32, elem__512 int32) struct{} {
    vec_push__Vec_5int32(self__511, elem__512)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_insert____T__i32(self__547 *_goml_vec_int32, index__548 int, value__549 int32) struct{} {
    var len__550 int
    var inline1626 int = vec_len__Vec_5int32(self__547)
    len__550 = inline1626
    var t1082 bool = index__548 == len__550
    if t1082 {
        vec_push__Vec_5int32(self__547, value__549)
        return struct{}{}
    } else {
        vec_get__Vec_5int32(self__547, index__548)
        var t1084 int = len__550 - 1
        var t1085 int32 = vec_get__Vec_5int32(self__547, t1084)
        vec_push__Vec_5int32(self__547, t1085)
        var current__551 int = len__550 - 1
        Loop_loop1088:
        for {
            var t1089 bool = current__551 > index__548
            if t1089 {
                var index606 int = current__551
                vec_get__Vec_5int32(self__547, index606)
                var t1090 int = current__551 - 1
                var value608 int32 = vec_get__Vec_5int32(self__547, t1090)
                vec_set__Vec_5int32(self__547, index606, value608)
                var compound_old610 int = current__551
                var compound_value611 int = 1
                var t1092 int = compound_old610 - compound_value611
                current__551 = t1092
                continue
            } else {
                break Loop_loop1088
            }
        }
        vec_get__Vec_5int32(self__547, index__548)
        vec_set__Vec_5int32(self__547, index__548, value__549)
        return struct{}{}
    }
}

func println__T_i32(value__1 int32) struct{} {
    var t1095 string
    var inline1628 string = __goml_builtin_int32_to_string(value__1)
    t1095 = inline1628
    _goml_runtime_core_string_println(t1095)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_remove____T__i32(self__552 *_goml_vec_int32, index__553 int) int32 {
    var len__554 int
    var inline1632 int = vec_len__Vec_5int32(self__552)
    len__554 = inline1632
    var value__555 int32 = vec_get__Vec_5int32(self__552, index__553)
    var current__556 int = index__553
    Loop_loop1101:
    for {
        var t1102 int = current__556 + 1
        var t1103 bool = t1102 < len__554
        if t1103 {
            var index620 int = current__556
            vec_get__Vec_5int32(self__552, index620)
            var t1104 int = current__556 + 1
            var value622 int32 = vec_get__Vec_5int32(self__552, t1104)
            vec_set__Vec_5int32(self__552, index620, value622)
            var compound_old624 int = current__556
            var compound_value625 int = 1
            var t1106 int = compound_old624 + compound_value625
            current__556 = t1106
            continue
        } else {
            break Loop_loop1101
        }
    }
    var t1100 int = len__554 - 1
    vec_truncate__Vec_5int32(self__552, t1100)
    return value__555
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_swap__remove____T__i32(self__543 *_goml_vec_int32, index__544 int) int32 {
    var len__545 int
    var inline1636 int = vec_len__Vec_5int32(self__543)
    len__545 = inline1636
    var value__546 int32 = vec_get__Vec_5int32(self__543, index__544)
    var t1112 int = index__544 + 1
    var t1113 bool = t1112 < len__545
    if t1113 {
        vec_get__Vec_5int32(self__543, index__544)
        var t1114 int = len__545 - 1
        var value599 int32 = vec_get__Vec_5int32(self__543, t1114)
        vec_set__Vec_5int32(self__543, index__544, value599)
    } else {}
    var t1111 int = len__545 - 1
    vec_truncate__Vec_5int32(self__543, t1111)
    return value__546
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reverse____T__i32(self__557 *_goml_vec_int32) struct{} {
    var left__558 int = 0
    var t1117 int
    var inline1652 int = vec_len__Vec_5int32(self__557)
    t1117 = inline1652
    var right__559 int = t1117 - 1
    Loop_loop1119:
    for {
        var t1120 bool = left__558 < right__559
        if t1120 {
            var inline1638 int32 = vec_get__Vec_5int32(self__557, left__558)
            vec_get__Vec_5int32(self__557, left__558)
            var inline1642 int32 = vec_get__Vec_5int32(self__557, right__559)
            vec_set__Vec_5int32(self__557, left__558, inline1642)
            vec_get__Vec_5int32(self__557, right__559)
            vec_set__Vec_5int32(self__557, right__559, inline1638)
            var compound_old630 int = left__558
            var compound_value631 int = 1
            var t1121 int = compound_old630 + compound_value631
            left__558 = t1121
            var compound_old633 int = right__559
            var compound_value634 int = 1
            var t1123 int = compound_old633 - compound_value634
            right__559 = t1123
            continue
        } else {
            break Loop_loop1119
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__i32(self__521 *_goml_vec_int32, index__522 int) int32 {
    var t1127 int32 = vec_get__Vec_5int32(self__521, index__522)
    return t1127
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_last____T__i32(self__534 *_goml_vec_int32) Option__i32 {
    var len__535 int
    var inline1654 int = vec_len__Vec_5int32(self__534)
    len__535 = inline1654
    var t1132 bool = len__535 == 0
    if t1132 {
        return Option__i32{
            _tag: 0,
        }
    } else {
        var t1133 int = len__535 - 1
        var t1134 int32 = vec_get__Vec_5int32(self__534, t1133)
        var t1135 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: t1134,
        }
        return t1135
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pop____T__i32(self__536 *_goml_vec_int32) Option__i32 {
    var len__537 int
    var inline1658 int = vec_len__Vec_5int32(self__536)
    len__537 = inline1658
    var t1140 bool = len__537 == 0
    if t1140 {
        return Option__i32{
            _tag: 0,
        }
    } else {
        var t1141 int = len__537 - 1
        var value__538 int32 = vec_get__Vec_5int32(self__536, t1141)
        var t1142 int = len__537 - 1
        vec_truncate__Vec_5int32(self__536, t1142)
        var t1143 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: value__538,
        }
        return t1143
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_truncate____T__i32(self__531 *_goml_vec_int32, len__532 int) struct{} {
    vec_truncate__Vec_5int32(self__531, len__532)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_is__empty____T__i32(self__528 *_goml_vec_int32) bool {
    var t1148 int = vec_len__Vec_5int32(self__528)
    var t1149 bool = t1148 == 0
    return t1149
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_clear____T__i32(self__533 *_goml_vec_int32) struct{} {
    var inline1660 int = 0
    vec_truncate__Vec_5int32(self__533, inline1660)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_new____K__string____V__i32() *hashmap_string_int32_x {
    var t1154 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    return t1154
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_set____K__string____V__i32(self__675 *hashmap_string_int32_x, key__676 string, value__677 int32) struct{} {
    hashmap_set__HashMap_6string_5int32(self__675, key__676, value__677)
    return struct{}{}
}

func _goml_m_inherent_i_HashMap_i_HashMap_l_K_c_V_r__i_entries____K__string____V__i32(self__683 *hashmap_string_int32_x) *_goml_vec_Tuple2_6string_5int32 {
    var t1159 *_goml_vec_Tuple2_6string_5int32 = hashmap_entries__HashMap_6string_5int32(self__683)
    return t1159
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T___o_string_c_i32_q_(self__526 *_goml_vec_Tuple2_6string_5int32) int {
    var t1162 int = vec_len__Vec_21Tuple2_6string_5int32(self__526)
    return t1162
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__char(next_fn__507 func() Option__char) FnIterator__char {
    var t1175 FnIterator__char = FnIterator__char{
        next_fn: next_fn__507,
    }
    return t1175
}

func _goml_m_trait__impl_i_ToString_i_char_i_to__string(self__403 rune) string {
    var inline1663 uint32 = uint32(rune(self__403))
    var inline1664 bool = utf8_valid_scalar(inline1663)
    if inline1664 {
        var inline1665 string = _goml_runtime_core_char_to_string(self__403)
        return inline1665
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_FnIterator__hae8fa7fd40ba2a3681525fbecfca8da7_isize_c_char_q_(next_fn__507 func() _goml_m_Option_____o_isize_c_char_q_) _goml_m_FnIterator_____o_isize_c_char_q_ {
    var t1181 _goml_m_FnIterator_____o_isize_c_char_q_ = _goml_m_FnIterator_____o_isize_c_char_q_{
        next_fn: next_fn__507,
    }
    return t1181
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t1186 int64 = int64(int(value__222))
    var inline1668 bool = t1186 < 0
    if inline1668 {
        var inline1669 uint64 = uint64(int64(t1186))
        var inline1670 uint64 = 0 - inline1669
        var inline1671 string = decimal_string(inline1670)
        var inline1672 string = "-" + inline1671
        return inline1672
    } else {
        var inline1673 uint64 = uint64(int64(t1186))
        var inline1674 string = decimal_string(inline1673)
        return inline1674
    }
}

func char_to_string(value__282 rune) string {
    var t1192 uint32 = uint32(rune(value__282))
    var t1193 bool
    var inline1676 bool = t1192 <= 1114111
    if inline1676 {
        var inline1677 bool = t1192 >= 55296
        var inline1679 bool
        if inline1677 {
            var inline1681 bool = t1192 <= 57343
            inline1679 = inline1681
        } else {
            inline1679 = false
        }
        var inline1680 bool = !inline1679
        t1193 = inline1680
    } else {
        t1193 = false
    }
    if t1193 {
        var t1194 string = _goml_runtime_core_char_to_string(value__282)
        return t1194
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline1683 int64 = int64(int(self__404))
    var inline1684 string = signed_decimal_string(inline1683)
    return inline1684
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t1205 string = _goml_runtime_core_bool_to_string(self__401)
    return t1205
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1219 bool = index__269 < 0
    var jp1211 bool
    if t1219 {
        jp1211 = true
    } else {
        var t1220 int
        var inline1686 int = _goml_runtime_core_string_len(value__268)
        t1220 = inline1686
        var t1221 bool = index__269 > t1220
        jp1211 = t1221
    }
    if jp1211 {
        return false
    } else {
        var t1214 int
        var inline1690 int = _goml_runtime_core_string_len(value__268)
        t1214 = inline1690
        var t1215 bool = index__269 == t1214
        if t1215 {
            return true
        } else {
            var t1216 uint8
            var inline1688 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1216 = inline1688
            var t1217_rhs uint8 = 192
            var t1217 uint8 = t1216 & t1217_rhs
            var t1218 bool = t1217 != 128
            return t1218
        }
    }
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t1350 bool = index__259 < 0
    var jp1348 bool
    if t1350 {
        jp1348 = true
    } else {
        var t1351 bool = index__259 >= length__260
        jp1348 = t1351
    }
    if jp1348 {
        var inline1692 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1692
    } else {
        var t1235 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t1235))
        var t1238 bool = first__261 < 128
        if t1238 {
            var inline1694 int = 1
            var inline1695 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline1695._tag {
            case 0:
                var inline1696 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1696
            case 1:
                var inline1697 rune = inline1695._v1_0
                var inline1699 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1697,
                    _2: inline1694,
                }
                return inline1699
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1242 bool = first__261 < 194
            if t1242 {
                var inline1701 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1701
            } else {
                var t1246 bool = first__261 < 224
                if t1246 {
                    var t1259 int = length__260 - index__259
                    var t1260 bool = t1259 < 2
                    if t1260 {
                        var inline1703 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1703
                    } else {
                        var t1248 int = index__259 + 1
                        var t1249 uint8
                        var inline1717 uint8 = _goml_runtime_core_string_byte_get(value__258, t1248)
                        t1249 = inline1717
                        var second__262 uint32 = uint32(uint8(t1249))
                        var t1252 bool
                        var inline1714 bool = second__262 < 128
                        if inline1714 {
                            t1252 = true
                        } else {
                            var inline1715 bool = second__262 > 191
                            t1252 = inline1715
                        }
                        if t1252 {
                            var inline1705 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1705
                        } else {
                            var t1254_rhs uint32 = 31
                            var t1254 uint32 = first__261 & t1254_rhs
                            var t1255_rhs int = 6
                            var t1255 uint32 = t1254 << t1255_rhs
                            var t1256_rhs uint32 = 63
                            var t1256 uint32 = second__262 & t1256_rhs
                            var t1257 uint32 = t1255 | t1256
                            var inline1707 int = 2
                            var inline1708 Option__char = __goml_builtin_char_from_uint32(t1257)
                            switch inline1708._tag {
                            case 0:
                                var inline1709 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1709
                            case 1:
                                var inline1710 rune = inline1708._v1_0
                                var inline1712 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1710,
                                    _2: inline1707,
                                }
                                return inline1712
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1264 bool = first__261 < 240
                    if t1264 {
                        var t1297 int = length__260 - index__259
                        var t1298 bool = t1297 < 3
                        if t1298 {
                            var inline1719 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1719
                        } else {
                            var t1266 int = index__259 + 1
                            var t1267 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1266)
                            var second__263 uint32 = uint32(uint8(t1267))
                            var t1268 int = index__259 + 2
                            var t1269 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1268)
                            var third__264 uint32 = uint32(uint8(t1269))
                            var t1295 bool = utf8_invalid_continuation(second__263)
                            var jp1290 bool
                            if t1295 {
                                jp1290 = true
                            } else {
                                var inline1721 bool = third__264 < 128
                                if inline1721 {
                                    jp1290 = true
                                } else {
                                    var inline1722 bool = third__264 > 191
                                    jp1290 = inline1722
                                }
                            }
                            var jp1284 bool
                            if jp1290 {
                                jp1284 = true
                            } else {
                                var t1293 bool = first__261 == 224
                                if t1293 {
                                    var t1294 bool = second__263 < 160
                                    jp1284 = t1294
                                } else {
                                    jp1284 = false
                                }
                            }
                            var jp1273 bool
                            if jp1284 {
                                jp1273 = true
                            } else {
                                var t1287 bool = first__261 == 237
                                if t1287 {
                                    var t1288 bool = second__263 >= 160
                                    jp1273 = t1288
                                } else {
                                    jp1273 = false
                                }
                            }
                            if jp1273 {
                                var inline1724 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1724
                            } else {
                                var t1275_rhs uint32 = 15
                                var t1275 uint32 = first__261 & t1275_rhs
                                var t1276_rhs int = 12
                                var t1276 uint32 = t1275 << t1276_rhs
                                var t1277_rhs uint32 = 63
                                var t1277 uint32 = second__263 & t1277_rhs
                                var t1278_rhs int = 6
                                var t1278 uint32 = t1277 << t1278_rhs
                                var t1279 uint32 = t1276 | t1278
                                var t1280_rhs uint32 = 63
                                var t1280 uint32 = third__264 & t1280_rhs
                                var t1281 uint32 = t1279 | t1280
                                var inline1726 int = 3
                                var inline1727 Option__char = __goml_builtin_char_from_uint32(t1281)
                                switch inline1727._tag {
                                case 0:
                                    var inline1728 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1728
                                case 1:
                                    var inline1729 rune = inline1727._v1_0
                                    var inline1731 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1729,
                                        _2: inline1726,
                                    }
                                    return inline1731
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1302 bool = first__261 < 245
                        if t1302 {
                            var t1343 int = length__260 - index__259
                            var t1344 bool = t1343 < 4
                            if t1344 {
                                var t1345 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1345
                            } else {
                                var t1304 int = index__259 + 1
                                var t1305 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1304)
                                var second__265 uint32 = uint32(uint8(t1305))
                                var t1306 int = index__259 + 2
                                var t1307 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1306)
                                var third__266 uint32 = uint32(uint8(t1307))
                                var t1308 int = index__259 + 3
                                var t1309 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1308)
                                var fourth__267 uint32 = uint32(uint8(t1309))
                                var t1341 bool = utf8_invalid_continuation(second__265)
                                var jp1339 bool
                                if t1341 {
                                    jp1339 = true
                                } else {
                                    var t1342 bool = utf8_invalid_continuation(third__266)
                                    jp1339 = t1342
                                }
                                var jp1333 bool
                                if jp1339 {
                                    jp1333 = true
                                } else {
                                    var t1340 bool = utf8_invalid_continuation(fourth__267)
                                    jp1333 = t1340
                                }
                                var jp1327 bool
                                if jp1333 {
                                    jp1327 = true
                                } else {
                                    var t1336 bool = first__261 == 240
                                    if t1336 {
                                        var t1337 bool = second__265 < 144
                                        jp1327 = t1337
                                    } else {
                                        jp1327 = false
                                    }
                                }
                                var jp1313 bool
                                if jp1327 {
                                    jp1313 = true
                                } else {
                                    var t1330 bool = first__261 == 244
                                    if t1330 {
                                        var t1331 bool = second__265 > 143
                                        jp1313 = t1331
                                    } else {
                                        jp1313 = false
                                    }
                                }
                                if jp1313 {
                                    var t1314 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1314
                                } else {
                                    var t1315_rhs uint32 = 7
                                    var t1315 uint32 = first__261 & t1315_rhs
                                    var t1316_rhs int = 18
                                    var t1316 uint32 = t1315 << t1316_rhs
                                    var t1317_rhs uint32 = 63
                                    var t1317 uint32 = second__265 & t1317_rhs
                                    var t1318_rhs int = 12
                                    var t1318 uint32 = t1317 << t1318_rhs
                                    var t1319 uint32 = t1316 | t1318
                                    var t1320_rhs uint32 = 63
                                    var t1320 uint32 = third__266 & t1320_rhs
                                    var t1321_rhs int = 6
                                    var t1321 uint32 = t1320 << t1321_rhs
                                    var t1322 uint32 = t1319 | t1321
                                    var t1323_rhs uint32 = 63
                                    var t1323 uint32 = fourth__267 & t1323_rhs
                                    var t1324 uint32 = t1322 | t1323
                                    var t1325 Tuple3_4bool_4char_3int = utf8_valid_decode(t1324, 4)
                                    return t1325
                                }
                            }
                        } else {
                            var t1346 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1346
                        }
                    }
                }
            }
        }
    }
}

func __goml_builtin_string_concat(values__215 *_goml_vec_string) string {
    var length__216 int = 0
    var value_index__217 int = 0
    Loop_loop1371:
    for {
        var t1372 int
        var inline1738 int = vec_len__Vec_6string(values__215)
        t1372 = inline1738
        var t1373 bool = value_index__217 < t1372
        if t1373 {
            var compound_old365 int = length__216
            var t1374 string = vec_get__Vec_6string(values__215, value_index__217)
            var compound_value366 int
            var inline1736 int = _goml_runtime_core_string_len(t1374)
            compound_value366 = inline1736
            var t1375 int = compound_old365 + compound_value366
            length__216 = t1375
            var compound_old368 int = value_index__217
            var compound_value369 int = 1
            var t1377 int = compound_old368 + compound_value369
            value_index__217 = t1377
            continue
        } else {
            break Loop_loop1371
        }
    }
    var bytes__218 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(length__216)
    value_index__217 = 0
    Loop_loop1359:
    for {
        var t1360 int
        var inline1746 int = vec_len__Vec_6string(values__215)
        t1360 = inline1746
        var t1361 bool = value_index__217 < t1360
        if t1361 {
            var value__219 string = vec_get__Vec_6string(values__215, value_index__217)
            var byte_index__220 int = 0
            Loop_loop1365:
            for {
                var t1366 int
                var inline1744 int = _goml_runtime_core_string_len(value__219)
                t1366 = inline1744
                var t1367 bool = byte_index__220 < t1366
                if t1367 {
                    var t1368 uint8
                    var inline1742 uint8 = _goml_runtime_core_string_byte_get(value__219, byte_index__220)
                    t1368 = inline1742
                    vec_push__Vec_5uint8(bytes__218, t1368)
                    var compound_old374 int = byte_index__220
                    var compound_value375 int = 1
                    var t1369 int = compound_old374 + compound_value375
                    byte_index__220 = t1369
                    continue
                } else {
                    break Loop_loop1365
                }
            }
            var compound_old378 int = value_index__217
            var compound_value379 int = 1
            var t1363 int = compound_old378 + compound_value379
            value_index__217 = t1363
            continue
        } else {
            break Loop_loop1359
        }
    }
    var mtmp382 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__218)
    var x384 string = mtmp382._1
    return x384
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__407 int32) string {
    var inline1748 int64 = int64(int32(self__407))
    var inline1749 string = signed_decimal_string(inline1748)
    return inline1749
}

func signed_decimal_string(value__214 int64) string {
    var t1389 bool = value__214 < 0
    if t1389 {
        var t1390 uint64 = uint64(int64(value__214))
        var t1391 uint64 = 0 - t1390
        var t1392 string = decimal_string(t1391)
        var t1393 string = "-" + t1392
        return t1393
    } else {
        var t1394 uint64 = uint64(int64(value__214))
        var t1395 string = decimal_string(t1394)
        return t1395
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t1400 bool = value__257 <= 1114111
    if t1400 {
        var t1404 bool = value__257 >= 55296
        var jp1402 bool
        if t1404 {
            var t1405 bool = value__257 <= 57343
            jp1402 = t1405
        } else {
            jp1402 = false
        }
        var t1403 bool = !jp1402
        return t1403
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1408 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1408
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1411 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1411
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field1810 rune
    var inline1753 bool = utf8_valid_scalar(value__253)
    if inline1753 {
        var inline1754 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline1755 rune = inline1754._1
        commute_field1810 = inline1755
        var t1417 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1810,
            _2: width__254,
        }
        return t1417
    } else {
        var inline1751 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1751
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t1422 bool = value__256 < 128
    if t1422 {
        return true
    } else {
        var t1423 bool = value__256 > 191
        return t1423
    }
}

func __goml_builtin_uint8_to_string(value__228 uint8) string {
    var t1426 uint64 = uint64(uint8(value__228))
    var t1427 string = decimal_string(t1426)
    return t1427
}

func __goml_builtin_int32_to_string(value__225 int32) string {
    var t1433 int64 = int64(int32(value__225))
    var inline1759 bool = t1433 < 0
    if inline1759 {
        var inline1760 uint64 = uint64(int64(t1433))
        var inline1761 uint64 = 0 - inline1760
        var inline1762 string = decimal_string(inline1761)
        var inline1763 string = "-" + inline1762
        return inline1763
    } else {
        var inline1764 uint64 = uint64(int64(t1433))
        var inline1765 string = decimal_string(inline1764)
        return inline1765
    }
}

func decimal_string(value__208 uint64) string {
    var t1457 bool = value__208 == 0
    if t1457 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop1450:
        for {
            var t1451 bool = remaining__210 > 0
            if t1451 {
                var t1452_rhs uint64 = 10
                var t1452 uint64 = remaining__210 % t1452_rhs
                var t1453 uint8 = uint8(uint64(t1452))
                var t1454 uint8 = t1453 + 48
                vec_push__Vec_5uint8(reversed__209, t1454)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t1455 uint64 = compound_old353 / compound_value354
                remaining__210 = t1455
                continue
            } else {
                break Loop_loop1450
            }
        }
        var t1439 int
        var inline1775 int = vec_len__Vec_5uint8(reversed__209)
        t1439 = inline1775
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t1439)
        var offset__212 int = 0
        Loop_loop1441:
        for {
            var t1442 int
            var inline1773 int = vec_len__Vec_5uint8(reversed__209)
            t1442 = inline1773
            var t1443 bool = offset__212 < t1442
            if t1443 {
                var t1444 int
                var inline1771 int = vec_len__Vec_5uint8(reversed__209)
                t1444 = inline1771
                var t1445 int = t1444 - offset__212
                var t1446 int = t1445 - 1
                var t1447 uint8 = vec_get__Vec_5uint8(reversed__209, t1446)
                vec_push__Vec_5uint8(bytes__211, t1447)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t1448 int = compound_old358 + compound_value359
                offset__212 = t1448
                continue
            } else {
                break Loop_loop1441
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t1462 bool
    var inline1777 bool = value__283 <= 1114111
    if inline1777 {
        var inline1778 bool = value__283 >= 55296
        var inline1780 bool
        if inline1778 {
            var inline1782 bool = value__283 <= 57343
            inline1780 = inline1782
        } else {
            inline1780 = false
        }
        var inline1781 bool = !inline1780
        t1462 = inline1781
    } else {
        t1462 = false
    }
    if t1462 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t1463 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t1463
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_h3f9733c4625dbd2f543c79fa467f2508_hars__0_i_apply(env887 closure_env_inherent_string_string_chars_0) Option__char {
    var self__305 string = env887.self_0
    var index__306 *ref_int_x = env887.index_1
    var t1485 int = ref_get__Ref_3int(index__306)
    var commute_field1813 Tuple2_4char_3int
    var inline1784 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__305, t1485)
    var inline1785 bool = inline1784._0
    var inline1786 rune = inline1784._1
    var inline1787 int = inline1784._2
    if inline1785 {
        var inline1791 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1786,
            _1: inline1787,
        }
        commute_field1813 = inline1791
        var x417 rune = commute_field1813._0
        var x418 int = commute_field1813._1
        var compound_old419 int = ref_get__Ref_3int(index__306)
        var t1488 int = compound_old419 + x418
        ref_set__Ref_3int(index__306, t1488)
        var t1490 Option__char = Option__char{
            _tag: 1,
            _v1_0: x417,
        }
        return t1490
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_closure__en_hf9055ebad38fcb339d5a880925418115_ices__1_i_apply(env888 closure_env_inherent_string_string_char_indices_1) _goml_m_Option_____o_isize_c_char_q_ {
    var index__311 *ref_int_x = env888.index_0
    var self__310 string = env888.self_1
    var current__312 int = ref_get__Ref_3int(index__311)
    var commute_field1816 Tuple2_4char_3int
    var inline1794 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__310, current__312)
    var inline1795 bool = inline1794._0
    var inline1796 rune = inline1794._1
    var inline1797 int = inline1794._2
    if inline1795 {
        var inline1801 Tuple2_4char_3int = Tuple2_4char_3int{
            _0: inline1796,
            _1: inline1797,
        }
        commute_field1816 = inline1801
        var x425 rune = commute_field1816._0
        var x426 int = commute_field1816._1
        var t1495 int = current__312 + x426
        ref_set__Ref_3int(index__311, t1495)
        var t1496 Tuple2_3int_4char = Tuple2_3int_4char{
            _0: current__312,
            _1: x425,
        }
        var t1497 _goml_m_Option_____o_isize_c_char_q_ = _goml_m_Option_____o_isize_c_char_q_{
            _tag: 1,
            _v1_0: t1496,
        }
        return t1497
    } else {
        return _goml_m_Option_____o_isize_c_char_q_{
            _tag: 0,
        }
    }
}

func main() {
    main0()
}
