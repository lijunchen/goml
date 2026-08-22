package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
}

func array_set__Array_2_3int(arr [2]int, index int, value int) [2]int {
    arr[index] = value
    return arr
}

func array_get__Array_3_3int(arr [3]int, index int) int {
    return arr[index]
}

func array_set__Array_3_3int(arr [3]int, index int, value int) [3]int {
    arr[index] = value
    return arr
}

func array_get__Array_2_5int32(arr [2]int32, index int) int32 {
    return arr[index]
}

func array_set__Array_2_5int32(arr [2]int32, index int, value int32) [2]int32 {
    arr[index] = value
    return arr
}

func array_get__Array_2_14Array_2_5int32(arr [2][2]int32, index int) [2]int32 {
    return arr[index]
}

func array_set__Array_2_14Array_2_5int32(arr [2][2]int32, index int, value [2]int32) [2][2]int32 {
    arr[index] = value
    return arr
}

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_with_capacity__Vec_3int(capacity int) *_goml_vec_int {
    return &_goml_vec_int{
        items: make([]int, 0, capacity),
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
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

type _goml_vec_Array_2_5int32 struct {
    items [][2]int32
}

func vec_new__Vec_14Array_2_5int32() *_goml_vec_Array_2_5int32 {
    return &_goml_vec_Array_2_5int32{
        items: nil,
    }
}

func vec_with_capacity__Vec_14Array_2_5int32(capacity int) *_goml_vec_Array_2_5int32 {
    return &_goml_vec_Array_2_5int32{
        items: make([][2]int32, 0, capacity),
    }
}

func vec_push__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, elem [2]int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, index int) [2]int32 {
    return vec.items[index]
}

func vec_set__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32, index int, value [2]int32) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_14Array_2_5int32(vec *_goml_vec_Array_2_5int32) int {
    return int(len(vec.items))
}

type ref_Array_2_5int32_x struct {
    value [2]int32
}

func ref__Ref_14Array_2_5int32(value [2]int32) *ref_Array_2_5int32_x {
    return &ref_Array_2_5int32_x{
        value: value,
    }
}

func ref_get__Ref_14Array_2_5int32(reference *ref_Array_2_5int32_x) [2]int32 {
    return reference.value
}

func ref_set__Ref_14Array_2_5int32(reference *ref_Array_2_5int32_x, value [2]int32) struct{} {
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

func hashmap_lookup__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) (int32, bool) {
    if m == nil {
        var zero int32
        return zero, false
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if !found {
        var zero int32
        return zero, false
    }
    var entry hashmap_string_int32_x_entry = m.entries[index]
    if entry.active {
        return entry.value, true
    }
    var zero int32
    return zero, false
}

func hashmap_get__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) Option__i32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_5int32(m, key)
    if ok {
        return Option__i32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__i32{
        _tag: 0,
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

type Tuple2_11Array2_3int_3int struct {
    _0 [2]int
    _1 int
}

type Holder struct {
    data [2]int32
    vecs *_goml_vec_Array_2_5int32
}

type Ordering int32

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func main0() struct{} {
    var t478 [2]int = [2]int{31, 32}
    var t479 int = array_get__Array_2_3int(t478, 1)
    println__T_isize(t479)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t480 int = array_get__Array_3_3int(arr__2, 0)
    println__T_isize(t480)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root414 [3]int = arr2__3
    var index415 int = 1
    array_get__Array_3_3int(place_root414, index415)
    var value417 int = 50
    var t481 [3]int = array_set__Array_3_3int(place_root414, index415, value417)
    arr2__3 = t481
    var t483 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_isize(t483)
    var t484 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize()
    var t485 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__isize(t484, 7)
    var t486 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__isize(t485, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__isize(t486, 9)
    var t487 int = vec_get__Vec_3int(vec__4, 2)
    println__T_isize(t487)
    var t488 *_goml_vec_int32
    var inline711 *_goml_vec_int32 = vec_new__Vec_5int32()
    t488 = inline711
    var t489 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__i32(t488, 10)
    var t490 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__i32(t489, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__i32(t490, 12)
    var index422 int = 0
    vec_get__Vec_5int32(vec2__5, index422)
    var value424 int32 = 100
    vec_set__Vec_5int32(vec2__5, index422, value424)
    var t492 int32 = vec_get__Vec_5int32(vec2__5, 0)
    var inline708 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t492)
    _goml_runtime_core_string_println(inline708)
    var s__6 []int32
    var inline704 int = 0
    var inline705 int = 2
    var inline706 []int32 = vec2__5.items[inline704:inline705]
    s__6 = inline706
    var t493 int32 = s__6[1]
    var inline701 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t493)
    _goml_runtime_core_string_println(inline701)
    var map__7 *hashmap_string_int32_x
    var inline699 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    map__7 = inline699
    var index429 string = "a"
    hashmap_get__HashMap_6string_5int32(map__7, index429)
    var value431 int32 = 13
    hashmap_set__HashMap_6string_5int32(map__7, index429, value431)
    var t495 Option__i32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    switch t495._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline695 int32 = t495._v1_0
        println__T_i32(inline695)
    default:
        panic("non-exhaustive match")
    }
    var t496 Option__i32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    switch t496._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline690 int32 = t496._v1_0
        println__T_i32(inline690)
    default:
        panic("non-exhaustive match")
    }
    var t497 [2]int32 = [2]int32{1, 2}
    var t498 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t497, t498}
    var place_root435 [2][2]int32 = matrix__8
    var index436 int = 1
    var place437 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root435, index436)
    var index438 int = 0
    array_get__Array_2_5int32(place437, index438)
    var value440 int32 = 30
    var t499 [2]int32 = array_set__Array_2_5int32(place437, index438, value440)
    var t500 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root435, index436, t499)
    matrix__8 = t500
    var t502 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t503 int32 = array_get__Array_2_5int32(t502, 0)
    var inline686 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t503)
    _goml_runtime_core_string_println(inline686)
    var t504 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t504,
        _1: 0,
    }
    var place_root443 Tuple2_11Array2_3int_3int = pair__9
    var place444 [2]int = place_root443._0
    var index445 int = 1
    array_get__Array_2_3int(place444, index445)
    var value447 int = 150
    var t505 [2]int = array_set__Array_2_3int(place444, index445, value447)
    var t506 int = place_root443._1
    var t507 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t505,
        _1: t506,
    }
    pair__9 = t507
    var t509 [2]int = pair__9._0
    var t510 int = array_get__Array_2_3int(t509, 1)
    var inline683 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t510)
    _goml_runtime_core_string_println(inline683)
    var t511 [2]int32 = [2]int32{16, 17}
    var t512 *_goml_vec_Array_2_5int32
    var inline681 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    t512 = inline681
    var t513 [2]int32 = [2]int32{18, 19}
    var t514 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_i32_x3b_2_r_(t512, t513)
    var t515 [2]int32 = [2]int32{20, 21}
    var t516 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_i32_x3b_2_r_(t514, t515)
    var holder__10 Holder = Holder{
        data: t511,
        vecs: t516,
    }
    var place_root450 Holder = holder__10
    var place451 [2]int32 = place_root450.data
    var index452 int = 0
    array_get__Array_2_5int32(place451, index452)
    var value454 int32 = 160
    var t517 [2]int32 = array_set__Array_2_5int32(place451, index452, value454)
    var t518 *_goml_vec_Array_2_5int32 = place_root450.vecs
    var t519 Holder = Holder{
        data: t517,
        vecs: t518,
    }
    holder__10 = t519
    var t521 [2]int32 = holder__10.data
    var t522 int32 = array_get__Array_2_5int32(t521, 0)
    var inline678 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t522)
    _goml_runtime_core_string_println(inline678)
    var place_root457 Holder = holder__10
    var place458 *_goml_vec_Array_2_5int32 = place_root457.vecs
    var index459 int = 1
    var place460 [2]int32 = vec_get__Vec_14Array_2_5int32(place458, index459)
    var index461 int = 0
    array_get__Array_2_5int32(place460, index461)
    var value463 int32 = 200
    var t523 [2]int32 = array_set__Array_2_5int32(place460, index461, value463)
    vec_set__Vec_14Array_2_5int32(place458, index459, t523)
    var t525 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t526 [2]int32 = vec_get__Vec_14Array_2_5int32(t525, 1)
    var t527 int32 = array_get__Array_2_5int32(t526, 0)
    var inline675 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t527)
    _goml_runtime_core_string_println(inline675)
    var t528 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x
    var inline673 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(t528)
    r__11 = inline673
    var place_root467 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    var index468 int = 1
    array_get__Array_2_5int32(place_root467, index468)
    var value470 int32 = 230
    var t529 [2]int32 = array_set__Array_2_5int32(place_root467, index468, value470)
    ref_set__Ref_14Array_2_5int32(r__11, t529)
    var t531 [2]int32
    var inline671 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    t531 = inline671
    var t532 int32 = array_get__Array_2_5int32(t531, 1)
    var inline668 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t532)
    _goml_runtime_core_string_println(inline668)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t534 string
    t534 = value__1
    _goml_runtime_core_string_println(t534)
    return struct{}{}
}

func println__T_i32(value__1 int32) struct{} {
    var t537 string
    var inline714 string = _goml_runtime_core_int32_to_string(value__1)
    t537 = inline714
    _goml_runtime_core_string_println(t537)
    return struct{}{}
}

func println__T_isize(value__1 int) struct{} {
    var t540 string
    var inline716 string = _goml_runtime_core_int_to_string(value__1)
    t540 = inline716
    _goml_runtime_core_string_println(t540)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__isize() *_goml_vec_int {
    var t544 *_goml_vec_int = vec_new__Vec_3int()
    return t544
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__isize(self__260 *_goml_vec_int, elem__261 int) *_goml_vec_int {
    var t547 int
    var inline726 int = vec_len__Vec_3int(self__260)
    t547 = inline726
    var t548 int = t547 + 1
    var result__262 *_goml_vec_int
    var inline724 *_goml_vec_int = vec_with_capacity__Vec_3int(t548)
    result__262 = inline724
    var index__263 int = 0
    Loop_loop550:
    for {
        var t551 int
        var inline720 int = vec_len__Vec_3int(self__260)
        t551 = inline720
        var t552 bool = index__263 < t551
        if t552 {
            var t553 int = vec_get__Vec_3int(self__260, index__263)
            vec_push__Vec_3int(result__262, t553)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t554 int = compound_old190 + compound_value191
            index__263 = t554
            continue
        } else {
            break Loop_loop550
        }
    }
    vec_push__Vec_3int(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__i32(self__260 *_goml_vec_int32, elem__261 int32) *_goml_vec_int32 {
    var t561 int
    var inline736 int = vec_len__Vec_5int32(self__260)
    t561 = inline736
    var t562 int = t561 + 1
    var result__262 *_goml_vec_int32
    var inline734 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t562)
    result__262 = inline734
    var index__263 int = 0
    Loop_loop564:
    for {
        var t565 int
        var inline730 int = vec_len__Vec_5int32(self__260)
        t565 = inline730
        var t566 bool = index__263 < t565
        if t566 {
            var t567 int32 = vec_get__Vec_5int32(self__260, index__263)
            vec_push__Vec_5int32(result__262, t567)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t568 int = compound_old190 + compound_value191
            index__263 = t568
            continue
        } else {
            break Loop_loop564
        }
    }
    vec_push__Vec_5int32(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_i32_x3b_2_r_(self__260 *_goml_vec_Array_2_5int32, elem__261 [2]int32) *_goml_vec_Array_2_5int32 {
    var t581 int
    var inline746 int = vec_len__Vec_14Array_2_5int32(self__260)
    t581 = inline746
    var t582 int = t581 + 1
    var result__262 *_goml_vec_Array_2_5int32
    var inline744 *_goml_vec_Array_2_5int32 = vec_with_capacity__Vec_14Array_2_5int32(t582)
    result__262 = inline744
    var index__263 int = 0
    Loop_loop584:
    for {
        var t585 int
        var inline740 int = vec_len__Vec_14Array_2_5int32(self__260)
        t585 = inline740
        var t586 bool = index__263 < t585
        if t586 {
            var t587 [2]int32 = vec_get__Vec_14Array_2_5int32(self__260, index__263)
            vec_push__Vec_14Array_2_5int32(result__262, t587)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t588 int = compound_old190 + compound_value191
            index__263 = t588
            continue
        } else {
            break Loop_loop584
        }
    }
    vec_push__Vec_14Array_2_5int32(result__262, elem__261)
    return result__262
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t600 string = _goml_runtime_core_int32_to_string(self__154)
    return t600
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t603 string = _goml_runtime_core_int_to_string(self__151)
    return t603
}

func main() {
    main0()
}
