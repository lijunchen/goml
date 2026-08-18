package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
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
        items: _goml_slices.Grow([]int{}, int(capacity)),
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
        items: _goml_slices.Grow([][2]int32{}, int(capacity)),
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

func hashmap_get__HashMap_6string_5int32(m *hashmap_string_int32_x, key string) Option__int32 {
    var value int32
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_5int32(m, key)
    if ok {
        return Option__int32{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__int32{
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

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func main0() struct{} {
    var t475 [2]int = [2]int{31, 32}
    var t476 int = array_get__Array_2_3int(t475, 1)
    println__T_int(t476)
    var arr__2 [3]int = [3]int{1, 2, 3}
    var t477 int = array_get__Array_3_3int(arr__2, 0)
    println__T_int(t477)
    var arr2__3 [3]int = [3]int{4, 5, 6}
    var place_root411 [3]int = arr2__3
    var index412 int = 1
    array_get__Array_3_3int(place_root411, index412)
    var value414 int = 50
    var t478 [3]int = array_set__Array_3_3int(place_root411, index412, value414)
    arr2__3 = t478
    var t480 int = array_get__Array_3_3int(arr2__3, 1)
    println__T_int(t480)
    var t481 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t482 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t481, 7)
    var t483 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t482, 8)
    var vec__4 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(t483, 9)
    var t484 int = vec_get__Vec_3int(vec__4, 2)
    println__T_int(t484)
    var t485 *_goml_vec_int32
    var inline708 *_goml_vec_int32 = vec_new__Vec_5int32()
    t485 = inline708
    var t486 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t485, 10)
    var t487 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t486, 11)
    var vec2__5 *_goml_vec_int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(t487, 12)
    var index419 int = 0
    vec_get__Vec_5int32(vec2__5, index419)
    var value421 int32 = 100
    vec_set__Vec_5int32(vec2__5, index419, value421)
    var t489 int32 = vec_get__Vec_5int32(vec2__5, 0)
    var inline705 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t489)
    _goml_runtime_core_string_println(inline705)
    var s__6 []int32
    var inline701 int = 0
    var inline702 int = 2
    var inline703 []int32 = vec2__5.items[inline701:inline702]
    s__6 = inline703
    var t490 int32 = s__6[1]
    var inline698 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t490)
    _goml_runtime_core_string_println(inline698)
    var map__7 *hashmap_string_int32_x
    var inline696 *hashmap_string_int32_x = hashmap_new__HashMap_6string_5int32()
    map__7 = inline696
    var index426 string = "a"
    hashmap_get__HashMap_6string_5int32(map__7, index426)
    var value428 int32 = 13
    hashmap_set__HashMap_6string_5int32(map__7, index426, value428)
    var t492 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "a")
    switch t492._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline692 int32 = t492._v1_0
        println__T_int32(inline692)
    default:
        panic("non-exhaustive match")
    }
    var t493 Option__int32 = hashmap_get__HashMap_6string_5int32(map__7, "missing")
    switch t493._tag {
    case 0:
        println__T_string("none")
    case 1:
        var inline687 int32 = t493._v1_0
        println__T_int32(inline687)
    default:
        panic("non-exhaustive match")
    }
    var t494 [2]int32 = [2]int32{1, 2}
    var t495 [2]int32 = [2]int32{3, 4}
    var matrix__8 [2][2]int32 = [2][2]int32{t494, t495}
    var place_root432 [2][2]int32 = matrix__8
    var index433 int = 1
    var place434 [2]int32 = array_get__Array_2_14Array_2_5int32(place_root432, index433)
    var index435 int = 0
    array_get__Array_2_5int32(place434, index435)
    var value437 int32 = 30
    var t496 [2]int32 = array_set__Array_2_5int32(place434, index435, value437)
    var t497 [2][2]int32 = array_set__Array_2_14Array_2_5int32(place_root432, index433, t496)
    matrix__8 = t497
    var t499 [2]int32 = array_get__Array_2_14Array_2_5int32(matrix__8, 1)
    var t500 int32 = array_get__Array_2_5int32(t499, 0)
    var inline683 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t500)
    _goml_runtime_core_string_println(inline683)
    var t501 [2]int = [2]int{14, 15}
    var pair__9 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t501,
        _1: 0,
    }
    var place_root440 Tuple2_11Array2_3int_3int = pair__9
    var place441 [2]int = place_root440._0
    var index442 int = 1
    array_get__Array_2_3int(place441, index442)
    var value444 int = 150
    var t502 [2]int = array_set__Array_2_3int(place441, index442, value444)
    var t503 int = place_root440._1
    var t504 Tuple2_11Array2_3int_3int = Tuple2_11Array2_3int_3int{
        _0: t502,
        _1: t503,
    }
    pair__9 = t504
    var t506 [2]int = pair__9._0
    var t507 int = array_get__Array_2_3int(t506, 1)
    var inline680 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t507)
    _goml_runtime_core_string_println(inline680)
    var t508 [2]int32 = [2]int32{16, 17}
    var t509 *_goml_vec_Array_2_5int32
    var inline678 *_goml_vec_Array_2_5int32 = vec_new__Vec_14Array_2_5int32()
    t509 = inline678
    var t510 [2]int32 = [2]int32{18, 19}
    var t511 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t509, t510)
    var t512 [2]int32 = [2]int32{20, 21}
    var t513 *_goml_vec_Array_2_5int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(t511, t512)
    var holder__10 Holder = Holder{
        data: t508,
        vecs: t513,
    }
    var place_root447 Holder = holder__10
    var place448 [2]int32 = place_root447.data
    var index449 int = 0
    array_get__Array_2_5int32(place448, index449)
    var value451 int32 = 160
    var t514 [2]int32 = array_set__Array_2_5int32(place448, index449, value451)
    var t515 *_goml_vec_Array_2_5int32 = place_root447.vecs
    var t516 Holder = Holder{
        data: t514,
        vecs: t515,
    }
    holder__10 = t516
    var t518 [2]int32 = holder__10.data
    var t519 int32 = array_get__Array_2_5int32(t518, 0)
    var inline675 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t519)
    _goml_runtime_core_string_println(inline675)
    var place_root454 Holder = holder__10
    var place455 *_goml_vec_Array_2_5int32 = place_root454.vecs
    var index456 int = 1
    var place457 [2]int32 = vec_get__Vec_14Array_2_5int32(place455, index456)
    var index458 int = 0
    array_get__Array_2_5int32(place457, index458)
    var value460 int32 = 200
    var t520 [2]int32 = array_set__Array_2_5int32(place457, index458, value460)
    vec_set__Vec_14Array_2_5int32(place455, index456, t520)
    var t522 *_goml_vec_Array_2_5int32 = holder__10.vecs
    var t523 [2]int32 = vec_get__Vec_14Array_2_5int32(t522, 1)
    var t524 int32 = array_get__Array_2_5int32(t523, 0)
    var inline672 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t524)
    _goml_runtime_core_string_println(inline672)
    var t525 [2]int32 = [2]int32{22, 23}
    var r__11 *ref_Array_2_5int32_x
    var inline670 *ref_Array_2_5int32_x = ref__Ref_14Array_2_5int32(t525)
    r__11 = inline670
    var place_root464 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    var index465 int = 1
    array_get__Array_2_5int32(place_root464, index465)
    var value467 int32 = 230
    var t526 [2]int32 = array_set__Array_2_5int32(place_root464, index465, value467)
    ref_set__Ref_14Array_2_5int32(r__11, t526)
    var t528 [2]int32
    var inline668 [2]int32 = ref_get__Ref_14Array_2_5int32(r__11)
    t528 = inline668
    var t529 int32 = array_get__Array_2_5int32(t528, 1)
    var inline665 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t529)
    _goml_runtime_core_string_println(inline665)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t531 string
    t531 = value__1
    _goml_runtime_core_string_println(t531)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t534 string
    var inline711 string = _goml_runtime_core_int32_to_string(value__1)
    t534 = inline711
    _goml_runtime_core_string_println(t534)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t537 string
    var inline713 string = _goml_runtime_core_int_to_string(value__1)
    t537 = inline713
    _goml_runtime_core_string_println(t537)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t541 *_goml_vec_int = vec_new__Vec_3int()
    return t541
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int(self__260 *_goml_vec_int, elem__261 int) *_goml_vec_int {
    var t544 int
    var inline723 int = vec_len__Vec_3int(self__260)
    t544 = inline723
    var t545 int = t544 + 1
    var result__262 *_goml_vec_int
    var inline721 *_goml_vec_int = vec_with_capacity__Vec_3int(t545)
    result__262 = inline721
    var index__263 int = 0
    Loop_loop547:
    for {
        var t548 int
        var inline717 int = vec_len__Vec_3int(self__260)
        t548 = inline717
        var t549 bool = index__263 < t548
        if t549 {
            var t550 int = vec_get__Vec_3int(self__260, index__263)
            vec_push__Vec_3int(result__262, t550)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t551 int = compound_old190 + compound_value191
            index__263 = t551
            continue
        } else {
            break Loop_loop547
        }
    }
    vec_push__Vec_3int(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__int32(self__260 *_goml_vec_int32, elem__261 int32) *_goml_vec_int32 {
    var t558 int
    var inline733 int = vec_len__Vec_5int32(self__260)
    t558 = inline733
    var t559 int = t558 + 1
    var result__262 *_goml_vec_int32
    var inline731 *_goml_vec_int32 = vec_with_capacity__Vec_5int32(t559)
    result__262 = inline731
    var index__263 int = 0
    Loop_loop561:
    for {
        var t562 int
        var inline727 int = vec_len__Vec_5int32(self__260)
        t562 = inline727
        var t563 bool = index__263 < t562
        if t563 {
            var t564 int32 = vec_get__Vec_5int32(self__260, index__263)
            vec_push__Vec_5int32(result__262, t564)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t565 int = compound_old190 + compound_value191
            index__263 = t565
            continue
        } else {
            break Loop_loop561
        }
    }
    vec_push__Vec_5int32(result__262, elem__261)
    return result__262
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T___l_int32_x3b_2_r_(self__260 *_goml_vec_Array_2_5int32, elem__261 [2]int32) *_goml_vec_Array_2_5int32 {
    var t578 int
    var inline743 int = vec_len__Vec_14Array_2_5int32(self__260)
    t578 = inline743
    var t579 int = t578 + 1
    var result__262 *_goml_vec_Array_2_5int32
    var inline741 *_goml_vec_Array_2_5int32 = vec_with_capacity__Vec_14Array_2_5int32(t579)
    result__262 = inline741
    var index__263 int = 0
    Loop_loop581:
    for {
        var t582 int
        var inline737 int = vec_len__Vec_14Array_2_5int32(self__260)
        t582 = inline737
        var t583 bool = index__263 < t582
        if t583 {
            var t584 [2]int32 = vec_get__Vec_14Array_2_5int32(self__260, index__263)
            vec_push__Vec_14Array_2_5int32(result__262, t584)
            var compound_old190 int = index__263
            var compound_value191 int = 1
            var t585 int = compound_old190 + compound_value191
            index__263 = t585
            continue
        } else {
            break Loop_loop581
        }
    }
    vec_push__Vec_14Array_2_5int32(result__262, elem__261)
    return result__262
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t597 string = _goml_runtime_core_int32_to_string(self__154)
    return t597
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t600 string = _goml_runtime_core_int_to_string(self__151)
    return t600
}

func main() {
    main0()
}
