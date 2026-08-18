package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_int struct {
    items []int
}

func vec_get__Vec_3int(vec *_goml_vec_int, index int) int {
    return vec.items[index]
}

func vec_set__Vec_3int(vec *_goml_vec_int, index int, value int) struct{} {
    vec.items[index] = value
    return struct{}{}
}

func vec_len__Vec_3int(vec *_goml_vec_int) int {
    return int(len(vec.items))
}

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type hashmap_string_int_x_entry struct {
    active bool
    key string
    value int
}

type hashmap_string_int_x struct {
    indices map[string]int
    entries []hashmap_string_int_x_entry
    len int
}

func hashmap_new__HashMap_6string_3int() *hashmap_string_int_x {
    return &hashmap_string_int_x{
        indices: make(map[string]int),
        entries: nil,
        len: 0,
    }
}

func hashmap_len__HashMap_6string_3int(m *hashmap_string_int_x) int {
    if m == nil {
        return 0
    }
    return m.len
}

func hashmap_lookup__HashMap_6string_3int(m *hashmap_string_int_x, key string) (int, bool) {
    if m == nil {
        var zero int
        return zero, false
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if !found {
        var zero int
        return zero, false
    }
    var entry hashmap_string_int_x_entry = m.entries[index]
    if entry.active {
        return entry.value, true
    }
    var zero int
    return zero, false
}

func hashmap_get__HashMap_6string_3int(m *hashmap_string_int_x, key string) Option__int {
    var value int
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_3int(m, key)
    if ok {
        return Option__int{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__int{
        _tag: 0,
    }
}

func hashmap_set__HashMap_6string_3int(m *hashmap_string_int_x, key string, value int) struct{} {
    if m == nil {
        return struct{}{}
    }
    var index int
    var found bool
    index, found = m.indices[key]
    if found {
        var entry hashmap_string_int_x_entry = m.entries[index]
        if entry.active {
            m.entries[index].value = value
            return struct{}{}
        }
        m.entries[index] = hashmap_string_int_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    index = len(m.entries)
    m.indices[key] = index
    m.entries = append(m.entries, hashmap_string_int_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

type Tuple2_6string_3int struct {
    _0 string
    _1 int
}

type Point struct {
    x int
    y int
}

type Ordering int32

type Option__int struct {
    _tag int32
    _v1_0 int
}

func record(log__0 *ref_string_x, label__1 string, value__2 int) int {
    var t449 string
    var inline573 string = ref_get__Ref_6string(log__0)
    t449 = inline573
    var t450 string = t449 + label__1
    ref_set__Ref_6string(log__0, t450)
    return value__2
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var t453 string
    var inline577 string = ref_get__Ref_6string(log__3)
    t453 = inline577
    var t454 string = t453 + label__4
    ref_set__Ref_6string(log__3, t454)
    return value__5
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var t457 string
    var inline581 string = ref_get__Ref_6string(log__6)
    t457 = inline581
    var t458 string = t457 + label__7
    ref_set__Ref_6string(log__6, t458)
    return value__8
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old411 int = number__9
    var compound_value412 int = 3
    var t460 int = compound_old411 + compound_value412
    number__9 = t460
    var compound_old414 int = number__9
    var compound_value415 int = 2
    var t462 int = compound_old414 * compound_value415
    number__9 = t462
    var compound_old417 int = number__9
    var compound_value418 int = 1
    var t464 int = compound_old417 >> compound_value418
    number__9 = t464
    var t466 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    println__T_string(t466)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root421 Point = direct__10
    var place422 int = place_root421.x
    var value423 int = 5
    var t467 int = place422 + value423
    var t468 int = place_root421.y
    var t469 Point = Point{
        x: t467,
        y: t468,
    }
    direct__10 = t469
    var t471 int = direct__10.x
    var t472 string = _goml_m_inherent_i_int_i_int_i_to__string(t471)
    var t473 string = "" + t472
    var t474 string = t473 + ","
    var t475 int = direct__10.y
    var t476 string = _goml_m_inherent_i_int_i_int_i_to__string(t475)
    var t477 string = t474 + t476
    println__T_string(t477)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root426 Tuple2_3int_3int = pair__11
    var place427 int = place_root426._0
    var value428 int = 3
    var t478 int = place427 * value428
    var t479 int = place_root426._1
    var t480 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t478,
        _1: t479,
    }
    pair__11 = t480
    var t482 int = pair__11._0
    var t483 string = _goml_m_inherent_i_int_i_int_i_to__string(t482)
    var t484 string = "" + t483
    var t485 string = t484 + ","
    var t486 int = pair__11._1
    var t487 string = _goml_m_inherent_i_int_i_int_i_to__string(t486)
    var t488 string = t485 + t487
    println__T_string(t488)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__777__0 int = record(log__12, "F", 7)
    var struct_update_base__777 Point = record_point(log__12, "B", base__13)
    var t489 int = struct_update_base__777.y
    var t491 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t491)
    var t493 string = _goml_m_inherent_i_int_i_int_i_to__string(struct_update_field__777__0)
    var t494 string = "" + t493
    var t495 string = t494 + ","
    var t497 string = _goml_m_inherent_i_int_i_int_i_to__string(t489)
    var t498 string = t495 + t497
    println__T_string(t498)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var t499 int = record(log__12, "A", 10)
    var t500 int = record(log__12, "B", 20)
    var t501 [2]int = [2]int{t499, t500}
    var values__15 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t501)
    var place_root434 *_goml_vec_int = record_vec(log__12, "R", values__15)
    var index435 int = record(log__12, "I", 1)
    var place436 int = vec_get__Vec_3int(place_root434, index435)
    var value437 int = record(log__12, "V", 5)
    var t502 int = place436 + value437
    vec_set__Vec_3int(place_root434, index435, t502)
    var t504 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t504)
    var t505 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(values__15, 0)
    var t506 string = _goml_m_inherent_i_int_i_int_i_to__string(t505)
    var t507 string = "" + t506
    var t508 string = t507 + ","
    var t509 int
    var inline639 int = 1
    var inline640 int = vec_get__Vec_3int(values__15, inline639)
    t509 = inline640
    var t510 string
    var inline637 string = _goml_runtime_core_int_to_string(t509)
    t510 = inline637
    var t511 string = t508 + t510
    var inline634 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t511)
    _goml_runtime_core_string_println(inline634)
    var inline631 string = ""
    ref_set__Ref_6string(log__12, inline631)
    var t512 string = "" + "k"
    var t513 int
    var inline625 string = "K"
    var inline626 int = 1
    var inline627 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline628 string = inline627 + inline625
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline628)
    t513 = inline626
    var t514 string
    var inline623 string = _goml_runtime_core_int_to_string(t513)
    t514 = inline623
    var t515 string = t512 + t514
    var t516 int
    var inline617 string = "V"
    var inline618 int = 11
    var inline619 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline620 string = inline619 + inline617
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline620)
    t516 = inline618
    var t517 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: t515,
        _1: t516,
    }
    var t518 int
    var inline611 string = "A"
    var inline612 int = 1
    var inline613 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline614 string = inline613 + inline611
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline614)
    t518 = inline612
    var t519 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "same",
        _1: t518,
    }
    var t520 int
    var inline605 string = "B"
    var inline606 int = 2
    var inline607 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline608 string = inline607 + inline605
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline608)
    t520 = inline606
    var t521 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "same",
        _1: t520,
    }
    var t522 [3]Tuple2_6string_3int = [3]Tuple2_6string_3int{t517, t519, t521}
    var table__16 *hashmap_string_int_x = func(values [3]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }(t522)
    var t523 string
    var inline603 string = ref_get__Ref_6string(log__12)
    t523 = inline603
    var inline600 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t523)
    _goml_runtime_core_string_println(inline600)
    var mtmp443 Option__int
    var inline597 string = "same"
    var inline598 Option__int = hashmap_get__HashMap_6string_3int(table__16, inline597)
    mtmp443 = inline598
    var jp525 string
    switch mtmp443._tag {
    case 0:
        jp525 = "missing"
    case 1:
        var x444 int = mtmp443._v1_0
        var inline583 string = _goml_runtime_core_int_to_string(x444)
        jp525 = inline583
    default:
        panic("non-exhaustive match")
    }
    var inline594 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp525)
    _goml_runtime_core_string_println(inline594)
    var t526 [0]int = [0]int{}
    var empty_values__18 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t526)
    var t527 [0]Tuple2_6string_3int = [0]Tuple2_6string_3int{}
    var empty_table__19 *hashmap_string_int_x = func(values [0]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }(t527)
    var t528 string = "" + "empty="
    var t529 int
    var inline592 int = vec_len__Vec_3int(empty_values__18)
    t529 = inline592
    var t530 int
    var inline590 int = hashmap_len__HashMap_6string_3int(empty_table__19)
    t530 = inline590
    var t531 int = t529 + t530
    var t532 string
    var inline588 string = _goml_runtime_core_int_to_string(t531)
    t532 = inline588
    var t533 string = t528 + t532
    var t534 string = t533 + " {ok}"
    var inline585 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t534)
    _goml_runtime_core_string_println(inline585)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__432 *ref_string_x) string {
    var t538 string = ref_get__Ref_6string(self__432)
    return t538
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__433 *ref_string_x, value__434 string) struct{} {
    ref_set__Ref_6string(self__433, value__434)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t542 string
    t542 = value__1
    _goml_runtime_core_string_println(t542)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t546 string = _goml_runtime_core_int_to_string(self__32)
    return t546
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__431 string) *ref_string_x {
    var t549 *ref_string_x = ref__Ref_6string(value__431)
    return t549
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__268 *_goml_vec_int, index__269 int) int {
    var t552 int = vec_get__Vec_3int(self__268, index__269)
    return t552
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
