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

func hashmap_get__HashMap_6string_3int(m *hashmap_string_int_x, key string) Option__isize {
    var value int
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_3int(m, key)
    if ok {
        return Option__isize{
            _tag: 1,
            _v1_0: value,
        }
    }
    return Option__isize{
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

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func record(log__0 *ref_string_x, label__1 string, value__2 int) int {
    var t452 string
    var inline576 string = ref_get__Ref_6string(log__0)
    t452 = inline576
    var t453 string = t452 + label__1
    ref_set__Ref_6string(log__0, t453)
    return value__2
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var t456 string
    var inline580 string = ref_get__Ref_6string(log__3)
    t456 = inline580
    var t457 string = t456 + label__4
    ref_set__Ref_6string(log__3, t457)
    return value__5
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var t460 string
    var inline584 string = ref_get__Ref_6string(log__6)
    t460 = inline584
    var t461 string = t460 + label__7
    ref_set__Ref_6string(log__6, t461)
    return value__8
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old414 int = number__9
    var compound_value415 int = 3
    var t463 int = compound_old414 + compound_value415
    number__9 = t463
    var compound_old417 int = number__9
    var compound_value418 int = 2
    var t465 int = compound_old417 * compound_value418
    number__9 = t465
    var compound_old420 int = number__9
    var compound_value421 int = 1
    var t467 int = compound_old420 >> compound_value421
    number__9 = t467
    var t469 string = _goml_m_inherent_i_isize_i_isize_i_to__string(number__9)
    println__T_string(t469)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root424 Point = direct__10
    var place425 int = place_root424.x
    var value426 int = 5
    var t470 int = place425 + value426
    var t471 int = place_root424.y
    var t472 Point = Point{
        x: t470,
        y: t471,
    }
    direct__10 = t472
    var t474 int = direct__10.x
    var t475 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t474)
    var t476 string = "" + t475
    var t477 string = t476 + ","
    var t478 int = direct__10.y
    var t479 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t478)
    var t480 string = t477 + t479
    println__T_string(t480)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root429 Tuple2_3int_3int = pair__11
    var place430 int = place_root429._0
    var value431 int = 3
    var t481 int = place430 * value431
    var t482 int = place_root429._1
    var t483 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t481,
        _1: t482,
    }
    pair__11 = t483
    var t485 int = pair__11._0
    var t486 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t485)
    var t487 string = "" + t486
    var t488 string = t487 + ","
    var t489 int = pair__11._1
    var t490 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t489)
    var t491 string = t488 + t490
    println__T_string(t491)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__789__0 int = record(log__12, "F", 7)
    var struct_update_base__789 Point = record_point(log__12, "B", base__13)
    var t492 int = struct_update_base__789.y
    var t494 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t494)
    var t496 string = _goml_m_inherent_i_isize_i_isize_i_to__string(struct_update_field__789__0)
    var t497 string = "" + t496
    var t498 string = t497 + ","
    var t500 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t492)
    var t501 string = t498 + t500
    println__T_string(t501)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var t502 int = record(log__12, "A", 10)
    var t503 int = record(log__12, "B", 20)
    var t504 [2]int = [2]int{t502, t503}
    var values__15 *_goml_vec_int = func(values [2]int) *_goml_vec_int {
        var storage struct {
            vector _goml_vec_int
            values [2]int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t504)
    var place_root437 *_goml_vec_int = record_vec(log__12, "R", values__15)
    var index438 int = record(log__12, "I", 1)
    var place439 int = vec_get__Vec_3int(place_root437, index438)
    var value440 int = record(log__12, "V", 5)
    var t505 int = place439 + value440
    vec_set__Vec_3int(place_root437, index438, t505)
    var t507 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t507)
    var t508 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(values__15, 0)
    var t509 string = _goml_m_inherent_i_isize_i_isize_i_to__string(t508)
    var t510 string = "" + t509
    var t511 string = t510 + ","
    var t512 int
    var inline642 int = 1
    var inline643 int = vec_get__Vec_3int(values__15, inline642)
    t512 = inline643
    var t513 string
    var inline640 string = _goml_runtime_core_int_to_string(t512)
    t513 = inline640
    var t514 string = t511 + t513
    var inline637 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t514)
    _goml_runtime_core_string_println(inline637)
    var inline634 string = ""
    ref_set__Ref_6string(log__12, inline634)
    var t515 string = "" + "k"
    var t516 int
    var inline628 string = "K"
    var inline629 int = 1
    var inline630 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline631 string = inline630 + inline628
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline631)
    t516 = inline629
    var t517 string
    var inline626 string = _goml_runtime_core_int_to_string(t516)
    t517 = inline626
    var t518 string = t515 + t517
    var t519 int
    var inline620 string = "V"
    var inline621 int = 11
    var inline622 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline623 string = inline622 + inline620
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline623)
    t519 = inline621
    var t520 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: t518,
        _1: t519,
    }
    var t521 int
    var inline614 string = "A"
    var inline615 int = 1
    var inline616 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline617 string = inline616 + inline614
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline617)
    t521 = inline615
    var t522 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "same",
        _1: t521,
    }
    var t523 int
    var inline608 string = "B"
    var inline609 int = 2
    var inline610 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline611 string = inline610 + inline608
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline611)
    t523 = inline609
    var t524 Tuple2_6string_3int = Tuple2_6string_3int{
        _0: "same",
        _1: t523,
    }
    var t525 [3]Tuple2_6string_3int = [3]Tuple2_6string_3int{t520, t522, t524}
    var table__16 *hashmap_string_int_x = func(values [3]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }(t525)
    var t526 string
    var inline606 string = ref_get__Ref_6string(log__12)
    t526 = inline606
    var inline603 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t526)
    _goml_runtime_core_string_println(inline603)
    var mtmp446 Option__isize
    var inline600 string = "same"
    var inline601 Option__isize = hashmap_get__HashMap_6string_3int(table__16, inline600)
    mtmp446 = inline601
    var jp528 string
    switch mtmp446._tag {
    case 0:
        jp528 = "missing"
    case 1:
        var x447 int = mtmp446._v1_0
        var inline586 string = _goml_runtime_core_int_to_string(x447)
        jp528 = inline586
    default:
        panic("non-exhaustive match")
    }
    var inline597 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp528)
    _goml_runtime_core_string_println(inline597)
    var t529 [0]int = [0]int{}
    var empty_values__18 *_goml_vec_int = func(values [0]int) *_goml_vec_int {
        return &_goml_vec_int{
            items: values[0:len(values)],
        }
    }(t529)
    var t530 [0]Tuple2_6string_3int = [0]Tuple2_6string_3int{}
    var empty_table__19 *hashmap_string_int_x = func(values [0]Tuple2_6string_3int) *hashmap_string_int_x {
        var result *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
        for _, entry := range values {
            hashmap_set__HashMap_6string_3int(result, entry._0, entry._1)
        }
        return result
    }(t530)
    var t531 string = "" + "empty="
    var t532 int
    var inline595 int = vec_len__Vec_3int(empty_values__18)
    t532 = inline595
    var t533 int
    var inline593 int = hashmap_len__HashMap_6string_3int(empty_table__19)
    t533 = inline593
    var t534 int = t532 + t533
    var t535 string
    var inline591 string = _goml_runtime_core_int_to_string(t534)
    t535 = inline591
    var t536 string = t531 + t535
    var t537 string = t536 + " {ok}"
    var inline588 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t537)
    _goml_runtime_core_string_println(inline588)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__432 *ref_string_x) string {
    var t541 string = ref_get__Ref_6string(self__432)
    return t541
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__433 *ref_string_x, value__434 string) struct{} {
    ref_set__Ref_6string(self__433, value__434)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t545 string
    t545 = value__1
    _goml_runtime_core_string_println(t545)
    return struct{}{}
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__32 int) string {
    var t549 string = _goml_runtime_core_int_to_string(self__32)
    return t549
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__431 string) *ref_string_x {
    var t552 *ref_string_x = ref__Ref_6string(value__431)
    return t552
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__isize(self__268 *_goml_vec_int, index__269 int) int {
    var t555 int = vec_get__Vec_3int(self__268, index__269)
    return t555
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
