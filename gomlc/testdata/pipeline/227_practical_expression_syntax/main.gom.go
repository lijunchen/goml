package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
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

type _goml_vec_int struct {
    items []int
}

func vec_new__Vec_3int() *_goml_vec_int {
    return &_goml_vec_int{
        items: nil,
    }
}

func vec_push__Vec_3int(vec *_goml_vec_int, elem int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
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
    buckets map[uint64][]hashmap_string_int_x_entry
    hashes []uint64
    len int
}

func hashmap_new__HashMap_6string_3int() *hashmap_string_int_x {
    return &hashmap_string_int_x{
        buckets: make(map[uint64][]hashmap_string_int_x_entry),
        len: 0,
        hashes: nil,
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
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int_x_entry = m.buckets[h]
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_string_int_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_string_i_eq(entry.key, key) {
            return entry.value, true
        }
        i = i + 1
    }
    var zero int
    return zero, false
}

func hashmap_get__HashMap_6string_3int(m *hashmap_string_int_x, key string) Option__int {
    var value int
    var ok bool
    value, ok = hashmap_lookup__HashMap_6string_3int(m, key)
    if ok {
        return Some{
            _0: value,
        }
    }
    return None{}
}

func hashmap_set__HashMap_6string_3int(m *hashmap_string_int_x, key string, value int) struct{} {
    var reuse_index int = -1
    if m == nil {
        return struct{}{}
    }
    var h uint64 = _goml_m_trait__impl_i_Hash_i_string_i_hash(key)
    var bucket []hashmap_string_int_x_entry = m.buckets[h]
    if len(bucket) == 0 {
        m.hashes = append(m.hashes, h)
    }
    var i int = 0
    for {
        if i >= int(len(bucket)) {
            break
        }
        var entry hashmap_string_int_x_entry = bucket[i]
        if entry.active && _goml_m_trait__impl_i_PartialEq_i_string_i_eq(entry.key, key) {
            bucket[i].value = value
            return struct{}{}
        }
        if !entry.active && reuse_index < 0 {
            reuse_index = i
        }
        i = i + 1
    }
    if reuse_index >= 0 {
        bucket[reuse_index] = hashmap_string_int_x_entry{
            active: true,
            key: key,
            value: value,
        }
        m.len = m.len + 1
        return struct{}{}
    }
    bucket = append(bucket, hashmap_string_int_x_entry{
        active: true,
        key: key,
        value: value,
    })
    m.buckets[h] = bucket
    m.len = m.len + 1
    return struct{}{}
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

type Point struct {
    x int
    y int
}

type Ordering int32

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func record(log__0 *ref_string_x, label__1 string, value__2 int) int {
    var t454 string
    var inline581 string = ref_get__Ref_6string(log__0)
    t454 = inline581
    var t455 string = t454 + label__1
    ref_set__Ref_6string(log__0, t455)
    return value__2
}

func record_point(log__3 *ref_string_x, label__4 string, value__5 Point) Point {
    var t458 string
    var inline585 string = ref_get__Ref_6string(log__3)
    t458 = inline585
    var t459 string = t458 + label__4
    ref_set__Ref_6string(log__3, t459)
    return value__5
}

func record_vec(log__6 *ref_string_x, label__7 string, value__8 *_goml_vec_int) *_goml_vec_int {
    var t462 string
    var inline589 string = ref_get__Ref_6string(log__6)
    t462 = inline589
    var t463 string = t462 + label__7
    ref_set__Ref_6string(log__6, t463)
    return value__8
}

func main0() struct{} {
    var number__9 int = 5
    var compound_old411 int = number__9
    var compound_value412 int = 3
    var t465 int = compound_old411 + compound_value412
    number__9 = t465
    var compound_old414 int = number__9
    var compound_value415 int = 2
    var t467 int = compound_old414 * compound_value415
    number__9 = t467
    var compound_old417 int = number__9
    var compound_value418 int = 1
    var t469 int = compound_old417 >> compound_value418
    number__9 = t469
    var t471 string = _goml_m_inherent_i_int_i_int_i_to__string(number__9)
    println__T_string(t471)
    var direct__10 Point = Point{
        x: 3,
        y: 4,
    }
    var place_root421 Point = direct__10
    var place422 int = place_root421.x
    var value423 int = 5
    var t472 int = place422 + value423
    var t473 int = place_root421.y
    var t474 Point = Point{
        x: t472,
        y: t473,
    }
    direct__10 = t474
    var t476 int = direct__10.x
    var t477 string = _goml_m_inherent_i_int_i_int_i_to__string(t476)
    var t478 string = "" + t477
    var t479 string = t478 + ","
    var t480 int = direct__10.y
    var t481 string = _goml_m_inherent_i_int_i_int_i_to__string(t480)
    var t482 string = t479 + t481
    println__T_string(t482)
    var pair__11 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 2,
        _1: 9,
    }
    var place_root426 Tuple2_3int_3int = pair__11
    var place427 int = place_root426._0
    var value428 int = 3
    var t483 int = place427 * value428
    var t484 int = place_root426._1
    var t485 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t483,
        _1: t484,
    }
    pair__11 = t485
    var t487 int = pair__11._0
    var t488 string = _goml_m_inherent_i_int_i_int_i_to__string(t487)
    var t489 string = "" + t488
    var t490 string = t489 + ","
    var t491 int = pair__11._1
    var t492 string = _goml_m_inherent_i_int_i_int_i_to__string(t491)
    var t493 string = t490 + t492
    println__T_string(t493)
    var log__12 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var base__13 Point = Point{
        x: 1,
        y: 2,
    }
    var struct_update_field__780__0 int = record(log__12, "F", 7)
    var struct_update_base__780 Point = record_point(log__12, "B", base__13)
    var t494 int = struct_update_base__780.y
    var t496 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t496)
    var t498 string = _goml_m_inherent_i_int_i_int_i_to__string(struct_update_field__780__0)
    var t499 string = "" + t498
    var t500 string = t499 + ","
    var t502 string = _goml_m_inherent_i_int_i_int_i_to__string(t494)
    var t503 string = t500 + t502
    println__T_string(t503)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var vec_literal__967 *_goml_vec_int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int()
    var t504 int = record(log__12, "A", 10)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t504)
    var t505 int = record(log__12, "B", 20)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(vec_literal__967, t505)
    var place_root436 *_goml_vec_int = record_vec(log__12, "R", vec_literal__967)
    var index437 int = record(log__12, "I", 1)
    var place438 int = vec_get__Vec_3int(place_root436, index437)
    var value439 int = record(log__12, "V", 5)
    var t506 int = place438 + value439
    vec_set__Vec_3int(place_root436, index437, t506)
    var t508 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    println__T_string(t508)
    var t509 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 0)
    var t510 string = _goml_m_inherent_i_int_i_int_i_to__string(t509)
    var t511 string = "" + t510
    var t512 string = t511 + ","
    var t513 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(vec_literal__967, 1)
    var t514 string = _goml_m_inherent_i_int_i_int_i_to__string(t513)
    var t515 string = t512 + t514
    println__T_string(t515)
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, "")
    var hashmap_literal__1226 *hashmap_string_int_x
    var inline645 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1226 = inline645
    var t516 string = "" + "k"
    var t517 int = record(log__12, "K", 1)
    var t518 string
    var inline643 string = _goml_runtime_core_int_to_string(t517)
    t518 = inline643
    var t519 string = t516 + t518
    var t520 int
    var inline637 string = "V"
    var inline638 int = 11
    var inline639 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline640 string = inline639 + inline637
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline640)
    t520 = inline638
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, t519, t520)
    var t521 int
    var inline629 string = "A"
    var inline630 int = 1
    var inline631 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline632 string = inline631 + inline629
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline632)
    t521 = inline630
    var inline626 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline626, t521)
    var t522 int
    var inline620 string = "B"
    var inline621 int = 2
    var inline622 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__12)
    var inline623 string = inline622 + inline620
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__12, inline623)
    t522 = inline621
    var inline617 string = "same"
    hashmap_set__HashMap_6string_3int(hashmap_literal__1226, inline617, t522)
    var t523 string
    var inline615 string = ref_get__Ref_6string(log__12)
    t523 = inline615
    var inline612 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t523)
    _goml_runtime_core_string_println(inline612)
    var mtmp448 Option__int
    var inline609 string = "same"
    var inline610 Option__int = hashmap_get__HashMap_6string_3int(hashmap_literal__1226, inline609)
    mtmp448 = inline610
    var jp525 string
    switch mtmp448.(type) {
    case None:
        jp525 = "missing"
    case Some:
        var x449 int = mtmp448.(Some)._0
        var inline591 string = _goml_runtime_core_int_to_string(x449)
        jp525 = inline591
    default:
        panic("non-exhaustive match")
    }
    var inline606 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp525)
    _goml_runtime_core_string_println(inline606)
    var vec_literal__1570 *_goml_vec_int
    var inline604 *_goml_vec_int = vec_new__Vec_3int()
    vec_literal__1570 = inline604
    var hashmap_literal__1623 *hashmap_string_int_x
    var inline602 *hashmap_string_int_x = hashmap_new__HashMap_6string_3int()
    hashmap_literal__1623 = inline602
    var t526 string = "" + "empty="
    var t527 int
    var inline600 int = vec_len__Vec_3int(vec_literal__1570)
    t527 = inline600
    var t528 int
    var inline598 int = hashmap_len__HashMap_6string_3int(hashmap_literal__1623)
    t528 = inline598
    var t529 int = t527 + t528
    var t530 string
    var inline596 string = _goml_runtime_core_int_to_string(t529)
    t530 = inline596
    var t531 string = t526 + t530
    var t532 string = t531 + " {ok}"
    var inline593 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t532)
    _goml_runtime_core_string_println(inline593)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__432 *ref_string_x) string {
    var t536 string = ref_get__Ref_6string(self__432)
    return t536
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__433 *ref_string_x, value__434 string) struct{} {
    ref_set__Ref_6string(self__433, value__434)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t540 string
    t540 = value__1
    _goml_runtime_core_string_println(t540)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t544 string = _goml_runtime_core_int_to_string(self__32)
    return t544
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__431 string) *ref_string_x {
    var t547 *ref_string_x = ref__Ref_6string(value__431)
    return t547
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__int() *_goml_vec_int {
    var t550 *_goml_vec_int = vec_new__Vec_3int()
    return t550
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__int(self__258 *_goml_vec_int, elem__259 int) struct{} {
    vec_push__Vec_3int(self__258, elem__259)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__int(self__268 *_goml_vec_int, index__269 int) int {
    var t555 int = vec_get__Vec_3int(self__268, index__269)
    return t555
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_PartialEq_i_string_i_eq(self__181 string, other__182 string) bool {
    var t574 bool = self__181 == other__182
    return t574
}

func _goml_m_trait__impl_i_Hash_i_string_i_hash(self__209 string) uint64 {
    var t577 uint64 = _goml_runtime_core_string_hash(self__209)
    return t577
}

func main() {
    main0()
}
