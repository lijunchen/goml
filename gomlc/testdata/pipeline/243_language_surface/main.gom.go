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

func vec_len__Vec_3int(vec *_goml_vec_int) int {
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

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

type NumberSource struct {
    value int
}

type closure_env_increment_0 struct {
    captured_0 *ref_int_x
}

type Ordering int32

type Option__int struct {
    _tag int32
    _v1_0 int
}

type dyn__Source_vtable struct {
    get func(any) int
}

type dyn__Source struct {
    data any
    vtable *dyn__Source_vtable
}

func dyn__Source__wrap__NumberSource__get(self any) int {
    return _goml_m_trait__impl_i_Source_i_NumberSource_i_get(self.(NumberSource))
}

func dyn__Source__vtable__NumberSource() *dyn__Source_vtable {
    return &dyn__Source_vtable{
        get: dyn__Source__wrap__NumberSource__get,
    }
}

func _goml_m_trait__impl_i_Source_i_NumberSource_i_get(self__0 NumberSource) int {
    var t476 int = self__0.value
    return t476
}

func labeled_cleanup() struct{} {
    var inline585 string = "inner cleanup"
    var inline586 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline585)
    _goml_runtime_core_string_println(inline586)
    var inline581 string = "outer cleanup"
    var inline582 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline581)
    _goml_runtime_core_string_println(inline582)
    return struct{}{}
}

func main0() struct{} {
    var t487 NumberSource = NumberSource{
        value: 11,
    }
    var t488 dyn__Source = dyn__Source{
        data: t487,
        vtable: dyn__Source__vtable__NumberSource(),
    }
    var t489 int
    var inline630 int = t488.vtable.get(t488.data)
    t489 = inline630
    var inline627 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t489)
    _goml_runtime_core_string_println(inline627)
    var x414 int = 1
    var x415 int = 2
    var index__2 int = x414
    var compound_old416 int = index__2
    var t490 int = compound_old416 + x415
    index__2 = t490
    var inline624 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(index__2)
    _goml_runtime_core_string_println(inline624)
    var x421 int = 3
    var captured__4 *ref_int_x = ref__Ref_3int(x421)
    var t492 closure_env_increment_0 = closure_env_increment_0{
        captured_0: captured__4,
    }
    var increment__5 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(t492)
    }
    increment__5()
    var t493 int = ref_get__Ref_3int(captured__4)
    var inline621 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t493)
    _goml_runtime_core_string_println(inline621)
    var x429 int = 4
    var count__6 int = x429
    var compound_old430 int = count__6
    var compound_value431 int = 1
    var t544 int = compound_old430 + compound_value431
    count__6 = t544
    var inline589 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(count__6)
    _goml_runtime_core_string_println(inline589)
    var values__7 *_goml_vec_int
    var inline619 *_goml_vec_int = vec_new__Vec_3int()
    values__7 = inline619
    var inline616 int = 6
    vec_push__Vec_3int(values__7, inline616)
    var for_limit436 int = vec_len__Vec_3int(values__7)
    var for_index437 int = 0
    Loop_loop538:
    for {
        var t539 bool = for_index437 < for_limit436
        if t539 {
            var for_item438 int = vec_get__Vec_3int(values__7, for_index437)
            var t540 int = for_index437 + 1
            for_index437 = t540
            var item__8 int = for_item438
            var compound_old440 int = item__8
            var compound_value441 int = 1
            var t541 int = compound_old440 + compound_value441
            item__8 = t541
            var inline592 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(item__8)
            _goml_runtime_core_string_println(inline592)
            continue
        } else {
            break Loop_loop538
        }
    }
    var legacy__9 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 8,
        _1: 9,
    }
    var place_root444 Tuple2_3int_3int = legacy__9
    var place445 int = place_root444._0
    var value446 int = 1
    var t496 int = place445 + value446
    var t497 int = place_root444._1
    var t498 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t496,
        _1: t497,
    }
    legacy__9 = t498
    var place_root448 Tuple2_3int_3int = legacy__9
    var place449 int = place_root448._1
    var value450 int = 1
    var t500 int = place_root448._0
    var t501 int = place449 + value450
    var t502 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t500,
        _1: t501,
    }
    legacy__9 = t502
    var t504 int = legacy__9._0
    var t505 int = legacy__9._1
    var t506 int = t504 + t505
    var inline613 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t506)
    _goml_runtime_core_string_println(inline613)
    var steps__10 int = 0
    Loop_loop531:
    for {
        var t532 bool = steps__10 < 3
        if t532 {
            var compound_old453 int = steps__10
            var compound_value454 int = 1
            var t533 int = compound_old453 + compound_value454
            steps__10 = t533
            continue
        } else {
            break Loop_loop531
        }
    }
    var inline610 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(steps__10)
    _goml_runtime_core_string_println(inline610)
    var seen__11 *ref_int_x
    var inline607 int = 0
    var inline608 *ref_int_x = ref__Ref_3int(inline607)
    seen__11 = inline608
    var for_index459 int = 0
    var for_limit460 int = 3
    Loop_loop517:
    for {
        var t518 bool = for_index459 < for_limit460
        if t518 {
            var for_item461 int = for_index459
            var t519 int = for_index459 + 1
            for_index459 = t519
            var for_index463 int = 0
            var for_limit464 int = 3
            var t529 bool = for_item461 == 1
            Loop_loop521:
            for {
                var t522 bool = for_index463 < for_limit464
                if t522 {
                    var for_item465 int = for_index463
                    var t523 int = for_index463 + 1
                    for_index463 = t523
                    var t524 int
                    var inline597 int = ref_get__Ref_3int(seen__11)
                    t524 = inline597
                    var t525 int = t524 + 1
                    ref_set__Ref_3int(seen__11, t525)
                    var jp528 bool
                    if t529 {
                        var t530 bool = for_item465 == 1
                        jp528 = t530
                    } else {
                        jp528 = false
                    }
                    if jp528 {
                        var t509 int
                        var inline605 int = ref_get__Ref_3int(seen__11)
                        t509 = inline605
                        var inline602 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t509)
                        _goml_runtime_core_string_println(inline602)
                        var jp511 int
                        jp511 = 42
                        var inline599 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp511)
                        _goml_runtime_core_string_println(inline599)
                        labeled_cleanup()
                        return struct{}{}
                    } else {
                        continue
                    }
                } else {
                    break Loop_loop521
                }
            }
            continue
        } else {
            break Loop_loop517
        }
    }
    var t509 int
    var inline605 int = ref_get__Ref_3int(seen__11)
    t509 = inline605
    var inline602 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t509)
    _goml_runtime_core_string_println(inline602)
    var jp511 int
    jp511 = 42
    var inline599 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp511)
    _goml_runtime_core_string_println(inline599)
    labeled_cleanup()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t570 string = _goml_runtime_core_int_to_string(self__151)
    return t570
}

func _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(env473 closure_env_increment_0) struct{} {
    var captured__4 *ref_int_x = env473.captured_0
    var compound_old423 int = ref_get__Ref_3int(captured__4)
    var compound_value424 int = 1
    var t578 int = compound_old423 + compound_value424
    ref_set__Ref_3int(captured__4, t578)
    return struct{}{}
}

func main() {
    main0()
}
