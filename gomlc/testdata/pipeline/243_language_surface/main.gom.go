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

type Option__isize struct {
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
    var t479 int = self__0.value
    return t479
}

func labeled_cleanup() struct{} {
    var inline588 string = "inner cleanup"
    var inline589 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline588)
    _goml_runtime_core_string_println(inline589)
    var inline584 string = "outer cleanup"
    var inline585 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline584)
    _goml_runtime_core_string_println(inline585)
    return struct{}{}
}

func main0() struct{} {
    var t490 NumberSource = NumberSource{
        value: 11,
    }
    var t491 dyn__Source = dyn__Source{
        data: t490,
        vtable: dyn__Source__vtable__NumberSource(),
    }
    var t492 int
    var inline633 int = t491.vtable.get(t491.data)
    t492 = inline633
    var inline630 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t492)
    _goml_runtime_core_string_println(inline630)
    var x417 int = 1
    var x418 int = 2
    var index__2 int = x417
    var compound_old419 int = index__2
    var t493 int = compound_old419 + x418
    index__2 = t493
    var inline627 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(index__2)
    _goml_runtime_core_string_println(inline627)
    var x424 int = 3
    var captured__4 *ref_int_x = ref__Ref_3int(x424)
    var t495 closure_env_increment_0 = closure_env_increment_0{
        captured_0: captured__4,
    }
    var increment__5 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(t495)
    }
    increment__5()
    var t496 int = ref_get__Ref_3int(captured__4)
    var inline624 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t496)
    _goml_runtime_core_string_println(inline624)
    var x432 int = 4
    var count__6 int = x432
    var compound_old433 int = count__6
    var compound_value434 int = 1
    var t547 int = compound_old433 + compound_value434
    count__6 = t547
    var inline592 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(count__6)
    _goml_runtime_core_string_println(inline592)
    var values__7 *_goml_vec_int
    var inline622 *_goml_vec_int = vec_new__Vec_3int()
    values__7 = inline622
    var inline619 int = 6
    vec_push__Vec_3int(values__7, inline619)
    var for_limit439 int = vec_len__Vec_3int(values__7)
    var for_index440 int = 0
    Loop_loop541:
    for {
        var t542 bool = for_index440 < for_limit439
        if t542 {
            var for_item441 int = vec_get__Vec_3int(values__7, for_index440)
            var t543 int = for_index440 + 1
            for_index440 = t543
            var item__8 int = for_item441
            var compound_old443 int = item__8
            var compound_value444 int = 1
            var t544 int = compound_old443 + compound_value444
            item__8 = t544
            var inline595 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(item__8)
            _goml_runtime_core_string_println(inline595)
            continue
        } else {
            break Loop_loop541
        }
    }
    var legacy__9 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: 8,
        _1: 9,
    }
    var place_root447 Tuple2_3int_3int = legacy__9
    var place448 int = place_root447._0
    var value449 int = 1
    var t499 int = place448 + value449
    var t500 int = place_root447._1
    var t501 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t499,
        _1: t500,
    }
    legacy__9 = t501
    var place_root451 Tuple2_3int_3int = legacy__9
    var place452 int = place_root451._1
    var value453 int = 1
    var t503 int = place_root451._0
    var t504 int = place452 + value453
    var t505 Tuple2_3int_3int = Tuple2_3int_3int{
        _0: t503,
        _1: t504,
    }
    legacy__9 = t505
    var t507 int = legacy__9._0
    var t508 int = legacy__9._1
    var t509 int = t507 + t508
    var inline616 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t509)
    _goml_runtime_core_string_println(inline616)
    var steps__10 int = 0
    Loop_loop534:
    for {
        var t535 bool = steps__10 < 3
        if t535 {
            var compound_old456 int = steps__10
            var compound_value457 int = 1
            var t536 int = compound_old456 + compound_value457
            steps__10 = t536
            continue
        } else {
            break Loop_loop534
        }
    }
    var inline613 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(steps__10)
    _goml_runtime_core_string_println(inline613)
    var seen__11 *ref_int_x
    var inline610 int = 0
    var inline611 *ref_int_x = ref__Ref_3int(inline610)
    seen__11 = inline611
    var for_index462 int = 0
    var for_limit463 int = 3
    Loop_loop520:
    for {
        var t521 bool = for_index462 < for_limit463
        if t521 {
            var for_item464 int = for_index462
            var t522 int = for_index462 + 1
            for_index462 = t522
            var for_index466 int = 0
            var for_limit467 int = 3
            var t532 bool = for_item464 == 1
            Loop_loop524:
            for {
                var t525 bool = for_index466 < for_limit467
                if t525 {
                    var for_item468 int = for_index466
                    var t526 int = for_index466 + 1
                    for_index466 = t526
                    var t527 int
                    var inline600 int = ref_get__Ref_3int(seen__11)
                    t527 = inline600
                    var t528 int = t527 + 1
                    ref_set__Ref_3int(seen__11, t528)
                    var jp531 bool
                    if t532 {
                        var t533 bool = for_item468 == 1
                        jp531 = t533
                    } else {
                        jp531 = false
                    }
                    if jp531 {
                        var t512 int
                        var inline608 int = ref_get__Ref_3int(seen__11)
                        t512 = inline608
                        var inline605 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t512)
                        _goml_runtime_core_string_println(inline605)
                        var jp514 int
                        jp514 = 42
                        var inline602 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp514)
                        _goml_runtime_core_string_println(inline602)
                        labeled_cleanup()
                        return struct{}{}
                    } else {
                        continue
                    }
                } else {
                    break Loop_loop524
                }
            }
            continue
        } else {
            break Loop_loop520
        }
    }
    var t512 int
    var inline608 int = ref_get__Ref_3int(seen__11)
    t512 = inline608
    var inline605 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t512)
    _goml_runtime_core_string_println(inline605)
    var jp514 int
    jp514 = 42
    var inline602 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp514)
    _goml_runtime_core_string_println(inline602)
    labeled_cleanup()
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t573 string = _goml_runtime_core_int_to_string(self__151)
    return t573
}

func _goml_m_inherent_i_closure__en_hd344b745b40be6f4a908632f0feb9f48_ment__0_i_apply(env476 closure_env_increment_0) struct{} {
    var captured__4 *ref_int_x = env476.captured_0
    var compound_old426 int = ref_get__Ref_3int(captured__4)
    var compound_value427 int = 1
    var t581 int = compound_old426 + compound_value427
    ref_set__Ref_3int(captured__4, t581)
    return struct{}{}
}

func main() {
    main0()
}
