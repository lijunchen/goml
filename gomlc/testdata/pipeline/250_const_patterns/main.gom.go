package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_intrinsic_missing(s string) struct{} {
    println("missing: " + s)
    panic("")
    return struct{}{}
}

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
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

func missing__string(s string) string {
    _goml_intrinsic_missing(s)
    var ret string
    return ret
}

type Tuple2_3int_5uint8 struct {
    _0 int
    _1 uint8
}

type Ordering int32

const (
    RATIO float64 = 1.5
    COMPUTED_ENABLED bool = true
    CLASSIFIED_AT_COMPILE_TIME int = 1
)

func classify(value__1 int) string {
    switch value__1 {
    case 42:
        return "known"
    case 7:
        return "known"
    default:
        return "other"
    }
}

func classify_bool(value__2 bool) string {
    switch value__2 {
    case true:
        return "enabled"
    case false:
        return "disabled"
    default:
        panic("non-exhaustive match")
    }
}

func classify_computed_bool(value__3 bool) string {
    var t466 bool = value__3 == COMPUTED_ENABLED
    if t466 {
        return "enabled"
    } else {
        var t469 bool = value__3 == false
        if t469 {
            return "disabled"
        } else {
            var t470 string = missing__string("")
            return t470
        }
    }
}

func classify_comptime_guard(value__4 bool) string {
    var t475 bool = value__4 == true
    var jp474 string
    if t475 {
        jp474 = "enabled"
    } else {
        var t484 bool = value__4 == false
        if t484 {
            jp474 = "disabled"
        } else {
            var t485 string = missing__string("")
            jp474 = t485
        }
    }
    return jp474
}

func classify_pair(value__6 Tuple2_3int_5uint8) bool {
    var x414 int = value__6._0
    var x415 uint8 = value__6._1
    switch x415 {
    case 65:
        switch x414 {
        case 42:
            return true
        default:
            return false
        }
    default:
        return false
    }
}

func classify_string(value__7 string) bool {
    switch value__7 {
    case "hello":
        return true
    default:
        return false
    }
}

func classify_float(value__8 float64) bool {
    var t500 bool = value__8 == RATIO
    if t500 {
        return true
    } else {
        return false
    }
}

func for_binding() int {
    var total__14 *ref_int_x
    var inline585 int = 0
    var inline586 *ref_int_x = ref__Ref_3int(inline585)
    total__14 = inline586
    var for_source419 [2]int = [2]int{1, 2}
    var for_limit420 int = 2
    var for_index421 int = 0
    Loop_loop517:
    for {
        var t518 bool = for_index421 < for_limit420
        if t518 {
            var for_item422 int = array_get__Array_2_3int(for_source419, for_index421)
            var t519 int = for_index421 + 1
            for_index421 = t519
            var t520 int
            var inline581 int = ref_get__Ref_3int(total__14)
            t520 = inline581
            var t521 int = t520 + for_item422
            ref_set__Ref_3int(total__14, t521)
            continue
        } else {
            break Loop_loop517
        }
    }
    var inline583 int = ref_get__Ref_3int(total__14)
    return inline583
}

func main0() struct{} {
    var t524 string = classify(42)
    println__T_string(t524)
    var t525 string = classify(7)
    println__T_string(t525)
    var t526 string = classify(0)
    println__T_string(t526)
    var t527 string = classify_bool(true)
    println__T_string(t527)
    var t528 string = classify_bool(false)
    println__T_string(t528)
    var t529 string = classify_computed_bool(true)
    println__T_string(t529)
    var t530 string = classify_computed_bool(false)
    println__T_string(t530)
    var t531 string = classify_comptime_guard(true)
    println__T_string(t531)
    var t532 string = classify_comptime_guard(false)
    println__T_string(t532)
    var t533 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 65,
    }
    var t534 bool = classify_pair(t533)
    println__T_bool(t534)
    var t535 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 66,
    }
    var t536 bool = classify_pair(t535)
    println__T_bool(t536)
    var t537 bool = classify_string("hello")
    println__T_bool(t537)
    var t538 bool = classify_float(1.5)
    var inline628 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t538)
    _goml_runtime_core_string_println(inline628)
    var t539 int
    var inline626 int = 9
    t539 = inline626
    var inline623 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t539)
    _goml_runtime_core_string_println(inline623)
    var t540 int
    var inline619 int = 11
    t540 = inline619
    var inline616 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t540)
    _goml_runtime_core_string_println(inline616)
    var t541 bool
    var inline614 int = 42
    switch inline614 {
    case 42:
        t541 = true
    default:
        t541 = false
    }
    var inline611 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t541)
    _goml_runtime_core_string_println(inline611)
    var t542 bool
    var inline609 int = 41
    switch inline609 {
    case 42:
        t542 = true
    default:
        t542 = false
    }
    var inline606 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t542)
    _goml_runtime_core_string_println(inline606)
    var t543 bool
    var inline603 int = 42
    switch inline603 {
    case 42:
        t543 = true
    default:
        t543 = false
    }
    var inline600 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t543)
    _goml_runtime_core_string_println(inline600)
    var t544 bool
    var inline597 int = 41
    switch inline597 {
    case 42:
        t544 = true
    default:
        t544 = false
    }
    var inline594 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t544)
    _goml_runtime_core_string_println(inline594)
    var t545 int = for_binding()
    var inline591 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t545)
    _goml_runtime_core_string_println(inline591)
    var inline588 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(CLASSIFIED_AT_COMPILE_TIME)
    _goml_runtime_core_string_println(inline588)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t556 string
    t556 = value__1
    _goml_runtime_core_string_println(t556)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t559 string
    var inline632 string = _goml_runtime_core_bool_to_string(value__1)
    t559 = inline632
    _goml_runtime_core_string_println(t559)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t568 string = _goml_runtime_core_bool_to_string(self__148)
    return t568
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t571 string = _goml_runtime_core_int_to_string(self__151)
    return t571
}

func main() {
    main0()
}
