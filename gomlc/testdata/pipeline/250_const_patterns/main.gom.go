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
    var t463 bool = value__3 == COMPUTED_ENABLED
    if t463 {
        return "enabled"
    } else {
        var t466 bool = value__3 == false
        if t466 {
            return "disabled"
        } else {
            var t467 string = missing__string("")
            return t467
        }
    }
}

func classify_comptime_guard(value__4 bool) string {
    var t472 bool = value__4 == true
    var jp471 string
    if t472 {
        jp471 = "enabled"
    } else {
        var t481 bool = value__4 == false
        if t481 {
            jp471 = "disabled"
        } else {
            var t482 string = missing__string("")
            jp471 = t482
        }
    }
    return jp471
}

func classify_pair(value__6 Tuple2_3int_5uint8) bool {
    var x411 int = value__6._0
    var x412 uint8 = value__6._1
    switch x412 {
    case 65:
        switch x411 {
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
    var t497 bool = value__8 == RATIO
    if t497 {
        return true
    } else {
        return false
    }
}

func for_binding() int {
    var total__14 *ref_int_x
    var inline582 int = 0
    var inline583 *ref_int_x = ref__Ref_3int(inline582)
    total__14 = inline583
    var for_source416 [2]int = [2]int{1, 2}
    var for_limit417 int = 2
    var for_index418 int = 0
    Loop_loop514:
    for {
        var t515 bool = for_index418 < for_limit417
        if t515 {
            var for_item419 int = array_get__Array_2_3int(for_source416, for_index418)
            var t516 int = for_index418 + 1
            for_index418 = t516
            var t517 int
            var inline578 int = ref_get__Ref_3int(total__14)
            t517 = inline578
            var t518 int = t517 + for_item419
            ref_set__Ref_3int(total__14, t518)
            continue
        } else {
            break Loop_loop514
        }
    }
    var inline580 int = ref_get__Ref_3int(total__14)
    return inline580
}

func main0() struct{} {
    var t521 string = classify(42)
    println__T_string(t521)
    var t522 string = classify(7)
    println__T_string(t522)
    var t523 string = classify(0)
    println__T_string(t523)
    var t524 string = classify_bool(true)
    println__T_string(t524)
    var t525 string = classify_bool(false)
    println__T_string(t525)
    var t526 string = classify_computed_bool(true)
    println__T_string(t526)
    var t527 string = classify_computed_bool(false)
    println__T_string(t527)
    var t528 string = classify_comptime_guard(true)
    println__T_string(t528)
    var t529 string = classify_comptime_guard(false)
    println__T_string(t529)
    var t530 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 65,
    }
    var t531 bool = classify_pair(t530)
    println__T_bool(t531)
    var t532 Tuple2_3int_5uint8 = Tuple2_3int_5uint8{
        _0: 42,
        _1: 66,
    }
    var t533 bool = classify_pair(t532)
    println__T_bool(t533)
    var t534 bool = classify_string("hello")
    println__T_bool(t534)
    var t535 bool = classify_float(1.5)
    var inline625 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t535)
    _goml_runtime_core_string_println(inline625)
    var t536 int
    var inline623 int = 9
    t536 = inline623
    var inline620 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t536)
    _goml_runtime_core_string_println(inline620)
    var t537 int
    var inline616 int = 11
    t537 = inline616
    var inline613 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t537)
    _goml_runtime_core_string_println(inline613)
    var t538 bool
    var inline611 int = 42
    switch inline611 {
    case 42:
        t538 = true
    default:
        t538 = false
    }
    var inline608 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t538)
    _goml_runtime_core_string_println(inline608)
    var t539 bool
    var inline606 int = 41
    switch inline606 {
    case 42:
        t539 = true
    default:
        t539 = false
    }
    var inline603 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t539)
    _goml_runtime_core_string_println(inline603)
    var t540 bool
    var inline600 int = 42
    switch inline600 {
    case 42:
        t540 = true
    default:
        t540 = false
    }
    var inline597 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t540)
    _goml_runtime_core_string_println(inline597)
    var t541 bool
    var inline594 int = 41
    switch inline594 {
    case 42:
        t541 = true
    default:
        t541 = false
    }
    var inline591 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t541)
    _goml_runtime_core_string_println(inline591)
    var t542 int = for_binding()
    var inline588 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t542)
    _goml_runtime_core_string_println(inline588)
    var inline585 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(CLASSIFIED_AT_COMPILE_TIME)
    _goml_runtime_core_string_println(inline585)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t553 string
    t553 = value__1
    _goml_runtime_core_string_println(t553)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t556 string
    var inline629 string = _goml_runtime_core_bool_to_string(value__1)
    t556 = inline629
    _goml_runtime_core_string_println(t556)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t565 string = _goml_runtime_core_bool_to_string(self__148)
    return t565
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t568 string = _goml_runtime_core_int_to_string(self__151)
    return t568
}

func main() {
    main0()
}
