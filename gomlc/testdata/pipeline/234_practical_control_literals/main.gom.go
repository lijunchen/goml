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

type Ordering int32

type Option__int struct {
    _tag int32
    _v1_0 int
}

func unwrap_or_negative(value__0 Option__int) int {
    switch value__0._tag {
    case 1:
        var x409 int = value__0._v1_0
        return x409
    default:
        return -1
    }
}

func count_to(limit__2 int) int {
    var counter__3 *ref_int_x
    var inline512 int = 0
    var inline513 *ref_int_x = ref__Ref_3int(inline512)
    counter__3 = inline513
    var jp437 int
    Loop_loop_expr438:
    for {
        var current__4 int
        var inline510 int = ref_get__Ref_3int(counter__3)
        current__4 = inline510
        var t441 bool = current__4 >= limit__2
        if t441 {
            jp437 = current__4
            break Loop_loop_expr438
        } else {
            var t440 int = current__4 + 1
            ref_set__Ref_3int(counter__3, t440)
            continue
        }
    }
    return jp437
}

func loop_option(value__5 Option__int) int {
    var jp445 int
    switch value__5._tag {
    case 1:
        var x415 int = value__5._v1_0
        jp445 = x415
        return jp445
    default:
        jp445 = -2
        return jp445
    }
}

func nested_loop_value() int {
    var jp451 int
    jp451 = 7
    return jp451
}

func main0() struct{} {
    println__T_string("C:\\tmp\\\"quoted\\\"")
    var t465 string = "" + "}"
    var inline556 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t465)
    _goml_runtime_core_string_println(inline556)
    var t466 Option__int = Option__int{
        _tag: 1,
        _v1_0: 11,
    }
    var t467 int = unwrap_or_negative(t466)
    var t468 string
    var inline554 string = _goml_runtime_core_int_to_string(t467)
    t468 = inline554
    var inline551 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t468)
    _goml_runtime_core_string_println(inline551)
    var t469 int
    t469 = -1
    var t470 string
    var inline545 string = _goml_runtime_core_int_to_string(t469)
    t470 = inline545
    var inline542 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t470)
    _goml_runtime_core_string_println(inline542)
    var t471 int = count_to(4)
    var t472 string
    var inline540 string = _goml_runtime_core_int_to_string(t471)
    t472 = inline540
    var inline537 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t472)
    _goml_runtime_core_string_println(inline537)
    var t473 Option__int = Option__int{
        _tag: 1,
        _v1_0: 9,
    }
    var t474 int = loop_option(t473)
    var t475 string
    var inline535 string = _goml_runtime_core_int_to_string(t474)
    t475 = inline535
    var inline532 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t475)
    _goml_runtime_core_string_println(inline532)
    var t476 int = loop_option(Option__int{
        _tag: 0,
    })
    var t477 string
    var inline530 string = _goml_runtime_core_int_to_string(t476)
    t477 = inline530
    var inline527 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t477)
    _goml_runtime_core_string_println(inline527)
    var t478 int = nested_loop_value()
    var t479 string
    var inline525 string = _goml_runtime_core_int_to_string(t478)
    t479 = inline525
    var inline522 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t479)
    _goml_runtime_core_string_println(inline522)
    var t480 bool
    var inline520 string = "C:\\tmp"
    switch inline520 {
    case "C:\\tmp":
        t480 = true
    default:
        t480 = false
    }
    var t481 string
    var inline518 string = _goml_runtime_core_bool_to_string(t480)
    t481 = inline518
    var inline515 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t481)
    _goml_runtime_core_string_println(inline515)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t491 string
    t491 = value__1
    _goml_runtime_core_string_println(t491)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
