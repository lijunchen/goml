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
        var x412 int = value__0._v1_0
        return x412
    default:
        return -1
    }
}

func count_to(limit__2 int) int {
    var counter__3 *ref_int_x
    var inline515 int = 0
    var inline516 *ref_int_x = ref__Ref_3int(inline515)
    counter__3 = inline516
    var jp440 int
    Loop_loop_expr441:
    for {
        var current__4 int
        var inline513 int = ref_get__Ref_3int(counter__3)
        current__4 = inline513
        var t444 bool = current__4 >= limit__2
        if t444 {
            jp440 = current__4
            break Loop_loop_expr441
        } else {
            var t443 int = current__4 + 1
            ref_set__Ref_3int(counter__3, t443)
            continue
        }
    }
    return jp440
}

func loop_option(value__5 Option__int) int {
    var jp448 int
    switch value__5._tag {
    case 1:
        var x418 int = value__5._v1_0
        jp448 = x418
        return jp448
    default:
        jp448 = -2
        return jp448
    }
}

func nested_loop_value() int {
    var jp454 int
    jp454 = 7
    return jp454
}

func main0() struct{} {
    println__T_string("C:\\tmp\\\"quoted\\\"")
    var t468 string = "" + "}"
    var inline559 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t468)
    _goml_runtime_core_string_println(inline559)
    var t469 Option__int = Option__int{
        _tag: 1,
        _v1_0: 11,
    }
    var t470 int = unwrap_or_negative(t469)
    var t471 string
    var inline557 string = _goml_runtime_core_int_to_string(t470)
    t471 = inline557
    var inline554 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t471)
    _goml_runtime_core_string_println(inline554)
    var t472 int
    t472 = -1
    var t473 string
    var inline548 string = _goml_runtime_core_int_to_string(t472)
    t473 = inline548
    var inline545 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t473)
    _goml_runtime_core_string_println(inline545)
    var t474 int = count_to(4)
    var t475 string
    var inline543 string = _goml_runtime_core_int_to_string(t474)
    t475 = inline543
    var inline540 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t475)
    _goml_runtime_core_string_println(inline540)
    var t476 Option__int = Option__int{
        _tag: 1,
        _v1_0: 9,
    }
    var t477 int = loop_option(t476)
    var t478 string
    var inline538 string = _goml_runtime_core_int_to_string(t477)
    t478 = inline538
    var inline535 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t478)
    _goml_runtime_core_string_println(inline535)
    var t479 int = loop_option(Option__int{
        _tag: 0,
    })
    var t480 string
    var inline533 string = _goml_runtime_core_int_to_string(t479)
    t480 = inline533
    var inline530 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t480)
    _goml_runtime_core_string_println(inline530)
    var t481 int = nested_loop_value()
    var t482 string
    var inline528 string = _goml_runtime_core_int_to_string(t481)
    t482 = inline528
    var inline525 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t482)
    _goml_runtime_core_string_println(inline525)
    var t483 bool
    var inline523 string = "C:\\tmp"
    switch inline523 {
    case "C:\\tmp":
        t483 = true
    default:
        t483 = false
    }
    var t484 string
    var inline521 string = _goml_runtime_core_bool_to_string(t483)
    t484 = inline521
    var inline518 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t484)
    _goml_runtime_core_string_println(inline518)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t494 string
    t494 = value__1
    _goml_runtime_core_string_println(t494)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
