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

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func unwrap_or_negative(value__0 Option__int) int {
    switch value__0.(type) {
    case Some:
        var x188 int = value__0.(Some)._0
        return x188
    default:
        return -1
    }
}

func count_to(limit__2 int) int {
    var counter__3 *ref_int_x
    var inline291 int = 0
    var inline292 *ref_int_x = ref__Ref_3int(inline291)
    counter__3 = inline292
    var jp216 int
    Loop_loop_expr217:
    for {
        var current__4 int
        var inline289 int = ref_get__Ref_3int(counter__3)
        current__4 = inline289
        var t220 bool = current__4 >= limit__2
        if t220 {
            jp216 = current__4
            break Loop_loop_expr217
        } else {
            var t219 int = current__4 + 1
            ref_set__Ref_3int(counter__3, t219)
            continue
        }
    }
    return jp216
}

func loop_option(value__5 Option__int) int {
    var jp224 int
    switch value__5.(type) {
    case Some:
        var x194 int = value__5.(Some)._0
        jp224 = x194
        return jp224
    default:
        jp224 = -2
        return jp224
    }
}

func nested_loop_value() int {
    var jp230 int
    jp230 = 7
    return jp230
}

func main0() struct{} {
    println__T_string("C:\\tmp\\\"quoted\\\"")
    var t244 string = "" + "}"
    var inline335 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline335)
    var t245 Option__int = Some{
        _0: 11,
    }
    var t246 int = unwrap_or_negative(t245)
    var t247 string
    var inline333 string = _goml_runtime_core_int_to_string(t246)
    t247 = inline333
    var inline330 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t247)
    _goml_runtime_core_string_println(inline330)
    var t248 int
    t248 = -1
    var t249 string
    var inline324 string = _goml_runtime_core_int_to_string(t248)
    t249 = inline324
    var inline321 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t249)
    _goml_runtime_core_string_println(inline321)
    var t250 int = count_to(4)
    var t251 string
    var inline319 string = _goml_runtime_core_int_to_string(t250)
    t251 = inline319
    var inline316 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t251)
    _goml_runtime_core_string_println(inline316)
    var t252 Option__int = Some{
        _0: 9,
    }
    var t253 int = loop_option(t252)
    var t254 string
    var inline314 string = _goml_runtime_core_int_to_string(t253)
    t254 = inline314
    var inline311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t254)
    _goml_runtime_core_string_println(inline311)
    var t255 int = loop_option(None{})
    var t256 string
    var inline309 string = _goml_runtime_core_int_to_string(t255)
    t256 = inline309
    var inline306 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t256)
    _goml_runtime_core_string_println(inline306)
    var t257 int = nested_loop_value()
    var t258 string
    var inline304 string = _goml_runtime_core_int_to_string(t257)
    t258 = inline304
    var inline301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t258)
    _goml_runtime_core_string_println(inline301)
    var t259 bool
    var inline299 string = "C:\\tmp"
    switch inline299 {
    case "C:\\tmp":
        t259 = true
    default:
        t259 = false
    }
    var t260 string
    var inline297 string = _goml_runtime_core_bool_to_string(t259)
    t260 = inline297
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t260)
    _goml_runtime_core_string_println(inline294)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t270 string
    t270 = value__1
    _goml_runtime_core_string_println(t270)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
