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
        var x183 int = value__0.(Some)._0
        return x183
    default:
        return -1
    }
}

func count_to(limit__2 int) int {
    var counter__3 *ref_int_x
    var inline286 int = 0
    var inline287 *ref_int_x = ref__Ref_3int(inline286)
    counter__3 = inline287
    var jp211 int
    Loop_loop_expr212:
    for {
        var current__4 int
        var inline284 int = ref_get__Ref_3int(counter__3)
        current__4 = inline284
        var t215 bool = current__4 >= limit__2
        if t215 {
            jp211 = current__4
            break Loop_loop_expr212
        } else {
            var t214 int = current__4 + 1
            ref_set__Ref_3int(counter__3, t214)
            continue
        }
    }
    return jp211
}

func loop_option(value__5 Option__int) int {
    var jp219 int
    switch value__5.(type) {
    case Some:
        var x189 int = value__5.(Some)._0
        jp219 = x189
        return jp219
    default:
        jp219 = -2
        return jp219
    }
}

func nested_loop_value() int {
    var jp225 int
    jp225 = 7
    return jp225
}

func main0() struct{} {
    println__T_string("C:\\tmp\\\"quoted\\\"")
    var t239 string = "" + "}"
    var inline330 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline330)
    var t240 Option__int = Some{
        _0: 11,
    }
    var t241 int = unwrap_or_negative(t240)
    var t242 string
    var inline328 string = _goml_runtime_core_int_to_string(t241)
    t242 = inline328
    var inline325 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t242)
    _goml_runtime_core_string_println(inline325)
    var t243 int
    t243 = -1
    var t244 string
    var inline319 string = _goml_runtime_core_int_to_string(t243)
    t244 = inline319
    var inline316 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline316)
    var t245 int = count_to(4)
    var t246 string
    var inline314 string = _goml_runtime_core_int_to_string(t245)
    t246 = inline314
    var inline311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t246)
    _goml_runtime_core_string_println(inline311)
    var t247 Option__int = Some{
        _0: 9,
    }
    var t248 int = loop_option(t247)
    var t249 string
    var inline309 string = _goml_runtime_core_int_to_string(t248)
    t249 = inline309
    var inline306 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t249)
    _goml_runtime_core_string_println(inline306)
    var t250 int = loop_option(None{})
    var t251 string
    var inline304 string = _goml_runtime_core_int_to_string(t250)
    t251 = inline304
    var inline301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t251)
    _goml_runtime_core_string_println(inline301)
    var t252 int = nested_loop_value()
    var t253 string
    var inline299 string = _goml_runtime_core_int_to_string(t252)
    t253 = inline299
    var inline296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t253)
    _goml_runtime_core_string_println(inline296)
    var t254 bool
    var inline294 string = "C:\\tmp"
    switch inline294 {
    case "C:\\tmp":
        t254 = true
    default:
        t254 = false
    }
    var t255 string
    var inline292 string = _goml_runtime_core_bool_to_string(t254)
    t255 = inline292
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t255)
    _goml_runtime_core_string_println(inline289)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t265 string
    t265 = value__1
    _goml_runtime_core_string_println(t265)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
