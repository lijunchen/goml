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
        var x178 int = value__0.(Some)._0
        return x178
    default:
        return -1
    }
}

func count_to(limit__2 int) int {
    var counter__3 *ref_int_x
    var inline281 int = 0
    var inline282 *ref_int_x = ref__Ref_3int(inline281)
    counter__3 = inline282
    var jp206 int
    Loop_loop_expr207:
    for {
        var current__4 int
        var inline279 int = ref_get__Ref_3int(counter__3)
        current__4 = inline279
        var t210 bool = current__4 >= limit__2
        if t210 {
            jp206 = current__4
            break Loop_loop_expr207
        } else {
            var t209 int = current__4 + 1
            ref_set__Ref_3int(counter__3, t209)
            continue
        }
    }
    return jp206
}

func loop_option(value__5 Option__int) int {
    var jp214 int
    switch value__5.(type) {
    case Some:
        var x184 int = value__5.(Some)._0
        jp214 = x184
        return jp214
    default:
        jp214 = -2
        return jp214
    }
}

func nested_loop_value() int {
    var jp220 int
    jp220 = 7
    return jp220
}

func main0() struct{} {
    println__T_string("C:\\tmp\\\"quoted\\\"")
    var t234 string = "" + "}"
    var inline325 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline325)
    var t235 Option__int = Some{
        _0: 11,
    }
    var t236 int = unwrap_or_negative(t235)
    var t237 string
    var inline323 string = _goml_runtime_core_int_to_string(t236)
    t237 = inline323
    var inline320 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t237)
    _goml_runtime_core_string_println(inline320)
    var t238 int
    t238 = -1
    var t239 string
    var inline314 string = _goml_runtime_core_int_to_string(t238)
    t239 = inline314
    var inline311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline311)
    var t240 int = count_to(4)
    var t241 string
    var inline309 string = _goml_runtime_core_int_to_string(t240)
    t241 = inline309
    var inline306 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t241)
    _goml_runtime_core_string_println(inline306)
    var t242 Option__int = Some{
        _0: 9,
    }
    var t243 int = loop_option(t242)
    var t244 string
    var inline304 string = _goml_runtime_core_int_to_string(t243)
    t244 = inline304
    var inline301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t244)
    _goml_runtime_core_string_println(inline301)
    var t245 int = loop_option(None{})
    var t246 string
    var inline299 string = _goml_runtime_core_int_to_string(t245)
    t246 = inline299
    var inline296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t246)
    _goml_runtime_core_string_println(inline296)
    var t247 int = nested_loop_value()
    var t248 string
    var inline294 string = _goml_runtime_core_int_to_string(t247)
    t248 = inline294
    var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t248)
    _goml_runtime_core_string_println(inline291)
    var t249 bool
    var inline289 string = "C:\\tmp"
    switch inline289 {
    case "C:\\tmp":
        t249 = true
    default:
        t249 = false
    }
    var t250 string
    var inline287 string = _goml_runtime_core_bool_to_string(t249)
    t250 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t250)
    _goml_runtime_core_string_println(inline284)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t260 string
    t260 = value__31
    _goml_runtime_core_string_println(t260)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
