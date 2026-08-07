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
        var x173 int = value__0.(Some)._0
        return x173
    default:
        return -1
    }
}

func count_to(limit__2 int) int {
    var counter__3 *ref_int_x
    var inline276 int = 0
    var inline277 *ref_int_x = ref__Ref_3int(inline276)
    counter__3 = inline277
    var jp201 int
    Loop_loop_expr202:
    for {
        var current__4 int
        var inline274 int = ref_get__Ref_3int(counter__3)
        current__4 = inline274
        var t205 bool = current__4 >= limit__2
        if t205 {
            jp201 = current__4
            break Loop_loop_expr202
        } else {
            var t204 int = current__4 + 1
            ref_set__Ref_3int(counter__3, t204)
            continue
        }
    }
    return jp201
}

func loop_option(value__5 Option__int) int {
    var jp209 int
    switch value__5.(type) {
    case Some:
        var x179 int = value__5.(Some)._0
        jp209 = x179
        return jp209
    default:
        jp209 = -2
        return jp209
    }
}

func nested_loop_value() int {
    var jp215 int
    jp215 = 7
    return jp215
}

func main0() struct{} {
    println__T_string("C:\\tmp\\\"quoted\\\"")
    var t229 string = "" + "}"
    var inline320 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t229)
    _goml_runtime_core_string_println(inline320)
    var t230 Option__int = Some{
        _0: 11,
    }
    var t231 int = unwrap_or_negative(t230)
    var t232 string
    var inline318 string = _goml_runtime_core_int_to_string(t231)
    t232 = inline318
    var inline315 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t232)
    _goml_runtime_core_string_println(inline315)
    var t233 int
    t233 = -1
    var t234 string
    var inline309 string = _goml_runtime_core_int_to_string(t233)
    t234 = inline309
    var inline306 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t234)
    _goml_runtime_core_string_println(inline306)
    var t235 int = count_to(4)
    var t236 string
    var inline304 string = _goml_runtime_core_int_to_string(t235)
    t236 = inline304
    var inline301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t236)
    _goml_runtime_core_string_println(inline301)
    var t237 Option__int = Some{
        _0: 9,
    }
    var t238 int = loop_option(t237)
    var t239 string
    var inline299 string = _goml_runtime_core_int_to_string(t238)
    t239 = inline299
    var inline296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t239)
    _goml_runtime_core_string_println(inline296)
    var t240 int = loop_option(None{})
    var t241 string
    var inline294 string = _goml_runtime_core_int_to_string(t240)
    t241 = inline294
    var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t241)
    _goml_runtime_core_string_println(inline291)
    var t242 int = nested_loop_value()
    var t243 string
    var inline289 string = _goml_runtime_core_int_to_string(t242)
    t243 = inline289
    var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t243)
    _goml_runtime_core_string_println(inline286)
    var t244 bool
    var inline284 string = "C:\\tmp"
    switch inline284 {
    case "C:\\tmp":
        t244 = true
    default:
        t244 = false
    }
    var t245 string
    var inline282 string = _goml_runtime_core_bool_to_string(t244)
    t245 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t245)
    _goml_runtime_core_string_println(inline279)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t255 string
    t255 = value__31
    _goml_runtime_core_string_println(t255)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
