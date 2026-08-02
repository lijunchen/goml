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
        var x156 int = value__0.(Some)._0
        return x156
    default:
        return -1
    }
}

func count_to(limit__2 int) int {
    var counter__3 *ref_int_x
    var inline259 int = 0
    var inline260 *ref_int_x = ref__Ref_3int(inline259)
    counter__3 = inline260
    var jp184 int
    Loop_loop_expr185:
    for {
        var current__4 int
        var inline257 int = ref_get__Ref_3int(counter__3)
        current__4 = inline257
        var t188 bool = current__4 >= limit__2
        if t188 {
            jp184 = current__4
            break Loop_loop_expr185
        } else {
            var t187 int = current__4 + 1
            ref_set__Ref_3int(counter__3, t187)
            continue
        }
    }
    return jp184
}

func loop_option(value__5 Option__int) int {
    var jp192 int
    switch value__5.(type) {
    case Some:
        var x162 int = value__5.(Some)._0
        jp192 = x162
        return jp192
    default:
        jp192 = -2
        return jp192
    }
}

func nested_loop_value() int {
    var jp198 int
    jp198 = 7
    return jp198
}

func main0() struct{} {
    _goml_runtime_core_string_println("C:\\tmp\\\"quoted\\\"")
    var t212 string = "" + "}"
    _goml_runtime_core_string_println(t212)
    var t213 Option__int = Some{
        _0: 11,
    }
    var t214 int = unwrap_or_negative(t213)
    var t215 string
    var inline301 string = _goml_runtime_core_int_to_string(t214)
    t215 = inline301
    var inline298 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
    _goml_runtime_core_string_println(inline298)
    var t216 int
    t216 = -1
    var t217 string
    var inline292 string = _goml_runtime_core_int_to_string(t216)
    t217 = inline292
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t217)
    _goml_runtime_core_string_println(inline289)
    var t218 int = count_to(4)
    var t219 string
    var inline287 string = _goml_runtime_core_int_to_string(t218)
    t219 = inline287
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t219)
    _goml_runtime_core_string_println(inline284)
    var t220 Option__int = Some{
        _0: 9,
    }
    var t221 int = loop_option(t220)
    var t222 string
    var inline282 string = _goml_runtime_core_int_to_string(t221)
    t222 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t222)
    _goml_runtime_core_string_println(inline279)
    var t223 int = loop_option(None{})
    var t224 string
    var inline277 string = _goml_runtime_core_int_to_string(t223)
    t224 = inline277
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t224)
    _goml_runtime_core_string_println(inline274)
    var t225 int = nested_loop_value()
    var t226 string
    var inline272 string = _goml_runtime_core_int_to_string(t225)
    t226 = inline272
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline269)
    var t227 bool
    var inline267 string = "C:\\tmp"
    switch inline267 {
    case "C:\\tmp":
        t227 = true
    default:
        t227 = false
    }
    var t228 string
    var inline265 string = _goml_runtime_core_bool_to_string(t227)
    t228 = inline265
    var inline262 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t228)
    _goml_runtime_core_string_println(inline262)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
