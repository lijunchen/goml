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
        var x137 int = value__0.(Some)._0
        return x137
    default:
        return -1
    }
}

func count_to(limit__2 int) int {
    var counter__3 *ref_int_x
    var inline240 int = 0
    var inline241 *ref_int_x = ref__Ref_3int(inline240)
    counter__3 = inline241
    var jp165 int
    Loop_loop_expr166:
    for {
        var current__4 int
        var inline238 int = ref_get__Ref_3int(counter__3)
        current__4 = inline238
        var t169 bool = current__4 >= limit__2
        if t169 {
            jp165 = current__4
            break Loop_loop_expr166
        } else {
            var t168 int = current__4 + 1
            ref_set__Ref_3int(counter__3, t168)
            continue
        }
    }
    return jp165
}

func loop_option(value__5 Option__int) int {
    var jp173 int
    switch value__5.(type) {
    case Some:
        var x143 int = value__5.(Some)._0
        jp173 = x143
        return jp173
    default:
        jp173 = -2
        return jp173
    }
}

func nested_loop_value() int {
    var jp179 int
    jp179 = 7
    return jp179
}

func main0() struct{} {
    println__T_string("C:\\tmp\\\"quoted\\\"")
    var t193 string = "" + "}"
    var inline284 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline284)
    var t194 Option__int = Some{
        _0: 11,
    }
    var t195 int = unwrap_or_negative(t194)
    var t196 string
    var inline282 string = _goml_runtime_core_int_to_string(t195)
    t196 = inline282
    var inline279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline279)
    var t197 int
    t197 = -1
    var t198 string
    var inline273 string = _goml_runtime_core_int_to_string(t197)
    t198 = inline273
    var inline270 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline270)
    var t199 int = count_to(4)
    var t200 string
    var inline268 string = _goml_runtime_core_int_to_string(t199)
    t200 = inline268
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline265)
    var t201 Option__int = Some{
        _0: 9,
    }
    var t202 int = loop_option(t201)
    var t203 string
    var inline263 string = _goml_runtime_core_int_to_string(t202)
    t203 = inline263
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline260)
    var t204 int = loop_option(None{})
    var t205 string
    var inline258 string = _goml_runtime_core_int_to_string(t204)
    t205 = inline258
    var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline255)
    var t206 int = nested_loop_value()
    var t207 string
    var inline253 string = _goml_runtime_core_int_to_string(t206)
    t207 = inline253
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline250)
    var t208 bool
    var inline248 string = "C:\\tmp"
    switch inline248 {
    case "C:\\tmp":
        t208 = true
    default:
        t208 = false
    }
    var t209 string
    var inline246 string = _goml_runtime_core_bool_to_string(t208)
    t209 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline243)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t219 string
    t219 = value__31
    _goml_runtime_core_string_println(t219)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
