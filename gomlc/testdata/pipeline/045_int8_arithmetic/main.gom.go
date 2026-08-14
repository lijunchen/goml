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

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var a__4 int8
    a__4 = 90
    var b__5 int8
    b__5 = -20
    var c__6 int8
    c__6 = 3
    var sum__7 int8 = a__4 + b__5
    var diff__8 int8 = a__4 - c__6
    var prod__9 int8 = b__5 * c__6
    var quot__10 int8 = a__4 / c__6
    var neg__11 int8 = -b__5
    var less__12 bool = b__5 < a__4
    var inline268 string = "a="
    var inline269 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__4)
    var inline270 string = inline268 + inline269
    println__T_string(inline270)
    var inline263 string = "b="
    var inline264 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__5)
    var inline265 string = inline263 + inline264
    println__T_string(inline265)
    var inline258 string = "c="
    var inline259 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(c__6)
    var inline260 string = inline258 + inline259
    println__T_string(inline260)
    var inline253 string = "sum="
    var inline254 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(sum__7)
    var inline255 string = inline253 + inline254
    println__T_string(inline255)
    var inline248 string = "diff="
    var inline249 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(diff__8)
    var inline250 string = inline248 + inline249
    println__T_string(inline250)
    var inline243 string = "prod="
    var inline244 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(prod__9)
    var inline245 string = inline243 + inline244
    println__T_string(inline245)
    var inline238 string = "quot="
    var inline239 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(quot__10)
    var inline240 string = inline238 + inline239
    println__T_string(inline240)
    var inline233 string = "neg="
    var inline234 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(neg__11)
    var inline235 string = inline233 + inline234
    println__T_string(inline235)
    var inline228 string = "b<a="
    var inline229 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__12)
    var inline230 string = inline228 + inline229
    println__T_string(inline230)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t207 string
    t207 = value__1
    _goml_runtime_core_string_println(t207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__68 int8) string {
    var t211 string = _goml_runtime_core_int8_to_string(self__68)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t214 string = _goml_runtime_core_bool_to_string(self__64)
    return t214
}

func main() {
    main0()
}
