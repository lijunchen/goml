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
    var inline241 string = "a="
    var inline242 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__4)
    var inline243 string = inline241 + inline242
    println__T_string(inline243)
    var inline236 string = "b="
    var inline237 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__5)
    var inline238 string = inline236 + inline237
    println__T_string(inline238)
    var inline231 string = "c="
    var inline232 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(c__6)
    var inline233 string = inline231 + inline232
    println__T_string(inline233)
    var inline226 string = "sum="
    var inline227 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(sum__7)
    var inline228 string = inline226 + inline227
    println__T_string(inline228)
    var inline221 string = "diff="
    var inline222 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(diff__8)
    var inline223 string = inline221 + inline222
    println__T_string(inline223)
    var inline216 string = "prod="
    var inline217 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(prod__9)
    var inline218 string = inline216 + inline217
    println__T_string(inline218)
    var inline211 string = "quot="
    var inline212 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(quot__10)
    var inline213 string = inline211 + inline212
    println__T_string(inline213)
    var inline206 string = "neg="
    var inline207 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(neg__11)
    var inline208 string = inline206 + inline207
    println__T_string(inline208)
    var inline201 string = "b<a="
    var inline202 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__12)
    var inline203 string = inline201 + inline202
    println__T_string(inline203)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t180 string
    t180 = value__1
    _goml_runtime_core_string_println(t180)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__41 int8) string {
    var t184 string = _goml_runtime_core_int8_to_string(self__41)
    return t184
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t187 string = _goml_runtime_core_bool_to_string(self__37)
    return t187
}

func main() {
    main0()
}
