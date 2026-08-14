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

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    var a__7 int32
    var inline252 int32 = 1
    a__7 = inline252
    var b__8 int32
    var inline250 int32 = 2
    b__8 = inline250
    var c__9 int32
    var inline248 int32 = a__7 + b__8
    c__9 = inline248
    var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(c__9)
    println__T_string(inline245)
    var a__10 int32
    var inline243 int32 = 3
    a__10 = inline243
    var b__11 int32
    var inline241 int32 = 4
    b__11 = inline241
    var c__12 bool
    var inline239 bool = a__10 < b__11
    c__12 = inline239
    var inline236 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(c__12)
    println__T_string(inline236)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t205 string
    t205 = value__1
    _goml_runtime_core_string_println(t205)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t209 string = _goml_runtime_core_int32_to_string(self__70)
    return t209
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t212 string = _goml_runtime_core_bool_to_string(self__64)
    return t212
}

func main() {
    main0()
}
