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
    var inline247 int32 = 1
    a__7 = inline247
    var b__8 int32
    var inline245 int32 = 2
    b__8 = inline245
    var c__9 int32
    var inline243 int32 = a__7 + b__8
    c__9 = inline243
    var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(c__9)
    println__T_string(inline240)
    var a__10 int32
    var inline238 int32 = 3
    a__10 = inline238
    var b__11 int32
    var inline236 int32 = 4
    b__11 = inline236
    var c__12 bool
    var inline234 bool = a__10 < b__11
    c__12 = inline234
    var inline231 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(c__12)
    println__T_string(inline231)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t200 string
    t200 = value__1
    _goml_runtime_core_string_println(t200)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t204 string = _goml_runtime_core_int32_to_string(self__70)
    return t204
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t207 string = _goml_runtime_core_bool_to_string(self__64)
    return t207
}

func main() {
    main0()
}
