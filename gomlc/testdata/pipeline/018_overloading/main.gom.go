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
    var inline242 int32 = 1
    a__7 = inline242
    var b__8 int32
    var inline240 int32 = 2
    b__8 = inline240
    var c__9 int32
    var inline238 int32 = a__7 + b__8
    c__9 = inline238
    var inline235 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(c__9)
    println__T_string(inline235)
    var a__10 int32
    var inline233 int32 = 3
    a__10 = inline233
    var b__11 int32
    var inline231 int32 = 4
    b__11 = inline231
    var c__12 bool
    var inline229 bool = a__10 < b__11
    c__12 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(c__12)
    println__T_string(inline226)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t195 string
    t195 = value__31
    _goml_runtime_core_string_println(t195)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t199 string = _goml_runtime_core_int32_to_string(self__72)
    return t199
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t202 string = _goml_runtime_core_bool_to_string(self__66)
    return t202
}

func main() {
    main0()
}
