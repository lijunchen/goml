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
    var inline220 int32 = 1
    a__7 = inline220
    var b__8 int32
    var inline218 int32 = 2
    b__8 = inline218
    var c__9 int32
    var inline216 int32 = a__7 + b__8
    c__9 = inline216
    var inline213 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(c__9)
    println__T_string(inline213)
    var a__10 int32
    var inline211 int32 = 3
    a__10 = inline211
    var b__11 int32
    var inline209 int32 = 4
    b__11 = inline209
    var c__12 bool
    var inline207 bool = a__10 < b__11
    c__12 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(c__12)
    println__T_string(inline204)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t173 string
    t173 = value__1
    _goml_runtime_core_string_println(t173)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t177 string = _goml_runtime_core_int32_to_string(self__43)
    return t177
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t180 string = _goml_runtime_core_bool_to_string(self__37)
    return t180
}

func main() {
    main0()
}
