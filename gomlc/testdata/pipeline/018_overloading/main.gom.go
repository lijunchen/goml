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
    var inline201 int32 = 1
    a__7 = inline201
    var b__8 int32
    var inline199 int32 = 2
    b__8 = inline199
    var c__9 int32
    var inline197 int32 = a__7 + b__8
    c__9 = inline197
    var inline194 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(c__9)
    println__T_string(inline194)
    var a__10 int32
    var inline192 int32 = 3
    a__10 = inline192
    var b__11 int32
    var inline190 int32 = 4
    b__11 = inline190
    var c__12 bool
    var inline188 bool = a__10 < b__11
    c__12 = inline188
    var inline185 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(c__12)
    println__T_string(inline185)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t154 string
    t154 = value__31
    _goml_runtime_core_string_println(t154)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t158 string = _goml_runtime_core_int32_to_string(self__72)
    return t158
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t161 string = _goml_runtime_core_bool_to_string(self__66)
    return t161
}

func main() {
    main0()
}
