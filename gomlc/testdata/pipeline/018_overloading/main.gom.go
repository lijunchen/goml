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
    var inline237 int32 = 1
    a__7 = inline237
    var b__8 int32
    var inline235 int32 = 2
    b__8 = inline235
    var c__9 int32
    var inline233 int32 = a__7 + b__8
    c__9 = inline233
    var inline230 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(c__9)
    println__T_string(inline230)
    var a__10 int32
    var inline228 int32 = 3
    a__10 = inline228
    var b__11 int32
    var inline226 int32 = 4
    b__11 = inline226
    var c__12 bool
    var inline224 bool = a__10 < b__11
    c__12 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(c__12)
    println__T_string(inline221)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t190 string
    t190 = value__31
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t194 string = _goml_runtime_core_int32_to_string(self__72)
    return t194
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t197 string = _goml_runtime_core_bool_to_string(self__66)
    return t197
}

func main() {
    main0()
}
