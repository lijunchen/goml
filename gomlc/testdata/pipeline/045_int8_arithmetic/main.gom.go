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
    var inline222 string = "a="
    var inline223 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__4)
    var inline224 string = inline222 + inline223
    println__T_string(inline224)
    var inline217 string = "b="
    var inline218 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__5)
    var inline219 string = inline217 + inline218
    println__T_string(inline219)
    var inline212 string = "c="
    var inline213 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(c__6)
    var inline214 string = inline212 + inline213
    println__T_string(inline214)
    var inline207 string = "sum="
    var inline208 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(sum__7)
    var inline209 string = inline207 + inline208
    println__T_string(inline209)
    var inline202 string = "diff="
    var inline203 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(diff__8)
    var inline204 string = inline202 + inline203
    println__T_string(inline204)
    var inline197 string = "prod="
    var inline198 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(prod__9)
    var inline199 string = inline197 + inline198
    println__T_string(inline199)
    var inline192 string = "quot="
    var inline193 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(quot__10)
    var inline194 string = inline192 + inline193
    println__T_string(inline194)
    var inline187 string = "neg="
    var inline188 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(neg__11)
    var inline189 string = inline187 + inline188
    println__T_string(inline189)
    var inline182 string = "b<a="
    var inline183 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__12)
    var inline184 string = inline182 + inline183
    println__T_string(inline184)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t161 string
    t161 = value__31
    _goml_runtime_core_string_println(t161)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__70 int8) string {
    var t165 string = _goml_runtime_core_int8_to_string(self__70)
    return t165
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t168 string = _goml_runtime_core_bool_to_string(self__66)
    return t168
}

func main() {
    main0()
}
