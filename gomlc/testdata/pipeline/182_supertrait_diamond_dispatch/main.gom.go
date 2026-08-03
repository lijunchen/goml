package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Value struct {}

func _goml_m_trait__impl_i_Base_i_Value_i_base(self__0 Value) string {
    return "B"
}

func _goml_m_trait__impl_i_Left_i_Value_i_left(self__1 Value) string {
    return "L"
}

func _goml_m_trait__impl_i_Right_i_Value_i_right(self__2 Value) string {
    return "R"
}

func _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(self__3 Value) string {
    return "D"
}

func main0() struct{} {
    var t187 Value = Value{}
    var t188 string
    var inline207 string = _goml_m_trait__impl_i_Base_i_Value_i_base(t187)
    var inline208 string = _goml_m_trait__impl_i_Left_i_Value_i_left(t187)
    var inline209 string = inline207 + inline208
    var inline210 string = _goml_m_trait__impl_i_Right_i_Value_i_right(t187)
    var inline211 string = inline209 + inline210
    var inline212 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(t187)
    var inline213 string = inline211 + inline212
    t188 = inline213
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
