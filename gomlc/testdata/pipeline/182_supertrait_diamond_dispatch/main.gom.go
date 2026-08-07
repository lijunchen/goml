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
    var t182 Value = Value{}
    var t183 string
    var inline202 string = _goml_m_trait__impl_i_Base_i_Value_i_base(t182)
    var inline203 string = _goml_m_trait__impl_i_Left_i_Value_i_left(t182)
    var inline204 string = inline202 + inline203
    var inline205 string = _goml_m_trait__impl_i_Right_i_Value_i_right(t182)
    var inline206 string = inline204 + inline205
    var inline207 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(t182)
    var inline208 string = inline206 + inline207
    t183 = inline208
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
