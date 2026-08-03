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
    var t146 Value = Value{}
    var t147 string
    var inline166 string = _goml_m_trait__impl_i_Base_i_Value_i_base(t146)
    var inline167 string = _goml_m_trait__impl_i_Left_i_Value_i_left(t146)
    var inline168 string = inline166 + inline167
    var inline169 string = _goml_m_trait__impl_i_Right_i_Value_i_right(t146)
    var inline170 string = inline168 + inline169
    var inline171 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(t146)
    var inline172 string = inline170 + inline171
    t147 = inline172
    var inline163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t147)
    _goml_runtime_core_string_println(inline163)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
