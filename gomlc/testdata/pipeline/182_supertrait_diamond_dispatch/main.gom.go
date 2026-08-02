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
    var t165 Value = Value{}
    var t166 string
    var inline185 string = _goml_m_trait__impl_i_Base_i_Value_i_base(t165)
    var inline186 string = _goml_m_trait__impl_i_Left_i_Value_i_left(t165)
    var inline187 string = inline185 + inline186
    var inline188 string = _goml_m_trait__impl_i_Right_i_Value_i_right(t165)
    var inline189 string = inline187 + inline188
    var inline190 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(t165)
    var inline191 string = inline189 + inline190
    t166 = inline191
    var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline182)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
