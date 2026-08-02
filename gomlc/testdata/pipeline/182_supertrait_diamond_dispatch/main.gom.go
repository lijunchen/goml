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
    var t166 string = describe__T_Value(t165)
    println__T_string(t166)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func describe__T_Value(value__4 Value) string {
    var t172 string = _goml_m_trait__impl_i_Base_i_Value_i_base(value__4)
    var t173 string = _goml_m_trait__impl_i_Left_i_Value_i_left(value__4)
    var t174 string = t172 + t173
    var t175 string = _goml_m_trait__impl_i_Right_i_Value_i_right(value__4)
    var t176 string = t174 + t175
    var t177 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(value__4)
    var t178 string = t176 + t177
    return t178
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
