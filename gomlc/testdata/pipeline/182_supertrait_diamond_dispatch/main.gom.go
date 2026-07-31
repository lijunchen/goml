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
    var retv154 string
    retv154 = "B"
    return retv154
}

func _goml_m_trait__impl_i_Left_i_Value_i_left(self__1 Value) string {
    var retv156 string
    retv156 = "L"
    return retv156
}

func _goml_m_trait__impl_i_Right_i_Value_i_right(self__2 Value) string {
    var retv158 string
    retv158 = "R"
    return retv158
}

func _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(self__3 Value) string {
    var retv160 string
    retv160 = "D"
    return retv160
}

func main0() struct{} {
    var t162 Value = Value{}
    var t163 string = describe__T_Value(t162)
    println__T_string(t163)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t165 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t165)
    return struct{}{}
}

func describe__T_Value(value__4 Value) string {
    var retv168 string
    var t169 string = _goml_m_trait__impl_i_Base_i_Value_i_base(value__4)
    var t170 string = _goml_m_trait__impl_i_Left_i_Value_i_left(value__4)
    var t171 string = t169 + t170
    var t172 string = _goml_m_trait__impl_i_Right_i_Value_i_right(value__4)
    var t173 string = t171 + t172
    var t174 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(value__4)
    var t175 string = t173 + t174
    retv168 = t175
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv177 string
    retv177 = self__38
    return retv177
}

func main() {
    main0()
}
