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
    var t192 Value = Value{}
    var t193 string
    var inline212 string = _goml_m_trait__impl_i_Base_i_Value_i_base(t192)
    var inline213 string = _goml_m_trait__impl_i_Left_i_Value_i_left(t192)
    var inline214 string = inline212 + inline213
    var inline215 string = _goml_m_trait__impl_i_Right_i_Value_i_right(t192)
    var inline216 string = inline214 + inline215
    var inline217 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(t192)
    var inline218 string = inline216 + inline217
    t193 = inline218
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
