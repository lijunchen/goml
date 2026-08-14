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
    var t197 Value = Value{}
    var t198 string
    var inline217 string = _goml_m_trait__impl_i_Base_i_Value_i_base(t197)
    var inline218 string = _goml_m_trait__impl_i_Left_i_Value_i_left(t197)
    var inline219 string = inline217 + inline218
    var inline220 string = _goml_m_trait__impl_i_Right_i_Value_i_right(t197)
    var inline221 string = inline219 + inline220
    var inline222 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(t197)
    var inline223 string = inline221 + inline222
    t198 = inline223
    var inline214 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline214)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
