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
    var retv66 string
    retv66 = "B"
    return retv66
}

func _goml_m_trait__impl_i_Left_i_Value_i_left(self__1 Value) string {
    var retv68 string
    retv68 = "L"
    return retv68
}

func _goml_m_trait__impl_i_Right_i_Value_i_right(self__2 Value) string {
    var retv70 string
    retv70 = "R"
    return retv70
}

func _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(self__3 Value) string {
    var retv72 string
    retv72 = "D"
    return retv72
}

func main0() struct{} {
    var t74 Value = Value{}
    var t75 string = describe__T_Value(t74)
    println__T_string(t75)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func describe__T_Value(value__4 Value) string {
    var retv80 string
    var t81 string = _goml_m_trait__impl_i_Base_i_Value_i_base(value__4)
    var t82 string = _goml_m_trait__impl_i_Left_i_Value_i_left(value__4)
    var t83 string = t81 + t82
    var t84 string = _goml_m_trait__impl_i_Right_i_Value_i_right(value__4)
    var t85 string = t83 + t84
    var t86 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(value__4)
    var t87 string = t85 + t86
    retv80 = t87
    return retv80
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv89 string
    retv89 = self__38
    return retv89
}

func main() {
    main0()
}
