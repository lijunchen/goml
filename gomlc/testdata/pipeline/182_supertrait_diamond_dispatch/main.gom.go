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
    var retv70 string
    retv70 = "B"
    return retv70
}

func _goml_m_trait__impl_i_Left_i_Value_i_left(self__1 Value) string {
    var retv72 string
    retv72 = "L"
    return retv72
}

func _goml_m_trait__impl_i_Right_i_Value_i_right(self__2 Value) string {
    var retv74 string
    retv74 = "R"
    return retv74
}

func _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(self__3 Value) string {
    var retv76 string
    retv76 = "D"
    return retv76
}

func main0() struct{} {
    var t78 Value = Value{}
    var t79 string = describe__T_Value(t78)
    println__T_string(t79)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func describe__T_Value(value__4 Value) string {
    var retv84 string
    var t85 string = _goml_m_trait__impl_i_Base_i_Value_i_base(value__4)
    var t86 string = _goml_m_trait__impl_i_Left_i_Value_i_left(value__4)
    var t87 string = t85 + t86
    var t88 string = _goml_m_trait__impl_i_Right_i_Value_i_right(value__4)
    var t89 string = t87 + t88
    var t90 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(value__4)
    var t91 string = t89 + t90
    retv84 = t91
    return retv84
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv93 string
    retv93 = self__38
    return retv93
}

func main() {
    main0()
}
