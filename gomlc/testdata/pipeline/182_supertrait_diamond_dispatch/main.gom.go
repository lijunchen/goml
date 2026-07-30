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
    var retv110 string
    retv110 = "B"
    return retv110
}

func _goml_m_trait__impl_i_Left_i_Value_i_left(self__1 Value) string {
    var retv112 string
    retv112 = "L"
    return retv112
}

func _goml_m_trait__impl_i_Right_i_Value_i_right(self__2 Value) string {
    var retv114 string
    retv114 = "R"
    return retv114
}

func _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(self__3 Value) string {
    var retv116 string
    retv116 = "D"
    return retv116
}

func main0() struct{} {
    var t118 Value = Value{}
    var t119 string = describe__T_Value(t118)
    println__T_string(t119)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t121 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t121)
    return struct{}{}
}

func describe__T_Value(value__4 Value) string {
    var retv124 string
    var t125 string = _goml_m_trait__impl_i_Base_i_Value_i_base(value__4)
    var t126 string = _goml_m_trait__impl_i_Left_i_Value_i_left(value__4)
    var t127 string = t125 + t126
    var t128 string = _goml_m_trait__impl_i_Right_i_Value_i_right(value__4)
    var t129 string = t127 + t128
    var t130 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(value__4)
    var t131 string = t129 + t130
    retv124 = t131
    return retv124
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv133 string
    retv133 = self__38
    return retv133
}

func main() {
    main0()
}
