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
    var retv60 string
    retv60 = "B"
    return retv60
}

func _goml_m_trait__impl_i_Left_i_Value_i_left(self__1 Value) string {
    var retv62 string
    retv62 = "L"
    return retv62
}

func _goml_m_trait__impl_i_Right_i_Value_i_right(self__2 Value) string {
    var retv64 string
    retv64 = "R"
    return retv64
}

func _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(self__3 Value) string {
    var retv66 string
    retv66 = "D"
    return retv66
}

func main0() struct{} {
    var t68 Value = Value{}
    var t69 string = describe__T_Value(t68)
    println__T_string(t69)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t71)
    return struct{}{}
}

func describe__T_Value(value__4 Value) string {
    var retv74 string
    var t75 string = _goml_m_trait__impl_i_Base_i_Value_i_base(value__4)
    var t76 string = _goml_m_trait__impl_i_Left_i_Value_i_left(value__4)
    var t77 string = t75 + t76
    var t78 string = _goml_m_trait__impl_i_Right_i_Value_i_right(value__4)
    var t79 string = t77 + t78
    var t80 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(value__4)
    var t81 string = t79 + t80
    retv74 = t81
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv83 string
    retv83 = self__34
    return retv83
}

func main() {
    main0()
}
