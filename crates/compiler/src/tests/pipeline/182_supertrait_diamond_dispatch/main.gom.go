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
    var retv63 string
    retv63 = "B"
    return retv63
}

func _goml_m_trait__impl_i_Left_i_Value_i_left(self__1 Value) string {
    var retv65 string
    retv65 = "L"
    return retv65
}

func _goml_m_trait__impl_i_Right_i_Value_i_right(self__2 Value) string {
    var retv67 string
    retv67 = "R"
    return retv67
}

func _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(self__3 Value) string {
    var retv69 string
    retv69 = "D"
    return retv69
}

func main0() struct{} {
    var t71 Value = Value{}
    var t72 string = describe__T_Value(t71)
    println__T_string(t72)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func describe__T_Value(value__4 Value) string {
    var retv77 string
    var t78 string = _goml_m_trait__impl_i_Base_i_Value_i_base(value__4)
    var t79 string = _goml_m_trait__impl_i_Left_i_Value_i_left(value__4)
    var t80 string = t78 + t79
    var t81 string = _goml_m_trait__impl_i_Right_i_Value_i_right(value__4)
    var t82 string = t80 + t81
    var t83 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(value__4)
    var t84 string = t82 + t83
    retv77 = t84
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv86 string
    retv86 = self__37
    return retv86
}

func main() {
    main0()
}
