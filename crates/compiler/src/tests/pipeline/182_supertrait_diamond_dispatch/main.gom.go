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
    var retv24 string
    retv24 = "B"
    return retv24
}

func _goml_m_trait__impl_i_Left_i_Value_i_left(self__1 Value) string {
    var retv26 string
    retv26 = "L"
    return retv26
}

func _goml_m_trait__impl_i_Right_i_Value_i_right(self__2 Value) string {
    var retv28 string
    retv28 = "R"
    return retv28
}

func _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(self__3 Value) string {
    var retv30 string
    retv30 = "D"
    return retv30
}

func main0() struct{} {
    var t32 Value = Value{}
    var t33 string = describe__T_Value(t32)
    println__T_string(t33)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t35 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t35)
    return struct{}{}
}

func describe__T_Value(value__4 Value) string {
    var retv38 string
    var t39 string = _goml_m_trait__impl_i_Base_i_Value_i_base(value__4)
    var t40 string = _goml_m_trait__impl_i_Left_i_Value_i_left(value__4)
    var t41 string = t39 + t40
    var t42 string = _goml_m_trait__impl_i_Right_i_Value_i_right(value__4)
    var t43 string = t41 + t42
    var t44 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(value__4)
    var t45 string = t43 + t44
    retv38 = t45
    return retv38
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv47 string
    retv47 = self__9
    return retv47
}

func main() {
    main0()
}
