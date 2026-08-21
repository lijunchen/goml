package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Value struct {}

type Ordering int32

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
    var t421 Value = Value{}
    var t422 string
    var inline441 string = _goml_m_trait__impl_i_Base_i_Value_i_base(t421)
    var inline442 string = _goml_m_trait__impl_i_Left_i_Value_i_left(t421)
    var inline443 string = inline441 + inline442
    var inline444 string = _goml_m_trait__impl_i_Right_i_Value_i_right(t421)
    var inline445 string = inline443 + inline444
    var inline446 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(t421)
    var inline447 string = inline445 + inline446
    t422 = inline447
    var inline438 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline438)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
