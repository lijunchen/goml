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
    var t418 Value = Value{}
    var t419 string
    var inline438 string = _goml_m_trait__impl_i_Base_i_Value_i_base(t418)
    var inline439 string = _goml_m_trait__impl_i_Left_i_Value_i_left(t418)
    var inline440 string = inline438 + inline439
    var inline441 string = _goml_m_trait__impl_i_Right_i_Value_i_right(t418)
    var inline442 string = inline440 + inline441
    var inline443 string = _goml_m_trait__impl_i_Diamond_i_Value_i_diamond(t418)
    var inline444 string = inline442 + inline443
    t419 = inline444
    var inline435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline435)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
