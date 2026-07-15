package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Boxed struct {
    value int32
}

func _goml_m_inherent_i_Boxed_i_Boxed_i_format(self__0 Boxed) string {
    var retv25 string
    retv25 = "inherent"
    return retv25
}

func _goml_m_trait__impl_i_Render_i_Boxed_i_format(self__1 Boxed) string {
    var retv27 string
    var t28 int32 = self__1.value
    var t29 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t28)
    retv27 = t29
    return retv27
}

func main0() struct{} {
    var t31 Boxed = Boxed{
        value: 9,
    }
    var t32 string = _goml_m_inherent_i_Boxed_i_Boxed_i_format(t31)
    println__T_string(t32)
    var t33 Boxed = Boxed{
        value: 9,
    }
    var t34 string = _goml_m_trait__impl_i_Render_i_Boxed_i_format(t33)
    println__T_string(t34)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv36 string
    var t37 string = _goml_runtime_core_int32_to_string(self__2)
    retv36 = t37
    return retv36
}

func println__T_string(value__1 string) struct{} {
    var t39 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t39)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv42 string
    retv42 = self__9
    return retv42
}

func main() {
    main0()
}
