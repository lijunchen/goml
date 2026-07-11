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
    var retv7 string
    retv7 = "inherent"
    return retv7
}

func _goml_m_trait__impl_i_Render_i_Boxed_i_format(self__1 Boxed) string {
    var retv9 string
    var t10 int32 = self__1.value
    var t11 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t10)
    retv9 = t11
    return retv9
}

func main0() struct{} {
    var t13 Boxed = Boxed{
        value: 9,
    }
    var t14 string = _goml_m_inherent_i_Boxed_i_Boxed_i_format(t13)
    println__T_string(t14)
    var t15 Boxed = Boxed{
        value: 9,
    }
    var t16 string = _goml_m_trait__impl_i_Render_i_Boxed_i_format(t15)
    println__T_string(t16)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv18 string
    var t19 string = _goml_runtime_core_int32_to_string(self__2)
    retv18 = t19
    return retv18
}

func println__T_string(value__1 string) struct{} {
    var t21 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t21)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv24 string
    retv24 = self__9
    return retv24
}

func main() {
    main0()
}
