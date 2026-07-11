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
    var retv10 string
    retv10 = "inherent"
    return retv10
}

func _goml_m_trait__impl_i_Render_i_Boxed_i_format(self__1 Boxed) string {
    var retv12 string
    var t13 int32 = self__1.value
    var t14 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t13)
    retv12 = t14
    return retv12
}

func main0() struct{} {
    var t16 Boxed = Boxed{
        value: 9,
    }
    var t17 string = _goml_m_inherent_i_Boxed_i_Boxed_i_format(t16)
    println__T_string(t17)
    var t18 Boxed = Boxed{
        value: 9,
    }
    var t19 string = _goml_m_trait__impl_i_Render_i_Boxed_i_format(t18)
    println__T_string(t19)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv21 string
    var t22 string = _goml_runtime_core_int32_to_string(self__2)
    retv21 = t22
    return retv21
}

func println__T_string(value__1 string) struct{} {
    var t24 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t24)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv27 string
    retv27 = self__9
    return retv27
}

func main() {
    main0()
}
