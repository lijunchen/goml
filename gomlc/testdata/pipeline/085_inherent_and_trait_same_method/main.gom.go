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
    var retv158 string
    retv158 = "inherent"
    return retv158
}

func _goml_m_trait__impl_i_Render_i_Boxed_i_format(self__1 Boxed) string {
    var retv160 string
    var t161 int32 = self__1.value
    var t162 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t161)
    retv160 = t162
    return retv160
}

func main0() struct{} {
    var t164 Boxed = Boxed{
        value: 9,
    }
    var t165 string = _goml_m_inherent_i_Boxed_i_Boxed_i_format(t164)
    println__T_string(t165)
    var t166 Boxed = Boxed{
        value: 9,
    }
    var t167 string = _goml_m_trait__impl_i_Render_i_Boxed_i_format(t166)
    println__T_string(t167)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv169 string
    var t170 string = _goml_runtime_core_int32_to_string(self__6)
    retv169 = t170
    return retv169
}

func println__T_string(value__1 string) struct{} {
    var t172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t172)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv175 string
    retv175 = self__38
    return retv175
}

func main() {
    main0()
}
