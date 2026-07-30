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
    var retv111 string
    retv111 = "inherent"
    return retv111
}

func _goml_m_trait__impl_i_Render_i_Boxed_i_format(self__1 Boxed) string {
    var retv113 string
    var t114 int32 = self__1.value
    var t115 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t114)
    retv113 = t115
    return retv113
}

func main0() struct{} {
    var t117 Boxed = Boxed{
        value: 9,
    }
    var t118 string = _goml_m_inherent_i_Boxed_i_Boxed_i_format(t117)
    println__T_string(t118)
    var t119 Boxed = Boxed{
        value: 9,
    }
    var t120 string = _goml_m_trait__impl_i_Render_i_Boxed_i_format(t119)
    println__T_string(t120)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv122 string
    var t123 string = _goml_runtime_core_int32_to_string(self__6)
    retv122 = t123
    return retv122
}

func println__T_string(value__1 string) struct{} {
    var t125 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t125)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv128 string
    retv128 = self__38
    return retv128
}

func main() {
    main0()
}
