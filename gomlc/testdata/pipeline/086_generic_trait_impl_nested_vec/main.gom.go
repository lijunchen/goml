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

type Wrap__int struct {
    value int
}

type Wrap__string struct {
    value string
}

func main0() struct{} {
    var a__1 Wrap__int = Wrap__int{
        value: 1,
    }
    var b__2 Wrap__string = Wrap__string{
        value: "x",
    }
    var t155 int32 = _goml_m_trait__impl_i_Size_i_Wrap____int_i_size(a__1)
    var t156 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t155)
    println__T_string(t156)
    var t157 int32 = _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(b__2)
    var t158 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t157)
    println__T_string(t158)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t160 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t160)
    return struct{}{}
}

func _goml_m_trait__impl_i_Size_i_Wrap____int_i_size(self__0 Wrap__int) int32 {
    var retv163 int32
    retv163 = 1
    return retv163
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv165 string
    var t166 string = _goml_runtime_core_int32_to_string(self__6)
    retv165 = t166
    return retv165
}

func _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(self__0 Wrap__string) int32 {
    var retv168 int32
    retv168 = 1
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv170 string
    retv170 = self__38
    return retv170
}

func main() {
    main0()
}
