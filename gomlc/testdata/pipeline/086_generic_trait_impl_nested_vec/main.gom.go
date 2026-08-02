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
    var t158 int32 = _goml_m_trait__impl_i_Size_i_Wrap____int_i_size(a__1)
    var t159 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t158)
    println__T_string(t159)
    var t160 int32 = _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(b__2)
    var t161 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t160)
    println__T_string(t161)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t163 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t163)
    return struct{}{}
}

func _goml_m_trait__impl_i_Size_i_Wrap____int_i_size(self__0 Wrap__int) int32 {
    return 1
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t169 string = _goml_runtime_core_int32_to_string(self__6)
    return t169
}

func _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(self__0 Wrap__string) int32 {
    return 1
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
