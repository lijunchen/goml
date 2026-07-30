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
    var t111 int32 = _goml_m_trait__impl_i_Size_i_Wrap____int_i_size(a__1)
    var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t111)
    println__T_string(t112)
    var t113 int32 = _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(b__2)
    var t114 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t113)
    println__T_string(t114)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t116 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t116)
    return struct{}{}
}

func _goml_m_trait__impl_i_Size_i_Wrap____int_i_size(self__0 Wrap__int) int32 {
    var retv119 int32
    retv119 = 1
    return retv119
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv121 string
    var t122 string = _goml_runtime_core_int32_to_string(self__6)
    retv121 = t122
    return retv121
}

func _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(self__0 Wrap__string) int32 {
    var retv124 int32
    retv124 = 1
    return retv124
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv126 string
    retv126 = self__38
    return retv126
}

func main() {
    main0()
}
