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

type Wrap__int32 struct {
    value int32
}

type Wrap__string struct {
    value string
}

func main0() struct{} {
    var a__1 Wrap__int32 = Wrap__int32{
        value: 1,
    }
    var b__2 Wrap__string = Wrap__string{
        value: "x",
    }
    var t7 int32 = _goml_m_trait__impl_i_Size_i_Wrap____int32_i_size(a__1)
    var t8 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t7)
    println__T_string(t8)
    var t9 int32 = _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(b__2)
    var t10 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t9)
    println__T_string(t10)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t12 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t12)
    return struct{}{}
}

func _goml_m_trait__impl_i_Size_i_Wrap____int32_i_size(self__0 Wrap__int32) int32 {
    var retv15 int32
    retv15 = 1
    return retv15
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv17 string
    var t18 string = _goml_runtime_core_int32_to_string(self__2)
    retv17 = t18
    return retv17
}

func _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(self__0 Wrap__string) int32 {
    var retv20 int32
    retv20 = 1
    return retv20
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv22 string
    retv22 = self__9
    return retv22
}

func main() {
    main0()
}
