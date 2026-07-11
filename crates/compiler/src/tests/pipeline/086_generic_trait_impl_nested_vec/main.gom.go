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
    var t10 int32 = _goml_m_trait__impl_i_Size_i_Wrap____int32_i_size(a__1)
    var t11 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t10)
    println__T_string(t11)
    var t12 int32 = _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(b__2)
    var t13 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t12)
    println__T_string(t13)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t15 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t15)
    return struct{}{}
}

func _goml_m_trait__impl_i_Size_i_Wrap____int32_i_size(self__0 Wrap__int32) int32 {
    var retv18 int32
    retv18 = 1
    return retv18
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv20 string
    var t21 string = _goml_runtime_core_int32_to_string(self__2)
    retv20 = t21
    return retv20
}

func _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(self__0 Wrap__string) int32 {
    var retv23 int32
    retv23 = 1
    return retv23
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv25 string
    retv25 = self__9
    return retv25
}

func main() {
    main0()
}
