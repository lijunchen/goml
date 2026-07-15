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
    var t25 int32 = _goml_m_trait__impl_i_Size_i_Wrap____int32_i_size(a__1)
    var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t25)
    println__T_string(t26)
    var t27 int32 = _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(b__2)
    var t28 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t27)
    println__T_string(t28)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t30 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t30)
    return struct{}{}
}

func _goml_m_trait__impl_i_Size_i_Wrap____int32_i_size(self__0 Wrap__int32) int32 {
    var retv33 int32
    retv33 = 1
    return retv33
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv35 string
    var t36 string = _goml_runtime_core_int32_to_string(self__2)
    retv35 = t36
    return retv35
}

func _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(self__0 Wrap__string) int32 {
    var retv38 int32
    retv38 = 1
    return retv38
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv40 string
    retv40 = self__9
    return retv40
}

func main() {
    main0()
}
