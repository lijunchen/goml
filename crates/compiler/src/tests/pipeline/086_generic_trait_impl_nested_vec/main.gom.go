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
    var t61 int32 = _goml_m_trait__impl_i_Size_i_Wrap____int32_i_size(a__1)
    var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t61)
    println__T_string(t62)
    var t63 int32 = _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(b__2)
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t63)
    println__T_string(t64)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t66 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t66)
    return struct{}{}
}

func _goml_m_trait__impl_i_Size_i_Wrap____int32_i_size(self__0 Wrap__int32) int32 {
    var retv69 int32
    retv69 = 1
    return retv69
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv71 string
    var t72 string = _goml_runtime_core_int32_to_string(self__2)
    retv71 = t72
    return retv71
}

func _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(self__0 Wrap__string) int32 {
    var retv74 int32
    retv74 = 1
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv76 string
    retv76 = self__34
    return retv76
}

func main() {
    main0()
}
