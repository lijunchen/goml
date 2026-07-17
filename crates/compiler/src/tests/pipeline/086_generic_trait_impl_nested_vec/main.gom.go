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
    var t64 int32 = _goml_m_trait__impl_i_Size_i_Wrap____int32_i_size(a__1)
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t64)
    println__T_string(t65)
    var t66 int32 = _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(b__2)
    var t67 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t66)
    println__T_string(t67)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t69)
    return struct{}{}
}

func _goml_m_trait__impl_i_Size_i_Wrap____int32_i_size(self__0 Wrap__int32) int32 {
    var retv72 int32
    retv72 = 1
    return retv72
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv74 string
    var t75 string = _goml_runtime_core_int32_to_string(self__5)
    retv74 = t75
    return retv74
}

func _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(self__0 Wrap__string) int32 {
    var retv77 int32
    retv77 = 1
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv79 string
    retv79 = self__37
    return retv79
}

func main() {
    main0()
}
