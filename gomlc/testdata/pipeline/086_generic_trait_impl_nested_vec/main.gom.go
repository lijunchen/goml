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
    var t71 int32 = _goml_m_trait__impl_i_Size_i_Wrap____int_i_size(a__1)
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t71)
    println__T_string(t72)
    var t73 int32 = _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(b__2)
    var t74 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t73)
    println__T_string(t74)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_trait__impl_i_Size_i_Wrap____int_i_size(self__0 Wrap__int) int32 {
    var retv79 int32
    retv79 = 1
    return retv79
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv81 string
    var t82 string = _goml_runtime_core_int32_to_string(self__6)
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_Size_i_Wrap____string_i_size(self__0 Wrap__string) int32 {
    var retv84 int32
    retv84 = 1
    return retv84
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv86 string
    retv86 = self__38
    return retv86
}

func main() {
    main0()
}
