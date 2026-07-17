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

type NumberBox struct {
    value int32
}

func _goml_m_trait__impl_i_Mark_i_int32_i_mark(self__0 int32) string {
    var retv63 string
    var t64 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t65 string = "marked:" + t64
    retv63 = t65
    return retv63
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var retv67 int32
    var t68 int32 = self__1.value
    retv67 = t68
    return retv67
}

func main0() struct{} {
    var t70 NumberBox = NumberBox{
        value: 7,
    }
    var t71 string = describe__C_NumberBox__T_int32(t70)
    println__T_string(t71)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int32_to_string(self__5)
    retv73 = t74
    return retv73
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func describe__C_NumberBox__T_int32(container__2 NumberBox) string {
    var retv79 string
    var t80 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(container__2)
    var t81 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t80)
    retv79 = t81
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv83 string
    retv83 = self__37
    return retv83
}

func main() {
    main0()
}
