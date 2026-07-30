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
    var retv70 string
    var t71 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t72 string = "marked:" + t71
    retv70 = t72
    return retv70
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var retv74 int32
    var t75 int32 = self__1.value
    retv74 = t75
    return retv74
}

func main0() struct{} {
    var t77 NumberBox = NumberBox{
        value: 7,
    }
    var t78 string = describe__C_NumberBox__T_int32(t77)
    println__T_string(t78)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv80 string
    var t81 string = _goml_runtime_core_int32_to_string(self__6)
    retv80 = t81
    return retv80
}

func println__T_string(value__1 string) struct{} {
    var t83 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t83)
    return struct{}{}
}

func describe__C_NumberBox__T_int32(container__2 NumberBox) string {
    var retv86 string
    var t87 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(container__2)
    var t88 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t87)
    retv86 = t88
    return retv86
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv90 string
    retv90 = self__38
    return retv90
}

func main() {
    main0()
}
