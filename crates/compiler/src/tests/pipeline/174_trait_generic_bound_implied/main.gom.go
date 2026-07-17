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
    var retv60 string
    var t61 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    var t62 string = "marked:" + t61
    retv60 = t62
    return retv60
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var retv64 int32
    var t65 int32 = self__1.value
    retv64 = t65
    return retv64
}

func main0() struct{} {
    var t67 NumberBox = NumberBox{
        value: 7,
    }
    var t68 string = describe__C_NumberBox__T_int32(t67)
    println__T_string(t68)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv70 string
    var t71 string = _goml_runtime_core_int32_to_string(self__2)
    retv70 = t71
    return retv70
}

func println__T_string(value__1 string) struct{} {
    var t73 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t73)
    return struct{}{}
}

func describe__C_NumberBox__T_int32(container__2 NumberBox) string {
    var retv76 string
    var t77 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(container__2)
    var t78 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t77)
    retv76 = t78
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv80 string
    retv80 = self__34
    return retv80
}

func main() {
    main0()
}
