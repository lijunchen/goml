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
    var t190 string
    var inline211 string = _goml_runtime_core_int32_to_string(self__0)
    t190 = inline211
    var t191 string = "marked:" + t190
    return t191
}

func _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(self__1 NumberBox) int32 {
    var t194 int32 = self__1.value
    return t194
}

func main0() struct{} {
    var t196 NumberBox = NumberBox{
        value: 7,
    }
    var t197 string
    var inline216 int32 = _goml_m_trait__impl_i_Container_i__l_int32_r__x40_NumberBox_i_value(t196)
    var inline217 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(inline216)
    t197 = inline217
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline213)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
