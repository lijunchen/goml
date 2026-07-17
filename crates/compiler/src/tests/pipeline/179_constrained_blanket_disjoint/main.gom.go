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

type Box__string struct {
    value string
}

type Box__int32 struct {
    value int32
}

func _goml_m_trait__impl_i_Mark_i_int32_i_mark(self__0 int32) string {
    var retv61 string
    var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv61 = t62
    return retv61
}

func _goml_m_trait__impl_i_Label_i_Box____string_i_label(self__2 Box__string) string {
    var retv64 string
    var t65 string = self__2.value
    var t66 string = "string:" + t65
    retv64 = t66
    return retv64
}

func main0() struct{} {
    var t68 Box__string = Box__string{
        value: "text",
    }
    var t69 string = _goml_m_trait__impl_i_Label_i_Box____string_i_label(t68)
    println__T_string(t69)
    var t70 Box__int32 = Box__int32{
        value: 7,
    }
    var t71 string = _goml_m_trait__impl_i_Label_i_Box____int32_i_label(t70)
    println__T_string(t71)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv73 string
    var t74 string = _goml_runtime_core_int32_to_string(self__2)
    retv73 = t74
    return retv73
}

func println__T_string(value__1 string) struct{} {
    var t76 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t76)
    return struct{}{}
}

func _goml_m_trait__impl_i_Label_i_Box____int32_i_label(self__1 Box__int32) string {
    var retv79 string
    var t80 int32 = self__1.value
    var t81 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t80)
    var t82 string = "blanket:" + t81
    retv79 = t82
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv84 string
    retv84 = self__34
    return retv84
}

func main() {
    main0()
}
