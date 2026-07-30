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
    var retv111 string
    var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv111 = t112
    return retv111
}

func _goml_m_trait__impl_i_Label_i_Box____string_i_label(self__2 Box__string) string {
    var retv114 string
    var t115 string = self__2.value
    var t116 string = "string:" + t115
    retv114 = t116
    return retv114
}

func main0() struct{} {
    var t118 Box__string = Box__string{
        value: "text",
    }
    var t119 string = _goml_m_trait__impl_i_Label_i_Box____string_i_label(t118)
    println__T_string(t119)
    var value__3 Box__int32 = Box__int32{
        value: 7,
    }
    var t120 string = _goml_m_trait__impl_i_Label_i_Box____int32_i_label(value__3)
    println__T_string(t120)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv122 string
    var t123 string = _goml_runtime_core_int32_to_string(self__6)
    retv122 = t123
    return retv122
}

func println__T_string(value__1 string) struct{} {
    var t125 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t125)
    return struct{}{}
}

func _goml_m_trait__impl_i_Label_i_Box____int32_i_label(self__1 Box__int32) string {
    var retv128 string
    var t129 int32 = self__1.value
    var t130 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t129)
    var t131 string = "blanket:" + t130
    retv128 = t131
    return retv128
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv133 string
    retv133 = self__38
    return retv133
}

func main() {
    main0()
}
