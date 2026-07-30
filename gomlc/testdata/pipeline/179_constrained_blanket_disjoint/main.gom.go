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
    var retv71 string
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv71 = t72
    return retv71
}

func _goml_m_trait__impl_i_Label_i_Box____string_i_label(self__2 Box__string) string {
    var retv74 string
    var t75 string = self__2.value
    var t76 string = "string:" + t75
    retv74 = t76
    return retv74
}

func main0() struct{} {
    var t78 Box__string = Box__string{
        value: "text",
    }
    var t79 string = _goml_m_trait__impl_i_Label_i_Box____string_i_label(t78)
    println__T_string(t79)
    var value__3 Box__int32 = Box__int32{
        value: 7,
    }
    var t80 string = _goml_m_trait__impl_i_Label_i_Box____int32_i_label(value__3)
    println__T_string(t80)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv82 string
    var t83 string = _goml_runtime_core_int32_to_string(self__6)
    retv82 = t83
    return retv82
}

func println__T_string(value__1 string) struct{} {
    var t85 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t85)
    return struct{}{}
}

func _goml_m_trait__impl_i_Label_i_Box____int32_i_label(self__1 Box__int32) string {
    var retv88 string
    var t89 int32 = self__1.value
    var t90 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t89)
    var t91 string = "blanket:" + t90
    retv88 = t91
    return retv88
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv93 string
    retv93 = self__38
    return retv93
}

func main() {
    main0()
}
