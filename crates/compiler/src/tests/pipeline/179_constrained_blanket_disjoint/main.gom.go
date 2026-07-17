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
    var retv64 string
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv64 = t65
    return retv64
}

func _goml_m_trait__impl_i_Label_i_Box____string_i_label(self__2 Box__string) string {
    var retv67 string
    var t68 string = self__2.value
    var t69 string = "string:" + t68
    retv67 = t69
    return retv67
}

func main0() struct{} {
    var t71 Box__string = Box__string{
        value: "text",
    }
    var t72 string = _goml_m_trait__impl_i_Label_i_Box____string_i_label(t71)
    println__T_string(t72)
    var t73 Box__int32 = Box__int32{
        value: 7,
    }
    var t74 string = _goml_m_trait__impl_i_Label_i_Box____int32_i_label(t73)
    println__T_string(t74)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int32_to_string(self__5)
    retv76 = t77
    return retv76
}

func println__T_string(value__1 string) struct{} {
    var t79 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t79)
    return struct{}{}
}

func _goml_m_trait__impl_i_Label_i_Box____int32_i_label(self__1 Box__int32) string {
    var retv82 string
    var t83 int32 = self__1.value
    var t84 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t83)
    var t85 string = "blanket:" + t84
    retv82 = t85
    return retv82
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv87 string
    retv87 = self__37
    return retv87
}

func main() {
    main0()
}
