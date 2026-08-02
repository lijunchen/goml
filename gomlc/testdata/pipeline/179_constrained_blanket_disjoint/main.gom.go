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
    var t159 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    return t159
}

func _goml_m_trait__impl_i_Label_i_Box____string_i_label(self__2 Box__string) string {
    var t162 string = self__2.value
    var t163 string = "string:" + t162
    return t163
}

func main0() struct{} {
    var t165 Box__string = Box__string{
        value: "text",
    }
    var t166 string = _goml_m_trait__impl_i_Label_i_Box____string_i_label(t165)
    println__T_string(t166)
    var value__3 Box__int32 = Box__int32{
        value: 7,
    }
    var t167 string = _goml_m_trait__impl_i_Label_i_Box____int32_i_label(value__3)
    println__T_string(t167)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t170 string = _goml_runtime_core_int32_to_string(self__6)
    return t170
}

func println__T_string(value__1 string) struct{} {
    var t172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t172)
    return struct{}{}
}

func _goml_m_trait__impl_i_Label_i_Box____int32_i_label(self__1 Box__int32) string {
    var t176 int32 = self__1.value
    var t177 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t176)
    var t178 string = "blanket:" + t177
    return t178
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
