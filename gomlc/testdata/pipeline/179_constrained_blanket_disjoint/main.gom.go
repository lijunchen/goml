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
    var retv158 string
    var t159 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv158 = t159
    return retv158
}

func _goml_m_trait__impl_i_Label_i_Box____string_i_label(self__2 Box__string) string {
    var retv161 string
    var t162 string = self__2.value
    var t163 string = "string:" + t162
    retv161 = t163
    return retv161
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
    var retv169 string
    var t170 string = _goml_runtime_core_int32_to_string(self__6)
    retv169 = t170
    return retv169
}

func println__T_string(value__1 string) struct{} {
    var t172 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t172)
    return struct{}{}
}

func _goml_m_trait__impl_i_Label_i_Box____int32_i_label(self__1 Box__int32) string {
    var retv175 string
    var t176 int32 = self__1.value
    var t177 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t176)
    var t178 string = "blanket:" + t177
    retv175 = t178
    return retv175
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv180 string
    retv180 = self__38
    return retv180
}

func main() {
    main0()
}
