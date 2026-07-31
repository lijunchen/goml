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
    var retv155 string
    var t156 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv155 = t156
    return retv155
}

func _goml_m_trait__impl_i_Label_i_Box____string_i_label(self__2 Box__string) string {
    var retv158 string
    var t159 string = self__2.value
    var t160 string = "string:" + t159
    retv158 = t160
    return retv158
}

func main0() struct{} {
    var t162 Box__string = Box__string{
        value: "text",
    }
    var t163 string = _goml_m_trait__impl_i_Label_i_Box____string_i_label(t162)
    println__T_string(t163)
    var value__3 Box__int32 = Box__int32{
        value: 7,
    }
    var t164 string = _goml_m_trait__impl_i_Label_i_Box____int32_i_label(value__3)
    println__T_string(t164)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv166 string
    var t167 string = _goml_runtime_core_int32_to_string(self__6)
    retv166 = t167
    return retv166
}

func println__T_string(value__1 string) struct{} {
    var t169 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t169)
    return struct{}{}
}

func _goml_m_trait__impl_i_Label_i_Box____int32_i_label(self__1 Box__int32) string {
    var retv172 string
    var t173 int32 = self__1.value
    var t174 string = _goml_m_trait__impl_i_Mark_i_int32_i_mark(t173)
    var t175 string = "blanket:" + t174
    retv172 = t175
    return retv172
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv177 string
    retv177 = self__38
    return retv177
}

func main() {
    main0()
}
