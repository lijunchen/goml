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

type Target struct {}

type Convertible struct {}

type Number struct {
    value int32
}

type Selected struct {
    value int32
}

func _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(self__6 Number) int32 {
    var t155 int32 = self__6.value
    return t155
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var t158 int32 = self__9.value
    return t158
}

func main0() struct{} {
    var t161 string
    var inline226 int32 = 3
    var inline227 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline226)
    var inline228 string = "number:" + inline227
    t161 = inline228
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t161)
    _goml_runtime_core_string_println(inline223)
    var t163 string
    var inline220 string = "goml"
    var inline221 string = "text:" + inline220
    t163 = inline221
    var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline217)
    var text__12 string
    text__12 = "converted"
    var number__13 int32
    number__13 = 7
    var inline212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__12)
    _goml_runtime_core_string_println(inline212)
    var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(number__13)
    _goml_runtime_core_string_println(inline209)
    var t166 Number = Number{
        value: 8,
    }
    var t167 int32
    var inline207 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(t166)
    t167 = inline207
    var inline204 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t167)
    _goml_runtime_core_string_println(inline204)
    var t168 Selected = Selected{
        value: 9,
    }
    var t169 int32
    var inline202 int32 = invoke__S_Selected__T_int32(t168)
    t169 = inline202
    var inline199 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t169)
    _goml_runtime_core_string_println(inline199)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t172 string = _goml_runtime_core_int32_to_string(self__35)
    return t172
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t189 string = _goml_runtime_core_int32_to_string(self__72)
    return t189
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var inline237 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(source__10)
    return inline237
}

func main() {
    main0()
}
