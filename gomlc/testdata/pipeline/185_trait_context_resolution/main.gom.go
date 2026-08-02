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
    var t174 int32 = self__6.value
    return t174
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var t177 int32 = self__9.value
    return t177
}

func main0() struct{} {
    var t180 string
    var inline245 int32 = 3
    var inline246 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline245)
    var inline247 string = "number:" + inline246
    t180 = inline247
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
    _goml_runtime_core_string_println(inline242)
    var t182 string
    var inline239 string = "goml"
    var inline240 string = "text:" + inline239
    t182 = inline240
    var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline236)
    var text__12 string
    text__12 = "converted"
    var number__13 int32
    number__13 = 7
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__12)
    _goml_runtime_core_string_println(inline231)
    var inline228 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(number__13)
    _goml_runtime_core_string_println(inline228)
    var t185 Number = Number{
        value: 8,
    }
    var t186 int32
    var inline226 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(t185)
    t186 = inline226
    var inline223 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t186)
    _goml_runtime_core_string_println(inline223)
    var t187 Selected = Selected{
        value: 9,
    }
    var t188 int32
    var inline221 int32 = invoke__S_Selected__T_int32(t187)
    t188 = inline221
    var inline218 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t188)
    _goml_runtime_core_string_println(inline218)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t191 string = _goml_runtime_core_int32_to_string(self__6)
    return t191
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t208 string = _goml_runtime_core_int32_to_string(self__43)
    return t208
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var inline256 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(source__10)
    return inline256
}

func main() {
    main0()
}
