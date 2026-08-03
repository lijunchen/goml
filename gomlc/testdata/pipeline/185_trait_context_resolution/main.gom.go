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
    var t196 int32 = self__6.value
    return t196
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var t199 int32 = self__9.value
    return t199
}

func main0() struct{} {
    var t202 string
    var inline267 int32 = 3
    var inline268 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline267)
    var inline269 string = "number:" + inline268
    t202 = inline269
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline264)
    var t204 string
    var inline261 string = "goml"
    var inline262 string = "text:" + inline261
    t204 = inline262
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t204)
    _goml_runtime_core_string_println(inline258)
    var text__12 string
    text__12 = "converted"
    var number__13 int32
    number__13 = 7
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__12)
    _goml_runtime_core_string_println(inline253)
    var inline250 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(number__13)
    _goml_runtime_core_string_println(inline250)
    var t207 Number = Number{
        value: 8,
    }
    var t208 int32
    var inline248 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(t207)
    t208 = inline248
    var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
    _goml_runtime_core_string_println(inline245)
    var t209 Selected = Selected{
        value: 9,
    }
    var t210 int32
    var inline243 int32 = invoke__S_Selected__T_int32(t209)
    t210 = inline243
    var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t210)
    _goml_runtime_core_string_println(inline240)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t213 string = _goml_runtime_core_int32_to_string(self__35)
    return t213
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t230 string = _goml_runtime_core_int32_to_string(self__72)
    return t230
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var inline278 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(source__10)
    return inline278
}

func main() {
    main0()
}
