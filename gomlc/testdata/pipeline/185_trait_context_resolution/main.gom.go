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
    var t191 int32 = self__6.value
    return t191
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var t194 int32 = self__9.value
    return t194
}

func main0() struct{} {
    var t197 string
    var inline262 int32 = 3
    var inline263 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline262)
    var inline264 string = "number:" + inline263
    t197 = inline264
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
    _goml_runtime_core_string_println(inline259)
    var t199 string
    var inline256 string = "goml"
    var inline257 string = "text:" + inline256
    t199 = inline257
    var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t199)
    _goml_runtime_core_string_println(inline253)
    var text__12 string
    text__12 = "converted"
    var number__13 int32
    number__13 = 7
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__12)
    _goml_runtime_core_string_println(inline248)
    var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(number__13)
    _goml_runtime_core_string_println(inline245)
    var t202 Number = Number{
        value: 8,
    }
    var t203 int32
    var inline243 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(t202)
    t203 = inline243
    var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
    _goml_runtime_core_string_println(inline240)
    var t204 Selected = Selected{
        value: 9,
    }
    var t205 int32
    var inline238 int32 = invoke__S_Selected__T_int32(t204)
    t205 = inline238
    var inline235 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t205)
    _goml_runtime_core_string_println(inline235)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t208 string = _goml_runtime_core_int32_to_string(self__33)
    return t208
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t225 string = _goml_runtime_core_int32_to_string(self__70)
    return t225
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var inline273 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(source__10)
    return inline273
}

func main() {
    main0()
}
