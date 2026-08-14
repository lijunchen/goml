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
    var t206 int32 = self__6.value
    return t206
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var t209 int32 = self__9.value
    return t209
}

func main0() struct{} {
    var t212 string
    var inline277 int32 = 3
    var inline278 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline277)
    var inline279 string = "number:" + inline278
    t212 = inline279
    var inline274 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
    _goml_runtime_core_string_println(inline274)
    var t214 string
    var inline271 string = "goml"
    var inline272 string = "text:" + inline271
    t214 = inline272
    var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline268)
    var text__12 string
    text__12 = "converted"
    var number__13 int32
    number__13 = 7
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__12)
    _goml_runtime_core_string_println(inline263)
    var inline260 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(number__13)
    _goml_runtime_core_string_println(inline260)
    var t217 Number = Number{
        value: 8,
    }
    var t218 int32
    var inline258 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(t217)
    t218 = inline258
    var inline255 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t218)
    _goml_runtime_core_string_println(inline255)
    var t219 Selected = Selected{
        value: 9,
    }
    var t220 int32
    var inline253 int32 = invoke__S_Selected__T_int32(t219)
    t220 = inline253
    var inline250 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t220)
    _goml_runtime_core_string_println(inline250)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t223 string = _goml_runtime_core_int32_to_string(self__33)
    return t223
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t240 string = _goml_runtime_core_int32_to_string(self__70)
    return t240
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var inline288 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(source__10)
    return inline288
}

func main() {
    main0()
}
