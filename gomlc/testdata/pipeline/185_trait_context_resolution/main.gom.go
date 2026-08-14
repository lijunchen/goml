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
    var t201 int32 = self__6.value
    return t201
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var t204 int32 = self__9.value
    return t204
}

func main0() struct{} {
    var t207 string
    var inline272 int32 = 3
    var inline273 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline272)
    var inline274 string = "number:" + inline273
    t207 = inline274
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline269)
    var t209 string
    var inline266 string = "goml"
    var inline267 string = "text:" + inline266
    t209 = inline267
    var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t209)
    _goml_runtime_core_string_println(inline263)
    var text__12 string
    text__12 = "converted"
    var number__13 int32
    number__13 = 7
    var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__12)
    _goml_runtime_core_string_println(inline258)
    var inline255 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(number__13)
    _goml_runtime_core_string_println(inline255)
    var t212 Number = Number{
        value: 8,
    }
    var t213 int32
    var inline253 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(t212)
    t213 = inline253
    var inline250 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
    _goml_runtime_core_string_println(inline250)
    var t214 Selected = Selected{
        value: 9,
    }
    var t215 int32
    var inline248 int32 = invoke__S_Selected__T_int32(t214)
    t215 = inline248
    var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t215)
    _goml_runtime_core_string_println(inline245)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t218 string = _goml_runtime_core_int32_to_string(self__33)
    return t218
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t235 string = _goml_runtime_core_int32_to_string(self__70)
    return t235
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var inline283 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(source__10)
    return inline283
}

func main() {
    main0()
}
