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

func _goml_m_trait__impl_i_Put_i__l_int32_r__x40_Target_i_put(self__0 Target, value__1 int32) string {
    var retv162 string
    var t163 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t164 string = "number:" + t163
    retv162 = t164
    return retv162
}

func _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(self__2 Target, value__3 string) string {
    var retv166 string
    var t167 string = "text:" + value__3
    retv166 = t167
    return retv166
}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(self__4 Convertible) string {
    var retv169 string
    retv169 = "converted"
    return retv169
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(self__5 Convertible) int32 {
    var retv171 int32
    retv171 = 7
    return retv171
}

func _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(self__6 Number) int32 {
    var retv173 int32
    var t174 int32 = self__6.value
    retv173 = t174
    return retv173
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var retv176 int32
    var t177 int32 = self__9.value
    retv176 = t177
    return retv176
}

func main0() struct{} {
    var t179 Target = Target{}
    var t180 string = _goml_m_trait__impl_i_Put_i__l_int32_r__x40_Target_i_put(t179, 3)
    println__T_string(t180)
    var t181 Target = Target{}
    var t182 string = _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(t181, "goml")
    println__T_string(t182)
    var t183 Convertible = Convertible{}
    var text__12 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(t183)
    var t184 Convertible = Convertible{}
    var number__13 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(t184)
    println__T_string(text__12)
    println__T_int32(number__13)
    var t185 Number = Number{
        value: 8,
    }
    var t186 int32 = read__S_Number__T_int32(t185)
    println__T_int32(t186)
    var t187 Selected = Selected{
        value: 9,
    }
    var t188 int32 = select_item__S_Selected(t187)
    println__T_int32(t188)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv190 string
    var t191 string = _goml_runtime_core_int32_to_string(self__6)
    retv190 = t191
    return retv190
}

func println__T_string(value__1 string) struct{} {
    var t193 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t193)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t196 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t196)
    return struct{}{}
}

func read__S_Number__T_int32(source__7 Number) int32 {
    var retv199 int32
    var t200 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(source__7)
    retv199 = t200
    return retv199
}

func select_item__S_Selected(source__11 Selected) int32 {
    var retv202 int32
    var t203 int32 = invoke__S_Selected__T_int32(source__11)
    retv202 = t203
    return retv202
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv205 string
    retv205 = self__38
    return retv205
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv207 string
    var t208 string = _goml_runtime_core_int32_to_string(self__43)
    retv207 = t208
    return retv207
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var retv210 int32
    var t211 int32 = _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(source__10)
    retv210 = t211
    return retv210
}

func _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(self__8 Selected) int32 {
    var retv213 int32
    var t214 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__8)
    retv213 = t214
    return retv213
}

func main() {
    main0()
}
