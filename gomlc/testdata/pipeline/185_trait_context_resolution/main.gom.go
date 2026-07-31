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
    var retv159 string
    var t160 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t161 string = "number:" + t160
    retv159 = t161
    return retv159
}

func _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(self__2 Target, value__3 string) string {
    var retv163 string
    var t164 string = "text:" + value__3
    retv163 = t164
    return retv163
}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(self__4 Convertible) string {
    var retv166 string
    retv166 = "converted"
    return retv166
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(self__5 Convertible) int32 {
    var retv168 int32
    retv168 = 7
    return retv168
}

func _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(self__6 Number) int32 {
    var retv170 int32
    var t171 int32 = self__6.value
    retv170 = t171
    return retv170
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var retv173 int32
    var t174 int32 = self__9.value
    retv173 = t174
    return retv173
}

func main0() struct{} {
    var t176 Target = Target{}
    var t177 string = _goml_m_trait__impl_i_Put_i__l_int32_r__x40_Target_i_put(t176, 3)
    println__T_string(t177)
    var t178 Target = Target{}
    var t179 string = _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(t178, "goml")
    println__T_string(t179)
    var t180 Convertible = Convertible{}
    var text__12 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(t180)
    var t181 Convertible = Convertible{}
    var number__13 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(t181)
    println__T_string(text__12)
    println__T_int32(number__13)
    var t182 Number = Number{
        value: 8,
    }
    var t183 int32 = read__S_Number__T_int32(t182)
    println__T_int32(t183)
    var t184 Selected = Selected{
        value: 9,
    }
    var t185 int32 = select_item__S_Selected(t184)
    println__T_int32(t185)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv187 string
    var t188 string = _goml_runtime_core_int32_to_string(self__6)
    retv187 = t188
    return retv187
}

func println__T_string(value__1 string) struct{} {
    var t190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t190)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t193 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t193)
    return struct{}{}
}

func read__S_Number__T_int32(source__7 Number) int32 {
    var retv196 int32
    var t197 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(source__7)
    retv196 = t197
    return retv196
}

func select_item__S_Selected(source__11 Selected) int32 {
    var retv199 int32
    var t200 int32 = invoke__S_Selected__T_int32(source__11)
    retv199 = t200
    return retv199
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv202 string
    retv202 = self__38
    return retv202
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv204 string
    var t205 string = _goml_runtime_core_int32_to_string(self__43)
    retv204 = t205
    return retv204
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var retv207 int32
    var t208 int32 = _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(source__10)
    retv207 = t208
    return retv207
}

func _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(self__8 Selected) int32 {
    var retv210 int32
    var t211 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__8)
    retv210 = t211
    return retv210
}

func main() {
    main0()
}
