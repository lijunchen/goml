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
    var retv115 string
    var t116 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t117 string = "number:" + t116
    retv115 = t117
    return retv115
}

func _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(self__2 Target, value__3 string) string {
    var retv119 string
    var t120 string = "text:" + value__3
    retv119 = t120
    return retv119
}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(self__4 Convertible) string {
    var retv122 string
    retv122 = "converted"
    return retv122
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(self__5 Convertible) int32 {
    var retv124 int32
    retv124 = 7
    return retv124
}

func _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(self__6 Number) int32 {
    var retv126 int32
    var t127 int32 = self__6.value
    retv126 = t127
    return retv126
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var retv129 int32
    var t130 int32 = self__9.value
    retv129 = t130
    return retv129
}

func main0() struct{} {
    var t132 Target = Target{}
    var t133 string = _goml_m_trait__impl_i_Put_i__l_int32_r__x40_Target_i_put(t132, 3)
    println__T_string(t133)
    var t134 Target = Target{}
    var t135 string = _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(t134, "goml")
    println__T_string(t135)
    var t136 Convertible = Convertible{}
    var text__12 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(t136)
    var t137 Convertible = Convertible{}
    var number__13 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(t137)
    println__T_string(text__12)
    println__T_int32(number__13)
    var t138 Number = Number{
        value: 8,
    }
    var t139 int32 = read__S_Number__T_int32(t138)
    println__T_int32(t139)
    var t140 Selected = Selected{
        value: 9,
    }
    var t141 int32 = select_item__S_Selected(t140)
    println__T_int32(t141)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv143 string
    var t144 string = _goml_runtime_core_int32_to_string(self__6)
    retv143 = t144
    return retv143
}

func println__T_string(value__1 string) struct{} {
    var t146 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t146)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t149 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t149)
    return struct{}{}
}

func read__S_Number__T_int32(source__7 Number) int32 {
    var retv152 int32
    var t153 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(source__7)
    retv152 = t153
    return retv152
}

func select_item__S_Selected(source__11 Selected) int32 {
    var retv155 int32
    var t156 int32 = invoke__S_Selected__T_int32(source__11)
    retv155 = t156
    return retv155
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv158 string
    retv158 = self__38
    return retv158
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv160 string
    var t161 string = _goml_runtime_core_int32_to_string(self__43)
    retv160 = t161
    return retv160
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var retv163 int32
    var t164 int32 = _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(source__10)
    retv163 = t164
    return retv163
}

func _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(self__8 Selected) int32 {
    var retv166 int32
    var t167 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__8)
    retv166 = t167
    return retv166
}

func main() {
    main0()
}
