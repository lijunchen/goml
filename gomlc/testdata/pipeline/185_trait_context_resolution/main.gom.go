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
    var t163 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t164 string = "number:" + t163
    return t164
}

func _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(self__2 Target, value__3 string) string {
    var t167 string = "text:" + value__3
    return t167
}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(self__4 Convertible) string {
    return "converted"
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(self__5 Convertible) int32 {
    return 7
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
    var t191 string = _goml_runtime_core_int32_to_string(self__6)
    return t191
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
    var t200 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(source__7)
    return t200
}

func select_item__S_Selected(source__11 Selected) int32 {
    var t203 int32 = invoke__S_Selected__T_int32(source__11)
    return t203
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t208 string = _goml_runtime_core_int32_to_string(self__43)
    return t208
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var t211 int32 = _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(source__10)
    return t211
}

func _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(self__8 Selected) int32 {
    var t214 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__8)
    return t214
}

func main() {
    main0()
}
