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
    var retv65 string
    var t66 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t67 string = "number:" + t66
    retv65 = t67
    return retv65
}

func _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(self__2 Target, value__3 string) string {
    var retv69 string
    var t70 string = "text:" + value__3
    retv69 = t70
    return retv69
}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(self__4 Convertible) string {
    var retv72 string
    retv72 = "converted"
    return retv72
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(self__5 Convertible) int32 {
    var retv74 int32
    retv74 = 7
    return retv74
}

func _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(self__6 Number) int32 {
    var retv76 int32
    var t77 int32 = self__6.value
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var retv79 int32
    var t80 int32 = self__9.value
    retv79 = t80
    return retv79
}

func main0() struct{} {
    var t82 Target = Target{}
    var t83 string = _goml_m_trait__impl_i_Put_i__l_int32_r__x40_Target_i_put(t82, 3)
    println__T_string(t83)
    var t84 Target = Target{}
    var t85 string = _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(t84, "goml")
    println__T_string(t85)
    var t86 Convertible = Convertible{}
    var text__12 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(t86)
    var t87 Convertible = Convertible{}
    var number__13 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(t87)
    println__T_string(text__12)
    println__T_int32(number__13)
    var t88 Number = Number{
        value: 8,
    }
    var t89 int32 = read__S_Number__T_int32(t88)
    println__T_int32(t89)
    var t90 Selected = Selected{
        value: 9,
    }
    var t91 int32 = select_item__S_Selected(t90)
    println__T_int32(t91)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv93 string
    var t94 string = _goml_runtime_core_int32_to_string(self__2)
    retv93 = t94
    return retv93
}

func println__T_string(value__1 string) struct{} {
    var t96 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t96)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func read__S_Number__T_int32(source__7 Number) int32 {
    var retv102 int32
    var t103 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(source__7)
    retv102 = t103
    return retv102
}

func select_item__S_Selected(source__11 Selected) int32 {
    var retv105 int32
    var t106 int32 = invoke__S_Selected__T_int32(source__11)
    retv105 = t106
    return retv105
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv108 string
    retv108 = self__34
    return retv108
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv110 string
    var t111 string = _goml_runtime_core_int32_to_string(self__38)
    retv110 = t111
    return retv110
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var retv113 int32
    var t114 int32 = _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(source__10)
    retv113 = t114
    return retv113
}

func _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(self__8 Selected) int32 {
    var retv116 int32
    var t117 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__8)
    retv116 = t117
    return retv116
}

func main() {
    main0()
}
