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
    var retv68 string
    var t69 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t70 string = "number:" + t69
    retv68 = t70
    return retv68
}

func _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(self__2 Target, value__3 string) string {
    var retv72 string
    var t73 string = "text:" + value__3
    retv72 = t73
    return retv72
}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(self__4 Convertible) string {
    var retv75 string
    retv75 = "converted"
    return retv75
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(self__5 Convertible) int32 {
    var retv77 int32
    retv77 = 7
    return retv77
}

func _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(self__6 Number) int32 {
    var retv79 int32
    var t80 int32 = self__6.value
    retv79 = t80
    return retv79
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var retv82 int32
    var t83 int32 = self__9.value
    retv82 = t83
    return retv82
}

func main0() struct{} {
    var t85 Target = Target{}
    var t86 string = _goml_m_trait__impl_i_Put_i__l_int32_r__x40_Target_i_put(t85, 3)
    println__T_string(t86)
    var t87 Target = Target{}
    var t88 string = _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(t87, "goml")
    println__T_string(t88)
    var t89 Convertible = Convertible{}
    var text__12 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(t89)
    var t90 Convertible = Convertible{}
    var number__13 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(t90)
    println__T_string(text__12)
    println__T_int32(number__13)
    var t91 Number = Number{
        value: 8,
    }
    var t92 int32 = read__S_Number__T_int32(t91)
    println__T_int32(t92)
    var t93 Selected = Selected{
        value: 9,
    }
    var t94 int32 = select_item__S_Selected(t93)
    println__T_int32(t94)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv96 string
    var t97 string = _goml_runtime_core_int32_to_string(self__5)
    retv96 = t97
    return retv96
}

func println__T_string(value__1 string) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func read__S_Number__T_int32(source__7 Number) int32 {
    var retv105 int32
    var t106 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(source__7)
    retv105 = t106
    return retv105
}

func select_item__S_Selected(source__11 Selected) int32 {
    var retv108 int32
    var t109 int32 = invoke__S_Selected__T_int32(source__11)
    retv108 = t109
    return retv108
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv111 string
    retv111 = self__37
    return retv111
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv113 string
    var t114 string = _goml_runtime_core_int32_to_string(self__41)
    retv113 = t114
    return retv113
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var retv116 int32
    var t117 int32 = _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(source__10)
    retv116 = t117
    return retv116
}

func _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(self__8 Selected) int32 {
    var retv119 int32
    var t120 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__8)
    retv119 = t120
    return retv119
}

func main() {
    main0()
}
