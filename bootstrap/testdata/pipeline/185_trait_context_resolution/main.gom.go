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
    var retv71 string
    var t72 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t73 string = "number:" + t72
    retv71 = t73
    return retv71
}

func _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(self__2 Target, value__3 string) string {
    var retv75 string
    var t76 string = "text:" + value__3
    retv75 = t76
    return retv75
}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(self__4 Convertible) string {
    var retv78 string
    retv78 = "converted"
    return retv78
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(self__5 Convertible) int32 {
    var retv80 int32
    retv80 = 7
    return retv80
}

func _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(self__6 Number) int32 {
    var retv82 int32
    var t83 int32 = self__6.value
    retv82 = t83
    return retv82
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var retv85 int32
    var t86 int32 = self__9.value
    retv85 = t86
    return retv85
}

func main0() struct{} {
    var t88 Target = Target{}
    var t89 string = _goml_m_trait__impl_i_Put_i__l_int32_r__x40_Target_i_put(t88, 3)
    println__T_string(t89)
    var t90 Target = Target{}
    var t91 string = _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(t90, "goml")
    println__T_string(t91)
    var t92 Convertible = Convertible{}
    var text__12 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(t92)
    var t93 Convertible = Convertible{}
    var number__13 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(t93)
    println__T_string(text__12)
    println__T_int32(number__13)
    var t94 Number = Number{
        value: 8,
    }
    var t95 int32 = read__S_Number__T_int32(t94)
    println__T_int32(t95)
    var t96 Selected = Selected{
        value: 9,
    }
    var t97 int32 = select_item__S_Selected(t96)
    println__T_int32(t97)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv99 string
    var t100 string = _goml_runtime_core_int32_to_string(self__6)
    retv99 = t100
    return retv99
}

func println__T_string(value__1 string) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t105 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t105)
    return struct{}{}
}

func read__S_Number__T_int32(source__7 Number) int32 {
    var retv108 int32
    var t109 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(source__7)
    retv108 = t109
    return retv108
}

func select_item__S_Selected(source__11 Selected) int32 {
    var retv111 int32
    var t112 int32 = invoke__S_Selected__T_int32(source__11)
    retv111 = t112
    return retv111
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv114 string
    retv114 = self__38
    return retv114
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv116 string
    var t117 string = _goml_runtime_core_int32_to_string(self__43)
    retv116 = t117
    return retv116
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var retv119 int32
    var t120 int32 = _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(source__10)
    retv119 = t120
    return retv119
}

func _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(self__8 Selected) int32 {
    var retv122 int32
    var t123 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__8)
    retv122 = t123
    return retv122
}

func main() {
    main0()
}
