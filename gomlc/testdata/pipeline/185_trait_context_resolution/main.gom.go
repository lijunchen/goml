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
    var retv75 string
    var t76 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t77 string = "number:" + t76
    retv75 = t77
    return retv75
}

func _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(self__2 Target, value__3 string) string {
    var retv79 string
    var t80 string = "text:" + value__3
    retv79 = t80
    return retv79
}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(self__4 Convertible) string {
    var retv82 string
    retv82 = "converted"
    return retv82
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(self__5 Convertible) int32 {
    var retv84 int32
    retv84 = 7
    return retv84
}

func _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(self__6 Number) int32 {
    var retv86 int32
    var t87 int32 = self__6.value
    retv86 = t87
    return retv86
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var retv89 int32
    var t90 int32 = self__9.value
    retv89 = t90
    return retv89
}

func main0() struct{} {
    var t92 Target = Target{}
    var t93 string = _goml_m_trait__impl_i_Put_i__l_int32_r__x40_Target_i_put(t92, 3)
    println__T_string(t93)
    var t94 Target = Target{}
    var t95 string = _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(t94, "goml")
    println__T_string(t95)
    var t96 Convertible = Convertible{}
    var text__12 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(t96)
    var t97 Convertible = Convertible{}
    var number__13 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(t97)
    println__T_string(text__12)
    println__T_int32(number__13)
    var t98 Number = Number{
        value: 8,
    }
    var t99 int32 = read__S_Number__T_int32(t98)
    println__T_int32(t99)
    var t100 Selected = Selected{
        value: 9,
    }
    var t101 int32 = select_item__S_Selected(t100)
    println__T_int32(t101)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv103 string
    var t104 string = _goml_runtime_core_int32_to_string(self__6)
    retv103 = t104
    return retv103
}

func println__T_string(value__1 string) struct{} {
    var t106 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t106)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t109 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t109)
    return struct{}{}
}

func read__S_Number__T_int32(source__7 Number) int32 {
    var retv112 int32
    var t113 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(source__7)
    retv112 = t113
    return retv112
}

func select_item__S_Selected(source__11 Selected) int32 {
    var retv115 int32
    var t116 int32 = invoke__S_Selected__T_int32(source__11)
    retv115 = t116
    return retv115
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv118 string
    retv118 = self__38
    return retv118
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv120 string
    var t121 string = _goml_runtime_core_int32_to_string(self__43)
    retv120 = t121
    return retv120
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var retv123 int32
    var t124 int32 = _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(source__10)
    retv123 = t124
    return retv123
}

func _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(self__8 Selected) int32 {
    var retv126 int32
    var t127 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__8)
    retv126 = t127
    return retv126
}

func main() {
    main0()
}
