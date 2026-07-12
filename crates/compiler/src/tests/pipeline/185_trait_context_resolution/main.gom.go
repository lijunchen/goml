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
    var retv29 string
    var t30 string = _goml_m_inherent_i_int32_i_int32_i_to__string(value__1)
    var t31 string = "number:" + t30
    retv29 = t31
    return retv29
}

func _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(self__2 Target, value__3 string) string {
    var retv33 string
    var t34 string = "text:" + value__3
    retv33 = t34
    return retv33
}

func _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(self__4 Convertible) string {
    var retv36 string
    retv36 = "converted"
    return retv36
}

func _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(self__5 Convertible) int32 {
    var retv38 int32
    retv38 = 7
    return retv38
}

func _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(self__6 Number) int32 {
    var retv40 int32
    var t41 int32 = self__6.value
    retv40 = t41
    return retv40
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var retv43 int32
    var t44 int32 = self__9.value
    retv43 = t44
    return retv43
}

func main0() struct{} {
    var t46 Target = Target{}
    var t47 string = _goml_m_trait__impl_i_Put_i__l_int32_r__x40_Target_i_put(t46, 3)
    println__T_string(t47)
    var t48 Target = Target{}
    var t49 string = _goml_m_trait__impl_i_Put_i__l_string_r__x40_Target_i_put(t48, "goml")
    println__T_string(t49)
    var t50 Convertible = Convertible{}
    var text__12 string = _goml_m_trait__impl_i_Convert_i__l_int32_r__x40_Convertible_i_convert(t50)
    var t51 Convertible = Convertible{}
    var number__13 int32 = _goml_m_trait__impl_i_Convert_i__l_string_r__x40_Convertible_i_convert(t51)
    println__T_string(text__12)
    println__T_int32(number__13)
    var t52 Number = Number{
        value: 8,
    }
    var t53 int32 = read__S_Number__T_int32(t52)
    println__T_int32(t53)
    var t54 Selected = Selected{
        value: 9,
    }
    var t55 int32 = select_item__S_Selected(t54)
    println__T_int32(t55)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv57 string
    var t58 string = _goml_runtime_core_int32_to_string(self__2)
    retv57 = t58
    return retv57
}

func println__T_string(value__1 string) struct{} {
    var t60 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t60)
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t63 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t63)
    return struct{}{}
}

func read__S_Number__T_int32(source__7 Number) int32 {
    var retv66 int32
    var t67 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(source__7)
    retv66 = t67
    return retv66
}

func select_item__S_Selected(source__11 Selected) int32 {
    var retv69 int32
    var t70 int32 = invoke__S_Selected__T_int32(source__11)
    retv69 = t70
    return retv69
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv72 string
    retv72 = self__9
    return retv72
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv74 string
    var t75 string = _goml_runtime_core_int32_to_string(self__13)
    retv74 = t75
    return retv74
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var retv77 int32
    var t78 int32 = _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(source__10)
    retv77 = t78
    return retv77
}

func _goml_m_trait__impl_i_Select_i__l_int32_r__x40_Selected_i_select(self__8 Selected) int32 {
    var retv80 int32
    var t81 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__8)
    retv80 = t81
    return retv80
}

func main() {
    main0()
}
