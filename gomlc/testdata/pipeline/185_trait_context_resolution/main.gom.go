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

type Ordering int32

func _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(self__6 Number) int32 {
    var t427 int32 = self__6.value
    return t427
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var t430 int32 = self__9.value
    return t430
}

func main0() struct{} {
    var t433 string
    var inline498 int32 = 3
    var inline499 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline498)
    var inline500 string = "number:" + inline499
    t433 = inline500
    var inline495 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline495)
    var t435 string
    var inline492 string = "goml"
    var inline493 string = "text:" + inline492
    t435 = inline493
    var inline489 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline489)
    var text__12 string
    text__12 = "converted"
    var number__13 int32
    number__13 = 7
    var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__12)
    _goml_runtime_core_string_println(inline484)
    var inline481 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(number__13)
    _goml_runtime_core_string_println(inline481)
    var t438 Number = Number{
        value: 8,
    }
    var t439 int32
    var inline479 int32 = _goml_m_trait__impl_i_Source_i__l_int32_r__x40_Number_i_get(t438)
    t439 = inline479
    var inline476 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t439)
    _goml_runtime_core_string_println(inline476)
    var t440 Selected = Selected{
        value: 9,
    }
    var t441 int32
    var inline474 int32 = invoke__S_Selected__T_int32(t440)
    t441 = inline474
    var inline471 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t441)
    _goml_runtime_core_string_println(inline471)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t444 string = _goml_runtime_core_int32_to_string(self__33)
    return t444
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t461 string = _goml_runtime_core_int32_to_string(self__154)
    return t461
}

func invoke__S_Selected__T_int32(source__10 Selected) int32 {
    var inline509 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(source__10)
    return inline509
}

func main() {
    main0()
}
