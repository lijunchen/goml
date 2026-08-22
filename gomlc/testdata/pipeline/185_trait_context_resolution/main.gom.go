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

func _goml_m_trait__impl_i_Source_i__l_i32_r__x40_Number_i_get(self__6 Number) int32 {
    var t430 int32 = self__6.value
    return t430
}

func _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(self__9 Selected) int32 {
    var t433 int32 = self__9.value
    return t433
}

func main0() struct{} {
    var t436 string
    var inline501 int32 = 3
    var inline502 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline501)
    var inline503 string = "number:" + inline502
    t436 = inline503
    var inline498 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline498)
    var t438 string
    var inline495 string = "goml"
    var inline496 string = "text:" + inline495
    t438 = inline496
    var inline492 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline492)
    var text__12 string
    text__12 = "converted"
    var number__13 int32
    number__13 = 7
    var inline487 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__12)
    _goml_runtime_core_string_println(inline487)
    var inline484 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(number__13)
    _goml_runtime_core_string_println(inline484)
    var t441 Number = Number{
        value: 8,
    }
    var t442 int32
    var inline482 int32 = _goml_m_trait__impl_i_Source_i__l_i32_r__x40_Number_i_get(t441)
    t442 = inline482
    var inline479 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t442)
    _goml_runtime_core_string_println(inline479)
    var t443 Selected = Selected{
        value: 9,
    }
    var t444 int32
    var inline477 int32 = invoke__S_Selected__T_i32(t443)
    t444 = inline477
    var inline474 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t444)
    _goml_runtime_core_string_println(inline474)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t447 string = _goml_runtime_core_int32_to_string(self__33)
    return t447
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t464 string = _goml_runtime_core_int32_to_string(self__154)
    return t464
}

func invoke__S_Selected__T_i32(source__10 Selected) int32 {
    var inline512 int32 = _goml_m_trait__impl_i_ItemSource_i_Selected_i_item(source__10)
    return inline512
}

func main() {
    main0()
}
