package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int16_to_string(x int16) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_uint32_to_string(x uint32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

const (
    WRAPPED uint8 = 255
    LETTER uint32 = 65
)

func main0() struct{} {
    var value__11 int16 = 511
    var t529 uint8 = uint8(int16(value__11))
    var inline567 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t529)
    _goml_runtime_core_string_println(inline567)
    var inline564 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(WRAPPED)
    _goml_runtime_core_string_println(inline564)
    var inline561 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(LETTER)
    _goml_runtime_core_string_println(inline561)
    var t530 uint8
    var inline558 int16 = -1
    var inline559 uint8 = uint8(int16(inline558))
    t530 = inline559
    var inline555 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t530)
    _goml_runtime_core_string_println(inline555)
    var octet__12 uint8 = 255
    var t531 int16 = int16(uint8(octet__12))
    var inline552 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(t531)
    _goml_runtime_core_string_println(inline552)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__156 uint8) string {
    var t544 string = _goml_runtime_core_uint8_to_string(self__156)
    return t544
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__158 uint32) string {
    var t547 string = _goml_runtime_core_uint32_to_string(self__158)
    return t547
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__153 int16) string {
    var t550 string = _goml_runtime_core_int16_to_string(self__153)
    return t550
}

func main() {
    main0()
}
