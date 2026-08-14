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

const (
    WRAPPED uint8 = 255
    LETTER uint32 = 65
)

func main0() struct{} {
    var value__11 int16 = 511
    var t300 uint8 = uint8(int16(value__11))
    var inline338 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t300)
    _goml_runtime_core_string_println(inline338)
    var inline335 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(WRAPPED)
    _goml_runtime_core_string_println(inline335)
    var inline332 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(LETTER)
    _goml_runtime_core_string_println(inline332)
    var t301 uint8
    var inline329 int16 = -1
    var inline330 uint8 = uint8(int16(inline329))
    t301 = inline330
    var inline326 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t301)
    _goml_runtime_core_string_println(inline326)
    var octet__12 uint8 = 255
    var t302 int16 = int16(uint8(octet__12))
    var inline323 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(t302)
    _goml_runtime_core_string_println(inline323)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__72 uint8) string {
    var t315 string = _goml_runtime_core_uint8_to_string(self__72)
    return t315
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__74 uint32) string {
    var t318 string = _goml_runtime_core_uint32_to_string(self__74)
    return t318
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__69 int16) string {
    var t321 string = _goml_runtime_core_int16_to_string(self__69)
    return t321
}

func main() {
    main0()
}
