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
    var t305 uint8 = uint8(int16(value__11))
    var inline343 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t305)
    _goml_runtime_core_string_println(inline343)
    var inline340 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(WRAPPED)
    _goml_runtime_core_string_println(inline340)
    var inline337 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(LETTER)
    _goml_runtime_core_string_println(inline337)
    var t306 uint8
    var inline334 int16 = -1
    var inline335 uint8 = uint8(int16(inline334))
    t306 = inline335
    var inline331 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t306)
    _goml_runtime_core_string_println(inline331)
    var octet__12 uint8 = 255
    var t307 int16 = int16(uint8(octet__12))
    var inline328 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(t307)
    _goml_runtime_core_string_println(inline328)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__72 uint8) string {
    var t320 string = _goml_runtime_core_uint8_to_string(self__72)
    return t320
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__74 uint32) string {
    var t323 string = _goml_runtime_core_uint32_to_string(self__74)
    return t323
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__69 int16) string {
    var t326 string = _goml_runtime_core_int16_to_string(self__69)
    return t326
}

func main() {
    main0()
}
