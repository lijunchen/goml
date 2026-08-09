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
    var t290 uint8 = uint8(int16(value__11))
    var inline328 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t290)
    _goml_runtime_core_string_println(inline328)
    var inline325 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(WRAPPED)
    _goml_runtime_core_string_println(inline325)
    var inline322 string = _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(LETTER)
    _goml_runtime_core_string_println(inline322)
    var t291 uint8
    var inline319 int16 = -1
    var inline320 uint8 = uint8(int16(inline319))
    t291 = inline320
    var inline316 string = _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(t291)
    _goml_runtime_core_string_println(inline316)
    var octet__12 uint8 = 255
    var t292 int16 = int16(uint8(octet__12))
    var inline313 string = _goml_m_trait__impl_i_ToString_i_int16_i_to__string(t292)
    _goml_runtime_core_string_println(inline313)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_uint8_i_to__string(self__74 uint8) string {
    var t305 string = _goml_runtime_core_uint8_to_string(self__74)
    return t305
}

func _goml_m_trait__impl_i_ToString_i_uint32_i_to__string(self__76 uint32) string {
    var t308 string = _goml_runtime_core_uint32_to_string(self__76)
    return t308
}

func _goml_m_trait__impl_i_ToString_i_int16_i_to__string(self__71 int16) string {
    var t311 string = _goml_runtime_core_int16_to_string(self__71)
    return t311
}

func main() {
    main0()
}
