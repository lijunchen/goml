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

type Number struct {
    value int32
}

type Box__string struct {
    value string
}

type Ordering int32

func _goml_m_trait__impl_i_Provider_i_Number_i_get(self__0 Number) int32 {
    var t413 int32 = self__0.value
    return t413
}

func main0() struct{} {
    var t415 Number = Number{
        value: 42,
    }
    var t416 int32
    var inline471 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t415)
    t416 = inline471
    var t417 string
    var inline469 string = _goml_runtime_core_int32_to_string(t416)
    t417 = inline469
    var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t417)
    _goml_runtime_core_string_println(inline466)
    var t418 Number = Number{
        value: 7,
    }
    var value__4 int32
    var inline464 int32 = _goml_m_trait__impl_i_Provider_i_Number_i_get(t418)
    value__4 = inline464
    var t419 string
    var inline462 string = _goml_runtime_core_int32_to_string(value__4)
    t419 = inline462
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline459)
    var t420 Box__string = Box__string{
        value: "generic",
    }
    var t421 string
    var inline457 string = _goml_m_trait__impl_i_Provider_i_Box____string_i_get(t420)
    t421 = inline457
    var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline454)
    var t423 int32
    var inline452 int32 = 11
    t423 = inline452
    var t424 string
    var inline450 string = _goml_runtime_core_int32_to_string(t423)
    t424 = inline450
    var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline447)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_Provider_i_Box____string_i_get(self__1 Box__string) string {
    var t445 string = self__1.value
    return t445
}

func main() {
    main0()
}
