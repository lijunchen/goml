package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_float64_to_string(x float64) string {
    var formatted string = _goml_strconv.FormatFloat(x, 102, -1, 64)
    if formatted == "+Inf" {
        return "inf"
    }
    if formatted == "-Inf" {
        return "-inf"
    }
    return formatted
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var t415 string
    var inline471 float64 = 18318654708.7
    var inline472 string = _goml_runtime_core_float64_to_string(inline471)
    t415 = inline472
    var inline468 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
    _goml_runtime_core_string_println(inline468)
    var t416 string
    var inline465 float64 = 0.0000001
    var inline466 string = _goml_runtime_core_float64_to_string(inline465)
    t416 = inline466
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t416)
    _goml_runtime_core_string_println(inline462)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t417 float64 = negative_one__1 * zero__0
    var t418 string
    var inline460 string = _goml_runtime_core_float64_to_string(t417)
    t418 = inline460
    var inline457 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
    _goml_runtime_core_string_println(inline457)
    var t419 float64 = 1 / zero__0
    var t420 string
    var inline455 string = _goml_runtime_core_float64_to_string(t419)
    t420 = inline455
    var inline452 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t420)
    _goml_runtime_core_string_println(inline452)
    var t421 float64 = -1
    var t422 float64 = t421 / zero__0
    var t423 string
    var inline450 string = _goml_runtime_core_float64_to_string(t422)
    t423 = inline450
    var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline447)
    var t424 float64 = zero__0 / zero__0
    var t425 string
    var inline445 string = _goml_runtime_core_float64_to_string(t424)
    t425 = inline445
    var inline442 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline442)
    var wide__2 float64 = 12345678
    var t426 string
    var inline440 string = _goml_runtime_core_float64_to_string(wide__2)
    t426 = inline440
    var inline437 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline437)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
