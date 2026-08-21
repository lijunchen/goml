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
    var t418 string
    var inline474 float64 = 18318654708.7
    var inline475 string = _goml_runtime_core_float64_to_string(inline474)
    t418 = inline475
    var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t418)
    _goml_runtime_core_string_println(inline471)
    var t419 string
    var inline468 float64 = 0.0000001
    var inline469 string = _goml_runtime_core_float64_to_string(inline468)
    t419 = inline469
    var inline465 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline465)
    var zero__0 float64 = 0
    var negative_one__1 float64 = -1
    var t420 float64 = negative_one__1 * zero__0
    var t421 string
    var inline463 string = _goml_runtime_core_float64_to_string(t420)
    t421 = inline463
    var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t421)
    _goml_runtime_core_string_println(inline460)
    var t422 float64 = 1 / zero__0
    var t423 string
    var inline458 string = _goml_runtime_core_float64_to_string(t422)
    t423 = inline458
    var inline455 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t423)
    _goml_runtime_core_string_println(inline455)
    var t424 float64 = -1
    var t425 float64 = t424 / zero__0
    var t426 string
    var inline453 string = _goml_runtime_core_float64_to_string(t425)
    t426 = inline453
    var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline450)
    var t427 float64 = zero__0 / zero__0
    var t428 string
    var inline448 string = _goml_runtime_core_float64_to_string(t427)
    t428 = inline448
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline445)
    var wide__2 float64 = 12345678
    var t429 string
    var inline443 string = _goml_runtime_core_float64_to_string(wide__2)
    t429 = inline443
    var inline440 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline440)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
