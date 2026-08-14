package main

import (
    _goml_fmt "fmt"
    _goml_strconv "strconv"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_float32_to_string(x float32) string {
    var formatted string = _goml_strconv.FormatFloat(float64(x), 102, -1, 32)
    if formatted == "+Inf" {
        return "inf"
    }
    if formatted == "-Inf" {
        return "-inf"
    }
    return formatted
}

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
    var start32__13 float32 = 1.25
    var end32__14 float32 = 5.75
    var half__15 float32 = 0.5
    var scale__16 float32 = 2
    var mid32__17 float32
    var inline508 float32 = end32__14 - start32__13
    var inline509 float32 = inline508 * half__15
    var inline510 float32 = start32__13 + inline509
    mid32__17 = inline510
    var neg_end32__18 float32 = -end32__14
    var ratio32__19 float32 = end32__14 / scale__16
    var less32__20 bool = start32__13 < end32__14
    var dx__21 float64 = 6.5
    var dy__22 float64 = 3.5
    var quarter__23 float64 = 0.25
    var energy__24 float64
    var inline503 float64 = dx__21 * dx__21
    var inline504 float64 = dy__22 * dy__22
    var inline505 float64 = inline503 + inline504
    var inline506 float64 = inline505 / 2
    energy__24 = inline506
    var neg_dx__25 float64 = -dx__21
    var t432 float64 = energy__24 + dy__22
    var t433 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t432 - t433
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    var inline498 string = "mid32="
    var inline499 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(mid32__17)
    var inline500 string = inline498 + inline499
    println__T_string(inline500)
    var inline493 string = "neg_end32="
    var inline494 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(neg_end32__18)
    var inline495 string = inline493 + inline494
    println__T_string(inline495)
    var inline488 string = "ratio32="
    var inline489 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(ratio32__19)
    var inline490 string = inline488 + inline489
    println__T_string(inline490)
    var t434 string
    var inline486 string = _goml_runtime_core_bool_to_string(less32__20)
    t434 = inline486
    var t435 string = "less32=" + t434
    var inline483 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline483)
    var inline478 string = "energy="
    var inline479 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(energy__24)
    var inline480 string = inline478 + inline479
    println__T_string(inline480)
    var inline473 string = "neg_dx="
    var inline474 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(neg_dx__25)
    var inline475 string = inline473 + inline474
    println__T_string(inline475)
    var inline468 string = "adjusted="
    var inline469 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(adjusted__26)
    var inline470 string = inline468 + inline469
    println__T_string(inline470)
    var t436 string
    var inline466 string = _goml_runtime_core_bool_to_string(less64__28)
    t436 = inline466
    var t437 string = "less64=" + t436
    var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline463)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__160 float32) string {
    var t440 string = _goml_runtime_core_float32_to_string(self__160)
    return t440
}

func println__T_string(value__1 string) struct{} {
    var t442 string
    t442 = value__1
    _goml_runtime_core_string_println(t442)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__161 float64) string {
    var t446 string = _goml_runtime_core_float64_to_string(self__161)
    return t446
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
