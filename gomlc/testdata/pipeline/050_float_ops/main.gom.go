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
    var inline511 float32 = end32__14 - start32__13
    var inline512 float32 = inline511 * half__15
    var inline513 float32 = start32__13 + inline512
    mid32__17 = inline513
    var neg_end32__18 float32 = -end32__14
    var ratio32__19 float32 = end32__14 / scale__16
    var less32__20 bool = start32__13 < end32__14
    var dx__21 float64 = 6.5
    var dy__22 float64 = 3.5
    var quarter__23 float64 = 0.25
    var energy__24 float64
    var inline506 float64 = dx__21 * dx__21
    var inline507 float64 = dy__22 * dy__22
    var inline508 float64 = inline506 + inline507
    var inline509 float64 = inline508 / 2
    energy__24 = inline509
    var neg_dx__25 float64 = -dx__21
    var t435 float64 = energy__24 + dy__22
    var t436 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t435 - t436
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    var inline501 string = "mid32="
    var inline502 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(mid32__17)
    var inline503 string = inline501 + inline502
    println__T_string(inline503)
    var inline496 string = "neg_end32="
    var inline497 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(neg_end32__18)
    var inline498 string = inline496 + inline497
    println__T_string(inline498)
    var inline491 string = "ratio32="
    var inline492 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(ratio32__19)
    var inline493 string = inline491 + inline492
    println__T_string(inline493)
    var t437 string
    var inline489 string = _goml_runtime_core_bool_to_string(less32__20)
    t437 = inline489
    var t438 string = "less32=" + t437
    var inline486 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline486)
    var inline481 string = "energy="
    var inline482 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(energy__24)
    var inline483 string = inline481 + inline482
    println__T_string(inline483)
    var inline476 string = "neg_dx="
    var inline477 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(neg_dx__25)
    var inline478 string = inline476 + inline477
    println__T_string(inline478)
    var inline471 string = "adjusted="
    var inline472 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(adjusted__26)
    var inline473 string = inline471 + inline472
    println__T_string(inline473)
    var t439 string
    var inline469 string = _goml_runtime_core_bool_to_string(less64__28)
    t439 = inline469
    var t440 string = "less64=" + t439
    var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
    _goml_runtime_core_string_println(inline466)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__160 float32) string {
    var t443 string = _goml_runtime_core_float32_to_string(self__160)
    return t443
}

func println__T_string(value__1 string) struct{} {
    var t445 string
    t445 = value__1
    _goml_runtime_core_string_println(t445)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__161 float64) string {
    var t449 string = _goml_runtime_core_float64_to_string(self__161)
    return t449
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
