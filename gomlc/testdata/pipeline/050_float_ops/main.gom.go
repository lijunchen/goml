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

func show32(label__0 string, value__1 float32) struct{} {
    var t119 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__1)
    var message__2 string = label__0 + t119
    println__T_string(message__2)
    return struct{}{}
}

func show64(label__3 string, value__4 float64) struct{} {
    var t121 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
    var message__5 string = label__3 + t121
    println__T_string(message__5)
    return struct{}{}
}

func lerp32(a__6 float32, b__7 float32, weight__8 float32) float32 {
    var retv123 float32
    var delta__9 float32 = b__7 - a__6
    var t124 float32 = delta__9 * weight__8
    var t125 float32 = a__6 + t124
    retv123 = t125
    return retv123
}

func midpoint_energy(x__10 float64, y__11 float64) float64 {
    var retv127 float64
    var t128 float64 = x__10 * x__10
    var t129 float64 = y__11 * y__11
    var sum__12 float64 = t128 + t129
    var t130 float64 = sum__12 / 2
    retv127 = t130
    return retv127
}

func main0() struct{} {
    var start32__13 float32 = 1.25
    var end32__14 float32 = 5.75
    var half__15 float32 = 0.5
    var scale__16 float32 = 2
    var mid32__17 float32 = lerp32(start32__13, end32__14, half__15)
    var neg_end32__18 float32 = -end32__14
    var ratio32__19 float32 = end32__14 / scale__16
    var less32__20 bool = start32__13 < end32__14
    var dx__21 float64 = 6.5
    var dy__22 float64 = 3.5
    var quarter__23 float64 = 0.25
    var energy__24 float64 = midpoint_energy(dx__21, dy__22)
    var neg_dx__25 float64 = -dx__21
    var t132 float64 = energy__24 + dy__22
    var t133 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t132 - t133
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    show32("mid32=", mid32__17)
    show32("neg_end32=", neg_end32__18)
    show32("ratio32=", ratio32__19)
    var t134 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less32__20)
    var t135 string = "less32=" + t134
    println__T_string(t135)
    show64("energy=", energy__24)
    show64("neg_dx=", neg_dx__25)
    show64("adjusted=", adjusted__26)
    var t136 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less64__28)
    var t137 string = "less64=" + t136
    println__T_string(t137)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__49 float32) string {
    var retv139 string
    var t140 string = _goml_runtime_core_float32_to_string(self__49)
    retv139 = t140
    return retv139
}

func println__T_string(value__1 string) struct{} {
    var t142 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t142)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__50 float64) string {
    var retv145 string
    var t146 string = _goml_runtime_core_float64_to_string(self__50)
    retv145 = t146
    return retv145
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv148 string
    var t149 string = _goml_runtime_core_bool_to_string(self__37)
    retv148 = t149
    return retv148
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv151 string
    retv151 = self__38
    return retv151
}

func main() {
    main0()
}
