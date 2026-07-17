package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_float32_to_string(x float32) string {
    return _goml_fmt.Sprintf("%g", x)
}

func _goml_runtime_core_float64_to_string(x float64) string {
    return _goml_fmt.Sprintf("%g", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func show32(label__0 string, value__1 float32) struct{} {
    var t69 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__1)
    var message__2 string = label__0 + t69
    println__T_string(message__2)
    return struct{}{}
}

func show64(label__3 string, value__4 float64) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
    var message__5 string = label__3 + t71
    println__T_string(message__5)
    return struct{}{}
}

func lerp32(a__6 float32, b__7 float32, weight__8 float32) float32 {
    var retv73 float32
    var delta__9 float32 = b__7 - a__6
    var t74 float32 = delta__9 * weight__8
    var t75 float32 = a__6 + t74
    retv73 = t75
    return retv73
}

func midpoint_energy(x__10 float64, y__11 float64) float64 {
    var retv77 float64
    var t78 float64 = x__10 * x__10
    var t79 float64 = y__11 * y__11
    var sum__12 float64 = t78 + t79
    var t80 float64 = sum__12 / 2
    retv77 = t80
    return retv77
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
    var t82 float64 = energy__24 + dy__22
    var t83 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t82 - t83
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    show32("mid32=", mid32__17)
    show32("neg_end32=", neg_end32__18)
    show32("ratio32=", ratio32__19)
    var t84 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less32__20)
    var t85 string = "less32=" + t84
    println__T_string(t85)
    show64("energy=", energy__24)
    show64("neg_dx=", neg_dx__25)
    show64("adjusted=", adjusted__26)
    var t86 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less64__28)
    var t87 string = "less64=" + t86
    println__T_string(t87)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__44 float32) string {
    var retv89 string
    var t90 string = _goml_runtime_core_float32_to_string(self__44)
    retv89 = t90
    return retv89
}

func println__T_string(value__1 string) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__45 float64) string {
    var retv95 string
    var t96 string = _goml_runtime_core_float64_to_string(self__45)
    retv95 = t96
    return retv95
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv98 string
    var t99 string = _goml_runtime_core_bool_to_string(self__33)
    retv98 = t99
    return retv98
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv101 string
    retv101 = self__34
    return retv101
}

func main() {
    main0()
}
