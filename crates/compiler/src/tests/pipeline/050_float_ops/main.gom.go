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
    var t18 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__1)
    var message__2 string = label__0 + t18
    println__T_string(message__2)
    return struct{}{}
}

func show64(label__3 string, value__4 float64) struct{} {
    var t20 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
    var message__5 string = label__3 + t20
    println__T_string(message__5)
    return struct{}{}
}

func lerp32(a__6 float32, b__7 float32, weight__8 float32) float32 {
    var retv22 float32
    var delta__9 float32 = b__7 - a__6
    var t23 float32 = delta__9 * weight__8
    var t24 float32 = a__6 + t23
    retv22 = t24
    return retv22
}

func midpoint_energy(x__10 float64, y__11 float64) float64 {
    var retv26 float64
    var t27 float64 = x__10 * x__10
    var t28 float64 = y__11 * y__11
    var sum__12 float64 = t27 + t28
    var t29 float64 = sum__12 / 2
    retv26 = t29
    return retv26
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
    var t31 float64 = energy__24 + dy__22
    var t32 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t31 - t32
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    show32("mid32=", mid32__17)
    show32("neg_end32=", neg_end32__18)
    show32("ratio32=", ratio32__19)
    var t33 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less32__20)
    var t34 string = "less32=" + t33
    println__T_string(t34)
    show64("energy=", energy__24)
    show64("neg_dx=", neg_dx__25)
    show64("adjusted=", adjusted__26)
    var t35 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less64__28)
    var t36 string = "less64=" + t35
    println__T_string(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv38 string
    var t39 string = _goml_runtime_core_float32_to_string(self__19)
    retv38 = t39
    return retv38
}

func println__T_string(value__1 string) struct{} {
    var t41 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t41)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__20 float64) string {
    var retv44 string
    var t45 string = _goml_runtime_core_float64_to_string(self__20)
    retv44 = t45
    return retv44
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv47 string
    var t48 string = _goml_runtime_core_bool_to_string(self__8)
    retv47 = t48
    return retv47
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv50 string
    retv50 = self__9
    return retv50
}

func main() {
    main0()
}
