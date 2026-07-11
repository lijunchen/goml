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
    var t33 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__1)
    var message__2 string = label__0 + t33
    println__T_string(message__2)
    return struct{}{}
}

func show64(label__3 string, value__4 float64) struct{} {
    var t35 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
    var message__5 string = label__3 + t35
    println__T_string(message__5)
    return struct{}{}
}

func lerp32(a__6 float32, b__7 float32, weight__8 float32) float32 {
    var retv37 float32
    var delta__9 float32 = b__7 - a__6
    var t38 float32 = delta__9 * weight__8
    var t39 float32 = a__6 + t38
    retv37 = t39
    return retv37
}

func midpoint_energy(x__10 float64, y__11 float64) float64 {
    var retv41 float64
    var t42 float64 = x__10 * x__10
    var t43 float64 = y__11 * y__11
    var sum__12 float64 = t42 + t43
    var t44 float64 = sum__12 / 2
    retv41 = t44
    return retv41
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
    var t46 float64 = energy__24 + dy__22
    var t47 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t46 - t47
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    show32("mid32=", mid32__17)
    show32("neg_end32=", neg_end32__18)
    show32("ratio32=", ratio32__19)
    var t48 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less32__20)
    var t49 string = "less32=" + t48
    println__T_string(t49)
    show64("energy=", energy__24)
    show64("neg_dx=", neg_dx__25)
    show64("adjusted=", adjusted__26)
    var t50 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less64__28)
    var t51 string = "less64=" + t50
    println__T_string(t51)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv53 string
    var t54 string = _goml_runtime_core_float32_to_string(self__19)
    retv53 = t54
    return retv53
}

func println__T_string(value__1 string) struct{} {
    var t56 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t56)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__20 float64) string {
    var retv59 string
    var t60 string = _goml_runtime_core_float64_to_string(self__20)
    retv59 = t60
    return retv59
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv62 string
    var t63 string = _goml_runtime_core_bool_to_string(self__8)
    retv62 = t63
    return retv62
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv65 string
    retv65 = self__9
    return retv65
}

func main() {
    main0()
}
