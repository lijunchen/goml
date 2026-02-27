package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func float32_to_string(x float32) string {
    return fmt.Sprintf("%d", x)
}

func float64_to_string(x float64) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func show32(label__0 string, value__1 float32) struct{} {
    var ret21 struct{}
    var t10 string = float32_to_string(value__1)
    var message__2 string = label__0 + t10
    string_println(message__2)
    ret21 = struct{}{}
    return ret21
}

func show64(label__3 string, value__4 float64) struct{} {
    var ret22 struct{}
    var t11 string = float64_to_string(value__4)
    var message__5 string = label__3 + t11
    string_println(message__5)
    ret22 = struct{}{}
    return ret22
}

func lerp32(a__6 float32, b__7 float32, weight__8 float32) float32 {
    var ret23 float32
    var delta__9 float32 = b__7 - a__6
    var t12 float32 = delta__9 * weight__8
    ret23 = a__6 + t12
    return ret23
}

func midpoint_energy(x__10 float64, y__11 float64) float64 {
    var ret24 float64
    var t13 float64 = x__10 * x__10
    var t14 float64 = y__11 * y__11
    var sum__12 float64 = t13 + t14
    ret24 = sum__12 / 2
    return ret24
}

func main0() struct{} {
    var ret25 struct{}
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
    var t15 float64 = energy__24 + dy__22
    var t16 float64 = dx__21 * quarter__23
    var adjusted__26 float64 = t15 - t16
    var threshold__27 float64 = 4
    var less64__28 bool = adjusted__26 < threshold__27
    show32("mid32=", mid32__17)
    show32("neg_end32=", neg_end32__18)
    show32("ratio32=", ratio32__19)
    var t18 string = bool_to_string(less32__20)
    var t17 string = "less32=" + t18
    string_println(t17)
    show64("energy=", energy__24)
    show64("neg_dx=", neg_dx__25)
    show64("adjusted=", adjusted__26)
    var t20 string = bool_to_string(less64__28)
    var t19 string = "less64=" + t20
    string_println(t19)
    ret25 = struct{}{}
    return ret25
}

func main() {
    main0()
}
