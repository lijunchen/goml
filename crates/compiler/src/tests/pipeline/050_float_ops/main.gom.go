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
    var t10 string
    var message__2 string
    var mtmp0 struct{}
    _ = mtmp0
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t10 = float32_to_string(value__1)
            message__2 = label__0 + t10
            string_println(message__2)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func show64(label__3 string, value__4 float64) struct{} {
    var t11 string
    var message__5 string
    var mtmp1 struct{}
    _ = mtmp1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t11 = float64_to_string(value__4)
            message__5 = label__3 + t11
            string_println(message__5)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func lerp32(a__6 float32, b__7 float32, weight__8 float32) float32 {
    var delta__9 float32
    var t12 float32
    var t13 float32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            delta__9 = b__7 - a__6
            t12 = delta__9 * weight__8
            t13 = a__6 + t12
            return t13
        default:
            panic("invalid pc")
        }
    }
}

func midpoint_energy(x__10 float64, y__11 float64) float64 {
    var t14 float64
    var t15 float64
    var sum__12 float64
    var t16 float64
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t14 = x__10 * x__10
            t15 = y__11 * y__11
            sum__12 = t14 + t15
            t16 = sum__12 / 2
            return t16
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var start32__13 float32
    var end32__14 float32
    var half__15 float32
    var scale__16 float32
    var mid32__17 float32
    var neg_end32__18 float32
    var ratio32__19 float32
    var less32__20 bool
    var dx__21 float64
    var dy__22 float64
    var quarter__23 float64
    var energy__24 float64
    var neg_dx__25 float64
    var t17 float64
    var t18 float64
    var adjusted__26 float64
    var threshold__27 float64
    var less64__28 bool
    var _wild2 struct{}
    var _wild3 struct{}
    var _wild4 struct{}
    var t19 string
    var t20 string
    var mtmp5 struct{}
    var _wild6 struct{}
    var _wild7 struct{}
    var _wild8 struct{}
    var t21 string
    var t22 string
    var mtmp9 struct{}
    _ = _wild2
    _ = _wild3
    _ = _wild4
    _ = mtmp5
    _ = _wild6
    _ = _wild7
    _ = _wild8
    _ = mtmp9
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            start32__13 = 1.25
            end32__14 = 5.75
            half__15 = 0.5
            scale__16 = 2
            mid32__17 = lerp32(start32__13, end32__14, half__15)
            neg_end32__18 = -end32__14
            ratio32__19 = end32__14 / scale__16
            less32__20 = start32__13 < end32__14
            dx__21 = 6.5
            dy__22 = 3.5
            quarter__23 = 0.25
            energy__24 = midpoint_energy(dx__21, dy__22)
            neg_dx__25 = -dx__21
            t17 = energy__24 + dy__22
            t18 = dx__21 * quarter__23
            adjusted__26 = t17 - t18
            threshold__27 = 4
            less64__28 = adjusted__26 < threshold__27
            show32("mid32=", mid32__17)
            show32("neg_end32=", neg_end32__18)
            show32("ratio32=", ratio32__19)
            t19 = bool_to_string(less32__20)
            t20 = "less32=" + t19
            string_println(t20)
            show64("energy=", energy__24)
            show64("neg_dx=", neg_dx__25)
            show64("adjusted=", adjusted__26)
            t21 = bool_to_string(less64__28)
            t22 = "less64=" + t21
            string_println(t22)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
