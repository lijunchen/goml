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

type Tuple2_float32_float64 struct {
    _0 float32
    _1 float64
}

type FloatEvent interface {
    isFloatEvent()
}

type Sample32 struct {
    _0 string
    _1 float32
}

func (_ Sample32) isFloatEvent() {}

type Sample64 struct {
    _0 string
    _1 float64
}

func (_ Sample64) isFloatEvent() {}

func summarize(event__0 FloatEvent) string {
    var ret21 string
    switch event__0 := event__0.(type) {
    case Sample32:
        var x0 string = event__0._0
        var x1 float32 = event__0._1
        var value__2 float32 = x1
        var label__1 string = x0
        var t7 string = float32_to_string(value__2)
        ret21 = label__1 + t7
    case Sample64:
        var x2 string = event__0._0
        var x3 float64 = event__0._1
        var value__4 float64 = x3
        var label__3 string = x2
        var t8 string = float64_to_string(value__4)
        ret21 = label__3 + t8
    }
    return ret21
}

func compare(values__5 Tuple2_float32_float64) string {
    var ret22 string
    var x4 float32 = values__5._0
    var x5 float64 = values__5._1
    var right__7 float64 = x5
    var left__6 float32 = x4
    var limit32__8 float32 = 1
    var limit64__9 float64 = 5
    var less_left__10 bool = left__6 < limit32__8
    var less_right__11 bool = right__7 < limit64__9
    var t11 string = bool_to_string(less_left__10)
    var t10 string = "left<1?=" + t11
    var t9 string = t10 + ",right<5?="
    var t12 string = bool_to_string(less_right__11)
    ret22 = t9 + t12
    return ret22
}

func main0() struct{} {
    var ret23 struct{}
    var first_value__12 float32 = 0.5
    var second_value__13 float32 = 2.25
    var third_value__14 float64 = 9.5
    var first__15 FloatEvent = Sample32{
        _0: "f32=",
        _1: first_value__12,
    }
    var second__16 FloatEvent = Sample32{
        _0: "f32_b=",
        _1: second_value__13,
    }
    var third__17 FloatEvent = Sample64{
        _0: "f64=",
        _1: third_value__14,
    }
    var tuple__18 Tuple2_float32_float64 = Tuple2_float32_float64{
        _0: 0.75,
        _1: 4,
    }
    var tuple_other__19 Tuple2_float32_float64 = Tuple2_float32_float64{
        _0: 1.5,
        _1: 7.25,
    }
    var t16 string = summarize(first__15)
    var t17 string = summarize(second__16)
    var t15 string = t16 + t17
    var t18 string = summarize(third__17)
    var t14 string = t15 + t18
    var t19 string = compare(tuple__18)
    var t13 string = t14 + t19
    var t20 string = compare(tuple_other__19)
    var message__20 string = t13 + t20
    string_println(message__20)
    ret23 = struct{}{}
    return ret23
}

func main() {
    main0()
}
