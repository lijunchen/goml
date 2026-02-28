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
    var jp8 string
    var x0 string
    var x1 float32
    var value__2 float32
    var label__1 string
    var t9 string
    var t10 string
    var x2 string
    var x3 float64
    var value__4 float64
    var label__3 string
    var t11 string
    var t12 string
    switch event__0.(type) {
    case Sample32:
        goto b2
    case Sample64:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp8
    b2:
    x0 = event__0.(Sample32)._0
    x1 = event__0.(Sample32)._1
    value__2 = x1
    label__1 = x0
    t9 = float32_to_string(value__2)
    t10 = label__1 + t9
    jp8 = t10
    goto b1
    b3:
    x2 = event__0.(Sample64)._0
    x3 = event__0.(Sample64)._1
    value__4 = x3
    label__3 = x2
    t11 = float64_to_string(value__4)
    t12 = label__3 + t11
    jp8 = t12
    goto b1
}

func compare(values__5 Tuple2_float32_float64) string {
    var x4 float32
    var x5 float64
    var right__7 float64
    var left__6 float32
    var limit32__8 float32
    var limit64__9 float64
    var less_left__10 bool
    var less_right__11 bool
    var t13 string
    var t14 string
    var t15 string
    var t16 string
    var t17 string
    x4 = values__5._0
    x5 = values__5._1
    right__7 = x5
    left__6 = x4
    limit32__8 = 1
    limit64__9 = 5
    less_left__10 = left__6 < limit32__8
    less_right__11 = right__7 < limit64__9
    t13 = bool_to_string(less_left__10)
    t14 = "left<1?=" + t13
    t15 = t14 + ",right<5?="
    t16 = bool_to_string(less_right__11)
    t17 = t15 + t16
    return t17
}

func main0() struct{} {
    var first_value__12 float32
    var second_value__13 float32
    var third_value__14 float64
    var first__15 FloatEvent
    var second__16 FloatEvent
    var third__17 FloatEvent
    var tuple__18 Tuple2_float32_float64
    var tuple_other__19 Tuple2_float32_float64
    var t18 string
    var t19 string
    var t20 string
    var t21 string
    var t22 string
    var t23 string
    var t24 string
    var t25 string
    var message__20 string
    first_value__12 = 0.5
    second_value__13 = 2.25
    third_value__14 = 9.5
    first__15 = Sample32{
        _0: "f32=",
        _1: first_value__12,
    }
    second__16 = Sample32{
        _0: "f32_b=",
        _1: second_value__13,
    }
    third__17 = Sample64{
        _0: "f64=",
        _1: third_value__14,
    }
    tuple__18 = Tuple2_float32_float64{
        _0: 0.75,
        _1: 4,
    }
    tuple_other__19 = Tuple2_float32_float64{
        _0: 1.5,
        _1: 7.25,
    }
    t18 = summarize(first__15)
    t19 = summarize(second__16)
    t20 = t18 + t19
    t21 = summarize(third__17)
    t22 = t20 + t21
    t23 = compare(tuple__18)
    t24 = t22 + t23
    t25 = compare(tuple_other__19)
    message__20 = t24 + t25
    string_println(message__20)
    return struct{}{}
}

func main() {
    main0()
}
