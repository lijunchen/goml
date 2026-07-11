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

type Tuple2_7float32_7float64 struct {
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
    var retv12 string
    var jp14 string
    switch event__0.(type) {
    case Sample32:
        var x4 string = event__0.(Sample32)._0
        var x5 float32 = event__0.(Sample32)._1
        var value__2 float32 = x5
        var label__1 string = x4
        var t15 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__2)
        var t16 string = label__1 + t15
        jp14 = t16
    case Sample64:
        var x6 string = event__0.(Sample64)._0
        var x7 float64 = event__0.(Sample64)._1
        var value__4 float64 = x7
        var label__3 string = x6
        var t17 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
        var t18 string = label__3 + t17
        jp14 = t18
    default:
        panic("non-exhaustive match")
    }
    retv12 = jp14
    return retv12
}

func compare(values__5 Tuple2_7float32_7float64) string {
    var retv20 string
    var x8 float32 = values__5._0
    var x9 float64 = values__5._1
    var right__7 float64 = x9
    var left__6 float32 = x8
    var limit32__8 float32 = 1
    var limit64__9 float64 = 5
    var less_left__10 bool = left__6 < limit32__8
    var less_right__11 bool = right__7 < limit64__9
    var t21 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_left__10)
    var t22 string = "left<1?=" + t21
    var t23 string = t22 + ",right<5?="
    var t24 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_right__11)
    var t25 string = t23 + t24
    retv20 = t25
    return retv20
}

func main0() struct{} {
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
    var tuple__18 Tuple2_7float32_7float64 = Tuple2_7float32_7float64{
        _0: 0.75,
        _1: 4,
    }
    var tuple_other__19 Tuple2_7float32_7float64 = Tuple2_7float32_7float64{
        _0: 1.5,
        _1: 7.25,
    }
    var t27 string = summarize(first__15)
    var t28 string = summarize(second__16)
    var t29 string = t27 + t28
    var t30 string = summarize(third__17)
    var t31 string = t29 + t30
    var t32 string = compare(tuple__18)
    var t33 string = t31 + t32
    var t34 string = compare(tuple_other__19)
    var message__20 string = t33 + t34
    println__T_string(message__20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv36 string
    var t37 string = _goml_runtime_core_float32_to_string(self__19)
    retv36 = t37
    return retv36
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__20 float64) string {
    var retv39 string
    var t40 string = _goml_runtime_core_float64_to_string(self__20)
    retv39 = t40
    return retv39
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv42 string
    var t43 string = _goml_runtime_core_bool_to_string(self__8)
    retv42 = t43
    return retv42
}

func println__T_string(value__1 string) struct{} {
    var t45 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t45)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv48 string
    retv48 = self__9
    return retv48
}

func main() {
    main0()
}
