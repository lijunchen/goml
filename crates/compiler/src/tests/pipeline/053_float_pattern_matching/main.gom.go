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
    var retv30 string
    var jp32 string
    switch event__0.(type) {
    case Sample32:
        var x22 string = event__0.(Sample32)._0
        var x23 float32 = event__0.(Sample32)._1
        var value__2 float32 = x23
        var label__1 string = x22
        var t33 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__2)
        var t34 string = label__1 + t33
        jp32 = t34
    case Sample64:
        var x24 string = event__0.(Sample64)._0
        var x25 float64 = event__0.(Sample64)._1
        var value__4 float64 = x25
        var label__3 string = x24
        var t35 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
        var t36 string = label__3 + t35
        jp32 = t36
    default:
        panic("non-exhaustive match")
    }
    retv30 = jp32
    return retv30
}

func compare(values__5 Tuple2_7float32_7float64) string {
    var retv38 string
    var x26 float32 = values__5._0
    var x27 float64 = values__5._1
    var right__7 float64 = x27
    var left__6 float32 = x26
    var limit32__8 float32 = 1
    var limit64__9 float64 = 5
    var less_left__10 bool = left__6 < limit32__8
    var less_right__11 bool = right__7 < limit64__9
    var t39 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_left__10)
    var t40 string = "left<1?=" + t39
    var t41 string = t40 + ",right<5?="
    var t42 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_right__11)
    var t43 string = t41 + t42
    retv38 = t43
    return retv38
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
    var t45 string = summarize(first__15)
    var t46 string = summarize(second__16)
    var t47 string = t45 + t46
    var t48 string = summarize(third__17)
    var t49 string = t47 + t48
    var t50 string = compare(tuple__18)
    var t51 string = t49 + t50
    var t52 string = compare(tuple_other__19)
    var message__20 string = t51 + t52
    println__T_string(message__20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__19 float32) string {
    var retv54 string
    var t55 string = _goml_runtime_core_float32_to_string(self__19)
    retv54 = t55
    return retv54
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__20 float64) string {
    var retv57 string
    var t58 string = _goml_runtime_core_float64_to_string(self__20)
    retv57 = t58
    return retv57
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv60 string
    var t61 string = _goml_runtime_core_bool_to_string(self__8)
    retv60 = t61
    return retv60
}

func println__T_string(value__1 string) struct{} {
    var t63 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t63)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv66 string
    retv66 = self__9
    return retv66
}

func main() {
    main0()
}
