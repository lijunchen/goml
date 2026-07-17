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
    var retv66 string
    var jp68 string
    switch event__0.(type) {
    case Sample32:
        var x58 string = event__0.(Sample32)._0
        var x59 float32 = event__0.(Sample32)._1
        var value__2 float32 = x59
        var label__1 string = x58
        var t69 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__2)
        var t70 string = label__1 + t69
        jp68 = t70
    case Sample64:
        var x60 string = event__0.(Sample64)._0
        var x61 float64 = event__0.(Sample64)._1
        var value__4 float64 = x61
        var label__3 string = x60
        var t71 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
        var t72 string = label__3 + t71
        jp68 = t72
    default:
        panic("non-exhaustive match")
    }
    retv66 = jp68
    return retv66
}

func compare(values__5 Tuple2_7float32_7float64) string {
    var retv74 string
    var x62 float32 = values__5._0
    var x63 float64 = values__5._1
    var right__7 float64 = x63
    var left__6 float32 = x62
    var limit32__8 float32 = 1
    var limit64__9 float64 = 5
    var less_left__10 bool = left__6 < limit32__8
    var less_right__11 bool = right__7 < limit64__9
    var t75 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_left__10)
    var t76 string = "left<1?=" + t75
    var t77 string = t76 + ",right<5?="
    var t78 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_right__11)
    var t79 string = t77 + t78
    retv74 = t79
    return retv74
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
    var t81 string = summarize(first__15)
    var t82 string = summarize(second__16)
    var t83 string = t81 + t82
    var t84 string = summarize(third__17)
    var t85 string = t83 + t84
    var t86 string = compare(tuple__18)
    var t87 string = t85 + t86
    var t88 string = compare(tuple_other__19)
    var message__20 string = t87 + t88
    println__T_string(message__20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__44 float32) string {
    var retv90 string
    var t91 string = _goml_runtime_core_float32_to_string(self__44)
    retv90 = t91
    return retv90
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__45 float64) string {
    var retv93 string
    var t94 string = _goml_runtime_core_float64_to_string(self__45)
    retv93 = t94
    return retv93
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv96 string
    var t97 string = _goml_runtime_core_bool_to_string(self__33)
    retv96 = t97
    return retv96
}

func println__T_string(value__1 string) struct{} {
    var t99 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t99)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv102 string
    retv102 = self__34
    return retv102
}

func main() {
    main0()
}
