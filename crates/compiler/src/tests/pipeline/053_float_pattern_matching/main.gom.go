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
    var retv69 string
    var jp71 string
    switch event__0.(type) {
    case Sample32:
        var x61 string = event__0.(Sample32)._0
        var x62 float32 = event__0.(Sample32)._1
        var value__2 float32 = x62
        var label__1 string = x61
        var t72 string = _goml_m_trait__impl_i_ToString_i_float32_i_to__string(value__2)
        var t73 string = label__1 + t72
        jp71 = t73
    case Sample64:
        var x63 string = event__0.(Sample64)._0
        var x64 float64 = event__0.(Sample64)._1
        var value__4 float64 = x64
        var label__3 string = x63
        var t74 string = _goml_m_trait__impl_i_ToString_i_float64_i_to__string(value__4)
        var t75 string = label__3 + t74
        jp71 = t75
    default:
        panic("non-exhaustive match")
    }
    retv69 = jp71
    return retv69
}

func compare(values__5 Tuple2_7float32_7float64) string {
    var retv77 string
    var x65 float32 = values__5._0
    var x66 float64 = values__5._1
    var right__7 float64 = x66
    var left__6 float32 = x65
    var limit32__8 float32 = 1
    var limit64__9 float64 = 5
    var less_left__10 bool = left__6 < limit32__8
    var less_right__11 bool = right__7 < limit64__9
    var t78 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_left__10)
    var t79 string = "left<1?=" + t78
    var t80 string = t79 + ",right<5?="
    var t81 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less_right__11)
    var t82 string = t80 + t81
    retv77 = t82
    return retv77
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
    var t84 string = summarize(first__15)
    var t85 string = summarize(second__16)
    var t86 string = t84 + t85
    var t87 string = summarize(third__17)
    var t88 string = t86 + t87
    var t89 string = compare(tuple__18)
    var t90 string = t88 + t89
    var t91 string = compare(tuple_other__19)
    var message__20 string = t90 + t91
    println__T_string(message__20)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_float32_i_to__string(self__47 float32) string {
    var retv93 string
    var t94 string = _goml_runtime_core_float32_to_string(self__47)
    retv93 = t94
    return retv93
}

func _goml_m_trait__impl_i_ToString_i_float64_i_to__string(self__48 float64) string {
    var retv96 string
    var t97 string = _goml_runtime_core_float64_to_string(self__48)
    retv96 = t97
    return retv96
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv99 string
    var t100 string = _goml_runtime_core_bool_to_string(self__36)
    retv99 = t100
    return retv99
}

func println__T_string(value__1 string) struct{} {
    var t102 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t102)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv105 string
    retv105 = self__37
    return retv105
}

func main() {
    main0()
}
