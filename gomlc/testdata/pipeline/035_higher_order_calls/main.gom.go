package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_2_20Fn1_5int32_to_5int32(arr [2]func(int32) int32, index int) func(int32) int32 {
    return arr[index]
}

type Ordering int32

func double(x__0 int32) int32 {
    var t416 int32 = x__0 * 2
    return t416
}

func increment(x__1 int32) int32 {
    var t419 int32 = x__1 + 1
    return t419
}

func main0() struct{} {
    var xs__3 [2]func(int32) int32 = [2]func(int32) int32{double, increment}
    var f__4 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 0)
    var g__5 func(int32) int32 = array_get__Array_2_20Fn1_5int32_to_5int32(xs__3, 1)
    var t425 int32 = f__4(10)
    var t426 int32 = g__5(t425)
    var inline459 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t426)
    _goml_runtime_core_string_println(inline459)
    var chosen__6 func(int32) int32
    var inline457 bool = true
    if inline457 {
        chosen__6 = double
    } else {
        chosen__6 = increment
    }
    var applied__7 int32 = chosen__6(5)
    var t427 func(int32) int32
    var inline455 bool = false
    if inline455 {
        t427 = double
    } else {
        t427 = increment
    }
    var direct__8 int32 = t427(5)
    var t428 string
    var inline453 string = _goml_runtime_core_int32_to_string(applied__7)
    t428 = inline453
    var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t428)
    _goml_runtime_core_string_println(inline450)
    var t429 string
    var inline448 string = _goml_runtime_core_int32_to_string(direct__8)
    t429 = inline448
    var inline445 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline445)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t441 string = _goml_runtime_core_int32_to_string(self__154)
    return t441
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
