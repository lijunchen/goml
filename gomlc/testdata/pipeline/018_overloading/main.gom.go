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

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var a__7 int32
    var inline476 int32 = 1
    a__7 = inline476
    var b__8 int32
    var inline474 int32 = 2
    b__8 = inline474
    var c__9 int32
    var inline472 int32 = a__7 + b__8
    c__9 = inline472
    var inline469 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(c__9)
    println__T_string(inline469)
    var a__10 int32
    var inline467 int32 = 3
    a__10 = inline467
    var b__11 int32
    var inline465 int32 = 4
    b__11 = inline465
    var c__12 bool
    var inline463 bool = a__10 < b__11
    c__12 = inline463
    var inline460 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(c__12)
    println__T_string(inline460)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t429 string
    t429 = value__1
    _goml_runtime_core_string_println(t429)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t433 string = _goml_runtime_core_int32_to_string(self__154)
    return t433
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t436 string = _goml_runtime_core_bool_to_string(self__148)
    return t436
}

func main() {
    main0()
}
