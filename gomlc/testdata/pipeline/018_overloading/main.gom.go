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
    var inline473 int32 = 1
    a__7 = inline473
    var b__8 int32
    var inline471 int32 = 2
    b__8 = inline471
    var c__9 int32
    var inline469 int32 = a__7 + b__8
    c__9 = inline469
    var inline466 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(c__9)
    println__T_string(inline466)
    var a__10 int32
    var inline464 int32 = 3
    a__10 = inline464
    var b__11 int32
    var inline462 int32 = 4
    b__11 = inline462
    var c__12 bool
    var inline460 bool = a__10 < b__11
    c__12 = inline460
    var inline457 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(c__12)
    println__T_string(inline457)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t426 string
    t426 = value__1
    _goml_runtime_core_string_println(t426)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t430 string = _goml_runtime_core_int32_to_string(self__154)
    return t430
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t433 string = _goml_runtime_core_bool_to_string(self__148)
    return t433
}

func main() {
    main0()
}
