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

func _goml_runtime_core_int8_to_string(x int8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

func main0() struct{} {
    var a__4 int8
    a__4 = 90
    var b__5 int8
    b__5 = -20
    var c__6 int8
    c__6 = 3
    var sum__7 int8 = a__4 + b__5
    var diff__8 int8 = a__4 - c__6
    var prod__9 int8 = b__5 * c__6
    var quot__10 int8 = a__4 / c__6
    var neg__11 int8 = -b__5
    var less__12 bool = b__5 < a__4
    var inline494 string = "a="
    var inline495 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__4)
    var inline496 string = inline494 + inline495
    println__T_string(inline496)
    var inline489 string = "b="
    var inline490 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__5)
    var inline491 string = inline489 + inline490
    println__T_string(inline491)
    var inline484 string = "c="
    var inline485 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(c__6)
    var inline486 string = inline484 + inline485
    println__T_string(inline486)
    var inline479 string = "sum="
    var inline480 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(sum__7)
    var inline481 string = inline479 + inline480
    println__T_string(inline481)
    var inline474 string = "diff="
    var inline475 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(diff__8)
    var inline476 string = inline474 + inline475
    println__T_string(inline476)
    var inline469 string = "prod="
    var inline470 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(prod__9)
    var inline471 string = inline469 + inline470
    println__T_string(inline471)
    var inline464 string = "quot="
    var inline465 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(quot__10)
    var inline466 string = inline464 + inline465
    println__T_string(inline466)
    var inline459 string = "neg="
    var inline460 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(neg__11)
    var inline461 string = inline459 + inline460
    println__T_string(inline461)
    var inline454 string = "b<a="
    var inline455 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__12)
    var inline456 string = inline454 + inline455
    println__T_string(inline456)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t433 string
    t433 = value__1
    _goml_runtime_core_string_println(t433)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__152 int8) string {
    var t437 string = _goml_runtime_core_int8_to_string(self__152)
    return t437
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t440 string = _goml_runtime_core_bool_to_string(self__148)
    return t440
}

func main() {
    main0()
}
