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
    var inline497 string = "a="
    var inline498 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(a__4)
    var inline499 string = inline497 + inline498
    println__T_string(inline499)
    var inline492 string = "b="
    var inline493 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(b__5)
    var inline494 string = inline492 + inline493
    println__T_string(inline494)
    var inline487 string = "c="
    var inline488 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(c__6)
    var inline489 string = inline487 + inline488
    println__T_string(inline489)
    var inline482 string = "sum="
    var inline483 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(sum__7)
    var inline484 string = inline482 + inline483
    println__T_string(inline484)
    var inline477 string = "diff="
    var inline478 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(diff__8)
    var inline479 string = inline477 + inline478
    println__T_string(inline479)
    var inline472 string = "prod="
    var inline473 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(prod__9)
    var inline474 string = inline472 + inline473
    println__T_string(inline474)
    var inline467 string = "quot="
    var inline468 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(quot__10)
    var inline469 string = inline467 + inline468
    println__T_string(inline469)
    var inline462 string = "neg="
    var inline463 string = _goml_m_trait__impl_i_ToString_i_int8_i_to__string(neg__11)
    var inline464 string = inline462 + inline463
    println__T_string(inline464)
    var inline457 string = "b<a="
    var inline458 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(less__12)
    var inline459 string = inline457 + inline458
    println__T_string(inline459)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t436 string
    t436 = value__1
    _goml_runtime_core_string_println(t436)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int8_i_to__string(self__152 int8) string {
    var t440 string = _goml_runtime_core_int8_to_string(self__152)
    return t440
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t443 string = _goml_runtime_core_bool_to_string(self__148)
    return t443
}

func main() {
    main0()
}
