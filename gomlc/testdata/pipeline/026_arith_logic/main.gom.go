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
    var base__4 int32 = 10
    var sum__5 int32 = base__4 + 5
    var diff__6 int32 = sum__5 - 3
    var prod__7 int32 = diff__6 * 2
    var quot__8 int32 = prod__7 / 4
    var inline504 string = "sum="
    var inline505 string = _goml_m_inherent_i_int32_i_int32_i_to__string(sum__5)
    var inline506 string = inline504 + inline505
    println__T_string(inline506)
    var inline499 string = "diff="
    var inline500 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff__6)
    var inline501 string = inline499 + inline500
    println__T_string(inline501)
    var inline494 string = "prod="
    var inline495 string = _goml_m_inherent_i_int32_i_int32_i_to__string(prod__7)
    var inline496 string = inline494 + inline495
    println__T_string(inline496)
    var inline489 string = "quot="
    var inline490 string = _goml_m_inherent_i_int32_i_int32_i_to__string(quot__8)
    var inline491 string = inline489 + inline490
    println__T_string(inline491)
    var jp428 bool
    jp428 = false
    var jp430 bool
    jp430 = true
    var not_result__11 bool = !false
    var t441 bool = !jp428
    var jp434 bool
    if t441 {
        var t442 int32 = prod__7 * base__4
        var t443 int32 = sum__5 + t442
        var t444 int32 = prod__7 / 2
        var mtmp414 int32 = t443 - t444
        switch mtmp414 {
        case 0:
            jp434 = false
        default:
            jp434 = true
        }
    } else {
        jp434 = false
    }
    var jp432 bool
    if jp434 {
        jp432 = true
    } else {
        var t435 int32 = diff__6 - quot__8
        var t436 int32 = t435 + base__4
        var t437 int32 = sum__5 / 2
        var mtmp415 int32 = t436 - t437
        var jp439 bool
        switch mtmp415 {
        case 0:
            jp439 = false
        default:
            jp439 = true
        }
        var t440 bool = !jp439
        jp432 = t440
    }
    var inline484 string = "and="
    var inline485 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp428)
    var inline486 string = inline484 + inline485
    println__T_string(inline486)
    var inline479 string = "or="
    var inline480 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp430)
    var inline481 string = inline479 + inline480
    println__T_string(inline481)
    var inline474 string = "not="
    var inline475 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(not_result__11)
    var inline476 string = inline474 + inline475
    println__T_string(inline476)
    var inline469 string = "mixed="
    var inline470 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp432)
    var inline471 string = inline469 + inline470
    println__T_string(inline471)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t448 string
    t448 = value__1
    _goml_runtime_core_string_println(t448)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t452 string = _goml_runtime_core_int32_to_string(self__33)
    return t452
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t455 string = _goml_runtime_core_bool_to_string(self__148)
    return t455
}

func main() {
    main0()
}
