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
    var inline507 string = "sum="
    var inline508 string = _goml_m_inherent_i_int32_i_int32_i_to__string(sum__5)
    var inline509 string = inline507 + inline508
    println__T_string(inline509)
    var inline502 string = "diff="
    var inline503 string = _goml_m_inherent_i_int32_i_int32_i_to__string(diff__6)
    var inline504 string = inline502 + inline503
    println__T_string(inline504)
    var inline497 string = "prod="
    var inline498 string = _goml_m_inherent_i_int32_i_int32_i_to__string(prod__7)
    var inline499 string = inline497 + inline498
    println__T_string(inline499)
    var inline492 string = "quot="
    var inline493 string = _goml_m_inherent_i_int32_i_int32_i_to__string(quot__8)
    var inline494 string = inline492 + inline493
    println__T_string(inline494)
    var jp431 bool
    jp431 = false
    var jp433 bool
    jp433 = true
    var not_result__11 bool = !false
    var t444 bool = !jp431
    var jp437 bool
    if t444 {
        var t445 int32 = prod__7 * base__4
        var t446 int32 = sum__5 + t445
        var t447 int32 = prod__7 / 2
        var mtmp417 int32 = t446 - t447
        switch mtmp417 {
        case 0:
            jp437 = false
        default:
            jp437 = true
        }
    } else {
        jp437 = false
    }
    var jp435 bool
    if jp437 {
        jp435 = true
    } else {
        var t438 int32 = diff__6 - quot__8
        var t439 int32 = t438 + base__4
        var t440 int32 = sum__5 / 2
        var mtmp418 int32 = t439 - t440
        var jp442 bool
        switch mtmp418 {
        case 0:
            jp442 = false
        default:
            jp442 = true
        }
        var t443 bool = !jp442
        jp435 = t443
    }
    var inline487 string = "and="
    var inline488 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp431)
    var inline489 string = inline487 + inline488
    println__T_string(inline489)
    var inline482 string = "or="
    var inline483 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp433)
    var inline484 string = inline482 + inline483
    println__T_string(inline484)
    var inline477 string = "not="
    var inline478 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(not_result__11)
    var inline479 string = inline477 + inline478
    println__T_string(inline479)
    var inline472 string = "mixed="
    var inline473 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp435)
    var inline474 string = inline472 + inline473
    println__T_string(inline474)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t451 string
    t451 = value__1
    _goml_runtime_core_string_println(t451)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t455 string = _goml_runtime_core_int32_to_string(self__33)
    return t455
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t458 string = _goml_runtime_core_bool_to_string(self__148)
    return t458
}

func main() {
    main0()
}
