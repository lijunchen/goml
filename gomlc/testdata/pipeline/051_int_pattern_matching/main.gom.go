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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4int8_5int16 struct {
    _0 int8
    _1 int16
}

type PairData struct {
    head int32
    tail int64
}

type Ordering int32

func is_special8(value__0 int8) bool {
    switch value__0 {
    case 5:
        return true
    case 7:
        return true
    default:
        return false
    }
}

func match_tuple(values__4 Tuple2_4int8_5int16) bool {
    var x408 int8 = values__4._0
    var x409 int16 = values__4._1
    switch x409 {
    case 2:
        switch x408 {
        case 1:
            return true
        default:
            return false
        }
    default:
        return false
    }
}

func match_struct(pair__5 PairData) bool {
    var x410 int32 = pair__5.head
    var x411 int64 = pair__5.tail
    switch x411 {
    case 200:
        switch x410 {
        case 100:
            return true
        default:
            return false
        }
    case 300:
        return true
    default:
        return false
    }
}

func main0() struct{} {
    var tuple_first__8 int8 = 1
    var tuple_second__9 int16 = 2
    var t446 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t446)
    var t447 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t447)
    var t448 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t448)
    var t449 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t449)
    var t450 bool = is_special8(5)
    var part1__14 string
    var inline516 string = "int8="
    var inline517 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t450)
    var inline518 string = inline516 + inline517
    part1__14 = inline518
    var t451 bool
    var inline514 int16 = 1024
    switch inline514 {
    case 1024:
        t451 = true
    case 2048:
        t451 = true
    default:
        t451 = false
    }
    var part2__15 string
    var inline510 string = ",int16="
    var inline511 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t451)
    var inline512 string = inline510 + inline511
    part2__15 = inline512
    var t452 bool
    var inline508 int32 = 8192
    switch inline508 {
    case 4096:
        t452 = true
    case 8192:
        t452 = true
    default:
        t452 = false
    }
    var part3__16 string
    var inline504 string = ",int32="
    var inline505 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t452)
    var inline506 string = inline504 + inline505
    part3__16 = inline506
    var t453 bool
    var inline502 int64 = 16384
    switch inline502 {
    case 16384:
        t453 = true
    case 32768:
        t453 = true
    default:
        t453 = false
    }
    var part4__17 string
    var inline498 string = ",int64_a="
    var inline499 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t453)
    var inline500 string = inline498 + inline499
    part4__17 = inline500
    var t454 bool
    var inline496 int64 = 32768
    switch inline496 {
    case 16384:
        t454 = true
    case 32768:
        t454 = true
    default:
        t454 = false
    }
    var part5__18 string
    var inline492 string = ",int64_b="
    var inline493 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t454)
    var inline494 string = inline492 + inline493
    part5__18 = inline494
    var part6__19 string
    var inline488 string = ",tuple_hit="
    var inline489 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_hit__10)
    var inline490 string = inline488 + inline489
    part6__19 = inline490
    var part7__20 string
    var inline484 string = ",tuple_miss="
    var inline485 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_miss__11)
    var inline486 string = inline484 + inline485
    part7__20 = inline486
    var part8__21 string
    var inline480 string = ",struct_first="
    var inline481 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_first__12)
    var inline482 string = inline480 + inline481
    part8__21 = inline482
    var part9__22 string
    var inline476 string = ",struct_second="
    var inline477 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_second__13)
    var inline478 string = inline476 + inline477
    part9__22 = inline478
    var t455 string = part1__14 + part2__15
    var t456 string = t455 + part3__16
    var t457 string = t456 + part4__17
    var t458 string = t457 + part5__18
    var t459 string = t458 + part6__19
    var t460 string = t459 + part7__20
    var t461 string = t460 + part8__21
    var message__23 string = t461 + part9__22
    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__23)
    _goml_runtime_core_string_println(inline473)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t464 string = _goml_runtime_core_bool_to_string(self__148)
    return t464
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
