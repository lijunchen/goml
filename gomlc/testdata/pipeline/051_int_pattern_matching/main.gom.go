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
    var x411 int8 = values__4._0
    var x412 int16 = values__4._1
    switch x412 {
    case 2:
        switch x411 {
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
    var x413 int32 = pair__5.head
    var x414 int64 = pair__5.tail
    switch x414 {
    case 200:
        switch x413 {
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
    var t449 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: tuple_first__8,
        _1: tuple_second__9,
    }
    var tuple_result_hit__10 bool = match_tuple(t449)
    var t450 Tuple2_4int8_5int16 = Tuple2_4int8_5int16{
        _0: 3,
        _1: 4,
    }
    var tuple_result_miss__11 bool = match_tuple(t450)
    var t451 PairData = PairData{
        head: 100,
        tail: 200,
    }
    var pair_first__12 bool = match_struct(t451)
    var t452 PairData = PairData{
        head: 10,
        tail: 300,
    }
    var pair_second__13 bool = match_struct(t452)
    var t453 bool = is_special8(5)
    var part1__14 string
    var inline519 string = "i8="
    var inline520 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t453)
    var inline521 string = inline519 + inline520
    part1__14 = inline521
    var t454 bool
    var inline517 int16 = 1024
    switch inline517 {
    case 1024:
        t454 = true
    case 2048:
        t454 = true
    default:
        t454 = false
    }
    var part2__15 string
    var inline513 string = ",i16="
    var inline514 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t454)
    var inline515 string = inline513 + inline514
    part2__15 = inline515
    var t455 bool
    var inline511 int32 = 8192
    switch inline511 {
    case 4096:
        t455 = true
    case 8192:
        t455 = true
    default:
        t455 = false
    }
    var part3__16 string
    var inline507 string = ",i32="
    var inline508 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t455)
    var inline509 string = inline507 + inline508
    part3__16 = inline509
    var t456 bool
    var inline505 int64 = 16384
    switch inline505 {
    case 16384:
        t456 = true
    case 32768:
        t456 = true
    default:
        t456 = false
    }
    var part4__17 string
    var inline501 string = ",int64_a="
    var inline502 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t456)
    var inline503 string = inline501 + inline502
    part4__17 = inline503
    var t457 bool
    var inline499 int64 = 32768
    switch inline499 {
    case 16384:
        t457 = true
    case 32768:
        t457 = true
    default:
        t457 = false
    }
    var part5__18 string
    var inline495 string = ",int64_b="
    var inline496 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t457)
    var inline497 string = inline495 + inline496
    part5__18 = inline497
    var part6__19 string
    var inline491 string = ",tuple_hit="
    var inline492 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_hit__10)
    var inline493 string = inline491 + inline492
    part6__19 = inline493
    var part7__20 string
    var inline487 string = ",tuple_miss="
    var inline488 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(tuple_result_miss__11)
    var inline489 string = inline487 + inline488
    part7__20 = inline489
    var part8__21 string
    var inline483 string = ",struct_first="
    var inline484 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_first__12)
    var inline485 string = inline483 + inline484
    part8__21 = inline485
    var part9__22 string
    var inline479 string = ",struct_second="
    var inline480 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(pair_second__13)
    var inline481 string = inline479 + inline480
    part9__22 = inline481
    var t458 string = part1__14 + part2__15
    var t459 string = t458 + part3__16
    var t460 string = t459 + part4__17
    var t461 string = t460 + part5__18
    var t462 string = t461 + part6__19
    var t463 string = t462 + part7__20
    var t464 string = t463 + part8__21
    var message__23 string = t464 + part9__22
    var inline476 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(message__23)
    _goml_runtime_core_string_println(inline476)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t467 string = _goml_runtime_core_bool_to_string(self__148)
    return t467
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
