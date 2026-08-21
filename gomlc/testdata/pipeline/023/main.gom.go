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

type Tuple2_5int32_6string struct {
    _0 int32
    _1 string
}

type Ordering int32

type Mixed interface {
    isMixed()
}

type OnlyInt struct {
    _0 int32
}

func (_ OnlyInt) isMixed() {}

type OnlyStr struct {
    _0 string
}

func (_ OnlyStr) isMixed() {}

type Both struct {
    _0 int32
    _1 string
}

func (_ Both) isMixed() {}

func match_mixed_pair(pair__0 Tuple2_5int32_6string) int32 {
    var x411 int32 = pair__0._0
    var x412 string = pair__0._1
    switch x412 {
    case "zero":
        switch x411 {
        case 0:
            return 1
        default:
            return 4
        }
    case "one":
        switch x411 {
        case 0:
            return 2
        case 1:
            return 3
        default:
            return 5
        }
    default:
        switch x411 {
        case 0:
            return 2
        default:
            return 5
        }
    }
}

func match_mixed_enum(value__1 Mixed) int32 {
    switch value__1.(type) {
    case OnlyInt:
        var x413 int32 = value__1.(OnlyInt)._0
        switch x413 {
        case 0:
            return 6
        default:
            return 7
        }
    case OnlyStr:
        var x414 string = value__1.(OnlyStr)._0
        switch x414 {
        case "zero":
            return 8
        default:
            return 9
        }
    case Both:
        var x415 int32 = value__1.(Both)._0
        var x416 string = value__1.(Both)._1
        switch x416 {
        case "zero":
            switch x415 {
            case 0:
                return 10
            default:
                return 12
            }
        default:
            switch x415 {
            case 0:
                return 11
            default:
                return 13
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t454 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t455 int32 = match_mixed_pair(t454)
    var t456 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t455)
    println__T_string(t456)
    var t457 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t458 int32 = match_mixed_pair(t457)
    var t459 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t458)
    println__T_string(t459)
    var t460 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t461 int32 = match_mixed_pair(t460)
    var t462 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t461)
    println__T_string(t462)
    var t463 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t464 int32 = match_mixed_pair(t463)
    var t465 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t464)
    var inline548 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t465)
    _goml_runtime_core_string_println(inline548)
    var t466 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t467 int32 = match_mixed_pair(t466)
    var t468 string
    var inline546 string = _goml_runtime_core_int32_to_string(t467)
    t468 = inline546
    var inline543 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t468)
    _goml_runtime_core_string_println(inline543)
    var t469 Mixed = OnlyInt{
        _0: 0,
    }
    var t470 int32 = match_mixed_enum(t469)
    var t471 string
    var inline541 string = _goml_runtime_core_int32_to_string(t470)
    t471 = inline541
    var inline538 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t471)
    _goml_runtime_core_string_println(inline538)
    var t472 Mixed = OnlyInt{
        _0: 5,
    }
    var t473 int32 = match_mixed_enum(t472)
    var t474 string
    var inline536 string = _goml_runtime_core_int32_to_string(t473)
    t474 = inline536
    var inline533 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t474)
    _goml_runtime_core_string_println(inline533)
    var t475 Mixed = OnlyStr{
        _0: "zero",
    }
    var t476 int32 = match_mixed_enum(t475)
    var t477 string
    var inline531 string = _goml_runtime_core_int32_to_string(t476)
    t477 = inline531
    var inline528 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t477)
    _goml_runtime_core_string_println(inline528)
    var t478 Mixed = OnlyStr{
        _0: "hello",
    }
    var t479 int32 = match_mixed_enum(t478)
    var t480 string
    var inline526 string = _goml_runtime_core_int32_to_string(t479)
    t480 = inline526
    var inline523 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t480)
    _goml_runtime_core_string_println(inline523)
    var t481 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t482 int32 = match_mixed_enum(t481)
    var t483 string
    var inline521 string = _goml_runtime_core_int32_to_string(t482)
    t483 = inline521
    var inline518 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t483)
    _goml_runtime_core_string_println(inline518)
    var t484 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t485 int32 = match_mixed_enum(t484)
    var t486 string
    var inline516 string = _goml_runtime_core_int32_to_string(t485)
    t486 = inline516
    var inline513 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t486)
    _goml_runtime_core_string_println(inline513)
    var t487 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t488 int32 = match_mixed_enum(t487)
    var t489 string
    var inline511 string = _goml_runtime_core_int32_to_string(t488)
    t489 = inline511
    var inline508 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t489)
    _goml_runtime_core_string_println(inline508)
    var t490 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t491 int32 = match_mixed_enum(t490)
    var t492 string
    var inline506 string = _goml_runtime_core_int32_to_string(t491)
    t492 = inline506
    var inline503 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t492)
    _goml_runtime_core_string_println(inline503)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t495 string
    t495 = value__1
    _goml_runtime_core_string_println(t495)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t499 string = _goml_runtime_core_int32_to_string(self__33)
    return t499
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
