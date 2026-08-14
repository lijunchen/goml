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
    var x408 int32 = pair__0._0
    var x409 string = pair__0._1
    switch x409 {
    case "zero":
        switch x408 {
        case 0:
            return 1
        default:
            return 4
        }
    case "one":
        switch x408 {
        case 0:
            return 2
        case 1:
            return 3
        default:
            return 5
        }
    default:
        switch x408 {
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
        var x410 int32 = value__1.(OnlyInt)._0
        switch x410 {
        case 0:
            return 6
        default:
            return 7
        }
    case OnlyStr:
        var x411 string = value__1.(OnlyStr)._0
        switch x411 {
        case "zero":
            return 8
        default:
            return 9
        }
    case Both:
        var x412 int32 = value__1.(Both)._0
        var x413 string = value__1.(Both)._1
        switch x413 {
        case "zero":
            switch x412 {
            case 0:
                return 10
            default:
                return 12
            }
        default:
            switch x412 {
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
    var t451 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "zero",
    }
    var t452 int32 = match_mixed_pair(t451)
    var t453 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t452)
    println__T_string(t453)
    var t454 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 0,
        _1: "other",
    }
    var t455 int32 = match_mixed_pair(t454)
    var t456 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t455)
    println__T_string(t456)
    var t457 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 1,
        _1: "one",
    }
    var t458 int32 = match_mixed_pair(t457)
    var t459 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t458)
    println__T_string(t459)
    var t460 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "zero",
    }
    var t461 int32 = match_mixed_pair(t460)
    var t462 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t461)
    var inline545 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t462)
    _goml_runtime_core_string_println(inline545)
    var t463 Tuple2_5int32_6string = Tuple2_5int32_6string{
        _0: 2,
        _1: "two",
    }
    var t464 int32 = match_mixed_pair(t463)
    var t465 string
    var inline543 string = _goml_runtime_core_int32_to_string(t464)
    t465 = inline543
    var inline540 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t465)
    _goml_runtime_core_string_println(inline540)
    var t466 Mixed = OnlyInt{
        _0: 0,
    }
    var t467 int32 = match_mixed_enum(t466)
    var t468 string
    var inline538 string = _goml_runtime_core_int32_to_string(t467)
    t468 = inline538
    var inline535 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t468)
    _goml_runtime_core_string_println(inline535)
    var t469 Mixed = OnlyInt{
        _0: 5,
    }
    var t470 int32 = match_mixed_enum(t469)
    var t471 string
    var inline533 string = _goml_runtime_core_int32_to_string(t470)
    t471 = inline533
    var inline530 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t471)
    _goml_runtime_core_string_println(inline530)
    var t472 Mixed = OnlyStr{
        _0: "zero",
    }
    var t473 int32 = match_mixed_enum(t472)
    var t474 string
    var inline528 string = _goml_runtime_core_int32_to_string(t473)
    t474 = inline528
    var inline525 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t474)
    _goml_runtime_core_string_println(inline525)
    var t475 Mixed = OnlyStr{
        _0: "hello",
    }
    var t476 int32 = match_mixed_enum(t475)
    var t477 string
    var inline523 string = _goml_runtime_core_int32_to_string(t476)
    t477 = inline523
    var inline520 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t477)
    _goml_runtime_core_string_println(inline520)
    var t478 Mixed = Both{
        _0: 0,
        _1: "zero",
    }
    var t479 int32 = match_mixed_enum(t478)
    var t480 string
    var inline518 string = _goml_runtime_core_int32_to_string(t479)
    t480 = inline518
    var inline515 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t480)
    _goml_runtime_core_string_println(inline515)
    var t481 Mixed = Both{
        _0: 0,
        _1: "hello",
    }
    var t482 int32 = match_mixed_enum(t481)
    var t483 string
    var inline513 string = _goml_runtime_core_int32_to_string(t482)
    t483 = inline513
    var inline510 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t483)
    _goml_runtime_core_string_println(inline510)
    var t484 Mixed = Both{
        _0: 2,
        _1: "zero",
    }
    var t485 int32 = match_mixed_enum(t484)
    var t486 string
    var inline508 string = _goml_runtime_core_int32_to_string(t485)
    t486 = inline508
    var inline505 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t486)
    _goml_runtime_core_string_println(inline505)
    var t487 Mixed = Both{
        _0: 3,
        _1: "three",
    }
    var t488 int32 = match_mixed_enum(t487)
    var t489 string
    var inline503 string = _goml_runtime_core_int32_to_string(t488)
    t489 = inline503
    var inline500 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t489)
    _goml_runtime_core_string_println(inline500)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t492 string
    t492 = value__1
    _goml_runtime_core_string_println(t492)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t496 string = _goml_runtime_core_int32_to_string(self__33)
    return t496
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
