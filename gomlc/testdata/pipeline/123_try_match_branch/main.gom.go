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

type Ordering int32

type Choice interface {
    isChoice()
}

type Left struct {
    _0 bool
}

func (_ Left) isChoice() {}

type Right struct {
    _0 bool
}

func (_ Right) isChoice() {}

type Keep struct {
    _0 int32
}

func (_ Keep) isChoice() {}

type Result__int32__string interface {
    isResult__int32__string()
}

type Ok struct {
    _0 int32
}

func (_ Ok) isResult__int32__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int32__string() {}

func choose(choice__2 Choice) Result__int32__string {
    var jp439 int32
    switch choice__2.(type) {
    case Left:
        var x408 bool = choice__2.(Left)._0
        var commute_field527 int32
        var commute_field529 string
        if x408 {
            commute_field527 = 10
            jp439 = commute_field527
            var t440 Result__int32__string = Ok{
                _0: jp439,
            }
            return t440
        } else {
            commute_field529 = "left failed"
            var t443 Result__int32__string = Err{
                _0: commute_field529,
            }
            return t443
        }
    case Right:
        var x409 bool = choice__2.(Right)._0
        var mtmp414 Result__int32__string
        if x409 {
            var inline483 Result__int32__string = Ok{
                _0: 20,
            }
            mtmp414 = inline483
        } else {
            var inline484 Result__int32__string = Err{
                _0: "right failed",
            }
            mtmp414 = inline484
        }
        var jp445 int32
        switch mtmp414.(type) {
        case Ok:
            var x415 int32 = mtmp414.(Ok)._0
            jp445 = x415
            var t446 int32 = jp445 + 1
            jp439 = t446
            var t440 Result__int32__string = Ok{
                _0: jp439,
            }
            return t440
        case Err:
            var x416 string = mtmp414.(Err)._0
            var t447 Result__int32__string = Err{
                _0: x416,
            }
            return t447
        default:
            panic("non-exhaustive match")
        }
    case Keep:
        var x410 int32 = choice__2.(Keep)._0
        jp439 = x410
        var t440 Result__int32__string = Ok{
            _0: jp439,
        }
        return t440
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    switch res__7.(type) {
    case Ok:
        var x417 int32 = res__7.(Ok)._0
        var t452 string
        var inline486 string = _goml_runtime_core_int32_to_string(x417)
        t452 = inline486
        var t453 string = "ok " + t452
        return t453
    case Err:
        var x418 string = res__7.(Err)._0
        var t454 string = "err " + x418
        return t454
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t456 Choice = Left{
        _0: true,
    }
    var t457 Result__int32__string = choose(t456)
    var t458 string = show(t457)
    var inline524 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t458)
    _goml_runtime_core_string_println(inline524)
    var t459 Choice = Right{
        _0: true,
    }
    var t460 Result__int32__string = choose(t459)
    var t461 string = show(t460)
    var inline521 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t461)
    _goml_runtime_core_string_println(inline521)
    var t462 Choice = Keep{
        _0: 5,
    }
    var t463 Result__int32__string = choose(t462)
    var t464 string
    switch t463.(type) {
    case Ok:
        var inline513 int32 = t463.(Ok)._0
        var inline515 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline513)
        var inline516 string = "ok " + inline515
        t464 = inline516
    case Err:
        var inline517 string = t463.(Err)._0
        var inline519 string = "err " + inline517
        t464 = inline519
    default:
        panic("non-exhaustive match")
    }
    var inline510 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t464)
    _goml_runtime_core_string_println(inline510)
    var t465 Choice = Left{
        _0: false,
    }
    var t466 Result__int32__string = choose(t465)
    var t467 string
    switch t466.(type) {
    case Ok:
        var inline502 int32 = t466.(Ok)._0
        var inline504 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline502)
        var inline505 string = "ok " + inline504
        t467 = inline505
    case Err:
        var inline506 string = t466.(Err)._0
        var inline508 string = "err " + inline506
        t467 = inline508
    default:
        panic("non-exhaustive match")
    }
    var inline499 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t467)
    _goml_runtime_core_string_println(inline499)
    var t468 Choice = Right{
        _0: false,
    }
    var t469 Result__int32__string = choose(t468)
    var t470 string
    switch t469.(type) {
    case Ok:
        var inline491 int32 = t469.(Ok)._0
        var inline493 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline491)
        var inline494 string = "ok " + inline493
        t470 = inline494
    case Err:
        var inline495 string = t469.(Err)._0
        var inline497 string = "err " + inline495
        t470 = inline497
    default:
        panic("non-exhaustive match")
    }
    var inline488 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t470)
    _goml_runtime_core_string_println(inline488)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t473 string = _goml_runtime_core_int32_to_string(self__33)
    return t473
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
