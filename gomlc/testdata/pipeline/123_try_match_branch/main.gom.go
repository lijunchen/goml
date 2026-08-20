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

type Choice struct {
    _tag int32
    _v0_0 bool
    _v1_0 bool
    _v2_0 int32
}

type Result__int32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

func choose(choice__2 Choice) Result__int32__string {
    var jp439 int32
    switch choice__2._tag {
    case 0:
        var x408 bool = choice__2._v0_0
        var commute_field527 int32
        var commute_field529 string
        if x408 {
            commute_field527 = 10
            jp439 = commute_field527
            var t440 Result__int32__string = Result__int32__string{
                _tag: 0,
                _v0_0: jp439,
            }
            return t440
        } else {
            commute_field529 = "left failed"
            var t443 Result__int32__string = Result__int32__string{
                _tag: 1,
                _v1_0: commute_field529,
            }
            return t443
        }
    case 1:
        var x409 bool = choice__2._v1_0
        var mtmp414 Result__int32__string
        if x409 {
            var inline483 Result__int32__string = Result__int32__string{
                _tag: 0,
                _v0_0: 20,
            }
            mtmp414 = inline483
        } else {
            var inline484 Result__int32__string = Result__int32__string{
                _tag: 1,
                _v1_0: "right failed",
            }
            mtmp414 = inline484
        }
        var jp445 int32
        switch mtmp414._tag {
        case 0:
            var x415 int32 = mtmp414._v0_0
            jp445 = x415
            var t446 int32 = jp445 + 1
            jp439 = t446
            var t440 Result__int32__string = Result__int32__string{
                _tag: 0,
                _v0_0: jp439,
            }
            return t440
        case 1:
            var x416 string = mtmp414._v1_0
            var t447 Result__int32__string = Result__int32__string{
                _tag: 1,
                _v1_0: x416,
            }
            return t447
        default:
            panic("non-exhaustive match")
        }
    case 2:
        var x410 int32 = choice__2._v2_0
        jp439 = x410
        var t440 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: jp439,
        }
        return t440
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__int32__string) string {
    switch res__7._tag {
    case 0:
        var x417 int32 = res__7._v0_0
        var t452 string
        var inline486 string = _goml_runtime_core_int32_to_string(x417)
        t452 = inline486
        var t453 string = "ok " + t452
        return t453
    case 1:
        var x418 string = res__7._v1_0
        var t454 string = "err " + x418
        return t454
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t456 Choice = Choice{
        _tag: 0,
        _v0_0: true,
    }
    var t457 Result__int32__string = choose(t456)
    var t458 string = show(t457)
    var inline524 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t458)
    _goml_runtime_core_string_println(inline524)
    var t459 Choice = Choice{
        _tag: 1,
        _v1_0: true,
    }
    var t460 Result__int32__string = choose(t459)
    var t461 string = show(t460)
    var inline521 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t461)
    _goml_runtime_core_string_println(inline521)
    var t462 Choice = Choice{
        _tag: 2,
        _v2_0: 5,
    }
    var t463 Result__int32__string = choose(t462)
    var t464 string
    switch t463._tag {
    case 0:
        var inline513 int32 = t463._v0_0
        var inline515 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline513)
        var inline516 string = "ok " + inline515
        t464 = inline516
    case 1:
        var inline517 string = t463._v1_0
        var inline519 string = "err " + inline517
        t464 = inline519
    default:
        panic("non-exhaustive match")
    }
    var inline510 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t464)
    _goml_runtime_core_string_println(inline510)
    var t465 Choice = Choice{
        _tag: 0,
        _v0_0: false,
    }
    var t466 Result__int32__string = choose(t465)
    var t467 string
    switch t466._tag {
    case 0:
        var inline502 int32 = t466._v0_0
        var inline504 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline502)
        var inline505 string = "ok " + inline504
        t467 = inline505
    case 1:
        var inline506 string = t466._v1_0
        var inline508 string = "err " + inline506
        t467 = inline508
    default:
        panic("non-exhaustive match")
    }
    var inline499 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t467)
    _goml_runtime_core_string_println(inline499)
    var t468 Choice = Choice{
        _tag: 1,
        _v1_0: false,
    }
    var t469 Result__int32__string = choose(t468)
    var t470 string
    switch t469._tag {
    case 0:
        var inline491 int32 = t469._v0_0
        var inline493 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline491)
        var inline494 string = "ok " + inline493
        t470 = inline494
    case 1:
        var inline495 string = t469._v1_0
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
