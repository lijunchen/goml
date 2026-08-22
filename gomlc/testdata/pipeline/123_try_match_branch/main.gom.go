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

type Result__i32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

func choose(choice__2 Choice) Result__i32__string {
    var jp442 int32
    switch choice__2._tag {
    case 0:
        var x411 bool = choice__2._v0_0
        var commute_field530 int32
        var commute_field532 string
        if x411 {
            commute_field530 = 10
            jp442 = commute_field530
            var t443 Result__i32__string = Result__i32__string{
                _tag: 0,
                _v0_0: jp442,
            }
            return t443
        } else {
            commute_field532 = "left failed"
            var t446 Result__i32__string = Result__i32__string{
                _tag: 1,
                _v1_0: commute_field532,
            }
            return t446
        }
    case 1:
        var x412 bool = choice__2._v1_0
        var mtmp417 Result__i32__string
        if x412 {
            var inline486 Result__i32__string = Result__i32__string{
                _tag: 0,
                _v0_0: 20,
            }
            mtmp417 = inline486
        } else {
            var inline487 Result__i32__string = Result__i32__string{
                _tag: 1,
                _v1_0: "right failed",
            }
            mtmp417 = inline487
        }
        var jp448 int32
        switch mtmp417._tag {
        case 0:
            var x418 int32 = mtmp417._v0_0
            jp448 = x418
            var t449 int32 = jp448 + 1
            jp442 = t449
            var t443 Result__i32__string = Result__i32__string{
                _tag: 0,
                _v0_0: jp442,
            }
            return t443
        case 1:
            var x419 string = mtmp417._v1_0
            var t450 Result__i32__string = Result__i32__string{
                _tag: 1,
                _v1_0: x419,
            }
            return t450
        default:
            panic("non-exhaustive match")
        }
    case 2:
        var x413 int32 = choice__2._v2_0
        jp442 = x413
        var t443 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: jp442,
        }
        return t443
    default:
        panic("non-exhaustive match")
    }
}

func show(res__7 Result__i32__string) string {
    switch res__7._tag {
    case 0:
        var x420 int32 = res__7._v0_0
        var t455 string
        var inline489 string = _goml_runtime_core_int32_to_string(x420)
        t455 = inline489
        var t456 string = "ok " + t455
        return t456
    case 1:
        var x421 string = res__7._v1_0
        var t457 string = "err " + x421
        return t457
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t459 Choice = Choice{
        _tag: 0,
        _v0_0: true,
    }
    var t460 Result__i32__string = choose(t459)
    var t461 string = show(t460)
    var inline527 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t461)
    _goml_runtime_core_string_println(inline527)
    var t462 Choice = Choice{
        _tag: 1,
        _v1_0: true,
    }
    var t463 Result__i32__string = choose(t462)
    var t464 string = show(t463)
    var inline524 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t464)
    _goml_runtime_core_string_println(inline524)
    var t465 Choice = Choice{
        _tag: 2,
        _v2_0: 5,
    }
    var t466 Result__i32__string = choose(t465)
    var t467 string
    switch t466._tag {
    case 0:
        var inline516 int32 = t466._v0_0
        var inline518 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline516)
        var inline519 string = "ok " + inline518
        t467 = inline519
    case 1:
        var inline520 string = t466._v1_0
        var inline522 string = "err " + inline520
        t467 = inline522
    default:
        panic("non-exhaustive match")
    }
    var inline513 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t467)
    _goml_runtime_core_string_println(inline513)
    var t468 Choice = Choice{
        _tag: 0,
        _v0_0: false,
    }
    var t469 Result__i32__string = choose(t468)
    var t470 string
    switch t469._tag {
    case 0:
        var inline505 int32 = t469._v0_0
        var inline507 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline505)
        var inline508 string = "ok " + inline507
        t470 = inline508
    case 1:
        var inline509 string = t469._v1_0
        var inline511 string = "err " + inline509
        t470 = inline511
    default:
        panic("non-exhaustive match")
    }
    var inline502 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t470)
    _goml_runtime_core_string_println(inline502)
    var t471 Choice = Choice{
        _tag: 1,
        _v1_0: false,
    }
    var t472 Result__i32__string = choose(t471)
    var t473 string
    switch t472._tag {
    case 0:
        var inline494 int32 = t472._v0_0
        var inline496 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline494)
        var inline497 string = "ok " + inline496
        t473 = inline497
    case 1:
        var inline498 string = t472._v1_0
        var inline500 string = "err " + inline498
        t473 = inline500
    default:
        panic("non-exhaustive match")
    }
    var inline491 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t473)
    _goml_runtime_core_string_println(inline491)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t476 string = _goml_runtime_core_int32_to_string(self__33)
    return t476
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
