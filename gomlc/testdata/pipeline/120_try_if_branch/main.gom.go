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

type Result__int32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

func parse(flag__0 bool) Result__int32__string {
    if flag__0 {
        var t420 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: 5,
        }
        return t420
    } else {
        var t421 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: "bad-branch",
        }
        return t421
    }
}

func bump(flag__1 bool, fallback__2 bool) Result__int32__string {
    var jp425 int32
    if flag__1 {
        var commute_field498 int32
        var commute_field500 string
        if fallback__2 {
            commute_field498 = 5
            jp425 = commute_field498
            var t426 int32 = jp425 + 1
            var t427 Result__int32__string = Result__int32__string{
                _tag: 0,
                _v0_0: t426,
            }
            return t427
        } else {
            commute_field500 = "bad-branch"
            var t430 Result__int32__string = Result__int32__string{
                _tag: 1,
                _v1_0: commute_field500,
            }
            return t430
        }
    } else {
        jp425 = 10
        var t426 int32 = jp425 + 1
        var t427 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: t426,
        }
        return t427
    }
}

func show(res__4 Result__int32__string) string {
    switch res__4._tag {
    case 0:
        var x411 int32 = res__4._v0_0
        var t435 string
        var inline457 string = _goml_runtime_core_int32_to_string(x411)
        t435 = inline457
        var t436 string = "ok=" + t435
        return t436
    case 1:
        var x412 string = res__4._v1_0
        var t437 string = "err=" + x412
        return t437
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t439 Result__int32__string = bump(true, true)
    var t440 string = show(t439)
    var inline495 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
    _goml_runtime_core_string_println(inline495)
    var t441 Result__int32__string = bump(true, false)
    var t442 string
    switch t441._tag {
    case 0:
        var inline487 int32 = t441._v0_0
        var inline489 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline487)
        var inline490 string = "ok=" + inline489
        t442 = inline490
    case 1:
        var inline491 string = t441._v1_0
        var inline493 string = "err=" + inline491
        t442 = inline493
    default:
        panic("non-exhaustive match")
    }
    var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t442)
    _goml_runtime_core_string_println(inline484)
    var t443 Result__int32__string
    var inline470 bool = false
    var inline471 bool = false
    var inline473 int32
    if inline470 {
        var inline477 Result__int32__string = parse(inline471)
        switch inline477._tag {
        case 0:
            var inline478 int32 = inline477._v0_0
            inline473 = inline478
            var inline475 int32 = inline473 + 1
            var inline476 Result__int32__string = Result__int32__string{
                _tag: 0,
                _v0_0: inline475,
            }
            t443 = inline476
            var t444 string
            switch t443._tag {
            case 0:
                var inline462 int32 = t443._v0_0
                var inline464 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline462)
                var inline465 string = "ok=" + inline464
                t444 = inline465
            case 1:
                var inline466 string = t443._v1_0
                var inline468 string = "err=" + inline466
                t444 = inline468
            default:
                panic("non-exhaustive match")
            }
            var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
            _goml_runtime_core_string_println(inline459)
            return struct{}{}
        case 1:
            var inline480 string = inline477._v1_0
            var inline482 Result__int32__string = Result__int32__string{
                _tag: 1,
                _v1_0: inline480,
            }
            t443 = inline482
            var t444 string
            switch t443._tag {
            case 0:
                var inline462 int32 = t443._v0_0
                var inline464 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline462)
                var inline465 string = "ok=" + inline464
                t444 = inline465
            case 1:
                var inline466 string = t443._v1_0
                var inline468 string = "err=" + inline466
                t444 = inline468
            default:
                panic("non-exhaustive match")
            }
            var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
            _goml_runtime_core_string_println(inline459)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    } else {
        inline473 = 10
        var inline475 int32 = inline473 + 1
        var inline476 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: inline475,
        }
        t443 = inline476
        var t444 string
        switch t443._tag {
        case 0:
            var inline462 int32 = t443._v0_0
            var inline464 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline462)
            var inline465 string = "ok=" + inline464
            t444 = inline465
        case 1:
            var inline466 string = t443._v1_0
            var inline468 string = "err=" + inline466
            t444 = inline468
        default:
            panic("non-exhaustive match")
        }
        var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t444)
        _goml_runtime_core_string_println(inline459)
        return struct{}{}
    }
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t447 string = _goml_runtime_core_int32_to_string(self__33)
    return t447
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
