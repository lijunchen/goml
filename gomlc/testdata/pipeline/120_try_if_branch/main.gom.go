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

type Result__i32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

func parse(flag__0 bool) Result__i32__string {
    if flag__0 {
        var t423 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: 5,
        }
        return t423
    } else {
        var t424 Result__i32__string = Result__i32__string{
            _tag: 1,
            _v1_0: "bad-branch",
        }
        return t424
    }
}

func bump(flag__1 bool, fallback__2 bool) Result__i32__string {
    var jp428 int32
    if flag__1 {
        var commute_field501 int32
        var commute_field503 string
        if fallback__2 {
            commute_field501 = 5
            jp428 = commute_field501
            var t429 int32 = jp428 + 1
            var t430 Result__i32__string = Result__i32__string{
                _tag: 0,
                _v0_0: t429,
            }
            return t430
        } else {
            commute_field503 = "bad-branch"
            var t433 Result__i32__string = Result__i32__string{
                _tag: 1,
                _v1_0: commute_field503,
            }
            return t433
        }
    } else {
        jp428 = 10
        var t429 int32 = jp428 + 1
        var t430 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: t429,
        }
        return t430
    }
}

func show(res__4 Result__i32__string) string {
    switch res__4._tag {
    case 0:
        var x414 int32 = res__4._v0_0
        var t438 string
        var inline460 string = _goml_runtime_core_int32_to_string(x414)
        t438 = inline460
        var t439 string = "ok=" + t438
        return t439
    case 1:
        var x415 string = res__4._v1_0
        var t440 string = "err=" + x415
        return t440
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t442 Result__i32__string = bump(true, true)
    var t443 string = show(t442)
    var inline498 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline498)
    var t444 Result__i32__string = bump(true, false)
    var t445 string
    switch t444._tag {
    case 0:
        var inline490 int32 = t444._v0_0
        var inline492 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline490)
        var inline493 string = "ok=" + inline492
        t445 = inline493
    case 1:
        var inline494 string = t444._v1_0
        var inline496 string = "err=" + inline494
        t445 = inline496
    default:
        panic("non-exhaustive match")
    }
    var inline487 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t445)
    _goml_runtime_core_string_println(inline487)
    var t446 Result__i32__string
    var inline473 bool = false
    var inline474 bool = false
    var inline476 int32
    if inline473 {
        var inline480 Result__i32__string = parse(inline474)
        switch inline480._tag {
        case 0:
            var inline481 int32 = inline480._v0_0
            inline476 = inline481
            var inline478 int32 = inline476 + 1
            var inline479 Result__i32__string = Result__i32__string{
                _tag: 0,
                _v0_0: inline478,
            }
            t446 = inline479
            var t447 string
            switch t446._tag {
            case 0:
                var inline465 int32 = t446._v0_0
                var inline467 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline465)
                var inline468 string = "ok=" + inline467
                t447 = inline468
            case 1:
                var inline469 string = t446._v1_0
                var inline471 string = "err=" + inline469
                t447 = inline471
            default:
                panic("non-exhaustive match")
            }
            var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
            _goml_runtime_core_string_println(inline462)
            return struct{}{}
        case 1:
            var inline483 string = inline480._v1_0
            var inline485 Result__i32__string = Result__i32__string{
                _tag: 1,
                _v1_0: inline483,
            }
            t446 = inline485
            var t447 string
            switch t446._tag {
            case 0:
                var inline465 int32 = t446._v0_0
                var inline467 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline465)
                var inline468 string = "ok=" + inline467
                t447 = inline468
            case 1:
                var inline469 string = t446._v1_0
                var inline471 string = "err=" + inline469
                t447 = inline471
            default:
                panic("non-exhaustive match")
            }
            var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
            _goml_runtime_core_string_println(inline462)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    } else {
        inline476 = 10
        var inline478 int32 = inline476 + 1
        var inline479 Result__i32__string = Result__i32__string{
            _tag: 0,
            _v0_0: inline478,
        }
        t446 = inline479
        var t447 string
        switch t446._tag {
        case 0:
            var inline465 int32 = t446._v0_0
            var inline467 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline465)
            var inline468 string = "ok=" + inline467
            t447 = inline468
        case 1:
            var inline469 string = t446._v1_0
            var inline471 string = "err=" + inline469
            t447 = inline471
        default:
            panic("non-exhaustive match")
        }
        var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
        _goml_runtime_core_string_println(inline462)
        return struct{}{}
    }
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t450 string = _goml_runtime_core_int32_to_string(self__33)
    return t450
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
