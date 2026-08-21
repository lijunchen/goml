package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

func parse_text(ok__0 bool) Result__string__string {
    if ok__0 {
        var t425 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "goml",
        }
        return t425
    } else {
        var t426 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        return t426
    }
}

func normalize_text(ok__1 bool) Result__string__string {
    var mtmp411 Result__string__string
    if ok__1 {
        var inline459 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "goml",
        }
        mtmp411 = inline459
    } else {
        var inline460 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        mtmp411 = inline460
    }
    var jp430 string
    switch mtmp411._tag {
    case 0:
        var x412 string = mtmp411._v0_0
        jp430 = x412
        var t431 string = jp430 + "!"
        var t432 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t431,
        }
        return t432
    case 1:
        var x413 string = mtmp411._v1_0
        var t433 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x413,
        }
        return t433
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var mtmp414 Result__string__string
    var inline462 Result__string__string = parse_text(ok__3)
    var inline464 string
    switch inline462._tag {
    case 0:
        var inline468 string = inline462._v0_0
        inline464 = inline468
        var inline466 string = inline464 + "!"
        var inline467 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: inline466,
        }
        mtmp414 = inline467
        var jp437 string
        switch mtmp414._tag {
        case 0:
            var x415 string = mtmp414._v0_0
            jp437 = x415
            var t438 string = "[" + jp437
            var t439 string = t438 + "]"
            var t440 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t439,
            }
            return t440
        case 1:
            var x416 string = mtmp414._v1_0
            var t441 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x416,
            }
            return t441
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var inline470 string = inline462._v1_0
        var inline472 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline470,
        }
        mtmp414 = inline472
        var jp437 string
        switch mtmp414._tag {
        case 0:
            var x415 string = mtmp414._v0_0
            jp437 = x415
            var t438 string = "[" + jp437
            var t439 string = t438 + "]"
            var t440 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t439,
            }
            return t440
        case 1:
            var x416 string = mtmp414._v1_0
            var t441 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x416,
            }
            return t441
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t449 Result__string__string = decorate_text(true)
    var t450 string
    switch t449._tag {
    case 0:
        var inline501 string = t449._v0_0
        var inline503 string = "ok " + inline501
        t450 = inline503
    case 1:
        var inline504 string = t449._v1_0
        var inline506 string = "err " + inline504
        t450 = inline506
    default:
        panic("non-exhaustive match")
    }
    var inline498 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t450)
    _goml_runtime_core_string_println(inline498)
    var t451 Result__string__string
    var inline484 bool = false
    var inline485 Result__string__string = normalize_text(inline484)
    var inline487 string
    switch inline485._tag {
    case 0:
        var inline492 string = inline485._v0_0
        inline487 = inline492
        var inline489 string = "[" + inline487
        var inline490 string = inline489 + "]"
        var inline491 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: inline490,
        }
        t451 = inline491
        var t452 string
        switch t451._tag {
        case 0:
            var inline477 string = t451._v0_0
            var inline479 string = "ok " + inline477
            t452 = inline479
        case 1:
            var inline480 string = t451._v1_0
            var inline482 string = "err " + inline480
            t452 = inline482
        default:
            panic("non-exhaustive match")
        }
        var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t452)
        _goml_runtime_core_string_println(inline474)
        return struct{}{}
    case 1:
        var inline494 string = inline485._v1_0
        var inline496 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline494,
        }
        t451 = inline496
        var t452 string
        switch t451._tag {
        case 0:
            var inline477 string = t451._v0_0
            var inline479 string = "ok " + inline477
            t452 = inline479
        case 1:
            var inline480 string = t451._v1_0
            var inline482 string = "err " + inline480
            t452 = inline482
        default:
            panic("non-exhaustive match")
        }
        var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t452)
        _goml_runtime_core_string_println(inline474)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
