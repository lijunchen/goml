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
        var t423 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ignored",
        }
        return t423
    } else {
        var t424 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        return t424
    }
}

func check(ok__1 bool) Result__string__string {
    var mtmp411 Result__string__string
    if ok__1 {
        var inline448 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ignored",
        }
        mtmp411 = inline448
    } else {
        var inline449 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        mtmp411 = inline449
    }
    switch mtmp411._tag {
    case 0:
        var t429 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ok",
        }
        return t429
    case 1:
        var x413 string = mtmp411._v1_0
        var t430 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x413,
        }
        return t430
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t438 Result__string__string = check(true)
    var t439 string
    switch t438._tag {
    case 0:
        var inline476 string = t438._v0_0
        var inline478 string = "ok " + inline476
        t439 = inline478
    case 1:
        var inline479 string = t438._v1_0
        var inline481 string = "err " + inline479
        t439 = inline481
    default:
        panic("non-exhaustive match")
    }
    var inline473 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
    _goml_runtime_core_string_println(inline473)
    var t440 Result__string__string
    var inline461 bool = false
    var inline462 Result__string__string = parse_text(inline461)
    switch inline462._tag {
    case 0:
        var inline466 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ok",
        }
        t440 = inline466
        var t441 string
        switch t440._tag {
        case 0:
            var inline454 string = t440._v0_0
            var inline456 string = "ok " + inline454
            t441 = inline456
        case 1:
            var inline457 string = t440._v1_0
            var inline459 string = "err " + inline457
            t441 = inline459
        default:
            panic("non-exhaustive match")
        }
        var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
        _goml_runtime_core_string_println(inline451)
        return struct{}{}
    case 1:
        var inline469 string = inline462._v1_0
        var inline471 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline469,
        }
        t440 = inline471
        var t441 string
        switch t440._tag {
        case 0:
            var inline454 string = t440._v0_0
            var inline456 string = "ok " + inline454
            t441 = inline456
        case 1:
            var inline457 string = t440._v1_0
            var inline459 string = "err " + inline457
            t441 = inline459
        default:
            panic("non-exhaustive match")
        }
        var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
        _goml_runtime_core_string_println(inline451)
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
