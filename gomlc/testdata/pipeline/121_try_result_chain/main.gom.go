package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type Result__string__string interface {
    isResult__string__string()
}

type Ok struct {
    _0 string
}

func (_ Ok) isResult__string__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__string__string() {}

func parse_text(ok__0 bool) Result__string__string {
    if ok__0 {
        var t422 Result__string__string = Ok{
            _0: "goml",
        }
        return t422
    } else {
        var t423 Result__string__string = Err{
            _0: "parse failed",
        }
        return t423
    }
}

func normalize_text(ok__1 bool) Result__string__string {
    var mtmp408 Result__string__string
    if ok__1 {
        var inline456 Result__string__string = Ok{
            _0: "goml",
        }
        mtmp408 = inline456
    } else {
        var inline457 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp408 = inline457
    }
    var jp427 string
    switch mtmp408.(type) {
    case Ok:
        var x409 string = mtmp408.(Ok)._0
        jp427 = x409
        var t428 string = jp427 + "!"
        var t429 Result__string__string = Ok{
            _0: t428,
        }
        return t429
    case Err:
        var x410 string = mtmp408.(Err)._0
        var t430 Result__string__string = Err{
            _0: x410,
        }
        return t430
    default:
        panic("non-exhaustive match")
    }
}

func decorate_text(ok__3 bool) Result__string__string {
    var mtmp411 Result__string__string
    var inline459 Result__string__string = parse_text(ok__3)
    var inline461 string
    switch inline459.(type) {
    case Ok:
        var inline465 string = inline459.(Ok)._0
        inline461 = inline465
        var inline463 string = inline461 + "!"
        var inline464 Result__string__string = Ok{
            _0: inline463,
        }
        mtmp411 = inline464
        var jp434 string
        switch mtmp411.(type) {
        case Ok:
            var x412 string = mtmp411.(Ok)._0
            jp434 = x412
            var t435 string = "[" + jp434
            var t436 string = t435 + "]"
            var t437 Result__string__string = Ok{
                _0: t436,
            }
            return t437
        case Err:
            var x413 string = mtmp411.(Err)._0
            var t438 Result__string__string = Err{
                _0: x413,
            }
            return t438
        default:
            panic("non-exhaustive match")
        }
    case Err:
        var inline467 string = inline459.(Err)._0
        var inline469 Result__string__string = Err{
            _0: inline467,
        }
        mtmp411 = inline469
        var jp434 string
        switch mtmp411.(type) {
        case Ok:
            var x412 string = mtmp411.(Ok)._0
            jp434 = x412
            var t435 string = "[" + jp434
            var t436 string = t435 + "]"
            var t437 Result__string__string = Ok{
                _0: t436,
            }
            return t437
        case Err:
            var x413 string = mtmp411.(Err)._0
            var t438 Result__string__string = Err{
                _0: x413,
            }
            return t438
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t446 Result__string__string = decorate_text(true)
    var t447 string
    switch t446.(type) {
    case Ok:
        var inline498 string = t446.(Ok)._0
        var inline500 string = "ok " + inline498
        t447 = inline500
    case Err:
        var inline501 string = t446.(Err)._0
        var inline503 string = "err " + inline501
        t447 = inline503
    default:
        panic("non-exhaustive match")
    }
    var inline495 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t447)
    _goml_runtime_core_string_println(inline495)
    var t448 Result__string__string
    var inline481 bool = false
    var inline482 Result__string__string = normalize_text(inline481)
    var inline484 string
    switch inline482.(type) {
    case Ok:
        var inline489 string = inline482.(Ok)._0
        inline484 = inline489
        var inline486 string = "[" + inline484
        var inline487 string = inline486 + "]"
        var inline488 Result__string__string = Ok{
            _0: inline487,
        }
        t448 = inline488
        var t449 string
        switch t448.(type) {
        case Ok:
            var inline474 string = t448.(Ok)._0
            var inline476 string = "ok " + inline474
            t449 = inline476
        case Err:
            var inline477 string = t448.(Err)._0
            var inline479 string = "err " + inline477
            t449 = inline479
        default:
            panic("non-exhaustive match")
        }
        var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t449)
        _goml_runtime_core_string_println(inline471)
        return struct{}{}
    case Err:
        var inline491 string = inline482.(Err)._0
        var inline493 Result__string__string = Err{
            _0: inline491,
        }
        t448 = inline493
        var t449 string
        switch t448.(type) {
        case Ok:
            var inline474 string = t448.(Ok)._0
            var inline476 string = "ok " + inline474
            t449 = inline476
        case Err:
            var inline477 string = t448.(Err)._0
            var inline479 string = "err " + inline477
            t449 = inline479
        default:
            panic("non-exhaustive match")
        }
        var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t449)
        _goml_runtime_core_string_println(inline471)
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
