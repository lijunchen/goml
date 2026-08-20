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
        var t420 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ignored",
        }
        return t420
    } else {
        var t421 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        return t421
    }
}

func check(ok__1 bool) Result__string__string {
    var mtmp408 Result__string__string
    if ok__1 {
        var inline445 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ignored",
        }
        mtmp408 = inline445
    } else {
        var inline446 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        mtmp408 = inline446
    }
    switch mtmp408._tag {
    case 0:
        var t426 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ok",
        }
        return t426
    case 1:
        var x410 string = mtmp408._v1_0
        var t427 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x410,
        }
        return t427
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t435 Result__string__string = check(true)
    var t436 string
    switch t435._tag {
    case 0:
        var inline473 string = t435._v0_0
        var inline475 string = "ok " + inline473
        t436 = inline475
    case 1:
        var inline476 string = t435._v1_0
        var inline478 string = "err " + inline476
        t436 = inline478
    default:
        panic("non-exhaustive match")
    }
    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline470)
    var t437 Result__string__string
    var inline458 bool = false
    var inline459 Result__string__string = parse_text(inline458)
    switch inline459._tag {
    case 0:
        var inline463 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "ok",
        }
        t437 = inline463
        var t438 string
        switch t437._tag {
        case 0:
            var inline451 string = t437._v0_0
            var inline453 string = "ok " + inline451
            t438 = inline453
        case 1:
            var inline454 string = t437._v1_0
            var inline456 string = "err " + inline454
            t438 = inline456
        default:
            panic("non-exhaustive match")
        }
        var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
        _goml_runtime_core_string_println(inline448)
        return struct{}{}
    case 1:
        var inline466 string = inline459._v1_0
        var inline468 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline466,
        }
        t437 = inline468
        var t438 string
        switch t437._tag {
        case 0:
            var inline451 string = t437._v0_0
            var inline453 string = "ok " + inline451
            t438 = inline453
        case 1:
            var inline454 string = t437._v1_0
            var inline456 string = "err " + inline454
            t438 = inline456
        default:
            panic("non-exhaustive match")
        }
        var inline448 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
        _goml_runtime_core_string_println(inline448)
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
