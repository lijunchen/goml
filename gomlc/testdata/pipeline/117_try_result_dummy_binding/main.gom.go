package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type Result__unit__string struct {
    _tag int32
    _v0_0 struct{}
    _v1_0 string
}

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

func configure_and_format(config_ok__3 bool, read_ok__4 bool) Result__string__string {
    var mtmp408 Result__unit__string
    if config_ok__3 {
        var inline468 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        mtmp408 = inline468
    } else {
        var inline469 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: "config failed",
        }
        mtmp408 = inline469
    }
    switch mtmp408._tag {
    case 0:
        var mtmp412 Result__string__string
        if read_ok__4 {
            var inline465 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: "2s",
            }
            mtmp412 = inline465
        } else {
            var inline466 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: "duration failed",
            }
            mtmp412 = inline466
        }
        var jp439 string
        switch mtmp412._tag {
        case 0:
            var x413 string = mtmp412._v0_0
            jp439 = x413
            var t440 string
            var inline463 string = "duration=" + jp439
            t440 = inline463
            var t441 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t440,
            }
            return t441
        case 1:
            var x414 string = mtmp412._v1_0
            var t442 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x414,
            }
            return t442
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var x410 string = mtmp408._v1_0
        var t443 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x410,
        }
        return t443
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t451 Result__string__string = configure_and_format(true, true)
    var t452 string
    switch t451._tag {
    case 0:
        var inline494 string = t451._v0_0
        var inline496 string = "ok " + inline494
        t452 = inline496
    case 1:
        var inline497 string = t451._v1_0
        var inline499 string = "err " + inline497
        t452 = inline499
    default:
        panic("non-exhaustive match")
    }
    var inline491 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t452)
    _goml_runtime_core_string_println(inline491)
    var t453 Result__string__string = configure_and_format(true, false)
    var t454 string
    switch t453._tag {
    case 0:
        var inline484 string = t453._v0_0
        var inline486 string = "ok " + inline484
        t454 = inline486
    case 1:
        var inline487 string = t453._v1_0
        var inline489 string = "err " + inline487
        t454 = inline489
    default:
        panic("non-exhaustive match")
    }
    var inline481 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t454)
    _goml_runtime_core_string_println(inline481)
    var t455 Result__string__string = configure_and_format(false, true)
    var t456 string
    switch t455._tag {
    case 0:
        var inline474 string = t455._v0_0
        var inline476 string = "ok " + inline474
        t456 = inline476
    case 1:
        var inline477 string = t455._v1_0
        var inline479 string = "err " + inline477
        t456 = inline479
    default:
        panic("non-exhaustive match")
    }
    var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t456)
    _goml_runtime_core_string_println(inline471)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
