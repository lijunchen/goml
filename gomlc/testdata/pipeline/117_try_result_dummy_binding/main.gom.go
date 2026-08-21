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
    var mtmp411 Result__unit__string
    if config_ok__3 {
        var inline471 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        mtmp411 = inline471
    } else {
        var inline472 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: "config failed",
        }
        mtmp411 = inline472
    }
    switch mtmp411._tag {
    case 0:
        var mtmp415 Result__string__string
        if read_ok__4 {
            var inline468 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: "2s",
            }
            mtmp415 = inline468
        } else {
            var inline469 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: "duration failed",
            }
            mtmp415 = inline469
        }
        var jp442 string
        switch mtmp415._tag {
        case 0:
            var x416 string = mtmp415._v0_0
            jp442 = x416
            var t443 string
            var inline466 string = "duration=" + jp442
            t443 = inline466
            var t444 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t443,
            }
            return t444
        case 1:
            var x417 string = mtmp415._v1_0
            var t445 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x417,
            }
            return t445
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var x413 string = mtmp411._v1_0
        var t446 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x413,
        }
        return t446
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t454 Result__string__string = configure_and_format(true, true)
    var t455 string
    switch t454._tag {
    case 0:
        var inline497 string = t454._v0_0
        var inline499 string = "ok " + inline497
        t455 = inline499
    case 1:
        var inline500 string = t454._v1_0
        var inline502 string = "err " + inline500
        t455 = inline502
    default:
        panic("non-exhaustive match")
    }
    var inline494 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t455)
    _goml_runtime_core_string_println(inline494)
    var t456 Result__string__string = configure_and_format(true, false)
    var t457 string
    switch t456._tag {
    case 0:
        var inline487 string = t456._v0_0
        var inline489 string = "ok " + inline487
        t457 = inline489
    case 1:
        var inline490 string = t456._v1_0
        var inline492 string = "err " + inline490
        t457 = inline492
    default:
        panic("non-exhaustive match")
    }
    var inline484 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t457)
    _goml_runtime_core_string_println(inline484)
    var t458 Result__string__string = configure_and_format(false, true)
    var t459 string
    switch t458._tag {
    case 0:
        var inline477 string = t458._v0_0
        var inline479 string = "ok " + inline477
        t459 = inline479
    case 1:
        var inline480 string = t458._v1_0
        var inline482 string = "err " + inline480
        t459 = inline482
    default:
        panic("non-exhaustive match")
    }
    var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t459)
    _goml_runtime_core_string_println(inline474)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
