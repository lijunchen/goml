package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Handle struct {
    name string
}

type Ordering int32

type Result__Handle__string struct {
    _tag int32
    _v0_0 Handle
    _v1_0 string
}

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

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var mtmp411 Result__Handle__string
    if open_ok__3 {
        var inline471 Handle = Handle{
            name: "config",
        }
        var inline472 Result__Handle__string = Result__Handle__string{
            _tag: 0,
            _v0_0: inline471,
        }
        mtmp411 = inline472
    } else {
        var inline473 Result__Handle__string = Result__Handle__string{
            _tag: 1,
            _v1_0: "open failed",
        }
        mtmp411 = inline473
    }
    var jp441 Handle
    switch mtmp411._tag {
    case 0:
        var x412 Handle = mtmp411._v0_0
        jp441 = x412
        var name__6 string = jp441.name
        var mtmp414 Result__unit__string
        if close_ok__4 {
            var inline466 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            mtmp414 = inline466
        } else {
            var inline467 string = jp441.name
            var inline468 string = "close failed for " + inline467
            var inline469 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline468,
            }
            mtmp414 = inline469
        }
        switch mtmp414._tag {
        case 0:
            var t443 string = "closed " + name__6
            var t444 Result__string__string = Result__string__string{
                _tag: 0,
                _v0_0: t443,
            }
            return t444
        case 1:
            var x416 string = mtmp414._v1_0
            var t445 Result__string__string = Result__string__string{
                _tag: 1,
                _v1_0: x416,
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
    var t454 Result__string__string = use_handle(true, true)
    var t455 string
    switch t454._tag {
    case 0:
        var inline498 string = t454._v0_0
        var inline500 string = "ok " + inline498
        t455 = inline500
    case 1:
        var inline501 string = t454._v1_0
        var inline503 string = "err " + inline501
        t455 = inline503
    default:
        panic("non-exhaustive match")
    }
    var inline495 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t455)
    _goml_runtime_core_string_println(inline495)
    var t456 Result__string__string = use_handle(false, true)
    var t457 string
    switch t456._tag {
    case 0:
        var inline488 string = t456._v0_0
        var inline490 string = "ok " + inline488
        t457 = inline490
    case 1:
        var inline491 string = t456._v1_0
        var inline493 string = "err " + inline491
        t457 = inline493
    default:
        panic("non-exhaustive match")
    }
    var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t457)
    _goml_runtime_core_string_println(inline485)
    var t458 Result__string__string = use_handle(true, false)
    var t459 string
    switch t458._tag {
    case 0:
        var inline478 string = t458._v0_0
        var inline480 string = "ok " + inline478
        t459 = inline480
    case 1:
        var inline481 string = t458._v1_0
        var inline483 string = "err " + inline481
        t459 = inline483
    default:
        panic("non-exhaustive match")
    }
    var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t459)
    _goml_runtime_core_string_println(inline475)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
