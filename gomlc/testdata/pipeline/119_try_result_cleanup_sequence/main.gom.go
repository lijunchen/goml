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

type Result__Handle__string interface {
    isResult__Handle__string()
}

type Result__Handle__string_Ok struct {
    _0 Handle
}

func (_ Result__Handle__string_Ok) isResult__Handle__string() {}

type Result__Handle__string_Err struct {
    _0 string
}

func (_ Result__Handle__string_Err) isResult__Handle__string() {}

type Result__unit__string struct {
    _tag int32
    _v0_0 struct{}
    _v1_0 string
}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

func use_handle(open_ok__3 bool, close_ok__4 bool) Result__string__string {
    var mtmp408 Result__Handle__string
    if open_ok__3 {
        var inline468 Handle = Handle{
            name: "config",
        }
        var inline469 Result__Handle__string = Result__Handle__string_Ok{
            _0: inline468,
        }
        mtmp408 = inline469
    } else {
        var inline470 Result__Handle__string = Result__Handle__string_Err{
            _0: "open failed",
        }
        mtmp408 = inline470
    }
    var jp438 Handle
    switch mtmp408.(type) {
    case Result__Handle__string_Ok:
        var x409 Handle = mtmp408.(Result__Handle__string_Ok)._0
        jp438 = x409
        var name__6 string = jp438.name
        var mtmp411 Result__unit__string
        if close_ok__4 {
            var inline463 Result__unit__string = Result__unit__string{
                _tag: 0,
                _v0_0: struct{}{},
            }
            mtmp411 = inline463
        } else {
            var inline464 string = jp438.name
            var inline465 string = "close failed for " + inline464
            var inline466 Result__unit__string = Result__unit__string{
                _tag: 1,
                _v1_0: inline465,
            }
            mtmp411 = inline466
        }
        switch mtmp411._tag {
        case 0:
            var t440 string = "closed " + name__6
            var t441 Result__string__string = Result__string__string_Ok{
                _0: t440,
            }
            return t441
        case 1:
            var x413 string = mtmp411._v1_0
            var t442 Result__string__string = Result__string__string_Err{
                _0: x413,
            }
            return t442
        default:
            panic("non-exhaustive match")
        }
    case Result__Handle__string_Err:
        var x410 string = mtmp408.(Result__Handle__string_Err)._0
        var t443 Result__string__string = Result__string__string_Err{
            _0: x410,
        }
        return t443
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t451 Result__string__string = use_handle(true, true)
    var t452 string
    switch t451.(type) {
    case Result__string__string_Ok:
        var inline495 string = t451.(Result__string__string_Ok)._0
        var inline497 string = "ok " + inline495
        t452 = inline497
    case Result__string__string_Err:
        var inline498 string = t451.(Result__string__string_Err)._0
        var inline500 string = "err " + inline498
        t452 = inline500
    default:
        panic("non-exhaustive match")
    }
    var inline492 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t452)
    _goml_runtime_core_string_println(inline492)
    var t453 Result__string__string = use_handle(false, true)
    var t454 string
    switch t453.(type) {
    case Result__string__string_Ok:
        var inline485 string = t453.(Result__string__string_Ok)._0
        var inline487 string = "ok " + inline485
        t454 = inline487
    case Result__string__string_Err:
        var inline488 string = t453.(Result__string__string_Err)._0
        var inline490 string = "err " + inline488
        t454 = inline490
    default:
        panic("non-exhaustive match")
    }
    var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t454)
    _goml_runtime_core_string_println(inline482)
    var t455 Result__string__string = use_handle(true, false)
    var t456 string
    switch t455.(type) {
    case Result__string__string_Ok:
        var inline475 string = t455.(Result__string__string_Ok)._0
        var inline477 string = "ok " + inline475
        t456 = inline477
    case Result__string__string_Err:
        var inline478 string = t455.(Result__string__string_Err)._0
        var inline480 string = "err " + inline478
        t456 = inline480
    default:
        panic("non-exhaustive match")
    }
    var inline472 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t456)
    _goml_runtime_core_string_println(inline472)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
