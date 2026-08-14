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

type Option__int32 interface {
    isOption__int32()
}

type Option__int32_None struct {}

func (_ Option__int32_None) isOption__int32() {}

type Option__int32_Some struct {
    _0 int32
}

func (_ Option__int32_Some) isOption__int32() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func mixed(primary__2 bool, secondary__3 bool) Option__string {
    var mtmp408 Option__int32
    if primary__2 {
        var inline466 Option__int32 = Option__int32_Some{
            _0: 4,
        }
        mtmp408 = inline466
    } else {
        mtmp408 = Option__int32_None{}
    }
    var jp429 int32
    switch mtmp408.(type) {
    case Option__int32_None:
        return Option__string_None{}
    case Option__int32_Some:
        var x409 int32 = mtmp408.(Option__int32_Some)._0
        jp429 = x409
        var mtmp410 Option__int32
        if secondary__3 {
            var inline464 Option__int32 = Option__int32_Some{
                _0: 9,
            }
            mtmp410 = inline464
        } else {
            mtmp410 = Option__int32_None{}
        }
        var jp431 string
        switch mtmp410.(type) {
        case Option__int32_None:
            jp431 = "extra=none"
        case Option__int32_Some:
            var x411 int32 = mtmp410.(Option__int32_Some)._0
            var t437 string
            var inline460 string = _goml_runtime_core_int32_to_string(x411)
            t437 = inline460
            var t438 string = "extra=" + t437
            jp431 = t438
        default:
            panic("non-exhaustive match")
        }
        var t432 string
        var inline462 string = _goml_runtime_core_int32_to_string(jp429)
        t432 = inline462
        var t433 string = "value=" + t432
        var t434 string = t433 + ","
        var t435 string = t434 + jp431
        var t436 Option__string = Option__string_Some{
            _0: t435,
        }
        return t436
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t445 Option__string = mixed(true, true)
    var t446 string
    switch t445.(type) {
    case Option__string_None:
        t446 = "none"
    case Option__string_Some:
        var inline485 string = t445.(Option__string_Some)._0
        var inline487 string = "some=" + inline485
        t446 = inline487
    default:
        panic("non-exhaustive match")
    }
    var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t446)
    _goml_runtime_core_string_println(inline482)
    var t447 Option__string = mixed(true, false)
    var t448 string
    switch t447.(type) {
    case Option__string_None:
        t448 = "none"
    case Option__string_Some:
        var inline478 string = t447.(Option__string_Some)._0
        var inline480 string = "some=" + inline478
        t448 = inline480
    default:
        panic("non-exhaustive match")
    }
    var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t448)
    _goml_runtime_core_string_println(inline475)
    var t449 Option__string = mixed(false, true)
    var t450 string
    switch t449.(type) {
    case Option__string_None:
        t450 = "none"
    case Option__string_Some:
        var inline471 string = t449.(Option__string_Some)._0
        var inline473 string = "some=" + inline471
        t450 = inline473
    default:
        panic("non-exhaustive match")
    }
    var inline468 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t450)
    _goml_runtime_core_string_println(inline468)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
