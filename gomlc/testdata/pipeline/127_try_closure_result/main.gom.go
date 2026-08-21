package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_run_0 struct {
    ok_0 bool
    prefix_1 string
}

type Ordering int32

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

func main0() struct{} {
    var t436 Result__string__string
    var inline480 string = "outer"
    var inline481 bool = true
    var inline482 closure_env_run_0 = closure_env_run_0{
        ok_0: inline481,
        prefix_1: inline480,
    }
    var inline483 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline482)
    }
    var inline484 Result__string__string = inline483()
    t436 = inline484
    var t437 string
    switch t436._tag {
    case 0:
        var inline473 string = t436._v0_0
        var inline475 string = "ok " + inline473
        t437 = inline475
    case 1:
        var inline476 string = t436._v1_0
        var inline478 string = "err " + inline476
        t437 = inline478
    default:
        panic("non-exhaustive match")
    }
    var inline470 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline470)
    var t438 Result__string__string
    var inline464 string = "outer"
    var inline465 bool = false
    var inline466 closure_env_run_0 = closure_env_run_0{
        ok_0: inline465,
        prefix_1: inline464,
    }
    var inline467 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline466)
    }
    var inline468 Result__string__string = inline467()
    t438 = inline468
    var t439 string
    switch t438._tag {
    case 0:
        var inline457 string = t438._v0_0
        var inline459 string = "ok " + inline457
        t439 = inline459
    case 1:
        var inline460 string = t438._v1_0
        var inline462 string = "err " + inline460
        t439 = inline462
    default:
        panic("non-exhaustive match")
    }
    var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
    _goml_runtime_core_string_println(inline454)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env418 closure_env_run_0) Result__string__string {
    var ok__2 bool = env418.ok_0
    var prefix__1 string = env418.prefix_1
    var mtmp411 Result__string__string
    if ok__2 {
        var inline487 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: "body",
        }
        mtmp411 = inline487
    } else {
        var inline488 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "parse failed",
        }
        mtmp411 = inline488
    }
    var jp448 string
    switch mtmp411._tag {
    case 0:
        var x412 string = mtmp411._v0_0
        jp448 = x412
        var t449 string = prefix__1 + ":"
        var t450 string = t449 + jp448
        var t451 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: t450,
        }
        return t451
    case 1:
        var x413 string = mtmp411._v1_0
        var t452 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: x413,
        }
        return t452
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
