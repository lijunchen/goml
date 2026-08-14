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

func main0() struct{} {
    var t433 Result__string__string
    var inline477 string = "outer"
    var inline478 bool = true
    var inline479 closure_env_run_0 = closure_env_run_0{
        ok_0: inline478,
        prefix_1: inline477,
    }
    var inline480 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline479)
    }
    var inline481 Result__string__string = inline480()
    t433 = inline481
    var t434 string
    switch t433.(type) {
    case Ok:
        var inline470 string = t433.(Ok)._0
        var inline472 string = "ok " + inline470
        t434 = inline472
    case Err:
        var inline473 string = t433.(Err)._0
        var inline475 string = "err " + inline473
        t434 = inline475
    default:
        panic("non-exhaustive match")
    }
    var inline467 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline467)
    var t435 Result__string__string
    var inline461 string = "outer"
    var inline462 bool = false
    var inline463 closure_env_run_0 = closure_env_run_0{
        ok_0: inline462,
        prefix_1: inline461,
    }
    var inline464 func() Result__string__string = func() Result__string__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline463)
    }
    var inline465 Result__string__string = inline464()
    t435 = inline465
    var t436 string
    switch t435.(type) {
    case Ok:
        var inline454 string = t435.(Ok)._0
        var inline456 string = "ok " + inline454
        t436 = inline456
    case Err:
        var inline457 string = t435.(Err)._0
        var inline459 string = "err " + inline457
        t436 = inline459
    default:
        panic("non-exhaustive match")
    }
    var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline451)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env415 closure_env_run_0) Result__string__string {
    var ok__2 bool = env415.ok_0
    var prefix__1 string = env415.prefix_1
    var mtmp408 Result__string__string
    if ok__2 {
        var inline484 Result__string__string = Ok{
            _0: "body",
        }
        mtmp408 = inline484
    } else {
        var inline485 Result__string__string = Err{
            _0: "parse failed",
        }
        mtmp408 = inline485
    }
    var jp445 string
    switch mtmp408.(type) {
    case Ok:
        var x409 string = mtmp408.(Ok)._0
        jp445 = x409
        var t446 string = prefix__1 + ":"
        var t447 string = t446 + jp445
        var t448 Result__string__string = Ok{
            _0: t447,
        }
        return t448
    case Err:
        var x410 string = mtmp408.(Err)._0
        var t449 Result__string__string = Err{
            _0: x410,
        }
        return t449
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
