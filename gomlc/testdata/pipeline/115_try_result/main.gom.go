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

type closure_env_run_0 struct {
    flag_0 bool
}

type Ordering int32

type Result__int32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

func main0() struct{} {
    var t437 Result__int32__string
    var inline486 bool = true
    var inline487 closure_env_run_0 = closure_env_run_0{
        flag_0: inline486,
    }
    var inline488 func() Result__int32__string = func() Result__int32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline487)
    }
    var inline489 Result__int32__string = inline488()
    t437 = inline489
    var t438 string
    switch t437._tag {
    case 0:
        var inline478 int32 = t437._v0_0
        var inline480 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline478)
        var inline481 string = "ok=" + inline480
        t438 = inline481
    case 1:
        var inline482 string = t437._v1_0
        var inline484 string = "err=" + inline482
        t438 = inline484
    default:
        panic("non-exhaustive match")
    }
    var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline475)
    var t439 Result__int32__string
    var inline470 bool = false
    var inline471 closure_env_run_0 = closure_env_run_0{
        flag_0: inline470,
    }
    var inline472 func() Result__int32__string = func() Result__int32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline471)
    }
    var inline473 Result__int32__string = inline472()
    t439 = inline473
    var t440 string
    switch t439._tag {
    case 0:
        var inline462 int32 = t439._v0_0
        var inline464 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline462)
        var inline465 string = "ok=" + inline464
        t440 = inline465
    case 1:
        var inline466 string = t439._v1_0
        var inline468 string = "err=" + inline466
        t440 = inline468
    default:
        panic("non-exhaustive match")
    }
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t440)
    _goml_runtime_core_string_println(inline459)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t443 string = _goml_runtime_core_int32_to_string(self__33)
    return t443
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env415 closure_env_run_0) Result__int32__string {
    var flag__3 bool = env415.flag_0
    var mtmp408 Result__int32__string
    if flag__3 {
        var inline495 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: 7,
        }
        mtmp408 = inline495
    } else {
        var inline496 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: "nope",
        }
        mtmp408 = inline496
    }
    var jp452 int32
    switch mtmp408._tag {
    case 0:
        var x409 int32 = mtmp408._v0_0
        jp452 = x409
        var t453 int32
        var inline492 int32 = 1
        var inline493 int32 = jp452 + inline492
        t453 = inline493
        var t454 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: t453,
        }
        return t454
    case 1:
        var x410 string = mtmp408._v1_0
        var t455 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: x410,
        }
        return t455
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
