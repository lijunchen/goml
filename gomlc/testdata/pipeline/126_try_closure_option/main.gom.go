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
    base_1 int32
}

type Ordering int32

type Option__int32 struct {
    _tag int32
    _v1_0 int32
}

func main0() struct{} {
    var t430 Option__int32
    var inline473 int32 = 3
    var inline474 bool = true
    var inline475 closure_env_run_0 = closure_env_run_0{
        flag_0: inline474,
        base_1: inline473,
    }
    var inline476 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline475)
    }
    var inline477 Option__int32 = inline476()
    t430 = inline477
    var t431 string
    switch t430._tag {
    case 0:
        t431 = "none"
    case 1:
        var inline468 int32 = t430._v1_0
        var inline470 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline468)
        var inline471 string = "some=" + inline470
        t431 = inline471
    default:
        panic("non-exhaustive match")
    }
    var inline465 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
    _goml_runtime_core_string_println(inline465)
    var t432 Option__int32
    var inline459 int32 = 3
    var inline460 bool = false
    var inline461 closure_env_run_0 = closure_env_run_0{
        flag_0: inline460,
        base_1: inline459,
    }
    var inline462 func() Option__int32 = func() Option__int32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline461)
    }
    var inline463 Option__int32 = inline462()
    t432 = inline463
    var t433 string
    switch t432._tag {
    case 0:
        t433 = "none"
    case 1:
        var inline454 int32 = t432._v1_0
        var inline456 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline454)
        var inline457 string = "some=" + inline456
        t433 = inline457
    default:
        panic("non-exhaustive match")
    }
    var inline451 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
    _goml_runtime_core_string_println(inline451)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t436 string = _goml_runtime_core_int32_to_string(self__33)
    return t436
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env413 closure_env_run_0) Option__int32 {
    var flag__2 bool = env413.flag_0
    var base__1 int32 = env413.base_1
    var mtmp408 Option__int32
    if flag__2 {
        var inline480 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: 4,
        }
        mtmp408 = inline480
    } else {
        mtmp408 = Option__int32{
            _tag: 0,
        }
    }
    var jp445 int32
    switch mtmp408._tag {
    case 0:
        return Option__int32{
            _tag: 0,
        }
    case 1:
        var x409 int32 = mtmp408._v1_0
        jp445 = x409
        var t446 int32 = jp445 + base__1
        var t447 Option__int32 = Option__int32{
            _tag: 1,
            _v1_0: t446,
        }
        return t447
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
