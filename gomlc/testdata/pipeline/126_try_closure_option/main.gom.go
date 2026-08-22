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

type Option__i32 struct {
    _tag int32
    _v1_0 int32
}

func main0() struct{} {
    var t433 Option__i32
    var inline476 int32 = 3
    var inline477 bool = true
    var inline478 closure_env_run_0 = closure_env_run_0{
        flag_0: inline477,
        base_1: inline476,
    }
    var inline479 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline478)
    }
    var inline480 Option__i32 = inline479()
    t433 = inline480
    var t434 string
    switch t433._tag {
    case 0:
        t434 = "none"
    case 1:
        var inline471 int32 = t433._v1_0
        var inline473 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline471)
        var inline474 string = "some=" + inline473
        t434 = inline474
    default:
        panic("non-exhaustive match")
    }
    var inline468 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline468)
    var t435 Option__i32
    var inline462 int32 = 3
    var inline463 bool = false
    var inline464 closure_env_run_0 = closure_env_run_0{
        flag_0: inline463,
        base_1: inline462,
    }
    var inline465 func() Option__i32 = func() Option__i32 {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline464)
    }
    var inline466 Option__i32 = inline465()
    t435 = inline466
    var t436 string
    switch t435._tag {
    case 0:
        t436 = "none"
    case 1:
        var inline457 int32 = t435._v1_0
        var inline459 string = _goml_m_inherent_i_i32_i_i32_i_to__string(inline457)
        var inline460 string = "some=" + inline459
        t436 = inline460
    default:
        panic("non-exhaustive match")
    }
    var inline454 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
    _goml_runtime_core_string_println(inline454)
    return struct{}{}
}

func _goml_m_inherent_i_i32_i_i32_i_to__string(self__33 int32) string {
    var t439 string = _goml_runtime_core_int32_to_string(self__33)
    return t439
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env416 closure_env_run_0) Option__i32 {
    var flag__2 bool = env416.flag_0
    var base__1 int32 = env416.base_1
    var mtmp411 Option__i32
    if flag__2 {
        var inline483 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: 4,
        }
        mtmp411 = inline483
    } else {
        mtmp411 = Option__i32{
            _tag: 0,
        }
    }
    var jp448 int32
    switch mtmp411._tag {
    case 0:
        return Option__i32{
            _tag: 0,
        }
    case 1:
        var x412 int32 = mtmp411._v1_0
        jp448 = x412
        var t449 int32 = jp448 + base__1
        var t450 Option__i32 = Option__i32{
            _tag: 1,
            _v1_0: t449,
        }
        return t450
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
