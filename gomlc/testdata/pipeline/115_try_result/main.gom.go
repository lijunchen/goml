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
    var t440 Result__int32__string
    var inline489 bool = true
    var inline490 closure_env_run_0 = closure_env_run_0{
        flag_0: inline489,
    }
    var inline491 func() Result__int32__string = func() Result__int32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline490)
    }
    var inline492 Result__int32__string = inline491()
    t440 = inline492
    var t441 string
    switch t440._tag {
    case 0:
        var inline481 int32 = t440._v0_0
        var inline483 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline481)
        var inline484 string = "ok=" + inline483
        t441 = inline484
    case 1:
        var inline485 string = t440._v1_0
        var inline487 string = "err=" + inline485
        t441 = inline487
    default:
        panic("non-exhaustive match")
    }
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline478)
    var t442 Result__int32__string
    var inline473 bool = false
    var inline474 closure_env_run_0 = closure_env_run_0{
        flag_0: inline473,
    }
    var inline475 func() Result__int32__string = func() Result__int32__string {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline474)
    }
    var inline476 Result__int32__string = inline475()
    t442 = inline476
    var t443 string
    switch t442._tag {
    case 0:
        var inline465 int32 = t442._v0_0
        var inline467 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline465)
        var inline468 string = "ok=" + inline467
        t443 = inline468
    case 1:
        var inline469 string = t442._v1_0
        var inline471 string = "err=" + inline469
        t443 = inline471
    default:
        panic("non-exhaustive match")
    }
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t443)
    _goml_runtime_core_string_println(inline462)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t446 string = _goml_runtime_core_int32_to_string(self__33)
    return t446
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env418 closure_env_run_0) Result__int32__string {
    var flag__3 bool = env418.flag_0
    var mtmp411 Result__int32__string
    if flag__3 {
        var inline498 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: 7,
        }
        mtmp411 = inline498
    } else {
        var inline499 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: "nope",
        }
        mtmp411 = inline499
    }
    var jp455 int32
    switch mtmp411._tag {
    case 0:
        var x412 int32 = mtmp411._v0_0
        jp455 = x412
        var t456 int32
        var inline495 int32 = 1
        var inline496 int32 = jp455 + inline495
        t456 = inline496
        var t457 Result__int32__string = Result__int32__string{
            _tag: 0,
            _v0_0: t456,
        }
        return t457
    case 1:
        var x413 string = mtmp411._v1_0
        var t458 Result__int32__string = Result__int32__string{
            _tag: 1,
            _v1_0: x413,
        }
        return t458
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
