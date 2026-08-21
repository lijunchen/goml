package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type closure_env_run_0 struct {}

type Ordering int32

type Option__int struct {
    _tag int32
    _v1_0 int
}

func early_return() int {
    var defer_return411 int = 7
    var inline542 string = "return:inner"
    var inline543 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline542)
    _goml_runtime_core_string_println(inline543)
    var inline538 string = "return:outer"
    var inline539 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline538)
    _goml_runtime_core_string_println(inline539)
    return defer_return411
}

func maybe(value__0 Option__int) Option__int {
    var jp466 int
    switch value__0._tag {
    case 0:
        var defer_return420 Option__int = Option__int{
            _tag: 0,
        }
        var inline546 string = "try:cleanup"
        var inline547 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline546)
        _goml_runtime_core_string_println(inline547)
        return defer_return420
    case 1:
        var x419 int = value__0._v1_0
        jp466 = x419
        var defer_result422 Option__int = Option__int{
            _tag: 1,
            _v1_0: jp466,
        }
        var inline550 string = "try:cleanup"
        var inline551 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline550)
        _goml_runtime_core_string_println(inline551)
        return defer_result422
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x
    var inline575 int = 0
    var inline576 *ref_int_x = ref__Ref_3int(inline575)
    index__2 = inline576
    Loop_loop469:
    for {
        var t470 int
        var inline573 int = ref_get__Ref_3int(index__2)
        t470 = inline573
        var t471 bool = t470 < 3
        if t471 {
            var current__3 int
            var inline571 int = ref_get__Ref_3int(index__2)
            current__3 = inline571
            var t472 int = current__3 + 1
            ref_set__Ref_3int(index__2, t472)
            var t476 bool = current__3 == 0
            if t476 {
                var t477 string
                var inline557 string = _goml_runtime_core_int_to_string(current__3)
                t477 = inline557
                var t478 string = "loop:" + t477
                var inline554 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t478)
                _goml_runtime_core_string_println(inline554)
                continue
            } else {
                var t480 bool = current__3 == 1
                if t480 {
                    var t481 string
                    var inline562 string = _goml_runtime_core_int_to_string(current__3)
                    t481 = inline562
                    var t482 string = "loop:" + t481
                    var inline559 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t482)
                    _goml_runtime_core_string_println(inline559)
                    break Loop_loop469
                } else {
                    var t474 string
                    var inline567 string = _goml_runtime_core_int_to_string(current__3)
                    t474 = inline567
                    var t475 string = "loop:" + t474
                    var inline564 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t475)
                    _goml_runtime_core_string_println(inline564)
                    continue
                }
            }
        } else {
            break Loop_loop469
        }
    }
    return struct{}{}
}

func pattern_cleanup(value__5 Option__int) int {
    switch value__5._tag {
    case 1:
        var x435 int = value__5._v1_0
        var x438 int = 2
        var defer_tast_result434 int = x435 + x438
        var inline589 string = "pattern:cleanup"
        var inline590 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline589)
        _goml_runtime_core_string_println(inline590)
        return defer_tast_result434
    default:
        var defer_return440 int = 0
        var inline593 string = "pattern:cleanup"
        var inline594 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline593)
        _goml_runtime_core_string_println(inline594)
        return defer_return440
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t493 int = early_return()
    var t494 string
    var inline651 string = _goml_runtime_core_int_to_string(t493)
    t494 = inline651
    var inline648 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t494)
    _goml_runtime_core_string_println(inline648)
    maybe(Option__int{
        _tag: 0,
    })
    loop_cleanup()
    var inline642 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline642, "after")
    var inline644 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline642)
    var inline645 string = "observed:" + inline644
    println__T_string(inline645)
    var t495 Option__int = Option__int{
        _tag: 1,
        _v1_0: 3,
    }
    var t496 int = pattern_cleanup(t495)
    var t497 string
    var inline640 string = _goml_runtime_core_int_to_string(t496)
    t497 = inline640
    var inline637 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t497)
    _goml_runtime_core_string_println(inline637)
    var t498 int
    var inline634 int = 0
    println__T_string("pattern:cleanup")
    t498 = inline634
    var t499 string
    var inline622 string = _goml_runtime_core_int_to_string(t498)
    t499 = inline622
    var inline619 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t499)
    _goml_runtime_core_string_println(inline619)
    var inline613 closure_env_run_0 = closure_env_run_0{}
    var inline614 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline613)
    }
    inline614()
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline609 string = "main:second"
    var inline610 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline609)
    _goml_runtime_core_string_println(inline610)
    var inline605 string = "main:first"
    var inline606 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline605)
    _goml_runtime_core_string_println(inline606)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t501 string
    t501 = value__1
    _goml_runtime_core_string_println(t501)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__431 string) *ref_string_x {
    var t516 *ref_string_x = ref__Ref_6string(value__431)
    return t516
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__433 *ref_string_x, value__434 string) struct{} {
    ref_set__Ref_6string(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__432 *ref_string_x) string {
    var t521 string = ref_get__Ref_6string(self__432)
    return t521
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env460 closure_env_run_0) struct{} {
    var inline658 string = "closure:body"
    var inline659 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline658)
    _goml_runtime_core_string_println(inline659)
    var inline654 string = "closure:inner"
    var inline655 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline654)
    _goml_runtime_core_string_println(inline655)
    return struct{}{}
}

func main() {
    main0()
}
