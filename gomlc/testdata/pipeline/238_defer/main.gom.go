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

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func early_return() int {
    var defer_return408 int = 7
    var inline539 string = "return:inner"
    var inline540 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline539)
    _goml_runtime_core_string_println(inline540)
    var inline535 string = "return:outer"
    var inline536 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline535)
    _goml_runtime_core_string_println(inline536)
    return defer_return408
}

func maybe(value__0 Option__int) Option__int {
    var jp463 int
    switch value__0.(type) {
    case None:
        var defer_return417 Option__int = None{}
        var inline543 string = "try:cleanup"
        var inline544 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline543)
        _goml_runtime_core_string_println(inline544)
        return defer_return417
    case Some:
        var x416 int = value__0.(Some)._0
        jp463 = x416
        var defer_result419 Option__int = Some{
            _0: jp463,
        }
        var inline547 string = "try:cleanup"
        var inline548 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline547)
        _goml_runtime_core_string_println(inline548)
        return defer_result419
    default:
        panic("non-exhaustive match")
    }
}

func loop_cleanup() struct{} {
    var index__2 *ref_int_x
    var inline572 int = 0
    var inline573 *ref_int_x = ref__Ref_3int(inline572)
    index__2 = inline573
    Loop_loop466:
    for {
        var t467 int
        var inline570 int = ref_get__Ref_3int(index__2)
        t467 = inline570
        var t468 bool = t467 < 3
        if t468 {
            var current__3 int
            var inline568 int = ref_get__Ref_3int(index__2)
            current__3 = inline568
            var t469 int = current__3 + 1
            ref_set__Ref_3int(index__2, t469)
            var t473 bool = current__3 == 0
            if t473 {
                var t474 string
                var inline554 string = _goml_runtime_core_int_to_string(current__3)
                t474 = inline554
                var t475 string = "loop:" + t474
                var inline551 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t475)
                _goml_runtime_core_string_println(inline551)
                continue
            } else {
                var t477 bool = current__3 == 1
                if t477 {
                    var t478 string
                    var inline559 string = _goml_runtime_core_int_to_string(current__3)
                    t478 = inline559
                    var t479 string = "loop:" + t478
                    var inline556 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t479)
                    _goml_runtime_core_string_println(inline556)
                    break Loop_loop466
                } else {
                    var t471 string
                    var inline564 string = _goml_runtime_core_int_to_string(current__3)
                    t471 = inline564
                    var t472 string = "loop:" + t471
                    var inline561 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t472)
                    _goml_runtime_core_string_println(inline561)
                    continue
                }
            }
        } else {
            break Loop_loop466
        }
    }
    return struct{}{}
}

func pattern_cleanup(value__5 Option__int) int {
    switch value__5.(type) {
    case Some:
        var x432 int = value__5.(Some)._0
        var x435 int = 2
        var defer_tast_result431 int = x432 + x435
        var inline586 string = "pattern:cleanup"
        var inline587 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline586)
        _goml_runtime_core_string_println(inline587)
        return defer_tast_result431
    default:
        var defer_return437 int = 0
        var inline590 string = "pattern:cleanup"
        var inline591 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline590)
        _goml_runtime_core_string_println(inline591)
        return defer_return437
    }
}

func main0() struct{} {
    println__T_string("body")
    println__T_string("block")
    var t490 int = early_return()
    var t491 string
    var inline648 string = _goml_runtime_core_int_to_string(t490)
    t491 = inline648
    var inline645 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t491)
    _goml_runtime_core_string_println(inline645)
    maybe(None{})
    loop_cleanup()
    var inline639 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("before")
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(inline639, "after")
    var inline641 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(inline639)
    var inline642 string = "observed:" + inline641
    println__T_string(inline642)
    var t492 Option__int = Some{
        _0: 3,
    }
    var t493 int = pattern_cleanup(t492)
    var t494 string
    var inline637 string = _goml_runtime_core_int_to_string(t493)
    t494 = inline637
    var inline634 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t494)
    _goml_runtime_core_string_println(inline634)
    var t495 int
    var inline631 int = 0
    println__T_string("pattern:cleanup")
    t495 = inline631
    var t496 string
    var inline619 string = _goml_runtime_core_int_to_string(t495)
    t496 = inline619
    var inline616 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t496)
    _goml_runtime_core_string_println(inline616)
    var inline610 closure_env_run_0 = closure_env_run_0{}
    var inline611 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(inline610)
    }
    inline611()
    println__T_string("closure:after")
    println__T_string("closure:outer")
    var inline606 string = "main:second"
    var inline607 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline606)
    _goml_runtime_core_string_println(inline607)
    var inline602 string = "main:first"
    var inline603 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline602)
    _goml_runtime_core_string_println(inline603)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t498 string
    t498 = value__1
    _goml_runtime_core_string_println(t498)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__431 string) *ref_string_x {
    var t513 *ref_string_x = ref__Ref_6string(value__431)
    return t513
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__433 *ref_string_x, value__434 string) struct{} {
    ref_set__Ref_6string(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__432 *ref_string_x) string {
    var t518 string = ref_get__Ref_6string(self__432)
    return t518
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__run__0_i_closure__env__run__0_i_apply(env457 closure_env_run_0) struct{} {
    var inline655 string = "closure:body"
    var inline656 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline655)
    _goml_runtime_core_string_println(inline656)
    var inline651 string = "closure:inner"
    var inline652 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline651)
    _goml_runtime_core_string_println(inline652)
    return struct{}{}
}

func main() {
    main0()
}
