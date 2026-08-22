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

type Tuple2_6string_4bool struct {
    _0 string
    _1 bool
}

type closure_env_read_0 struct {
    value_0 Option__isize
}

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func main0() struct{} {
    var received__6 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(1)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(received__6, 7)
    var jp433 int
    var _goml_m_value____7_i_select__value int
    var _goml_m_value____7_i_select__open bool
    var value__7 Option__isize = Option__isize{
        _tag: 0,
    }
    select {
    case _goml_m_value____7_i_select__value, _goml_m_value____7_i_select__open = <-received__6:
        if _goml_m_value____7_i_select__open {
            value__7 = Option__isize{
                _tag: 1,
                _v1_0: _goml_m_value____7_i_select__value,
            }
        }
        var t451 closure_env_read_0 = closure_env_read_0{
            value_0: value__7,
        }
        var read__8 func() int = func() int {
            return _goml_m_inherent_i_closure__env__read__0_i_closure__env__read__0_i_apply(t451)
        }
        var t452 int = read__8()
        jp433 = t452
    default:
        jp433 = 0
    }
    println__T_isize(jp433)
    var sent__10 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(1)
    var jp435 string
    select {
    case sent__10 <- "ready":
        jp435 = "sent"
    default:
        jp435 = "blocked"
    }
    var inline567 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp435)
    _goml_runtime_core_string_println(inline567)
    var t436 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(sent__10)
    var t437 string
    var inline563 string = "missing"
    switch t436._tag {
    case 0:
        t437 = inline563
    case 1:
        var inline564 string = t436._v1_0
        t437 = inline564
    default:
        panic("non-exhaustive match")
    }
    var inline560 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline560)
    var empty__12 chan int
    var inline557 int = 0
    var inline558 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline557)
    empty__12 = inline558
    var jp439 int
    var _goml_m_______13_i_select__open bool
    select {
    case _, _goml_m_______13_i_select__open = <-empty__12:
        if _goml_m_______13_i_select__open {}
        jp439 = -1
    default:
        jp439 = 42
    }
    var inline554 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(jp439)
    _goml_runtime_core_string_println(inline554)
    var log__15 *ref_string_x
    var inline551 string = ""
    var inline552 *ref_string_x = ref__Ref_6string(inline551)
    log__15 = inline552
    var left__16 chan int
    var inline548 int = 0
    var inline549 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline548)
    left__16 = inline549
    var right__17 chan int
    var inline545 int = 0
    var inline546 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline545)
    right__17 = inline546
    var t447 chan int
    var inline540 string = "a"
    var inline541 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline542 string = inline541 + inline540
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline542)
    t447 = left__16
    var t448 int
    var inline534 string = "1"
    var inline535 int = 1
    var inline536 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline537 string = inline536 + inline534
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline537)
    t448 = inline535
    var t449 chan int
    var inline529 string = "b"
    var inline530 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline531 string = inline530 + inline529
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline531)
    t449 = right__17
    var t450 int
    var inline523 string = "2"
    var inline524 int = 2
    var inline525 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline526 string = inline525 + inline523
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline526)
    t450 = inline524
    select {
    case t447 <- t448:
    case t449 <- t450:
    default:
    }
    var t441 string
    var inline521 string = ref_get__Ref_6string(log__15)
    t441 = inline521
    var inline518 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t441)
    _goml_runtime_core_string_println(inline518)
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(received__6)
    var jp443 string
    var _goml_m_value____18_i_select__value int
    var _goml_m_value____18_i_select__open bool
    var value__18 Option__isize = Option__isize{
        _tag: 0,
    }
    select {
    case _goml_m_value____18_i_select__value, _goml_m_value____18_i_select__open = <-received__6:
        if _goml_m_value____18_i_select__open {
            value__18 = Option__isize{
                _tag: 1,
                _v1_0: _goml_m_value____18_i_select__value,
            }
        }
        switch value__18._tag {
        case 0:
            jp443 = "closed"
        case 1:
            jp443 = "open"
        default:
            panic("non-exhaustive match")
        }
    }
    var inline513 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp443)
    _goml_runtime_core_string_println(inline513)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__432 *ref_string_x) string {
    var t455 string = ref_get__Ref_6string(self__432)
    return t455
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__433 *ref_string_x, value__434 string) struct{} {
    ref_set__Ref_6string(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__isize(capacity__435 int) chan int {
    var t460 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__435)
    return t460
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__isize(self__436 chan int, value__437 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__436, value__437)
    return struct{}{}
}

func println__T_isize(value__1 int) struct{} {
    var t468 string
    var inline570 string = _goml_runtime_core_int_to_string(value__1)
    t468 = inline570
    _goml_runtime_core_string_println(t468)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__435 int) chan string {
    var t472 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__435)
    return t472
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__438 chan string) Option__string {
    var mtmp379 Tuple2_6string_4bool = func(p0 chan string) Tuple2_6string_4bool {
        var value string
        var ok bool
        value, ok = <-p0
        return Tuple2_6string_4bool{
            _0: value,
            _1: ok,
        }
    }(self__438)
    var x380 string = mtmp379._0
    var x381 bool = mtmp379._1
    if x381 {
        var t480 Option__string = Option__string{
            _tag: 1,
            _v1_0: x380,
        }
        return t480
    } else {
        return Option__string{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t492 string = _goml_runtime_core_int_to_string(self__151)
    return t492
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__read__0_i_closure__env__read__0_i_apply(env422 closure_env_read_0) int {
    var value__7 Option__isize = env422.value_0
    var inline573 int = -1
    switch value__7._tag {
    case 0:
        return inline573
    case 1:
        var inline574 int = value__7._v1_0
        return inline574
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
