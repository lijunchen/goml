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
    value_0 Option__int
}

type Ordering int32

type Option__int struct {
    _tag int32
    _v1_0 int
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

func main0() struct{} {
    var received__6 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(1)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(received__6, 7)
    var jp430 int
    var _goml_m_value____7_i_select__value int
    var _goml_m_value____7_i_select__open bool
    var value__7 Option__int = Option__int{
        _tag: 0,
    }
    select {
    case _goml_m_value____7_i_select__value, _goml_m_value____7_i_select__open = <-received__6:
        if _goml_m_value____7_i_select__open {
            value__7 = Option__int{
                _tag: 1,
                _v1_0: _goml_m_value____7_i_select__value,
            }
        }
        var t448 closure_env_read_0 = closure_env_read_0{
            value_0: value__7,
        }
        var read__8 func() int = func() int {
            return _goml_m_inherent_i_closure__env__read__0_i_closure__env__read__0_i_apply(t448)
        }
        var t449 int = read__8()
        jp430 = t449
    default:
        jp430 = 0
    }
    println__T_int(jp430)
    var sent__10 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(1)
    var jp432 string
    select {
    case sent__10 <- "ready":
        jp432 = "sent"
    default:
        jp432 = "blocked"
    }
    var inline564 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp432)
    _goml_runtime_core_string_println(inline564)
    var t433 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(sent__10)
    var t434 string
    var inline560 string = "missing"
    switch t433._tag {
    case 0:
        t434 = inline560
    case 1:
        var inline561 string = t433._v1_0
        t434 = inline561
    default:
        panic("non-exhaustive match")
    }
    var inline557 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t434)
    _goml_runtime_core_string_println(inline557)
    var empty__12 chan int
    var inline554 int = 0
    var inline555 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline554)
    empty__12 = inline555
    var jp436 int
    var _goml_m_______13_i_select__open bool
    select {
    case _, _goml_m_______13_i_select__open = <-empty__12:
        if _goml_m_______13_i_select__open {}
        jp436 = -1
    default:
        jp436 = 42
    }
    var inline551 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp436)
    _goml_runtime_core_string_println(inline551)
    var log__15 *ref_string_x
    var inline548 string = ""
    var inline549 *ref_string_x = ref__Ref_6string(inline548)
    log__15 = inline549
    var left__16 chan int
    var inline545 int = 0
    var inline546 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline545)
    left__16 = inline546
    var right__17 chan int
    var inline542 int = 0
    var inline543 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline542)
    right__17 = inline543
    var t444 chan int
    var inline537 string = "a"
    var inline538 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline539 string = inline538 + inline537
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline539)
    t444 = left__16
    var t445 int
    var inline531 string = "1"
    var inline532 int = 1
    var inline533 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline534 string = inline533 + inline531
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline534)
    t445 = inline532
    var t446 chan int
    var inline526 string = "b"
    var inline527 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline528 string = inline527 + inline526
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline528)
    t446 = right__17
    var t447 int
    var inline520 string = "2"
    var inline521 int = 2
    var inline522 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__15)
    var inline523 string = inline522 + inline520
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__15, inline523)
    t447 = inline521
    select {
    case t444 <- t445:
    case t446 <- t447:
    default:
    }
    var t438 string
    var inline518 string = ref_get__Ref_6string(log__15)
    t438 = inline518
    var inline515 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t438)
    _goml_runtime_core_string_println(inline515)
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(received__6)
    var jp440 string
    var _goml_m_value____18_i_select__value int
    var _goml_m_value____18_i_select__open bool
    var value__18 Option__int = Option__int{
        _tag: 0,
    }
    select {
    case _goml_m_value____18_i_select__value, _goml_m_value____18_i_select__open = <-received__6:
        if _goml_m_value____18_i_select__open {
            value__18 = Option__int{
                _tag: 1,
                _v1_0: _goml_m_value____18_i_select__value,
            }
        }
        switch value__18._tag {
        case 0:
            jp440 = "closed"
        case 1:
            jp440 = "open"
        default:
            panic("non-exhaustive match")
        }
    }
    var inline510 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp440)
    _goml_runtime_core_string_println(inline510)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__432 *ref_string_x) string {
    var t452 string = ref_get__Ref_6string(self__432)
    return t452
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__433 *ref_string_x, value__434 string) struct{} {
    ref_set__Ref_6string(self__433, value__434)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__435 int) chan int {
    var t457 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__435)
    return t457
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(self__436 chan int, value__437 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__436, value__437)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t465 string
    var inline567 string = _goml_runtime_core_int_to_string(value__1)
    t465 = inline567
    _goml_runtime_core_string_println(t465)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__435 int) chan string {
    var t469 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__435)
    return t469
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
        var t477 Option__string = Option__string{
            _tag: 1,
            _v1_0: x380,
        }
        return t477
    } else {
        return Option__string{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t489 string = _goml_runtime_core_int_to_string(self__151)
    return t489
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__read__0_i_closure__env__read__0_i_apply(env419 closure_env_read_0) int {
    var value__7 Option__int = env419.value_0
    var inline570 int = -1
    switch value__7._tag {
    case 0:
        return inline570
    case 1:
        var inline571 int = value__7._v1_0
        return inline571
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
