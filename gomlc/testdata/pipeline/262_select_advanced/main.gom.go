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

type Ordering int32

type Option__int struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var log__9 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var disabled__10 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(1)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(disabled__10)
    var t450 chan int
    var inline554 string = "c"
    var inline555 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__9)
    var inline556 string = inline555 + inline554
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__9, inline556)
    t450 = disabled__10
    var t451 int
    var inline548 string = "v"
    var inline549 int = 1
    var inline550 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__9)
    var inline551 string = inline550 + inline548
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__9, inline551)
    t451 = inline549
    var t452 bool
    var inline542 string = "g"
    var inline543 bool = false
    var inline544 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(log__9)
    var inline545 string = inline544 + inline542
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(log__9, inline545)
    t452 = inline543
    var jp436 string
    var select_channel_0_0 chan int
    if t452 {
        select_channel_0_0 = t450
    }
    select {
    case select_channel_0_0 <- t451:
        jp436 = "sent"
    default:
        jp436 = "default"
    }
    var t437 string
    var inline540 string = ref_get__Ref_6string(log__9)
    t437 = inline540
    var inline537 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t437)
    _goml_runtime_core_string_println(inline537)
    var inline534 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp436)
    _goml_runtime_core_string_println(inline534)
    var first__12 chan int
    var inline531 int = 1
    var inline532 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline531)
    first__12 = inline532
    var second__13 chan int
    var inline528 int = 1
    var inline529 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline528)
    second__13 = inline529
    var inline525 int = 10
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(first__12, inline525)
    var inline522 int = 20
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(second__13, inline522)
    var jp439 int
    var _goml_m_value____14_i_select__value int
    var _goml_m_value____14_i_select__open bool
    var value__14 Option__int = Option__int{
        _tag: 0,
    }
    var select_channel_1_0 chan int
    if true {
        select_channel_1_0 = first__12
    }
    var _goml_m_value____15_i_select__value int
    var _goml_m_value____15_i_select__open bool
    var value__15 Option__int = Option__int{
        _tag: 0,
    }
    var select_channel_1_1 chan int
    if true {
        select_channel_1_1 = second__13
    }
    select {
    case _goml_m_value____14_i_select__value, _goml_m_value____14_i_select__open = <-select_channel_1_0:
        if _goml_m_value____14_i_select__open {
            value__14 = Option__int{
                _tag: 1,
                _v1_0: _goml_m_value____14_i_select__value,
            }
        }
        var inline502 int = -1
        switch value__14._tag {
        case 0:
            jp439 = inline502
        case 1:
            var inline503 int = value__14._v1_0
            jp439 = inline503
        default:
            panic("non-exhaustive match")
        }
    default:
        select {
        case _goml_m_value____15_i_select__value, _goml_m_value____15_i_select__open = <-select_channel_1_1:
            if _goml_m_value____15_i_select__open {
                value__15 = Option__int{
                    _tag: 1,
                    _v1_0: _goml_m_value____15_i_select__value,
                }
            }
            var inline506 int = -1
            switch value__15._tag {
            case 0:
                jp439 = inline506
            case 1:
                var inline507 int = value__15._v1_0
                jp439 = inline507
            default:
                panic("non-exhaustive match")
            }
        default:
            jp439 = 0
        }
    }
    var inline519 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp439)
    _goml_runtime_core_string_println(inline519)
    var events__17 chan int
    var inline516 int = 1
    var inline517 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline516)
    events__17 = inline517
    var inline513 int = 7
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(events__17, inline513)
    var t443 int = 1 + 1
    var t444 bool = t443 == 2
    var jp441 int
    var _goml_m__d_select__recv__1170____18_i_select__value int
    var _goml_m__d_select__recv__1170____18_i_select__open bool
    var _goml_m__d_select__recv__1170____18 Option__int = Option__int{
        _tag: 0,
    }
    var select_channel_2_0 chan int
    if t444 {
        select_channel_2_0 = events__17
    }
    select {
    case _goml_m__d_select__recv__1170____18_i_select__value, _goml_m__d_select__recv__1170____18_i_select__open = <-select_channel_2_0:
        if _goml_m__d_select__recv__1170____18_i_select__open {
            _goml_m__d_select__recv__1170____18 = Option__int{
                _tag: 1,
                _v1_0: _goml_m__d_select__recv__1170____18_i_select__value,
            }
        }
        switch _goml_m__d_select__recv__1170____18._tag {
        case 0:
            jp441 = 0
        case 1:
            var x421 int = _goml_m__d_select__recv__1170____18._v1_0
            var t447 int = x421 + 1
            jp441 = t447
        default:
            panic("non-exhaustive match")
        }
    default:
        jp441 = -1
    }
    var inline510 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp441)
    _goml_runtime_core_string_println(inline510)
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

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__431 string) *ref_string_x {
    var t460 *ref_string_x = ref__Ref_6string(value__431)
    return t460
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__435 int) chan int {
    var t463 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__435)
    return t463
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(self__441 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__441)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t482 string = _goml_runtime_core_int_to_string(self__151)
    return t482
}

func main() {
    main0()
}
