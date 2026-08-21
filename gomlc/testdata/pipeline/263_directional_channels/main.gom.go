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

type Tuple2_11Sender_3int_13Receiver_3int struct {
    _0 chan<- int
    _1 <-chan int
}

type Tuple2_3int_4bool struct {
    _0 int
    _1 bool
}

type Ordering int32

type Option__int struct {
    _tag int32
    _v1_0 int
}

func main0() struct{} {
    var channel__0 chan int
    var inline497 int = 2
    var inline498 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline497)
    channel__0 = inline498
    var mtmp411 Tuple2_11Sender_3int_13Receiver_3int
    var inline495 Tuple2_11Sender_3int_13Receiver_3int = func(p0 chan int) Tuple2_11Sender_3int_13Receiver_3int {
        return Tuple2_11Sender_3int_13Receiver_3int{
            _0: p0,
            _1: p0,
        }
    }(channel__0)
    mtmp411 = inline495
    var x412 chan<- int = mtmp411._0
    var x413 <-chan int = mtmp411._1
    var inline492 int = 7
    func(p0 chan<- int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(x412, inline492)
    var _goml_m_value____3_i_select__value int
    var _goml_m_value____3_i_select__open bool
    var value__3 Option__int = Option__int{
        _tag: 0,
    }
    select {
    case _goml_m_value____3_i_select__value, _goml_m_value____3_i_select__open = <-x413:
        if _goml_m_value____3_i_select__open {
            value__3 = Option__int{
                _tag: 1,
                _v1_0: _goml_m_value____3_i_select__value,
            }
        }
        var t423 int
        var inline459 int = 0
        switch value__3._tag {
        case 0:
            t423 = inline459
        case 1:
            var inline460 int = value__3._v1_0
            t423 = inline460
        default:
            panic("non-exhaustive match")
        }
        var inline456 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t423)
        _goml_runtime_core_string_println(inline456)
    default:
        var inline463 int = 1
        var inline464 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline463)
        _goml_runtime_core_string_println(inline464)
    }
    var channel__4 chan int
    var inline489 int = 1
    var inline490 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline489)
    channel__4 = inline490
    var sender__5 chan<- int
    var inline487 chan<- int = func(p0 chan int) chan<- int {
        return p0
    }(channel__4)
    sender__5 = inline487
    var receiver__6 <-chan int
    var inline485 <-chan int = func(p0 chan int) <-chan int {
        return p0
    }(channel__4)
    receiver__6 = inline485
    select {
    case sender__5 <- 9:
        var t419 Option__int
        var inline474 Tuple2_3int_4bool = func(p0 <-chan int) Tuple2_3int_4bool {
            var value int
            var ok bool
            value, ok = <-p0
            return Tuple2_3int_4bool{
                _0: value,
                _1: ok,
            }
        }(receiver__6)
        var inline475 int = inline474._0
        var inline476 bool = inline474._1
        if inline476 {
            var inline479 Option__int = Option__int{
                _tag: 1,
                _v1_0: inline475,
            }
            t419 = inline479
        } else {
            t419 = Option__int{
                _tag: 0,
            }
        }
        var t420 int
        var inline470 int = 0
        switch t419._tag {
        case 0:
            t420 = inline470
        case 1:
            var inline471 int = t419._v1_0
            t420 = inline471
        default:
            panic("non-exhaustive match")
        }
        var inline467 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t420)
        _goml_runtime_core_string_println(inline467)
        return struct{}{}
    default:
        var inline481 int = 2
        var inline482 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline481)
        _goml_runtime_core_string_println(inline482)
        return struct{}{}
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t454 string = _goml_runtime_core_int_to_string(self__151)
    return t454
}

func main() {
    main0()
}
