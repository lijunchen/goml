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

type Tuple2_3int_4bool struct {
    _0 int
    _1 bool
}

type Tuple2_6string_4bool struct {
    _0 string
    _1 bool
}

type closure_env_roundtrip_T_string_0 struct {
    channel_0 chan string
    value_1 string
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
    var buffered__2 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(2)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 10)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 20)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(buffered__2)
    var mtmp412 Option__int
    var inline520 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline521 int = inline520._0
    var inline522 bool = inline520._1
    if inline522 {
        var inline525 Option__int = Option__int{
            _tag: 1,
            _v1_0: inline521,
        }
        mtmp412 = inline525
    } else {
        mtmp412 = Option__int{
            _tag: 0,
        }
    }
    var jp431 int
    switch mtmp412._tag {
    case 0:
        jp431 = -1
    case 1:
        var x413 int = mtmp412._v1_0
        jp431 = x413
    default:
        panic("non-exhaustive match")
    }
    var inline517 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp431)
    _goml_runtime_core_string_println(inline517)
    var mtmp415 Option__int
    var inline510 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline511 int = inline510._0
    var inline512 bool = inline510._1
    if inline512 {
        var inline515 Option__int = Option__int{
            _tag: 1,
            _v1_0: inline511,
        }
        mtmp415 = inline515
    } else {
        mtmp415 = Option__int{
            _tag: 0,
        }
    }
    var jp433 int
    switch mtmp415._tag {
    case 0:
        jp433 = -1
    case 1:
        var x416 int = mtmp415._v1_0
        jp433 = x416
    default:
        panic("non-exhaustive match")
    }
    var inline507 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp433)
    _goml_runtime_core_string_println(inline507)
    var mtmp418 Option__int
    var inline500 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline501 int = inline500._0
    var inline502 bool = inline500._1
    if inline502 {
        var inline505 Option__int = Option__int{
            _tag: 1,
            _v1_0: inline501,
        }
        mtmp418 = inline505
    } else {
        mtmp418 = Option__int{
            _tag: 0,
        }
    }
    var jp435 string
    switch mtmp418._tag {
    case 0:
        jp435 = "closed"
    case 1:
        jp435 = "open"
    default:
        panic("non-exhaustive match")
    }
    var inline497 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp435)
    _goml_runtime_core_string_println(inline497)
    var unbuffered__5 chan string
    var inline495 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    unbuffered__5 = inline495
    var mtmp421 Option__string
    var inline489 string = "ready"
    var inline490 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: unbuffered__5,
        value_1: inline489,
    }
    var inline491 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(inline490)
    }
    go inline491()
    var inline493 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(unbuffered__5)
    mtmp421 = inline493
    var jp437 string
    switch mtmp421._tag {
    case 0:
        jp437 = "closed"
    case 1:
        var x422 string = mtmp421._v1_0
        jp437 = x422
    default:
        panic("non-exhaustive match")
    }
    var inline486 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp437)
    _goml_runtime_core_string_println(inline486)
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__435 int) chan string {
    var t440 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__435)
    return t440
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__435 int) chan int {
    var t443 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__435)
    return t443
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(self__436 chan int, value__437 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__436, value__437)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(self__441 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__441)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t468 string = _goml_runtime_core_int_to_string(self__151)
    return t468
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
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

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env425 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env425.channel_0
    var value__1 string = env425.value_1
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
