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
    var mtmp415 Option__int
    var inline523 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline524 int = inline523._0
    var inline525 bool = inline523._1
    if inline525 {
        var inline528 Option__int = Option__int{
            _tag: 1,
            _v1_0: inline524,
        }
        mtmp415 = inline528
    } else {
        mtmp415 = Option__int{
            _tag: 0,
        }
    }
    var jp434 int
    switch mtmp415._tag {
    case 0:
        jp434 = -1
    case 1:
        var x416 int = mtmp415._v1_0
        jp434 = x416
    default:
        panic("non-exhaustive match")
    }
    var inline520 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp434)
    _goml_runtime_core_string_println(inline520)
    var mtmp418 Option__int
    var inline513 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline514 int = inline513._0
    var inline515 bool = inline513._1
    if inline515 {
        var inline518 Option__int = Option__int{
            _tag: 1,
            _v1_0: inline514,
        }
        mtmp418 = inline518
    } else {
        mtmp418 = Option__int{
            _tag: 0,
        }
    }
    var jp436 int
    switch mtmp418._tag {
    case 0:
        jp436 = -1
    case 1:
        var x419 int = mtmp418._v1_0
        jp436 = x419
    default:
        panic("non-exhaustive match")
    }
    var inline510 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp436)
    _goml_runtime_core_string_println(inline510)
    var mtmp421 Option__int
    var inline503 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline504 int = inline503._0
    var inline505 bool = inline503._1
    if inline505 {
        var inline508 Option__int = Option__int{
            _tag: 1,
            _v1_0: inline504,
        }
        mtmp421 = inline508
    } else {
        mtmp421 = Option__int{
            _tag: 0,
        }
    }
    var jp438 string
    switch mtmp421._tag {
    case 0:
        jp438 = "closed"
    case 1:
        jp438 = "open"
    default:
        panic("non-exhaustive match")
    }
    var inline500 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp438)
    _goml_runtime_core_string_println(inline500)
    var unbuffered__5 chan string
    var inline498 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    unbuffered__5 = inline498
    var mtmp424 Option__string
    var inline492 string = "ready"
    var inline493 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: unbuffered__5,
        value_1: inline492,
    }
    var inline494 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(inline493)
    }
    go inline494()
    var inline496 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(unbuffered__5)
    mtmp424 = inline496
    var jp440 string
    switch mtmp424._tag {
    case 0:
        jp440 = "closed"
    case 1:
        var x425 string = mtmp424._v1_0
        jp440 = x425
    default:
        panic("non-exhaustive match")
    }
    var inline489 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp440)
    _goml_runtime_core_string_println(inline489)
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__435 int) chan string {
    var t443 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__435)
    return t443
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__435 int) chan int {
    var t446 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__435)
    return t446
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
    var t471 string = _goml_runtime_core_int_to_string(self__151)
    return t471
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

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env428 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env428.channel_0
    var value__1 string = env428.value_1
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
