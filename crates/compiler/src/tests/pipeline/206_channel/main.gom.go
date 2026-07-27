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

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

func make_string_channel() chan string {
    var retv83 chan string
    var t84 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    retv83 = t84
    return retv83
}

func main0() struct{} {
    var buffered__2 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(2)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 10)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 20)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(buffered__2)
    var mtmp68 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp87 int
    switch mtmp68.(type) {
    case Option__int_None:
        jp87 = -1
    case Option__int_Some:
        var x69 int = mtmp68.(Option__int_Some)._0
        var value__3 int = x69
        jp87 = value__3
    default:
        panic("non-exhaustive match")
    }
    println__T_int(jp87)
    var mtmp71 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp89 int
    switch mtmp71.(type) {
    case Option__int_None:
        jp89 = -1
    case Option__int_Some:
        var x72 int = mtmp71.(Option__int_Some)._0
        var value__4 int = x72
        jp89 = value__4
    default:
        panic("non-exhaustive match")
    }
    println__T_int(jp89)
    var mtmp74 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp91 string
    switch mtmp74.(type) {
    case Option__int_None:
        jp91 = "closed"
    case Option__int_Some:
        jp91 = "open"
    default:
        panic("non-exhaustive match")
    }
    println__T_string(jp91)
    var unbuffered__5 chan string = make_string_channel()
    var mtmp77 Option__string = roundtrip__T_string(unbuffered__5, "ready")
    var jp93 string
    switch mtmp77.(type) {
    case Option__string_None:
        jp93 = "closed"
    case Option__string_Some:
        var x78 string = mtmp77.(Option__string_Some)._0
        var value__6 string = x78
        jp93 = value__6
    default:
        panic("non-exhaustive match")
    }
    println__T_string(jp93)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__string(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__213 int) chan string {
    var retv95 chan string
    var t96 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__213)
    retv95 = t96
    return retv95
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__213 int) chan int {
    var retv98 chan int
    var t99 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__213)
    retv98 = t99
    return retv98
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(self__214 chan int, value__215 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__214, value__215)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(self__219 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__219)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t105 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t105)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(self__216 chan int) Option__int {
    var retv108 Option__int
    var mtmp60 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(self__216)
    var x61 int = mtmp60._0
    var x62 bool = mtmp60._1
    var ok__218 bool = x62
    var value__217 int = x61
    var jp110 Option__int
    if ok__218 {
        var t111 Option__int = Option__int_Some{
            _0: value__217,
        }
        jp110 = t111
    } else {
        jp110 = Option__int_None{}
    }
    retv108 = jp110
    return retv108
}

func println__T_string(value__1 string) struct{} {
    var t113 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t113)
    return struct{}{}
}

func roundtrip__T_string(channel__0 chan string, value__1 string) Option__string {
    var retv116 Option__string
    var t117 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: channel__0,
        value_1: value__1,
    }
    go _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(t117)
    var t118 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(channel__0)
    retv116 = t118
    return retv116
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__string(self__219 chan string) struct{} {
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(self__219)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv122 string
    var t123 string = _goml_runtime_core_int_to_string(self__40)
    retv122 = t123
    return retv122
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv125 string
    retv125 = self__38
    return retv125
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__string(self__214 chan string, value__215 string) struct{} {
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__214, value__215)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__216 chan string) Option__string {
    var retv129 Option__string
    var mtmp60 Tuple2_6string_4bool = func(p0 chan string) Tuple2_6string_4bool {
        var value string
        var ok bool
        value, ok = <-p0
        return Tuple2_6string_4bool{
            _0: value,
            _1: ok,
        }
    }(self__216)
    var x61 string = mtmp60._0
    var x62 bool = mtmp60._1
    var ok__218 bool = x62
    var value__217 string = x61
    var jp131 Option__string
    if ok__218 {
        var t132 Option__string = Option__string_Some{
            _0: value__217,
        }
        jp131 = t132
    } else {
        jp131 = Option__string_None{}
    }
    retv129 = jp131
    return retv129
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env81 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env81.channel_0
    var value__1 string = env81.value_1
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__string(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
