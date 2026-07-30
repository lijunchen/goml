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
    var retv87 chan string
    var t88 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    retv87 = t88
    return retv87
}

func main0() struct{} {
    var buffered__2 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(2)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 10)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 20)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(buffered__2)
    var mtmp72 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp91 int
    switch mtmp72.(type) {
    case Option__int_None:
        jp91 = -1
    case Option__int_Some:
        var x73 int = mtmp72.(Option__int_Some)._0
        var value__3 int = x73
        jp91 = value__3
    default:
        panic("non-exhaustive match")
    }
    println__T_int(jp91)
    var mtmp75 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp93 int
    switch mtmp75.(type) {
    case Option__int_None:
        jp93 = -1
    case Option__int_Some:
        var x76 int = mtmp75.(Option__int_Some)._0
        var value__4 int = x76
        jp93 = value__4
    default:
        panic("non-exhaustive match")
    }
    println__T_int(jp93)
    var mtmp78 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp95 string
    switch mtmp78.(type) {
    case Option__int_None:
        jp95 = "closed"
    case Option__int_Some:
        jp95 = "open"
    default:
        panic("non-exhaustive match")
    }
    println__T_string(jp95)
    var unbuffered__5 chan string = make_string_channel()
    var mtmp81 Option__string = roundtrip__T_string(unbuffered__5, "ready")
    var jp97 string
    switch mtmp81.(type) {
    case Option__string_None:
        jp97 = "closed"
    case Option__string_Some:
        var x82 string = mtmp81.(Option__string_Some)._0
        var value__6 string = x82
        jp97 = value__6
    default:
        panic("non-exhaustive match")
    }
    println__T_string(jp97)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__string(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__211 int) chan string {
    var retv99 chan string
    var t100 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__211)
    retv99 = t100
    return retv99
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__211 int) chan int {
    var retv102 chan int
    var t103 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__211)
    retv102 = t103
    return retv102
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(self__212 chan int, value__213 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__212, value__213)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(self__217 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__217)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t109 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t109)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(self__214 chan int) Option__int {
    var retv112 Option__int
    var mtmp60 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(self__214)
    var x61 int = mtmp60._0
    var x62 bool = mtmp60._1
    var ok__216 bool = x62
    var value__215 int = x61
    var jp114 Option__int
    if ok__216 {
        var t115 Option__int = Option__int_Some{
            _0: value__215,
        }
        jp114 = t115
    } else {
        jp114 = Option__int_None{}
    }
    retv112 = jp114
    return retv112
}

func println__T_string(value__1 string) struct{} {
    var t117 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t117)
    return struct{}{}
}

func roundtrip__T_string(channel__0 chan string, value__1 string) Option__string {
    var retv120 Option__string
    var t121 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: channel__0,
        value_1: value__1,
    }
    go _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(t121)
    var t122 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(channel__0)
    retv120 = t122
    return retv120
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__string(self__217 chan string) struct{} {
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(self__217)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv126 string
    var t127 string = _goml_runtime_core_int_to_string(self__40)
    retv126 = t127
    return retv126
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv129 string
    retv129 = self__38
    return retv129
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__string(self__212 chan string, value__213 string) struct{} {
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__212, value__213)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__214 chan string) Option__string {
    var retv133 Option__string
    var mtmp60 Tuple2_6string_4bool = func(p0 chan string) Tuple2_6string_4bool {
        var value string
        var ok bool
        value, ok = <-p0
        return Tuple2_6string_4bool{
            _0: value,
            _1: ok,
        }
    }(self__214)
    var x61 string = mtmp60._0
    var x62 bool = mtmp60._1
    var ok__216 bool = x62
    var value__215 string = x61
    var jp135 Option__string
    if ok__216 {
        var t136 Option__string = Option__string_Some{
            _0: value__215,
        }
        jp135 = t136
    } else {
        jp135 = Option__string_None{}
    }
    retv133 = jp135
    return retv133
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env85 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env85.channel_0
    var value__1 string = env85.value_1
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__string(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
