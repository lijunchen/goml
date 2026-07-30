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
    var retv127 chan string
    var t128 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    retv127 = t128
    return retv127
}

func main0() struct{} {
    var buffered__2 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(2)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 10)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 20)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(buffered__2)
    var mtmp112 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp131 int
    switch mtmp112.(type) {
    case Option__int_None:
        jp131 = -1
    case Option__int_Some:
        var x113 int = mtmp112.(Option__int_Some)._0
        var value__3 int = x113
        jp131 = value__3
    default:
        panic("non-exhaustive match")
    }
    println__T_int(jp131)
    var mtmp115 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp133 int
    switch mtmp115.(type) {
    case Option__int_None:
        jp133 = -1
    case Option__int_Some:
        var x116 int = mtmp115.(Option__int_Some)._0
        var value__4 int = x116
        jp133 = value__4
    default:
        panic("non-exhaustive match")
    }
    println__T_int(jp133)
    var mtmp118 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp135 string
    switch mtmp118.(type) {
    case Option__int_None:
        jp135 = "closed"
    case Option__int_Some:
        jp135 = "open"
    default:
        panic("non-exhaustive match")
    }
    println__T_string(jp135)
    var unbuffered__5 chan string = make_string_channel()
    var mtmp121 Option__string = roundtrip__T_string(unbuffered__5, "ready")
    var jp137 string
    switch mtmp121.(type) {
    case Option__string_None:
        jp137 = "closed"
    case Option__string_Some:
        var x122 string = mtmp121.(Option__string_Some)._0
        var value__6 string = x122
        jp137 = value__6
    default:
        panic("non-exhaustive match")
    }
    println__T_string(jp137)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__string(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__211 int) chan string {
    var retv139 chan string
    var t140 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__211)
    retv139 = t140
    return retv139
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__211 int) chan int {
    var retv142 chan int
    var t143 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__211)
    retv142 = t143
    return retv142
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
    var t149 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t149)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(self__214 chan int) Option__int {
    var retv152 Option__int
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
    var jp154 Option__int
    if ok__216 {
        var t155 Option__int = Option__int_Some{
            _0: value__215,
        }
        jp154 = t155
    } else {
        jp154 = Option__int_None{}
    }
    retv152 = jp154
    return retv152
}

func println__T_string(value__1 string) struct{} {
    var t157 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t157)
    return struct{}{}
}

func roundtrip__T_string(channel__0 chan string, value__1 string) Option__string {
    var retv160 Option__string
    var t161 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: channel__0,
        value_1: value__1,
    }
    go _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(t161)
    var t162 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(channel__0)
    retv160 = t162
    return retv160
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__string(self__217 chan string) struct{} {
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(self__217)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv166 string
    var t167 string = _goml_runtime_core_int_to_string(self__40)
    retv166 = t167
    return retv166
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv169 string
    retv169 = self__38
    return retv169
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__string(self__212 chan string, value__213 string) struct{} {
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__212, value__213)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__214 chan string) Option__string {
    var retv173 Option__string
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
    var jp175 Option__string
    if ok__216 {
        var t176 Option__string = Option__string_Some{
            _0: value__215,
        }
        jp175 = t176
    } else {
        jp175 = Option__string_None{}
    }
    retv173 = jp175
    return retv173
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env125 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env125.channel_0
    var value__1 string = env125.value_1
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__string(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
