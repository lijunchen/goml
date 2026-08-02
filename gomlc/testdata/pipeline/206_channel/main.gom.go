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
    var retv174 chan string
    var t175 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    retv174 = t175
    return retv174
}

func main0() struct{} {
    var buffered__2 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(2)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 10)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 20)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(buffered__2)
    var mtmp159 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp178 int
    switch mtmp159.(type) {
    case Option__int_None:
        jp178 = -1
    case Option__int_Some:
        var x160 int = mtmp159.(Option__int_Some)._0
        var value__3 int = x160
        jp178 = value__3
    default:
        panic("non-exhaustive match")
    }
    println__T_int(jp178)
    var mtmp162 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp180 int
    switch mtmp162.(type) {
    case Option__int_None:
        jp180 = -1
    case Option__int_Some:
        var x163 int = mtmp162.(Option__int_Some)._0
        var value__4 int = x163
        jp180 = value__4
    default:
        panic("non-exhaustive match")
    }
    println__T_int(jp180)
    var mtmp165 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp182 string
    switch mtmp165.(type) {
    case Option__int_None:
        jp182 = "closed"
    case Option__int_Some:
        jp182 = "open"
    default:
        panic("non-exhaustive match")
    }
    println__T_string(jp182)
    var unbuffered__5 chan string = make_string_channel()
    var mtmp168 Option__string = roundtrip__T_string(unbuffered__5, "ready")
    var jp184 string
    switch mtmp168.(type) {
    case Option__string_None:
        jp184 = "closed"
    case Option__string_Some:
        var x169 string = mtmp168.(Option__string_Some)._0
        var value__6 string = x169
        jp184 = value__6
    default:
        panic("non-exhaustive match")
    }
    println__T_string(jp184)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__string(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__211 int) chan string {
    var retv186 chan string
    var t187 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__211)
    retv186 = t187
    return retv186
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__211 int) chan int {
    var retv189 chan int
    var t190 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__211)
    retv189 = t190
    return retv189
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
    var t196 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t196)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(self__214 chan int) Option__int {
    var retv199 Option__int
    var mtmp101 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(self__214)
    var x102 int = mtmp101._0
    var x103 bool = mtmp101._1
    var ok__216 bool = x103
    var value__215 int = x102
    var jp201 Option__int
    if ok__216 {
        var t202 Option__int = Option__int_Some{
            _0: value__215,
        }
        jp201 = t202
    } else {
        jp201 = Option__int_None{}
    }
    retv199 = jp201
    return retv199
}

func println__T_string(value__1 string) struct{} {
    var t204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t204)
    return struct{}{}
}

func roundtrip__T_string(channel__0 chan string, value__1 string) Option__string {
    var retv207 Option__string
    var t208 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: channel__0,
        value_1: value__1,
    }
    go _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(t208)
    var t209 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(channel__0)
    retv207 = t209
    return retv207
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__string(self__217 chan string) struct{} {
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(self__217)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv213 string
    var t214 string = _goml_runtime_core_int_to_string(self__40)
    retv213 = t214
    return retv213
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv216 string
    retv216 = self__38
    return retv216
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__string(self__212 chan string, value__213 string) struct{} {
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__212, value__213)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__214 chan string) Option__string {
    var retv220 Option__string
    var mtmp101 Tuple2_6string_4bool = func(p0 chan string) Tuple2_6string_4bool {
        var value string
        var ok bool
        value, ok = <-p0
        return Tuple2_6string_4bool{
            _0: value,
            _1: ok,
        }
    }(self__214)
    var x102 string = mtmp101._0
    var x103 bool = mtmp101._1
    var ok__216 bool = x103
    var value__215 string = x102
    var jp222 Option__string
    if ok__216 {
        var t223 Option__string = Option__string_Some{
            _0: value__215,
        }
        jp222 = t223
    } else {
        jp222 = Option__string_None{}
    }
    retv220 = jp222
    return retv220
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env172 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env172.channel_0
    var value__1 string = env172.value_1
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__string(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
