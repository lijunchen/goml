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
    var retv171 chan string
    var t172 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    retv171 = t172
    return retv171
}

func main0() struct{} {
    var buffered__2 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(2)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 10)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 20)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(buffered__2)
    var mtmp156 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp175 int
    switch mtmp156.(type) {
    case Option__int_None:
        jp175 = -1
    case Option__int_Some:
        var x157 int = mtmp156.(Option__int_Some)._0
        var value__3 int = x157
        jp175 = value__3
    default:
        panic("non-exhaustive match")
    }
    println__T_int(jp175)
    var mtmp159 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp177 int
    switch mtmp159.(type) {
    case Option__int_None:
        jp177 = -1
    case Option__int_Some:
        var x160 int = mtmp159.(Option__int_Some)._0
        var value__4 int = x160
        jp177 = value__4
    default:
        panic("non-exhaustive match")
    }
    println__T_int(jp177)
    var mtmp162 Option__int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(buffered__2)
    var jp179 string
    switch mtmp162.(type) {
    case Option__int_None:
        jp179 = "closed"
    case Option__int_Some:
        jp179 = "open"
    default:
        panic("non-exhaustive match")
    }
    println__T_string(jp179)
    var unbuffered__5 chan string = make_string_channel()
    var mtmp165 Option__string = roundtrip__T_string(unbuffered__5, "ready")
    var jp181 string
    switch mtmp165.(type) {
    case Option__string_None:
        jp181 = "closed"
    case Option__string_Some:
        var x166 string = mtmp165.(Option__string_Some)._0
        var value__6 string = x166
        jp181 = value__6
    default:
        panic("non-exhaustive match")
    }
    println__T_string(jp181)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__string(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__211 int) chan string {
    var retv183 chan string
    var t184 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__211)
    retv183 = t184
    return retv183
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__211 int) chan int {
    var retv186 chan int
    var t187 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__211)
    retv186 = t187
    return retv186
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
    var t193 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t193)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__int(self__214 chan int) Option__int {
    var retv196 Option__int
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
    var jp198 Option__int
    if ok__216 {
        var t199 Option__int = Option__int_Some{
            _0: value__215,
        }
        jp198 = t199
    } else {
        jp198 = Option__int_None{}
    }
    retv196 = jp198
    return retv196
}

func println__T_string(value__1 string) struct{} {
    var t201 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t201)
    return struct{}{}
}

func roundtrip__T_string(channel__0 chan string, value__1 string) Option__string {
    var retv204 Option__string
    var t205 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: channel__0,
        value_1: value__1,
    }
    go _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(t205)
    var t206 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(channel__0)
    retv204 = t206
    return retv204
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__string(self__217 chan string) struct{} {
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(self__217)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv210 string
    var t211 string = _goml_runtime_core_int_to_string(self__40)
    retv210 = t211
    return retv210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv213 string
    retv213 = self__38
    return retv213
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__string(self__212 chan string, value__213 string) struct{} {
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__212, value__213)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__214 chan string) Option__string {
    var retv217 Option__string
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
    var jp219 Option__string
    if ok__216 {
        var t220 Option__string = Option__string_Some{
            _0: value__215,
        }
        jp219 = t220
    } else {
        jp219 = Option__string_None{}
    }
    retv217 = jp219
    return retv217
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env169 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env169.channel_0
    var value__1 string = env169.value_1
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__string(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
