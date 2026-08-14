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

func main0() struct{} {
    var buffered__2 chan int = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(2)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 10)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(buffered__2, 20)
    _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(buffered__2)
    var mtmp191 Option__int
    var inline299 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline300 int = inline299._0
    var inline301 bool = inline299._1
    if inline301 {
        var inline304 Option__int = Option__int_Some{
            _0: inline300,
        }
        mtmp191 = inline304
    } else {
        mtmp191 = Option__int_None{}
    }
    var jp210 int
    switch mtmp191.(type) {
    case Option__int_None:
        jp210 = -1
    case Option__int_Some:
        var x192 int = mtmp191.(Option__int_Some)._0
        jp210 = x192
    default:
        panic("non-exhaustive match")
    }
    var inline296 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp210)
    _goml_runtime_core_string_println(inline296)
    var mtmp194 Option__int
    var inline289 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline290 int = inline289._0
    var inline291 bool = inline289._1
    if inline291 {
        var inline294 Option__int = Option__int_Some{
            _0: inline290,
        }
        mtmp194 = inline294
    } else {
        mtmp194 = Option__int_None{}
    }
    var jp212 int
    switch mtmp194.(type) {
    case Option__int_None:
        jp212 = -1
    case Option__int_Some:
        var x195 int = mtmp194.(Option__int_Some)._0
        jp212 = x195
    default:
        panic("non-exhaustive match")
    }
    var inline286 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp212)
    _goml_runtime_core_string_println(inline286)
    var mtmp197 Option__int
    var inline279 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline280 int = inline279._0
    var inline281 bool = inline279._1
    if inline281 {
        var inline284 Option__int = Option__int_Some{
            _0: inline280,
        }
        mtmp197 = inline284
    } else {
        mtmp197 = Option__int_None{}
    }
    var jp214 string
    switch mtmp197.(type) {
    case Option__int_None:
        jp214 = "closed"
    case Option__int_Some:
        jp214 = "open"
    default:
        panic("non-exhaustive match")
    }
    var inline276 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp214)
    _goml_runtime_core_string_println(inline276)
    var unbuffered__5 chan string
    var inline274 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    unbuffered__5 = inline274
    var mtmp200 Option__string
    var inline268 string = "ready"
    var inline269 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: unbuffered__5,
        value_1: inline268,
    }
    var inline270 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(inline269)
    }
    go inline270()
    var inline272 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(unbuffered__5)
    mtmp200 = inline272
    var jp216 string
    switch mtmp200.(type) {
    case Option__string_None:
        jp216 = "closed"
    case Option__string_Some:
        var x201 string = mtmp200.(Option__string_Some)._0
        jp216 = x201
    default:
        panic("non-exhaustive match")
    }
    var inline265 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp216)
    _goml_runtime_core_string_println(inline265)
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__277 int) chan string {
    var t219 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__277)
    return t219
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__277 int) chan int {
    var t222 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__277)
    return t222
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(self__278 chan int, value__279 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__278, value__279)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(self__283 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__283)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t247 string = _goml_runtime_core_int_to_string(self__67)
    return t247
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__280 chan string) Option__string {
    var mtmp158 Tuple2_6string_4bool = func(p0 chan string) Tuple2_6string_4bool {
        var value string
        var ok bool
        value, ok = <-p0
        return Tuple2_6string_4bool{
            _0: value,
            _1: ok,
        }
    }(self__280)
    var x159 string = mtmp158._0
    var x160 bool = mtmp158._1
    if x160 {
        var t256 Option__string = Option__string_Some{
            _0: x159,
        }
        return t256
    } else {
        return Option__string_None{}
    }
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env204 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env204.channel_0
    var value__1 string = env204.value_1
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
