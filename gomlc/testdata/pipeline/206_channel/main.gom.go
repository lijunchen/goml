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
    var mtmp176 Option__int
    var inline284 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline285 int = inline284._0
    var inline286 bool = inline284._1
    if inline286 {
        var inline289 Option__int = Option__int_Some{
            _0: inline285,
        }
        mtmp176 = inline289
    } else {
        mtmp176 = Option__int_None{}
    }
    var jp195 int
    switch mtmp176.(type) {
    case Option__int_None:
        jp195 = -1
    case Option__int_Some:
        var x177 int = mtmp176.(Option__int_Some)._0
        jp195 = x177
    default:
        panic("non-exhaustive match")
    }
    var inline281 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp195)
    _goml_runtime_core_string_println(inline281)
    var mtmp179 Option__int
    var inline274 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline275 int = inline274._0
    var inline276 bool = inline274._1
    if inline276 {
        var inline279 Option__int = Option__int_Some{
            _0: inline275,
        }
        mtmp179 = inline279
    } else {
        mtmp179 = Option__int_None{}
    }
    var jp197 int
    switch mtmp179.(type) {
    case Option__int_None:
        jp197 = -1
    case Option__int_Some:
        var x180 int = mtmp179.(Option__int_Some)._0
        jp197 = x180
    default:
        panic("non-exhaustive match")
    }
    var inline271 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp197)
    _goml_runtime_core_string_println(inline271)
    var mtmp182 Option__int
    var inline264 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline265 int = inline264._0
    var inline266 bool = inline264._1
    if inline266 {
        var inline269 Option__int = Option__int_Some{
            _0: inline265,
        }
        mtmp182 = inline269
    } else {
        mtmp182 = Option__int_None{}
    }
    var jp199 string
    switch mtmp182.(type) {
    case Option__int_None:
        jp199 = "closed"
    case Option__int_Some:
        jp199 = "open"
    default:
        panic("non-exhaustive match")
    }
    var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp199)
    _goml_runtime_core_string_println(inline261)
    var unbuffered__5 chan string
    var inline259 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    unbuffered__5 = inline259
    var mtmp185 Option__string
    var inline253 string = "ready"
    var inline254 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: unbuffered__5,
        value_1: inline253,
    }
    var inline255 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(inline254)
    }
    go inline255()
    var inline257 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(unbuffered__5)
    mtmp185 = inline257
    var jp201 string
    switch mtmp185.(type) {
    case Option__string_None:
        jp201 = "closed"
    case Option__string_Some:
        var x186 string = mtmp185.(Option__string_Some)._0
        jp201 = x186
    default:
        panic("non-exhaustive match")
    }
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp201)
    _goml_runtime_core_string_println(inline250)
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__259 int) chan string {
    var t204 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__259)
    return t204
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__259 int) chan int {
    var t207 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__259)
    return t207
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(self__260 chan int, value__261 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__260, value__261)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(self__265 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__265)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t232 string = _goml_runtime_core_int_to_string(self__67)
    return t232
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__262 chan string) Option__string {
    var mtmp143 Tuple2_6string_4bool = func(p0 chan string) Tuple2_6string_4bool {
        var value string
        var ok bool
        value, ok = <-p0
        return Tuple2_6string_4bool{
            _0: value,
            _1: ok,
        }
    }(self__262)
    var x144 string = mtmp143._0
    var x145 bool = mtmp143._1
    if x145 {
        var t241 Option__string = Option__string_Some{
            _0: x144,
        }
        return t241
    } else {
        return Option__string_None{}
    }
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env189 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env189.channel_0
    var value__1 string = env189.value_1
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
