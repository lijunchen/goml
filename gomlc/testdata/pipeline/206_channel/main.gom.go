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
    var mtmp181 Option__int
    var inline287 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline288 int = inline287._0
    var inline289 bool = inline287._1
    if inline289 {
        var inline292 Option__int = Option__int_Some{
            _0: inline288,
        }
        mtmp181 = inline292
    } else {
        mtmp181 = Option__int_None{}
    }
    var jp200 int
    switch mtmp181.(type) {
    case Option__int_None:
        jp200 = -1
    case Option__int_Some:
        var x182 int = mtmp181.(Option__int_Some)._0
        jp200 = x182
    default:
        panic("non-exhaustive match")
    }
    var inline284 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp200)
    _goml_runtime_core_string_println(inline284)
    var mtmp184 Option__int
    var inline277 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline278 int = inline277._0
    var inline279 bool = inline277._1
    if inline279 {
        var inline282 Option__int = Option__int_Some{
            _0: inline278,
        }
        mtmp184 = inline282
    } else {
        mtmp184 = Option__int_None{}
    }
    var jp202 int
    switch mtmp184.(type) {
    case Option__int_None:
        jp202 = -1
    case Option__int_Some:
        var x185 int = mtmp184.(Option__int_Some)._0
        jp202 = x185
    default:
        panic("non-exhaustive match")
    }
    var inline274 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp202)
    _goml_runtime_core_string_println(inline274)
    var mtmp187 Option__int
    var inline267 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline268 int = inline267._0
    var inline269 bool = inline267._1
    if inline269 {
        var inline272 Option__int = Option__int_Some{
            _0: inline268,
        }
        mtmp187 = inline272
    } else {
        mtmp187 = Option__int_None{}
    }
    var jp204 string
    switch mtmp187.(type) {
    case Option__int_None:
        jp204 = "closed"
    case Option__int_Some:
        jp204 = "open"
    default:
        panic("non-exhaustive match")
    }
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp204)
    _goml_runtime_core_string_println(inline264)
    var unbuffered__5 chan string
    var inline262 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    unbuffered__5 = inline262
    var mtmp190 Option__string
    var inline257 string = "ready"
    var inline258 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: unbuffered__5,
        value_1: inline257,
    }
    go _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(inline258)
    var inline260 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(unbuffered__5)
    mtmp190 = inline260
    var jp206 string
    switch mtmp190.(type) {
    case Option__string_None:
        jp206 = "closed"
    case Option__string_Some:
        var x191 string = mtmp190.(Option__string_Some)._0
        jp206 = x191
    default:
        panic("non-exhaustive match")
    }
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp206)
    _goml_runtime_core_string_println(inline254)
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__240 int) chan string {
    var t209 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__240)
    return t209
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__240 int) chan int {
    var t212 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__240)
    return t212
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(self__241 chan int, value__242 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__241, value__242)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(self__246 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__246)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t236 string = _goml_runtime_core_int_to_string(self__69)
    return t236
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__243 chan string) Option__string {
    var mtmp123 Tuple2_6string_4bool = func(p0 chan string) Tuple2_6string_4bool {
        var value string
        var ok bool
        value, ok = <-p0
        return Tuple2_6string_4bool{
            _0: value,
            _1: ok,
        }
    }(self__243)
    var x124 string = mtmp123._0
    var x125 bool = mtmp123._1
    if x125 {
        var t245 Option__string = Option__string_Some{
            _0: x124,
        }
        return t245
    } else {
        return Option__string_None{}
    }
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env194 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env194.channel_0
    var value__1 string = env194.value_1
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
