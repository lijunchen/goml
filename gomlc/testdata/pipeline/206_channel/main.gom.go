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
    var mtmp186 Option__int
    var inline294 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline295 int = inline294._0
    var inline296 bool = inline294._1
    if inline296 {
        var inline299 Option__int = Option__int_Some{
            _0: inline295,
        }
        mtmp186 = inline299
    } else {
        mtmp186 = Option__int_None{}
    }
    var jp205 int
    switch mtmp186.(type) {
    case Option__int_None:
        jp205 = -1
    case Option__int_Some:
        var x187 int = mtmp186.(Option__int_Some)._0
        jp205 = x187
    default:
        panic("non-exhaustive match")
    }
    var inline291 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp205)
    _goml_runtime_core_string_println(inline291)
    var mtmp189 Option__int
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
        mtmp189 = inline289
    } else {
        mtmp189 = Option__int_None{}
    }
    var jp207 int
    switch mtmp189.(type) {
    case Option__int_None:
        jp207 = -1
    case Option__int_Some:
        var x190 int = mtmp189.(Option__int_Some)._0
        jp207 = x190
    default:
        panic("non-exhaustive match")
    }
    var inline281 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp207)
    _goml_runtime_core_string_println(inline281)
    var mtmp192 Option__int
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
        mtmp192 = inline279
    } else {
        mtmp192 = Option__int_None{}
    }
    var jp209 string
    switch mtmp192.(type) {
    case Option__int_None:
        jp209 = "closed"
    case Option__int_Some:
        jp209 = "open"
    default:
        panic("non-exhaustive match")
    }
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp209)
    _goml_runtime_core_string_println(inline271)
    var unbuffered__5 chan string
    var inline269 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    unbuffered__5 = inline269
    var mtmp195 Option__string
    var inline263 string = "ready"
    var inline264 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: unbuffered__5,
        value_1: inline263,
    }
    var inline265 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(inline264)
    }
    go inline265()
    var inline267 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(unbuffered__5)
    mtmp195 = inline267
    var jp211 string
    switch mtmp195.(type) {
    case Option__string_None:
        jp211 = "closed"
    case Option__string_Some:
        var x196 string = mtmp195.(Option__string_Some)._0
        jp211 = x196
    default:
        panic("non-exhaustive match")
    }
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp211)
    _goml_runtime_core_string_println(inline260)
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__274 int) chan string {
    var t214 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__274)
    return t214
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__274 int) chan int {
    var t217 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__274)
    return t217
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(self__275 chan int, value__276 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__275, value__276)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(self__280 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__280)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t242 string = _goml_runtime_core_int_to_string(self__67)
    return t242
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__277 chan string) Option__string {
    var mtmp153 Tuple2_6string_4bool = func(p0 chan string) Tuple2_6string_4bool {
        var value string
        var ok bool
        value, ok = <-p0
        return Tuple2_6string_4bool{
            _0: value,
            _1: ok,
        }
    }(self__277)
    var x154 string = mtmp153._0
    var x155 bool = mtmp153._1
    if x155 {
        var t251 Option__string = Option__string_Some{
            _0: x154,
        }
        return t251
    } else {
        return Option__string_None{}
    }
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env199 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env199.channel_0
    var value__1 string = env199.value_1
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
