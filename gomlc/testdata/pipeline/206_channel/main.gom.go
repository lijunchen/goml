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
    var mtmp140 Option__int
    var inline246 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline247 int = inline246._0
    var inline248 bool = inline246._1
    if inline248 {
        var inline251 Option__int = Option__int_Some{
            _0: inline247,
        }
        mtmp140 = inline251
    } else {
        mtmp140 = Option__int_None{}
    }
    var jp159 int
    switch mtmp140.(type) {
    case Option__int_None:
        jp159 = -1
    case Option__int_Some:
        var x141 int = mtmp140.(Option__int_Some)._0
        jp159 = x141
    default:
        panic("non-exhaustive match")
    }
    var inline243 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp159)
    _goml_runtime_core_string_println(inline243)
    var mtmp143 Option__int
    var inline236 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline237 int = inline236._0
    var inline238 bool = inline236._1
    if inline238 {
        var inline241 Option__int = Option__int_Some{
            _0: inline237,
        }
        mtmp143 = inline241
    } else {
        mtmp143 = Option__int_None{}
    }
    var jp161 int
    switch mtmp143.(type) {
    case Option__int_None:
        jp161 = -1
    case Option__int_Some:
        var x144 int = mtmp143.(Option__int_Some)._0
        jp161 = x144
    default:
        panic("non-exhaustive match")
    }
    var inline233 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp161)
    _goml_runtime_core_string_println(inline233)
    var mtmp146 Option__int
    var inline226 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline227 int = inline226._0
    var inline228 bool = inline226._1
    if inline228 {
        var inline231 Option__int = Option__int_Some{
            _0: inline227,
        }
        mtmp146 = inline231
    } else {
        mtmp146 = Option__int_None{}
    }
    var jp163 string
    switch mtmp146.(type) {
    case Option__int_None:
        jp163 = "closed"
    case Option__int_Some:
        jp163 = "open"
    default:
        panic("non-exhaustive match")
    }
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp163)
    _goml_runtime_core_string_println(inline223)
    var unbuffered__5 chan string
    var inline221 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    unbuffered__5 = inline221
    var mtmp149 Option__string
    var inline216 string = "ready"
    var inline217 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: unbuffered__5,
        value_1: inline216,
    }
    go _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(inline217)
    var inline219 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(unbuffered__5)
    mtmp149 = inline219
    var jp165 string
    switch mtmp149.(type) {
    case Option__string_None:
        jp165 = "closed"
    case Option__string_Some:
        var x150 string = mtmp149.(Option__string_Some)._0
        jp165 = x150
    default:
        panic("non-exhaustive match")
    }
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp165)
    _goml_runtime_core_string_println(inline213)
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__236 int) chan string {
    var t168 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__236)
    return t168
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__236 int) chan int {
    var t171 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__236)
    return t171
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_send____T__int(self__237 chan int, value__238 int) struct{} {
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(self__237, value__238)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_close____T__int(self__242 chan int) struct{} {
    func(p0 chan int) struct{} {
        close(p0)
        return struct{}{}
    }(self__242)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t195 string = _goml_runtime_core_int_to_string(self__69)
    return t195
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__239 chan string) Option__string {
    var mtmp107 Tuple2_6string_4bool = func(p0 chan string) Tuple2_6string_4bool {
        var value string
        var ok bool
        value, ok = <-p0
        return Tuple2_6string_4bool{
            _0: value,
            _1: ok,
        }
    }(self__239)
    var x108 string = mtmp107._0
    var x109 bool = mtmp107._1
    if x109 {
        var t204 Option__string = Option__string_Some{
            _0: x108,
        }
        return t204
    } else {
        return Option__string_None{}
    }
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env153 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env153.channel_0
    var value__1 string = env153.value_1
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
