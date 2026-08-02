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
    var mtmp159 Option__int
    var inline265 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline266 int = inline265._0
    var inline267 bool = inline265._1
    if inline267 {
        var inline270 Option__int = Option__int_Some{
            _0: inline266,
        }
        mtmp159 = inline270
    } else {
        mtmp159 = Option__int_None{}
    }
    var jp178 int
    switch mtmp159.(type) {
    case Option__int_None:
        jp178 = -1
    case Option__int_Some:
        var x160 int = mtmp159.(Option__int_Some)._0
        jp178 = x160
    default:
        panic("non-exhaustive match")
    }
    var inline262 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp178)
    _goml_runtime_core_string_println(inline262)
    var mtmp162 Option__int
    var inline255 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline256 int = inline255._0
    var inline257 bool = inline255._1
    if inline257 {
        var inline260 Option__int = Option__int_Some{
            _0: inline256,
        }
        mtmp162 = inline260
    } else {
        mtmp162 = Option__int_None{}
    }
    var jp180 int
    switch mtmp162.(type) {
    case Option__int_None:
        jp180 = -1
    case Option__int_Some:
        var x163 int = mtmp162.(Option__int_Some)._0
        jp180 = x163
    default:
        panic("non-exhaustive match")
    }
    var inline252 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp180)
    _goml_runtime_core_string_println(inline252)
    var mtmp165 Option__int
    var inline245 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(buffered__2)
    var inline246 int = inline245._0
    var inline247 bool = inline245._1
    if inline247 {
        var inline250 Option__int = Option__int_Some{
            _0: inline246,
        }
        mtmp165 = inline250
    } else {
        mtmp165 = Option__int_None{}
    }
    var jp182 string
    switch mtmp165.(type) {
    case Option__int_None:
        jp182 = "closed"
    case Option__int_Some:
        jp182 = "open"
    default:
        panic("non-exhaustive match")
    }
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp182)
    _goml_runtime_core_string_println(inline242)
    var unbuffered__5 chan string
    var inline240 chan string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(0)
    unbuffered__5 = inline240
    var mtmp168 Option__string
    var inline235 string = "ready"
    var inline236 closure_env_roundtrip_T_string_0 = closure_env_roundtrip_T_string_0{
        channel_0: unbuffered__5,
        value_1: inline235,
    }
    go _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(inline236)
    var inline238 Option__string = _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(unbuffered__5)
    mtmp168 = inline238
    var jp184 string
    switch mtmp168.(type) {
    case Option__string_None:
        jp184 = "closed"
    case Option__string_Some:
        var x169 string = mtmp168.(Option__string_Some)._0
        jp184 = x169
    default:
        panic("non-exhaustive match")
    }
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(jp184)
    _goml_runtime_core_string_println(inline232)
    func(p0 chan string) struct{} {
        close(p0)
        return struct{}{}
    }(unbuffered__5)
    return struct{}{}
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__string(capacity__211 int) chan string {
    var t187 chan string = func(p0 int) chan string {
        return make(chan string, p0)
    }(capacity__211)
    return t187
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_new____T__int(capacity__211 int) chan int {
    var t190 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(capacity__211)
    return t190
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

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t214 string = _goml_runtime_core_int_to_string(self__40)
    return t214
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_Channel_i_Channel_l_T_r__i_recv____T__string(self__214 chan string) Option__string {
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
    if x103 {
        var t223 Option__string = Option__string_Some{
            _0: x102,
        }
        return t223
    } else {
        return Option__string_None{}
    }
}

func _goml_m_inherent_i_closure__en_h6de6ca36633f2d445fcbfaf4a81e6dce_ring__0_i_apply(env172 closure_env_roundtrip_T_string_0) struct{} {
    var channel__0 chan string = env172.channel_0
    var value__1 string = env172.value_1
    func(p0 chan string, p1 string) struct{} {
        p0 <- p1
        return struct{}{}
    }(channel__0, value__1)
    return struct{}{}
}

func main() {
    main0()
}
