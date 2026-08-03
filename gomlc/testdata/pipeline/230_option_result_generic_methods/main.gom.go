package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {}

type closure_env_main_4 struct {}

type closure_env_main_5 struct {}

type closure_env_main_6 struct {}

type closure_env_main_7 struct {}

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

type Result__int__string interface {
    isResult__int__string()
}

type Result__int__string_Ok struct {
    _0 int
}

func (_ Result__int__string_Ok) isResult__int__string() {}

type Result__int__string_Err struct {
    _0 string
}

func (_ Result__int__string_Err) isResult__int__string() {}

type Result__int__int interface {
    isResult__int__int()
}

type Result__int__int_Ok struct {
    _0 int
}

func (_ Result__int__int_Ok) isResult__int__int() {}

type Result__int__int_Err struct {
    _0 int
}

func (_ Result__int__int_Err) isResult__int__int() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

func main0() struct{} {
    var some__0 Option__int = Option__int_Some{
        _0: 3,
    }
    var t152 closure_env_main_0 = closure_env_main_0{}
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t152, p0)
    })
    var t153 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t153)
    var t154 closure_env_main_1 = closure_env_main_1{}
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t154, p0)
    })
    var t155 string
    var inline311 string = "missing"
    switch static_mapped__4.(type) {
    case Option__string_None:
        t155 = inline311
    case Option__string_Some:
        var inline312 string = static_mapped__4.(Option__string_Some)._0
        t155 = inline312
    default:
        panic("non-exhaustive match")
    }
    var inline308 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t155)
    _goml_runtime_core_string_println(inline308)
    var t156 closure_env_main_2 = closure_env_main_2{}
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t156, p0)
    })
    var t157 string
    var inline304 string = "missing"
    switch chained__6.(type) {
    case Option__string_None:
        t157 = inline304
    case Option__string_Some:
        var inline305 string = chained__6.(Option__string_Some)._0
        t157 = inline305
    default:
        panic("non-exhaustive match")
    }
    var inline301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t157)
    _goml_runtime_core_string_println(inline301)
    var none__7 Option__int = Option__int_None{}
    var converted__8 Result__int__string
    var inline295 string = "none"
    switch none__7.(type) {
    case Option__int_None:
        var inline296 Result__int__string = Result__int__string_Err{
            _0: inline295,
        }
        converted__8 = inline296
    case Option__int_Some:
        var inline297 int = none__7.(Option__int_Some)._0
        var inline299 Result__int__string = Result__int__string_Ok{
            _0: inline297,
        }
        converted__8 = inline299
    default:
        panic("non-exhaustive match")
    }
    var t158 closure_env_main_3 = closure_env_main_3{}
    var t159 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t158, p0)
    })
    var inline292 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t159)
    _goml_runtime_core_string_println(inline292)
    var ok__10 Result__int__string = Result__int__string_Ok{
        _0: 5,
    }
    var t160 closure_env_main_4 = closure_env_main_4{}
    var t161 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t160, p0)
    })
    var t162 int
    var inline287 int = 0
    switch t161.(type) {
    case Result__int__string_Ok:
        var inline288 int = t161.(Result__int__string_Ok)._0
        t162 = inline288
    case Result__int__string_Err:
        t162 = inline287
    default:
        panic("non-exhaustive match")
    }
    var inline284 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t162)
    _goml_runtime_core_string_println(inline284)
    var error__12 Result__int__string = Result__int__string_Err{
        _0: "bad",
    }
    var t163 closure_env_main_5 = closure_env_main_5{}
    var mapped_error__14 Result__int__int = _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(error__12, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t163, p0)
    })
    var t164 closure_env_main_6 = closure_env_main_6{}
    var t165 int = _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(mapped_error__14, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t164, p0)
    })
    var inline281 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t165)
    _goml_runtime_core_string_println(inline281)
    var t166 closure_env_main_7 = closure_env_main_7{}
    var next__17 Result__string__string = _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(ok__10, func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t166, p0)
    })
    var t167 string
    var inline276 string = "missing"
    switch next__17.(type) {
    case Result__string__string_Ok:
        var inline277 string = next__17.(Result__string__string_Ok)._0
        t167 = inline277
    case Result__string__string_Err:
        t167 = inline276
    default:
        panic("non-exhaustive match")
    }
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t167)
    _goml_runtime_core_string_println(inline273)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__267 Option__int, map_fn__268 func(int) string) Option__string {
    switch self__267.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x127 int = self__267.(Option__int_Some)._0
        var t175 string = map_fn__268(x127)
        var t176 Option__string = Option__string_Some{
            _0: t175,
        }
        return t176
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__31 string) struct{} {
    var t178 string
    t178 = value__31
    _goml_runtime_core_string_println(t178)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__247 Option__string, fallback__248 string) string {
    switch self__247.(type) {
    case Option__string_None:
        return fallback__248
    case Option__string_Some:
        var x116 string = self__247.(Option__string_Some)._0
        return x116
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__270 Option__int, next__271 func(int) Option__string) Option__string {
    switch self__270.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x128 int = self__270.(Option__int_Some)._0
        var t188 Option__string = next__271(x128)
        return t188
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__258 Result__int__string, fallback__259 func(string) int) int {
    switch self__258.(type) {
    case Result__int__string_Ok:
        var x122 int = self__258.(Result__int__string_Ok)._0
        return x122
    case Result__int__string_Err:
        var x123 string = self__258.(Result__int__string_Err)._0
        var t205 int = fallback__259(x123)
        return t205
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__276 Result__int__string, map_fn__277 func(int) int) Result__int__string {
    switch self__276.(type) {
    case Result__int__string_Ok:
        var x130 int = self__276.(Result__int__string_Ok)._0
        var t210 int = map_fn__277(x130)
        var t211 Result__int__string = Result__int__string_Ok{
            _0: t210,
        }
        return t211
    case Result__int__string_Err:
        var x131 string = self__276.(Result__int__string_Err)._0
        var t212 Result__int__string = Result__int__string_Err{
            _0: x131,
        }
        return t212
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(self__280 Result__int__string, map_fn__281 func(string) int) Result__int__int {
    switch self__280.(type) {
    case Result__int__string_Ok:
        var x132 int = self__280.(Result__int__string_Ok)._0
        var t221 Result__int__int = Result__int__int_Ok{
            _0: x132,
        }
        return t221
    case Result__int__string_Err:
        var x133 string = self__280.(Result__int__string_Err)._0
        var t222 int = map_fn__281(x133)
        var t223 Result__int__int = Result__int__int_Err{
            _0: t222,
        }
        return t223
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(self__258 Result__int__int, fallback__259 func(int) int) int {
    switch self__258.(type) {
    case Result__int__int_Ok:
        var x122 int = self__258.(Result__int__int_Ok)._0
        return x122
    case Result__int__int_Err:
        var x123 int = self__258.(Result__int__int_Err)._0
        var t228 int = fallback__259(x123)
        return t228
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(self__284 Result__int__string, next__285 func(int) Result__string__string) Result__string__string {
    switch self__284.(type) {
    case Result__int__string_Ok:
        var x134 int = self__284.(Result__int__string_Ok)._0
        var t233 Result__string__string = next__285(x134)
        return t233
    case Result__int__string_Err:
        var x135 string = self__284.(Result__int__string_Err)._0
        var t234 Result__string__string = Result__string__string_Err{
            _0: x135,
        }
        return t234
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t243 string = _goml_runtime_core_int_to_string(self__69)
    return t243
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env143 closure_env_main_0, value__1 int) string {
    var inline318 string = _goml_runtime_core_int_to_string(value__1)
    return inline318
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env144 closure_env_main_1, value__3 int) string {
    var t249 string
    var inline320 string = _goml_runtime_core_int_to_string(value__3)
    t249 = inline320
    var t250 string = "static:" + t249
    return t250
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env145 closure_env_main_2, value__5 int) Option__string {
    var t253 string
    var inline322 string = _goml_runtime_core_int_to_string(value__5)
    t253 = inline322
    var t254 string = "value:" + t253
    var t255 Option__string = Option__string_Some{
        _0: t254,
    }
    return t255
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env146 closure_env_main_3, error__9 string) int {
    var inline324 int = _goml_runtime_core_string_len(error__9)
    return inline324
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env147 closure_env_main_4, value__11 int) int {
    var t261 int = value__11 + 2
    return t261
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env148 closure_env_main_5, value__13 string) int {
    var inline326 int = _goml_runtime_core_string_len(value__13)
    return inline326
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env149 closure_env_main_6, value__15 int) int {
    return value__15
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env150 closure_env_main_7, value__16 int) Result__string__string {
    var t269 string
    var inline328 string = _goml_runtime_core_int_to_string(value__16)
    t269 = inline328
    var t270 string = "next:" + t269
    var t271 Result__string__string = Result__string__string_Ok{
        _0: t270,
    }
    return t271
}

func main() {
    main0()
}
