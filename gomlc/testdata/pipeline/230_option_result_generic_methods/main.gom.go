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
    var t188 closure_env_main_0 = closure_env_main_0{}
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t188, p0)
    })
    var t189 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    var inline349 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline349)
    var t190 closure_env_main_1 = closure_env_main_1{}
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t190, p0)
    })
    var t191 string
    var inline345 string = "missing"
    switch static_mapped__4.(type) {
    case Option__string_None:
        t191 = inline345
    case Option__string_Some:
        var inline346 string = static_mapped__4.(Option__string_Some)._0
        t191 = inline346
    default:
        panic("non-exhaustive match")
    }
    var inline342 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline342)
    var t192 closure_env_main_2 = closure_env_main_2{}
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t192, p0)
    })
    var t193 string
    var inline338 string = "missing"
    switch chained__6.(type) {
    case Option__string_None:
        t193 = inline338
    case Option__string_Some:
        var inline339 string = chained__6.(Option__string_Some)._0
        t193 = inline339
    default:
        panic("non-exhaustive match")
    }
    var inline335 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline335)
    var none__7 Option__int = Option__int_None{}
    var converted__8 Result__int__string
    var inline329 string = "none"
    switch none__7.(type) {
    case Option__int_None:
        var inline330 Result__int__string = Result__int__string_Err{
            _0: inline329,
        }
        converted__8 = inline330
    case Option__int_Some:
        var inline331 int = none__7.(Option__int_Some)._0
        var inline333 Result__int__string = Result__int__string_Ok{
            _0: inline331,
        }
        converted__8 = inline333
    default:
        panic("non-exhaustive match")
    }
    var t194 closure_env_main_3 = closure_env_main_3{}
    var t195 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t194, p0)
    })
    var inline326 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t195)
    _goml_runtime_core_string_println(inline326)
    var ok__10 Result__int__string = Result__int__string_Ok{
        _0: 5,
    }
    var t196 closure_env_main_4 = closure_env_main_4{}
    var t197 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t196, p0)
    })
    var t198 int
    var inline322 int = 0
    switch t197.(type) {
    case Result__int__string_Ok:
        var inline323 int = t197.(Result__int__string_Ok)._0
        t198 = inline323
    case Result__int__string_Err:
        t198 = inline322
    default:
        panic("non-exhaustive match")
    }
    var inline319 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t198)
    _goml_runtime_core_string_println(inline319)
    var error__12 Result__int__string = Result__int__string_Err{
        _0: "bad",
    }
    var t199 closure_env_main_5 = closure_env_main_5{}
    var mapped_error__14 Result__int__int = _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(error__12, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t199, p0)
    })
    var t200 closure_env_main_6 = closure_env_main_6{}
    var t201 int = _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(mapped_error__14, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t200, p0)
    })
    var inline316 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t201)
    _goml_runtime_core_string_println(inline316)
    var t202 closure_env_main_7 = closure_env_main_7{}
    var next__17 Result__string__string = _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(ok__10, func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t202, p0)
    })
    var t203 string
    var inline312 string = "missing"
    switch next__17.(type) {
    case Result__string__string_Ok:
        var inline313 string = next__17.(Result__string__string_Ok)._0
        t203 = inline313
    case Result__string__string_Err:
        t203 = inline312
    default:
        panic("non-exhaustive match")
    }
    var inline309 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline309)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__310 Option__int, map_fn__311 func(int) string) Option__string {
    switch self__310.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x163 int = self__310.(Option__int_Some)._0
        var t211 string = map_fn__311(x163)
        var t212 Option__string = Option__string_Some{
            _0: t211,
        }
        return t212
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__290 Option__string, fallback__291 string) string {
    switch self__290.(type) {
    case Option__string_None:
        return fallback__291
    case Option__string_Some:
        var x152 string = self__290.(Option__string_Some)._0
        return x152
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__313 Option__int, next__314 func(int) Option__string) Option__string {
    switch self__313.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x164 int = self__313.(Option__int_Some)._0
        var t224 Option__string = next__314(x164)
        return t224
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__301 Result__int__string, fallback__302 func(string) int) int {
    switch self__301.(type) {
    case Result__int__string_Ok:
        var x158 int = self__301.(Result__int__string_Ok)._0
        return x158
    case Result__int__string_Err:
        var x159 string = self__301.(Result__int__string_Err)._0
        var t241 int = fallback__302(x159)
        return t241
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__319 Result__int__string, map_fn__320 func(int) int) Result__int__string {
    switch self__319.(type) {
    case Result__int__string_Ok:
        var x166 int = self__319.(Result__int__string_Ok)._0
        var t246 int = map_fn__320(x166)
        var t247 Result__int__string = Result__int__string_Ok{
            _0: t246,
        }
        return t247
    case Result__int__string_Err:
        var x167 string = self__319.(Result__int__string_Err)._0
        var t248 Result__int__string = Result__int__string_Err{
            _0: x167,
        }
        return t248
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(self__323 Result__int__string, map_fn__324 func(string) int) Result__int__int {
    switch self__323.(type) {
    case Result__int__string_Ok:
        var x168 int = self__323.(Result__int__string_Ok)._0
        var t257 Result__int__int = Result__int__int_Ok{
            _0: x168,
        }
        return t257
    case Result__int__string_Err:
        var x169 string = self__323.(Result__int__string_Err)._0
        var t258 int = map_fn__324(x169)
        var t259 Result__int__int = Result__int__int_Err{
            _0: t258,
        }
        return t259
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(self__301 Result__int__int, fallback__302 func(int) int) int {
    switch self__301.(type) {
    case Result__int__int_Ok:
        var x158 int = self__301.(Result__int__int_Ok)._0
        return x158
    case Result__int__int_Err:
        var x159 int = self__301.(Result__int__int_Err)._0
        var t264 int = fallback__302(x159)
        return t264
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(self__327 Result__int__string, next__328 func(int) Result__string__string) Result__string__string {
    switch self__327.(type) {
    case Result__int__string_Ok:
        var x170 int = self__327.(Result__int__string_Ok)._0
        var t269 Result__string__string = next__328(x170)
        return t269
    case Result__int__string_Err:
        var x171 string = self__327.(Result__int__string_Err)._0
        var t270 Result__string__string = Result__string__string_Err{
            _0: x171,
        }
        return t270
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t279 string = _goml_runtime_core_int_to_string(self__69)
    return t279
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env179 closure_env_main_0, value__1 int) string {
    var inline355 string = _goml_runtime_core_int_to_string(value__1)
    return inline355
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env180 closure_env_main_1, value__3 int) string {
    var t285 string
    var inline357 string = _goml_runtime_core_int_to_string(value__3)
    t285 = inline357
    var t286 string = "static:" + t285
    return t286
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env181 closure_env_main_2, value__5 int) Option__string {
    var t289 string
    var inline359 string = _goml_runtime_core_int_to_string(value__5)
    t289 = inline359
    var t290 string = "value:" + t289
    var t291 Option__string = Option__string_Some{
        _0: t290,
    }
    return t291
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env182 closure_env_main_3, error__9 string) int {
    var inline361 int = _goml_runtime_core_string_len(error__9)
    return inline361
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env183 closure_env_main_4, value__11 int) int {
    var t297 int = value__11 + 2
    return t297
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env184 closure_env_main_5, value__13 string) int {
    var inline363 int = _goml_runtime_core_string_len(value__13)
    return inline363
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env185 closure_env_main_6, value__15 int) int {
    return value__15
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env186 closure_env_main_7, value__16 int) Result__string__string {
    var t305 string
    var inline365 string = _goml_runtime_core_int_to_string(value__16)
    t305 = inline365
    var t306 string = "next:" + t305
    var t307 Result__string__string = Result__string__string_Ok{
        _0: t306,
    }
    return t307
}

func main() {
    main0()
}
