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
    var t203 closure_env_main_0 = closure_env_main_0{}
    var t204 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t203, p0)
    }
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, t204)
    var t205 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t205)
    var t206 closure_env_main_1 = closure_env_main_1{}
    var t207 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t206, p0)
    }
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, t207)
    var t208 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(static_mapped__4, "missing")
    println__T_string(t208)
    var t209 closure_env_main_2 = closure_env_main_2{}
    var t210 func(int) Option__string = func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t209, p0)
    }
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, t210)
    var t211 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(chained__6, "missing")
    println__T_string(t211)
    var none__7 Option__int = Option__int_None{}
    var converted__8 Result__int__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(none__7, "none")
    var t212 closure_env_main_3 = closure_env_main_3{}
    var t213 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t212, p0)
    }
    var t214 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, t213)
    println__T_int(t214)
    var ok__10 Result__int__string = Result__int__string_Ok{
        _0: 5,
    }
    var t215 closure_env_main_4 = closure_env_main_4{}
    var t216 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t215, p0)
    }
    var t217 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, t216)
    var t218 int
    var inline366 int = 0
    switch t217.(type) {
    case Result__int__string_Ok:
        var inline367 int = t217.(Result__int__string_Ok)._0
        t218 = inline367
    case Result__int__string_Err:
        t218 = inline366
    default:
        panic("non-exhaustive match")
    }
    var inline363 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t218)
    _goml_runtime_core_string_println(inline363)
    var t219 closure_env_main_5 = closure_env_main_5{}
    var t220 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t219, p0)
    }
    var mapped_error__14 Result__int__int
    var inline358 string = "bad"
    var inline360 int = t220(inline358)
    var inline361 Result__int__int = Result__int__int_Err{
        _0: inline360,
    }
    mapped_error__14 = inline361
    var t221 closure_env_main_6 = closure_env_main_6{}
    var t222 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t221, p0)
    }
    var t223 int
    switch mapped_error__14.(type) {
    case Result__int__int_Ok:
        var inline349 int = mapped_error__14.(Result__int__int_Ok)._0
        t223 = inline349
    case Result__int__int_Err:
        var inline351 int = mapped_error__14.(Result__int__int_Err)._0
        var inline353 int = t222(inline351)
        t223 = inline353
    default:
        panic("non-exhaustive match")
    }
    var inline346 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t223)
    _goml_runtime_core_string_println(inline346)
    var t224 closure_env_main_7 = closure_env_main_7{}
    var t225 func(int) Result__string__string = func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t224, p0)
    }
    var next__17 Result__string__string
    var inline339 int = 5
    var inline341 Result__string__string = t225(inline339)
    next__17 = inline341
    var t226 string
    var inline335 string = "missing"
    switch next__17.(type) {
    case Result__string__string_Ok:
        var inline336 string = next__17.(Result__string__string_Ok)._0
        t226 = inline336
    case Result__string__string_Err:
        t226 = inline335
    default:
        panic("non-exhaustive match")
    }
    var inline332 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t226)
    _goml_runtime_core_string_println(inline332)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__315 Option__int, map_fn__316 func(int) string) Option__string {
    switch self__315.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x174 int = self__315.(Option__int_Some)._0
        var t234 string = map_fn__316(x174)
        var t235 Option__string = Option__string_Some{
            _0: t234,
        }
        return t235
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__1 string) struct{} {
    var t237 string
    t237 = value__1
    _goml_runtime_core_string_println(t237)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__300 Option__string, fallback__301 string) string {
    switch self__300.(type) {
    case Option__string_None:
        return fallback__301
    case Option__string_Some:
        var x166 string = self__300.(Option__string_Some)._0
        return x166
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__318 Option__int, next__319 func(int) Option__string) Option__string {
    switch self__318.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x175 int = self__318.(Option__int_Some)._0
        var t247 Option__string = next__319(x175)
        return t247
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(self__321 Option__int, error__322 string) Result__int__string {
    switch self__321.(type) {
    case Option__int_None:
        var t252 Result__int__string = Result__int__string_Err{
            _0: error__322,
        }
        return t252
    case Option__int_Some:
        var x176 int = self__321.(Option__int_Some)._0
        var t253 Result__int__string = Result__int__string_Ok{
            _0: x176,
        }
        return t253
    default:
        panic("non-exhaustive match")
    }
}

func println__T_int(value__1 int) struct{} {
    var t255 string
    var inline371 string = _goml_runtime_core_int_to_string(value__1)
    t255 = inline371
    _goml_runtime_core_string_println(t255)
    return struct{}{}
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__311 Result__int__string, fallback__312 func(string) int) int {
    switch self__311.(type) {
    case Result__int__string_Ok:
        var x172 int = self__311.(Result__int__string_Ok)._0
        return x172
    case Result__int__string_Err:
        var x173 string = self__311.(Result__int__string_Err)._0
        var t264 int = fallback__312(x173)
        return t264
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__324 Result__int__string, map_fn__325 func(int) int) Result__int__string {
    switch self__324.(type) {
    case Result__int__string_Ok:
        var x177 int = self__324.(Result__int__string_Ok)._0
        var t269 int = map_fn__325(x177)
        var t270 Result__int__string = Result__int__string_Ok{
            _0: t269,
        }
        return t270
    case Result__int__string_Err:
        var x178 string = self__324.(Result__int__string_Err)._0
        var t271 Result__int__string = Result__int__string_Err{
            _0: x178,
        }
        return t271
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t302 string = _goml_runtime_core_int_to_string(self__67)
    return t302
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env194 closure_env_main_0, value__1 int) string {
    var inline373 string = _goml_runtime_core_int_to_string(value__1)
    return inline373
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env195 closure_env_main_1, value__3 int) string {
    var t308 string
    var inline375 string = _goml_runtime_core_int_to_string(value__3)
    t308 = inline375
    var t309 string = "static:" + t308
    return t309
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env196 closure_env_main_2, value__5 int) Option__string {
    var t312 string
    var inline377 string = _goml_runtime_core_int_to_string(value__5)
    t312 = inline377
    var t313 string = "value:" + t312
    var t314 Option__string = Option__string_Some{
        _0: t313,
    }
    return t314
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env197 closure_env_main_3, error__9 string) int {
    var inline379 int = _goml_runtime_core_string_len(error__9)
    return inline379
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env198 closure_env_main_4, value__11 int) int {
    var t320 int = value__11 + 2
    return t320
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env199 closure_env_main_5, value__13 string) int {
    var inline381 int = _goml_runtime_core_string_len(value__13)
    return inline381
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env200 closure_env_main_6, value__15 int) int {
    return value__15
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env201 closure_env_main_7, value__16 int) Result__string__string {
    var t328 string
    var inline383 string = _goml_runtime_core_int_to_string(value__16)
    t328 = inline383
    var t329 string = "next:" + t328
    var t330 Result__string__string = Result__string__string_Ok{
        _0: t329,
    }
    return t330
}

func main() {
    main0()
}
