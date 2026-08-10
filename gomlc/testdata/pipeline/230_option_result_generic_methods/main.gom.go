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
    var t189 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t188, p0)
    }
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, t189)
    var t190 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t190)
    var t191 closure_env_main_1 = closure_env_main_1{}
    var t192 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t191, p0)
    }
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, t192)
    var t193 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(static_mapped__4, "missing")
    println__T_string(t193)
    var t194 closure_env_main_2 = closure_env_main_2{}
    var t195 func(int) Option__string = func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t194, p0)
    }
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, t195)
    var t196 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(chained__6, "missing")
    println__T_string(t196)
    var none__7 Option__int = Option__int_None{}
    var converted__8 Result__int__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(none__7, "none")
    var t197 closure_env_main_3 = closure_env_main_3{}
    var t198 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t197, p0)
    }
    var t199 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, t198)
    println__T_int(t199)
    var ok__10 Result__int__string = Result__int__string_Ok{
        _0: 5,
    }
    var t200 closure_env_main_4 = closure_env_main_4{}
    var t201 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t200, p0)
    }
    var t202 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, t201)
    var t203 int
    var inline351 int = 0
    switch t202.(type) {
    case Result__int__string_Ok:
        var inline352 int = t202.(Result__int__string_Ok)._0
        t203 = inline352
    case Result__int__string_Err:
        t203 = inline351
    default:
        panic("non-exhaustive match")
    }
    var inline348 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t203)
    _goml_runtime_core_string_println(inline348)
    var t204 closure_env_main_5 = closure_env_main_5{}
    var t205 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t204, p0)
    }
    var mapped_error__14 Result__int__int
    var inline343 string = "bad"
    var inline345 int = t205(inline343)
    var inline346 Result__int__int = Result__int__int_Err{
        _0: inline345,
    }
    mapped_error__14 = inline346
    var t206 closure_env_main_6 = closure_env_main_6{}
    var t207 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t206, p0)
    }
    var t208 int
    switch mapped_error__14.(type) {
    case Result__int__int_Ok:
        var inline334 int = mapped_error__14.(Result__int__int_Ok)._0
        t208 = inline334
    case Result__int__int_Err:
        var inline336 int = mapped_error__14.(Result__int__int_Err)._0
        var inline338 int = t207(inline336)
        t208 = inline338
    default:
        panic("non-exhaustive match")
    }
    var inline331 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t208)
    _goml_runtime_core_string_println(inline331)
    var t209 closure_env_main_7 = closure_env_main_7{}
    var t210 func(int) Result__string__string = func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t209, p0)
    }
    var next__17 Result__string__string
    var inline324 int = 5
    var inline326 Result__string__string = t210(inline324)
    next__17 = inline326
    var t211 string
    var inline320 string = "missing"
    switch next__17.(type) {
    case Result__string__string_Ok:
        var inline321 string = next__17.(Result__string__string_Ok)._0
        t211 = inline321
    case Result__string__string_Err:
        t211 = inline320
    default:
        panic("non-exhaustive match")
    }
    var inline317 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline317)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__297 Option__int, map_fn__298 func(int) string) Option__string {
    switch self__297.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x159 int = self__297.(Option__int_Some)._0
        var t219 string = map_fn__298(x159)
        var t220 Option__string = Option__string_Some{
            _0: t219,
        }
        return t220
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__1 string) struct{} {
    var t222 string
    t222 = value__1
    _goml_runtime_core_string_println(t222)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__282 Option__string, fallback__283 string) string {
    switch self__282.(type) {
    case Option__string_None:
        return fallback__283
    case Option__string_Some:
        var x151 string = self__282.(Option__string_Some)._0
        return x151
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__300 Option__int, next__301 func(int) Option__string) Option__string {
    switch self__300.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x160 int = self__300.(Option__int_Some)._0
        var t232 Option__string = next__301(x160)
        return t232
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(self__303 Option__int, error__304 string) Result__int__string {
    switch self__303.(type) {
    case Option__int_None:
        var t237 Result__int__string = Result__int__string_Err{
            _0: error__304,
        }
        return t237
    case Option__int_Some:
        var x161 int = self__303.(Option__int_Some)._0
        var t238 Result__int__string = Result__int__string_Ok{
            _0: x161,
        }
        return t238
    default:
        panic("non-exhaustive match")
    }
}

func println__T_int(value__1 int) struct{} {
    var t240 string
    var inline356 string = _goml_runtime_core_int_to_string(value__1)
    t240 = inline356
    _goml_runtime_core_string_println(t240)
    return struct{}{}
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__293 Result__int__string, fallback__294 func(string) int) int {
    switch self__293.(type) {
    case Result__int__string_Ok:
        var x157 int = self__293.(Result__int__string_Ok)._0
        return x157
    case Result__int__string_Err:
        var x158 string = self__293.(Result__int__string_Err)._0
        var t249 int = fallback__294(x158)
        return t249
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__306 Result__int__string, map_fn__307 func(int) int) Result__int__string {
    switch self__306.(type) {
    case Result__int__string_Ok:
        var x162 int = self__306.(Result__int__string_Ok)._0
        var t254 int = map_fn__307(x162)
        var t255 Result__int__string = Result__int__string_Ok{
            _0: t254,
        }
        return t255
    case Result__int__string_Err:
        var x163 string = self__306.(Result__int__string_Err)._0
        var t256 Result__int__string = Result__int__string_Err{
            _0: x163,
        }
        return t256
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t287 string = _goml_runtime_core_int_to_string(self__67)
    return t287
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env179 closure_env_main_0, value__1 int) string {
    var inline358 string = _goml_runtime_core_int_to_string(value__1)
    return inline358
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env180 closure_env_main_1, value__3 int) string {
    var t293 string
    var inline360 string = _goml_runtime_core_int_to_string(value__3)
    t293 = inline360
    var t294 string = "static:" + t293
    return t294
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env181 closure_env_main_2, value__5 int) Option__string {
    var t297 string
    var inline362 string = _goml_runtime_core_int_to_string(value__5)
    t297 = inline362
    var t298 string = "value:" + t297
    var t299 Option__string = Option__string_Some{
        _0: t298,
    }
    return t299
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env182 closure_env_main_3, error__9 string) int {
    var inline364 int = _goml_runtime_core_string_len(error__9)
    return inline364
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env183 closure_env_main_4, value__11 int) int {
    var t305 int = value__11 + 2
    return t305
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env184 closure_env_main_5, value__13 string) int {
    var inline366 int = _goml_runtime_core_string_len(value__13)
    return inline366
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env185 closure_env_main_6, value__15 int) int {
    return value__15
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env186 closure_env_main_7, value__16 int) Result__string__string {
    var t313 string
    var inline368 string = _goml_runtime_core_int_to_string(value__16)
    t313 = inline368
    var t314 string = "next:" + t313
    var t315 Result__string__string = Result__string__string_Ok{
        _0: t314,
    }
    return t315
}

func main() {
    main0()
}
