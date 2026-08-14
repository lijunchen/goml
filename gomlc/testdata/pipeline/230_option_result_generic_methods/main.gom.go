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
    var t198 closure_env_main_0 = closure_env_main_0{}
    var t199 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t198, p0)
    }
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, t199)
    var t200 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t200)
    var t201 closure_env_main_1 = closure_env_main_1{}
    var t202 func(int) string = func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t201, p0)
    }
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, t202)
    var t203 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(static_mapped__4, "missing")
    println__T_string(t203)
    var t204 closure_env_main_2 = closure_env_main_2{}
    var t205 func(int) Option__string = func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t204, p0)
    }
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, t205)
    var t206 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(chained__6, "missing")
    println__T_string(t206)
    var none__7 Option__int = Option__int_None{}
    var converted__8 Result__int__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(none__7, "none")
    var t207 closure_env_main_3 = closure_env_main_3{}
    var t208 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t207, p0)
    }
    var t209 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, t208)
    println__T_int(t209)
    var ok__10 Result__int__string = Result__int__string_Ok{
        _0: 5,
    }
    var t210 closure_env_main_4 = closure_env_main_4{}
    var t211 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t210, p0)
    }
    var t212 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, t211)
    var t213 int
    var inline361 int = 0
    switch t212.(type) {
    case Result__int__string_Ok:
        var inline362 int = t212.(Result__int__string_Ok)._0
        t213 = inline362
    case Result__int__string_Err:
        t213 = inline361
    default:
        panic("non-exhaustive match")
    }
    var inline358 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t213)
    _goml_runtime_core_string_println(inline358)
    var t214 closure_env_main_5 = closure_env_main_5{}
    var t215 func(string) int = func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t214, p0)
    }
    var mapped_error__14 Result__int__int
    var inline353 string = "bad"
    var inline355 int = t215(inline353)
    var inline356 Result__int__int = Result__int__int_Err{
        _0: inline355,
    }
    mapped_error__14 = inline356
    var t216 closure_env_main_6 = closure_env_main_6{}
    var t217 func(int) int = func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t216, p0)
    }
    var t218 int
    switch mapped_error__14.(type) {
    case Result__int__int_Ok:
        var inline344 int = mapped_error__14.(Result__int__int_Ok)._0
        t218 = inline344
    case Result__int__int_Err:
        var inline346 int = mapped_error__14.(Result__int__int_Err)._0
        var inline348 int = t217(inline346)
        t218 = inline348
    default:
        panic("non-exhaustive match")
    }
    var inline341 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t218)
    _goml_runtime_core_string_println(inline341)
    var t219 closure_env_main_7 = closure_env_main_7{}
    var t220 func(int) Result__string__string = func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t219, p0)
    }
    var next__17 Result__string__string
    var inline334 int = 5
    var inline336 Result__string__string = t220(inline334)
    next__17 = inline336
    var t221 string
    var inline330 string = "missing"
    switch next__17.(type) {
    case Result__string__string_Ok:
        var inline331 string = next__17.(Result__string__string_Ok)._0
        t221 = inline331
    case Result__string__string_Err:
        t221 = inline330
    default:
        panic("non-exhaustive match")
    }
    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t221)
    _goml_runtime_core_string_println(inline327)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__312 Option__int, map_fn__313 func(int) string) Option__string {
    switch self__312.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x169 int = self__312.(Option__int_Some)._0
        var t229 string = map_fn__313(x169)
        var t230 Option__string = Option__string_Some{
            _0: t229,
        }
        return t230
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__1 string) struct{} {
    var t232 string
    t232 = value__1
    _goml_runtime_core_string_println(t232)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__297 Option__string, fallback__298 string) string {
    switch self__297.(type) {
    case Option__string_None:
        return fallback__298
    case Option__string_Some:
        var x161 string = self__297.(Option__string_Some)._0
        return x161
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__315 Option__int, next__316 func(int) Option__string) Option__string {
    switch self__315.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x170 int = self__315.(Option__int_Some)._0
        var t242 Option__string = next__316(x170)
        return t242
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(self__318 Option__int, error__319 string) Result__int__string {
    switch self__318.(type) {
    case Option__int_None:
        var t247 Result__int__string = Result__int__string_Err{
            _0: error__319,
        }
        return t247
    case Option__int_Some:
        var x171 int = self__318.(Option__int_Some)._0
        var t248 Result__int__string = Result__int__string_Ok{
            _0: x171,
        }
        return t248
    default:
        panic("non-exhaustive match")
    }
}

func println__T_int(value__1 int) struct{} {
    var t250 string
    var inline366 string = _goml_runtime_core_int_to_string(value__1)
    t250 = inline366
    _goml_runtime_core_string_println(t250)
    return struct{}{}
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__308 Result__int__string, fallback__309 func(string) int) int {
    switch self__308.(type) {
    case Result__int__string_Ok:
        var x167 int = self__308.(Result__int__string_Ok)._0
        return x167
    case Result__int__string_Err:
        var x168 string = self__308.(Result__int__string_Err)._0
        var t259 int = fallback__309(x168)
        return t259
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__321 Result__int__string, map_fn__322 func(int) int) Result__int__string {
    switch self__321.(type) {
    case Result__int__string_Ok:
        var x172 int = self__321.(Result__int__string_Ok)._0
        var t264 int = map_fn__322(x172)
        var t265 Result__int__string = Result__int__string_Ok{
            _0: t264,
        }
        return t265
    case Result__int__string_Err:
        var x173 string = self__321.(Result__int__string_Err)._0
        var t266 Result__int__string = Result__int__string_Err{
            _0: x173,
        }
        return t266
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t297 string = _goml_runtime_core_int_to_string(self__67)
    return t297
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env189 closure_env_main_0, value__1 int) string {
    var inline368 string = _goml_runtime_core_int_to_string(value__1)
    return inline368
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env190 closure_env_main_1, value__3 int) string {
    var t303 string
    var inline370 string = _goml_runtime_core_int_to_string(value__3)
    t303 = inline370
    var t304 string = "static:" + t303
    return t304
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env191 closure_env_main_2, value__5 int) Option__string {
    var t307 string
    var inline372 string = _goml_runtime_core_int_to_string(value__5)
    t307 = inline372
    var t308 string = "value:" + t307
    var t309 Option__string = Option__string_Some{
        _0: t308,
    }
    return t309
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env192 closure_env_main_3, error__9 string) int {
    var inline374 int = _goml_runtime_core_string_len(error__9)
    return inline374
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env193 closure_env_main_4, value__11 int) int {
    var t315 int = value__11 + 2
    return t315
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env194 closure_env_main_5, value__13 string) int {
    var inline376 int = _goml_runtime_core_string_len(value__13)
    return inline376
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env195 closure_env_main_6, value__15 int) int {
    return value__15
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env196 closure_env_main_7, value__16 int) Result__string__string {
    var t323 string
    var inline378 string = _goml_runtime_core_int_to_string(value__16)
    t323 = inline378
    var t324 string = "next:" + t323
    var t325 Result__string__string = Result__string__string_Ok{
        _0: t324,
    }
    return t325
}

func main() {
    main0()
}
