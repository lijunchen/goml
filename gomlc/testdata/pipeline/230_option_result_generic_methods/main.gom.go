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
    var t193 closure_env_main_0 = closure_env_main_0{}
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t193, p0)
    })
    var t194 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t194)
    var t195 closure_env_main_1 = closure_env_main_1{}
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t195, p0)
    })
    var t196 string
    var inline352 string = "missing"
    switch static_mapped__4.(type) {
    case Option__string_None:
        t196 = inline352
    case Option__string_Some:
        var inline353 string = static_mapped__4.(Option__string_Some)._0
        t196 = inline353
    default:
        panic("non-exhaustive match")
    }
    var inline349 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline349)
    var t197 closure_env_main_2 = closure_env_main_2{}
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t197, p0)
    })
    var t198 string
    var inline345 string = "missing"
    switch chained__6.(type) {
    case Option__string_None:
        t198 = inline345
    case Option__string_Some:
        var inline346 string = chained__6.(Option__string_Some)._0
        t198 = inline346
    default:
        panic("non-exhaustive match")
    }
    var inline342 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline342)
    var none__7 Option__int = Option__int_None{}
    var converted__8 Result__int__string
    var inline336 string = "none"
    switch none__7.(type) {
    case Option__int_None:
        var inline337 Result__int__string = Result__int__string_Err{
            _0: inline336,
        }
        converted__8 = inline337
    case Option__int_Some:
        var inline338 int = none__7.(Option__int_Some)._0
        var inline340 Result__int__string = Result__int__string_Ok{
            _0: inline338,
        }
        converted__8 = inline340
    default:
        panic("non-exhaustive match")
    }
    var t199 closure_env_main_3 = closure_env_main_3{}
    var t200 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t199, p0)
    })
    var inline333 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t200)
    _goml_runtime_core_string_println(inline333)
    var ok__10 Result__int__string = Result__int__string_Ok{
        _0: 5,
    }
    var t201 closure_env_main_4 = closure_env_main_4{}
    var t202 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t201, p0)
    })
    var t203 int
    var inline328 int = 0
    switch t202.(type) {
    case Result__int__string_Ok:
        var inline329 int = t202.(Result__int__string_Ok)._0
        t203 = inline329
    case Result__int__string_Err:
        t203 = inline328
    default:
        panic("non-exhaustive match")
    }
    var inline325 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t203)
    _goml_runtime_core_string_println(inline325)
    var error__12 Result__int__string = Result__int__string_Err{
        _0: "bad",
    }
    var t204 closure_env_main_5 = closure_env_main_5{}
    var mapped_error__14 Result__int__int = _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(error__12, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t204, p0)
    })
    var t205 closure_env_main_6 = closure_env_main_6{}
    var t206 int = _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(mapped_error__14, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t205, p0)
    })
    var inline322 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t206)
    _goml_runtime_core_string_println(inline322)
    var t207 closure_env_main_7 = closure_env_main_7{}
    var next__17 Result__string__string = _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(ok__10, func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t207, p0)
    })
    var t208 string
    var inline317 string = "missing"
    switch next__17.(type) {
    case Result__string__string_Ok:
        var inline318 string = next__17.(Result__string__string_Ok)._0
        t208 = inline318
    case Result__string__string_Err:
        t208 = inline317
    default:
        panic("non-exhaustive match")
    }
    var inline314 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline314)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__314 Option__int, map_fn__315 func(int) string) Option__string {
    switch self__314.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x168 int = self__314.(Option__int_Some)._0
        var t216 string = map_fn__315(x168)
        var t217 Option__string = Option__string_Some{
            _0: t216,
        }
        return t217
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__31 string) struct{} {
    var t219 string
    t219 = value__31
    _goml_runtime_core_string_println(t219)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__294 Option__string, fallback__295 string) string {
    switch self__294.(type) {
    case Option__string_None:
        return fallback__295
    case Option__string_Some:
        var x157 string = self__294.(Option__string_Some)._0
        return x157
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__317 Option__int, next__318 func(int) Option__string) Option__string {
    switch self__317.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x169 int = self__317.(Option__int_Some)._0
        var t229 Option__string = next__318(x169)
        return t229
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__305 Result__int__string, fallback__306 func(string) int) int {
    switch self__305.(type) {
    case Result__int__string_Ok:
        var x163 int = self__305.(Result__int__string_Ok)._0
        return x163
    case Result__int__string_Err:
        var x164 string = self__305.(Result__int__string_Err)._0
        var t246 int = fallback__306(x164)
        return t246
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__323 Result__int__string, map_fn__324 func(int) int) Result__int__string {
    switch self__323.(type) {
    case Result__int__string_Ok:
        var x171 int = self__323.(Result__int__string_Ok)._0
        var t251 int = map_fn__324(x171)
        var t252 Result__int__string = Result__int__string_Ok{
            _0: t251,
        }
        return t252
    case Result__int__string_Err:
        var x172 string = self__323.(Result__int__string_Err)._0
        var t253 Result__int__string = Result__int__string_Err{
            _0: x172,
        }
        return t253
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(self__327 Result__int__string, map_fn__328 func(string) int) Result__int__int {
    switch self__327.(type) {
    case Result__int__string_Ok:
        var x173 int = self__327.(Result__int__string_Ok)._0
        var t262 Result__int__int = Result__int__int_Ok{
            _0: x173,
        }
        return t262
    case Result__int__string_Err:
        var x174 string = self__327.(Result__int__string_Err)._0
        var t263 int = map_fn__328(x174)
        var t264 Result__int__int = Result__int__int_Err{
            _0: t263,
        }
        return t264
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(self__305 Result__int__int, fallback__306 func(int) int) int {
    switch self__305.(type) {
    case Result__int__int_Ok:
        var x163 int = self__305.(Result__int__int_Ok)._0
        return x163
    case Result__int__int_Err:
        var x164 int = self__305.(Result__int__int_Err)._0
        var t269 int = fallback__306(x164)
        return t269
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(self__331 Result__int__string, next__332 func(int) Result__string__string) Result__string__string {
    switch self__331.(type) {
    case Result__int__string_Ok:
        var x175 int = self__331.(Result__int__string_Ok)._0
        var t274 Result__string__string = next__332(x175)
        return t274
    case Result__int__string_Err:
        var x176 string = self__331.(Result__int__string_Err)._0
        var t275 Result__string__string = Result__string__string_Err{
            _0: x176,
        }
        return t275
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t284 string = _goml_runtime_core_int_to_string(self__69)
    return t284
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env184 closure_env_main_0, value__1 int) string {
    var inline359 string = _goml_runtime_core_int_to_string(value__1)
    return inline359
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env185 closure_env_main_1, value__3 int) string {
    var t290 string
    var inline361 string = _goml_runtime_core_int_to_string(value__3)
    t290 = inline361
    var t291 string = "static:" + t290
    return t291
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env186 closure_env_main_2, value__5 int) Option__string {
    var t294 string
    var inline363 string = _goml_runtime_core_int_to_string(value__5)
    t294 = inline363
    var t295 string = "value:" + t294
    var t296 Option__string = Option__string_Some{
        _0: t295,
    }
    return t296
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env187 closure_env_main_3, error__9 string) int {
    var inline365 int = _goml_runtime_core_string_len(error__9)
    return inline365
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env188 closure_env_main_4, value__11 int) int {
    var t302 int = value__11 + 2
    return t302
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env189 closure_env_main_5, value__13 string) int {
    var inline367 int = _goml_runtime_core_string_len(value__13)
    return inline367
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env190 closure_env_main_6, value__15 int) int {
    return value__15
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env191 closure_env_main_7, value__16 int) Result__string__string {
    var t310 string
    var inline369 string = _goml_runtime_core_int_to_string(value__16)
    t310 = inline369
    var t311 string = "next:" + t310
    var t312 Result__string__string = Result__string__string_Ok{
        _0: t311,
    }
    return t312
}

func main() {
    main0()
}
