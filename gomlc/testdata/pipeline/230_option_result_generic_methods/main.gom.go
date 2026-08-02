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
    var t171 closure_env_main_0 = closure_env_main_0{}
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t171, p0)
    })
    var t172 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t172)
    var t173 closure_env_main_1 = closure_env_main_1{}
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t173, p0)
    })
    var t174 string
    var inline330 string = "missing"
    switch static_mapped__4.(type) {
    case Option__string_None:
        t174 = inline330
    case Option__string_Some:
        var inline331 string = static_mapped__4.(Option__string_Some)._0
        t174 = inline331
    default:
        panic("non-exhaustive match")
    }
    var inline327 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t174)
    _goml_runtime_core_string_println(inline327)
    var t175 closure_env_main_2 = closure_env_main_2{}
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t175, p0)
    })
    var t176 string
    var inline323 string = "missing"
    switch chained__6.(type) {
    case Option__string_None:
        t176 = inline323
    case Option__string_Some:
        var inline324 string = chained__6.(Option__string_Some)._0
        t176 = inline324
    default:
        panic("non-exhaustive match")
    }
    var inline320 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t176)
    _goml_runtime_core_string_println(inline320)
    var none__7 Option__int = Option__int_None{}
    var converted__8 Result__int__string
    var inline314 string = "none"
    switch none__7.(type) {
    case Option__int_None:
        var inline315 Result__int__string = Result__int__string_Err{
            _0: inline314,
        }
        converted__8 = inline315
    case Option__int_Some:
        var inline316 int = none__7.(Option__int_Some)._0
        var inline318 Result__int__string = Result__int__string_Ok{
            _0: inline316,
        }
        converted__8 = inline318
    default:
        panic("non-exhaustive match")
    }
    var t177 closure_env_main_3 = closure_env_main_3{}
    var t178 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t177, p0)
    })
    var inline311 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t178)
    _goml_runtime_core_string_println(inline311)
    var ok__10 Result__int__string = Result__int__string_Ok{
        _0: 5,
    }
    var t179 closure_env_main_4 = closure_env_main_4{}
    var t180 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t179, p0)
    })
    var t181 int
    var inline306 int = 0
    switch t180.(type) {
    case Result__int__string_Ok:
        var inline307 int = t180.(Result__int__string_Ok)._0
        t181 = inline307
    case Result__int__string_Err:
        t181 = inline306
    default:
        panic("non-exhaustive match")
    }
    var inline303 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t181)
    _goml_runtime_core_string_println(inline303)
    var error__12 Result__int__string = Result__int__string_Err{
        _0: "bad",
    }
    var t182 closure_env_main_5 = closure_env_main_5{}
    var mapped_error__14 Result__int__int = _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(error__12, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t182, p0)
    })
    var t183 closure_env_main_6 = closure_env_main_6{}
    var t184 int = _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(mapped_error__14, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t183, p0)
    })
    var inline300 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t184)
    _goml_runtime_core_string_println(inline300)
    var t185 closure_env_main_7 = closure_env_main_7{}
    var next__17 Result__string__string = _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(ok__10, func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t185, p0)
    })
    var t186 string
    var inline295 string = "missing"
    switch next__17.(type) {
    case Result__string__string_Ok:
        var inline296 string = next__17.(Result__string__string_Ok)._0
        t186 = inline296
    case Result__string__string_Err:
        t186 = inline295
    default:
        panic("non-exhaustive match")
    }
    var inline292 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline292)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__285 Option__int, map_fn__286 func(int) string) Option__string {
    switch self__285.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x146 int = self__285.(Option__int_Some)._0
        var t194 string = map_fn__286(x146)
        var t195 Option__string = Option__string_Some{
            _0: t194,
        }
        return t195
    default:
        panic("non-exhaustive match")
    }
}

func println__T_string(value__1 string) struct{} {
    var t197 string
    t197 = value__1
    _goml_runtime_core_string_println(t197)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__265 Option__string, fallback__266 string) string {
    switch self__265.(type) {
    case Option__string_None:
        return fallback__266
    case Option__string_Some:
        var x135 string = self__265.(Option__string_Some)._0
        return x135
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__288 Option__int, next__289 func(int) Option__string) Option__string {
    switch self__288.(type) {
    case Option__int_None:
        return Option__string_None{}
    case Option__int_Some:
        var x147 int = self__288.(Option__int_Some)._0
        var t207 Option__string = next__289(x147)
        return t207
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__276 Result__int__string, fallback__277 func(string) int) int {
    switch self__276.(type) {
    case Result__int__string_Ok:
        var x141 int = self__276.(Result__int__string_Ok)._0
        return x141
    case Result__int__string_Err:
        var x142 string = self__276.(Result__int__string_Err)._0
        var t224 int = fallback__277(x142)
        return t224
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__294 Result__int__string, map_fn__295 func(int) int) Result__int__string {
    switch self__294.(type) {
    case Result__int__string_Ok:
        var x149 int = self__294.(Result__int__string_Ok)._0
        var t229 int = map_fn__295(x149)
        var t230 Result__int__string = Result__int__string_Ok{
            _0: t229,
        }
        return t230
    case Result__int__string_Err:
        var x150 string = self__294.(Result__int__string_Err)._0
        var t231 Result__int__string = Result__int__string_Err{
            _0: x150,
        }
        return t231
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(self__298 Result__int__string, map_fn__299 func(string) int) Result__int__int {
    switch self__298.(type) {
    case Result__int__string_Ok:
        var x151 int = self__298.(Result__int__string_Ok)._0
        var t240 Result__int__int = Result__int__int_Ok{
            _0: x151,
        }
        return t240
    case Result__int__string_Err:
        var x152 string = self__298.(Result__int__string_Err)._0
        var t241 int = map_fn__299(x152)
        var t242 Result__int__int = Result__int__int_Err{
            _0: t241,
        }
        return t242
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(self__276 Result__int__int, fallback__277 func(int) int) int {
    switch self__276.(type) {
    case Result__int__int_Ok:
        var x141 int = self__276.(Result__int__int_Ok)._0
        return x141
    case Result__int__int_Err:
        var x142 int = self__276.(Result__int__int_Err)._0
        var t247 int = fallback__277(x142)
        return t247
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(self__302 Result__int__string, next__303 func(int) Result__string__string) Result__string__string {
    switch self__302.(type) {
    case Result__int__string_Ok:
        var x153 int = self__302.(Result__int__string_Ok)._0
        var t252 Result__string__string = next__303(x153)
        return t252
    case Result__int__string_Err:
        var x154 string = self__302.(Result__int__string_Err)._0
        var t253 Result__string__string = Result__string__string_Err{
            _0: x154,
        }
        return t253
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t262 string = _goml_runtime_core_int_to_string(self__40)
    return t262
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env162 closure_env_main_0, value__1 int) string {
    var inline337 string = _goml_runtime_core_int_to_string(value__1)
    return inline337
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env163 closure_env_main_1, value__3 int) string {
    var t268 string
    var inline339 string = _goml_runtime_core_int_to_string(value__3)
    t268 = inline339
    var t269 string = "static:" + t268
    return t269
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env164 closure_env_main_2, value__5 int) Option__string {
    var t272 string
    var inline341 string = _goml_runtime_core_int_to_string(value__5)
    t272 = inline341
    var t273 string = "value:" + t272
    var t274 Option__string = Option__string_Some{
        _0: t273,
    }
    return t274
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env165 closure_env_main_3, error__9 string) int {
    var inline343 int = _goml_runtime_core_string_len(error__9)
    return inline343
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env166 closure_env_main_4, value__11 int) int {
    var t280 int = value__11 + 2
    return t280
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env167 closure_env_main_5, value__13 string) int {
    var inline345 int = _goml_runtime_core_string_len(value__13)
    return inline345
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env168 closure_env_main_6, value__15 int) int {
    return value__15
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env169 closure_env_main_7, value__16 int) Result__string__string {
    var t288 string
    var inline347 string = _goml_runtime_core_int_to_string(value__16)
    t288 = inline347
    var t289 string = "next:" + t288
    var t290 Result__string__string = Result__string__string_Ok{
        _0: t289,
    }
    return t290
}

func main() {
    main0()
}
