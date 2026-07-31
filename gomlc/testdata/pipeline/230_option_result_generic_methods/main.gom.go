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
    var t168 closure_env_main_0 = closure_env_main_0{}
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t168, p0)
    })
    var t169 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t169)
    var t170 closure_env_main_1 = closure_env_main_1{}
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t170, p0)
    })
    var t171 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(static_mapped__4, "missing")
    println__T_string(t171)
    var t172 closure_env_main_2 = closure_env_main_2{}
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t172, p0)
    })
    var t173 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(chained__6, "missing")
    println__T_string(t173)
    var none__7 Option__int = Option__int_None{}
    var converted__8 Result__int__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(none__7, "none")
    var t174 closure_env_main_3 = closure_env_main_3{}
    var t175 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t174, p0)
    })
    println__T_int(t175)
    var ok__10 Result__int__string = Result__int__string_Ok{
        _0: 5,
    }
    var t176 closure_env_main_4 = closure_env_main_4{}
    var t177 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t176, p0)
    })
    var t178 int = _goml_m_inherent_i_Result_i_Re_h266fe7aa4d9cdaaf018f0ba861729c70_tring____T__int(t177, 0)
    println__T_int(t178)
    var error__12 Result__int__string = Result__int__string_Err{
        _0: "bad",
    }
    var t179 closure_env_main_5 = closure_env_main_5{}
    var mapped_error__14 Result__int__int = _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(error__12, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t179, p0)
    })
    var t180 closure_env_main_6 = closure_env_main_6{}
    var t181 int = _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(mapped_error__14, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t180, p0)
    })
    println__T_int(t181)
    var t182 closure_env_main_7 = closure_env_main_7{}
    var next__17 Result__string__string = _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(ok__10, func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t182, p0)
    })
    var t183 string = _goml_m_inherent_i_Result_i_Re_h142090784e44b30b8c35ba2616159d65_ng____T__string(next__17, "missing")
    println__T_string(t183)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv185 string
    var t186 string = _goml_runtime_core_int_to_string(self__5)
    retv185 = t186
    return retv185
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__280 Option__int, map_fn__281 func(int) string) Option__string {
    var retv188 Option__string
    var jp190 Option__string
    switch self__280.(type) {
    case Option__int_None:
        jp190 = Option__string_None{}
    case Option__int_Some:
        var x143 int = self__280.(Option__int_Some)._0
        var value__282 int = x143
        var t191 string = map_fn__281(value__282)
        var t192 Option__string = Option__string_Some{
            _0: t191,
        }
        jp190 = t192
    default:
        panic("non-exhaustive match")
    }
    retv188 = jp190
    return retv188
}

func println__T_string(value__1 string) struct{} {
    var t194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t194)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__265 Option__string, fallback__266 string) string {
    var retv197 string
    var jp199 string
    switch self__265.(type) {
    case Option__string_None:
        jp199 = fallback__266
    case Option__string_Some:
        var x135 string = self__265.(Option__string_Some)._0
        var value__267 string = x135
        jp199 = value__267
    default:
        panic("non-exhaustive match")
    }
    retv197 = jp199
    return retv197
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__283 Option__int, next__284 func(int) Option__string) Option__string {
    var retv201 Option__string
    var jp203 Option__string
    switch self__283.(type) {
    case Option__int_None:
        jp203 = Option__string_None{}
    case Option__int_Some:
        var x144 int = self__283.(Option__int_Some)._0
        var value__285 int = x144
        var t204 Option__string = next__284(value__285)
        jp203 = t204
    default:
        panic("non-exhaustive match")
    }
    retv201 = jp203
    return retv201
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(self__286 Option__int, error__287 string) Result__int__string {
    var retv206 Result__int__string
    var jp208 Result__int__string
    switch self__286.(type) {
    case Option__int_None:
        var t209 Result__int__string = Result__int__string_Err{
            _0: error__287,
        }
        jp208 = t209
    case Option__int_Some:
        var x145 int = self__286.(Option__int_Some)._0
        var value__288 int = x145
        var t210 Result__int__string = Result__int__string_Ok{
            _0: value__288,
        }
        jp208 = t210
    default:
        panic("non-exhaustive match")
    }
    retv206 = jp208
    return retv206
}

func println__T_int(value__1 int) struct{} {
    var t212 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t212)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv215 int
    var t216 int = _goml_runtime_core_string_len(self__8)
    retv215 = t216
    return retv215
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__276 Result__int__string, fallback__277 func(string) int) int {
    var retv218 int
    var jp220 int
    switch self__276.(type) {
    case Result__int__string_Ok:
        var x141 int = self__276.(Result__int__string_Ok)._0
        var value__278 int = x141
        jp220 = value__278
    case Result__int__string_Err:
        var x142 string = self__276.(Result__int__string_Err)._0
        var error__279 string = x142
        var t221 int = fallback__277(error__279)
        jp220 = t221
    default:
        panic("non-exhaustive match")
    }
    retv218 = jp220
    return retv218
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__289 Result__int__string, map_fn__290 func(int) int) Result__int__string {
    var retv223 Result__int__string
    var jp225 Result__int__string
    switch self__289.(type) {
    case Result__int__string_Ok:
        var x146 int = self__289.(Result__int__string_Ok)._0
        var value__291 int = x146
        var t226 int = map_fn__290(value__291)
        var t227 Result__int__string = Result__int__string_Ok{
            _0: t226,
        }
        jp225 = t227
    case Result__int__string_Err:
        var x147 string = self__289.(Result__int__string_Err)._0
        var error__292 string = x147
        var t228 Result__int__string = Result__int__string_Err{
            _0: error__292,
        }
        jp225 = t228
    default:
        panic("non-exhaustive match")
    }
    retv223 = jp225
    return retv223
}

func _goml_m_inherent_i_Result_i_Re_h266fe7aa4d9cdaaf018f0ba861729c70_tring____T__int(self__273 Result__int__string, fallback__274 int) int {
    var retv230 int
    var jp232 int
    switch self__273.(type) {
    case Result__int__string_Ok:
        var x139 int = self__273.(Result__int__string_Ok)._0
        var value__275 int = x139
        jp232 = value__275
    case Result__int__string_Err:
        jp232 = fallback__274
    default:
        panic("non-exhaustive match")
    }
    retv230 = jp232
    return retv230
}

func _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(self__293 Result__int__string, map_fn__294 func(string) int) Result__int__int {
    var retv234 Result__int__int
    var jp236 Result__int__int
    switch self__293.(type) {
    case Result__int__string_Ok:
        var x148 int = self__293.(Result__int__string_Ok)._0
        var value__295 int = x148
        var t237 Result__int__int = Result__int__int_Ok{
            _0: value__295,
        }
        jp236 = t237
    case Result__int__string_Err:
        var x149 string = self__293.(Result__int__string_Err)._0
        var error__296 string = x149
        var t238 int = map_fn__294(error__296)
        var t239 Result__int__int = Result__int__int_Err{
            _0: t238,
        }
        jp236 = t239
    default:
        panic("non-exhaustive match")
    }
    retv234 = jp236
    return retv234
}

func _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(self__276 Result__int__int, fallback__277 func(int) int) int {
    var retv241 int
    var jp243 int
    switch self__276.(type) {
    case Result__int__int_Ok:
        var x141 int = self__276.(Result__int__int_Ok)._0
        var value__278 int = x141
        jp243 = value__278
    case Result__int__int_Err:
        var x142 int = self__276.(Result__int__int_Err)._0
        var error__279 int = x142
        var t244 int = fallback__277(error__279)
        jp243 = t244
    default:
        panic("non-exhaustive match")
    }
    retv241 = jp243
    return retv241
}

func _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(self__297 Result__int__string, next__298 func(int) Result__string__string) Result__string__string {
    var retv246 Result__string__string
    var jp248 Result__string__string
    switch self__297.(type) {
    case Result__int__string_Ok:
        var x150 int = self__297.(Result__int__string_Ok)._0
        var value__299 int = x150
        var t249 Result__string__string = next__298(value__299)
        jp248 = t249
    case Result__int__string_Err:
        var x151 string = self__297.(Result__int__string_Err)._0
        var error__300 string = x151
        var t250 Result__string__string = Result__string__string_Err{
            _0: error__300,
        }
        jp248 = t250
    default:
        panic("non-exhaustive match")
    }
    retv246 = jp248
    return retv246
}

func _goml_m_inherent_i_Result_i_Re_h142090784e44b30b8c35ba2616159d65_ng____T__string(self__273 Result__string__string, fallback__274 string) string {
    var retv252 string
    var jp254 string
    switch self__273.(type) {
    case Result__string__string_Ok:
        var x139 string = self__273.(Result__string__string_Ok)._0
        var value__275 string = x139
        jp254 = value__275
    case Result__string__string_Err:
        jp254 = fallback__274
    default:
        panic("non-exhaustive match")
    }
    retv252 = jp254
    return retv252
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv256 string
    retv256 = self__38
    return retv256
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv258 string
    var t259 string = _goml_runtime_core_int_to_string(self__40)
    retv258 = t259
    return retv258
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env159 closure_env_main_0, value__1 int) string {
    var retv261 string
    var t262 string = _goml_m_inherent_i_int_i_int_i_to__string(value__1)
    retv261 = t262
    return retv261
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env160 closure_env_main_1, value__3 int) string {
    var retv264 string
    var t265 string = _goml_m_inherent_i_int_i_int_i_to__string(value__3)
    var t266 string = "static:" + t265
    retv264 = t266
    return retv264
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env161 closure_env_main_2, value__5 int) Option__string {
    var retv268 Option__string
    var t269 string = _goml_m_inherent_i_int_i_int_i_to__string(value__5)
    var t270 string = "value:" + t269
    var t271 Option__string = Option__string_Some{
        _0: t270,
    }
    retv268 = t271
    return retv268
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env162 closure_env_main_3, error__9 string) int {
    var retv273 int
    var t274 int = _goml_m_inherent_i_string_i_string_i_len(error__9)
    retv273 = t274
    return retv273
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env163 closure_env_main_4, value__11 int) int {
    var retv276 int
    var t277 int = value__11 + 2
    retv276 = t277
    return retv276
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env164 closure_env_main_5, value__13 string) int {
    var retv279 int
    var t280 int = _goml_m_inherent_i_string_i_string_i_len(value__13)
    retv279 = t280
    return retv279
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env165 closure_env_main_6, value__15 int) int {
    var retv282 int
    retv282 = value__15
    return retv282
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env166 closure_env_main_7, value__16 int) Result__string__string {
    var retv284 Result__string__string
    var t285 string = _goml_m_inherent_i_int_i_int_i_to__string(value__16)
    var t286 string = "next:" + t285
    var t287 Result__string__string = Result__string__string_Ok{
        _0: t286,
    }
    retv284 = t287
    return retv284
}

func main() {
    main0()
}
