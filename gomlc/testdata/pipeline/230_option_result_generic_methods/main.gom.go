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
    var t174 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(static_mapped__4, "missing")
    println__T_string(t174)
    var t175 closure_env_main_2 = closure_env_main_2{}
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t175, p0)
    })
    var t176 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(chained__6, "missing")
    println__T_string(t176)
    var none__7 Option__int = Option__int_None{}
    var converted__8 Result__int__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(none__7, "none")
    var t177 closure_env_main_3 = closure_env_main_3{}
    var t178 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t177, p0)
    })
    println__T_int(t178)
    var ok__10 Result__int__string = Result__int__string_Ok{
        _0: 5,
    }
    var t179 closure_env_main_4 = closure_env_main_4{}
    var t180 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t179, p0)
    })
    var t181 int = _goml_m_inherent_i_Result_i_Re_h266fe7aa4d9cdaaf018f0ba861729c70_tring____T__int(t180, 0)
    println__T_int(t181)
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
    println__T_int(t184)
    var t185 closure_env_main_7 = closure_env_main_7{}
    var next__17 Result__string__string = _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(ok__10, func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t185, p0)
    })
    var t186 string = _goml_m_inherent_i_Result_i_Re_h142090784e44b30b8c35ba2616159d65_ng____T__string(next__17, "missing")
    println__T_string(t186)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv188 string
    var t189 string = _goml_runtime_core_int_to_string(self__5)
    retv188 = t189
    return retv188
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__285 Option__int, map_fn__286 func(int) string) Option__string {
    var retv191 Option__string
    var jp193 Option__string
    switch self__285.(type) {
    case Option__int_None:
        jp193 = Option__string_None{}
    case Option__int_Some:
        var x146 int = self__285.(Option__int_Some)._0
        var value__287 int = x146
        var t194 string = map_fn__286(value__287)
        var t195 Option__string = Option__string_Some{
            _0: t194,
        }
        jp193 = t195
    default:
        panic("non-exhaustive match")
    }
    retv191 = jp193
    return retv191
}

func println__T_string(value__1 string) struct{} {
    var t197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t197)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__265 Option__string, fallback__266 string) string {
    var retv200 string
    var jp202 string
    switch self__265.(type) {
    case Option__string_None:
        jp202 = fallback__266
    case Option__string_Some:
        var x135 string = self__265.(Option__string_Some)._0
        var value__267 string = x135
        jp202 = value__267
    default:
        panic("non-exhaustive match")
    }
    retv200 = jp202
    return retv200
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__288 Option__int, next__289 func(int) Option__string) Option__string {
    var retv204 Option__string
    var jp206 Option__string
    switch self__288.(type) {
    case Option__int_None:
        jp206 = Option__string_None{}
    case Option__int_Some:
        var x147 int = self__288.(Option__int_Some)._0
        var value__290 int = x147
        var t207 Option__string = next__289(value__290)
        jp206 = t207
    default:
        panic("non-exhaustive match")
    }
    retv204 = jp206
    return retv204
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(self__291 Option__int, error__292 string) Result__int__string {
    var retv209 Result__int__string
    var jp211 Result__int__string
    switch self__291.(type) {
    case Option__int_None:
        var t212 Result__int__string = Result__int__string_Err{
            _0: error__292,
        }
        jp211 = t212
    case Option__int_Some:
        var x148 int = self__291.(Option__int_Some)._0
        var value__293 int = x148
        var t213 Result__int__string = Result__int__string_Ok{
            _0: value__293,
        }
        jp211 = t213
    default:
        panic("non-exhaustive match")
    }
    retv209 = jp211
    return retv209
}

func println__T_int(value__1 int) struct{} {
    var t215 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t215)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv218 int
    var t219 int = _goml_runtime_core_string_len(self__8)
    retv218 = t219
    return retv218
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__276 Result__int__string, fallback__277 func(string) int) int {
    var retv221 int
    var jp223 int
    switch self__276.(type) {
    case Result__int__string_Ok:
        var x141 int = self__276.(Result__int__string_Ok)._0
        var value__278 int = x141
        jp223 = value__278
    case Result__int__string_Err:
        var x142 string = self__276.(Result__int__string_Err)._0
        var error__279 string = x142
        var t224 int = fallback__277(error__279)
        jp223 = t224
    default:
        panic("non-exhaustive match")
    }
    retv221 = jp223
    return retv221
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__294 Result__int__string, map_fn__295 func(int) int) Result__int__string {
    var retv226 Result__int__string
    var jp228 Result__int__string
    switch self__294.(type) {
    case Result__int__string_Ok:
        var x149 int = self__294.(Result__int__string_Ok)._0
        var value__296 int = x149
        var t229 int = map_fn__295(value__296)
        var t230 Result__int__string = Result__int__string_Ok{
            _0: t229,
        }
        jp228 = t230
    case Result__int__string_Err:
        var x150 string = self__294.(Result__int__string_Err)._0
        var error__297 string = x150
        var t231 Result__int__string = Result__int__string_Err{
            _0: error__297,
        }
        jp228 = t231
    default:
        panic("non-exhaustive match")
    }
    retv226 = jp228
    return retv226
}

func _goml_m_inherent_i_Result_i_Re_h266fe7aa4d9cdaaf018f0ba861729c70_tring____T__int(self__273 Result__int__string, fallback__274 int) int {
    var retv233 int
    var jp235 int
    switch self__273.(type) {
    case Result__int__string_Ok:
        var x139 int = self__273.(Result__int__string_Ok)._0
        var value__275 int = x139
        jp235 = value__275
    case Result__int__string_Err:
        jp235 = fallback__274
    default:
        panic("non-exhaustive match")
    }
    retv233 = jp235
    return retv233
}

func _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(self__298 Result__int__string, map_fn__299 func(string) int) Result__int__int {
    var retv237 Result__int__int
    var jp239 Result__int__int
    switch self__298.(type) {
    case Result__int__string_Ok:
        var x151 int = self__298.(Result__int__string_Ok)._0
        var value__300 int = x151
        var t240 Result__int__int = Result__int__int_Ok{
            _0: value__300,
        }
        jp239 = t240
    case Result__int__string_Err:
        var x152 string = self__298.(Result__int__string_Err)._0
        var error__301 string = x152
        var t241 int = map_fn__299(error__301)
        var t242 Result__int__int = Result__int__int_Err{
            _0: t241,
        }
        jp239 = t242
    default:
        panic("non-exhaustive match")
    }
    retv237 = jp239
    return retv237
}

func _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(self__276 Result__int__int, fallback__277 func(int) int) int {
    var retv244 int
    var jp246 int
    switch self__276.(type) {
    case Result__int__int_Ok:
        var x141 int = self__276.(Result__int__int_Ok)._0
        var value__278 int = x141
        jp246 = value__278
    case Result__int__int_Err:
        var x142 int = self__276.(Result__int__int_Err)._0
        var error__279 int = x142
        var t247 int = fallback__277(error__279)
        jp246 = t247
    default:
        panic("non-exhaustive match")
    }
    retv244 = jp246
    return retv244
}

func _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(self__302 Result__int__string, next__303 func(int) Result__string__string) Result__string__string {
    var retv249 Result__string__string
    var jp251 Result__string__string
    switch self__302.(type) {
    case Result__int__string_Ok:
        var x153 int = self__302.(Result__int__string_Ok)._0
        var value__304 int = x153
        var t252 Result__string__string = next__303(value__304)
        jp251 = t252
    case Result__int__string_Err:
        var x154 string = self__302.(Result__int__string_Err)._0
        var error__305 string = x154
        var t253 Result__string__string = Result__string__string_Err{
            _0: error__305,
        }
        jp251 = t253
    default:
        panic("non-exhaustive match")
    }
    retv249 = jp251
    return retv249
}

func _goml_m_inherent_i_Result_i_Re_h142090784e44b30b8c35ba2616159d65_ng____T__string(self__273 Result__string__string, fallback__274 string) string {
    var retv255 string
    var jp257 string
    switch self__273.(type) {
    case Result__string__string_Ok:
        var x139 string = self__273.(Result__string__string_Ok)._0
        var value__275 string = x139
        jp257 = value__275
    case Result__string__string_Err:
        jp257 = fallback__274
    default:
        panic("non-exhaustive match")
    }
    retv255 = jp257
    return retv255
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv259 string
    retv259 = self__38
    return retv259
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv261 string
    var t262 string = _goml_runtime_core_int_to_string(self__40)
    retv261 = t262
    return retv261
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env162 closure_env_main_0, value__1 int) string {
    var retv264 string
    var t265 string = _goml_m_inherent_i_int_i_int_i_to__string(value__1)
    retv264 = t265
    return retv264
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env163 closure_env_main_1, value__3 int) string {
    var retv267 string
    var t268 string = _goml_m_inherent_i_int_i_int_i_to__string(value__3)
    var t269 string = "static:" + t268
    retv267 = t269
    return retv267
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env164 closure_env_main_2, value__5 int) Option__string {
    var retv271 Option__string
    var t272 string = _goml_m_inherent_i_int_i_int_i_to__string(value__5)
    var t273 string = "value:" + t272
    var t274 Option__string = Option__string_Some{
        _0: t273,
    }
    retv271 = t274
    return retv271
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env165 closure_env_main_3, error__9 string) int {
    var retv276 int
    var t277 int = _goml_m_inherent_i_string_i_string_i_len(error__9)
    retv276 = t277
    return retv276
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env166 closure_env_main_4, value__11 int) int {
    var retv279 int
    var t280 int = value__11 + 2
    retv279 = t280
    return retv279
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env167 closure_env_main_5, value__13 string) int {
    var retv282 int
    var t283 int = _goml_m_inherent_i_string_i_string_i_len(value__13)
    retv282 = t283
    return retv282
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env168 closure_env_main_6, value__15 int) int {
    var retv285 int
    retv285 = value__15
    return retv285
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env169 closure_env_main_7, value__16 int) Result__string__string {
    var retv287 Result__string__string
    var t288 string = _goml_m_inherent_i_int_i_int_i_to__string(value__16)
    var t289 string = "next:" + t288
    var t290 Result__string__string = Result__string__string_Ok{
        _0: t289,
    }
    retv287 = t290
    return retv287
}

func main() {
    main0()
}
