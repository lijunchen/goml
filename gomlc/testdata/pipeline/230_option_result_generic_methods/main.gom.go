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
    var t124 closure_env_main_0 = closure_env_main_0{}
    var mapped__2 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t124, p0)
    })
    var t125 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(mapped__2, "missing")
    println__T_string(t125)
    var t126 closure_env_main_1 = closure_env_main_1{}
    var static_mapped__4 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(some__0, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t126, p0)
    })
    var t127 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(static_mapped__4, "missing")
    println__T_string(t127)
    var t128 closure_env_main_2 = closure_env_main_2{}
    var chained__6 Option__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(some__0, func(p0 int) Option__string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t128, p0)
    })
    var t129 string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(chained__6, "missing")
    println__T_string(t129)
    var none__7 Option__int = Option__int_None{}
    var converted__8 Result__int__string = _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(none__7, "none")
    var t130 closure_env_main_3 = closure_env_main_3{}
    var t131 int = _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(converted__8, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t130, p0)
    })
    println__T_int(t131)
    var ok__10 Result__int__string = Result__int__string_Ok{
        _0: 5,
    }
    var t132 closure_env_main_4 = closure_env_main_4{}
    var t133 Result__int__string = _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(ok__10, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(t132, p0)
    })
    var t134 int = _goml_m_inherent_i_Result_i_Re_h266fe7aa4d9cdaaf018f0ba861729c70_tring____T__int(t133, 0)
    println__T_int(t134)
    var error__12 Result__int__string = Result__int__string_Err{
        _0: "bad",
    }
    var t135 closure_env_main_5 = closure_env_main_5{}
    var mapped_error__14 Result__int__int = _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(error__12, func(p0 string) int {
        return _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(t135, p0)
    })
    var t136 closure_env_main_6 = closure_env_main_6{}
    var t137 int = _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(mapped_error__14, func(p0 int) int {
        return _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(t136, p0)
    })
    println__T_int(t137)
    var t138 closure_env_main_7 = closure_env_main_7{}
    var next__17 Result__string__string = _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(ok__10, func(p0 int) Result__string__string {
        return _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(t138, p0)
    })
    var t139 string = _goml_m_inherent_i_Result_i_Re_h142090784e44b30b8c35ba2616159d65_ng____T__string(next__17, "missing")
    println__T_string(t139)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv141 string
    var t142 string = _goml_runtime_core_int_to_string(self__5)
    retv141 = t142
    return retv141
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_map____T__int____U__string(self__280 Option__int, map_fn__281 func(int) string) Option__string {
    var retv144 Option__string
    var jp146 Option__string
    switch self__280.(type) {
    case Option__int_None:
        jp146 = Option__string_None{}
    case Option__int_Some:
        var x99 int = self__280.(Option__int_Some)._0
        var value__282 int = x99
        var t147 string = map_fn__281(value__282)
        var t148 Option__string = Option__string_Some{
            _0: t147,
        }
        jp146 = t148
    default:
        panic("non-exhaustive match")
    }
    retv144 = jp146
    return retv144
}

func println__T_string(value__1 string) struct{} {
    var t150 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t150)
    return struct{}{}
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_unwrap__or____T__string(self__265 Option__string, fallback__266 string) string {
    var retv153 string
    var jp155 string
    switch self__265.(type) {
    case Option__string_None:
        jp155 = fallback__266
    case Option__string_Some:
        var x91 string = self__265.(Option__string_Some)._0
        var value__267 string = x91
        jp155 = value__267
    default:
        panic("non-exhaustive match")
    }
    retv153 = jp155
    return retv153
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_and__then____T__int____U__string(self__283 Option__int, next__284 func(int) Option__string) Option__string {
    var retv157 Option__string
    var jp159 Option__string
    switch self__283.(type) {
    case Option__int_None:
        jp159 = Option__string_None{}
    case Option__int_Some:
        var x100 int = self__283.(Option__int_Some)._0
        var value__285 int = x100
        var t160 Option__string = next__284(value__285)
        jp159 = t160
    default:
        panic("non-exhaustive match")
    }
    retv157 = jp159
    return retv157
}

func _goml_m_inherent_i_Option_i_Option_l_T_r__i_ok__or____E__string____T__int(self__286 Option__int, error__287 string) Result__int__string {
    var retv162 Result__int__string
    var jp164 Result__int__string
    switch self__286.(type) {
    case Option__int_None:
        var t165 Result__int__string = Result__int__string_Err{
            _0: error__287,
        }
        jp164 = t165
    case Option__int_Some:
        var x101 int = self__286.(Option__int_Some)._0
        var value__288 int = x101
        var t166 Result__int__string = Result__int__string_Ok{
            _0: value__288,
        }
        jp164 = t166
    default:
        panic("non-exhaustive match")
    }
    retv162 = jp164
    return retv162
}

func println__T_int(value__1 int) struct{} {
    var t168 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t168)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var retv171 int
    var t172 int = _goml_runtime_core_string_len(self__8)
    retv171 = t172
    return retv171
}

func _goml_m_inherent_i_Result_i_Re_hc96813df8abfc41fedd0a57a48dec607_tring____T__int(self__276 Result__int__string, fallback__277 func(string) int) int {
    var retv174 int
    var jp176 int
    switch self__276.(type) {
    case Result__int__string_Ok:
        var x97 int = self__276.(Result__int__string_Ok)._0
        var value__278 int = x97
        jp176 = value__278
    case Result__int__string_Err:
        var x98 string = self__276.(Result__int__string_Err)._0
        var error__279 string = x98
        var t177 int = fallback__277(error__279)
        jp176 = t177
    default:
        panic("non-exhaustive match")
    }
    retv174 = jp176
    return retv174
}

func _goml_m_inherent_i_Result_i_Re_h53d708ed89bfa167dab0055b53066fb7___int____U__int(self__289 Result__int__string, map_fn__290 func(int) int) Result__int__string {
    var retv179 Result__int__string
    var jp181 Result__int__string
    switch self__289.(type) {
    case Result__int__string_Ok:
        var x102 int = self__289.(Result__int__string_Ok)._0
        var value__291 int = x102
        var t182 int = map_fn__290(value__291)
        var t183 Result__int__string = Result__int__string_Ok{
            _0: t182,
        }
        jp181 = t183
    case Result__int__string_Err:
        var x103 string = self__289.(Result__int__string_Err)._0
        var error__292 string = x103
        var t184 Result__int__string = Result__int__string_Err{
            _0: error__292,
        }
        jp181 = t184
    default:
        panic("non-exhaustive match")
    }
    retv179 = jp181
    return retv179
}

func _goml_m_inherent_i_Result_i_Re_h266fe7aa4d9cdaaf018f0ba861729c70_tring____T__int(self__273 Result__int__string, fallback__274 int) int {
    var retv186 int
    var jp188 int
    switch self__273.(type) {
    case Result__int__string_Ok:
        var x95 int = self__273.(Result__int__string_Ok)._0
        var value__275 int = x95
        jp188 = value__275
    case Result__int__string_Err:
        jp188 = fallback__274
    default:
        panic("non-exhaustive match")
    }
    retv186 = jp188
    return retv186
}

func _goml_m_inherent_i_Result_i_Re_h03ce7a8acc541bce463170495d022e64___int____T__int(self__293 Result__int__string, map_fn__294 func(string) int) Result__int__int {
    var retv190 Result__int__int
    var jp192 Result__int__int
    switch self__293.(type) {
    case Result__int__string_Ok:
        var x104 int = self__293.(Result__int__string_Ok)._0
        var value__295 int = x104
        var t193 Result__int__int = Result__int__int_Ok{
            _0: value__295,
        }
        jp192 = t193
    case Result__int__string_Err:
        var x105 string = self__293.(Result__int__string_Err)._0
        var error__296 string = x105
        var t194 int = map_fn__294(error__296)
        var t195 Result__int__int = Result__int__int_Err{
            _0: t194,
        }
        jp192 = t195
    default:
        panic("non-exhaustive match")
    }
    retv190 = jp192
    return retv190
}

func _goml_m_inherent_i_Result_i_Re_h2418c806a6b88d5083ec4bf87e533749___int____T__int(self__276 Result__int__int, fallback__277 func(int) int) int {
    var retv197 int
    var jp199 int
    switch self__276.(type) {
    case Result__int__int_Ok:
        var x97 int = self__276.(Result__int__int_Ok)._0
        var value__278 int = x97
        jp199 = value__278
    case Result__int__int_Err:
        var x98 int = self__276.(Result__int__int_Err)._0
        var error__279 int = x98
        var t200 int = fallback__277(error__279)
        jp199 = t200
    default:
        panic("non-exhaustive match")
    }
    retv197 = jp199
    return retv197
}

func _goml_m_inherent_i_Result_i_Re_he0ddc48027611d42dc9ea3b49867094c_nt____U__string(self__297 Result__int__string, next__298 func(int) Result__string__string) Result__string__string {
    var retv202 Result__string__string
    var jp204 Result__string__string
    switch self__297.(type) {
    case Result__int__string_Ok:
        var x106 int = self__297.(Result__int__string_Ok)._0
        var value__299 int = x106
        var t205 Result__string__string = next__298(value__299)
        jp204 = t205
    case Result__int__string_Err:
        var x107 string = self__297.(Result__int__string_Err)._0
        var error__300 string = x107
        var t206 Result__string__string = Result__string__string_Err{
            _0: error__300,
        }
        jp204 = t206
    default:
        panic("non-exhaustive match")
    }
    retv202 = jp204
    return retv202
}

func _goml_m_inherent_i_Result_i_Re_h142090784e44b30b8c35ba2616159d65_ng____T__string(self__273 Result__string__string, fallback__274 string) string {
    var retv208 string
    var jp210 string
    switch self__273.(type) {
    case Result__string__string_Ok:
        var x95 string = self__273.(Result__string__string_Ok)._0
        var value__275 string = x95
        jp210 = value__275
    case Result__string__string_Err:
        jp210 = fallback__274
    default:
        panic("non-exhaustive match")
    }
    retv208 = jp210
    return retv208
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv212 string
    retv212 = self__38
    return retv212
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv214 string
    var t215 string = _goml_runtime_core_int_to_string(self__40)
    retv214 = t215
    return retv214
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env115 closure_env_main_0, value__1 int) string {
    var retv217 string
    var t218 string = _goml_m_inherent_i_int_i_int_i_to__string(value__1)
    retv217 = t218
    return retv217
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env116 closure_env_main_1, value__3 int) string {
    var retv220 string
    var t221 string = _goml_m_inherent_i_int_i_int_i_to__string(value__3)
    var t222 string = "static:" + t221
    retv220 = t222
    return retv220
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env117 closure_env_main_2, value__5 int) Option__string {
    var retv224 Option__string
    var t225 string = _goml_m_inherent_i_int_i_int_i_to__string(value__5)
    var t226 string = "value:" + t225
    var t227 Option__string = Option__string_Some{
        _0: t226,
    }
    retv224 = t227
    return retv224
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env118 closure_env_main_3, error__9 string) int {
    var retv229 int
    var t230 int = _goml_m_inherent_i_string_i_string_i_len(error__9)
    retv229 = t230
    return retv229
}

func _goml_m_inherent_i_closure__env__main__4_i_closure__env__main__4_i_apply(env119 closure_env_main_4, value__11 int) int {
    var retv232 int
    var t233 int = value__11 + 2
    retv232 = t233
    return retv232
}

func _goml_m_inherent_i_closure__env__main__5_i_closure__env__main__5_i_apply(env120 closure_env_main_5, value__13 string) int {
    var retv235 int
    var t236 int = _goml_m_inherent_i_string_i_string_i_len(value__13)
    retv235 = t236
    return retv235
}

func _goml_m_inherent_i_closure__env__main__6_i_closure__env__main__6_i_apply(env121 closure_env_main_6, value__15 int) int {
    var retv238 int
    retv238 = value__15
    return retv238
}

func _goml_m_inherent_i_closure__env__main__7_i_closure__env__main__7_i_apply(env122 closure_env_main_7, value__16 int) Result__string__string {
    var retv240 Result__string__string
    var t241 string = _goml_m_inherent_i_int_i_int_i_to__string(value__16)
    var t242 string = "next:" + t241
    var t243 Result__string__string = Result__string__string_Ok{
        _0: t242,
    }
    retv240 = t243
    return retv240
}

func main() {
    main0()
}
