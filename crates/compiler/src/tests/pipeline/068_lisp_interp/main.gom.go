package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func string_len(s string) int32 {
    return int32(len(s))
}

func string_get(s string, i int32) rune {
    return rune(s[i])
}

func char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_5int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_string_x struct {
    value string
}

func ref__Ref_6string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_6string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_6string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5Token_x struct {
    value []Token
}

func ref__Ref_10Vec_5Token(value []Token) *ref_Vec_5Token_x {
    return &ref_Vec_5Token_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Token(reference *ref_Vec_5Token_x) []Token {
    return reference.value
}

func ref_set__Ref_10Vec_5Token(reference *ref_Vec_5Token_x, value []Token) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Value_x struct {
    value Value
}

func ref__Ref_5Value(value Value) *ref_Value_x {
    return &ref_Value_x{
        value: value,
    }
}

func ref_get__Ref_5Value(reference *ref_Value_x) Value {
    return reference.value
}

func ref_set__Ref_5Value(reference *ref_Value_x, value Value) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5SExpr_x struct {
    value []SExpr
}

func ref__Ref_10Vec_5SExpr(value []SExpr) *ref_Vec_5SExpr_x {
    return &ref_Vec_5SExpr_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x) []SExpr {
    return reference.value
}

func ref_set__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x, value []SExpr) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_7Binding_x struct {
    value []Binding
}

func ref__Ref_12Vec_7Binding(value []Binding) *ref_Vec_7Binding_x {
    return &ref_Vec_7Binding_x{
        value: value,
    }
}

func ref_get__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x) []Binding {
    return reference.value
}

func ref_set__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x, value []Binding) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_6string_x struct {
    value []string
}

func ref__Ref_11Vec_6string(value []string) *ref_Vec_6string_x {
    return &ref_Vec_6string_x{
        value: value,
    }
}

func ref_get__Ref_11Vec_6string(reference *ref_Vec_6string_x) []string {
    return reference.value
}

func ref_set__Ref_11Vec_6string(reference *ref_Vec_6string_x, value []string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5Value_x struct {
    value []Value
}

func ref__Ref_10Vec_5Value(value []Value) *ref_Vec_5Value_x {
    return &ref_Vec_5Value_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Value(reference *ref_Vec_5Value_x) []Value {
    return reference.value
}

func ref_set__Ref_10Vec_5Value(reference *ref_Vec_5Value_x, value []Value) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_5Token_5int32 struct {
    _0 Token
    _1 int32
}

type Tuple2_10Vec_5SExpr_5int32 struct {
    _0 []SExpr
    _1 int32
}

type Tuple2_5SExpr_5int32 struct {
    _0 SExpr
    _1 int32
}

type Tuple2_5Value_5Value struct {
    _0 Value
    _1 Value
}

type Binding struct {
    name string
    value Value
}

type Lambda struct {
    params []string
    body SExpr
    env []Binding
    global *ref_Vec_7Binding_x
}

type Token interface {
    isToken()
}

type LParen struct {}

func (_ LParen) isToken() {}

type RParen struct {}

func (_ RParen) isToken() {}

type Token_Sym struct {
    _0 string
}

func (_ Token_Sym) isToken() {}

type Token_Int struct {
    _0 int32
}

func (_ Token_Int) isToken() {}

type Token_Bool struct {
    _0 bool
}

func (_ Token_Bool) isToken() {}

type Value interface {
    isValue()
}

type Value_Int struct {
    _0 int32
}

func (_ Value_Int) isValue() {}

type Value_Bool struct {
    _0 bool
}

func (_ Value_Bool) isValue() {}

type Func struct {
    _0 Lambda
}

func (_ Func) isValue() {}

type Nil struct {}

func (_ Nil) isValue() {}

type SExpr interface {
    isSExpr()
}

type SExpr_Int struct {
    _0 int32
}

func (_ SExpr_Int) isSExpr() {}

type SExpr_Bool struct {
    _0 bool
}

func (_ SExpr_Bool) isSExpr() {}

type SExpr_Sym struct {
    _0 string
}

func (_ SExpr_Sym) isSExpr() {}

type List struct {
    _0 []SExpr
}

func (_ List) isSExpr() {}

type GoError = error

func is_digit(ch__0 rune) bool {
    var retv149 bool
    var t150 bool = ch__0 >= 48
    var t151 bool = ch__0 <= 57
    var t152 bool = t150 && t151
    retv149 = t152
    return retv149
}

func digit_value(ch__1 rune) int32 {
    var retv154 int32
    var jp156 int32
    switch ch__1 {
    case 48:
        jp156 = 0
    case 49:
        jp156 = 1
    case 50:
        jp156 = 2
    case 51:
        jp156 = 3
    case 52:
        jp156 = 4
    case 53:
        jp156 = 5
    case 54:
        jp156 = 6
    case 55:
        jp156 = 7
    case 56:
        jp156 = 8
    case 57:
        jp156 = 9
    default:
        jp156 = 0
    }
    retv154 = jp156
    return retv154
}

func is_int_text(text__2 string) bool {
    var retv158 bool
    var len__3 int32 = string_len(text__2)
    var t161 bool = len__3 == 0
    var jp160 bool
    if t161 {
        jp160 = false
        retv158 = jp160
        return retv158
    } else {
        var i__4 *ref_int32_x = ref__Ref_5int32(0)
        var saw_digit__5 *ref_bool_x = ref__Ref_4bool(false)
        var ok__6 *ref_bool_x = ref__Ref_4bool(true)
        var started__7 *ref_bool_x = ref__Ref_4bool(false)
        Loop_loop166:
        for {
            var t167 bool = ref_get__Ref_4bool(ok__6)
            var t168 int32 = ref_get__Ref_5int32(i__4)
            var t169 bool = t168 < len__3
            var t170 bool = t167 && t169
            if t170 {
                var t171 int32 = ref_get__Ref_5int32(i__4)
                var ch__8 rune = string_get(text__2, t171)
                var t173 bool = ref_get__Ref_4bool(started__7)
                var t174 bool = !t173
                var t175 bool = ch__8 == 45
                var t176 bool = t174 && t175
                if t176 {
                    ref_set__Ref_4bool(started__7, true)
                    var t177 int32 = ref_get__Ref_5int32(i__4)
                    var t178 int32 = t177 + 1
                    ref_set__Ref_5int32(i__4, t178)
                } else {
                    var t181 bool = is_digit(ch__8)
                    if t181 {
                        ref_set__Ref_4bool(started__7, true)
                        ref_set__Ref_4bool(saw_digit__5, true)
                        var t182 int32 = ref_get__Ref_5int32(i__4)
                        var t183 int32 = t182 + 1
                        ref_set__Ref_5int32(i__4, t183)
                    } else {
                        ref_set__Ref_4bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop166
            }
        }
        var t163 bool = ref_get__Ref_4bool(ok__6)
        var t164 bool = ref_get__Ref_4bool(saw_digit__5)
        var t165 bool = t163 && t164
        jp160 = t165
        retv158 = jp160
        return retv158
    }
}

func parse_int32(text__9 string) int32 {
    var retv187 int32
    var len__10 int32 = string_len(text__9)
    var i__11 *ref_int32_x = ref__Ref_5int32(0)
    var negative__12 *ref_bool_x = ref__Ref_4bool(false)
    var started__13 *ref_bool_x = ref__Ref_4bool(false)
    var acc__14 *ref_int32_x = ref__Ref_5int32(0)
    Loop_loop195:
    for {
        var t196 int32 = ref_get__Ref_5int32(i__11)
        var t197 bool = t196 < len__10
        if t197 {
            var t198 int32 = ref_get__Ref_5int32(i__11)
            var ch__15 rune = string_get(text__9, t198)
            var t200 bool = ref_get__Ref_4bool(started__13)
            var t201 bool = !t200
            var t202 bool = ch__15 == 45
            var t203 bool = t201 && t202
            if t203 {
                ref_set__Ref_4bool(started__13, true)
                ref_set__Ref_4bool(negative__12, true)
                var t204 int32 = ref_get__Ref_5int32(i__11)
                var t205 int32 = t204 + 1
                ref_set__Ref_5int32(i__11, t205)
            } else {
                ref_set__Ref_4bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t207 int32 = ref_get__Ref_5int32(acc__14)
                var t208 int32 = t207 * 10
                var t209 int32 = t208 + d__16
                ref_set__Ref_5int32(acc__14, t209)
                var t210 int32 = ref_get__Ref_5int32(i__11)
                var t211 int32 = t210 + 1
                ref_set__Ref_5int32(i__11, t211)
            }
            continue
        } else {
            break Loop_loop195
        }
    }
    var t191 bool = ref_get__Ref_4bool(negative__12)
    var jp190 int32
    if t191 {
        var t192 int32 = ref_get__Ref_5int32(acc__14)
        var t193 int32 = 0 - t192
        jp190 = t193
    } else {
        var t194 int32 = ref_get__Ref_5int32(acc__14)
        jp190 = t194
    }
    retv187 = jp190
    return retv187
}

func is_delim(ch__17 rune) bool {
    var retv214 bool
    var t215 bool = ch__17 == 40
    var t216 bool = ch__17 == 41
    var t217 bool = t215 || t216
    var t218 bool = ch__17 == 32
    var t219 bool = t217 || t218
    retv214 = t219
    return retv214
}

func lex_atom(source__18 string, start__19 int32) Tuple2_5Token_5int32 {
    var retv221 Tuple2_5Token_5int32
    var len__20 int32 = string_len(source__18)
    var text__21 *ref_string_x = ref__Ref_6string("")
    var i__22 *ref_int32_x = ref__Ref_5int32(start__19)
    var done__23 *ref_bool_x = ref__Ref_4bool(false)
    Loop_loop235:
    for {
        var t236 bool = ref_get__Ref_4bool(done__23)
        var t237 bool = !t236
        var t238 int32 = ref_get__Ref_5int32(i__22)
        var t239 bool = t238 < len__20
        var t240 bool = t237 && t239
        if t240 {
            var t241 int32 = ref_get__Ref_5int32(i__22)
            var ch__24 rune = string_get(source__18, t241)
            var t243 bool = is_delim(ch__24)
            if t243 {
                ref_set__Ref_4bool(done__23, true)
            } else {
                var t245 string = ref_get__Ref_6string(text__21)
                var t246 string = char_to_string(ch__24)
                var t247 string = t245 + t246
                ref_set__Ref_6string(text__21, t247)
                var t248 int32 = ref_get__Ref_5int32(i__22)
                var t249 int32 = t248 + 1
                ref_set__Ref_5int32(i__22, t249)
            }
            continue
        } else {
            break Loop_loop235
        }
    }
    var atom__25 string = ref_get__Ref_6string(text__21)
    var jp224 Token
    switch atom__25 {
    case "true":
        var t227 Token = Token_Bool{
            _0: true,
        }
        jp224 = t227
    case "false":
        var t228 Token = Token_Bool{
            _0: false,
        }
        jp224 = t228
    default:
        var t231 bool = is_int_text(atom__25)
        var jp230 Token
        if t231 {
            var t232 int32 = parse_int32(atom__25)
            var t233 Token = Token_Int{
                _0: t232,
            }
            jp230 = t233
        } else {
            var t234 Token = Token_Sym{
                _0: atom__25,
            }
            jp230 = t234
        }
        jp224 = jp230
    }
    var token__26 Token = jp224
    var t225 int32 = ref_get__Ref_5int32(i__22)
    var t226 Tuple2_5Token_5int32 = Tuple2_5Token_5int32{
        _0: token__26,
        _1: t225,
    }
    retv221 = t226
    return retv221
}

func lex(source__27 string) []Token {
    var retv252 []Token
    var len__28 int32 = string_len(source__27)
    var toks0__29 []Token = nil
    var toks__30 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    var i__31 *ref_int32_x = ref__Ref_5int32(0)
    Loop_loop255:
    for {
        var t256 int32 = ref_get__Ref_5int32(i__31)
        var t257 bool = t256 < len__28
        if t257 {
            var t258 int32 = ref_get__Ref_5int32(i__31)
            var ch__32 rune = string_get(source__27, t258)
            var t260 bool = ch__32 == 40
            if t260 {
                var t261 []Token = ref_get__Ref_10Vec_5Token(toks__30)
                var t262 []Token = append(t261, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t262)
                var t263 int32 = ref_get__Ref_5int32(i__31)
                var t264 int32 = t263 + 1
                ref_set__Ref_5int32(i__31, t264)
            } else {
                var t267 bool = ch__32 == 41
                if t267 {
                    var t268 []Token = ref_get__Ref_10Vec_5Token(toks__30)
                    var t269 []Token = append(t268, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t269)
                    var t270 int32 = ref_get__Ref_5int32(i__31)
                    var t271 int32 = t270 + 1
                    ref_set__Ref_5int32(i__31, t271)
                } else {
                    var t274 bool = ch__32 == 32
                    if t274 {
                        var t275 int32 = ref_get__Ref_5int32(i__31)
                        var t276 int32 = t275 + 1
                        ref_set__Ref_5int32(i__31, t276)
                    } else {
                        var t278 int32 = ref_get__Ref_5int32(i__31)
                        var mtmp13 Tuple2_5Token_5int32 = lex_atom(source__27, t278)
                        var x14 Token = mtmp13._0
                        var x15 int32 = mtmp13._1
                        var next__34 int32 = x15
                        var tok__33 Token = x14
                        var t279 []Token = ref_get__Ref_10Vec_5Token(toks__30)
                        var t280 []Token = append(t279, tok__33)
                        ref_set__Ref_10Vec_5Token(toks__30, t280)
                        ref_set__Ref_5int32(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop255
        }
    }
    var t254 []Token = ref_get__Ref_10Vec_5Token(toks__30)
    retv252 = t254
    return retv252
}

func env_lookup(env__35 []Binding, name__36 string) Value {
    var retv283 Value
    var t284 int32 = int32(len(env__35))
    var t285 int32 = t284 - 1
    var i__37 *ref_int32_x = ref__Ref_5int32(t285)
    var result__38 *ref_Value_x = ref__Ref_5Value(Nil{})
    var done__39 *ref_bool_x = ref__Ref_4bool(false)
    Loop_loop288:
    for {
        var t289 bool = ref_get__Ref_4bool(done__39)
        var t290 bool = !t289
        var t291 int32 = ref_get__Ref_5int32(i__37)
        var t292 bool = t291 >= 0
        var t293 bool = t290 && t292
        if t293 {
            var t294 int32 = ref_get__Ref_5int32(i__37)
            var binding__40 Binding = env__35[t294]
            var t296 string = binding__40.name
            var t297 bool = t296 == name__36
            if t297 {
                var t298 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t298)
                ref_set__Ref_4bool(done__39, true)
            } else {
                var t300 int32 = ref_get__Ref_5int32(i__37)
                var t301 int32 = t300 - 1
                ref_set__Ref_5int32(i__37, t301)
            }
            continue
        } else {
            break Loop_loop288
        }
    }
    var t287 Value = ref_get__Ref_5Value(result__38)
    retv283 = t287
    return retv283
}

func lookup(local__41 []Binding, global__42 []Binding, name__43 string) Value {
    var retv304 Value
    var mtmp20 Value = env_lookup(local__41, name__43)
    var jp306 Value
    switch mtmp20.(type) {
    case Value_Int:
        var other__44 Value = mtmp20
        jp306 = other__44
    case Value_Bool:
        var other__44 Value = mtmp20
        jp306 = other__44
    case Func:
        var other__44 Value = mtmp20
        jp306 = other__44
    case Nil:
        var t307 Value = env_lookup(global__42, name__43)
        jp306 = t307
    default:
        panic("non-exhaustive match")
    }
    retv304 = jp306
    return retv304
}

func parse_list(tokens__45 []Token, start__46 int32) Tuple2_10Vec_5SExpr_5int32 {
    var retv309 Tuple2_10Vec_5SExpr_5int32
    var acc__47 []SExpr = nil
    var exprs__48 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    var i__49 *ref_int32_x = ref__Ref_5int32(start__46)
    var done__50 *ref_bool_x = ref__Ref_4bool(false)
    Loop_loop314:
    for {
        var t315 bool = ref_get__Ref_4bool(done__50)
        var t316 bool = !t315
        var t317 int32 = ref_get__Ref_5int32(i__49)
        var t318 int32 = int32(len(tokens__45))
        var t319 bool = t317 < t318
        var t320 bool = t316 && t319
        if t320 {
            var t321 int32 = ref_get__Ref_5int32(i__49)
            var mtmp24 Token = tokens__45[t321]
            switch mtmp24.(type) {
            case LParen:
                var t323 int32 = ref_get__Ref_5int32(i__49)
                var mtmp28 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t323)
                var x29 SExpr = mtmp28._0
                var x30 int32 = mtmp28._1
                var next__52 int32 = x30
                var expr__51 SExpr = x29
                var t324 []SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                var t325 []SExpr = append(t324, expr__51)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t325)
                ref_set__Ref_5int32(i__49, next__52)
            case RParen:
                ref_set__Ref_4bool(done__50, true)
                var t327 int32 = ref_get__Ref_5int32(i__49)
                var t328 int32 = t327 + 1
                ref_set__Ref_5int32(i__49, t328)
            case Token_Sym:
                var t330 int32 = ref_get__Ref_5int32(i__49)
                var mtmp33 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t330)
                var x34 SExpr = mtmp33._0
                var x35 int32 = mtmp33._1
                var next__52 int32 = x35
                var expr__51 SExpr = x34
                var t331 []SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                var t332 []SExpr = append(t331, expr__51)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t332)
                ref_set__Ref_5int32(i__49, next__52)
            case Token_Int:
                var t334 int32 = ref_get__Ref_5int32(i__49)
                var mtmp37 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t334)
                var x38 SExpr = mtmp37._0
                var x39 int32 = mtmp37._1
                var next__52 int32 = x39
                var expr__51 SExpr = x38
                var t335 []SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                var t336 []SExpr = append(t335, expr__51)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t336)
                ref_set__Ref_5int32(i__49, next__52)
            case Token_Bool:
                var t338 int32 = ref_get__Ref_5int32(i__49)
                var mtmp41 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t338)
                var x42 SExpr = mtmp41._0
                var x43 int32 = mtmp41._1
                var next__52 int32 = x43
                var expr__51 SExpr = x42
                var t339 []SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                var t340 []SExpr = append(t339, expr__51)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t340)
                ref_set__Ref_5int32(i__49, next__52)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop314
        }
    }
    var t311 []SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    var t312 int32 = ref_get__Ref_5int32(i__49)
    var t313 Tuple2_10Vec_5SExpr_5int32 = Tuple2_10Vec_5SExpr_5int32{
        _0: t311,
        _1: t312,
    }
    retv309 = t313
    return retv309
}

func parse_expr(tokens__53 []Token, start__54 int32) Tuple2_5SExpr_5int32 {
    var retv343 Tuple2_5SExpr_5int32
    var mtmp46 Token = tokens__53[start__54]
    var jp345 Tuple2_5SExpr_5int32
    switch mtmp46.(type) {
    case LParen:
        var t346 int32 = start__54 + 1
        var mtmp50 Tuple2_10Vec_5SExpr_5int32 = parse_list(tokens__53, t346)
        var x51 []SExpr = mtmp50._0
        var x52 int32 = mtmp50._1
        var next__56 int32 = x52
        var items__55 []SExpr = x51
        var t347 SExpr = List{
            _0: items__55,
        }
        var t348 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t347,
            _1: next__56,
        }
        jp345 = t348
    case RParen:
        var t349 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t350 int32 = start__54 + 1
        var t351 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t349,
            _1: t350,
        }
        jp345 = t351
    case Token_Sym:
        var x47 string = mtmp46.(Token_Sym)._0
        var name__59 string = x47
        var t352 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t353 int32 = start__54 + 1
        var t354 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t352,
            _1: t353,
        }
        jp345 = t354
    case Token_Int:
        var x48 int32 = mtmp46.(Token_Int)._0
        var n__58 int32 = x48
        var t355 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t356 int32 = start__54 + 1
        var t357 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t355,
            _1: t356,
        }
        jp345 = t357
    case Token_Bool:
        var x49 bool = mtmp46.(Token_Bool)._0
        var b__57 bool = x49
        var t358 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t359 int32 = start__54 + 1
        var t360 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t358,
            _1: t359,
        }
        jp345 = t360
    default:
        panic("non-exhaustive match")
    }
    retv343 = jp345
    return retv343
}

func parse_program(tokens__60 []Token) []SExpr {
    var retv362 []SExpr
    var i__61 *ref_int32_x = ref__Ref_5int32(0)
    var acc__62 []SExpr = nil
    var exprs__63 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    Loop_loop365:
    for {
        var t366 int32 = ref_get__Ref_5int32(i__61)
        var t367 int32 = int32(len(tokens__60))
        var t368 bool = t366 < t367
        if t368 {
            var t369 int32 = ref_get__Ref_5int32(i__61)
            var mtmp53 Tuple2_5SExpr_5int32 = parse_expr(tokens__60, t369)
            var x54 SExpr = mtmp53._0
            var x55 int32 = mtmp53._1
            var next__65 int32 = x55
            var expr__64 SExpr = x54
            var t370 []SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            var t371 []SExpr = append(t370, expr__64)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t371)
            ref_set__Ref_5int32(i__61, next__65)
            continue
        } else {
            break Loop_loop365
        }
    }
    var t364 []SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    retv362 = t364
    return retv362
}

func value_to_string(value__66 Value) string {
    var retv374 string
    var jp376 string
    switch value__66.(type) {
    case Value_Int:
        var x58 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x58
        var t377 string = int32_to_string(n__67)
        jp376 = t377
    case Value_Bool:
        var x59 bool = value__66.(Value_Bool)._0
        var b__68 bool = x59
        var t378 string = bool_to_string(b__68)
        jp376 = t378
    case Func:
        jp376 = "<lambda>"
    case Nil:
        jp376 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv374 = jp376
    return retv374
}

func truthy(value__69 Value) bool {
    var retv380 bool
    var jp382 bool
    switch value__69.(type) {
    case Value_Int:
        var x61 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x61
        var t383 bool = n__71 != 0
        jp382 = t383
    case Value_Bool:
        var x62 bool = value__69.(Value_Bool)._0
        var b__70 bool = x62
        jp382 = b__70
    case Func:
        jp382 = true
    case Nil:
        jp382 = false
    default:
        panic("non-exhaustive match")
    }
    retv380 = jp382
    return retv380
}

func eval(expr__72 SExpr, local__73 []Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv385 Value
    var jp387 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x64 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x64
        var t388 Value = Value_Int{
            _0: n__75,
        }
        jp387 = t388
    case SExpr_Bool:
        var x65 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x65
        var t389 Value = Value_Bool{
            _0: b__76,
        }
        jp387 = t389
    case SExpr_Sym:
        var x66 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x66
        var t390 []Binding = ref_get__Ref_12Vec_7Binding(global__74)
        var t391 Value = lookup(local__73, t390, name__77)
        jp387 = t391
    case List:
        var x67 []SExpr = expr__72.(List)._0
        var items__78 []SExpr = x67
        var t392 Value = eval_list(items__78, local__73, global__74)
        jp387 = t392
    default:
        panic("non-exhaustive match")
    }
    retv385 = jp387
    return retv385
}

func eval_list(items__79 []SExpr, local__80 []Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv394 Value
    var t397 int32 = int32(len(items__79))
    var t398 bool = t397 == 0
    var jp396 Value
    if t398 {
        jp396 = Nil{}
    } else {
        var head__82 SExpr = items__79[0]
        var jp400 Value
        switch head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            var t401 Value = apply(f__84, args__85, global__81)
            jp400 = t401
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            var t402 Value = apply(f__84, args__85, global__81)
            jp400 = t402
        case SExpr_Sym:
            var x70 string = head__82.(SExpr_Sym)._0
            var name__83 string = x70
            var t403 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp400 = t403
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            var t404 Value = apply(f__84, args__85, global__81)
            jp400 = t404
        default:
            panic("non-exhaustive match")
        }
        jp396 = jp400
    }
    retv394 = jp396
    return retv394
}

func eval_list_sym(name__86 string, items__87 []SExpr, local__88 []Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv406 Value
    var jp408 Value
    switch name__86 {
    case "begin":
        var t409 Value = eval_begin(items__87, 1, local__88, global__89)
        jp408 = t409
    case "define":
        var t412 int32 = int32(len(items__87))
        var t413 bool = t412 == 3
        var jp411 Value
        if t413 {
            var mtmp72 SExpr = items__87[1]
            var jp415 Value
            switch mtmp72.(type) {
            case SExpr_Int:
                jp415 = Nil{}
            case SExpr_Bool:
                jp415 = Nil{}
            case SExpr_Sym:
                var x75 string = mtmp72.(SExpr_Sym)._0
                var var__90 string = x75
                var t416 SExpr = items__87[2]
                var value__91 Value = eval(t416, local__88, global__89)
                var env__92 []Binding = ref_get__Ref_12Vec_7Binding(global__89)
                var t417 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 []Binding = append(env__92, t417)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                jp415 = value__91
            case List:
                jp415 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp411 = jp415
        } else {
            jp411 = Nil{}
        }
        jp408 = jp411
    case "if":
        var t420 int32 = int32(len(items__87))
        var t421 bool = t420 == 4
        var jp419 Value
        if t421 {
            var t422 SExpr = items__87[1]
            var cond__94 Value = eval(t422, local__88, global__89)
            var t425 bool = truthy(cond__94)
            var jp424 Value
            if t425 {
                var t426 SExpr = items__87[2]
                var t427 Value = eval(t426, local__88, global__89)
                jp424 = t427
            } else {
                var t428 SExpr = items__87[3]
                var t429 Value = eval(t428, local__88, global__89)
                jp424 = t429
            }
            jp419 = jp424
        } else {
            jp419 = Nil{}
        }
        jp408 = jp419
    case "lambda":
        var t432 int32 = int32(len(items__87))
        var t433 bool = t432 == 3
        var jp431 Value
        if t433 {
            var mtmp78 SExpr = items__87[1]
            var jp435 Value
            switch mtmp78.(type) {
            case SExpr_Int:
                jp435 = Nil{}
            case SExpr_Bool:
                jp435 = Nil{}
            case SExpr_Sym:
                jp435 = Nil{}
            case List:
                var x82 []SExpr = mtmp78.(List)._0
                var params_exprs__95 []SExpr = x82
                var params__96 []string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = items__87[2]
                var t436 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t437 Value = Func{
                    _0: t436,
                }
                jp435 = t437
            default:
                panic("non-exhaustive match")
            }
            jp431 = jp435
        } else {
            jp431 = Nil{}
        }
        jp408 = jp431
    case "+":
        var t438 []Value = eval_args(items__87, 1, local__88, global__89)
        var t439 Value = apply_builtin("+", t438)
        jp408 = t439
    case "-":
        var t440 []Value = eval_args(items__87, 1, local__88, global__89)
        var t441 Value = apply_builtin("-", t440)
        jp408 = t441
    case "*":
        var t442 []Value = eval_args(items__87, 1, local__88, global__89)
        var t443 Value = apply_builtin("*", t442)
        jp408 = t443
    case "/":
        var t444 []Value = eval_args(items__87, 1, local__88, global__89)
        var t445 Value = apply_builtin("/", t444)
        jp408 = t445
    case "=":
        var t446 []Value = eval_args(items__87, 1, local__88, global__89)
        var t447 Value = apply_builtin("=", t446)
        jp408 = t447
    default:
        var t448 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t448, local__88, global__89)
        var args__99 []Value = eval_args(items__87, 1, local__88, global__89)
        var t449 Value = apply(f__98, args__99, global__89)
        jp408 = t449
    }
    retv406 = jp408
    return retv406
}

func eval_begin(items__100 []SExpr, start__101 int32, local__102 []Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv451 Value
    var i__104 *ref_int32_x = ref__Ref_5int32(start__101)
    var last__105 *ref_Value_x = ref__Ref_5Value(Nil{})
    Loop_loop454:
    for {
        var t455 int32 = ref_get__Ref_5int32(i__104)
        var t456 int32 = int32(len(items__100))
        var t457 bool = t455 < t456
        if t457 {
            var t458 int32 = ref_get__Ref_5int32(i__104)
            var t459 SExpr = items__100[t458]
            var v__106 Value = eval(t459, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t460 int32 = ref_get__Ref_5int32(i__104)
            var t461 int32 = t460 + 1
            ref_set__Ref_5int32(i__104, t461)
            continue
        } else {
            break Loop_loop454
        }
    }
    var t453 Value = ref_get__Ref_5Value(last__105)
    retv451 = t453
    return retv451
}

func params_from_sexprs(items__107 []SExpr) []string {
    var retv464 []string
    var i__108 *ref_int32_x = ref__Ref_5int32(0)
    var acc__109 []string = nil
    var params__110 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    Loop_loop467:
    for {
        var t468 int32 = ref_get__Ref_5int32(i__108)
        var t469 int32 = int32(len(items__107))
        var t470 bool = t468 < t469
        if t470 {
            var t471 int32 = ref_get__Ref_5int32(i__108)
            var mtmp85 SExpr = items__107[t471]
            switch mtmp85.(type) {
            case SExpr_Int:
                var t473 int32 = ref_get__Ref_5int32(i__108)
                var t474 int32 = t473 + 1
                ref_set__Ref_5int32(i__108, t474)
            case SExpr_Bool:
                var t476 int32 = ref_get__Ref_5int32(i__108)
                var t477 int32 = t476 + 1
                ref_set__Ref_5int32(i__108, t477)
            case SExpr_Sym:
                var x88 string = mtmp85.(SExpr_Sym)._0
                var name__111 string = x88
                var t479 []string = ref_get__Ref_11Vec_6string(params__110)
                var t480 []string = append(t479, name__111)
                ref_set__Ref_11Vec_6string(params__110, t480)
                var t481 int32 = ref_get__Ref_5int32(i__108)
                var t482 int32 = t481 + 1
                ref_set__Ref_5int32(i__108, t482)
            case List:
                var t484 int32 = ref_get__Ref_5int32(i__108)
                var t485 int32 = t484 + 1
                ref_set__Ref_5int32(i__108, t485)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop467
        }
    }
    var t466 []string = ref_get__Ref_11Vec_6string(params__110)
    retv464 = t466
    return retv464
}

func eval_args(items__112 []SExpr, start__113 int32, local__114 []Binding, global__115 *ref_Vec_7Binding_x) []Value {
    var retv488 []Value
    var i__116 *ref_int32_x = ref__Ref_5int32(start__113)
    var acc__117 []Value = nil
    var args__118 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    Loop_loop491:
    for {
        var t492 int32 = ref_get__Ref_5int32(i__116)
        var t493 int32 = int32(len(items__112))
        var t494 bool = t492 < t493
        if t494 {
            var t495 int32 = ref_get__Ref_5int32(i__116)
            var t496 SExpr = items__112[t495]
            var v__119 Value = eval(t496, local__114, global__115)
            var t497 []Value = ref_get__Ref_10Vec_5Value(args__118)
            var t498 []Value = append(t497, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t498)
            var t499 int32 = ref_get__Ref_5int32(i__116)
            var t500 int32 = t499 + 1
            ref_set__Ref_5int32(i__116, t500)
            continue
        } else {
            break Loop_loop491
        }
    }
    var t490 []Value = ref_get__Ref_10Vec_5Value(args__118)
    retv488 = t490
    return retv488
}

func apply_builtin(name__120 string, args__121 []Value) Value {
    var retv503 Value
    var jp505 Value
    switch name__120 {
    case "=":
        var t508 int32 = int32(len(args__121))
        var t509 bool = t508 == 2
        var jp507 Value
        if t509 {
            var t510 Value = args__121[0]
            var t511 Value = args__121[1]
            var mtmp94 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t510,
                _1: t511,
            }
            var x95 Value = mtmp94._0
            var x96 Value = mtmp94._1
            var jp513 Value
            switch x96.(type) {
            case Value_Int:
                var x97 int32 = x96.(Value_Int)._0
                var jp515 Value
                switch x95.(type) {
                case Value_Int:
                    var x100 int32 = x95.(Value_Int)._0
                    var a__122 int32 = x100
                    var b__123 int32 = x97
                    var t516 bool = a__122 == b__123
                    var t517 Value = Value_Bool{
                        _0: t516,
                    }
                    jp515 = t517
                case Value_Bool:
                    var t518 Value = Value_Bool{
                        _0: false,
                    }
                    jp515 = t518
                case Func:
                    var t519 Value = Value_Bool{
                        _0: false,
                    }
                    jp515 = t519
                case Nil:
                    var t520 Value = Value_Bool{
                        _0: false,
                    }
                    jp515 = t520
                default:
                    panic("non-exhaustive match")
                }
                jp513 = jp515
            case Value_Bool:
                var x98 bool = x96.(Value_Bool)._0
                var jp522 Value
                switch x95.(type) {
                case Value_Int:
                    var t523 Value = Value_Bool{
                        _0: false,
                    }
                    jp522 = t523
                case Value_Bool:
                    var x104 bool = x95.(Value_Bool)._0
                    var a__124 bool = x104
                    var b__125 bool = x98
                    var t524 bool = a__124 == b__125
                    var t525 Value = Value_Bool{
                        _0: t524,
                    }
                    jp522 = t525
                case Func:
                    var t526 Value = Value_Bool{
                        _0: false,
                    }
                    jp522 = t526
                case Nil:
                    var t527 Value = Value_Bool{
                        _0: false,
                    }
                    jp522 = t527
                default:
                    panic("non-exhaustive match")
                }
                jp513 = jp522
            case Func:
                var t528 Value = Value_Bool{
                    _0: false,
                }
                jp513 = t528
            case Nil:
                var t529 Value = Value_Bool{
                    _0: false,
                }
                jp513 = t529
            default:
                panic("non-exhaustive match")
            }
            jp507 = jp513
        } else {
            var t530 Value = Value_Bool{
                _0: false,
            }
            jp507 = t530
        }
        jp505 = jp507
        retv503 = jp505
        return retv503
    case "+":
        var i__126 *ref_int32_x = ref__Ref_5int32(0)
        var acc__127 *ref_int32_x = ref__Ref_5int32(0)
        Loop_loop534:
        for {
            var t535 int32 = ref_get__Ref_5int32(i__126)
            var t536 int32 = int32(len(args__121))
            var t537 bool = t535 < t536
            if t537 {
                var t538 int32 = ref_get__Ref_5int32(i__126)
                var mtmp106 Value = args__121[t538]
                switch mtmp106.(type) {
                case Value_Int:
                    var x107 int32 = mtmp106.(Value_Int)._0
                    var n__128 int32 = x107
                    var t540 int32 = ref_get__Ref_5int32(acc__127)
                    var t541 int32 = t540 + n__128
                    ref_set__Ref_5int32(acc__127, t541)
                    var t542 int32 = ref_get__Ref_5int32(i__126)
                    var t543 int32 = t542 + 1
                    ref_set__Ref_5int32(i__126, t543)
                case Value_Bool:
                    var t545 int32 = ref_get__Ref_5int32(i__126)
                    var t546 int32 = t545 + 1
                    ref_set__Ref_5int32(i__126, t546)
                case Func:
                    var t548 int32 = ref_get__Ref_5int32(i__126)
                    var t549 int32 = t548 + 1
                    ref_set__Ref_5int32(i__126, t549)
                case Nil:
                    var t551 int32 = ref_get__Ref_5int32(i__126)
                    var t552 int32 = t551 + 1
                    ref_set__Ref_5int32(i__126, t552)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop534
            }
        }
        var t532 int32 = ref_get__Ref_5int32(acc__127)
        var t533 Value = Value_Int{
            _0: t532,
        }
        jp505 = t533
        retv503 = jp505
        return retv503
    case "*":
        var i__129 *ref_int32_x = ref__Ref_5int32(0)
        var acc__130 *ref_int32_x = ref__Ref_5int32(1)
        Loop_loop557:
        for {
            var t558 int32 = ref_get__Ref_5int32(i__129)
            var t559 int32 = int32(len(args__121))
            var t560 bool = t558 < t559
            if t560 {
                var t561 int32 = ref_get__Ref_5int32(i__129)
                var mtmp112 Value = args__121[t561]
                switch mtmp112.(type) {
                case Value_Int:
                    var x113 int32 = mtmp112.(Value_Int)._0
                    var n__131 int32 = x113
                    var t563 int32 = ref_get__Ref_5int32(acc__130)
                    var t564 int32 = t563 * n__131
                    ref_set__Ref_5int32(acc__130, t564)
                    var t565 int32 = ref_get__Ref_5int32(i__129)
                    var t566 int32 = t565 + 1
                    ref_set__Ref_5int32(i__129, t566)
                case Value_Bool:
                    var t568 int32 = ref_get__Ref_5int32(i__129)
                    var t569 int32 = t568 + 1
                    ref_set__Ref_5int32(i__129, t569)
                case Func:
                    var t571 int32 = ref_get__Ref_5int32(i__129)
                    var t572 int32 = t571 + 1
                    ref_set__Ref_5int32(i__129, t572)
                case Nil:
                    var t574 int32 = ref_get__Ref_5int32(i__129)
                    var t575 int32 = t574 + 1
                    ref_set__Ref_5int32(i__129, t575)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop557
            }
        }
        var t555 int32 = ref_get__Ref_5int32(acc__130)
        var t556 Value = Value_Int{
            _0: t555,
        }
        jp505 = t556
        retv503 = jp505
        return retv503
    case "-":
        var mtmp118 int32 = int32(len(args__121))
        var jp578 Value
        switch mtmp118 {
        case 1:
            var mtmp119 Value = args__121[0]
            var jp580 Value
            switch mtmp119.(type) {
            case Value_Int:
                var x120 int32 = mtmp119.(Value_Int)._0
                var n__132 int32 = x120
                var t581 int32 = 0 - n__132
                var t582 Value = Value_Int{
                    _0: t581,
                }
                jp580 = t582
            case Value_Bool:
                jp580 = Nil{}
            case Func:
                jp580 = Nil{}
            case Nil:
                jp580 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp578 = jp580
        case 2:
            var t583 Value = args__121[0]
            var t584 Value = args__121[1]
            var mtmp123 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t583,
                _1: t584,
            }
            var x124 Value = mtmp123._0
            var x125 Value = mtmp123._1
            var jp586 Value
            switch x125.(type) {
            case Value_Int:
                var x126 int32 = x125.(Value_Int)._0
                var jp588 Value
                switch x124.(type) {
                case Value_Int:
                    var x129 int32 = x124.(Value_Int)._0
                    var a__133 int32 = x129
                    var b__134 int32 = x126
                    var t589 int32 = a__133 - b__134
                    var t590 Value = Value_Int{
                        _0: t589,
                    }
                    jp588 = t590
                case Value_Bool:
                    jp588 = Nil{}
                case Func:
                    jp588 = Nil{}
                case Nil:
                    jp588 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp586 = jp588
            case Value_Bool:
                jp586 = Nil{}
            case Func:
                jp586 = Nil{}
            case Nil:
                jp586 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp578 = jp586
        default:
            jp578 = Nil{}
        }
        jp505 = jp578
        retv503 = jp505
        return retv503
    case "/":
        var t593 int32 = int32(len(args__121))
        var t594 bool = t593 == 2
        var jp592 Value
        if t594 {
            var t595 Value = args__121[0]
            var t596 Value = args__121[1]
            var mtmp132 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t595,
                _1: t596,
            }
            var x133 Value = mtmp132._0
            var x134 Value = mtmp132._1
            var jp598 Value
            switch x134.(type) {
            case Value_Int:
                var x135 int32 = x134.(Value_Int)._0
                var jp600 Value
                switch x133.(type) {
                case Value_Int:
                    var x138 int32 = x133.(Value_Int)._0
                    var a__135 int32 = x138
                    var b__136 int32 = x135
                    var t601 int32 = a__135 / b__136
                    var t602 Value = Value_Int{
                        _0: t601,
                    }
                    jp600 = t602
                case Value_Bool:
                    jp600 = Nil{}
                case Func:
                    jp600 = Nil{}
                case Nil:
                    jp600 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp598 = jp600
            case Value_Bool:
                jp598 = Nil{}
            case Func:
                jp598 = Nil{}
            case Nil:
                jp598 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp592 = jp598
        } else {
            jp592 = Nil{}
        }
        jp505 = jp592
        retv503 = jp505
        return retv503
    default:
        jp505 = Nil{}
        retv503 = jp505
        return retv503
    }
}

func apply(func__137 Value, args__138 []Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv604 Value
    var jp606 Value
    switch func__137.(type) {
    case Value_Int:
        jp606 = Nil{}
    case Value_Bool:
        jp606 = Nil{}
    case Func:
        var x143 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x143
        var t607 Value = apply_lambda(fun__140, args__138)
        jp606 = t607
    case Nil:
        jp606 = Nil{}
    default:
        panic("non-exhaustive match")
    }
    retv604 = jp606
    return retv604
}

func apply_lambda(lambda__141 Lambda, args__142 []Value) Value {
    var retv609 Value
    var t610 []Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t610)
    var i__144 *ref_int32_x = ref__Ref_5int32(0)
    Loop_loop616:
    for {
        var t617 int32 = ref_get__Ref_5int32(i__144)
        var t618 []string = lambda__141.params
        var t619 int32 = int32(len(t618))
        var t620 bool = t617 < t619
        var t621 int32 = ref_get__Ref_5int32(i__144)
        var t622 int32 = int32(len(args__142))
        var t623 bool = t621 < t622
        var t624 bool = t620 && t623
        if t624 {
            var t625 []string = lambda__141.params
            var t626 int32 = ref_get__Ref_5int32(i__144)
            var name__145 string = t625[t626]
            var t627 int32 = ref_get__Ref_5int32(i__144)
            var value__146 Value = args__142[t627]
            var t628 []Binding = ref_get__Ref_12Vec_7Binding(env__143)
            var t629 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 []Binding = append(t628, t629)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t630 int32 = ref_get__Ref_5int32(i__144)
            var t631 int32 = t630 + 1
            ref_set__Ref_5int32(i__144, t631)
            continue
        } else {
            break Loop_loop616
        }
    }
    var t612 SExpr = lambda__141.body
    var t613 []Binding = ref_get__Ref_12Vec_7Binding(env__143)
    var t614 *ref_Vec_7Binding_x = lambda__141.global
    var t615 Value = eval(t612, t613, t614)
    retv609 = t615
    return retv609
}

func main0() struct{} {
    var t634 []Binding = nil
    var global__148 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t634)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t635 []Token = lex(program__149)
    var exprs__150 []SExpr = parse_program(t635)
    var t636 SExpr = exprs__150[0]
    var t637 []Binding = nil
    var result__151 Value = eval(t636, t637, global__148)
    var t638 string = value_to_string(result__151)
    string_println(t638)
    var t639 []Token = lex("(add3 10 20 30)")
    var exprs2__152 []SExpr = parse_program(t639)
    var t640 SExpr = exprs2__152[0]
    var t641 []Binding = nil
    var result2__153 Value = eval(t640, t641, global__148)
    var t642 string = value_to_string(result2__153)
    string_println(t642)
    return struct{}{}
}

func main() {
    main0()
}
