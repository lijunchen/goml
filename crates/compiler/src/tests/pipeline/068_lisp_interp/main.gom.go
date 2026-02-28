package main

import (
    "fmt"
    "unicode/utf8"
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
    if !utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

func ref_set__Ref_int32(reference *ref_int32_x, value int32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_string_x struct {
    value string
}

func ref__Ref_string(value string) *ref_string_x {
    return &ref_string_x{
        value: value,
    }
}

func ref_get__Ref_string(reference *ref_string_x) string {
    return reference.value
}

func ref_set__Ref_string(reference *ref_string_x, value string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_vec_token_x struct {
    value []Token
}

func ref__Ref_Vec_Token(value []Token) *ref_vec_token_x {
    return &ref_vec_token_x{
        value: value,
    }
}

func ref_get__Ref_Vec_Token(reference *ref_vec_token_x) []Token {
    return reference.value
}

func ref_set__Ref_Vec_Token(reference *ref_vec_token_x, value []Token) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_value_x struct {
    value Value
}

func ref__Ref_Value(value Value) *ref_value_x {
    return &ref_value_x{
        value: value,
    }
}

func ref_get__Ref_Value(reference *ref_value_x) Value {
    return reference.value
}

func ref_set__Ref_Value(reference *ref_value_x, value Value) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_vec_sexpr_x struct {
    value []SExpr
}

func ref__Ref_Vec_SExpr(value []SExpr) *ref_vec_sexpr_x {
    return &ref_vec_sexpr_x{
        value: value,
    }
}

func ref_get__Ref_Vec_SExpr(reference *ref_vec_sexpr_x) []SExpr {
    return reference.value
}

func ref_set__Ref_Vec_SExpr(reference *ref_vec_sexpr_x, value []SExpr) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_vec_binding_x struct {
    value []Binding
}

func ref__Ref_Vec_Binding(value []Binding) *ref_vec_binding_x {
    return &ref_vec_binding_x{
        value: value,
    }
}

func ref_get__Ref_Vec_Binding(reference *ref_vec_binding_x) []Binding {
    return reference.value
}

func ref_set__Ref_Vec_Binding(reference *ref_vec_binding_x, value []Binding) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_vec_string_x struct {
    value []string
}

func ref__Ref_Vec_string(value []string) *ref_vec_string_x {
    return &ref_vec_string_x{
        value: value,
    }
}

func ref_get__Ref_Vec_string(reference *ref_vec_string_x) []string {
    return reference.value
}

func ref_set__Ref_Vec_string(reference *ref_vec_string_x, value []string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_vec_value_x struct {
    value []Value
}

func ref__Ref_Vec_Value(value []Value) *ref_vec_value_x {
    return &ref_vec_value_x{
        value: value,
    }
}

func ref_get__Ref_Vec_Value(reference *ref_vec_value_x) []Value {
    return reference.value
}

func ref_set__Ref_Vec_Value(reference *ref_vec_value_x, value []Value) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_Token_int32 struct {
    _0 Token
    _1 int32
}

type Tuple2_Vec_SExpr_int32 struct {
    _0 []SExpr
    _1 int32
}

type Tuple2_SExpr_int32 struct {
    _0 SExpr
    _1 int32
}

type Tuple2_Value_Value struct {
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
    global *ref_vec_binding_x
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

func is_digit(ch__0 rune) bool {
    var t148 bool
    var t149 bool
    var t150 bool
    t148 = ch__0 >= 48
    t149 = ch__0 <= 57
    t150 = t148 && t149
    return t150
}

func digit_value(ch__1 rune) int32 {
    var jp152 int32
    switch ch__1 {
    case 48:
        goto b2
    case 49:
        goto b3
    case 50:
        goto b4
    case 51:
        goto b5
    case 52:
        goto b6
    case 53:
        goto b7
    case 54:
        goto b8
    case 55:
        goto b9
    case 56:
        goto b10
    case 57:
        goto b11
    default:
        goto b12
    }
    b1:
    return jp152
    b2:
    jp152 = 0
    goto b1
    b3:
    jp152 = 1
    goto b1
    b4:
    jp152 = 2
    goto b1
    b5:
    jp152 = 3
    goto b1
    b6:
    jp152 = 4
    goto b1
    b7:
    jp152 = 5
    goto b1
    b8:
    jp152 = 6
    goto b1
    b9:
    jp152 = 7
    goto b1
    b10:
    jp152 = 8
    goto b1
    b11:
    jp152 = 9
    goto b1
    b12:
    jp152 = 0
    goto b1
}

func is_int_text(text__2 string) bool {
    var len__3 int32
    var t155 bool
    var jp154 bool
    var i__4 *ref_int32_x
    var saw_digit__5 *ref_bool_x
    var ok__6 *ref_bool_x
    var started__7 *ref_bool_x
    var t157 bool
    var t158 bool
    var t159 bool
    var t161 bool
    var t162 int32
    var t163 bool
    var t164 bool
    var t165 int32
    var ch__8 rune
    var t167 bool
    var t168 bool
    var t169 bool
    var t170 bool
    var t171 int32
    var t172 int32
    var t175 bool
    var t176 int32
    var t177 int32
    len__3 = string_len(text__2)
    t155 = len__3 == 0
    if t155 {
        goto b2
    } else {
        goto b3
    }
    b1:
    return jp154
    b2:
    jp154 = false
    goto b1
    b3:
    i__4 = ref__Ref_int32(0)
    saw_digit__5 = ref__Ref_bool(false)
    ok__6 = ref__Ref_bool(true)
    started__7 = ref__Ref_bool(false)
    goto b5
    b4:
    t157 = ref_get__Ref_bool(ok__6)
    t158 = ref_get__Ref_bool(saw_digit__5)
    t159 = t157 && t158
    jp154 = t159
    goto b1
    b5:
    t161 = ref_get__Ref_bool(ok__6)
    t162 = ref_get__Ref_int32(i__4)
    t163 = t162 < len__3
    t164 = t161 && t163
    if t164 {
        goto b6
    } else {
        goto b13
    }
    b6:
    t165 = ref_get__Ref_int32(i__4)
    ch__8 = string_get(text__2, t165)
    t167 = ref_get__Ref_bool(started__7)
    t168 = !t167
    t169 = ch__8 == 45
    t170 = t168 && t169
    if t170 {
        goto b8
    } else {
        goto b9
    }
    b7:
    goto b5
    b8:
    ref_set__Ref_bool(started__7, true)
    t171 = ref_get__Ref_int32(i__4)
    t172 = t171 + 1
    ref_set__Ref_int32(i__4, t172)
    goto b7
    b9:
    t175 = is_digit(ch__8)
    if t175 {
        goto b11
    } else {
        goto b12
    }
    b10:
    goto b7
    b11:
    ref_set__Ref_bool(started__7, true)
    ref_set__Ref_bool(saw_digit__5, true)
    t176 = ref_get__Ref_int32(i__4)
    t177 = t176 + 1
    ref_set__Ref_int32(i__4, t177)
    goto b10
    b12:
    ref_set__Ref_bool(ok__6, false)
    goto b10
    b13:
    goto b4
}

func parse_int32(text__9 string) int32 {
    var len__10 int32
    var i__11 *ref_int32_x
    var negative__12 *ref_bool_x
    var started__13 *ref_bool_x
    var acc__14 *ref_int32_x
    var t183 bool
    var jp182 int32
    var t184 int32
    var t185 int32
    var t186 int32
    var t188 int32
    var t189 bool
    var t190 int32
    var ch__15 rune
    var t192 bool
    var t193 bool
    var t194 bool
    var t195 bool
    var t196 int32
    var t197 int32
    var d__16 int32
    var t199 int32
    var t200 int32
    var t201 int32
    var t202 int32
    var t203 int32
    len__10 = string_len(text__9)
    i__11 = ref__Ref_int32(0)
    negative__12 = ref__Ref_bool(false)
    started__13 = ref__Ref_bool(false)
    acc__14 = ref__Ref_int32(0)
    goto b5
    b1:
    t183 = ref_get__Ref_bool(negative__12)
    if t183 {
        goto b3
    } else {
        goto b4
    }
    b2:
    return jp182
    b3:
    t184 = ref_get__Ref_int32(acc__14)
    t185 = 0 - t184
    jp182 = t185
    goto b2
    b4:
    t186 = ref_get__Ref_int32(acc__14)
    jp182 = t186
    goto b2
    b5:
    t188 = ref_get__Ref_int32(i__11)
    t189 = t188 < len__10
    if t189 {
        goto b6
    } else {
        goto b10
    }
    b6:
    t190 = ref_get__Ref_int32(i__11)
    ch__15 = string_get(text__9, t190)
    t192 = ref_get__Ref_bool(started__13)
    t193 = !t192
    t194 = ch__15 == 45
    t195 = t193 && t194
    if t195 {
        goto b8
    } else {
        goto b9
    }
    b7:
    goto b5
    b8:
    ref_set__Ref_bool(started__13, true)
    ref_set__Ref_bool(negative__12, true)
    t196 = ref_get__Ref_int32(i__11)
    t197 = t196 + 1
    ref_set__Ref_int32(i__11, t197)
    goto b7
    b9:
    ref_set__Ref_bool(started__13, true)
    d__16 = digit_value(ch__15)
    t199 = ref_get__Ref_int32(acc__14)
    t200 = t199 * 10
    t201 = t200 + d__16
    ref_set__Ref_int32(acc__14, t201)
    t202 = ref_get__Ref_int32(i__11)
    t203 = t202 + 1
    ref_set__Ref_int32(i__11, t203)
    goto b7
    b10:
    goto b1
}

func is_delim(ch__17 rune) bool {
    var t205 bool
    var t206 bool
    var t207 bool
    var t208 bool
    var t209 bool
    t205 = ch__17 == 40
    t206 = ch__17 == 41
    t207 = t205 || t206
    t208 = ch__17 == 32
    t209 = t207 || t208
    return t209
}

func lex_atom(source__18 string, start__19 int32) Tuple2_Token_int32 {
    var len__20 int32
    var text__21 *ref_string_x
    var i__22 *ref_int32_x
    var done__23 *ref_bool_x
    var atom__25 string
    var jp212 Token
    var token__26 Token
    var t213 int32
    var t214 Tuple2_Token_int32
    var t215 Token
    var t216 Token
    var t219 bool
    var jp218 Token
    var t220 int32
    var t221 Token
    var t222 Token
    var t224 bool
    var t225 bool
    var t226 int32
    var t227 bool
    var t228 bool
    var t229 int32
    var ch__24 rune
    var t231 bool
    var t233 string
    var t234 string
    var t235 string
    var t236 int32
    var t237 int32
    len__20 = string_len(source__18)
    text__21 = ref__Ref_string("")
    i__22 = ref__Ref_int32(start__19)
    done__23 = ref__Ref_bool(false)
    goto b9
    b1:
    atom__25 = ref_get__Ref_string(text__21)
    switch atom__25 {
    case "true":
        goto b3
    case "false":
        goto b4
    default:
        goto b5
    }
    b2:
    token__26 = jp212
    t213 = ref_get__Ref_int32(i__22)
    t214 = Tuple2_Token_int32{
        _0: token__26,
        _1: t213,
    }
    return t214
    b3:
    t215 = Token_Bool{
        _0: true,
    }
    jp212 = t215
    goto b2
    b4:
    t216 = Token_Bool{
        _0: false,
    }
    jp212 = t216
    goto b2
    b5:
    t219 = is_int_text(atom__25)
    if t219 {
        goto b7
    } else {
        goto b8
    }
    b6:
    jp212 = jp218
    goto b2
    b7:
    t220 = parse_int32(atom__25)
    t221 = Token_Int{
        _0: t220,
    }
    jp218 = t221
    goto b6
    b8:
    t222 = Token_Sym{
        _0: atom__25,
    }
    jp218 = t222
    goto b6
    b9:
    t224 = ref_get__Ref_bool(done__23)
    t225 = !t224
    t226 = ref_get__Ref_int32(i__22)
    t227 = t226 < len__20
    t228 = t225 && t227
    if t228 {
        goto b10
    } else {
        goto b14
    }
    b10:
    t229 = ref_get__Ref_int32(i__22)
    ch__24 = string_get(source__18, t229)
    t231 = is_delim(ch__24)
    if t231 {
        goto b12
    } else {
        goto b13
    }
    b11:
    goto b9
    b12:
    ref_set__Ref_bool(done__23, true)
    goto b11
    b13:
    t233 = ref_get__Ref_string(text__21)
    t234 = char_to_string(ch__24)
    t235 = t233 + t234
    ref_set__Ref_string(text__21, t235)
    t236 = ref_get__Ref_int32(i__22)
    t237 = t236 + 1
    ref_set__Ref_int32(i__22, t237)
    goto b11
    b14:
    goto b1
}

func lex(source__27 string) []Token {
    var len__28 int32
    var toks0__29 []Token
    var toks__30 *ref_vec_token_x
    var i__31 *ref_int32_x
    var t240 []Token
    var t242 int32
    var t243 bool
    var t244 int32
    var ch__32 rune
    var t246 bool
    var t247 []Token
    var t248 []Token
    var t249 int32
    var t250 int32
    var t253 bool
    var t254 []Token
    var t255 []Token
    var t256 int32
    var t257 int32
    var t260 bool
    var t261 int32
    var t262 int32
    var t264 int32
    var mtmp13 Tuple2_Token_int32
    var x14 Token
    var x15 int32
    var next__34 int32
    var tok__33 Token
    var t265 []Token
    var t266 []Token
    len__28 = string_len(source__27)
    toks0__29 = nil
    toks__30 = ref__Ref_Vec_Token(toks0__29)
    i__31 = ref__Ref_int32(0)
    goto b2
    b1:
    t240 = ref_get__Ref_Vec_Token(toks__30)
    return t240
    b2:
    t242 = ref_get__Ref_int32(i__31)
    t243 = t242 < len__28
    if t243 {
        goto b3
    } else {
        goto b13
    }
    b3:
    t244 = ref_get__Ref_int32(i__31)
    ch__32 = string_get(source__27, t244)
    t246 = ch__32 == 40
    if t246 {
        goto b5
    } else {
        goto b6
    }
    b4:
    goto b2
    b5:
    t247 = ref_get__Ref_Vec_Token(toks__30)
    t248 = append(t247, LParen{})
    ref_set__Ref_Vec_Token(toks__30, t248)
    t249 = ref_get__Ref_int32(i__31)
    t250 = t249 + 1
    ref_set__Ref_int32(i__31, t250)
    goto b4
    b6:
    t253 = ch__32 == 41
    if t253 {
        goto b8
    } else {
        goto b9
    }
    b7:
    goto b4
    b8:
    t254 = ref_get__Ref_Vec_Token(toks__30)
    t255 = append(t254, RParen{})
    ref_set__Ref_Vec_Token(toks__30, t255)
    t256 = ref_get__Ref_int32(i__31)
    t257 = t256 + 1
    ref_set__Ref_int32(i__31, t257)
    goto b7
    b9:
    t260 = ch__32 == 32
    if t260 {
        goto b11
    } else {
        goto b12
    }
    b10:
    goto b7
    b11:
    t261 = ref_get__Ref_int32(i__31)
    t262 = t261 + 1
    ref_set__Ref_int32(i__31, t262)
    goto b10
    b12:
    t264 = ref_get__Ref_int32(i__31)
    mtmp13 = lex_atom(source__27, t264)
    x14 = mtmp13._0
    x15 = mtmp13._1
    next__34 = x15
    tok__33 = x14
    t265 = ref_get__Ref_Vec_Token(toks__30)
    t266 = append(t265, tok__33)
    ref_set__Ref_Vec_Token(toks__30, t266)
    ref_set__Ref_int32(i__31, next__34)
    goto b10
    b13:
    goto b1
}

func env_lookup(env__35 []Binding, name__36 string) Value {
    var t268 int32
    var t269 int32
    var i__37 *ref_int32_x
    var result__38 *ref_value_x
    var done__39 *ref_bool_x
    var t271 Value
    var t273 bool
    var t274 bool
    var t275 int32
    var t276 bool
    var t277 bool
    var t278 int32
    var binding__40 Binding
    var t280 string
    var t281 bool
    var t282 Value
    var t284 int32
    var t285 int32
    t268 = int32(len(env__35))
    t269 = t268 - 1
    i__37 = ref__Ref_int32(t269)
    result__38 = ref__Ref_Value(Nil{})
    done__39 = ref__Ref_bool(false)
    goto b2
    b1:
    t271 = ref_get__Ref_Value(result__38)
    return t271
    b2:
    t273 = ref_get__Ref_bool(done__39)
    t274 = !t273
    t275 = ref_get__Ref_int32(i__37)
    t276 = t275 >= 0
    t277 = t274 && t276
    if t277 {
        goto b3
    } else {
        goto b7
    }
    b3:
    t278 = ref_get__Ref_int32(i__37)
    binding__40 = env__35[t278]
    t280 = binding__40.name
    t281 = t280 == name__36
    if t281 {
        goto b5
    } else {
        goto b6
    }
    b4:
    goto b2
    b5:
    t282 = binding__40.value
    ref_set__Ref_Value(result__38, t282)
    ref_set__Ref_bool(done__39, true)
    goto b4
    b6:
    t284 = ref_get__Ref_int32(i__37)
    t285 = t284 - 1
    ref_set__Ref_int32(i__37, t285)
    goto b4
    b7:
    goto b1
}

func lookup(local__41 []Binding, global__42 []Binding, name__43 string) Value {
    var mtmp20 Value
    var jp288 Value
    var other__44 Value
    var t289 Value
    mtmp20 = env_lookup(local__41, name__43)
    switch mtmp20.(type) {
    case Value_Int:
        goto b2
    case Value_Bool:
        goto b3
    case Func:
        goto b4
    case Nil:
        goto b5
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp288
    b2:
    other__44 = mtmp20
    jp288 = other__44
    goto b1
    b3:
    other__44 = mtmp20
    jp288 = other__44
    goto b1
    b4:
    other__44 = mtmp20
    jp288 = other__44
    goto b1
    b5:
    t289 = env_lookup(global__42, name__43)
    jp288 = t289
    goto b1
}

func parse_list(tokens__45 []Token, start__46 int32) Tuple2_Vec_SExpr_int32 {
    var acc__47 []SExpr
    var exprs__48 *ref_vec_sexpr_x
    var i__49 *ref_int32_x
    var done__50 *ref_bool_x
    var t291 []SExpr
    var t292 int32
    var t293 Tuple2_Vec_SExpr_int32
    var t295 bool
    var t296 bool
    var t297 int32
    var t298 int32
    var t299 bool
    var t300 bool
    var t301 int32
    var mtmp24 Token
    var t303 int32
    var mtmp28 Tuple2_SExpr_int32
    var x29 SExpr
    var x30 int32
    var next__52 int32
    var expr__51 SExpr
    var t304 []SExpr
    var t305 []SExpr
    var t307 int32
    var t308 int32
    var t310 int32
    var mtmp33 Tuple2_SExpr_int32
    var x34 SExpr
    var x35 int32
    var t311 []SExpr
    var t312 []SExpr
    var t314 int32
    var mtmp37 Tuple2_SExpr_int32
    var x38 SExpr
    var x39 int32
    var t315 []SExpr
    var t316 []SExpr
    var t318 int32
    var mtmp41 Tuple2_SExpr_int32
    var x42 SExpr
    var x43 int32
    var t319 []SExpr
    var t320 []SExpr
    acc__47 = nil
    exprs__48 = ref__Ref_Vec_SExpr(acc__47)
    i__49 = ref__Ref_int32(start__46)
    done__50 = ref__Ref_bool(false)
    goto b2
    b1:
    t291 = ref_get__Ref_Vec_SExpr(exprs__48)
    t292 = ref_get__Ref_int32(i__49)
    t293 = Tuple2_Vec_SExpr_int32{
        _0: t291,
        _1: t292,
    }
    return t293
    b2:
    t295 = ref_get__Ref_bool(done__50)
    t296 = !t295
    t297 = ref_get__Ref_int32(i__49)
    t298 = int32(len(tokens__45))
    t299 = t297 < t298
    t300 = t296 && t299
    if t300 {
        goto b3
    } else {
        goto b10
    }
    b3:
    t301 = ref_get__Ref_int32(i__49)
    mtmp24 = tokens__45[t301]
    switch mtmp24.(type) {
    case LParen:
        goto b5
    case RParen:
        goto b6
    case Token_Sym:
        goto b7
    case Token_Int:
        goto b8
    case Token_Bool:
        goto b9
    default:
        panic("non-exhaustive match")
    }
    b4:
    goto b2
    b5:
    t303 = ref_get__Ref_int32(i__49)
    mtmp28 = parse_expr(tokens__45, t303)
    x29 = mtmp28._0
    x30 = mtmp28._1
    next__52 = x30
    expr__51 = x29
    t304 = ref_get__Ref_Vec_SExpr(exprs__48)
    t305 = append(t304, expr__51)
    ref_set__Ref_Vec_SExpr(exprs__48, t305)
    ref_set__Ref_int32(i__49, next__52)
    goto b4
    b6:
    ref_set__Ref_bool(done__50, true)
    t307 = ref_get__Ref_int32(i__49)
    t308 = t307 + 1
    ref_set__Ref_int32(i__49, t308)
    goto b4
    b7:
    t310 = ref_get__Ref_int32(i__49)
    mtmp33 = parse_expr(tokens__45, t310)
    x34 = mtmp33._0
    x35 = mtmp33._1
    next__52 = x35
    expr__51 = x34
    t311 = ref_get__Ref_Vec_SExpr(exprs__48)
    t312 = append(t311, expr__51)
    ref_set__Ref_Vec_SExpr(exprs__48, t312)
    ref_set__Ref_int32(i__49, next__52)
    goto b4
    b8:
    t314 = ref_get__Ref_int32(i__49)
    mtmp37 = parse_expr(tokens__45, t314)
    x38 = mtmp37._0
    x39 = mtmp37._1
    next__52 = x39
    expr__51 = x38
    t315 = ref_get__Ref_Vec_SExpr(exprs__48)
    t316 = append(t315, expr__51)
    ref_set__Ref_Vec_SExpr(exprs__48, t316)
    ref_set__Ref_int32(i__49, next__52)
    goto b4
    b9:
    t318 = ref_get__Ref_int32(i__49)
    mtmp41 = parse_expr(tokens__45, t318)
    x42 = mtmp41._0
    x43 = mtmp41._1
    next__52 = x43
    expr__51 = x42
    t319 = ref_get__Ref_Vec_SExpr(exprs__48)
    t320 = append(t319, expr__51)
    ref_set__Ref_Vec_SExpr(exprs__48, t320)
    ref_set__Ref_int32(i__49, next__52)
    goto b4
    b10:
    goto b1
}

func parse_expr(tokens__53 []Token, start__54 int32) Tuple2_SExpr_int32 {
    var mtmp46 Token
    var jp323 Tuple2_SExpr_int32
    var t324 int32
    var mtmp50 Tuple2_Vec_SExpr_int32
    var x51 []SExpr
    var x52 int32
    var next__56 int32
    var items__55 []SExpr
    var t325 SExpr
    var t326 Tuple2_SExpr_int32
    var t327 SExpr
    var t328 int32
    var t329 Tuple2_SExpr_int32
    var x47 string
    var name__59 string
    var t330 SExpr
    var t331 int32
    var t332 Tuple2_SExpr_int32
    var x48 int32
    var n__58 int32
    var t333 SExpr
    var t334 int32
    var t335 Tuple2_SExpr_int32
    var x49 bool
    var b__57 bool
    var t336 SExpr
    var t337 int32
    var t338 Tuple2_SExpr_int32
    mtmp46 = tokens__53[start__54]
    switch mtmp46.(type) {
    case LParen:
        goto b2
    case RParen:
        goto b3
    case Token_Sym:
        goto b4
    case Token_Int:
        goto b5
    case Token_Bool:
        goto b6
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp323
    b2:
    t324 = start__54 + 1
    mtmp50 = parse_list(tokens__53, t324)
    x51 = mtmp50._0
    x52 = mtmp50._1
    next__56 = x52
    items__55 = x51
    t325 = List{
        _0: items__55,
    }
    t326 = Tuple2_SExpr_int32{
        _0: t325,
        _1: next__56,
    }
    jp323 = t326
    goto b1
    b3:
    t327 = SExpr_Sym{
        _0: ")",
    }
    t328 = start__54 + 1
    t329 = Tuple2_SExpr_int32{
        _0: t327,
        _1: t328,
    }
    jp323 = t329
    goto b1
    b4:
    x47 = mtmp46.(Token_Sym)._0
    name__59 = x47
    t330 = SExpr_Sym{
        _0: name__59,
    }
    t331 = start__54 + 1
    t332 = Tuple2_SExpr_int32{
        _0: t330,
        _1: t331,
    }
    jp323 = t332
    goto b1
    b5:
    x48 = mtmp46.(Token_Int)._0
    n__58 = x48
    t333 = SExpr_Int{
        _0: n__58,
    }
    t334 = start__54 + 1
    t335 = Tuple2_SExpr_int32{
        _0: t333,
        _1: t334,
    }
    jp323 = t335
    goto b1
    b6:
    x49 = mtmp46.(Token_Bool)._0
    b__57 = x49
    t336 = SExpr_Bool{
        _0: b__57,
    }
    t337 = start__54 + 1
    t338 = Tuple2_SExpr_int32{
        _0: t336,
        _1: t337,
    }
    jp323 = t338
    goto b1
}

func parse_program(tokens__60 []Token) []SExpr {
    var i__61 *ref_int32_x
    var acc__62 []SExpr
    var exprs__63 *ref_vec_sexpr_x
    var t340 []SExpr
    var t342 int32
    var t343 int32
    var t344 bool
    var t345 int32
    var mtmp53 Tuple2_SExpr_int32
    var x54 SExpr
    var x55 int32
    var next__65 int32
    var expr__64 SExpr
    var t346 []SExpr
    var t347 []SExpr
    i__61 = ref__Ref_int32(0)
    acc__62 = nil
    exprs__63 = ref__Ref_Vec_SExpr(acc__62)
    goto b2
    b1:
    t340 = ref_get__Ref_Vec_SExpr(exprs__63)
    return t340
    b2:
    t342 = ref_get__Ref_int32(i__61)
    t343 = int32(len(tokens__60))
    t344 = t342 < t343
    if t344 {
        goto b3
    } else {
        goto b4
    }
    b3:
    t345 = ref_get__Ref_int32(i__61)
    mtmp53 = parse_expr(tokens__60, t345)
    x54 = mtmp53._0
    x55 = mtmp53._1
    next__65 = x55
    expr__64 = x54
    t346 = ref_get__Ref_Vec_SExpr(exprs__63)
    t347 = append(t346, expr__64)
    ref_set__Ref_Vec_SExpr(exprs__63, t347)
    ref_set__Ref_int32(i__61, next__65)
    goto b2
    b4:
    goto b1
}

func value_to_string(value__66 Value) string {
    var jp350 string
    var x58 int32
    var n__67 int32
    var t351 string
    var x59 bool
    var b__68 bool
    var t352 string
    switch value__66.(type) {
    case Value_Int:
        goto b2
    case Value_Bool:
        goto b3
    case Func:
        goto b4
    case Nil:
        goto b5
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp350
    b2:
    x58 = value__66.(Value_Int)._0
    n__67 = x58
    t351 = int32_to_string(n__67)
    jp350 = t351
    goto b1
    b3:
    x59 = value__66.(Value_Bool)._0
    b__68 = x59
    t352 = bool_to_string(b__68)
    jp350 = t352
    goto b1
    b4:
    jp350 = "<lambda>"
    goto b1
    b5:
    jp350 = "nil"
    goto b1
}

func truthy(value__69 Value) bool {
    var jp354 bool
    var x61 int32
    var n__71 int32
    var t355 bool
    var x62 bool
    var b__70 bool
    switch value__69.(type) {
    case Value_Int:
        goto b2
    case Value_Bool:
        goto b3
    case Func:
        goto b4
    case Nil:
        goto b5
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp354
    b2:
    x61 = value__69.(Value_Int)._0
    n__71 = x61
    t355 = n__71 != 0
    jp354 = t355
    goto b1
    b3:
    x62 = value__69.(Value_Bool)._0
    b__70 = x62
    jp354 = b__70
    goto b1
    b4:
    jp354 = true
    goto b1
    b5:
    jp354 = false
    goto b1
}

func eval(expr__72 SExpr, local__73 []Binding, global__74 *ref_vec_binding_x) Value {
    var jp357 Value
    var x64 int32
    var n__75 int32
    var t358 Value
    var x65 bool
    var b__76 bool
    var t359 Value
    var x66 string
    var name__77 string
    var t360 []Binding
    var t361 Value
    var x67 []SExpr
    var items__78 []SExpr
    var t362 Value
    switch expr__72.(type) {
    case SExpr_Int:
        goto b2
    case SExpr_Bool:
        goto b3
    case SExpr_Sym:
        goto b4
    case List:
        goto b5
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp357
    b2:
    x64 = expr__72.(SExpr_Int)._0
    n__75 = x64
    t358 = Value_Int{
        _0: n__75,
    }
    jp357 = t358
    goto b1
    b3:
    x65 = expr__72.(SExpr_Bool)._0
    b__76 = x65
    t359 = Value_Bool{
        _0: b__76,
    }
    jp357 = t359
    goto b1
    b4:
    x66 = expr__72.(SExpr_Sym)._0
    name__77 = x66
    t360 = ref_get__Ref_Vec_Binding(global__74)
    t361 = lookup(local__73, t360, name__77)
    jp357 = t361
    goto b1
    b5:
    x67 = expr__72.(List)._0
    items__78 = x67
    t362 = eval_list(items__78, local__73, global__74)
    jp357 = t362
    goto b1
}

func eval_list(items__79 []SExpr, local__80 []Binding, global__81 *ref_vec_binding_x) Value {
    var t365 int32
    var t366 bool
    var jp364 Value
    var head__82 SExpr
    var jp368 Value
    var f__84 Value
    var args__85 []Value
    var t369 Value
    var t370 Value
    var x70 string
    var name__83 string
    var t371 Value
    var t372 Value
    t365 = int32(len(items__79))
    t366 = t365 == 0
    if t366 {
        goto b2
    } else {
        goto b3
    }
    b1:
    return jp364
    b2:
    jp364 = Nil{}
    goto b1
    b3:
    head__82 = items__79[0]
    switch head__82.(type) {
    case SExpr_Int:
        goto b5
    case SExpr_Bool:
        goto b6
    case SExpr_Sym:
        goto b7
    case List:
        goto b8
    default:
        panic("non-exhaustive match")
    }
    b4:
    jp364 = jp368
    goto b1
    b5:
    f__84 = eval(head__82, local__80, global__81)
    args__85 = eval_args(items__79, 1, local__80, global__81)
    t369 = apply(f__84, args__85, global__81)
    jp368 = t369
    goto b4
    b6:
    f__84 = eval(head__82, local__80, global__81)
    args__85 = eval_args(items__79, 1, local__80, global__81)
    t370 = apply(f__84, args__85, global__81)
    jp368 = t370
    goto b4
    b7:
    x70 = head__82.(SExpr_Sym)._0
    name__83 = x70
    t371 = eval_list_sym(name__83, items__79, local__80, global__81)
    jp368 = t371
    goto b4
    b8:
    f__84 = eval(head__82, local__80, global__81)
    args__85 = eval_args(items__79, 1, local__80, global__81)
    t372 = apply(f__84, args__85, global__81)
    jp368 = t372
    goto b4
}

func eval_list_sym(name__86 string, items__87 []SExpr, local__88 []Binding, global__89 *ref_vec_binding_x) Value {
    var jp374 Value
    var t375 Value
    var t378 int32
    var t379 bool
    var jp377 Value
    var mtmp72 SExpr
    var jp381 Value
    var x75 string
    var var__90 string
    var t382 SExpr
    var value__91 Value
    var env__92 []Binding
    var t383 Binding
    var updated__93 []Binding
    var t386 int32
    var t387 bool
    var jp385 Value
    var t388 SExpr
    var cond__94 Value
    var t391 bool
    var jp390 Value
    var t392 SExpr
    var t393 Value
    var t394 SExpr
    var t395 Value
    var t398 int32
    var t399 bool
    var jp397 Value
    var mtmp78 SExpr
    var jp401 Value
    var x82 []SExpr
    var params_exprs__95 []SExpr
    var params__96 []string
    var body__97 SExpr
    var t402 Lambda
    var t403 Value
    var t404 []Value
    var t405 Value
    var t406 []Value
    var t407 Value
    var t408 []Value
    var t409 Value
    var t410 []Value
    var t411 Value
    var t412 []Value
    var t413 Value
    var t414 SExpr
    var f__98 Value
    var args__99 []Value
    var t415 Value
    switch name__86 {
    case "begin":
        goto b2
    case "define":
        goto b3
    case "if":
        goto b12
    case "lambda":
        goto b19
    case "+":
        goto b28
    case "-":
        goto b29
    case "*":
        goto b30
    case "/":
        goto b31
    case "=":
        goto b32
    default:
        goto b33
    }
    b1:
    return jp374
    b2:
    t375 = eval_begin(items__87, 1, local__88, global__89)
    jp374 = t375
    goto b1
    b3:
    t378 = int32(len(items__87))
    t379 = t378 == 3
    if t379 {
        goto b5
    } else {
        goto b11
    }
    b4:
    jp374 = jp377
    goto b1
    b5:
    mtmp72 = items__87[1]
    switch mtmp72.(type) {
    case SExpr_Int:
        goto b7
    case SExpr_Bool:
        goto b8
    case SExpr_Sym:
        goto b9
    case List:
        goto b10
    default:
        panic("non-exhaustive match")
    }
    b6:
    jp377 = jp381
    goto b4
    b7:
    jp381 = Nil{}
    goto b6
    b8:
    jp381 = Nil{}
    goto b6
    b9:
    x75 = mtmp72.(SExpr_Sym)._0
    var__90 = x75
    t382 = items__87[2]
    value__91 = eval(t382, local__88, global__89)
    env__92 = ref_get__Ref_Vec_Binding(global__89)
    t383 = Binding{
        name: var__90,
        value: value__91,
    }
    updated__93 = append(env__92, t383)
    ref_set__Ref_Vec_Binding(global__89, updated__93)
    jp381 = value__91
    goto b6
    b10:
    jp381 = Nil{}
    goto b6
    b11:
    jp377 = Nil{}
    goto b4
    b12:
    t386 = int32(len(items__87))
    t387 = t386 == 4
    if t387 {
        goto b14
    } else {
        goto b18
    }
    b13:
    jp374 = jp385
    goto b1
    b14:
    t388 = items__87[1]
    cond__94 = eval(t388, local__88, global__89)
    t391 = truthy(cond__94)
    if t391 {
        goto b16
    } else {
        goto b17
    }
    b15:
    jp385 = jp390
    goto b13
    b16:
    t392 = items__87[2]
    t393 = eval(t392, local__88, global__89)
    jp390 = t393
    goto b15
    b17:
    t394 = items__87[3]
    t395 = eval(t394, local__88, global__89)
    jp390 = t395
    goto b15
    b18:
    jp385 = Nil{}
    goto b13
    b19:
    t398 = int32(len(items__87))
    t399 = t398 == 3
    if t399 {
        goto b21
    } else {
        goto b27
    }
    b20:
    jp374 = jp397
    goto b1
    b21:
    mtmp78 = items__87[1]
    switch mtmp78.(type) {
    case SExpr_Int:
        goto b23
    case SExpr_Bool:
        goto b24
    case SExpr_Sym:
        goto b25
    case List:
        goto b26
    default:
        panic("non-exhaustive match")
    }
    b22:
    jp397 = jp401
    goto b20
    b23:
    jp401 = Nil{}
    goto b22
    b24:
    jp401 = Nil{}
    goto b22
    b25:
    jp401 = Nil{}
    goto b22
    b26:
    x82 = mtmp78.(List)._0
    params_exprs__95 = x82
    params__96 = params_from_sexprs(params_exprs__95)
    body__97 = items__87[2]
    t402 = Lambda{
        params: params__96,
        body: body__97,
        env: local__88,
        global: global__89,
    }
    t403 = Func{
        _0: t402,
    }
    jp401 = t403
    goto b22
    b27:
    jp397 = Nil{}
    goto b20
    b28:
    t404 = eval_args(items__87, 1, local__88, global__89)
    t405 = apply_builtin("+", t404)
    jp374 = t405
    goto b1
    b29:
    t406 = eval_args(items__87, 1, local__88, global__89)
    t407 = apply_builtin("-", t406)
    jp374 = t407
    goto b1
    b30:
    t408 = eval_args(items__87, 1, local__88, global__89)
    t409 = apply_builtin("*", t408)
    jp374 = t409
    goto b1
    b31:
    t410 = eval_args(items__87, 1, local__88, global__89)
    t411 = apply_builtin("/", t410)
    jp374 = t411
    goto b1
    b32:
    t412 = eval_args(items__87, 1, local__88, global__89)
    t413 = apply_builtin("=", t412)
    jp374 = t413
    goto b1
    b33:
    t414 = SExpr_Sym{
        _0: name__86,
    }
    f__98 = eval(t414, local__88, global__89)
    args__99 = eval_args(items__87, 1, local__88, global__89)
    t415 = apply(f__98, args__99, global__89)
    jp374 = t415
    goto b1
}

func eval_begin(items__100 []SExpr, start__101 int32, local__102 []Binding, global__103 *ref_vec_binding_x) Value {
    var i__104 *ref_int32_x
    var last__105 *ref_value_x
    var t417 Value
    var t419 int32
    var t420 int32
    var t421 bool
    var t422 int32
    var t423 SExpr
    var v__106 Value
    var t424 int32
    var t425 int32
    i__104 = ref__Ref_int32(start__101)
    last__105 = ref__Ref_Value(Nil{})
    goto b2
    b1:
    t417 = ref_get__Ref_Value(last__105)
    return t417
    b2:
    t419 = ref_get__Ref_int32(i__104)
    t420 = int32(len(items__100))
    t421 = t419 < t420
    if t421 {
        goto b3
    } else {
        goto b4
    }
    b3:
    t422 = ref_get__Ref_int32(i__104)
    t423 = items__100[t422]
    v__106 = eval(t423, local__102, global__103)
    ref_set__Ref_Value(last__105, v__106)
    t424 = ref_get__Ref_int32(i__104)
    t425 = t424 + 1
    ref_set__Ref_int32(i__104, t425)
    goto b2
    b4:
    goto b1
}

func params_from_sexprs(items__107 []SExpr) []string {
    var i__108 *ref_int32_x
    var acc__109 []string
    var params__110 *ref_vec_string_x
    var t428 []string
    var t430 int32
    var t431 int32
    var t432 bool
    var t433 int32
    var mtmp85 SExpr
    var t435 int32
    var t436 int32
    var t438 int32
    var t439 int32
    var x88 string
    var name__111 string
    var t441 []string
    var t442 []string
    var t443 int32
    var t444 int32
    var t446 int32
    var t447 int32
    i__108 = ref__Ref_int32(0)
    acc__109 = nil
    params__110 = ref__Ref_Vec_string(acc__109)
    goto b2
    b1:
    t428 = ref_get__Ref_Vec_string(params__110)
    return t428
    b2:
    t430 = ref_get__Ref_int32(i__108)
    t431 = int32(len(items__107))
    t432 = t430 < t431
    if t432 {
        goto b3
    } else {
        goto b9
    }
    b3:
    t433 = ref_get__Ref_int32(i__108)
    mtmp85 = items__107[t433]
    switch mtmp85.(type) {
    case SExpr_Int:
        goto b5
    case SExpr_Bool:
        goto b6
    case SExpr_Sym:
        goto b7
    case List:
        goto b8
    default:
        panic("non-exhaustive match")
    }
    b4:
    goto b2
    b5:
    t435 = ref_get__Ref_int32(i__108)
    t436 = t435 + 1
    ref_set__Ref_int32(i__108, t436)
    goto b4
    b6:
    t438 = ref_get__Ref_int32(i__108)
    t439 = t438 + 1
    ref_set__Ref_int32(i__108, t439)
    goto b4
    b7:
    x88 = mtmp85.(SExpr_Sym)._0
    name__111 = x88
    t441 = ref_get__Ref_Vec_string(params__110)
    t442 = append(t441, name__111)
    ref_set__Ref_Vec_string(params__110, t442)
    t443 = ref_get__Ref_int32(i__108)
    t444 = t443 + 1
    ref_set__Ref_int32(i__108, t444)
    goto b4
    b8:
    t446 = ref_get__Ref_int32(i__108)
    t447 = t446 + 1
    ref_set__Ref_int32(i__108, t447)
    goto b4
    b9:
    goto b1
}

func eval_args(items__112 []SExpr, start__113 int32, local__114 []Binding, global__115 *ref_vec_binding_x) []Value {
    var i__116 *ref_int32_x
    var acc__117 []Value
    var args__118 *ref_vec_value_x
    var t450 []Value
    var t452 int32
    var t453 int32
    var t454 bool
    var t455 int32
    var t456 SExpr
    var v__119 Value
    var t457 []Value
    var t458 []Value
    var t459 int32
    var t460 int32
    i__116 = ref__Ref_int32(start__113)
    acc__117 = nil
    args__118 = ref__Ref_Vec_Value(acc__117)
    goto b2
    b1:
    t450 = ref_get__Ref_Vec_Value(args__118)
    return t450
    b2:
    t452 = ref_get__Ref_int32(i__116)
    t453 = int32(len(items__112))
    t454 = t452 < t453
    if t454 {
        goto b3
    } else {
        goto b4
    }
    b3:
    t455 = ref_get__Ref_int32(i__116)
    t456 = items__112[t455]
    v__119 = eval(t456, local__114, global__115)
    t457 = ref_get__Ref_Vec_Value(args__118)
    t458 = append(t457, v__119)
    ref_set__Ref_Vec_Value(args__118, t458)
    t459 = ref_get__Ref_int32(i__116)
    t460 = t459 + 1
    ref_set__Ref_int32(i__116, t460)
    goto b2
    b4:
    goto b1
}

func apply_builtin(name__120 string, args__121 []Value) Value {
    var jp463 Value
    var t466 int32
    var t467 bool
    var jp465 Value
    var t468 Value
    var t469 Value
    var mtmp94 Tuple2_Value_Value
    var x95 Value
    var x96 Value
    var jp471 Value
    var x97 int32
    var jp473 Value
    var x100 int32
    var a__122 int32
    var b__123 int32
    var t474 bool
    var t475 Value
    var t476 Value
    var t477 Value
    var t478 Value
    var x98 bool
    var jp480 Value
    var t481 Value
    var x104 bool
    var a__124 bool
    var b__125 bool
    var t482 bool
    var t483 Value
    var t484 Value
    var t485 Value
    var t486 Value
    var t487 Value
    var t488 Value
    var i__126 *ref_int32_x
    var acc__127 *ref_int32_x
    var t490 int32
    var t491 Value
    var t493 int32
    var t494 int32
    var t495 bool
    var t496 int32
    var mtmp106 Value
    var x107 int32
    var n__128 int32
    var t498 int32
    var t499 int32
    var t500 int32
    var t501 int32
    var t503 int32
    var t504 int32
    var t506 int32
    var t507 int32
    var t509 int32
    var t510 int32
    var i__129 *ref_int32_x
    var acc__130 *ref_int32_x
    var t513 int32
    var t514 Value
    var t516 int32
    var t517 int32
    var t518 bool
    var t519 int32
    var mtmp112 Value
    var x113 int32
    var n__131 int32
    var t521 int32
    var t522 int32
    var t523 int32
    var t524 int32
    var t526 int32
    var t527 int32
    var t529 int32
    var t530 int32
    var t532 int32
    var t533 int32
    var mtmp118 int32
    var jp536 Value
    var mtmp119 Value
    var jp538 Value
    var x120 int32
    var n__132 int32
    var t539 int32
    var t540 Value
    var t541 Value
    var t542 Value
    var mtmp123 Tuple2_Value_Value
    var x124 Value
    var x125 Value
    var jp544 Value
    var x126 int32
    var jp546 Value
    var x129 int32
    var a__133 int32
    var b__134 int32
    var t547 int32
    var t548 Value
    var t551 int32
    var t552 bool
    var jp550 Value
    var t553 Value
    var t554 Value
    var mtmp132 Tuple2_Value_Value
    var x133 Value
    var x134 Value
    var jp556 Value
    var x135 int32
    var jp558 Value
    var x138 int32
    var a__135 int32
    var b__136 int32
    var t559 int32
    var t560 Value
    switch name__120 {
    case "=":
        goto b2
    case "+":
        goto b21
    case "*":
        goto b31
    case "-":
        goto b41
    case "/":
        goto b61
    default:
        goto b75
    }
    b1:
    return jp463
    b2:
    t466 = int32(len(args__121))
    t467 = t466 == 2
    if t467 {
        goto b4
    } else {
        goto b20
    }
    b3:
    jp463 = jp465
    goto b1
    b4:
    t468 = args__121[0]
    t469 = args__121[1]
    mtmp94 = Tuple2_Value_Value{
        _0: t468,
        _1: t469,
    }
    x95 = mtmp94._0
    x96 = mtmp94._1
    switch x96.(type) {
    case Value_Int:
        goto b6
    case Value_Bool:
        goto b12
    case Func:
        goto b18
    case Nil:
        goto b19
    default:
        panic("non-exhaustive match")
    }
    b5:
    jp465 = jp471
    goto b3
    b6:
    x97 = x96.(Value_Int)._0
    switch x95.(type) {
    case Value_Int:
        goto b8
    case Value_Bool:
        goto b9
    case Func:
        goto b10
    case Nil:
        goto b11
    default:
        panic("non-exhaustive match")
    }
    b7:
    jp471 = jp473
    goto b5
    b8:
    x100 = x95.(Value_Int)._0
    a__122 = x100
    b__123 = x97
    t474 = a__122 == b__123
    t475 = Value_Bool{
        _0: t474,
    }
    jp473 = t475
    goto b7
    b9:
    t476 = Value_Bool{
        _0: false,
    }
    jp473 = t476
    goto b7
    b10:
    t477 = Value_Bool{
        _0: false,
    }
    jp473 = t477
    goto b7
    b11:
    t478 = Value_Bool{
        _0: false,
    }
    jp473 = t478
    goto b7
    b12:
    x98 = x96.(Value_Bool)._0
    switch x95.(type) {
    case Value_Int:
        goto b14
    case Value_Bool:
        goto b15
    case Func:
        goto b16
    case Nil:
        goto b17
    default:
        panic("non-exhaustive match")
    }
    b13:
    jp471 = jp480
    goto b5
    b14:
    t481 = Value_Bool{
        _0: false,
    }
    jp480 = t481
    goto b13
    b15:
    x104 = x95.(Value_Bool)._0
    a__124 = x104
    b__125 = x98
    t482 = a__124 == b__125
    t483 = Value_Bool{
        _0: t482,
    }
    jp480 = t483
    goto b13
    b16:
    t484 = Value_Bool{
        _0: false,
    }
    jp480 = t484
    goto b13
    b17:
    t485 = Value_Bool{
        _0: false,
    }
    jp480 = t485
    goto b13
    b18:
    t486 = Value_Bool{
        _0: false,
    }
    jp471 = t486
    goto b5
    b19:
    t487 = Value_Bool{
        _0: false,
    }
    jp471 = t487
    goto b5
    b20:
    t488 = Value_Bool{
        _0: false,
    }
    jp465 = t488
    goto b3
    b21:
    i__126 = ref__Ref_int32(0)
    acc__127 = ref__Ref_int32(0)
    goto b23
    b22:
    t490 = ref_get__Ref_int32(acc__127)
    t491 = Value_Int{
        _0: t490,
    }
    jp463 = t491
    goto b1
    b23:
    t493 = ref_get__Ref_int32(i__126)
    t494 = int32(len(args__121))
    t495 = t493 < t494
    if t495 {
        goto b24
    } else {
        goto b30
    }
    b24:
    t496 = ref_get__Ref_int32(i__126)
    mtmp106 = args__121[t496]
    switch mtmp106.(type) {
    case Value_Int:
        goto b26
    case Value_Bool:
        goto b27
    case Func:
        goto b28
    case Nil:
        goto b29
    default:
        panic("non-exhaustive match")
    }
    b25:
    goto b23
    b26:
    x107 = mtmp106.(Value_Int)._0
    n__128 = x107
    t498 = ref_get__Ref_int32(acc__127)
    t499 = t498 + n__128
    ref_set__Ref_int32(acc__127, t499)
    t500 = ref_get__Ref_int32(i__126)
    t501 = t500 + 1
    ref_set__Ref_int32(i__126, t501)
    goto b25
    b27:
    t503 = ref_get__Ref_int32(i__126)
    t504 = t503 + 1
    ref_set__Ref_int32(i__126, t504)
    goto b25
    b28:
    t506 = ref_get__Ref_int32(i__126)
    t507 = t506 + 1
    ref_set__Ref_int32(i__126, t507)
    goto b25
    b29:
    t509 = ref_get__Ref_int32(i__126)
    t510 = t509 + 1
    ref_set__Ref_int32(i__126, t510)
    goto b25
    b30:
    goto b22
    b31:
    i__129 = ref__Ref_int32(0)
    acc__130 = ref__Ref_int32(1)
    goto b33
    b32:
    t513 = ref_get__Ref_int32(acc__130)
    t514 = Value_Int{
        _0: t513,
    }
    jp463 = t514
    goto b1
    b33:
    t516 = ref_get__Ref_int32(i__129)
    t517 = int32(len(args__121))
    t518 = t516 < t517
    if t518 {
        goto b34
    } else {
        goto b40
    }
    b34:
    t519 = ref_get__Ref_int32(i__129)
    mtmp112 = args__121[t519]
    switch mtmp112.(type) {
    case Value_Int:
        goto b36
    case Value_Bool:
        goto b37
    case Func:
        goto b38
    case Nil:
        goto b39
    default:
        panic("non-exhaustive match")
    }
    b35:
    goto b33
    b36:
    x113 = mtmp112.(Value_Int)._0
    n__131 = x113
    t521 = ref_get__Ref_int32(acc__130)
    t522 = t521 * n__131
    ref_set__Ref_int32(acc__130, t522)
    t523 = ref_get__Ref_int32(i__129)
    t524 = t523 + 1
    ref_set__Ref_int32(i__129, t524)
    goto b35
    b37:
    t526 = ref_get__Ref_int32(i__129)
    t527 = t526 + 1
    ref_set__Ref_int32(i__129, t527)
    goto b35
    b38:
    t529 = ref_get__Ref_int32(i__129)
    t530 = t529 + 1
    ref_set__Ref_int32(i__129, t530)
    goto b35
    b39:
    t532 = ref_get__Ref_int32(i__129)
    t533 = t532 + 1
    ref_set__Ref_int32(i__129, t533)
    goto b35
    b40:
    goto b32
    b41:
    mtmp118 = int32(len(args__121))
    switch mtmp118 {
    case 1:
        goto b43
    case 2:
        goto b49
    default:
        goto b60
    }
    b42:
    jp463 = jp536
    goto b1
    b43:
    mtmp119 = args__121[0]
    switch mtmp119.(type) {
    case Value_Int:
        goto b45
    case Value_Bool:
        goto b46
    case Func:
        goto b47
    case Nil:
        goto b48
    default:
        panic("non-exhaustive match")
    }
    b44:
    jp536 = jp538
    goto b42
    b45:
    x120 = mtmp119.(Value_Int)._0
    n__132 = x120
    t539 = 0 - n__132
    t540 = Value_Int{
        _0: t539,
    }
    jp538 = t540
    goto b44
    b46:
    jp538 = Nil{}
    goto b44
    b47:
    jp538 = Nil{}
    goto b44
    b48:
    jp538 = Nil{}
    goto b44
    b49:
    t541 = args__121[0]
    t542 = args__121[1]
    mtmp123 = Tuple2_Value_Value{
        _0: t541,
        _1: t542,
    }
    x124 = mtmp123._0
    x125 = mtmp123._1
    switch x125.(type) {
    case Value_Int:
        goto b51
    case Value_Bool:
        goto b57
    case Func:
        goto b58
    case Nil:
        goto b59
    default:
        panic("non-exhaustive match")
    }
    b50:
    jp536 = jp544
    goto b42
    b51:
    x126 = x125.(Value_Int)._0
    switch x124.(type) {
    case Value_Int:
        goto b53
    case Value_Bool:
        goto b54
    case Func:
        goto b55
    case Nil:
        goto b56
    default:
        panic("non-exhaustive match")
    }
    b52:
    jp544 = jp546
    goto b50
    b53:
    x129 = x124.(Value_Int)._0
    a__133 = x129
    b__134 = x126
    t547 = a__133 - b__134
    t548 = Value_Int{
        _0: t547,
    }
    jp546 = t548
    goto b52
    b54:
    jp546 = Nil{}
    goto b52
    b55:
    jp546 = Nil{}
    goto b52
    b56:
    jp546 = Nil{}
    goto b52
    b57:
    jp544 = Nil{}
    goto b50
    b58:
    jp544 = Nil{}
    goto b50
    b59:
    jp544 = Nil{}
    goto b50
    b60:
    jp536 = Nil{}
    goto b42
    b61:
    t551 = int32(len(args__121))
    t552 = t551 == 2
    if t552 {
        goto b63
    } else {
        goto b74
    }
    b62:
    jp463 = jp550
    goto b1
    b63:
    t553 = args__121[0]
    t554 = args__121[1]
    mtmp132 = Tuple2_Value_Value{
        _0: t553,
        _1: t554,
    }
    x133 = mtmp132._0
    x134 = mtmp132._1
    switch x134.(type) {
    case Value_Int:
        goto b65
    case Value_Bool:
        goto b71
    case Func:
        goto b72
    case Nil:
        goto b73
    default:
        panic("non-exhaustive match")
    }
    b64:
    jp550 = jp556
    goto b62
    b65:
    x135 = x134.(Value_Int)._0
    switch x133.(type) {
    case Value_Int:
        goto b67
    case Value_Bool:
        goto b68
    case Func:
        goto b69
    case Nil:
        goto b70
    default:
        panic("non-exhaustive match")
    }
    b66:
    jp556 = jp558
    goto b64
    b67:
    x138 = x133.(Value_Int)._0
    a__135 = x138
    b__136 = x135
    t559 = a__135 / b__136
    t560 = Value_Int{
        _0: t559,
    }
    jp558 = t560
    goto b66
    b68:
    jp558 = Nil{}
    goto b66
    b69:
    jp558 = Nil{}
    goto b66
    b70:
    jp558 = Nil{}
    goto b66
    b71:
    jp556 = Nil{}
    goto b64
    b72:
    jp556 = Nil{}
    goto b64
    b73:
    jp556 = Nil{}
    goto b64
    b74:
    jp550 = Nil{}
    goto b62
    b75:
    jp463 = Nil{}
    goto b1
}

func apply(func__137 Value, args__138 []Value, global__139 *ref_vec_binding_x) Value {
    var jp562 Value
    var x143 Lambda
    var fun__140 Lambda
    var t563 Value
    switch func__137.(type) {
    case Value_Int:
        goto b2
    case Value_Bool:
        goto b3
    case Func:
        goto b4
    case Nil:
        goto b5
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp562
    b2:
    jp562 = Nil{}
    goto b1
    b3:
    jp562 = Nil{}
    goto b1
    b4:
    x143 = func__137.(Func)._0
    fun__140 = x143
    t563 = apply_lambda(fun__140, args__138)
    jp562 = t563
    goto b1
    b5:
    jp562 = Nil{}
    goto b1
}

func apply_lambda(lambda__141 Lambda, args__142 []Value) Value {
    var t564 []Binding
    var env__143 *ref_vec_binding_x
    var i__144 *ref_int32_x
    var t566 SExpr
    var t567 []Binding
    var t568 *ref_vec_binding_x
    var t569 Value
    var t571 int32
    var t572 []string
    var t573 int32
    var t574 bool
    var t575 int32
    var t576 int32
    var t577 bool
    var t578 bool
    var t579 []string
    var t580 int32
    var name__145 string
    var t581 int32
    var value__146 Value
    var t582 []Binding
    var t583 Binding
    var updated__147 []Binding
    var t584 int32
    var t585 int32
    t564 = lambda__141.env
    env__143 = ref__Ref_Vec_Binding(t564)
    i__144 = ref__Ref_int32(0)
    goto b2
    b1:
    t566 = lambda__141.body
    t567 = ref_get__Ref_Vec_Binding(env__143)
    t568 = lambda__141.global
    t569 = eval(t566, t567, t568)
    return t569
    b2:
    t571 = ref_get__Ref_int32(i__144)
    t572 = lambda__141.params
    t573 = int32(len(t572))
    t574 = t571 < t573
    t575 = ref_get__Ref_int32(i__144)
    t576 = int32(len(args__142))
    t577 = t575 < t576
    t578 = t574 && t577
    if t578 {
        goto b3
    } else {
        goto b4
    }
    b3:
    t579 = lambda__141.params
    t580 = ref_get__Ref_int32(i__144)
    name__145 = t579[t580]
    t581 = ref_get__Ref_int32(i__144)
    value__146 = args__142[t581]
    t582 = ref_get__Ref_Vec_Binding(env__143)
    t583 = Binding{
        name: name__145,
        value: value__146,
    }
    updated__147 = append(t582, t583)
    ref_set__Ref_Vec_Binding(env__143, updated__147)
    t584 = ref_get__Ref_int32(i__144)
    t585 = t584 + 1
    ref_set__Ref_int32(i__144, t585)
    goto b2
    b4:
    goto b1
}

func main0() struct{} {
    var t587 []Binding
    var global__148 *ref_vec_binding_x
    var program__149 string
    var t588 []Token
    var exprs__150 []SExpr
    var t589 SExpr
    var t590 []Binding
    var result__151 Value
    var t591 string
    var t592 []Token
    var exprs2__152 []SExpr
    var t593 SExpr
    var t594 []Binding
    var result2__153 Value
    var t595 string
    t587 = nil
    global__148 = ref__Ref_Vec_Binding(t587)
    program__149 = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    t588 = lex(program__149)
    exprs__150 = parse_program(t588)
    t589 = exprs__150[0]
    t590 = nil
    result__151 = eval(t589, t590, global__148)
    t591 = value_to_string(result__151)
    string_println(t591)
    t592 = lex("(add3 10 20 30)")
    exprs2__152 = parse_program(t592)
    t593 = exprs2__152[0]
    t594 = nil
    result2__153 = eval(t593, t594, global__148)
    t595 = value_to_string(result2__153)
    string_println(t595)
    return struct{}{}
}

func main() {
    main0()
}
