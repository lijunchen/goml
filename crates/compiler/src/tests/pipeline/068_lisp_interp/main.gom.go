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
    var ret399 bool
    var t158 bool = ch__0 >= 48
    var t159 bool = ch__0 <= 57
    ret399 = t158 && t159
    return ret399
}

func digit_value(ch__1 rune) int32 {
    var ret400 int32
    switch ch__1 {
    case 48:
        ret400 = 0
    case 49:
        ret400 = 1
    case 50:
        ret400 = 2
    case 51:
        ret400 = 3
    case 52:
        ret400 = 4
    case 53:
        ret400 = 5
    case 54:
        ret400 = 6
    case 55:
        ret400 = 7
    case 56:
        ret400 = 8
    case 57:
        ret400 = 9
    default:
        ret400 = 0
    }
    return ret400
}

func is_int_text(text__2 string) bool {
    var ret401 bool
    var len__3 int32 = string_len(text__2)
    var t160 bool = len__3 == 0
    if t160 {
        ret401 = false
    } else {
        var i__4 *ref_int32_x = ref__Ref_int32(0)
        var saw_digit__5 *ref_bool_x = ref__Ref_bool(false)
        var ok__6 *ref_bool_x = ref__Ref_bool(true)
        var started__7 *ref_bool_x = ref__Ref_bool(false)
        var cond402 bool
        for {
            var t161 bool = ref_get__Ref_bool(ok__6)
            var t163 int32 = ref_get__Ref_int32(i__4)
            var t162 bool = t163 < len__3
            cond402 = t161 && t162
            if !cond402 {
                break
            }
            var t164 int32 = ref_get__Ref_int32(i__4)
            var ch__8 rune = string_get(text__2, t164)
            var t167 bool = ref_get__Ref_bool(started__7)
            var t166 bool = !t167
            var t168 bool = ch__8 == 45
            var t165 bool = t166 && t168
            if t165 {
                ref_set__Ref_bool(started__7, true)
                var t170 int32 = ref_get__Ref_int32(i__4)
                var t169 int32 = t170 + 1
                ref_set__Ref_int32(i__4, t169)
            } else {
                var t171 bool = is_digit(ch__8)
                if t171 {
                    ref_set__Ref_bool(started__7, true)
                    ref_set__Ref_bool(saw_digit__5, true)
                    var t173 int32 = ref_get__Ref_int32(i__4)
                    var t172 int32 = t173 + 1
                    ref_set__Ref_int32(i__4, t172)
                } else {
                    ref_set__Ref_bool(ok__6, false)
                }
            }
        }
        var t174 bool = ref_get__Ref_bool(ok__6)
        var t175 bool = ref_get__Ref_bool(saw_digit__5)
        ret401 = t174 && t175
    }
    return ret401
}

func parse_int32(text__9 string) int32 {
    var ret403 int32
    var len__10 int32 = string_len(text__9)
    var i__11 *ref_int32_x = ref__Ref_int32(0)
    var negative__12 *ref_bool_x = ref__Ref_bool(false)
    var started__13 *ref_bool_x = ref__Ref_bool(false)
    var acc__14 *ref_int32_x = ref__Ref_int32(0)
    var cond404 bool
    for {
        var t176 int32 = ref_get__Ref_int32(i__11)
        cond404 = t176 < len__10
        if !cond404 {
            break
        }
        var t177 int32 = ref_get__Ref_int32(i__11)
        var ch__15 rune = string_get(text__9, t177)
        var t180 bool = ref_get__Ref_bool(started__13)
        var t179 bool = !t180
        var t181 bool = ch__15 == 45
        var t178 bool = t179 && t181
        if t178 {
            ref_set__Ref_bool(started__13, true)
            ref_set__Ref_bool(negative__12, true)
            var t183 int32 = ref_get__Ref_int32(i__11)
            var t182 int32 = t183 + 1
            ref_set__Ref_int32(i__11, t182)
        } else {
            ref_set__Ref_bool(started__13, true)
            var d__16 int32 = digit_value(ch__15)
            var t186 int32 = ref_get__Ref_int32(acc__14)
            var t185 int32 = t186 * 10
            var t184 int32 = t185 + d__16
            ref_set__Ref_int32(acc__14, t184)
            var t188 int32 = ref_get__Ref_int32(i__11)
            var t187 int32 = t188 + 1
            ref_set__Ref_int32(i__11, t187)
        }
    }
    var t189 bool = ref_get__Ref_bool(negative__12)
    if t189 {
        var t190 int32 = ref_get__Ref_int32(acc__14)
        ret403 = 0 - t190
    } else {
        ret403 = ref_get__Ref_int32(acc__14)
    }
    return ret403
}

func is_delim(ch__17 rune) bool {
    var ret405 bool
    var t192 bool = ch__17 == 40
    var t193 bool = ch__17 == 41
    var t191 bool = t192 || t193
    var t194 bool = ch__17 == 32
    ret405 = t191 || t194
    return ret405
}

func lex_atom(source__18 string, start__19 int32) Tuple2_Token_int32 {
    var ret406 Tuple2_Token_int32
    var len__20 int32 = string_len(source__18)
    var text__21 *ref_string_x = ref__Ref_string("")
    var i__22 *ref_int32_x = ref__Ref_int32(start__19)
    var done__23 *ref_bool_x = ref__Ref_bool(false)
    var cond407 bool
    for {
        var t196 bool = ref_get__Ref_bool(done__23)
        var t195 bool = !t196
        var t198 int32 = ref_get__Ref_int32(i__22)
        var t197 bool = t198 < len__20
        cond407 = t195 && t197
        if !cond407 {
            break
        }
        var t199 int32 = ref_get__Ref_int32(i__22)
        var ch__24 rune = string_get(source__18, t199)
        var t200 bool = is_delim(ch__24)
        if t200 {
            ref_set__Ref_bool(done__23, true)
        } else {
            var t202 string = ref_get__Ref_string(text__21)
            var t203 string = char_to_string(ch__24)
            var t201 string = t202 + t203
            ref_set__Ref_string(text__21, t201)
            var t205 int32 = ref_get__Ref_int32(i__22)
            var t204 int32 = t205 + 1
            ref_set__Ref_int32(i__22, t204)
        }
    }
    var atom__25 string = ref_get__Ref_string(text__21)
    var token__26 Token
    switch atom__25 {
    case "true":
        token__26 = Token_Bool{
            _0: true,
        }
    case "false":
        token__26 = Token_Bool{
            _0: false,
        }
    default:
        var t206 bool = is_int_text(atom__25)
        if t206 {
            var t207 int32 = parse_int32(atom__25)
            token__26 = Token_Int{
                _0: t207,
            }
        } else {
            token__26 = Token_Sym{
                _0: atom__25,
            }
        }
    }
    var t208 int32 = ref_get__Ref_int32(i__22)
    ret406 = Tuple2_Token_int32{
        _0: token__26,
        _1: t208,
    }
    return ret406
}

func lex(source__27 string) []Token {
    var ret408 []Token
    var len__28 int32 = string_len(source__27)
    var toks0__29 []Token = nil
    var toks__30 *ref_vec_token_x = ref__Ref_Vec_Token(toks0__29)
    var i__31 *ref_int32_x = ref__Ref_int32(0)
    var cond409 bool
    for {
        var t209 int32 = ref_get__Ref_int32(i__31)
        cond409 = t209 < len__28
        if !cond409 {
            break
        }
        var t210 int32 = ref_get__Ref_int32(i__31)
        var ch__32 rune = string_get(source__27, t210)
        var t211 bool = ch__32 == 40
        if t211 {
            var t213 []Token = ref_get__Ref_Vec_Token(toks__30)
            var t214 Token = LParen{}
            var t212 []Token = append(t213, t214)
            ref_set__Ref_Vec_Token(toks__30, t212)
            var t216 int32 = ref_get__Ref_int32(i__31)
            var t215 int32 = t216 + 1
            ref_set__Ref_int32(i__31, t215)
        } else {
            var t217 bool = ch__32 == 41
            if t217 {
                var t219 []Token = ref_get__Ref_Vec_Token(toks__30)
                var t220 Token = RParen{}
                var t218 []Token = append(t219, t220)
                ref_set__Ref_Vec_Token(toks__30, t218)
                var t222 int32 = ref_get__Ref_int32(i__31)
                var t221 int32 = t222 + 1
                ref_set__Ref_int32(i__31, t221)
            } else {
                var t223 bool = ch__32 == 32
                if t223 {
                    var t225 int32 = ref_get__Ref_int32(i__31)
                    var t224 int32 = t225 + 1
                    ref_set__Ref_int32(i__31, t224)
                } else {
                    var t226 int32 = ref_get__Ref_int32(i__31)
                    var mtmp17 Tuple2_Token_int32 = lex_atom(source__27, t226)
                    var x18 Token = mtmp17._0
                    var x19 int32 = mtmp17._1
                    var next__34 int32 = x19
                    var tok__33 Token = x18
                    var t228 []Token = ref_get__Ref_Vec_Token(toks__30)
                    var t227 []Token = append(t228, tok__33)
                    ref_set__Ref_Vec_Token(toks__30, t227)
                    ref_set__Ref_int32(i__31, next__34)
                }
            }
        }
    }
    ret408 = ref_get__Ref_Vec_Token(toks__30)
    return ret408
}

func env_lookup(env__35 []Binding, name__36 string) Value {
    var ret410 Value
    var t230 int32 = int32(len(env__35))
    var t229 int32 = t230 - 1
    var i__37 *ref_int32_x = ref__Ref_int32(t229)
    var t231 Value = Nil{}
    var result__38 *ref_value_x = ref__Ref_Value(t231)
    var done__39 *ref_bool_x = ref__Ref_bool(false)
    var cond411 bool
    for {
        var t233 bool = ref_get__Ref_bool(done__39)
        var t232 bool = !t233
        var t235 int32 = ref_get__Ref_int32(i__37)
        var t234 bool = t235 >= 0
        cond411 = t232 && t234
        if !cond411 {
            break
        }
        var t236 int32 = ref_get__Ref_int32(i__37)
        var binding__40 Binding = env__35[t236]
        var t238 string = binding__40.name
        var t237 bool = t238 == name__36
        if t237 {
            var t239 Value = binding__40.value
            ref_set__Ref_Value(result__38, t239)
            ref_set__Ref_bool(done__39, true)
        } else {
            var t241 int32 = ref_get__Ref_int32(i__37)
            var t240 int32 = t241 - 1
            ref_set__Ref_int32(i__37, t240)
        }
    }
    ret410 = ref_get__Ref_Value(result__38)
    return ret410
}

func lookup(local__41 []Binding, global__42 []Binding, name__43 string) Value {
    var ret412 Value
    var mtmp24 Value = env_lookup(local__41, name__43)
    switch mtmp24 := mtmp24.(type) {
    case Value_Int:
        var other__44 Value = mtmp24
        ret412 = other__44
    case Value_Bool:
        var other__44 Value = mtmp24
        ret412 = other__44
    case Func:
        var other__44 Value = mtmp24
        ret412 = other__44
    case Nil:
        ret412 = env_lookup(global__42, name__43)
    }
    return ret412
}

func parse_list(tokens__45 []Token, start__46 int32) Tuple2_Vec_SExpr_int32 {
    var ret413 Tuple2_Vec_SExpr_int32
    var acc__47 []SExpr = nil
    var exprs__48 *ref_vec_sexpr_x = ref__Ref_Vec_SExpr(acc__47)
    var i__49 *ref_int32_x = ref__Ref_int32(start__46)
    var done__50 *ref_bool_x = ref__Ref_bool(false)
    var cond414 bool
    for {
        var t243 bool = ref_get__Ref_bool(done__50)
        var t242 bool = !t243
        var t245 int32 = ref_get__Ref_int32(i__49)
        var t246 int32 = int32(len(tokens__45))
        var t244 bool = t245 < t246
        cond414 = t242 && t244
        if !cond414 {
            break
        }
        var t247 int32 = ref_get__Ref_int32(i__49)
        var mtmp28 Token = tokens__45[t247]
        switch mtmp28.(type) {
        case LParen:
            var t248 int32 = ref_get__Ref_int32(i__49)
            var mtmp33 Tuple2_SExpr_int32 = parse_expr(tokens__45, t248)
            var x34 SExpr = mtmp33._0
            var x35 int32 = mtmp33._1
            var next__52 int32 = x35
            var expr__51 SExpr = x34
            var t250 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t249 []SExpr = append(t250, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t249)
            ref_set__Ref_int32(i__49, next__52)
        case RParen:
            ref_set__Ref_bool(done__50, true)
            var t252 int32 = ref_get__Ref_int32(i__49)
            var t251 int32 = t252 + 1
            ref_set__Ref_int32(i__49, t251)
        case Token_Sym:
            var t253 int32 = ref_get__Ref_int32(i__49)
            var mtmp39 Tuple2_SExpr_int32 = parse_expr(tokens__45, t253)
            var x40 SExpr = mtmp39._0
            var x41 int32 = mtmp39._1
            var next__52 int32 = x41
            var expr__51 SExpr = x40
            var t255 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t254 []SExpr = append(t255, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t254)
            ref_set__Ref_int32(i__49, next__52)
        case Token_Int:
            var t256 int32 = ref_get__Ref_int32(i__49)
            var mtmp44 Tuple2_SExpr_int32 = parse_expr(tokens__45, t256)
            var x45 SExpr = mtmp44._0
            var x46 int32 = mtmp44._1
            var next__52 int32 = x46
            var expr__51 SExpr = x45
            var t258 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t257 []SExpr = append(t258, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t257)
            ref_set__Ref_int32(i__49, next__52)
        case Token_Bool:
            var t259 int32 = ref_get__Ref_int32(i__49)
            var mtmp49 Tuple2_SExpr_int32 = parse_expr(tokens__45, t259)
            var x50 SExpr = mtmp49._0
            var x51 int32 = mtmp49._1
            var next__52 int32 = x51
            var expr__51 SExpr = x50
            var t261 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
            var t260 []SExpr = append(t261, expr__51)
            ref_set__Ref_Vec_SExpr(exprs__48, t260)
            ref_set__Ref_int32(i__49, next__52)
        }
    }
    var t262 []SExpr = ref_get__Ref_Vec_SExpr(exprs__48)
    var t263 int32 = ref_get__Ref_int32(i__49)
    ret413 = Tuple2_Vec_SExpr_int32{
        _0: t262,
        _1: t263,
    }
    return ret413
}

func parse_expr(tokens__53 []Token, start__54 int32) Tuple2_SExpr_int32 {
    var ret415 Tuple2_SExpr_int32
    var mtmp54 Token = tokens__53[start__54]
    switch mtmp54 := mtmp54.(type) {
    case LParen:
        var t264 int32 = start__54 + 1
        var mtmp58 Tuple2_Vec_SExpr_int32 = parse_list(tokens__53, t264)
        var x59 []SExpr = mtmp58._0
        var x60 int32 = mtmp58._1
        var next__56 int32 = x60
        var items__55 []SExpr = x59
        var t265 SExpr = List{
            _0: items__55,
        }
        ret415 = Tuple2_SExpr_int32{
            _0: t265,
            _1: next__56,
        }
    case RParen:
        var t266 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t267 int32 = start__54 + 1
        ret415 = Tuple2_SExpr_int32{
            _0: t266,
            _1: t267,
        }
    case Token_Sym:
        var x55 string = mtmp54._0
        var name__59 string = x55
        var t268 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t269 int32 = start__54 + 1
        ret415 = Tuple2_SExpr_int32{
            _0: t268,
            _1: t269,
        }
    case Token_Int:
        var x56 int32 = mtmp54._0
        var n__58 int32 = x56
        var t270 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t271 int32 = start__54 + 1
        ret415 = Tuple2_SExpr_int32{
            _0: t270,
            _1: t271,
        }
    case Token_Bool:
        var x57 bool = mtmp54._0
        var b__57 bool = x57
        var t272 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t273 int32 = start__54 + 1
        ret415 = Tuple2_SExpr_int32{
            _0: t272,
            _1: t273,
        }
    }
    return ret415
}

func parse_program(tokens__60 []Token) []SExpr {
    var ret416 []SExpr
    var i__61 *ref_int32_x = ref__Ref_int32(0)
    var acc__62 []SExpr = nil
    var exprs__63 *ref_vec_sexpr_x = ref__Ref_Vec_SExpr(acc__62)
    var cond417 bool
    for {
        var t274 int32 = ref_get__Ref_int32(i__61)
        var t275 int32 = int32(len(tokens__60))
        cond417 = t274 < t275
        if !cond417 {
            break
        }
        var t276 int32 = ref_get__Ref_int32(i__61)
        var mtmp62 Tuple2_SExpr_int32 = parse_expr(tokens__60, t276)
        var x63 SExpr = mtmp62._0
        var x64 int32 = mtmp62._1
        var next__65 int32 = x64
        var expr__64 SExpr = x63
        var t278 []SExpr = ref_get__Ref_Vec_SExpr(exprs__63)
        var t277 []SExpr = append(t278, expr__64)
        ref_set__Ref_Vec_SExpr(exprs__63, t277)
        ref_set__Ref_int32(i__61, next__65)
    }
    ret416 = ref_get__Ref_Vec_SExpr(exprs__63)
    return ret416
}

func value_to_string(value__66 Value) string {
    var ret418 string
    switch value__66 := value__66.(type) {
    case Value_Int:
        var x67 int32 = value__66._0
        var n__67 int32 = x67
        ret418 = int32_to_string(n__67)
    case Value_Bool:
        var x68 bool = value__66._0
        var b__68 bool = x68
        ret418 = bool_to_string(b__68)
    case Func:
        ret418 = "<lambda>"
    case Nil:
        ret418 = "nil"
    }
    return ret418
}

func truthy(value__69 Value) bool {
    var ret419 bool
    switch value__69 := value__69.(type) {
    case Value_Int:
        var x70 int32 = value__69._0
        var n__71 int32 = x70
        ret419 = n__71 != 0
    case Value_Bool:
        var x71 bool = value__69._0
        var b__70 bool = x71
        ret419 = b__70
    case Func:
        ret419 = true
    case Nil:
        ret419 = false
    }
    return ret419
}

func eval(expr__72 SExpr, local__73 []Binding, global__74 *ref_vec_binding_x) Value {
    var ret420 Value
    switch expr__72 := expr__72.(type) {
    case SExpr_Int:
        var x73 int32 = expr__72._0
        var n__75 int32 = x73
        ret420 = Value_Int{
            _0: n__75,
        }
    case SExpr_Bool:
        var x74 bool = expr__72._0
        var b__76 bool = x74
        ret420 = Value_Bool{
            _0: b__76,
        }
    case SExpr_Sym:
        var x75 string = expr__72._0
        var name__77 string = x75
        var t279 []Binding = ref_get__Ref_Vec_Binding(global__74)
        ret420 = lookup(local__73, t279, name__77)
    case List:
        var x76 []SExpr = expr__72._0
        var items__78 []SExpr = x76
        ret420 = eval_list(items__78, local__73, global__74)
    }
    return ret420
}

func eval_list(items__79 []SExpr, local__80 []Binding, global__81 *ref_vec_binding_x) Value {
    var ret421 Value
    var t281 int32 = int32(len(items__79))
    var t280 bool = t281 == 0
    if t280 {
        ret421 = Nil{}
    } else {
        var head__82 SExpr = items__79[0]
        switch head__82 := head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            ret421 = apply(f__84, args__85, global__81)
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            ret421 = apply(f__84, args__85, global__81)
        case SExpr_Sym:
            var x79 string = head__82._0
            var name__83 string = x79
            ret421 = eval_list_sym(name__83, items__79, local__80, global__81)
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 []Value = eval_args(items__79, 1, local__80, global__81)
            ret421 = apply(f__84, args__85, global__81)
        }
    }
    return ret421
}

func eval_list_sym(name__86 string, items__87 []SExpr, local__88 []Binding, global__89 *ref_vec_binding_x) Value {
    var ret422 Value
    switch name__86 {
    case "begin":
        ret422 = eval_begin(items__87, 1, local__88, global__89)
    case "define":
        var t283 int32 = int32(len(items__87))
        var t282 bool = t283 == 3
        if t282 {
            var mtmp81 SExpr = items__87[1]
            switch mtmp81 := mtmp81.(type) {
            case SExpr_Int:
                ret422 = Nil{}
            case SExpr_Bool:
                ret422 = Nil{}
            case SExpr_Sym:
                var x84 string = mtmp81._0
                var var__90 string = x84
                var t284 SExpr = items__87[2]
                var value__91 Value = eval(t284, local__88, global__89)
                var env__92 []Binding = ref_get__Ref_Vec_Binding(global__89)
                var t285 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 []Binding = append(env__92, t285)
                ref_set__Ref_Vec_Binding(global__89, updated__93)
                ret422 = value__91
            case List:
                ret422 = Nil{}
            }
        } else {
            ret422 = Nil{}
        }
    case "if":
        var t287 int32 = int32(len(items__87))
        var t286 bool = t287 == 4
        if t286 {
            var t288 SExpr = items__87[1]
            var cond__94 Value = eval(t288, local__88, global__89)
            var t289 bool = truthy(cond__94)
            if t289 {
                var t290 SExpr = items__87[2]
                ret422 = eval(t290, local__88, global__89)
            } else {
                var t291 SExpr = items__87[3]
                ret422 = eval(t291, local__88, global__89)
            }
        } else {
            ret422 = Nil{}
        }
    case "lambda":
        var t293 int32 = int32(len(items__87))
        var t292 bool = t293 == 3
        if t292 {
            var mtmp87 SExpr = items__87[1]
            switch mtmp87 := mtmp87.(type) {
            case SExpr_Int:
                ret422 = Nil{}
            case SExpr_Bool:
                ret422 = Nil{}
            case SExpr_Sym:
                ret422 = Nil{}
            case List:
                var x91 []SExpr = mtmp87._0
                var params_exprs__95 []SExpr = x91
                var params__96 []string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = items__87[2]
                var t294 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                ret422 = Func{
                    _0: t294,
                }
            }
        } else {
            ret422 = Nil{}
        }
    case "+":
        var t295 []Value = eval_args(items__87, 1, local__88, global__89)
        ret422 = apply_builtin("+", t295)
    case "-":
        var t296 []Value = eval_args(items__87, 1, local__88, global__89)
        ret422 = apply_builtin("-", t296)
    case "*":
        var t297 []Value = eval_args(items__87, 1, local__88, global__89)
        ret422 = apply_builtin("*", t297)
    case "/":
        var t298 []Value = eval_args(items__87, 1, local__88, global__89)
        ret422 = apply_builtin("/", t298)
    case "=":
        var t299 []Value = eval_args(items__87, 1, local__88, global__89)
        ret422 = apply_builtin("=", t299)
    default:
        var t300 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t300, local__88, global__89)
        var args__99 []Value = eval_args(items__87, 1, local__88, global__89)
        ret422 = apply(f__98, args__99, global__89)
    }
    return ret422
}

func eval_begin(items__100 []SExpr, start__101 int32, local__102 []Binding, global__103 *ref_vec_binding_x) Value {
    var ret423 Value
    var i__104 *ref_int32_x = ref__Ref_int32(start__101)
    var t301 Value = Nil{}
    var last__105 *ref_value_x = ref__Ref_Value(t301)
    var cond424 bool
    for {
        var t302 int32 = ref_get__Ref_int32(i__104)
        var t303 int32 = int32(len(items__100))
        cond424 = t302 < t303
        if !cond424 {
            break
        }
        var t305 int32 = ref_get__Ref_int32(i__104)
        var t304 SExpr = items__100[t305]
        var v__106 Value = eval(t304, local__102, global__103)
        ref_set__Ref_Value(last__105, v__106)
        var t307 int32 = ref_get__Ref_int32(i__104)
        var t306 int32 = t307 + 1
        ref_set__Ref_int32(i__104, t306)
    }
    ret423 = ref_get__Ref_Value(last__105)
    return ret423
}

func params_from_sexprs(items__107 []SExpr) []string {
    var ret425 []string
    var i__108 *ref_int32_x = ref__Ref_int32(0)
    var acc__109 []string = nil
    var params__110 *ref_vec_string_x = ref__Ref_Vec_string(acc__109)
    var cond426 bool
    for {
        var t308 int32 = ref_get__Ref_int32(i__108)
        var t309 int32 = int32(len(items__107))
        cond426 = t308 < t309
        if !cond426 {
            break
        }
        var t310 int32 = ref_get__Ref_int32(i__108)
        var mtmp94 SExpr = items__107[t310]
        switch mtmp94 := mtmp94.(type) {
        case SExpr_Int:
            var t312 int32 = ref_get__Ref_int32(i__108)
            var t311 int32 = t312 + 1
            ref_set__Ref_int32(i__108, t311)
        case SExpr_Bool:
            var t314 int32 = ref_get__Ref_int32(i__108)
            var t313 int32 = t314 + 1
            ref_set__Ref_int32(i__108, t313)
        case SExpr_Sym:
            var x97 string = mtmp94._0
            var name__111 string = x97
            var t316 []string = ref_get__Ref_Vec_string(params__110)
            var t315 []string = append(t316, name__111)
            ref_set__Ref_Vec_string(params__110, t315)
            var t318 int32 = ref_get__Ref_int32(i__108)
            var t317 int32 = t318 + 1
            ref_set__Ref_int32(i__108, t317)
        case List:
            var t320 int32 = ref_get__Ref_int32(i__108)
            var t319 int32 = t320 + 1
            ref_set__Ref_int32(i__108, t319)
        }
    }
    ret425 = ref_get__Ref_Vec_string(params__110)
    return ret425
}

func eval_args(items__112 []SExpr, start__113 int32, local__114 []Binding, global__115 *ref_vec_binding_x) []Value {
    var ret427 []Value
    var i__116 *ref_int32_x = ref__Ref_int32(start__113)
    var acc__117 []Value = nil
    var args__118 *ref_vec_value_x = ref__Ref_Vec_Value(acc__117)
    var cond428 bool
    for {
        var t321 int32 = ref_get__Ref_int32(i__116)
        var t322 int32 = int32(len(items__112))
        cond428 = t321 < t322
        if !cond428 {
            break
        }
        var t324 int32 = ref_get__Ref_int32(i__116)
        var t323 SExpr = items__112[t324]
        var v__119 Value = eval(t323, local__114, global__115)
        var t326 []Value = ref_get__Ref_Vec_Value(args__118)
        var t325 []Value = append(t326, v__119)
        ref_set__Ref_Vec_Value(args__118, t325)
        var t328 int32 = ref_get__Ref_int32(i__116)
        var t327 int32 = t328 + 1
        ref_set__Ref_int32(i__116, t327)
    }
    ret427 = ref_get__Ref_Vec_Value(args__118)
    return ret427
}

func apply_builtin(name__120 string, args__121 []Value) Value {
    var ret429 Value
    switch name__120 {
    case "=":
        var t330 int32 = int32(len(args__121))
        var t329 bool = t330 == 2
        if t329 {
            var t331 Value = args__121[0]
            var t332 Value = args__121[1]
            var mtmp103 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t331,
                _1: t332,
            }
            var x104 Value = mtmp103._0
            var x105 Value = mtmp103._1
            switch x105 := x105.(type) {
            case Value_Int:
                var x106 int32 = x105._0
                switch x104 := x104.(type) {
                case Value_Int:
                    var x109 int32 = x104._0
                    var a__122 int32 = x109
                    var b__123 int32 = x106
                    var t333 bool = a__122 == b__123
                    ret429 = Value_Bool{
                        _0: t333,
                    }
                case Value_Bool:
                    ret429 = Value_Bool{
                        _0: false,
                    }
                case Func:
                    ret429 = Value_Bool{
                        _0: false,
                    }
                case Nil:
                    ret429 = Value_Bool{
                        _0: false,
                    }
                }
            case Value_Bool:
                var x107 bool = x105._0
                switch x104 := x104.(type) {
                case Value_Int:
                    ret429 = Value_Bool{
                        _0: false,
                    }
                case Value_Bool:
                    var x113 bool = x104._0
                    var a__124 bool = x113
                    var b__125 bool = x107
                    var t334 bool = a__124 == b__125
                    ret429 = Value_Bool{
                        _0: t334,
                    }
                case Func:
                    ret429 = Value_Bool{
                        _0: false,
                    }
                case Nil:
                    ret429 = Value_Bool{
                        _0: false,
                    }
                }
            case Func:
                ret429 = Value_Bool{
                    _0: false,
                }
            case Nil:
                ret429 = Value_Bool{
                    _0: false,
                }
            }
        } else {
            ret429 = Value_Bool{
                _0: false,
            }
        }
    case "+":
        var i__126 *ref_int32_x = ref__Ref_int32(0)
        var acc__127 *ref_int32_x = ref__Ref_int32(0)
        var cond430 bool
        for {
            var t335 int32 = ref_get__Ref_int32(i__126)
            var t336 int32 = int32(len(args__121))
            cond430 = t335 < t336
            if !cond430 {
                break
            }
            var t337 int32 = ref_get__Ref_int32(i__126)
            var mtmp115 Value = args__121[t337]
            switch mtmp115 := mtmp115.(type) {
            case Value_Int:
                var x116 int32 = mtmp115._0
                var n__128 int32 = x116
                var t339 int32 = ref_get__Ref_int32(acc__127)
                var t338 int32 = t339 + n__128
                ref_set__Ref_int32(acc__127, t338)
                var t341 int32 = ref_get__Ref_int32(i__126)
                var t340 int32 = t341 + 1
                ref_set__Ref_int32(i__126, t340)
            case Value_Bool:
                var t343 int32 = ref_get__Ref_int32(i__126)
                var t342 int32 = t343 + 1
                ref_set__Ref_int32(i__126, t342)
            case Func:
                var t345 int32 = ref_get__Ref_int32(i__126)
                var t344 int32 = t345 + 1
                ref_set__Ref_int32(i__126, t344)
            case Nil:
                var t347 int32 = ref_get__Ref_int32(i__126)
                var t346 int32 = t347 + 1
                ref_set__Ref_int32(i__126, t346)
            }
        }
        var t348 int32 = ref_get__Ref_int32(acc__127)
        ret429 = Value_Int{
            _0: t348,
        }
    case "*":
        var i__129 *ref_int32_x = ref__Ref_int32(0)
        var acc__130 *ref_int32_x = ref__Ref_int32(1)
        var cond431 bool
        for {
            var t349 int32 = ref_get__Ref_int32(i__129)
            var t350 int32 = int32(len(args__121))
            cond431 = t349 < t350
            if !cond431 {
                break
            }
            var t351 int32 = ref_get__Ref_int32(i__129)
            var mtmp121 Value = args__121[t351]
            switch mtmp121 := mtmp121.(type) {
            case Value_Int:
                var x122 int32 = mtmp121._0
                var n__131 int32 = x122
                var t353 int32 = ref_get__Ref_int32(acc__130)
                var t352 int32 = t353 * n__131
                ref_set__Ref_int32(acc__130, t352)
                var t355 int32 = ref_get__Ref_int32(i__129)
                var t354 int32 = t355 + 1
                ref_set__Ref_int32(i__129, t354)
            case Value_Bool:
                var t357 int32 = ref_get__Ref_int32(i__129)
                var t356 int32 = t357 + 1
                ref_set__Ref_int32(i__129, t356)
            case Func:
                var t359 int32 = ref_get__Ref_int32(i__129)
                var t358 int32 = t359 + 1
                ref_set__Ref_int32(i__129, t358)
            case Nil:
                var t361 int32 = ref_get__Ref_int32(i__129)
                var t360 int32 = t361 + 1
                ref_set__Ref_int32(i__129, t360)
            }
        }
        var t362 int32 = ref_get__Ref_int32(acc__130)
        ret429 = Value_Int{
            _0: t362,
        }
    case "-":
        var mtmp127 int32 = int32(len(args__121))
        switch mtmp127 {
        case 1:
            var mtmp128 Value = args__121[0]
            switch mtmp128 := mtmp128.(type) {
            case Value_Int:
                var x129 int32 = mtmp128._0
                var n__132 int32 = x129
                var t363 int32 = 0 - n__132
                ret429 = Value_Int{
                    _0: t363,
                }
            case Value_Bool:
                ret429 = Nil{}
            case Func:
                ret429 = Nil{}
            case Nil:
                ret429 = Nil{}
            }
        case 2:
            var t364 Value = args__121[0]
            var t365 Value = args__121[1]
            var mtmp132 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t364,
                _1: t365,
            }
            var x133 Value = mtmp132._0
            var x134 Value = mtmp132._1
            switch x134 := x134.(type) {
            case Value_Int:
                var x135 int32 = x134._0
                switch x133 := x133.(type) {
                case Value_Int:
                    var x138 int32 = x133._0
                    var a__133 int32 = x138
                    var b__134 int32 = x135
                    var t366 int32 = a__133 - b__134
                    ret429 = Value_Int{
                        _0: t366,
                    }
                case Value_Bool:
                    ret429 = Nil{}
                case Func:
                    ret429 = Nil{}
                case Nil:
                    ret429 = Nil{}
                }
            case Value_Bool:
                ret429 = Nil{}
            case Func:
                ret429 = Nil{}
            case Nil:
                ret429 = Nil{}
            }
        default:
            ret429 = Nil{}
        }
    case "/":
        var t368 int32 = int32(len(args__121))
        var t367 bool = t368 == 2
        if t367 {
            var t369 Value = args__121[0]
            var t370 Value = args__121[1]
            var mtmp141 Tuple2_Value_Value = Tuple2_Value_Value{
                _0: t369,
                _1: t370,
            }
            var x142 Value = mtmp141._0
            var x143 Value = mtmp141._1
            switch x143 := x143.(type) {
            case Value_Int:
                var x144 int32 = x143._0
                switch x142 := x142.(type) {
                case Value_Int:
                    var x147 int32 = x142._0
                    var a__135 int32 = x147
                    var b__136 int32 = x144
                    var t371 int32 = a__135 / b__136
                    ret429 = Value_Int{
                        _0: t371,
                    }
                case Value_Bool:
                    ret429 = Nil{}
                case Func:
                    ret429 = Nil{}
                case Nil:
                    ret429 = Nil{}
                }
            case Value_Bool:
                ret429 = Nil{}
            case Func:
                ret429 = Nil{}
            case Nil:
                ret429 = Nil{}
            }
        } else {
            ret429 = Nil{}
        }
    default:
        ret429 = Nil{}
    }
    return ret429
}

func apply(func__137 Value, args__138 []Value, global__139 *ref_vec_binding_x) Value {
    var ret432 Value
    switch func__137 := func__137.(type) {
    case Value_Int:
        ret432 = Nil{}
    case Value_Bool:
        ret432 = Nil{}
    case Func:
        var x152 Lambda = func__137._0
        var fun__140 Lambda = x152
        ret432 = apply_lambda(fun__140, args__138)
    case Nil:
        ret432 = Nil{}
    }
    return ret432
}

func apply_lambda(lambda__141 Lambda, args__142 []Value) Value {
    var ret433 Value
    var t372 []Binding = lambda__141.env
    var env__143 *ref_vec_binding_x = ref__Ref_Vec_Binding(t372)
    var i__144 *ref_int32_x = ref__Ref_int32(0)
    var cond434 bool
    for {
        var t374 int32 = ref_get__Ref_int32(i__144)
        var t376 []string = lambda__141.params
        var t375 int32 = int32(len(t376))
        var t373 bool = t374 < t375
        var t378 int32 = ref_get__Ref_int32(i__144)
        var t379 int32 = int32(len(args__142))
        var t377 bool = t378 < t379
        cond434 = t373 && t377
        if !cond434 {
            break
        }
        var t380 []string = lambda__141.params
        var t381 int32 = ref_get__Ref_int32(i__144)
        var name__145 string = t380[t381]
        var t382 int32 = ref_get__Ref_int32(i__144)
        var value__146 Value = args__142[t382]
        var t383 []Binding = ref_get__Ref_Vec_Binding(env__143)
        var t384 Binding = Binding{
            name: name__145,
            value: value__146,
        }
        var updated__147 []Binding = append(t383, t384)
        ref_set__Ref_Vec_Binding(env__143, updated__147)
        var t386 int32 = ref_get__Ref_int32(i__144)
        var t385 int32 = t386 + 1
        ref_set__Ref_int32(i__144, t385)
    }
    var t387 SExpr = lambda__141.body
    var t388 []Binding = ref_get__Ref_Vec_Binding(env__143)
    var t389 *ref_vec_binding_x = lambda__141.global
    ret433 = eval(t387, t388, t389)
    return ret433
}

func main0() struct{} {
    var ret435 struct{}
    var t390 []Binding = nil
    var global__148 *ref_vec_binding_x = ref__Ref_Vec_Binding(t390)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t391 []Token = lex(program__149)
    var exprs__150 []SExpr = parse_program(t391)
    var t392 SExpr = exprs__150[0]
    var t393 []Binding = nil
    var result__151 Value = eval(t392, t393, global__148)
    var t394 string = value_to_string(result__151)
    string_println(t394)
    var t395 []Token = lex("(add3 10 20 30)")
    var exprs2__152 []SExpr = parse_program(t395)
    var t396 SExpr = exprs2__152[0]
    var t397 []Binding = nil
    var result2__153 Value = eval(t396, t397, global__148)
    var t398 string = value_to_string(result2__153)
    string_println(t398)
    ret435 = struct{}{}
    return ret435
}

func main() {
    main0()
}
