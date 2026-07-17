package main

import (
    _goml_fmt "fmt"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int32 {
    return int32(len(s))
}

func _goml_runtime_core_string_get(s string, i int32) rune {
    return rune(s[i])
}

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_Token struct {
    items []Token
}

func vec_new__Vec_5Token() *_goml_vec_Token {
    return &_goml_vec_Token{
        items: nil,
    }
}

func vec_push__Vec_5Token(vec *_goml_vec_Token, elem Token) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5Token(vec *_goml_vec_Token, index int32) Token {
    return vec.items[index]
}

func vec_len__Vec_5Token(vec *_goml_vec_Token) int32 {
    return int32(len(vec.items))
}

type _goml_vec_Binding struct {
    items []Binding
}

func vec_new__Vec_7Binding() *_goml_vec_Binding {
    return &_goml_vec_Binding{
        items: nil,
    }
}

func vec_push__Vec_7Binding(vec *_goml_vec_Binding, elem Binding) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_7Binding(vec *_goml_vec_Binding, index int32) Binding {
    return vec.items[index]
}

func vec_len__Vec_7Binding(vec *_goml_vec_Binding) int32 {
    return int32(len(vec.items))
}

type _goml_vec_SExpr struct {
    items []SExpr
}

func vec_new__Vec_5SExpr() *_goml_vec_SExpr {
    return &_goml_vec_SExpr{
        items: nil,
    }
}

func vec_push__Vec_5SExpr(vec *_goml_vec_SExpr, elem SExpr) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5SExpr(vec *_goml_vec_SExpr, index int32) SExpr {
    return vec.items[index]
}

func vec_len__Vec_5SExpr(vec *_goml_vec_SExpr) int32 {
    return int32(len(vec.items))
}

type _goml_vec_Value struct {
    items []Value
}

func vec_new__Vec_5Value() *_goml_vec_Value {
    return &_goml_vec_Value{
        items: nil,
    }
}

func vec_push__Vec_5Value(vec *_goml_vec_Value, elem Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5Value(vec *_goml_vec_Value, index int32) Value {
    return vec.items[index]
}

func vec_len__Vec_5Value(vec *_goml_vec_Value) int32 {
    return int32(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int32) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int32 {
    return int32(len(vec.items))
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
    value *_goml_vec_Token
}

func ref__Ref_10Vec_5Token(value *_goml_vec_Token) *ref_Vec_5Token_x {
    return &ref_Vec_5Token_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Token(reference *ref_Vec_5Token_x) *_goml_vec_Token {
    return reference.value
}

func ref_set__Ref_10Vec_5Token(reference *ref_Vec_5Token_x, value *_goml_vec_Token) struct{} {
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
    value *_goml_vec_SExpr
}

func ref__Ref_10Vec_5SExpr(value *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    return &ref_Vec_5SExpr_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    return reference.value
}

func ref_set__Ref_10Vec_5SExpr(reference *ref_Vec_5SExpr_x, value *_goml_vec_SExpr) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_7Binding_x struct {
    value *_goml_vec_Binding
}

func ref__Ref_12Vec_7Binding(value *_goml_vec_Binding) *ref_Vec_7Binding_x {
    return &ref_Vec_7Binding_x{
        value: value,
    }
}

func ref_get__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x) *_goml_vec_Binding {
    return reference.value
}

func ref_set__Ref_12Vec_7Binding(reference *ref_Vec_7Binding_x, value *_goml_vec_Binding) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_6string_x struct {
    value *_goml_vec_string
}

func ref__Ref_11Vec_6string(value *_goml_vec_string) *ref_Vec_6string_x {
    return &ref_Vec_6string_x{
        value: value,
    }
}

func ref_get__Ref_11Vec_6string(reference *ref_Vec_6string_x) *_goml_vec_string {
    return reference.value
}

func ref_set__Ref_11Vec_6string(reference *ref_Vec_6string_x, value *_goml_vec_string) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_Vec_5Value_x struct {
    value *_goml_vec_Value
}

func ref__Ref_10Vec_5Value(value *_goml_vec_Value) *ref_Vec_5Value_x {
    return &ref_Vec_5Value_x{
        value: value,
    }
}

func ref_get__Ref_10Vec_5Value(reference *ref_Vec_5Value_x) *_goml_vec_Value {
    return reference.value
}

func ref_set__Ref_10Vec_5Value(reference *ref_Vec_5Value_x, value *_goml_vec_Value) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_5Token_5int32 struct {
    _0 Token
    _1 int32
}

type Tuple2_10Vec_5SExpr_5int32 struct {
    _0 *_goml_vec_SExpr
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
    params *_goml_vec_string
    body SExpr
    env *_goml_vec_Binding
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
    _0 *_goml_vec_SExpr
}

func (_ List) isSExpr() {}

func is_digit(ch__0 rune) bool {
    var retv171 bool
    var t174 bool = ch__0 >= 48
    var jp173 bool
    if t174 {
        var t175 bool = ch__0 <= 57
        jp173 = t175
    } else {
        jp173 = false
    }
    retv171 = jp173
    return retv171
}

func digit_value(ch__1 rune) int32 {
    var retv177 int32
    var jp179 int32
    switch ch__1 {
    case 48:
        jp179 = 0
    case 49:
        jp179 = 1
    case 50:
        jp179 = 2
    case 51:
        jp179 = 3
    case 52:
        jp179 = 4
    case 53:
        jp179 = 5
    case 54:
        jp179 = 6
    case 55:
        jp179 = 7
    case 56:
        jp179 = 8
    case 57:
        jp179 = 9
    default:
        jp179 = 0
    }
    retv177 = jp179
    return retv177
}

func is_int_text(text__2 string) bool {
    var retv181 bool
    var len__3 int32 = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t184 bool = len__3 == 0
    var jp183 bool
    if t184 {
        jp183 = false
        retv181 = jp183
        return retv181
    } else {
        var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop190:
        for {
            var t209 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp192 bool
            if t209 {
                var t210 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t211 bool = t210 < len__3
                jp192 = t211
            } else {
                jp192 = false
            }
            if jp192 {
                var t193 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t193)
                var t206 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t207 bool = !t206
                var jp196 bool
                if t207 {
                    var t208 bool = ch__8 == 45
                    jp196 = t208
                } else {
                    jp196 = false
                }
                if jp196 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t197 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                    var t198 int32 = t197 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t198)
                } else {
                    var t201 bool = is_digit(ch__8)
                    if t201 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t202 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                        var t203 int32 = t202 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t203)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop190
            }
        }
        var t188 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp187 bool
        if t188 {
            var t189 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp187 = t189
        } else {
            jp187 = false
        }
        jp183 = jp187
        retv181 = jp183
        return retv181
    }
}

func parse_int32(text__9 string) int32 {
    var retv213 int32
    var len__10 int32 = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop221:
    for {
        var t222 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
        var t223 bool = t222 < len__10
        if t223 {
            var t224 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t224)
            var t237 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t238 bool = !t237
            var jp227 bool
            if t238 {
                var t239 bool = ch__15 == 45
                jp227 = t239
            } else {
                jp227 = false
            }
            if jp227 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t228 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t229 int32 = t228 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t229)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t231 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t232 int32 = t231 * 10
                var t233 int32 = t232 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t233)
                var t234 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t235 int32 = t234 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t235)
            }
            continue
        } else {
            break Loop_loop221
        }
    }
    var t217 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp216 int32
    if t217 {
        var t218 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t219 int32 = 0 - t218
        jp216 = t219
    } else {
        var t220 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp216 = t220
    }
    retv213 = jp216
    return retv213
}

func is_delim(ch__17 rune) bool {
    var retv241 bool
    var t247 bool = ch__17 == 40
    var jp245 bool
    if t247 {
        jp245 = true
    } else {
        var t248 bool = ch__17 == 41
        jp245 = t248
    }
    var jp243 bool
    if jp245 {
        jp243 = true
    } else {
        var t246 bool = ch__17 == 32
        jp243 = t246
    }
    retv241 = jp243
    return retv241
}

func lex_atom(source__18 string, start__19 int32) Tuple2_5Token_5int32 {
    var retv250 Tuple2_5Token_5int32
    var len__20 int32 = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop264:
    for {
        var t277 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t278 bool = !t277
        var jp266 bool
        if t278 {
            var t279 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var t280 bool = t279 < len__20
            jp266 = t280
        } else {
            jp266 = false
        }
        if jp266 {
            var t267 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t267)
            var t269 bool = is_delim(ch__24)
            if t269 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t271 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t272 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t273 string = t271 + t272
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t273)
                var t274 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
                var t275 int32 = t274 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__22, t275)
            }
            continue
        } else {
            break Loop_loop264
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp253 Token
    switch atom__25 {
    case "true":
        var t256 Token = Token_Bool{
            _0: true,
        }
        jp253 = t256
    case "false":
        var t257 Token = Token_Bool{
            _0: false,
        }
        jp253 = t257
    default:
        var t260 bool = is_int_text(atom__25)
        var jp259 Token
        if t260 {
            var t261 int32 = parse_int32(atom__25)
            var t262 Token = Token_Int{
                _0: t261,
            }
            jp259 = t262
        } else {
            var t263 Token = Token_Sym{
                _0: atom__25,
            }
            jp259 = t263
        }
        jp253 = jp259
    }
    var token__26 Token = jp253
    var t254 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
    var t255 Tuple2_5Token_5int32 = Tuple2_5Token_5int32{
        _0: token__26,
        _1: t254,
    }
    retv250 = t255
    return retv250
}

func lex(source__27 string) *_goml_vec_Token {
    var retv282 *_goml_vec_Token
    var len__28 int32 = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop285:
    for {
        var t286 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
        var t287 bool = t286 < len__28
        if t287 {
            var t288 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t288)
            var t290 bool = ch__32 == 40
            if t290 {
                var t291 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t292 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t291, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t292)
                var t293 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                var t294 int32 = t293 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t294)
            } else {
                var t297 bool = ch__32 == 41
                if t297 {
                    var t298 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t299 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t298, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t299)
                    var t300 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                    var t301 int32 = t300 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t301)
                } else {
                    var t304 bool = ch__32 == 32
                    if t304 {
                        var t305 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var t306 int32 = t305 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t306)
                    } else {
                        var t308 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var mtmp35 Tuple2_5Token_5int32 = lex_atom(source__27, t308)
                        var x36 Token = mtmp35._0
                        var x37 int32 = mtmp35._1
                        var next__34 int32 = x37
                        var tok__33 Token = x36
                        var t309 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t310 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t309, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t310)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop285
        }
    }
    var t284 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv282 = t284
    return retv282
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv313 Value
    var t314 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t315 int32 = t314 - 1
    var i__37 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t315)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop318:
    for {
        var t330 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t331 bool = !t330
        var jp320 bool
        if t331 {
            var t332 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var t333 bool = t332 >= 0
            jp320 = t333
        } else {
            jp320 = false
        }
        if jp320 {
            var t321 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t321)
            var t323 string = binding__40.name
            var t324 bool = t323 == name__36
            if t324 {
                var t325 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t325)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t327 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
                var t328 int32 = t327 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__37, t328)
            }
            continue
        } else {
            break Loop_loop318
        }
    }
    var t317 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv313 = t317
    return retv313
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv335 Value
    var mtmp42 Value = env_lookup(local__41, name__43)
    var jp337 Value
    switch mtmp42.(type) {
    case Value_Int:
        var other__44 Value = mtmp42
        jp337 = other__44
    case Value_Bool:
        var other__44 Value = mtmp42
        jp337 = other__44
    case Func:
        var other__44 Value = mtmp42
        jp337 = other__44
    case Nil:
        var t338 Value = env_lookup(global__42, name__43)
        jp337 = t338
    default:
        panic("non-exhaustive match")
    }
    retv335 = jp337
    return retv335
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int32) Tuple2_10Vec_5SExpr_5int32 {
    var retv340 Tuple2_10Vec_5SExpr_5int32
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop345:
    for {
        var t369 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t370 bool = !t369
        var jp347 bool
        if t370 {
            var t371 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var t372 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t373 bool = t371 < t372
            jp347 = t373
        } else {
            jp347 = false
        }
        if jp347 {
            var t348 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var mtmp46 Token = vec_get__Vec_5Token(tokens__45, t348)
            switch mtmp46.(type) {
            case LParen:
                var t350 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp50 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t350)
                var x51 SExpr = mtmp50._0
                var x52 int32 = mtmp50._1
                var next__52 int32 = x52
                var expr__51 SExpr = x51
                var t351 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t352 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t351, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t352)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t354 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var t355 int32 = t354 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, t355)
            case Token_Sym:
                var t357 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp55 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t357)
                var x56 SExpr = mtmp55._0
                var x57 int32 = mtmp55._1
                var next__52 int32 = x57
                var expr__51 SExpr = x56
                var t358 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t359 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t358, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t359)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case Token_Int:
                var t361 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp59 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t361)
                var x60 SExpr = mtmp59._0
                var x61 int32 = mtmp59._1
                var next__52 int32 = x61
                var expr__51 SExpr = x60
                var t362 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t363 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t362, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t363)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case Token_Bool:
                var t365 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp63 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t365)
                var x64 SExpr = mtmp63._0
                var x65 int32 = mtmp63._1
                var next__52 int32 = x65
                var expr__51 SExpr = x64
                var t366 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t367 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t366, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t367)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop345
        }
    }
    var t342 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t343 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
    var t344 Tuple2_10Vec_5SExpr_5int32 = Tuple2_10Vec_5SExpr_5int32{
        _0: t342,
        _1: t343,
    }
    retv340 = t344
    return retv340
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int32) Tuple2_5SExpr_5int32 {
    var retv375 Tuple2_5SExpr_5int32
    var mtmp68 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp377 Tuple2_5SExpr_5int32
    switch mtmp68.(type) {
    case LParen:
        var t378 int32 = start__54 + 1
        var mtmp72 Tuple2_10Vec_5SExpr_5int32 = parse_list(tokens__53, t378)
        var x73 *_goml_vec_SExpr = mtmp72._0
        var x74 int32 = mtmp72._1
        var next__56 int32 = x74
        var items__55 *_goml_vec_SExpr = x73
        var t379 SExpr = List{
            _0: items__55,
        }
        var t380 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t379,
            _1: next__56,
        }
        jp377 = t380
    case RParen:
        var t381 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t382 int32 = start__54 + 1
        var t383 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t381,
            _1: t382,
        }
        jp377 = t383
    case Token_Sym:
        var x69 string = mtmp68.(Token_Sym)._0
        var name__59 string = x69
        var t384 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t385 int32 = start__54 + 1
        var t386 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t384,
            _1: t385,
        }
        jp377 = t386
    case Token_Int:
        var x70 int32 = mtmp68.(Token_Int)._0
        var n__58 int32 = x70
        var t387 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t388 int32 = start__54 + 1
        var t389 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t387,
            _1: t388,
        }
        jp377 = t389
    case Token_Bool:
        var x71 bool = mtmp68.(Token_Bool)._0
        var b__57 bool = x71
        var t390 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t391 int32 = start__54 + 1
        var t392 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t390,
            _1: t391,
        }
        jp377 = t392
    default:
        panic("non-exhaustive match")
    }
    retv375 = jp377
    return retv375
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv394 *_goml_vec_SExpr
    var i__61 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop397:
    for {
        var t398 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
        var t399 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t400 bool = t398 < t399
        if t400 {
            var t401 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
            var mtmp75 Tuple2_5SExpr_5int32 = parse_expr(tokens__60, t401)
            var x76 SExpr = mtmp75._0
            var x77 int32 = mtmp75._1
            var next__65 int32 = x77
            var expr__64 SExpr = x76
            var t402 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t403 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t402, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t403)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__61, next__65)
            continue
        } else {
            break Loop_loop397
        }
    }
    var t396 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv394 = t396
    return retv394
}

func value_to_string(value__66 Value) string {
    var retv406 string
    var jp408 string
    switch value__66.(type) {
    case Value_Int:
        var x80 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x80
        var t409 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp408 = t409
    case Value_Bool:
        var x81 bool = value__66.(Value_Bool)._0
        var b__68 bool = x81
        var t410 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp408 = t410
    case Func:
        jp408 = "<lambda>"
    case Nil:
        jp408 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv406 = jp408
    return retv406
}

func truthy(value__69 Value) bool {
    var retv412 bool
    var jp414 bool
    switch value__69.(type) {
    case Value_Int:
        var x83 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x83
        var t415 bool = n__71 != 0
        jp414 = t415
    case Value_Bool:
        var x84 bool = value__69.(Value_Bool)._0
        var b__70 bool = x84
        jp414 = b__70
    case Func:
        jp414 = true
    case Nil:
        jp414 = false
    default:
        panic("non-exhaustive match")
    }
    retv412 = jp414
    return retv412
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv417 Value
    var jp419 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x86 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x86
        var t420 Value = Value_Int{
            _0: n__75,
        }
        jp419 = t420
    case SExpr_Bool:
        var x87 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x87
        var t421 Value = Value_Bool{
            _0: b__76,
        }
        jp419 = t421
    case SExpr_Sym:
        var x88 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x88
        var t422 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t423 Value = lookup(local__73, t422, name__77)
        jp419 = t423
    case List:
        var x89 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x89
        var t424 Value = eval_list(items__78, local__73, global__74)
        jp419 = t424
    default:
        panic("non-exhaustive match")
    }
    retv417 = jp419
    return retv417
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv426 Value
    var t429 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t430 bool = t429 == 0
    var jp428 Value
    if t430 {
        jp428 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp432 Value
        switch head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t433 Value = apply(f__84, args__85, global__81)
            jp432 = t433
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t434 Value = apply(f__84, args__85, global__81)
            jp432 = t434
        case SExpr_Sym:
            var x92 string = head__82.(SExpr_Sym)._0
            var name__83 string = x92
            var t435 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp432 = t435
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t436 Value = apply(f__84, args__85, global__81)
            jp432 = t436
        default:
            panic("non-exhaustive match")
        }
        jp428 = jp432
    }
    retv426 = jp428
    return retv426
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv438 Value
    var jp440 Value
    switch name__86 {
    case "begin":
        var t441 Value = eval_begin(items__87, 1, local__88, global__89)
        jp440 = t441
    case "define":
        var t444 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t445 bool = t444 == 3
        var jp443 Value
        if t445 {
            var mtmp94 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp447 Value
            switch mtmp94.(type) {
            case SExpr_Int:
                jp447 = Nil{}
            case SExpr_Bool:
                jp447 = Nil{}
            case SExpr_Sym:
                var x97 string = mtmp94.(SExpr_Sym)._0
                var var__90 string = x97
                var t448 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t448, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t449 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t449)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp447 = value__91
            case List:
                jp447 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp443 = jp447
        } else {
            jp443 = Nil{}
        }
        jp440 = jp443
    case "if":
        var t452 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t453 bool = t452 == 4
        var jp451 Value
        if t453 {
            var t454 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t454, local__88, global__89)
            var t457 bool = truthy(cond__94)
            var jp456 Value
            if t457 {
                var t458 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t459 Value = eval(t458, local__88, global__89)
                jp456 = t459
            } else {
                var t460 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t461 Value = eval(t460, local__88, global__89)
                jp456 = t461
            }
            jp451 = jp456
        } else {
            jp451 = Nil{}
        }
        jp440 = jp451
    case "lambda":
        var t464 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t465 bool = t464 == 3
        var jp463 Value
        if t465 {
            var mtmp100 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp467 Value
            switch mtmp100.(type) {
            case SExpr_Int:
                jp467 = Nil{}
            case SExpr_Bool:
                jp467 = Nil{}
            case SExpr_Sym:
                jp467 = Nil{}
            case List:
                var x104 *_goml_vec_SExpr = mtmp100.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x104
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t468 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t469 Value = Func{
                    _0: t468,
                }
                jp467 = t469
            default:
                panic("non-exhaustive match")
            }
            jp463 = jp467
        } else {
            jp463 = Nil{}
        }
        jp440 = jp463
    case "+":
        var t470 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t471 Value = apply_builtin("+", t470)
        jp440 = t471
    case "-":
        var t472 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t473 Value = apply_builtin("-", t472)
        jp440 = t473
    case "*":
        var t474 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t475 Value = apply_builtin("*", t474)
        jp440 = t475
    case "/":
        var t476 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t477 Value = apply_builtin("/", t476)
        jp440 = t477
    case "=":
        var t478 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t479 Value = apply_builtin("=", t478)
        jp440 = t479
    default:
        var t480 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t480, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t481 Value = apply(f__98, args__99, global__89)
        jp440 = t481
    }
    retv438 = jp440
    return retv438
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int32, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv483 Value
    var i__104 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop486:
    for {
        var t487 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
        var t488 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t489 bool = t487 < t488
        if t489 {
            var t490 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t491 SExpr = vec_get__Vec_5SExpr(items__100, t490)
            var v__106 Value = eval(t491, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t492 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t493 int32 = t492 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__104, t493)
            continue
        } else {
            break Loop_loop486
        }
    }
    var t485 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv483 = t485
    return retv483
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv496 *_goml_vec_string
    var i__108 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop499:
    for {
        var t500 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
        var t501 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t502 bool = t500 < t501
        if t502 {
            var t503 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
            var mtmp107 SExpr = vec_get__Vec_5SExpr(items__107, t503)
            switch mtmp107.(type) {
            case SExpr_Int:
                var t505 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t506 int32 = t505 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t506)
            case SExpr_Bool:
                var t508 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t509 int32 = t508 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t509)
            case SExpr_Sym:
                var x110 string = mtmp107.(SExpr_Sym)._0
                var name__111 string = x110
                var t511 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t512 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t511, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t512)
                var t513 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t514 int32 = t513 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t514)
            case List:
                var t516 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t517 int32 = t516 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t517)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop499
        }
    }
    var t498 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv496 = t498
    return retv496
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int32, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv520 *_goml_vec_Value
    var i__116 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop523:
    for {
        var t524 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
        var t525 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t526 bool = t524 < t525
        if t526 {
            var t527 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t528 SExpr = vec_get__Vec_5SExpr(items__112, t527)
            var v__119 Value = eval(t528, local__114, global__115)
            var t529 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t530 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t529, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t530)
            var t531 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t532 int32 = t531 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__116, t532)
            continue
        } else {
            break Loop_loop523
        }
    }
    var t522 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv520 = t522
    return retv520
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv535 Value
    var jp537 Value
    switch name__120 {
    case "=":
        var t540 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t541 bool = t540 == 2
        var jp539 Value
        if t541 {
            var t542 Value = vec_get__Vec_5Value(args__121, 0)
            var t543 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp116 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t542,
                _1: t543,
            }
            var x117 Value = mtmp116._0
            var x118 Value = mtmp116._1
            var jp545 Value
            switch x118.(type) {
            case Value_Int:
                var x119 int32 = x118.(Value_Int)._0
                var jp547 Value
                switch x117.(type) {
                case Value_Int:
                    var x122 int32 = x117.(Value_Int)._0
                    var a__122 int32 = x122
                    var b__123 int32 = x119
                    var t548 bool = a__122 == b__123
                    var t549 Value = Value_Bool{
                        _0: t548,
                    }
                    jp547 = t549
                case Value_Bool:
                    var t550 Value = Value_Bool{
                        _0: false,
                    }
                    jp547 = t550
                case Func:
                    var t551 Value = Value_Bool{
                        _0: false,
                    }
                    jp547 = t551
                case Nil:
                    var t552 Value = Value_Bool{
                        _0: false,
                    }
                    jp547 = t552
                default:
                    panic("non-exhaustive match")
                }
                jp545 = jp547
            case Value_Bool:
                var x120 bool = x118.(Value_Bool)._0
                var jp554 Value
                switch x117.(type) {
                case Value_Int:
                    var t555 Value = Value_Bool{
                        _0: false,
                    }
                    jp554 = t555
                case Value_Bool:
                    var x126 bool = x117.(Value_Bool)._0
                    var a__124 bool = x126
                    var b__125 bool = x120
                    var t556 bool = a__124 == b__125
                    var t557 Value = Value_Bool{
                        _0: t556,
                    }
                    jp554 = t557
                case Func:
                    var t558 Value = Value_Bool{
                        _0: false,
                    }
                    jp554 = t558
                case Nil:
                    var t559 Value = Value_Bool{
                        _0: false,
                    }
                    jp554 = t559
                default:
                    panic("non-exhaustive match")
                }
                jp545 = jp554
            case Func:
                var t560 Value = Value_Bool{
                    _0: false,
                }
                jp545 = t560
            case Nil:
                var t561 Value = Value_Bool{
                    _0: false,
                }
                jp545 = t561
            default:
                panic("non-exhaustive match")
            }
            jp539 = jp545
        } else {
            var t562 Value = Value_Bool{
                _0: false,
            }
            jp539 = t562
        }
        jp537 = jp539
        retv535 = jp537
        return retv535
    case "+":
        var i__126 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop566:
        for {
            var t567 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
            var t568 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t569 bool = t567 < t568
            if t569 {
                var t570 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                var mtmp128 Value = vec_get__Vec_5Value(args__121, t570)
                switch mtmp128.(type) {
                case Value_Int:
                    var x129 int32 = mtmp128.(Value_Int)._0
                    var n__128 int32 = x129
                    var t572 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t573 int32 = t572 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t573)
                    var t574 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t575 int32 = t574 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t575)
                case Value_Bool:
                    var t577 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t578 int32 = t577 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t578)
                case Func:
                    var t580 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t581 int32 = t580 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t581)
                case Nil:
                    var t583 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t584 int32 = t583 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t584)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop566
            }
        }
        var t564 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t565 Value = Value_Int{
            _0: t564,
        }
        jp537 = t565
        retv535 = jp537
        return retv535
    case "*":
        var i__129 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop589:
        for {
            var t590 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
            var t591 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t592 bool = t590 < t591
            if t592 {
                var t593 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                var mtmp134 Value = vec_get__Vec_5Value(args__121, t593)
                switch mtmp134.(type) {
                case Value_Int:
                    var x135 int32 = mtmp134.(Value_Int)._0
                    var n__131 int32 = x135
                    var t595 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t596 int32 = t595 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t596)
                    var t597 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t598 int32 = t597 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t598)
                case Value_Bool:
                    var t600 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t601 int32 = t600 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t601)
                case Func:
                    var t603 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t604 int32 = t603 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t604)
                case Nil:
                    var t606 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t607 int32 = t606 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t607)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop589
            }
        }
        var t587 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t588 Value = Value_Int{
            _0: t587,
        }
        jp537 = t588
        retv535 = jp537
        return retv535
    case "-":
        var mtmp140 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp610 Value
        switch mtmp140 {
        case 1:
            var mtmp141 Value = vec_get__Vec_5Value(args__121, 0)
            var jp612 Value
            switch mtmp141.(type) {
            case Value_Int:
                var x142 int32 = mtmp141.(Value_Int)._0
                var n__132 int32 = x142
                var t613 int32 = 0 - n__132
                var t614 Value = Value_Int{
                    _0: t613,
                }
                jp612 = t614
            case Value_Bool:
                jp612 = Nil{}
            case Func:
                jp612 = Nil{}
            case Nil:
                jp612 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp610 = jp612
        case 2:
            var t615 Value = vec_get__Vec_5Value(args__121, 0)
            var t616 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp145 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t615,
                _1: t616,
            }
            var x146 Value = mtmp145._0
            var x147 Value = mtmp145._1
            var jp618 Value
            switch x147.(type) {
            case Value_Int:
                var x148 int32 = x147.(Value_Int)._0
                var jp620 Value
                switch x146.(type) {
                case Value_Int:
                    var x151 int32 = x146.(Value_Int)._0
                    var a__133 int32 = x151
                    var b__134 int32 = x148
                    var t621 int32 = a__133 - b__134
                    var t622 Value = Value_Int{
                        _0: t621,
                    }
                    jp620 = t622
                case Value_Bool:
                    jp620 = Nil{}
                case Func:
                    jp620 = Nil{}
                case Nil:
                    jp620 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp618 = jp620
            case Value_Bool:
                jp618 = Nil{}
            case Func:
                jp618 = Nil{}
            case Nil:
                jp618 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp610 = jp618
        default:
            jp610 = Nil{}
        }
        jp537 = jp610
        retv535 = jp537
        return retv535
    case "/":
        var t625 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t626 bool = t625 == 2
        var jp624 Value
        if t626 {
            var t627 Value = vec_get__Vec_5Value(args__121, 0)
            var t628 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp154 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t627,
                _1: t628,
            }
            var x155 Value = mtmp154._0
            var x156 Value = mtmp154._1
            var jp630 Value
            switch x156.(type) {
            case Value_Int:
                var x157 int32 = x156.(Value_Int)._0
                var jp632 Value
                switch x155.(type) {
                case Value_Int:
                    var x160 int32 = x155.(Value_Int)._0
                    var a__135 int32 = x160
                    var b__136 int32 = x157
                    var t633 int32 = a__135 / b__136
                    var t634 Value = Value_Int{
                        _0: t633,
                    }
                    jp632 = t634
                case Value_Bool:
                    jp632 = Nil{}
                case Func:
                    jp632 = Nil{}
                case Nil:
                    jp632 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp630 = jp632
            case Value_Bool:
                jp630 = Nil{}
            case Func:
                jp630 = Nil{}
            case Nil:
                jp630 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp624 = jp630
        } else {
            jp624 = Nil{}
        }
        jp537 = jp624
        retv535 = jp537
        return retv535
    default:
        jp537 = Nil{}
        retv535 = jp537
        return retv535
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv636 Value
    var jp638 Value
    switch func__137.(type) {
    case Value_Int:
        jp638 = Nil{}
    case Value_Bool:
        jp638 = Nil{}
    case Func:
        var x165 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x165
        var t639 Value = apply_lambda(fun__140, args__138)
        jp638 = t639
    case Nil:
        jp638 = Nil{}
    default:
        panic("non-exhaustive match")
    }
    retv636 = jp638
    return retv636
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv641 Value
    var t642 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t642)
    var i__144 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop648:
    for {
        var t659 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
        var t660 *_goml_vec_string = lambda__141.params
        var t661 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t660)
        var t662 bool = t659 < t661
        var jp650 bool
        if t662 {
            var t663 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t664 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t665 bool = t663 < t664
            jp650 = t665
        } else {
            jp650 = false
        }
        if jp650 {
            var t651 *_goml_vec_string = lambda__141.params
            var t652 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var name__145 string = vec_get__Vec_6string(t651, t652)
            var t653 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t653)
            var t654 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t655 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t654, t655)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t656 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t657 int32 = t656 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__144, t657)
            continue
        } else {
            break Loop_loop648
        }
    }
    var t644 SExpr = lambda__141.body
    var t645 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t646 *ref_Vec_7Binding_x = lambda__141.global
    var t647 Value = eval(t644, t645, t646)
    retv641 = t647
    return retv641
}

func main0() struct{} {
    var t667 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t667)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t668 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t668)
    var t669 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t670 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t669, t670, global__148)
    var t671 string = value_to_string(result__151)
    println__T_string(t671)
    var t672 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t672)
    var t673 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t674 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t673, t674, global__148)
    var t675 string = value_to_string(result2__153)
    println__T_string(t675)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__4 string) int32 {
    var retv677 int32
    var t678 int32 = _goml_runtime_core_string_len(self__4)
    retv677 = t678
    return retv677
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv680 *ref_int32_x
    var t681 *ref_int32_x = ref__Ref_5int32(value__137)
    retv680 = t681
    return retv680
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__137 bool) *ref_bool_x {
    var retv683 *ref_bool_x
    var t684 *ref_bool_x = ref__Ref_4bool(value__137)
    retv683 = t684
    return retv683
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__138 *ref_bool_x) bool {
    var retv686 bool
    var t687 bool = ref_get__Ref_4bool(self__138)
    retv686 = t687
    return retv686
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv689 int32
    var t690 int32 = ref_get__Ref_5int32(self__138)
    retv689 = t690
    return retv689
}

func _goml_m_inherent_i_string_i_string_i_get(self__5 string, index__6 int32) rune {
    var retv692 rune
    var t693 rune = _goml_runtime_core_string_get(self__5, index__6)
    retv692 = t693
    return retv692
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__139 *ref_bool_x, value__140 bool) struct{} {
    ref_set__Ref_4bool(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__139 *ref_int32_x, value__140 int32) struct{} {
    ref_set__Ref_5int32(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__137 string) *ref_string_x {
    var retv699 *ref_string_x
    var t700 *ref_string_x = ref__Ref_6string(value__137)
    retv699 = t700
    return retv699
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__138 *ref_string_x) string {
    var retv702 string
    var t703 string = ref_get__Ref_6string(self__138)
    retv702 = t703
    return retv702
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv705 string
    var t706 string = _goml_runtime_core_char_to_string(self__3)
    retv705 = t706
    return retv705
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__139 *ref_string_x, value__140 string) struct{} {
    ref_set__Ref_6string(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv710 *_goml_vec_Token
    var t711 *_goml_vec_Token = vec_new__Vec_5Token()
    retv710 = t711
    return retv710
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__137 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv713 *ref_Vec_5Token_x
    var t714 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__137)
    retv713 = t714
    return retv713
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__138 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv716 *_goml_vec_Token
    var t717 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__138)
    retv716 = t717
    return retv716
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__96 *_goml_vec_Token, elem__97 Token) *_goml_vec_Token {
    var retv719 *_goml_vec_Token
    var result__98 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var index__99 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop721:
    for {
        var t722 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
        var t723 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__96)
        var t724 bool = t722 < t723
        if t724 {
            var t725 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t726 Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__96, t725)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__98, t726)
            var t727 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t728 int32 = t727 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__99, t728)
            continue
        } else {
            break Loop_loop721
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__98, elem__97)
    retv719 = result__98
    return retv719
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__139 *ref_Vec_5Token_x, value__140 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__105 *_goml_vec_Binding) int32 {
    var retv732 int32
    var t733 int32 = vec_len__Vec_7Binding(self__105)
    retv732 = t733
    return retv732
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__137 Value) *ref_Value_x {
    var retv735 *ref_Value_x
    var t736 *ref_Value_x = ref__Ref_5Value(value__137)
    retv735 = t736
    return retv735
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__139 *ref_Value_x, value__140 Value) struct{} {
    ref_set__Ref_5Value(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__138 *ref_Value_x) Value {
    var retv740 Value
    var t741 Value = ref_get__Ref_5Value(self__138)
    retv740 = t741
    return retv740
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv743 *_goml_vec_SExpr
    var t744 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv743 = t744
    return retv743
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__137 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv746 *ref_Vec_5SExpr_x
    var t747 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__137)
    retv746 = t747
    return retv746
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__105 *_goml_vec_Token) int32 {
    var retv749 int32
    var t750 int32 = vec_len__Vec_5Token(self__105)
    retv749 = t750
    return retv749
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__138 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv752 *_goml_vec_SExpr
    var t753 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__138)
    retv752 = t753
    return retv752
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__96 *_goml_vec_SExpr, elem__97 SExpr) *_goml_vec_SExpr {
    var retv755 *_goml_vec_SExpr
    var result__98 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var index__99 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop757:
    for {
        var t758 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
        var t759 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__96)
        var t760 bool = t758 < t759
        if t760 {
            var t761 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t762 SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__96, t761)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__98, t762)
            var t763 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t764 int32 = t763 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__99, t764)
            continue
        } else {
            break Loop_loop757
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__98, elem__97)
    retv755 = result__98
    return retv755
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__139 *ref_Vec_5SExpr_x, value__140 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv768 string
    var t769 string = _goml_runtime_core_int32_to_string(self__2)
    retv768 = t769
    return retv768
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv771 string
    var t772 string = _goml_runtime_core_bool_to_string(self__8)
    retv771 = t772
    return retv771
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__138 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv774 *_goml_vec_Binding
    var t775 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__138)
    retv774 = t775
    return retv774
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__105 *_goml_vec_SExpr) int32 {
    var retv777 int32
    var t778 int32 = vec_len__Vec_5SExpr(self__105)
    retv777 = t778
    return retv777
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__96 *_goml_vec_Binding, elem__97 Binding) *_goml_vec_Binding {
    var retv780 *_goml_vec_Binding
    var result__98 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var index__99 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop782:
    for {
        var t783 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
        var t784 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__96)
        var t785 bool = t783 < t784
        if t785 {
            var t786 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t787 Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__96, t786)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__98, t787)
            var t788 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t789 int32 = t788 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__99, t789)
            continue
        } else {
            break Loop_loop782
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__98, elem__97)
    retv780 = result__98
    return retv780
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__139 *ref_Vec_7Binding_x, value__140 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv793 *_goml_vec_string
    var t794 *_goml_vec_string = vec_new__Vec_6string()
    retv793 = t794
    return retv793
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__137 *_goml_vec_string) *ref_Vec_6string_x {
    var retv796 *ref_Vec_6string_x
    var t797 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__137)
    retv796 = t797
    return retv796
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__138 *ref_Vec_6string_x) *_goml_vec_string {
    var retv799 *_goml_vec_string
    var t800 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__138)
    retv799 = t800
    return retv799
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__96 *_goml_vec_string, elem__97 string) *_goml_vec_string {
    var retv802 *_goml_vec_string
    var result__98 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var index__99 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop804:
    for {
        var t805 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
        var t806 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__96)
        var t807 bool = t805 < t806
        if t807 {
            var t808 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t809 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__96, t808)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__98, t809)
            var t810 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t811 int32 = t810 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__99, t811)
            continue
        } else {
            break Loop_loop804
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__98, elem__97)
    retv802 = result__98
    return retv802
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__139 *ref_Vec_6string_x, value__140 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv815 *_goml_vec_Value
    var t816 *_goml_vec_Value = vec_new__Vec_5Value()
    retv815 = t816
    return retv815
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__137 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv818 *ref_Vec_5Value_x
    var t819 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__137)
    retv818 = t819
    return retv818
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__138 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv821 *_goml_vec_Value
    var t822 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__138)
    retv821 = t822
    return retv821
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__96 *_goml_vec_Value, elem__97 Value) *_goml_vec_Value {
    var retv824 *_goml_vec_Value
    var result__98 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var index__99 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop826:
    for {
        var t827 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
        var t828 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__96)
        var t829 bool = t827 < t828
        if t829 {
            var t830 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t831 Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__96, t830)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__98, t831)
            var t832 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__99)
            var t833 int32 = t832 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__99, t833)
            continue
        } else {
            break Loop_loop826
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__98, elem__97)
    retv824 = result__98
    return retv824
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__139 *ref_Vec_5Value_x, value__140 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__139, value__140)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__105 *_goml_vec_Value) int32 {
    var retv837 int32
    var t838 int32 = vec_len__Vec_5Value(self__105)
    retv837 = t838
    return retv837
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__137 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv840 *ref_Vec_7Binding_x
    var t841 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__137)
    retv840 = t841
    return retv840
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__105 *_goml_vec_string) int32 {
    var retv843 int32
    var t844 int32 = vec_len__Vec_6string(self__105)
    retv843 = t844
    return retv843
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv846 *_goml_vec_Binding
    var t847 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv846 = t847
    return retv846
}

func println__T_string(value__1 string) struct{} {
    var t849 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t849)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__94 *_goml_vec_Token, elem__95 Token) struct{} {
    vec_push__Vec_5Token(self__94, elem__95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__100 *_goml_vec_Token, index__101 int32) Token {
    var retv854 Token
    var t855 Token = vec_get__Vec_5Token(self__100, index__101)
    retv854 = t855
    return retv854
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__94 *_goml_vec_SExpr, elem__95 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__94, elem__95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__100 *_goml_vec_SExpr, index__101 int32) SExpr {
    var retv859 SExpr
    var t860 SExpr = vec_get__Vec_5SExpr(self__100, index__101)
    retv859 = t860
    return retv859
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__94 *_goml_vec_Binding, elem__95 Binding) struct{} {
    vec_push__Vec_7Binding(self__94, elem__95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__100 *_goml_vec_Binding, index__101 int32) Binding {
    var retv864 Binding
    var t865 Binding = vec_get__Vec_7Binding(self__100, index__101)
    retv864 = t865
    return retv864
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__94 *_goml_vec_string, elem__95 string) struct{} {
    vec_push__Vec_6string(self__94, elem__95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__100 *_goml_vec_string, index__101 int32) string {
    var retv869 string
    var t870 string = vec_get__Vec_6string(self__100, index__101)
    retv869 = t870
    return retv869
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__94 *_goml_vec_Value, elem__95 Value) struct{} {
    vec_push__Vec_5Value(self__94, elem__95)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__100 *_goml_vec_Value, index__101 int32) Value {
    var retv874 Value
    var t875 Value = vec_get__Vec_5Value(self__100, index__101)
    retv874 = t875
    return retv874
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv877 string
    retv877 = self__9
    return retv877
}

func main() {
    main0()
}
