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
    var retv153 bool
    var t156 bool = ch__0 >= 48
    var jp155 bool
    if t156 {
        var t157 bool = ch__0 <= 57
        jp155 = t157
    } else {
        jp155 = false
    }
    retv153 = jp155
    return retv153
}

func digit_value(ch__1 rune) int32 {
    var retv159 int32
    var jp161 int32
    switch ch__1 {
    case 48:
        jp161 = 0
    case 49:
        jp161 = 1
    case 50:
        jp161 = 2
    case 51:
        jp161 = 3
    case 52:
        jp161 = 4
    case 53:
        jp161 = 5
    case 54:
        jp161 = 6
    case 55:
        jp161 = 7
    case 56:
        jp161 = 8
    case 57:
        jp161 = 9
    default:
        jp161 = 0
    }
    retv159 = jp161
    return retv159
}

func is_int_text(text__2 string) bool {
    var retv163 bool
    var len__3 int32 = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t166 bool = len__3 == 0
    var jp165 bool
    if t166 {
        jp165 = false
        retv163 = jp165
        return retv163
    } else {
        var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop172:
        for {
            var t191 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp174 bool
            if t191 {
                var t192 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t193 bool = t192 < len__3
                jp174 = t193
            } else {
                jp174 = false
            }
            if jp174 {
                var t175 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t175)
                var t188 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t189 bool = !t188
                var jp178 bool
                if t189 {
                    var t190 bool = ch__8 == 45
                    jp178 = t190
                } else {
                    jp178 = false
                }
                if jp178 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t179 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                    var t180 int32 = t179 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t180)
                } else {
                    var t183 bool = is_digit(ch__8)
                    if t183 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t184 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                        var t185 int32 = t184 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t185)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop172
            }
        }
        var t170 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp169 bool
        if t170 {
            var t171 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp169 = t171
        } else {
            jp169 = false
        }
        jp165 = jp169
        retv163 = jp165
        return retv163
    }
}

func parse_int32(text__9 string) int32 {
    var retv195 int32
    var len__10 int32 = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop203:
    for {
        var t204 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
        var t205 bool = t204 < len__10
        if t205 {
            var t206 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t206)
            var t219 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t220 bool = !t219
            var jp209 bool
            if t220 {
                var t221 bool = ch__15 == 45
                jp209 = t221
            } else {
                jp209 = false
            }
            if jp209 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t210 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t211 int32 = t210 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t211)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t213 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t214 int32 = t213 * 10
                var t215 int32 = t214 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t215)
                var t216 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t217 int32 = t216 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t217)
            }
            continue
        } else {
            break Loop_loop203
        }
    }
    var t199 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp198 int32
    if t199 {
        var t200 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t201 int32 = 0 - t200
        jp198 = t201
    } else {
        var t202 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp198 = t202
    }
    retv195 = jp198
    return retv195
}

func is_delim(ch__17 rune) bool {
    var retv223 bool
    var t229 bool = ch__17 == 40
    var jp227 bool
    if t229 {
        jp227 = true
    } else {
        var t230 bool = ch__17 == 41
        jp227 = t230
    }
    var jp225 bool
    if jp227 {
        jp225 = true
    } else {
        var t228 bool = ch__17 == 32
        jp225 = t228
    }
    retv223 = jp225
    return retv223
}

func lex_atom(source__18 string, start__19 int32) Tuple2_5Token_5int32 {
    var retv232 Tuple2_5Token_5int32
    var len__20 int32 = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop246:
    for {
        var t259 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t260 bool = !t259
        var jp248 bool
        if t260 {
            var t261 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var t262 bool = t261 < len__20
            jp248 = t262
        } else {
            jp248 = false
        }
        if jp248 {
            var t249 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t249)
            var t251 bool = is_delim(ch__24)
            if t251 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t253 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t254 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t255 string = t253 + t254
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t255)
                var t256 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
                var t257 int32 = t256 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__22, t257)
            }
            continue
        } else {
            break Loop_loop246
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp235 Token
    switch atom__25 {
    case "true":
        var t238 Token = Token_Bool{
            _0: true,
        }
        jp235 = t238
    case "false":
        var t239 Token = Token_Bool{
            _0: false,
        }
        jp235 = t239
    default:
        var t242 bool = is_int_text(atom__25)
        var jp241 Token
        if t242 {
            var t243 int32 = parse_int32(atom__25)
            var t244 Token = Token_Int{
                _0: t243,
            }
            jp241 = t244
        } else {
            var t245 Token = Token_Sym{
                _0: atom__25,
            }
            jp241 = t245
        }
        jp235 = jp241
    }
    var token__26 Token = jp235
    var t236 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
    var t237 Tuple2_5Token_5int32 = Tuple2_5Token_5int32{
        _0: token__26,
        _1: t236,
    }
    retv232 = t237
    return retv232
}

func lex(source__27 string) *_goml_vec_Token {
    var retv264 *_goml_vec_Token
    var len__28 int32 = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop267:
    for {
        var t268 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
        var t269 bool = t268 < len__28
        if t269 {
            var t270 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t270)
            var t272 bool = ch__32 == 40
            if t272 {
                var t273 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t274 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t273, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t274)
                var t275 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                var t276 int32 = t275 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t276)
            } else {
                var t279 bool = ch__32 == 41
                if t279 {
                    var t280 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t281 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t280, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t281)
                    var t282 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                    var t283 int32 = t282 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t283)
                } else {
                    var t286 bool = ch__32 == 32
                    if t286 {
                        var t287 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var t288 int32 = t287 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t288)
                    } else {
                        var t290 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var mtmp17 Tuple2_5Token_5int32 = lex_atom(source__27, t290)
                        var x18 Token = mtmp17._0
                        var x19 int32 = mtmp17._1
                        var next__34 int32 = x19
                        var tok__33 Token = x18
                        var t291 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t292 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t291, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t292)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop267
        }
    }
    var t266 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv264 = t266
    return retv264
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv295 Value
    var t296 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t297 int32 = t296 - 1
    var i__37 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t297)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop300:
    for {
        var t312 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t313 bool = !t312
        var jp302 bool
        if t313 {
            var t314 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var t315 bool = t314 >= 0
            jp302 = t315
        } else {
            jp302 = false
        }
        if jp302 {
            var t303 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t303)
            var t305 string = binding__40.name
            var t306 bool = t305 == name__36
            if t306 {
                var t307 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t307)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t309 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
                var t310 int32 = t309 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__37, t310)
            }
            continue
        } else {
            break Loop_loop300
        }
    }
    var t299 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv295 = t299
    return retv295
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv317 Value
    var mtmp24 Value = env_lookup(local__41, name__43)
    var jp319 Value
    switch mtmp24.(type) {
    case Value_Int:
        var other__44 Value = mtmp24
        jp319 = other__44
    case Value_Bool:
        var other__44 Value = mtmp24
        jp319 = other__44
    case Func:
        var other__44 Value = mtmp24
        jp319 = other__44
    case Nil:
        var t320 Value = env_lookup(global__42, name__43)
        jp319 = t320
    default:
        panic("non-exhaustive match")
    }
    retv317 = jp319
    return retv317
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int32) Tuple2_10Vec_5SExpr_5int32 {
    var retv322 Tuple2_10Vec_5SExpr_5int32
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop327:
    for {
        var t351 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t352 bool = !t351
        var jp329 bool
        if t352 {
            var t353 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var t354 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t355 bool = t353 < t354
            jp329 = t355
        } else {
            jp329 = false
        }
        if jp329 {
            var t330 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var mtmp28 Token = vec_get__Vec_5Token(tokens__45, t330)
            switch mtmp28.(type) {
            case LParen:
                var t332 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp32 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t332)
                var x33 SExpr = mtmp32._0
                var x34 int32 = mtmp32._1
                var next__52 int32 = x34
                var expr__51 SExpr = x33
                var t333 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t334 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t333, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t334)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t336 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var t337 int32 = t336 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, t337)
            case Token_Sym:
                var t339 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp37 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t339)
                var x38 SExpr = mtmp37._0
                var x39 int32 = mtmp37._1
                var next__52 int32 = x39
                var expr__51 SExpr = x38
                var t340 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t341 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t340, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t341)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case Token_Int:
                var t343 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp41 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t343)
                var x42 SExpr = mtmp41._0
                var x43 int32 = mtmp41._1
                var next__52 int32 = x43
                var expr__51 SExpr = x42
                var t344 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t345 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t344, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t345)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case Token_Bool:
                var t347 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp45 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t347)
                var x46 SExpr = mtmp45._0
                var x47 int32 = mtmp45._1
                var next__52 int32 = x47
                var expr__51 SExpr = x46
                var t348 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t349 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t348, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t349)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop327
        }
    }
    var t324 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t325 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
    var t326 Tuple2_10Vec_5SExpr_5int32 = Tuple2_10Vec_5SExpr_5int32{
        _0: t324,
        _1: t325,
    }
    retv322 = t326
    return retv322
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int32) Tuple2_5SExpr_5int32 {
    var retv357 Tuple2_5SExpr_5int32
    var mtmp50 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp359 Tuple2_5SExpr_5int32
    switch mtmp50.(type) {
    case LParen:
        var t360 int32 = start__54 + 1
        var mtmp54 Tuple2_10Vec_5SExpr_5int32 = parse_list(tokens__53, t360)
        var x55 *_goml_vec_SExpr = mtmp54._0
        var x56 int32 = mtmp54._1
        var next__56 int32 = x56
        var items__55 *_goml_vec_SExpr = x55
        var t361 SExpr = List{
            _0: items__55,
        }
        var t362 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t361,
            _1: next__56,
        }
        jp359 = t362
    case RParen:
        var t363 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t364 int32 = start__54 + 1
        var t365 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t363,
            _1: t364,
        }
        jp359 = t365
    case Token_Sym:
        var x51 string = mtmp50.(Token_Sym)._0
        var name__59 string = x51
        var t366 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t367 int32 = start__54 + 1
        var t368 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t366,
            _1: t367,
        }
        jp359 = t368
    case Token_Int:
        var x52 int32 = mtmp50.(Token_Int)._0
        var n__58 int32 = x52
        var t369 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t370 int32 = start__54 + 1
        var t371 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t369,
            _1: t370,
        }
        jp359 = t371
    case Token_Bool:
        var x53 bool = mtmp50.(Token_Bool)._0
        var b__57 bool = x53
        var t372 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t373 int32 = start__54 + 1
        var t374 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t372,
            _1: t373,
        }
        jp359 = t374
    default:
        panic("non-exhaustive match")
    }
    retv357 = jp359
    return retv357
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv376 *_goml_vec_SExpr
    var i__61 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop379:
    for {
        var t380 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
        var t381 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t382 bool = t380 < t381
        if t382 {
            var t383 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
            var mtmp57 Tuple2_5SExpr_5int32 = parse_expr(tokens__60, t383)
            var x58 SExpr = mtmp57._0
            var x59 int32 = mtmp57._1
            var next__65 int32 = x59
            var expr__64 SExpr = x58
            var t384 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t385 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t384, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t385)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__61, next__65)
            continue
        } else {
            break Loop_loop379
        }
    }
    var t378 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv376 = t378
    return retv376
}

func value_to_string(value__66 Value) string {
    var retv388 string
    var jp390 string
    switch value__66.(type) {
    case Value_Int:
        var x62 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x62
        var t391 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp390 = t391
    case Value_Bool:
        var x63 bool = value__66.(Value_Bool)._0
        var b__68 bool = x63
        var t392 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp390 = t392
    case Func:
        jp390 = "<lambda>"
    case Nil:
        jp390 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv388 = jp390
    return retv388
}

func truthy(value__69 Value) bool {
    var retv394 bool
    var jp396 bool
    switch value__69.(type) {
    case Value_Int:
        var x65 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x65
        var t397 bool = n__71 != 0
        jp396 = t397
    case Value_Bool:
        var x66 bool = value__69.(Value_Bool)._0
        var b__70 bool = x66
        jp396 = b__70
    case Func:
        jp396 = true
    case Nil:
        jp396 = false
    default:
        panic("non-exhaustive match")
    }
    retv394 = jp396
    return retv394
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv399 Value
    var jp401 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x68 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x68
        var t402 Value = Value_Int{
            _0: n__75,
        }
        jp401 = t402
    case SExpr_Bool:
        var x69 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x69
        var t403 Value = Value_Bool{
            _0: b__76,
        }
        jp401 = t403
    case SExpr_Sym:
        var x70 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x70
        var t404 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t405 Value = lookup(local__73, t404, name__77)
        jp401 = t405
    case List:
        var x71 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x71
        var t406 Value = eval_list(items__78, local__73, global__74)
        jp401 = t406
    default:
        panic("non-exhaustive match")
    }
    retv399 = jp401
    return retv399
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv408 Value
    var t411 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t412 bool = t411 == 0
    var jp410 Value
    if t412 {
        jp410 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp414 Value
        switch head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t415 Value = apply(f__84, args__85, global__81)
            jp414 = t415
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t416 Value = apply(f__84, args__85, global__81)
            jp414 = t416
        case SExpr_Sym:
            var x74 string = head__82.(SExpr_Sym)._0
            var name__83 string = x74
            var t417 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp414 = t417
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t418 Value = apply(f__84, args__85, global__81)
            jp414 = t418
        default:
            panic("non-exhaustive match")
        }
        jp410 = jp414
    }
    retv408 = jp410
    return retv408
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv420 Value
    var jp422 Value
    switch name__86 {
    case "begin":
        var t423 Value = eval_begin(items__87, 1, local__88, global__89)
        jp422 = t423
    case "define":
        var t426 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t427 bool = t426 == 3
        var jp425 Value
        if t427 {
            var mtmp76 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp429 Value
            switch mtmp76.(type) {
            case SExpr_Int:
                jp429 = Nil{}
            case SExpr_Bool:
                jp429 = Nil{}
            case SExpr_Sym:
                var x79 string = mtmp76.(SExpr_Sym)._0
                var var__90 string = x79
                var t430 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t430, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t431 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t431)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp429 = value__91
            case List:
                jp429 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp425 = jp429
        } else {
            jp425 = Nil{}
        }
        jp422 = jp425
    case "if":
        var t434 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t435 bool = t434 == 4
        var jp433 Value
        if t435 {
            var t436 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t436, local__88, global__89)
            var t439 bool = truthy(cond__94)
            var jp438 Value
            if t439 {
                var t440 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t441 Value = eval(t440, local__88, global__89)
                jp438 = t441
            } else {
                var t442 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t443 Value = eval(t442, local__88, global__89)
                jp438 = t443
            }
            jp433 = jp438
        } else {
            jp433 = Nil{}
        }
        jp422 = jp433
    case "lambda":
        var t446 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t447 bool = t446 == 3
        var jp445 Value
        if t447 {
            var mtmp82 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp449 Value
            switch mtmp82.(type) {
            case SExpr_Int:
                jp449 = Nil{}
            case SExpr_Bool:
                jp449 = Nil{}
            case SExpr_Sym:
                jp449 = Nil{}
            case List:
                var x86 *_goml_vec_SExpr = mtmp82.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x86
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t450 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t451 Value = Func{
                    _0: t450,
                }
                jp449 = t451
            default:
                panic("non-exhaustive match")
            }
            jp445 = jp449
        } else {
            jp445 = Nil{}
        }
        jp422 = jp445
    case "+":
        var t452 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t453 Value = apply_builtin("+", t452)
        jp422 = t453
    case "-":
        var t454 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t455 Value = apply_builtin("-", t454)
        jp422 = t455
    case "*":
        var t456 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t457 Value = apply_builtin("*", t456)
        jp422 = t457
    case "/":
        var t458 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t459 Value = apply_builtin("/", t458)
        jp422 = t459
    case "=":
        var t460 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t461 Value = apply_builtin("=", t460)
        jp422 = t461
    default:
        var t462 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t462, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t463 Value = apply(f__98, args__99, global__89)
        jp422 = t463
    }
    retv420 = jp422
    return retv420
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int32, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv465 Value
    var i__104 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop468:
    for {
        var t469 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
        var t470 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t471 bool = t469 < t470
        if t471 {
            var t472 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t473 SExpr = vec_get__Vec_5SExpr(items__100, t472)
            var v__106 Value = eval(t473, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t474 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t475 int32 = t474 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__104, t475)
            continue
        } else {
            break Loop_loop468
        }
    }
    var t467 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv465 = t467
    return retv465
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv478 *_goml_vec_string
    var i__108 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop481:
    for {
        var t482 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
        var t483 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t484 bool = t482 < t483
        if t484 {
            var t485 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
            var mtmp89 SExpr = vec_get__Vec_5SExpr(items__107, t485)
            switch mtmp89.(type) {
            case SExpr_Int:
                var t487 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t488 int32 = t487 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t488)
            case SExpr_Bool:
                var t490 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t491 int32 = t490 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t491)
            case SExpr_Sym:
                var x92 string = mtmp89.(SExpr_Sym)._0
                var name__111 string = x92
                var t493 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t494 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t493, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t494)
                var t495 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t496 int32 = t495 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t496)
            case List:
                var t498 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t499 int32 = t498 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t499)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop481
        }
    }
    var t480 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv478 = t480
    return retv478
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int32, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv502 *_goml_vec_Value
    var i__116 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop505:
    for {
        var t506 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
        var t507 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t508 bool = t506 < t507
        if t508 {
            var t509 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t510 SExpr = vec_get__Vec_5SExpr(items__112, t509)
            var v__119 Value = eval(t510, local__114, global__115)
            var t511 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t512 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t511, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t512)
            var t513 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t514 int32 = t513 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__116, t514)
            continue
        } else {
            break Loop_loop505
        }
    }
    var t504 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv502 = t504
    return retv502
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv517 Value
    var jp519 Value
    switch name__120 {
    case "=":
        var t522 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t523 bool = t522 == 2
        var jp521 Value
        if t523 {
            var t524 Value = vec_get__Vec_5Value(args__121, 0)
            var t525 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp98 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t524,
                _1: t525,
            }
            var x99 Value = mtmp98._0
            var x100 Value = mtmp98._1
            var jp527 Value
            switch x100.(type) {
            case Value_Int:
                var x101 int32 = x100.(Value_Int)._0
                var jp529 Value
                switch x99.(type) {
                case Value_Int:
                    var x104 int32 = x99.(Value_Int)._0
                    var a__122 int32 = x104
                    var b__123 int32 = x101
                    var t530 bool = a__122 == b__123
                    var t531 Value = Value_Bool{
                        _0: t530,
                    }
                    jp529 = t531
                case Value_Bool:
                    var t532 Value = Value_Bool{
                        _0: false,
                    }
                    jp529 = t532
                case Func:
                    var t533 Value = Value_Bool{
                        _0: false,
                    }
                    jp529 = t533
                case Nil:
                    var t534 Value = Value_Bool{
                        _0: false,
                    }
                    jp529 = t534
                default:
                    panic("non-exhaustive match")
                }
                jp527 = jp529
            case Value_Bool:
                var x102 bool = x100.(Value_Bool)._0
                var jp536 Value
                switch x99.(type) {
                case Value_Int:
                    var t537 Value = Value_Bool{
                        _0: false,
                    }
                    jp536 = t537
                case Value_Bool:
                    var x108 bool = x99.(Value_Bool)._0
                    var a__124 bool = x108
                    var b__125 bool = x102
                    var t538 bool = a__124 == b__125
                    var t539 Value = Value_Bool{
                        _0: t538,
                    }
                    jp536 = t539
                case Func:
                    var t540 Value = Value_Bool{
                        _0: false,
                    }
                    jp536 = t540
                case Nil:
                    var t541 Value = Value_Bool{
                        _0: false,
                    }
                    jp536 = t541
                default:
                    panic("non-exhaustive match")
                }
                jp527 = jp536
            case Func:
                var t542 Value = Value_Bool{
                    _0: false,
                }
                jp527 = t542
            case Nil:
                var t543 Value = Value_Bool{
                    _0: false,
                }
                jp527 = t543
            default:
                panic("non-exhaustive match")
            }
            jp521 = jp527
        } else {
            var t544 Value = Value_Bool{
                _0: false,
            }
            jp521 = t544
        }
        jp519 = jp521
        retv517 = jp519
        return retv517
    case "+":
        var i__126 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop548:
        for {
            var t549 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
            var t550 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t551 bool = t549 < t550
            if t551 {
                var t552 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                var mtmp110 Value = vec_get__Vec_5Value(args__121, t552)
                switch mtmp110.(type) {
                case Value_Int:
                    var x111 int32 = mtmp110.(Value_Int)._0
                    var n__128 int32 = x111
                    var t554 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t555 int32 = t554 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t555)
                    var t556 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t557 int32 = t556 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t557)
                case Value_Bool:
                    var t559 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t560 int32 = t559 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t560)
                case Func:
                    var t562 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t563 int32 = t562 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t563)
                case Nil:
                    var t565 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t566 int32 = t565 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t566)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop548
            }
        }
        var t546 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t547 Value = Value_Int{
            _0: t546,
        }
        jp519 = t547
        retv517 = jp519
        return retv517
    case "*":
        var i__129 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop571:
        for {
            var t572 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
            var t573 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t574 bool = t572 < t573
            if t574 {
                var t575 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                var mtmp116 Value = vec_get__Vec_5Value(args__121, t575)
                switch mtmp116.(type) {
                case Value_Int:
                    var x117 int32 = mtmp116.(Value_Int)._0
                    var n__131 int32 = x117
                    var t577 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t578 int32 = t577 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t578)
                    var t579 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t580 int32 = t579 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t580)
                case Value_Bool:
                    var t582 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t583 int32 = t582 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t583)
                case Func:
                    var t585 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t586 int32 = t585 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t586)
                case Nil:
                    var t588 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t589 int32 = t588 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t589)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop571
            }
        }
        var t569 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t570 Value = Value_Int{
            _0: t569,
        }
        jp519 = t570
        retv517 = jp519
        return retv517
    case "-":
        var mtmp122 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp592 Value
        switch mtmp122 {
        case 1:
            var mtmp123 Value = vec_get__Vec_5Value(args__121, 0)
            var jp594 Value
            switch mtmp123.(type) {
            case Value_Int:
                var x124 int32 = mtmp123.(Value_Int)._0
                var n__132 int32 = x124
                var t595 int32 = 0 - n__132
                var t596 Value = Value_Int{
                    _0: t595,
                }
                jp594 = t596
            case Value_Bool:
                jp594 = Nil{}
            case Func:
                jp594 = Nil{}
            case Nil:
                jp594 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp592 = jp594
        case 2:
            var t597 Value = vec_get__Vec_5Value(args__121, 0)
            var t598 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp127 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t597,
                _1: t598,
            }
            var x128 Value = mtmp127._0
            var x129 Value = mtmp127._1
            var jp600 Value
            switch x129.(type) {
            case Value_Int:
                var x130 int32 = x129.(Value_Int)._0
                var jp602 Value
                switch x128.(type) {
                case Value_Int:
                    var x133 int32 = x128.(Value_Int)._0
                    var a__133 int32 = x133
                    var b__134 int32 = x130
                    var t603 int32 = a__133 - b__134
                    var t604 Value = Value_Int{
                        _0: t603,
                    }
                    jp602 = t604
                case Value_Bool:
                    jp602 = Nil{}
                case Func:
                    jp602 = Nil{}
                case Nil:
                    jp602 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp600 = jp602
            case Value_Bool:
                jp600 = Nil{}
            case Func:
                jp600 = Nil{}
            case Nil:
                jp600 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp592 = jp600
        default:
            jp592 = Nil{}
        }
        jp519 = jp592
        retv517 = jp519
        return retv517
    case "/":
        var t607 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t608 bool = t607 == 2
        var jp606 Value
        if t608 {
            var t609 Value = vec_get__Vec_5Value(args__121, 0)
            var t610 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp136 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t609,
                _1: t610,
            }
            var x137 Value = mtmp136._0
            var x138 Value = mtmp136._1
            var jp612 Value
            switch x138.(type) {
            case Value_Int:
                var x139 int32 = x138.(Value_Int)._0
                var jp614 Value
                switch x137.(type) {
                case Value_Int:
                    var x142 int32 = x137.(Value_Int)._0
                    var a__135 int32 = x142
                    var b__136 int32 = x139
                    var t615 int32 = a__135 / b__136
                    var t616 Value = Value_Int{
                        _0: t615,
                    }
                    jp614 = t616
                case Value_Bool:
                    jp614 = Nil{}
                case Func:
                    jp614 = Nil{}
                case Nil:
                    jp614 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp612 = jp614
            case Value_Bool:
                jp612 = Nil{}
            case Func:
                jp612 = Nil{}
            case Nil:
                jp612 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp606 = jp612
        } else {
            jp606 = Nil{}
        }
        jp519 = jp606
        retv517 = jp519
        return retv517
    default:
        jp519 = Nil{}
        retv517 = jp519
        return retv517
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv618 Value
    var jp620 Value
    switch func__137.(type) {
    case Value_Int:
        jp620 = Nil{}
    case Value_Bool:
        jp620 = Nil{}
    case Func:
        var x147 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x147
        var t621 Value = apply_lambda(fun__140, args__138)
        jp620 = t621
    case Nil:
        jp620 = Nil{}
    default:
        panic("non-exhaustive match")
    }
    retv618 = jp620
    return retv618
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv623 Value
    var t624 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t624)
    var i__144 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop630:
    for {
        var t641 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
        var t642 *_goml_vec_string = lambda__141.params
        var t643 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t642)
        var t644 bool = t641 < t643
        var jp632 bool
        if t644 {
            var t645 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t646 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t647 bool = t645 < t646
            jp632 = t647
        } else {
            jp632 = false
        }
        if jp632 {
            var t633 *_goml_vec_string = lambda__141.params
            var t634 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var name__145 string = vec_get__Vec_6string(t633, t634)
            var t635 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t635)
            var t636 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t637 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t636, t637)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t638 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t639 int32 = t638 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__144, t639)
            continue
        } else {
            break Loop_loop630
        }
    }
    var t626 SExpr = lambda__141.body
    var t627 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t628 *ref_Vec_7Binding_x = lambda__141.global
    var t629 Value = eval(t626, t627, t628)
    retv623 = t629
    return retv623
}

func main0() struct{} {
    var t649 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t649)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t650 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t650)
    var t651 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t652 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t651, t652, global__148)
    var t653 string = value_to_string(result__151)
    println__T_string(t653)
    var t654 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t654)
    var t655 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t656 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t655, t656, global__148)
    var t657 string = value_to_string(result2__153)
    println__T_string(t657)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__4 string) int32 {
    var retv659 int32
    var t660 int32 = _goml_runtime_core_string_len(self__4)
    retv659 = t660
    return retv659
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv662 *ref_int32_x
    var t663 *ref_int32_x = ref__Ref_5int32(value__102)
    retv662 = t663
    return retv662
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__102 bool) *ref_bool_x {
    var retv665 *ref_bool_x
    var t666 *ref_bool_x = ref__Ref_4bool(value__102)
    retv665 = t666
    return retv665
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__103 *ref_bool_x) bool {
    var retv668 bool
    var t669 bool = ref_get__Ref_4bool(self__103)
    retv668 = t669
    return retv668
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv671 int32
    var t672 int32 = ref_get__Ref_5int32(self__103)
    retv671 = t672
    return retv671
}

func _goml_m_inherent_i_string_i_string_i_get(self__5 string, index__6 int32) rune {
    var retv674 rune
    var t675 rune = _goml_runtime_core_string_get(self__5, index__6)
    retv674 = t675
    return retv674
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__104 *ref_bool_x, value__105 bool) struct{} {
    ref_set__Ref_4bool(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__104 *ref_int32_x, value__105 int32) struct{} {
    ref_set__Ref_5int32(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__102 string) *ref_string_x {
    var retv681 *ref_string_x
    var t682 *ref_string_x = ref__Ref_6string(value__102)
    retv681 = t682
    return retv681
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__103 *ref_string_x) string {
    var retv684 string
    var t685 string = ref_get__Ref_6string(self__103)
    retv684 = t685
    return retv684
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv687 string
    var t688 string = _goml_runtime_core_char_to_string(self__3)
    retv687 = t688
    return retv687
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__104 *ref_string_x, value__105 string) struct{} {
    ref_set__Ref_6string(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv692 *_goml_vec_Token
    var t693 *_goml_vec_Token = vec_new__Vec_5Token()
    retv692 = t693
    return retv692
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__102 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv695 *ref_Vec_5Token_x
    var t696 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__102)
    retv695 = t696
    return retv695
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__103 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv698 *_goml_vec_Token
    var t699 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__103)
    retv698 = t699
    return retv698
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__73 *_goml_vec_Token, elem__74 Token) *_goml_vec_Token {
    var retv701 *_goml_vec_Token
    var result__75 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var index__76 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop703:
    for {
        var t704 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
        var t705 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__73)
        var t706 bool = t704 < t705
        if t706 {
            var t707 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t708 Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__73, t707)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__75, t708)
            var t709 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t710 int32 = t709 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__76, t710)
            continue
        } else {
            break Loop_loop703
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__75, elem__74)
    retv701 = result__75
    return retv701
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__104 *ref_Vec_5Token_x, value__105 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__82 *_goml_vec_Binding) int32 {
    var retv714 int32
    var t715 int32 = vec_len__Vec_7Binding(self__82)
    retv714 = t715
    return retv714
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__102 Value) *ref_Value_x {
    var retv717 *ref_Value_x
    var t718 *ref_Value_x = ref__Ref_5Value(value__102)
    retv717 = t718
    return retv717
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__104 *ref_Value_x, value__105 Value) struct{} {
    ref_set__Ref_5Value(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__103 *ref_Value_x) Value {
    var retv722 Value
    var t723 Value = ref_get__Ref_5Value(self__103)
    retv722 = t723
    return retv722
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv725 *_goml_vec_SExpr
    var t726 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv725 = t726
    return retv725
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__102 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv728 *ref_Vec_5SExpr_x
    var t729 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__102)
    retv728 = t729
    return retv728
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__82 *_goml_vec_Token) int32 {
    var retv731 int32
    var t732 int32 = vec_len__Vec_5Token(self__82)
    retv731 = t732
    return retv731
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__103 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv734 *_goml_vec_SExpr
    var t735 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__103)
    retv734 = t735
    return retv734
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__73 *_goml_vec_SExpr, elem__74 SExpr) *_goml_vec_SExpr {
    var retv737 *_goml_vec_SExpr
    var result__75 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var index__76 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop739:
    for {
        var t740 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
        var t741 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__73)
        var t742 bool = t740 < t741
        if t742 {
            var t743 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t744 SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__73, t743)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__75, t744)
            var t745 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t746 int32 = t745 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__76, t746)
            continue
        } else {
            break Loop_loop739
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__75, elem__74)
    retv737 = result__75
    return retv737
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__104 *ref_Vec_5SExpr_x, value__105 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv750 string
    var t751 string = _goml_runtime_core_int32_to_string(self__2)
    retv750 = t751
    return retv750
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv753 string
    var t754 string = _goml_runtime_core_bool_to_string(self__8)
    retv753 = t754
    return retv753
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__103 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv756 *_goml_vec_Binding
    var t757 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__103)
    retv756 = t757
    return retv756
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__82 *_goml_vec_SExpr) int32 {
    var retv759 int32
    var t760 int32 = vec_len__Vec_5SExpr(self__82)
    retv759 = t760
    return retv759
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__73 *_goml_vec_Binding, elem__74 Binding) *_goml_vec_Binding {
    var retv762 *_goml_vec_Binding
    var result__75 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var index__76 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop764:
    for {
        var t765 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
        var t766 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__73)
        var t767 bool = t765 < t766
        if t767 {
            var t768 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t769 Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__73, t768)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__75, t769)
            var t770 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t771 int32 = t770 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__76, t771)
            continue
        } else {
            break Loop_loop764
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__75, elem__74)
    retv762 = result__75
    return retv762
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__104 *ref_Vec_7Binding_x, value__105 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv775 *_goml_vec_string
    var t776 *_goml_vec_string = vec_new__Vec_6string()
    retv775 = t776
    return retv775
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__102 *_goml_vec_string) *ref_Vec_6string_x {
    var retv778 *ref_Vec_6string_x
    var t779 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__102)
    retv778 = t779
    return retv778
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__103 *ref_Vec_6string_x) *_goml_vec_string {
    var retv781 *_goml_vec_string
    var t782 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__103)
    retv781 = t782
    return retv781
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__73 *_goml_vec_string, elem__74 string) *_goml_vec_string {
    var retv784 *_goml_vec_string
    var result__75 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var index__76 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop786:
    for {
        var t787 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
        var t788 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__73)
        var t789 bool = t787 < t788
        if t789 {
            var t790 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t791 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__73, t790)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__75, t791)
            var t792 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t793 int32 = t792 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__76, t793)
            continue
        } else {
            break Loop_loop786
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__75, elem__74)
    retv784 = result__75
    return retv784
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__104 *ref_Vec_6string_x, value__105 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv797 *_goml_vec_Value
    var t798 *_goml_vec_Value = vec_new__Vec_5Value()
    retv797 = t798
    return retv797
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__102 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv800 *ref_Vec_5Value_x
    var t801 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__102)
    retv800 = t801
    return retv800
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__103 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv803 *_goml_vec_Value
    var t804 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__103)
    retv803 = t804
    return retv803
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__73 *_goml_vec_Value, elem__74 Value) *_goml_vec_Value {
    var retv806 *_goml_vec_Value
    var result__75 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var index__76 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop808:
    for {
        var t809 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
        var t810 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__73)
        var t811 bool = t809 < t810
        if t811 {
            var t812 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t813 Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__73, t812)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__75, t813)
            var t814 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__76)
            var t815 int32 = t814 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__76, t815)
            continue
        } else {
            break Loop_loop808
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__75, elem__74)
    retv806 = result__75
    return retv806
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__104 *ref_Vec_5Value_x, value__105 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__104, value__105)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__82 *_goml_vec_Value) int32 {
    var retv819 int32
    var t820 int32 = vec_len__Vec_5Value(self__82)
    retv819 = t820
    return retv819
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__102 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv822 *ref_Vec_7Binding_x
    var t823 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__102)
    retv822 = t823
    return retv822
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__82 *_goml_vec_string) int32 {
    var retv825 int32
    var t826 int32 = vec_len__Vec_6string(self__82)
    retv825 = t826
    return retv825
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv828 *_goml_vec_Binding
    var t829 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv828 = t829
    return retv828
}

func println__T_string(value__1 string) struct{} {
    var t831 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t831)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__71 *_goml_vec_Token, elem__72 Token) struct{} {
    vec_push__Vec_5Token(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__77 *_goml_vec_Token, index__78 int32) Token {
    var retv836 Token
    var t837 Token = vec_get__Vec_5Token(self__77, index__78)
    retv836 = t837
    return retv836
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__71 *_goml_vec_SExpr, elem__72 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__77 *_goml_vec_SExpr, index__78 int32) SExpr {
    var retv841 SExpr
    var t842 SExpr = vec_get__Vec_5SExpr(self__77, index__78)
    retv841 = t842
    return retv841
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__71 *_goml_vec_Binding, elem__72 Binding) struct{} {
    vec_push__Vec_7Binding(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__77 *_goml_vec_Binding, index__78 int32) Binding {
    var retv846 Binding
    var t847 Binding = vec_get__Vec_7Binding(self__77, index__78)
    retv846 = t847
    return retv846
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__71 *_goml_vec_string, elem__72 string) struct{} {
    vec_push__Vec_6string(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__77 *_goml_vec_string, index__78 int32) string {
    var retv851 string
    var t852 string = vec_get__Vec_6string(self__77, index__78)
    retv851 = t852
    return retv851
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__71 *_goml_vec_Value, elem__72 Value) struct{} {
    vec_push__Vec_5Value(self__71, elem__72)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__77 *_goml_vec_Value, index__78 int32) Value {
    var retv856 Value
    var t857 Value = vec_get__Vec_5Value(self__77, index__78)
    retv856 = t857
    return retv856
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv859 string
    retv859 = self__9
    return retv859
}

func main() {
    main0()
}
