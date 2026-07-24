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

func _goml_runtime_string_decode_utf8_at_native(s string, i int32) (bool, rune, int32) {
    if i < 0 || i >= int32(len(s)) {
        return false, 0, 0
    }
    var value rune
    var width int
    value, width = _goml_utf8.DecodeRuneInString(s[i:int32(len(s))])
    if value == _goml_utf8.RuneError && width == 1 {
        return false, 0, 0
    }
    return true, value, int32(width)
}

func _goml_runtime_core_string_get(s string, i int32) rune {
    var valid bool
    var value rune
    valid, value, _ = _goml_runtime_string_decode_utf8_at_native(s, i)
    if !valid {
        panic("invalid string byte index")
    }
    return value
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
    var retv198 bool
    var t201 bool = ch__0 >= 48
    var jp200 bool
    if t201 {
        var t202 bool = ch__0 <= 57
        jp200 = t202
    } else {
        jp200 = false
    }
    retv198 = jp200
    return retv198
}

func digit_value(ch__1 rune) int32 {
    var retv204 int32
    var jp206 int32
    switch ch__1 {
    case 48:
        jp206 = 0
    case 49:
        jp206 = 1
    case 50:
        jp206 = 2
    case 51:
        jp206 = 3
    case 52:
        jp206 = 4
    case 53:
        jp206 = 5
    case 54:
        jp206 = 6
    case 55:
        jp206 = 7
    case 56:
        jp206 = 8
    case 57:
        jp206 = 9
    default:
        jp206 = 0
    }
    retv204 = jp206
    return retv204
}

func is_int_text(text__2 string) bool {
    var retv208 bool
    var len__3 int32 = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t211 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(len__3, 0)
    var jp210 bool
    if t211 {
        jp210 = false
        retv208 = jp210
        return retv208
    } else {
        var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop217:
        for {
            var t236 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp219 bool
            if t236 {
                var t237 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t238 bool = t237 < len__3
                jp219 = t238
            } else {
                jp219 = false
            }
            if jp219 {
                var t220 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t220)
                var t233 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t234 bool = !t233
                var jp223 bool
                if t234 {
                    var t235 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__8, 45)
                    jp223 = t235
                } else {
                    jp223 = false
                }
                if jp223 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t224 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                    var t225 int32 = t224 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t225)
                } else {
                    var t228 bool = is_digit(ch__8)
                    if t228 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t229 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                        var t230 int32 = t229 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t230)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop217
            }
        }
        var t215 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp214 bool
        if t215 {
            var t216 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp214 = t216
        } else {
            jp214 = false
        }
        jp210 = jp214
        retv208 = jp210
        return retv208
    }
}

func parse_int32(text__9 string) int32 {
    var retv240 int32
    var len__10 int32 = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop248:
    for {
        var t249 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
        var t250 bool = t249 < len__10
        if t250 {
            var t251 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t251)
            var t264 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t265 bool = !t264
            var jp254 bool
            if t265 {
                var t266 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__15, 45)
                jp254 = t266
            } else {
                jp254 = false
            }
            if jp254 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t255 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t256 int32 = t255 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t256)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t258 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t259 int32 = t258 * 10
                var t260 int32 = t259 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t260)
                var t261 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t262 int32 = t261 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t262)
            }
            continue
        } else {
            break Loop_loop248
        }
    }
    var t244 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp243 int32
    if t244 {
        var t245 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t246 int32 = 0 - t245
        jp243 = t246
    } else {
        var t247 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp243 = t247
    }
    retv240 = jp243
    return retv240
}

func is_delim(ch__17 rune) bool {
    var retv268 bool
    var t274 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 40)
    var jp272 bool
    if t274 {
        jp272 = true
    } else {
        var t275 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 41)
        jp272 = t275
    }
    var jp270 bool
    if jp272 {
        jp270 = true
    } else {
        var t273 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__17, 32)
        jp270 = t273
    }
    retv268 = jp270
    return retv268
}

func lex_atom(source__18 string, start__19 int32) Tuple2_5Token_5int32 {
    var retv277 Tuple2_5Token_5int32
    var len__20 int32 = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop291:
    for {
        var t304 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t305 bool = !t304
        var jp293 bool
        if t305 {
            var t306 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var t307 bool = t306 < len__20
            jp293 = t307
        } else {
            jp293 = false
        }
        if jp293 {
            var t294 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t294)
            var t296 bool = is_delim(ch__24)
            if t296 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t298 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t299 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t300 string = t298 + t299
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t300)
                var t301 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
                var t302 int32 = t301 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__22, t302)
            }
            continue
        } else {
            break Loop_loop291
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp280 Token
    switch atom__25 {
    case "true":
        var t283 Token = Token_Bool{
            _0: true,
        }
        jp280 = t283
    case "false":
        var t284 Token = Token_Bool{
            _0: false,
        }
        jp280 = t284
    default:
        var t287 bool = is_int_text(atom__25)
        var jp286 Token
        if t287 {
            var t288 int32 = parse_int32(atom__25)
            var t289 Token = Token_Int{
                _0: t288,
            }
            jp286 = t289
        } else {
            var t290 Token = Token_Sym{
                _0: atom__25,
            }
            jp286 = t290
        }
        jp280 = jp286
    }
    var token__26 Token = jp280
    var t281 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
    var t282 Tuple2_5Token_5int32 = Tuple2_5Token_5int32{
        _0: token__26,
        _1: t281,
    }
    retv277 = t282
    return retv277
}

func lex(source__27 string) *_goml_vec_Token {
    var retv309 *_goml_vec_Token
    var len__28 int32 = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop312:
    for {
        var t313 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
        var t314 bool = t313 < len__28
        if t314 {
            var t315 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t315)
            var t317 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 40)
            if t317 {
                var t318 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t319 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t318, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t319)
                var t320 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                var t321 int32 = t320 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t321)
            } else {
                var t324 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 41)
                if t324 {
                    var t325 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t326 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t325, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t326)
                    var t327 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                    var t328 int32 = t327 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t328)
                } else {
                    var t331 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__32, 32)
                    if t331 {
                        var t332 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var t333 int32 = t332 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t333)
                    } else {
                        var t335 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var mtmp74 Tuple2_5Token_5int32 = lex_atom(source__27, t335)
                        var x75 Token = mtmp74._0
                        var x76 int32 = mtmp74._1
                        var next__34 int32 = x76
                        var tok__33 Token = x75
                        var t336 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t337 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t336, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t337)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop312
        }
    }
    var t311 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv309 = t311
    return retv309
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv340 Value
    var t341 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t342 int32 = t341 - 1
    var i__37 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t342)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop345:
    for {
        var t357 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t358 bool = !t357
        var jp347 bool
        if t358 {
            var t359 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var t360 bool = t359 >= 0
            jp347 = t360
        } else {
            jp347 = false
        }
        if jp347 {
            var t348 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t348)
            var t350 string = binding__40.name
            var t351 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t350, name__36)
            if t351 {
                var t352 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t352)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t354 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
                var t355 int32 = t354 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__37, t355)
            }
            continue
        } else {
            break Loop_loop345
        }
    }
    var t344 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv340 = t344
    return retv340
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv362 Value
    var mtmp81 Value = env_lookup(local__41, name__43)
    var jp364 Value
    switch mtmp81.(type) {
    case Nil:
        var t365 Value = env_lookup(global__42, name__43)
        jp364 = t365
    default:
        var other__44 Value = mtmp81
        jp364 = other__44
    }
    retv362 = jp364
    return retv362
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int32) Tuple2_10Vec_5SExpr_5int32 {
    var retv367 Tuple2_10Vec_5SExpr_5int32
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop372:
    for {
        var t384 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t385 bool = !t384
        var jp374 bool
        if t385 {
            var t386 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var t387 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t388 bool = t386 < t387
            jp374 = t388
        } else {
            jp374 = false
        }
        if jp374 {
            var t375 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var mtmp85 Token = vec_get__Vec_5Token(tokens__45, t375)
            switch mtmp85.(type) {
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t377 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var t378 int32 = t377 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, t378)
            default:
                var t380 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp90 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t380)
                var x91 SExpr = mtmp90._0
                var x92 int32 = mtmp90._1
                var next__52 int32 = x92
                var expr__51 SExpr = x91
                var t381 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t382 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t381, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t382)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            }
            continue
        } else {
            break Loop_loop372
        }
    }
    var t369 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t370 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
    var t371 Tuple2_10Vec_5SExpr_5int32 = Tuple2_10Vec_5SExpr_5int32{
        _0: t369,
        _1: t370,
    }
    retv367 = t371
    return retv367
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int32) Tuple2_5SExpr_5int32 {
    var retv390 Tuple2_5SExpr_5int32
    var mtmp95 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp392 Tuple2_5SExpr_5int32
    switch mtmp95.(type) {
    case LParen:
        var t393 int32 = start__54 + 1
        var mtmp99 Tuple2_10Vec_5SExpr_5int32 = parse_list(tokens__53, t393)
        var x100 *_goml_vec_SExpr = mtmp99._0
        var x101 int32 = mtmp99._1
        var next__56 int32 = x101
        var items__55 *_goml_vec_SExpr = x100
        var t394 SExpr = List{
            _0: items__55,
        }
        var t395 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t394,
            _1: next__56,
        }
        jp392 = t395
    case RParen:
        var t396 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t397 int32 = start__54 + 1
        var t398 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t396,
            _1: t397,
        }
        jp392 = t398
    case Token_Sym:
        var x96 string = mtmp95.(Token_Sym)._0
        var name__59 string = x96
        var t399 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t400 int32 = start__54 + 1
        var t401 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t399,
            _1: t400,
        }
        jp392 = t401
    case Token_Int:
        var x97 int32 = mtmp95.(Token_Int)._0
        var n__58 int32 = x97
        var t402 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t403 int32 = start__54 + 1
        var t404 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t402,
            _1: t403,
        }
        jp392 = t404
    case Token_Bool:
        var x98 bool = mtmp95.(Token_Bool)._0
        var b__57 bool = x98
        var t405 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t406 int32 = start__54 + 1
        var t407 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t405,
            _1: t406,
        }
        jp392 = t407
    default:
        panic("non-exhaustive match")
    }
    retv390 = jp392
    return retv390
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv409 *_goml_vec_SExpr
    var i__61 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop412:
    for {
        var t413 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
        var t414 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t415 bool = t413 < t414
        if t415 {
            var t416 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
            var mtmp102 Tuple2_5SExpr_5int32 = parse_expr(tokens__60, t416)
            var x103 SExpr = mtmp102._0
            var x104 int32 = mtmp102._1
            var next__65 int32 = x104
            var expr__64 SExpr = x103
            var t417 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t418 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t417, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t418)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__61, next__65)
            continue
        } else {
            break Loop_loop412
        }
    }
    var t411 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv409 = t411
    return retv409
}

func value_to_string(value__66 Value) string {
    var retv421 string
    var jp423 string
    switch value__66.(type) {
    case Value_Int:
        var x107 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x107
        var t424 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp423 = t424
    case Value_Bool:
        var x108 bool = value__66.(Value_Bool)._0
        var b__68 bool = x108
        var t425 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp423 = t425
    case Func:
        jp423 = "<lambda>"
    case Nil:
        jp423 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv421 = jp423
    return retv421
}

func truthy(value__69 Value) bool {
    var retv427 bool
    var jp429 bool
    switch value__69.(type) {
    case Value_Int:
        var x110 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x110
        var t430 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(n__71, 0)
        var t431 bool = !t430
        jp429 = t431
    case Value_Bool:
        var x111 bool = value__69.(Value_Bool)._0
        var b__70 bool = x111
        jp429 = b__70
    case Func:
        jp429 = true
    case Nil:
        jp429 = false
    default:
        panic("non-exhaustive match")
    }
    retv427 = jp429
    return retv427
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv433 Value
    var jp435 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x113 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x113
        var t436 Value = Value_Int{
            _0: n__75,
        }
        jp435 = t436
    case SExpr_Bool:
        var x114 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x114
        var t437 Value = Value_Bool{
            _0: b__76,
        }
        jp435 = t437
    case SExpr_Sym:
        var x115 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x115
        var t438 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t439 Value = lookup(local__73, t438, name__77)
        jp435 = t439
    case List:
        var x116 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x116
        var t440 Value = eval_list(items__78, local__73, global__74)
        jp435 = t440
    default:
        panic("non-exhaustive match")
    }
    retv433 = jp435
    return retv433
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv442 Value
    var t445 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t446 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t445, 0)
    var jp444 Value
    if t446 {
        jp444 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp448 Value
        switch head__82.(type) {
        case SExpr_Sym:
            var x119 string = head__82.(SExpr_Sym)._0
            var name__83 string = x119
            var t449 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp448 = t449
        default:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t450 Value = apply(f__84, args__85, global__81)
            jp448 = t450
        }
        jp444 = jp448
    }
    retv442 = jp444
    return retv442
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv452 Value
    var jp454 Value
    switch name__86 {
    case "begin":
        var t455 Value = eval_begin(items__87, 1, local__88, global__89)
        jp454 = t455
    case "define":
        var t458 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t459 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t458, 3)
        var jp457 Value
        if t459 {
            var mtmp121 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp461 Value
            switch mtmp121.(type) {
            case SExpr_Sym:
                var x124 string = mtmp121.(SExpr_Sym)._0
                var var__90 string = x124
                var t462 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t462, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t463 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t463)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp461 = value__91
            default:
                jp461 = Nil{}
            }
            jp457 = jp461
        } else {
            jp457 = Nil{}
        }
        jp454 = jp457
    case "if":
        var t466 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t467 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t466, 4)
        var jp465 Value
        if t467 {
            var t468 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t468, local__88, global__89)
            var t471 bool = truthy(cond__94)
            var jp470 Value
            if t471 {
                var t472 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t473 Value = eval(t472, local__88, global__89)
                jp470 = t473
            } else {
                var t474 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t475 Value = eval(t474, local__88, global__89)
                jp470 = t475
            }
            jp465 = jp470
        } else {
            jp465 = Nil{}
        }
        jp454 = jp465
    case "lambda":
        var t478 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t479 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t478, 3)
        var jp477 Value
        if t479 {
            var mtmp127 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp481 Value
            switch mtmp127.(type) {
            case List:
                var x131 *_goml_vec_SExpr = mtmp127.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x131
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t482 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t483 Value = Func{
                    _0: t482,
                }
                jp481 = t483
            default:
                jp481 = Nil{}
            }
            jp477 = jp481
        } else {
            jp477 = Nil{}
        }
        jp454 = jp477
    case "+":
        var t484 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t485 Value = apply_builtin("+", t484)
        jp454 = t485
    case "-":
        var t486 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t487 Value = apply_builtin("-", t486)
        jp454 = t487
    case "*":
        var t488 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t489 Value = apply_builtin("*", t488)
        jp454 = t489
    case "/":
        var t490 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t491 Value = apply_builtin("/", t490)
        jp454 = t491
    case "=":
        var t492 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t493 Value = apply_builtin("=", t492)
        jp454 = t493
    default:
        var t494 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t494, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t495 Value = apply(f__98, args__99, global__89)
        jp454 = t495
    }
    retv452 = jp454
    return retv452
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int32, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv497 Value
    var i__104 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop500:
    for {
        var t501 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
        var t502 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t503 bool = t501 < t502
        if t503 {
            var t504 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t505 SExpr = vec_get__Vec_5SExpr(items__100, t504)
            var v__106 Value = eval(t505, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t506 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t507 int32 = t506 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__104, t507)
            continue
        } else {
            break Loop_loop500
        }
    }
    var t499 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv497 = t499
    return retv497
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv510 *_goml_vec_string
    var i__108 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop513:
    for {
        var t514 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
        var t515 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t516 bool = t514 < t515
        if t516 {
            var t517 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
            var mtmp134 SExpr = vec_get__Vec_5SExpr(items__107, t517)
            switch mtmp134.(type) {
            case SExpr_Sym:
                var x137 string = mtmp134.(SExpr_Sym)._0
                var name__111 string = x137
                var t519 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t520 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t519, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t520)
                var t521 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t522 int32 = t521 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t522)
            default:
                var t524 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t525 int32 = t524 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t525)
            }
            continue
        } else {
            break Loop_loop513
        }
    }
    var t512 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv510 = t512
    return retv510
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int32, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv528 *_goml_vec_Value
    var i__116 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop531:
    for {
        var t532 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
        var t533 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t534 bool = t532 < t533
        if t534 {
            var t535 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t536 SExpr = vec_get__Vec_5SExpr(items__112, t535)
            var v__119 Value = eval(t536, local__114, global__115)
            var t537 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t538 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t537, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t538)
            var t539 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t540 int32 = t539 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__116, t540)
            continue
        } else {
            break Loop_loop531
        }
    }
    var t530 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv528 = t530
    return retv528
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv543 Value
    var jp545 Value
    switch name__120 {
    case "=":
        var t548 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t549 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t548, 2)
        var jp547 Value
        if t549 {
            var t550 Value = vec_get__Vec_5Value(args__121, 0)
            var t551 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp143 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t550,
                _1: t551,
            }
            var x144 Value = mtmp143._0
            var x145 Value = mtmp143._1
            var jp553 Value
            switch x145.(type) {
            case Value_Int:
                var x146 int32 = x145.(Value_Int)._0
                var jp555 Value
                switch x144.(type) {
                case Value_Int:
                    var x149 int32 = x144.(Value_Int)._0
                    var a__122 int32 = x149
                    var b__123 int32 = x146
                    var t556 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(a__122, b__123)
                    var t557 Value = Value_Bool{
                        _0: t556,
                    }
                    jp555 = t557
                default:
                    var t558 Value = Value_Bool{
                        _0: false,
                    }
                    jp555 = t558
                }
                jp553 = jp555
            case Value_Bool:
                var x147 bool = x145.(Value_Bool)._0
                var jp560 Value
                switch x144.(type) {
                case Value_Bool:
                    var x153 bool = x144.(Value_Bool)._0
                    var a__124 bool = x153
                    var b__125 bool = x147
                    var t561 bool = _goml_m_trait__impl_i_Eq_i_bool_i_eq(a__124, b__125)
                    var t562 Value = Value_Bool{
                        _0: t561,
                    }
                    jp560 = t562
                default:
                    var t563 Value = Value_Bool{
                        _0: false,
                    }
                    jp560 = t563
                }
                jp553 = jp560
            default:
                var t564 Value = Value_Bool{
                    _0: false,
                }
                jp553 = t564
            }
            jp547 = jp553
        } else {
            var t565 Value = Value_Bool{
                _0: false,
            }
            jp547 = t565
        }
        jp545 = jp547
        retv543 = jp545
        return retv543
    case "+":
        var i__126 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop569:
        for {
            var t570 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
            var t571 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t572 bool = t570 < t571
            if t572 {
                var t573 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                var mtmp155 Value = vec_get__Vec_5Value(args__121, t573)
                switch mtmp155.(type) {
                case Value_Int:
                    var x156 int32 = mtmp155.(Value_Int)._0
                    var n__128 int32 = x156
                    var t575 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t576 int32 = t575 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t576)
                    var t577 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t578 int32 = t577 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t578)
                default:
                    var t580 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t581 int32 = t580 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t581)
                }
                continue
            } else {
                break Loop_loop569
            }
        }
        var t567 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t568 Value = Value_Int{
            _0: t567,
        }
        jp545 = t568
        retv543 = jp545
        return retv543
    case "*":
        var i__129 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop586:
        for {
            var t587 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
            var t588 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t589 bool = t587 < t588
            if t589 {
                var t590 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                var mtmp161 Value = vec_get__Vec_5Value(args__121, t590)
                switch mtmp161.(type) {
                case Value_Int:
                    var x162 int32 = mtmp161.(Value_Int)._0
                    var n__131 int32 = x162
                    var t592 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t593 int32 = t592 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t593)
                    var t594 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t595 int32 = t594 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t595)
                default:
                    var t597 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t598 int32 = t597 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t598)
                }
                continue
            } else {
                break Loop_loop586
            }
        }
        var t584 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t585 Value = Value_Int{
            _0: t584,
        }
        jp545 = t585
        retv543 = jp545
        return retv543
    case "-":
        var mtmp167 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp601 Value
        switch mtmp167 {
        case 1:
            var mtmp168 Value = vec_get__Vec_5Value(args__121, 0)
            var jp603 Value
            switch mtmp168.(type) {
            case Value_Int:
                var x169 int32 = mtmp168.(Value_Int)._0
                var n__132 int32 = x169
                var t604 int32 = 0 - n__132
                var t605 Value = Value_Int{
                    _0: t604,
                }
                jp603 = t605
            default:
                jp603 = Nil{}
            }
            jp601 = jp603
        case 2:
            var t606 Value = vec_get__Vec_5Value(args__121, 0)
            var t607 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp172 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t606,
                _1: t607,
            }
            var x173 Value = mtmp172._0
            var x174 Value = mtmp172._1
            var jp609 Value
            switch x174.(type) {
            case Value_Int:
                var x175 int32 = x174.(Value_Int)._0
                var jp611 Value
                switch x173.(type) {
                case Value_Int:
                    var x178 int32 = x173.(Value_Int)._0
                    var a__133 int32 = x178
                    var b__134 int32 = x175
                    var t612 int32 = a__133 - b__134
                    var t613 Value = Value_Int{
                        _0: t612,
                    }
                    jp611 = t613
                default:
                    jp611 = Nil{}
                }
                jp609 = jp611
            default:
                jp609 = Nil{}
            }
            jp601 = jp609
        default:
            jp601 = Nil{}
        }
        jp545 = jp601
        retv543 = jp545
        return retv543
    case "/":
        var t616 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t617 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(t616, 2)
        var jp615 Value
        if t617 {
            var t618 Value = vec_get__Vec_5Value(args__121, 0)
            var t619 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp181 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t618,
                _1: t619,
            }
            var x182 Value = mtmp181._0
            var x183 Value = mtmp181._1
            var jp621 Value
            switch x183.(type) {
            case Value_Int:
                var x184 int32 = x183.(Value_Int)._0
                var jp623 Value
                switch x182.(type) {
                case Value_Int:
                    var x187 int32 = x182.(Value_Int)._0
                    var a__135 int32 = x187
                    var b__136 int32 = x184
                    var t624 int32 = a__135 / b__136
                    var t625 Value = Value_Int{
                        _0: t624,
                    }
                    jp623 = t625
                default:
                    jp623 = Nil{}
                }
                jp621 = jp623
            default:
                jp621 = Nil{}
            }
            jp615 = jp621
        } else {
            jp615 = Nil{}
        }
        jp545 = jp615
        retv543 = jp545
        return retv543
    default:
        jp545 = Nil{}
        retv543 = jp545
        return retv543
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv627 Value
    var jp629 Value
    switch func__137.(type) {
    case Func:
        var x192 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x192
        var t630 Value = apply_lambda(fun__140, args__138)
        jp629 = t630
    default:
        jp629 = Nil{}
    }
    retv627 = jp629
    return retv627
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv632 Value
    var t633 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t633)
    var i__144 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop639:
    for {
        var t650 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
        var t651 *_goml_vec_string = lambda__141.params
        var t652 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t651)
        var t653 bool = t650 < t652
        var jp641 bool
        if t653 {
            var t654 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t655 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t656 bool = t654 < t655
            jp641 = t656
        } else {
            jp641 = false
        }
        if jp641 {
            var t642 *_goml_vec_string = lambda__141.params
            var t643 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var name__145 string = vec_get__Vec_6string(t642, t643)
            var t644 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t644)
            var t645 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t646 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t645, t646)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t647 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t648 int32 = t647 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__144, t648)
            continue
        } else {
            break Loop_loop639
        }
    }
    var t635 SExpr = lambda__141.body
    var t636 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t637 *ref_Vec_7Binding_x = lambda__141.global
    var t638 Value = eval(t635, t636, t637)
    retv632 = t638
    return retv632
}

func main0() struct{} {
    var t658 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t658)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t659 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t659)
    var t660 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t661 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t660, t661, global__148)
    var t662 string = value_to_string(result__151)
    println__T_string(t662)
    var t663 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t663)
    var t664 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t665 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t664, t665, global__148)
    var t666 string = value_to_string(result2__153)
    println__T_string(t666)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__7 string) int32 {
    var retv668 int32
    var t669 int32 = _goml_runtime_core_string_len(self__7)
    retv668 = t669
    return retv668
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv671 bool
    var t672 bool = self__61 == other__62
    retv671 = t672
    return retv671
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv674 *ref_int32_x
    var t675 *ref_int32_x = ref__Ref_5int32(value__204)
    retv674 = t675
    return retv674
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__204 bool) *ref_bool_x {
    var retv677 *ref_bool_x
    var t678 *ref_bool_x = ref__Ref_4bool(value__204)
    retv677 = t678
    return retv677
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__205 *ref_bool_x) bool {
    var retv680 bool
    var t681 bool = ref_get__Ref_4bool(self__205)
    retv680 = t681
    return retv680
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv683 int32
    var t684 int32 = ref_get__Ref_5int32(self__205)
    retv683 = t684
    return retv683
}

func _goml_m_inherent_i_string_i_string_i_get(self__9 string, index__10 int32) rune {
    var retv686 rune
    var t687 rune = _goml_runtime_core_string_get(self__9, index__10)
    retv686 = t687
    return retv686
}

func _goml_m_trait__impl_i_Eq_i_char_i_eq(self__55 rune, other__56 rune) bool {
    var retv689 bool
    var t690 bool = self__55 == other__56
    retv689 = t690
    return retv689
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__206 *ref_bool_x, value__207 bool) struct{} {
    ref_set__Ref_4bool(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__206 *ref_int32_x, value__207 int32) struct{} {
    ref_set__Ref_5int32(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__204 string) *ref_string_x {
    var retv696 *ref_string_x
    var t697 *ref_string_x = ref__Ref_6string(value__204)
    retv696 = t697
    return retv696
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__205 *ref_string_x) string {
    var retv699 string
    var t700 string = ref_get__Ref_6string(self__205)
    retv699 = t700
    return retv699
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__6 rune) string {
    var retv702 string
    var t703 string = _goml_runtime_core_char_to_string(self__6)
    retv702 = t703
    return retv702
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__206 *ref_string_x, value__207 string) struct{} {
    ref_set__Ref_6string(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv707 *_goml_vec_Token
    var t708 *_goml_vec_Token = vec_new__Vec_5Token()
    retv707 = t708
    return retv707
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__204 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv710 *ref_Vec_5Token_x
    var t711 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__204)
    retv710 = t711
    return retv710
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__205 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv713 *_goml_vec_Token
    var t714 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__205)
    retv713 = t714
    return retv713
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__125 *_goml_vec_Token, elem__126 Token) *_goml_vec_Token {
    var retv716 *_goml_vec_Token
    var result__127 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop718:
    for {
        var t719 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t720 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__125)
        var t721 bool = t719 < t720
        if t721 {
            var t722 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t723 Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__125, t722)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__127, t723)
            var t724 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t725 int32 = t724 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t725)
            continue
        } else {
            break Loop_loop718
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__127, elem__126)
    retv716 = result__127
    return retv716
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__206 *ref_Vec_5Token_x, value__207 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__134 *_goml_vec_Binding) int32 {
    var retv729 int32
    var t730 int32 = vec_len__Vec_7Binding(self__134)
    retv729 = t730
    return retv729
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__204 Value) *ref_Value_x {
    var retv732 *ref_Value_x
    var t733 *ref_Value_x = ref__Ref_5Value(value__204)
    retv732 = t733
    return retv732
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__53 string, other__54 string) bool {
    var retv735 bool
    var t736 bool = self__53 == other__54
    retv735 = t736
    return retv735
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__206 *ref_Value_x, value__207 Value) struct{} {
    ref_set__Ref_5Value(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__205 *ref_Value_x) Value {
    var retv740 Value
    var t741 Value = ref_get__Ref_5Value(self__205)
    retv740 = t741
    return retv740
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv743 *_goml_vec_SExpr
    var t744 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv743 = t744
    return retv743
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__204 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv746 *ref_Vec_5SExpr_x
    var t747 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__204)
    retv746 = t747
    return retv746
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__134 *_goml_vec_Token) int32 {
    var retv749 int32
    var t750 int32 = vec_len__Vec_5Token(self__134)
    retv749 = t750
    return retv749
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__205 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv752 *_goml_vec_SExpr
    var t753 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__205)
    retv752 = t753
    return retv752
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__125 *_goml_vec_SExpr, elem__126 SExpr) *_goml_vec_SExpr {
    var retv755 *_goml_vec_SExpr
    var result__127 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop757:
    for {
        var t758 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t759 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__125)
        var t760 bool = t758 < t759
        if t760 {
            var t761 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t762 SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__125, t761)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__127, t762)
            var t763 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t764 int32 = t763 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t764)
            continue
        } else {
            break Loop_loop757
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__127, elem__126)
    retv755 = result__127
    return retv755
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__206 *ref_Vec_5SExpr_x, value__207 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv768 string
    var t769 string = _goml_runtime_core_int32_to_string(self__5)
    retv768 = t769
    return retv768
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv771 string
    var t772 string = _goml_runtime_core_bool_to_string(self__36)
    retv771 = t772
    return retv771
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__205 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv774 *_goml_vec_Binding
    var t775 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__205)
    retv774 = t775
    return retv774
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__134 *_goml_vec_SExpr) int32 {
    var retv777 int32
    var t778 int32 = vec_len__Vec_5SExpr(self__134)
    retv777 = t778
    return retv777
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__125 *_goml_vec_Binding, elem__126 Binding) *_goml_vec_Binding {
    var retv780 *_goml_vec_Binding
    var result__127 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop782:
    for {
        var t783 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t784 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__125)
        var t785 bool = t783 < t784
        if t785 {
            var t786 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t787 Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__125, t786)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__127, t787)
            var t788 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t789 int32 = t788 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t789)
            continue
        } else {
            break Loop_loop782
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__127, elem__126)
    retv780 = result__127
    return retv780
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__206 *ref_Vec_7Binding_x, value__207 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv793 *_goml_vec_string
    var t794 *_goml_vec_string = vec_new__Vec_6string()
    retv793 = t794
    return retv793
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__204 *_goml_vec_string) *ref_Vec_6string_x {
    var retv796 *ref_Vec_6string_x
    var t797 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__204)
    retv796 = t797
    return retv796
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__205 *ref_Vec_6string_x) *_goml_vec_string {
    var retv799 *_goml_vec_string
    var t800 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__205)
    retv799 = t800
    return retv799
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__125 *_goml_vec_string, elem__126 string) *_goml_vec_string {
    var retv802 *_goml_vec_string
    var result__127 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop804:
    for {
        var t805 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t806 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__125)
        var t807 bool = t805 < t806
        if t807 {
            var t808 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t809 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__125, t808)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__127, t809)
            var t810 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t811 int32 = t810 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t811)
            continue
        } else {
            break Loop_loop804
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__127, elem__126)
    retv802 = result__127
    return retv802
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__206 *ref_Vec_6string_x, value__207 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv815 *_goml_vec_Value
    var t816 *_goml_vec_Value = vec_new__Vec_5Value()
    retv815 = t816
    return retv815
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__204 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv818 *ref_Vec_5Value_x
    var t819 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__204)
    retv818 = t819
    return retv818
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__205 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv821 *_goml_vec_Value
    var t822 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__205)
    retv821 = t822
    return retv821
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__125 *_goml_vec_Value, elem__126 Value) *_goml_vec_Value {
    var retv824 *_goml_vec_Value
    var result__127 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop826:
    for {
        var t827 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t828 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__125)
        var t829 bool = t827 < t828
        if t829 {
            var t830 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t831 Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__125, t830)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__127, t831)
            var t832 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t833 int32 = t832 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t833)
            continue
        } else {
            break Loop_loop826
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__127, elem__126)
    retv824 = result__127
    return retv824
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__206 *ref_Vec_5Value_x, value__207 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__134 *_goml_vec_Value) int32 {
    var retv837 int32
    var t838 int32 = vec_len__Vec_5Value(self__134)
    retv837 = t838
    return retv837
}

func _goml_m_trait__impl_i_Eq_i_bool_i_eq(self__51 bool, other__52 bool) bool {
    var retv840 bool
    var t841 bool = self__51 == other__52
    retv840 = t841
    return retv840
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__204 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv843 *ref_Vec_7Binding_x
    var t844 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__204)
    retv843 = t844
    return retv843
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__134 *_goml_vec_string) int32 {
    var retv846 int32
    var t847 int32 = vec_len__Vec_6string(self__134)
    retv846 = t847
    return retv846
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv849 *_goml_vec_Binding
    var t850 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv849 = t850
    return retv849
}

func println__T_string(value__1 string) struct{} {
    var t852 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t852)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__123 *_goml_vec_Token, elem__124 Token) struct{} {
    vec_push__Vec_5Token(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__129 *_goml_vec_Token, index__130 int32) Token {
    var retv857 Token
    var t858 Token = vec_get__Vec_5Token(self__129, index__130)
    retv857 = t858
    return retv857
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__123 *_goml_vec_SExpr, elem__124 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__129 *_goml_vec_SExpr, index__130 int32) SExpr {
    var retv862 SExpr
    var t863 SExpr = vec_get__Vec_5SExpr(self__129, index__130)
    retv862 = t863
    return retv862
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__123 *_goml_vec_Binding, elem__124 Binding) struct{} {
    vec_push__Vec_7Binding(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__129 *_goml_vec_Binding, index__130 int32) Binding {
    var retv867 Binding
    var t868 Binding = vec_get__Vec_7Binding(self__129, index__130)
    retv867 = t868
    return retv867
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__123 *_goml_vec_string, elem__124 string) struct{} {
    vec_push__Vec_6string(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__129 *_goml_vec_string, index__130 int32) string {
    var retv872 string
    var t873 string = vec_get__Vec_6string(self__129, index__130)
    retv872 = t873
    return retv872
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__123 *_goml_vec_Value, elem__124 Value) struct{} {
    vec_push__Vec_5Value(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__129 *_goml_vec_Value, index__130 int32) Value {
    var retv877 Value
    var t878 Value = vec_get__Vec_5Value(self__129, index__130)
    retv877 = t878
    return retv877
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv880 string
    retv880 = self__37
    return retv880
}

func main() {
    main0()
}
