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
    var t211 bool = len__3 == 0
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
                    var t235 bool = ch__8 == 45
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
                var t266 bool = ch__15 == 45
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
    var t274 bool = ch__17 == 40
    var jp272 bool
    if t274 {
        jp272 = true
    } else {
        var t275 bool = ch__17 == 41
        jp272 = t275
    }
    var jp270 bool
    if jp272 {
        jp270 = true
    } else {
        var t273 bool = ch__17 == 32
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
            var t317 bool = ch__32 == 40
            if t317 {
                var t318 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t319 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t318, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t319)
                var t320 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                var t321 int32 = t320 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t321)
            } else {
                var t324 bool = ch__32 == 41
                if t324 {
                    var t325 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t326 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t325, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t326)
                    var t327 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                    var t328 int32 = t327 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t328)
                } else {
                    var t331 bool = ch__32 == 32
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
            var t351 bool = t350 == name__36
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
        var t430 bool = n__71 != 0
        jp429 = t430
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
    var retv432 Value
    var jp434 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x113 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x113
        var t435 Value = Value_Int{
            _0: n__75,
        }
        jp434 = t435
    case SExpr_Bool:
        var x114 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x114
        var t436 Value = Value_Bool{
            _0: b__76,
        }
        jp434 = t436
    case SExpr_Sym:
        var x115 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x115
        var t437 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t438 Value = lookup(local__73, t437, name__77)
        jp434 = t438
    case List:
        var x116 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x116
        var t439 Value = eval_list(items__78, local__73, global__74)
        jp434 = t439
    default:
        panic("non-exhaustive match")
    }
    retv432 = jp434
    return retv432
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv441 Value
    var t444 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t445 bool = t444 == 0
    var jp443 Value
    if t445 {
        jp443 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp447 Value
        switch head__82.(type) {
        case SExpr_Sym:
            var x119 string = head__82.(SExpr_Sym)._0
            var name__83 string = x119
            var t448 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp447 = t448
        default:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t449 Value = apply(f__84, args__85, global__81)
            jp447 = t449
        }
        jp443 = jp447
    }
    retv441 = jp443
    return retv441
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv451 Value
    var jp453 Value
    switch name__86 {
    case "begin":
        var t454 Value = eval_begin(items__87, 1, local__88, global__89)
        jp453 = t454
    case "define":
        var t457 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t458 bool = t457 == 3
        var jp456 Value
        if t458 {
            var mtmp121 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp460 Value
            switch mtmp121.(type) {
            case SExpr_Sym:
                var x124 string = mtmp121.(SExpr_Sym)._0
                var var__90 string = x124
                var t461 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t461, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t462 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t462)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp460 = value__91
            default:
                jp460 = Nil{}
            }
            jp456 = jp460
        } else {
            jp456 = Nil{}
        }
        jp453 = jp456
    case "if":
        var t465 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t466 bool = t465 == 4
        var jp464 Value
        if t466 {
            var t467 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t467, local__88, global__89)
            var t470 bool = truthy(cond__94)
            var jp469 Value
            if t470 {
                var t471 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t472 Value = eval(t471, local__88, global__89)
                jp469 = t472
            } else {
                var t473 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t474 Value = eval(t473, local__88, global__89)
                jp469 = t474
            }
            jp464 = jp469
        } else {
            jp464 = Nil{}
        }
        jp453 = jp464
    case "lambda":
        var t477 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t478 bool = t477 == 3
        var jp476 Value
        if t478 {
            var mtmp127 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp480 Value
            switch mtmp127.(type) {
            case List:
                var x131 *_goml_vec_SExpr = mtmp127.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x131
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t481 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t482 Value = Func{
                    _0: t481,
                }
                jp480 = t482
            default:
                jp480 = Nil{}
            }
            jp476 = jp480
        } else {
            jp476 = Nil{}
        }
        jp453 = jp476
    case "+":
        var t483 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t484 Value = apply_builtin("+", t483)
        jp453 = t484
    case "-":
        var t485 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t486 Value = apply_builtin("-", t485)
        jp453 = t486
    case "*":
        var t487 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t488 Value = apply_builtin("*", t487)
        jp453 = t488
    case "/":
        var t489 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t490 Value = apply_builtin("/", t489)
        jp453 = t490
    case "=":
        var t491 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t492 Value = apply_builtin("=", t491)
        jp453 = t492
    default:
        var t493 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t493, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t494 Value = apply(f__98, args__99, global__89)
        jp453 = t494
    }
    retv451 = jp453
    return retv451
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int32, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv496 Value
    var i__104 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop499:
    for {
        var t500 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
        var t501 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t502 bool = t500 < t501
        if t502 {
            var t503 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t504 SExpr = vec_get__Vec_5SExpr(items__100, t503)
            var v__106 Value = eval(t504, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t505 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t506 int32 = t505 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__104, t506)
            continue
        } else {
            break Loop_loop499
        }
    }
    var t498 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv496 = t498
    return retv496
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv509 *_goml_vec_string
    var i__108 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop512:
    for {
        var t513 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
        var t514 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t515 bool = t513 < t514
        if t515 {
            var t516 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
            var mtmp134 SExpr = vec_get__Vec_5SExpr(items__107, t516)
            switch mtmp134.(type) {
            case SExpr_Sym:
                var x137 string = mtmp134.(SExpr_Sym)._0
                var name__111 string = x137
                var t518 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t519 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t518, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t519)
                var t520 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t521 int32 = t520 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t521)
            default:
                var t523 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t524 int32 = t523 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t524)
            }
            continue
        } else {
            break Loop_loop512
        }
    }
    var t511 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv509 = t511
    return retv509
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int32, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv527 *_goml_vec_Value
    var i__116 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop530:
    for {
        var t531 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
        var t532 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t533 bool = t531 < t532
        if t533 {
            var t534 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t535 SExpr = vec_get__Vec_5SExpr(items__112, t534)
            var v__119 Value = eval(t535, local__114, global__115)
            var t536 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t537 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t536, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t537)
            var t538 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t539 int32 = t538 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__116, t539)
            continue
        } else {
            break Loop_loop530
        }
    }
    var t529 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv527 = t529
    return retv527
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv542 Value
    var jp544 Value
    switch name__120 {
    case "=":
        var t547 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t548 bool = t547 == 2
        var jp546 Value
        if t548 {
            var t549 Value = vec_get__Vec_5Value(args__121, 0)
            var t550 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp143 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t549,
                _1: t550,
            }
            var x144 Value = mtmp143._0
            var x145 Value = mtmp143._1
            var jp552 Value
            switch x145.(type) {
            case Value_Int:
                var x146 int32 = x145.(Value_Int)._0
                var jp554 Value
                switch x144.(type) {
                case Value_Int:
                    var x149 int32 = x144.(Value_Int)._0
                    var a__122 int32 = x149
                    var b__123 int32 = x146
                    var t555 bool = a__122 == b__123
                    var t556 Value = Value_Bool{
                        _0: t555,
                    }
                    jp554 = t556
                default:
                    var t557 Value = Value_Bool{
                        _0: false,
                    }
                    jp554 = t557
                }
                jp552 = jp554
            case Value_Bool:
                var x147 bool = x145.(Value_Bool)._0
                var jp559 Value
                switch x144.(type) {
                case Value_Bool:
                    var x153 bool = x144.(Value_Bool)._0
                    var a__124 bool = x153
                    var b__125 bool = x147
                    var t560 bool = a__124 == b__125
                    var t561 Value = Value_Bool{
                        _0: t560,
                    }
                    jp559 = t561
                default:
                    var t562 Value = Value_Bool{
                        _0: false,
                    }
                    jp559 = t562
                }
                jp552 = jp559
            default:
                var t563 Value = Value_Bool{
                    _0: false,
                }
                jp552 = t563
            }
            jp546 = jp552
        } else {
            var t564 Value = Value_Bool{
                _0: false,
            }
            jp546 = t564
        }
        jp544 = jp546
        retv542 = jp544
        return retv542
    case "+":
        var i__126 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop568:
        for {
            var t569 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
            var t570 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t571 bool = t569 < t570
            if t571 {
                var t572 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                var mtmp155 Value = vec_get__Vec_5Value(args__121, t572)
                switch mtmp155.(type) {
                case Value_Int:
                    var x156 int32 = mtmp155.(Value_Int)._0
                    var n__128 int32 = x156
                    var t574 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t575 int32 = t574 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t575)
                    var t576 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t577 int32 = t576 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t577)
                default:
                    var t579 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t580 int32 = t579 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t580)
                }
                continue
            } else {
                break Loop_loop568
            }
        }
        var t566 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t567 Value = Value_Int{
            _0: t566,
        }
        jp544 = t567
        retv542 = jp544
        return retv542
    case "*":
        var i__129 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop585:
        for {
            var t586 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
            var t587 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t588 bool = t586 < t587
            if t588 {
                var t589 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                var mtmp161 Value = vec_get__Vec_5Value(args__121, t589)
                switch mtmp161.(type) {
                case Value_Int:
                    var x162 int32 = mtmp161.(Value_Int)._0
                    var n__131 int32 = x162
                    var t591 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t592 int32 = t591 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t592)
                    var t593 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t594 int32 = t593 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t594)
                default:
                    var t596 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t597 int32 = t596 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t597)
                }
                continue
            } else {
                break Loop_loop585
            }
        }
        var t583 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t584 Value = Value_Int{
            _0: t583,
        }
        jp544 = t584
        retv542 = jp544
        return retv542
    case "-":
        var mtmp167 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp600 Value
        switch mtmp167 {
        case 1:
            var mtmp168 Value = vec_get__Vec_5Value(args__121, 0)
            var jp602 Value
            switch mtmp168.(type) {
            case Value_Int:
                var x169 int32 = mtmp168.(Value_Int)._0
                var n__132 int32 = x169
                var t603 int32 = 0 - n__132
                var t604 Value = Value_Int{
                    _0: t603,
                }
                jp602 = t604
            default:
                jp602 = Nil{}
            }
            jp600 = jp602
        case 2:
            var t605 Value = vec_get__Vec_5Value(args__121, 0)
            var t606 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp172 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t605,
                _1: t606,
            }
            var x173 Value = mtmp172._0
            var x174 Value = mtmp172._1
            var jp608 Value
            switch x174.(type) {
            case Value_Int:
                var x175 int32 = x174.(Value_Int)._0
                var jp610 Value
                switch x173.(type) {
                case Value_Int:
                    var x178 int32 = x173.(Value_Int)._0
                    var a__133 int32 = x178
                    var b__134 int32 = x175
                    var t611 int32 = a__133 - b__134
                    var t612 Value = Value_Int{
                        _0: t611,
                    }
                    jp610 = t612
                default:
                    jp610 = Nil{}
                }
                jp608 = jp610
            default:
                jp608 = Nil{}
            }
            jp600 = jp608
        default:
            jp600 = Nil{}
        }
        jp544 = jp600
        retv542 = jp544
        return retv542
    case "/":
        var t615 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t616 bool = t615 == 2
        var jp614 Value
        if t616 {
            var t617 Value = vec_get__Vec_5Value(args__121, 0)
            var t618 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp181 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t617,
                _1: t618,
            }
            var x182 Value = mtmp181._0
            var x183 Value = mtmp181._1
            var jp620 Value
            switch x183.(type) {
            case Value_Int:
                var x184 int32 = x183.(Value_Int)._0
                var jp622 Value
                switch x182.(type) {
                case Value_Int:
                    var x187 int32 = x182.(Value_Int)._0
                    var a__135 int32 = x187
                    var b__136 int32 = x184
                    var t623 int32 = a__135 / b__136
                    var t624 Value = Value_Int{
                        _0: t623,
                    }
                    jp622 = t624
                default:
                    jp622 = Nil{}
                }
                jp620 = jp622
            default:
                jp620 = Nil{}
            }
            jp614 = jp620
        } else {
            jp614 = Nil{}
        }
        jp544 = jp614
        retv542 = jp544
        return retv542
    default:
        jp544 = Nil{}
        retv542 = jp544
        return retv542
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv626 Value
    var jp628 Value
    switch func__137.(type) {
    case Func:
        var x192 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x192
        var t629 Value = apply_lambda(fun__140, args__138)
        jp628 = t629
    default:
        jp628 = Nil{}
    }
    retv626 = jp628
    return retv626
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv631 Value
    var t632 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t632)
    var i__144 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop638:
    for {
        var t649 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
        var t650 *_goml_vec_string = lambda__141.params
        var t651 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t650)
        var t652 bool = t649 < t651
        var jp640 bool
        if t652 {
            var t653 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t654 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t655 bool = t653 < t654
            jp640 = t655
        } else {
            jp640 = false
        }
        if jp640 {
            var t641 *_goml_vec_string = lambda__141.params
            var t642 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var name__145 string = vec_get__Vec_6string(t641, t642)
            var t643 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t643)
            var t644 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t645 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t644, t645)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t646 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t647 int32 = t646 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__144, t647)
            continue
        } else {
            break Loop_loop638
        }
    }
    var t634 SExpr = lambda__141.body
    var t635 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t636 *ref_Vec_7Binding_x = lambda__141.global
    var t637 Value = eval(t634, t635, t636)
    retv631 = t637
    return retv631
}

func main0() struct{} {
    var t657 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t657)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t658 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t658)
    var t659 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t660 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t659, t660, global__148)
    var t661 string = value_to_string(result__151)
    println__T_string(t661)
    var t662 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t662)
    var t663 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t664 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t663, t664, global__148)
    var t665 string = value_to_string(result2__153)
    println__T_string(t665)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__7 string) int32 {
    var retv667 int32
    var t668 int32 = _goml_runtime_core_string_len(self__7)
    retv667 = t668
    return retv667
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv670 *ref_int32_x
    var t671 *ref_int32_x = ref__Ref_5int32(value__204)
    retv670 = t671
    return retv670
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__204 bool) *ref_bool_x {
    var retv673 *ref_bool_x
    var t674 *ref_bool_x = ref__Ref_4bool(value__204)
    retv673 = t674
    return retv673
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__205 *ref_bool_x) bool {
    var retv676 bool
    var t677 bool = ref_get__Ref_4bool(self__205)
    retv676 = t677
    return retv676
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv679 int32
    var t680 int32 = ref_get__Ref_5int32(self__205)
    retv679 = t680
    return retv679
}

func _goml_m_inherent_i_string_i_string_i_get(self__9 string, index__10 int32) rune {
    var retv682 rune
    var t683 rune = _goml_runtime_core_string_get(self__9, index__10)
    retv682 = t683
    return retv682
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
    var retv689 *ref_string_x
    var t690 *ref_string_x = ref__Ref_6string(value__204)
    retv689 = t690
    return retv689
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__205 *ref_string_x) string {
    var retv692 string
    var t693 string = ref_get__Ref_6string(self__205)
    retv692 = t693
    return retv692
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__6 rune) string {
    var retv695 string
    var t696 string = _goml_runtime_core_char_to_string(self__6)
    retv695 = t696
    return retv695
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__206 *ref_string_x, value__207 string) struct{} {
    ref_set__Ref_6string(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv700 *_goml_vec_Token
    var t701 *_goml_vec_Token = vec_new__Vec_5Token()
    retv700 = t701
    return retv700
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__204 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv703 *ref_Vec_5Token_x
    var t704 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__204)
    retv703 = t704
    return retv703
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__205 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv706 *_goml_vec_Token
    var t707 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__205)
    retv706 = t707
    return retv706
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__125 *_goml_vec_Token, elem__126 Token) *_goml_vec_Token {
    var retv709 *_goml_vec_Token
    var result__127 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop711:
    for {
        var t712 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t713 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__125)
        var t714 bool = t712 < t713
        if t714 {
            var t715 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t716 Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__125, t715)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__127, t716)
            var t717 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t718 int32 = t717 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t718)
            continue
        } else {
            break Loop_loop711
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__127, elem__126)
    retv709 = result__127
    return retv709
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__206 *ref_Vec_5Token_x, value__207 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__134 *_goml_vec_Binding) int32 {
    var retv722 int32
    var t723 int32 = vec_len__Vec_7Binding(self__134)
    retv722 = t723
    return retv722
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__204 Value) *ref_Value_x {
    var retv725 *ref_Value_x
    var t726 *ref_Value_x = ref__Ref_5Value(value__204)
    retv725 = t726
    return retv725
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__206 *ref_Value_x, value__207 Value) struct{} {
    ref_set__Ref_5Value(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__205 *ref_Value_x) Value {
    var retv730 Value
    var t731 Value = ref_get__Ref_5Value(self__205)
    retv730 = t731
    return retv730
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv733 *_goml_vec_SExpr
    var t734 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv733 = t734
    return retv733
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__204 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv736 *ref_Vec_5SExpr_x
    var t737 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__204)
    retv736 = t737
    return retv736
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__134 *_goml_vec_Token) int32 {
    var retv739 int32
    var t740 int32 = vec_len__Vec_5Token(self__134)
    retv739 = t740
    return retv739
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__205 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv742 *_goml_vec_SExpr
    var t743 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__205)
    retv742 = t743
    return retv742
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__125 *_goml_vec_SExpr, elem__126 SExpr) *_goml_vec_SExpr {
    var retv745 *_goml_vec_SExpr
    var result__127 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop747:
    for {
        var t748 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t749 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__125)
        var t750 bool = t748 < t749
        if t750 {
            var t751 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t752 SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__125, t751)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__127, t752)
            var t753 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t754 int32 = t753 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t754)
            continue
        } else {
            break Loop_loop747
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__127, elem__126)
    retv745 = result__127
    return retv745
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__206 *ref_Vec_5SExpr_x, value__207 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv758 string
    var t759 string = _goml_runtime_core_int32_to_string(self__5)
    retv758 = t759
    return retv758
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv761 string
    var t762 string = _goml_runtime_core_bool_to_string(self__36)
    retv761 = t762
    return retv761
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__205 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv764 *_goml_vec_Binding
    var t765 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__205)
    retv764 = t765
    return retv764
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__134 *_goml_vec_SExpr) int32 {
    var retv767 int32
    var t768 int32 = vec_len__Vec_5SExpr(self__134)
    retv767 = t768
    return retv767
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__125 *_goml_vec_Binding, elem__126 Binding) *_goml_vec_Binding {
    var retv770 *_goml_vec_Binding
    var result__127 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop772:
    for {
        var t773 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t774 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__125)
        var t775 bool = t773 < t774
        if t775 {
            var t776 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t777 Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__125, t776)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__127, t777)
            var t778 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t779 int32 = t778 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t779)
            continue
        } else {
            break Loop_loop772
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__127, elem__126)
    retv770 = result__127
    return retv770
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__206 *ref_Vec_7Binding_x, value__207 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv783 *_goml_vec_string
    var t784 *_goml_vec_string = vec_new__Vec_6string()
    retv783 = t784
    return retv783
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__204 *_goml_vec_string) *ref_Vec_6string_x {
    var retv786 *ref_Vec_6string_x
    var t787 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__204)
    retv786 = t787
    return retv786
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__205 *ref_Vec_6string_x) *_goml_vec_string {
    var retv789 *_goml_vec_string
    var t790 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__205)
    retv789 = t790
    return retv789
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__125 *_goml_vec_string, elem__126 string) *_goml_vec_string {
    var retv792 *_goml_vec_string
    var result__127 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop794:
    for {
        var t795 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t796 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__125)
        var t797 bool = t795 < t796
        if t797 {
            var t798 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t799 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__125, t798)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__127, t799)
            var t800 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t801 int32 = t800 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t801)
            continue
        } else {
            break Loop_loop794
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__127, elem__126)
    retv792 = result__127
    return retv792
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__206 *ref_Vec_6string_x, value__207 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv805 *_goml_vec_Value
    var t806 *_goml_vec_Value = vec_new__Vec_5Value()
    retv805 = t806
    return retv805
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__204 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv808 *ref_Vec_5Value_x
    var t809 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__204)
    retv808 = t809
    return retv808
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__205 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv811 *_goml_vec_Value
    var t812 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__205)
    retv811 = t812
    return retv811
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__125 *_goml_vec_Value, elem__126 Value) *_goml_vec_Value {
    var retv814 *_goml_vec_Value
    var result__127 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop816:
    for {
        var t817 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t818 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__125)
        var t819 bool = t817 < t818
        if t819 {
            var t820 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t821 Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__125, t820)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__127, t821)
            var t822 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t823 int32 = t822 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t823)
            continue
        } else {
            break Loop_loop816
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__127, elem__126)
    retv814 = result__127
    return retv814
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__206 *ref_Vec_5Value_x, value__207 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__134 *_goml_vec_Value) int32 {
    var retv827 int32
    var t828 int32 = vec_len__Vec_5Value(self__134)
    retv827 = t828
    return retv827
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__204 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv830 *ref_Vec_7Binding_x
    var t831 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__204)
    retv830 = t831
    return retv830
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__134 *_goml_vec_string) int32 {
    var retv833 int32
    var t834 int32 = vec_len__Vec_6string(self__134)
    retv833 = t834
    return retv833
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv836 *_goml_vec_Binding
    var t837 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv836 = t837
    return retv836
}

func println__T_string(value__1 string) struct{} {
    var t839 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t839)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__123 *_goml_vec_Token, elem__124 Token) struct{} {
    vec_push__Vec_5Token(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__129 *_goml_vec_Token, index__130 int32) Token {
    var retv844 Token
    var t845 Token = vec_get__Vec_5Token(self__129, index__130)
    retv844 = t845
    return retv844
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__123 *_goml_vec_SExpr, elem__124 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__129 *_goml_vec_SExpr, index__130 int32) SExpr {
    var retv849 SExpr
    var t850 SExpr = vec_get__Vec_5SExpr(self__129, index__130)
    retv849 = t850
    return retv849
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__123 *_goml_vec_Binding, elem__124 Binding) struct{} {
    vec_push__Vec_7Binding(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__129 *_goml_vec_Binding, index__130 int32) Binding {
    var retv854 Binding
    var t855 Binding = vec_get__Vec_7Binding(self__129, index__130)
    retv854 = t855
    return retv854
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__123 *_goml_vec_string, elem__124 string) struct{} {
    vec_push__Vec_6string(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__129 *_goml_vec_string, index__130 int32) string {
    var retv859 string
    var t860 string = vec_get__Vec_6string(self__129, index__130)
    retv859 = t860
    return retv859
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__123 *_goml_vec_Value, elem__124 Value) struct{} {
    vec_push__Vec_5Value(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__129 *_goml_vec_Value, index__130 int32) Value {
    var retv864 Value
    var t865 Value = vec_get__Vec_5Value(self__129, index__130)
    retv864 = t865
    return retv864
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv867 string
    retv867 = self__37
    return retv867
}

func main() {
    main0()
}
