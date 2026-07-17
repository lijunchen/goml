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
    var retv207 bool
    var t210 bool = ch__0 >= 48
    var jp209 bool
    if t210 {
        var t211 bool = ch__0 <= 57
        jp209 = t211
    } else {
        jp209 = false
    }
    retv207 = jp209
    return retv207
}

func digit_value(ch__1 rune) int32 {
    var retv213 int32
    var jp215 int32
    switch ch__1 {
    case 48:
        jp215 = 0
    case 49:
        jp215 = 1
    case 50:
        jp215 = 2
    case 51:
        jp215 = 3
    case 52:
        jp215 = 4
    case 53:
        jp215 = 5
    case 54:
        jp215 = 6
    case 55:
        jp215 = 7
    case 56:
        jp215 = 8
    case 57:
        jp215 = 9
    default:
        jp215 = 0
    }
    retv213 = jp215
    return retv213
}

func is_int_text(text__2 string) bool {
    var retv217 bool
    var len__3 int32 = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t220 bool = len__3 == 0
    var jp219 bool
    if t220 {
        jp219 = false
        retv217 = jp219
        return retv217
    } else {
        var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop226:
        for {
            var t245 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp228 bool
            if t245 {
                var t246 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t247 bool = t246 < len__3
                jp228 = t247
            } else {
                jp228 = false
            }
            if jp228 {
                var t229 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t229)
                var t242 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t243 bool = !t242
                var jp232 bool
                if t243 {
                    var t244 bool = ch__8 == 45
                    jp232 = t244
                } else {
                    jp232 = false
                }
                if jp232 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t233 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                    var t234 int32 = t233 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t234)
                } else {
                    var t237 bool = is_digit(ch__8)
                    if t237 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t238 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                        var t239 int32 = t238 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t239)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop226
            }
        }
        var t224 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp223 bool
        if t224 {
            var t225 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp223 = t225
        } else {
            jp223 = false
        }
        jp219 = jp223
        retv217 = jp219
        return retv217
    }
}

func parse_int32(text__9 string) int32 {
    var retv249 int32
    var len__10 int32 = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop257:
    for {
        var t258 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
        var t259 bool = t258 < len__10
        if t259 {
            var t260 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t260)
            var t273 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t274 bool = !t273
            var jp263 bool
            if t274 {
                var t275 bool = ch__15 == 45
                jp263 = t275
            } else {
                jp263 = false
            }
            if jp263 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t264 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t265 int32 = t264 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t265)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t267 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t268 int32 = t267 * 10
                var t269 int32 = t268 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t269)
                var t270 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t271 int32 = t270 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t271)
            }
            continue
        } else {
            break Loop_loop257
        }
    }
    var t253 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp252 int32
    if t253 {
        var t254 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t255 int32 = 0 - t254
        jp252 = t255
    } else {
        var t256 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp252 = t256
    }
    retv249 = jp252
    return retv249
}

func is_delim(ch__17 rune) bool {
    var retv277 bool
    var t283 bool = ch__17 == 40
    var jp281 bool
    if t283 {
        jp281 = true
    } else {
        var t284 bool = ch__17 == 41
        jp281 = t284
    }
    var jp279 bool
    if jp281 {
        jp279 = true
    } else {
        var t282 bool = ch__17 == 32
        jp279 = t282
    }
    retv277 = jp279
    return retv277
}

func lex_atom(source__18 string, start__19 int32) Tuple2_5Token_5int32 {
    var retv286 Tuple2_5Token_5int32
    var len__20 int32 = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop300:
    for {
        var t313 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t314 bool = !t313
        var jp302 bool
        if t314 {
            var t315 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var t316 bool = t315 < len__20
            jp302 = t316
        } else {
            jp302 = false
        }
        if jp302 {
            var t303 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t303)
            var t305 bool = is_delim(ch__24)
            if t305 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t307 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t308 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t309 string = t307 + t308
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t309)
                var t310 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
                var t311 int32 = t310 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__22, t311)
            }
            continue
        } else {
            break Loop_loop300
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp289 Token
    switch atom__25 {
    case "true":
        var t292 Token = Token_Bool{
            _0: true,
        }
        jp289 = t292
    case "false":
        var t293 Token = Token_Bool{
            _0: false,
        }
        jp289 = t293
    default:
        var t296 bool = is_int_text(atom__25)
        var jp295 Token
        if t296 {
            var t297 int32 = parse_int32(atom__25)
            var t298 Token = Token_Int{
                _0: t297,
            }
            jp295 = t298
        } else {
            var t299 Token = Token_Sym{
                _0: atom__25,
            }
            jp295 = t299
        }
        jp289 = jp295
    }
    var token__26 Token = jp289
    var t290 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
    var t291 Tuple2_5Token_5int32 = Tuple2_5Token_5int32{
        _0: token__26,
        _1: t290,
    }
    retv286 = t291
    return retv286
}

func lex(source__27 string) *_goml_vec_Token {
    var retv318 *_goml_vec_Token
    var len__28 int32 = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop321:
    for {
        var t322 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
        var t323 bool = t322 < len__28
        if t323 {
            var t324 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t324)
            var t326 bool = ch__32 == 40
            if t326 {
                var t327 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t328 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t327, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t328)
                var t329 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                var t330 int32 = t329 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t330)
            } else {
                var t333 bool = ch__32 == 41
                if t333 {
                    var t334 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t335 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t334, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t335)
                    var t336 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                    var t337 int32 = t336 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t337)
                } else {
                    var t340 bool = ch__32 == 32
                    if t340 {
                        var t341 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var t342 int32 = t341 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t342)
                    } else {
                        var t344 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var mtmp71 Tuple2_5Token_5int32 = lex_atom(source__27, t344)
                        var x72 Token = mtmp71._0
                        var x73 int32 = mtmp71._1
                        var next__34 int32 = x73
                        var tok__33 Token = x72
                        var t345 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t346 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t345, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t346)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop321
        }
    }
    var t320 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv318 = t320
    return retv318
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv349 Value
    var t350 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t351 int32 = t350 - 1
    var i__37 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t351)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop354:
    for {
        var t366 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t367 bool = !t366
        var jp356 bool
        if t367 {
            var t368 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var t369 bool = t368 >= 0
            jp356 = t369
        } else {
            jp356 = false
        }
        if jp356 {
            var t357 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t357)
            var t359 string = binding__40.name
            var t360 bool = t359 == name__36
            if t360 {
                var t361 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t361)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t363 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
                var t364 int32 = t363 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__37, t364)
            }
            continue
        } else {
            break Loop_loop354
        }
    }
    var t353 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv349 = t353
    return retv349
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv371 Value
    var mtmp78 Value = env_lookup(local__41, name__43)
    var jp373 Value
    switch mtmp78.(type) {
    case Value_Int:
        var other__44 Value = mtmp78
        jp373 = other__44
    case Value_Bool:
        var other__44 Value = mtmp78
        jp373 = other__44
    case Func:
        var other__44 Value = mtmp78
        jp373 = other__44
    case Nil:
        var t374 Value = env_lookup(global__42, name__43)
        jp373 = t374
    default:
        panic("non-exhaustive match")
    }
    retv371 = jp373
    return retv371
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int32) Tuple2_10Vec_5SExpr_5int32 {
    var retv376 Tuple2_10Vec_5SExpr_5int32
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop381:
    for {
        var t405 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t406 bool = !t405
        var jp383 bool
        if t406 {
            var t407 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var t408 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t409 bool = t407 < t408
            jp383 = t409
        } else {
            jp383 = false
        }
        if jp383 {
            var t384 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var mtmp82 Token = vec_get__Vec_5Token(tokens__45, t384)
            switch mtmp82.(type) {
            case LParen:
                var t386 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp86 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t386)
                var x87 SExpr = mtmp86._0
                var x88 int32 = mtmp86._1
                var next__52 int32 = x88
                var expr__51 SExpr = x87
                var t387 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t388 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t387, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t388)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t390 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var t391 int32 = t390 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, t391)
            case Token_Sym:
                var t393 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp91 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t393)
                var x92 SExpr = mtmp91._0
                var x93 int32 = mtmp91._1
                var next__52 int32 = x93
                var expr__51 SExpr = x92
                var t394 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t395 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t394, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t395)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case Token_Int:
                var t397 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp95 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t397)
                var x96 SExpr = mtmp95._0
                var x97 int32 = mtmp95._1
                var next__52 int32 = x97
                var expr__51 SExpr = x96
                var t398 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t399 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t398, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t399)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case Token_Bool:
                var t401 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp99 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t401)
                var x100 SExpr = mtmp99._0
                var x101 int32 = mtmp99._1
                var next__52 int32 = x101
                var expr__51 SExpr = x100
                var t402 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t403 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t402, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t403)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop381
        }
    }
    var t378 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t379 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
    var t380 Tuple2_10Vec_5SExpr_5int32 = Tuple2_10Vec_5SExpr_5int32{
        _0: t378,
        _1: t379,
    }
    retv376 = t380
    return retv376
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int32) Tuple2_5SExpr_5int32 {
    var retv411 Tuple2_5SExpr_5int32
    var mtmp104 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp413 Tuple2_5SExpr_5int32
    switch mtmp104.(type) {
    case LParen:
        var t414 int32 = start__54 + 1
        var mtmp108 Tuple2_10Vec_5SExpr_5int32 = parse_list(tokens__53, t414)
        var x109 *_goml_vec_SExpr = mtmp108._0
        var x110 int32 = mtmp108._1
        var next__56 int32 = x110
        var items__55 *_goml_vec_SExpr = x109
        var t415 SExpr = List{
            _0: items__55,
        }
        var t416 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t415,
            _1: next__56,
        }
        jp413 = t416
    case RParen:
        var t417 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t418 int32 = start__54 + 1
        var t419 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t417,
            _1: t418,
        }
        jp413 = t419
    case Token_Sym:
        var x105 string = mtmp104.(Token_Sym)._0
        var name__59 string = x105
        var t420 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t421 int32 = start__54 + 1
        var t422 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t420,
            _1: t421,
        }
        jp413 = t422
    case Token_Int:
        var x106 int32 = mtmp104.(Token_Int)._0
        var n__58 int32 = x106
        var t423 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t424 int32 = start__54 + 1
        var t425 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t423,
            _1: t424,
        }
        jp413 = t425
    case Token_Bool:
        var x107 bool = mtmp104.(Token_Bool)._0
        var b__57 bool = x107
        var t426 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t427 int32 = start__54 + 1
        var t428 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t426,
            _1: t427,
        }
        jp413 = t428
    default:
        panic("non-exhaustive match")
    }
    retv411 = jp413
    return retv411
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv430 *_goml_vec_SExpr
    var i__61 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop433:
    for {
        var t434 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
        var t435 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t436 bool = t434 < t435
        if t436 {
            var t437 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
            var mtmp111 Tuple2_5SExpr_5int32 = parse_expr(tokens__60, t437)
            var x112 SExpr = mtmp111._0
            var x113 int32 = mtmp111._1
            var next__65 int32 = x113
            var expr__64 SExpr = x112
            var t438 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t439 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t438, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t439)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__61, next__65)
            continue
        } else {
            break Loop_loop433
        }
    }
    var t432 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv430 = t432
    return retv430
}

func value_to_string(value__66 Value) string {
    var retv442 string
    var jp444 string
    switch value__66.(type) {
    case Value_Int:
        var x116 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x116
        var t445 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp444 = t445
    case Value_Bool:
        var x117 bool = value__66.(Value_Bool)._0
        var b__68 bool = x117
        var t446 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp444 = t446
    case Func:
        jp444 = "<lambda>"
    case Nil:
        jp444 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv442 = jp444
    return retv442
}

func truthy(value__69 Value) bool {
    var retv448 bool
    var jp450 bool
    switch value__69.(type) {
    case Value_Int:
        var x119 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x119
        var t451 bool = n__71 != 0
        jp450 = t451
    case Value_Bool:
        var x120 bool = value__69.(Value_Bool)._0
        var b__70 bool = x120
        jp450 = b__70
    case Func:
        jp450 = true
    case Nil:
        jp450 = false
    default:
        panic("non-exhaustive match")
    }
    retv448 = jp450
    return retv448
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv453 Value
    var jp455 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x122 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x122
        var t456 Value = Value_Int{
            _0: n__75,
        }
        jp455 = t456
    case SExpr_Bool:
        var x123 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x123
        var t457 Value = Value_Bool{
            _0: b__76,
        }
        jp455 = t457
    case SExpr_Sym:
        var x124 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x124
        var t458 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t459 Value = lookup(local__73, t458, name__77)
        jp455 = t459
    case List:
        var x125 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x125
        var t460 Value = eval_list(items__78, local__73, global__74)
        jp455 = t460
    default:
        panic("non-exhaustive match")
    }
    retv453 = jp455
    return retv453
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv462 Value
    var t465 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t466 bool = t465 == 0
    var jp464 Value
    if t466 {
        jp464 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp468 Value
        switch head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t469 Value = apply(f__84, args__85, global__81)
            jp468 = t469
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t470 Value = apply(f__84, args__85, global__81)
            jp468 = t470
        case SExpr_Sym:
            var x128 string = head__82.(SExpr_Sym)._0
            var name__83 string = x128
            var t471 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp468 = t471
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t472 Value = apply(f__84, args__85, global__81)
            jp468 = t472
        default:
            panic("non-exhaustive match")
        }
        jp464 = jp468
    }
    retv462 = jp464
    return retv462
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv474 Value
    var jp476 Value
    switch name__86 {
    case "begin":
        var t477 Value = eval_begin(items__87, 1, local__88, global__89)
        jp476 = t477
    case "define":
        var t480 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t481 bool = t480 == 3
        var jp479 Value
        if t481 {
            var mtmp130 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp483 Value
            switch mtmp130.(type) {
            case SExpr_Int:
                jp483 = Nil{}
            case SExpr_Bool:
                jp483 = Nil{}
            case SExpr_Sym:
                var x133 string = mtmp130.(SExpr_Sym)._0
                var var__90 string = x133
                var t484 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t484, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t485 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t485)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp483 = value__91
            case List:
                jp483 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp479 = jp483
        } else {
            jp479 = Nil{}
        }
        jp476 = jp479
    case "if":
        var t488 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t489 bool = t488 == 4
        var jp487 Value
        if t489 {
            var t490 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t490, local__88, global__89)
            var t493 bool = truthy(cond__94)
            var jp492 Value
            if t493 {
                var t494 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t495 Value = eval(t494, local__88, global__89)
                jp492 = t495
            } else {
                var t496 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t497 Value = eval(t496, local__88, global__89)
                jp492 = t497
            }
            jp487 = jp492
        } else {
            jp487 = Nil{}
        }
        jp476 = jp487
    case "lambda":
        var t500 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t501 bool = t500 == 3
        var jp499 Value
        if t501 {
            var mtmp136 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp503 Value
            switch mtmp136.(type) {
            case SExpr_Int:
                jp503 = Nil{}
            case SExpr_Bool:
                jp503 = Nil{}
            case SExpr_Sym:
                jp503 = Nil{}
            case List:
                var x140 *_goml_vec_SExpr = mtmp136.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x140
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t504 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t505 Value = Func{
                    _0: t504,
                }
                jp503 = t505
            default:
                panic("non-exhaustive match")
            }
            jp499 = jp503
        } else {
            jp499 = Nil{}
        }
        jp476 = jp499
    case "+":
        var t506 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t507 Value = apply_builtin("+", t506)
        jp476 = t507
    case "-":
        var t508 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t509 Value = apply_builtin("-", t508)
        jp476 = t509
    case "*":
        var t510 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t511 Value = apply_builtin("*", t510)
        jp476 = t511
    case "/":
        var t512 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t513 Value = apply_builtin("/", t512)
        jp476 = t513
    case "=":
        var t514 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t515 Value = apply_builtin("=", t514)
        jp476 = t515
    default:
        var t516 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t516, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t517 Value = apply(f__98, args__99, global__89)
        jp476 = t517
    }
    retv474 = jp476
    return retv474
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int32, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv519 Value
    var i__104 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop522:
    for {
        var t523 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
        var t524 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t525 bool = t523 < t524
        if t525 {
            var t526 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t527 SExpr = vec_get__Vec_5SExpr(items__100, t526)
            var v__106 Value = eval(t527, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t528 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t529 int32 = t528 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__104, t529)
            continue
        } else {
            break Loop_loop522
        }
    }
    var t521 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv519 = t521
    return retv519
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv532 *_goml_vec_string
    var i__108 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop535:
    for {
        var t536 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
        var t537 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t538 bool = t536 < t537
        if t538 {
            var t539 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
            var mtmp143 SExpr = vec_get__Vec_5SExpr(items__107, t539)
            switch mtmp143.(type) {
            case SExpr_Int:
                var t541 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t542 int32 = t541 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t542)
            case SExpr_Bool:
                var t544 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t545 int32 = t544 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t545)
            case SExpr_Sym:
                var x146 string = mtmp143.(SExpr_Sym)._0
                var name__111 string = x146
                var t547 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t548 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t547, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t548)
                var t549 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t550 int32 = t549 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t550)
            case List:
                var t552 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t553 int32 = t552 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t553)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop535
        }
    }
    var t534 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv532 = t534
    return retv532
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int32, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv556 *_goml_vec_Value
    var i__116 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop559:
    for {
        var t560 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
        var t561 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t562 bool = t560 < t561
        if t562 {
            var t563 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t564 SExpr = vec_get__Vec_5SExpr(items__112, t563)
            var v__119 Value = eval(t564, local__114, global__115)
            var t565 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t566 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t565, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t566)
            var t567 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t568 int32 = t567 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__116, t568)
            continue
        } else {
            break Loop_loop559
        }
    }
    var t558 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv556 = t558
    return retv556
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv571 Value
    var jp573 Value
    switch name__120 {
    case "=":
        var t576 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t577 bool = t576 == 2
        var jp575 Value
        if t577 {
            var t578 Value = vec_get__Vec_5Value(args__121, 0)
            var t579 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp152 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t578,
                _1: t579,
            }
            var x153 Value = mtmp152._0
            var x154 Value = mtmp152._1
            var jp581 Value
            switch x154.(type) {
            case Value_Int:
                var x155 int32 = x154.(Value_Int)._0
                var jp583 Value
                switch x153.(type) {
                case Value_Int:
                    var x158 int32 = x153.(Value_Int)._0
                    var a__122 int32 = x158
                    var b__123 int32 = x155
                    var t584 bool = a__122 == b__123
                    var t585 Value = Value_Bool{
                        _0: t584,
                    }
                    jp583 = t585
                case Value_Bool:
                    var t586 Value = Value_Bool{
                        _0: false,
                    }
                    jp583 = t586
                case Func:
                    var t587 Value = Value_Bool{
                        _0: false,
                    }
                    jp583 = t587
                case Nil:
                    var t588 Value = Value_Bool{
                        _0: false,
                    }
                    jp583 = t588
                default:
                    panic("non-exhaustive match")
                }
                jp581 = jp583
            case Value_Bool:
                var x156 bool = x154.(Value_Bool)._0
                var jp590 Value
                switch x153.(type) {
                case Value_Int:
                    var t591 Value = Value_Bool{
                        _0: false,
                    }
                    jp590 = t591
                case Value_Bool:
                    var x162 bool = x153.(Value_Bool)._0
                    var a__124 bool = x162
                    var b__125 bool = x156
                    var t592 bool = a__124 == b__125
                    var t593 Value = Value_Bool{
                        _0: t592,
                    }
                    jp590 = t593
                case Func:
                    var t594 Value = Value_Bool{
                        _0: false,
                    }
                    jp590 = t594
                case Nil:
                    var t595 Value = Value_Bool{
                        _0: false,
                    }
                    jp590 = t595
                default:
                    panic("non-exhaustive match")
                }
                jp581 = jp590
            case Func:
                var t596 Value = Value_Bool{
                    _0: false,
                }
                jp581 = t596
            case Nil:
                var t597 Value = Value_Bool{
                    _0: false,
                }
                jp581 = t597
            default:
                panic("non-exhaustive match")
            }
            jp575 = jp581
        } else {
            var t598 Value = Value_Bool{
                _0: false,
            }
            jp575 = t598
        }
        jp573 = jp575
        retv571 = jp573
        return retv571
    case "+":
        var i__126 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop602:
        for {
            var t603 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
            var t604 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t605 bool = t603 < t604
            if t605 {
                var t606 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                var mtmp164 Value = vec_get__Vec_5Value(args__121, t606)
                switch mtmp164.(type) {
                case Value_Int:
                    var x165 int32 = mtmp164.(Value_Int)._0
                    var n__128 int32 = x165
                    var t608 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t609 int32 = t608 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t609)
                    var t610 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t611 int32 = t610 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t611)
                case Value_Bool:
                    var t613 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t614 int32 = t613 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t614)
                case Func:
                    var t616 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t617 int32 = t616 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t617)
                case Nil:
                    var t619 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t620 int32 = t619 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t620)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop602
            }
        }
        var t600 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t601 Value = Value_Int{
            _0: t600,
        }
        jp573 = t601
        retv571 = jp573
        return retv571
    case "*":
        var i__129 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop625:
        for {
            var t626 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
            var t627 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t628 bool = t626 < t627
            if t628 {
                var t629 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                var mtmp170 Value = vec_get__Vec_5Value(args__121, t629)
                switch mtmp170.(type) {
                case Value_Int:
                    var x171 int32 = mtmp170.(Value_Int)._0
                    var n__131 int32 = x171
                    var t631 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t632 int32 = t631 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t632)
                    var t633 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t634 int32 = t633 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t634)
                case Value_Bool:
                    var t636 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t637 int32 = t636 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t637)
                case Func:
                    var t639 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t640 int32 = t639 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t640)
                case Nil:
                    var t642 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t643 int32 = t642 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t643)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop625
            }
        }
        var t623 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t624 Value = Value_Int{
            _0: t623,
        }
        jp573 = t624
        retv571 = jp573
        return retv571
    case "-":
        var mtmp176 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp646 Value
        switch mtmp176 {
        case 1:
            var mtmp177 Value = vec_get__Vec_5Value(args__121, 0)
            var jp648 Value
            switch mtmp177.(type) {
            case Value_Int:
                var x178 int32 = mtmp177.(Value_Int)._0
                var n__132 int32 = x178
                var t649 int32 = 0 - n__132
                var t650 Value = Value_Int{
                    _0: t649,
                }
                jp648 = t650
            case Value_Bool:
                jp648 = Nil{}
            case Func:
                jp648 = Nil{}
            case Nil:
                jp648 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp646 = jp648
        case 2:
            var t651 Value = vec_get__Vec_5Value(args__121, 0)
            var t652 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp181 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t651,
                _1: t652,
            }
            var x182 Value = mtmp181._0
            var x183 Value = mtmp181._1
            var jp654 Value
            switch x183.(type) {
            case Value_Int:
                var x184 int32 = x183.(Value_Int)._0
                var jp656 Value
                switch x182.(type) {
                case Value_Int:
                    var x187 int32 = x182.(Value_Int)._0
                    var a__133 int32 = x187
                    var b__134 int32 = x184
                    var t657 int32 = a__133 - b__134
                    var t658 Value = Value_Int{
                        _0: t657,
                    }
                    jp656 = t658
                case Value_Bool:
                    jp656 = Nil{}
                case Func:
                    jp656 = Nil{}
                case Nil:
                    jp656 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp654 = jp656
            case Value_Bool:
                jp654 = Nil{}
            case Func:
                jp654 = Nil{}
            case Nil:
                jp654 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp646 = jp654
        default:
            jp646 = Nil{}
        }
        jp573 = jp646
        retv571 = jp573
        return retv571
    case "/":
        var t661 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t662 bool = t661 == 2
        var jp660 Value
        if t662 {
            var t663 Value = vec_get__Vec_5Value(args__121, 0)
            var t664 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp190 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t663,
                _1: t664,
            }
            var x191 Value = mtmp190._0
            var x192 Value = mtmp190._1
            var jp666 Value
            switch x192.(type) {
            case Value_Int:
                var x193 int32 = x192.(Value_Int)._0
                var jp668 Value
                switch x191.(type) {
                case Value_Int:
                    var x196 int32 = x191.(Value_Int)._0
                    var a__135 int32 = x196
                    var b__136 int32 = x193
                    var t669 int32 = a__135 / b__136
                    var t670 Value = Value_Int{
                        _0: t669,
                    }
                    jp668 = t670
                case Value_Bool:
                    jp668 = Nil{}
                case Func:
                    jp668 = Nil{}
                case Nil:
                    jp668 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp666 = jp668
            case Value_Bool:
                jp666 = Nil{}
            case Func:
                jp666 = Nil{}
            case Nil:
                jp666 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp660 = jp666
        } else {
            jp660 = Nil{}
        }
        jp573 = jp660
        retv571 = jp573
        return retv571
    default:
        jp573 = Nil{}
        retv571 = jp573
        return retv571
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv672 Value
    var jp674 Value
    switch func__137.(type) {
    case Value_Int:
        jp674 = Nil{}
    case Value_Bool:
        jp674 = Nil{}
    case Func:
        var x201 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x201
        var t675 Value = apply_lambda(fun__140, args__138)
        jp674 = t675
    case Nil:
        jp674 = Nil{}
    default:
        panic("non-exhaustive match")
    }
    retv672 = jp674
    return retv672
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv677 Value
    var t678 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t678)
    var i__144 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop684:
    for {
        var t695 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
        var t696 *_goml_vec_string = lambda__141.params
        var t697 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t696)
        var t698 bool = t695 < t697
        var jp686 bool
        if t698 {
            var t699 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t700 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t701 bool = t699 < t700
            jp686 = t701
        } else {
            jp686 = false
        }
        if jp686 {
            var t687 *_goml_vec_string = lambda__141.params
            var t688 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var name__145 string = vec_get__Vec_6string(t687, t688)
            var t689 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t689)
            var t690 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t691 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t690, t691)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t692 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t693 int32 = t692 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__144, t693)
            continue
        } else {
            break Loop_loop684
        }
    }
    var t680 SExpr = lambda__141.body
    var t681 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t682 *ref_Vec_7Binding_x = lambda__141.global
    var t683 Value = eval(t680, t681, t682)
    retv677 = t683
    return retv677
}

func main0() struct{} {
    var t703 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t703)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t704 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t704)
    var t705 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t706 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t705, t706, global__148)
    var t707 string = value_to_string(result__151)
    println__T_string(t707)
    var t708 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t708)
    var t709 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t710 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t709, t710, global__148)
    var t711 string = value_to_string(result2__153)
    println__T_string(t711)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__4 string) int32 {
    var retv713 int32
    var t714 int32 = _goml_runtime_core_string_len(self__4)
    retv713 = t714
    return retv713
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__201 int32) *ref_int32_x {
    var retv716 *ref_int32_x
    var t717 *ref_int32_x = ref__Ref_5int32(value__201)
    retv716 = t717
    return retv716
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__201 bool) *ref_bool_x {
    var retv719 *ref_bool_x
    var t720 *ref_bool_x = ref__Ref_4bool(value__201)
    retv719 = t720
    return retv719
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__202 *ref_bool_x) bool {
    var retv722 bool
    var t723 bool = ref_get__Ref_4bool(self__202)
    retv722 = t723
    return retv722
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__202 *ref_int32_x) int32 {
    var retv725 int32
    var t726 int32 = ref_get__Ref_5int32(self__202)
    retv725 = t726
    return retv725
}

func _goml_m_inherent_i_string_i_string_i_get(self__6 string, index__7 int32) rune {
    var retv728 rune
    var t729 rune = _goml_runtime_core_string_get(self__6, index__7)
    retv728 = t729
    return retv728
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(self__203 *ref_bool_x, value__204 bool) struct{} {
    ref_set__Ref_4bool(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(self__203 *ref_int32_x, value__204 int32) struct{} {
    ref_set__Ref_5int32(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string(value__201 string) *ref_string_x {
    var retv735 *ref_string_x
    var t736 *ref_string_x = ref__Ref_6string(value__201)
    retv735 = t736
    return retv735
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__202 *ref_string_x) string {
    var retv738 string
    var t739 string = ref_get__Ref_6string(self__202)
    retv738 = t739
    return retv738
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__3 rune) string {
    var retv741 string
    var t742 string = _goml_runtime_core_char_to_string(self__3)
    retv741 = t742
    return retv741
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__203 *ref_string_x, value__204 string) struct{} {
    ref_set__Ref_6string(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv746 *_goml_vec_Token
    var t747 *_goml_vec_Token = vec_new__Vec_5Token()
    retv746 = t747
    return retv746
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__201 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv749 *ref_Vec_5Token_x
    var t750 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__201)
    retv749 = t750
    return retv749
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__202 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv752 *_goml_vec_Token
    var t753 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__202)
    retv752 = t753
    return retv752
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__122 *_goml_vec_Token, elem__123 Token) *_goml_vec_Token {
    var retv755 *_goml_vec_Token
    var result__124 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop757:
    for {
        var t758 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
        var t759 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__122)
        var t760 bool = t758 < t759
        if t760 {
            var t761 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t762 Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__122, t761)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__124, t762)
            var t763 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t764 int32 = t763 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t764)
            continue
        } else {
            break Loop_loop757
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__124, elem__123)
    retv755 = result__124
    return retv755
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__203 *ref_Vec_5Token_x, value__204 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__131 *_goml_vec_Binding) int32 {
    var retv768 int32
    var t769 int32 = vec_len__Vec_7Binding(self__131)
    retv768 = t769
    return retv768
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__201 Value) *ref_Value_x {
    var retv771 *ref_Value_x
    var t772 *ref_Value_x = ref__Ref_5Value(value__201)
    retv771 = t772
    return retv771
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__203 *ref_Value_x, value__204 Value) struct{} {
    ref_set__Ref_5Value(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__202 *ref_Value_x) Value {
    var retv776 Value
    var t777 Value = ref_get__Ref_5Value(self__202)
    retv776 = t777
    return retv776
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv779 *_goml_vec_SExpr
    var t780 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv779 = t780
    return retv779
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__201 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv782 *ref_Vec_5SExpr_x
    var t783 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__201)
    retv782 = t783
    return retv782
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__131 *_goml_vec_Token) int32 {
    var retv785 int32
    var t786 int32 = vec_len__Vec_5Token(self__131)
    retv785 = t786
    return retv785
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__202 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv788 *_goml_vec_SExpr
    var t789 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__202)
    retv788 = t789
    return retv788
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__122 *_goml_vec_SExpr, elem__123 SExpr) *_goml_vec_SExpr {
    var retv791 *_goml_vec_SExpr
    var result__124 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop793:
    for {
        var t794 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
        var t795 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__122)
        var t796 bool = t794 < t795
        if t796 {
            var t797 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t798 SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__122, t797)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__124, t798)
            var t799 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t800 int32 = t799 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t800)
            continue
        } else {
            break Loop_loop793
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__124, elem__123)
    retv791 = result__124
    return retv791
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__203 *ref_Vec_5SExpr_x, value__204 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv804 string
    var t805 string = _goml_runtime_core_int32_to_string(self__2)
    retv804 = t805
    return retv804
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv807 string
    var t808 string = _goml_runtime_core_bool_to_string(self__33)
    retv807 = t808
    return retv807
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__202 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv810 *_goml_vec_Binding
    var t811 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__202)
    retv810 = t811
    return retv810
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__131 *_goml_vec_SExpr) int32 {
    var retv813 int32
    var t814 int32 = vec_len__Vec_5SExpr(self__131)
    retv813 = t814
    return retv813
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__122 *_goml_vec_Binding, elem__123 Binding) *_goml_vec_Binding {
    var retv816 *_goml_vec_Binding
    var result__124 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop818:
    for {
        var t819 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
        var t820 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__122)
        var t821 bool = t819 < t820
        if t821 {
            var t822 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t823 Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__122, t822)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__124, t823)
            var t824 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t825 int32 = t824 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t825)
            continue
        } else {
            break Loop_loop818
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__124, elem__123)
    retv816 = result__124
    return retv816
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__203 *ref_Vec_7Binding_x, value__204 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv829 *_goml_vec_string
    var t830 *_goml_vec_string = vec_new__Vec_6string()
    retv829 = t830
    return retv829
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__201 *_goml_vec_string) *ref_Vec_6string_x {
    var retv832 *ref_Vec_6string_x
    var t833 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__201)
    retv832 = t833
    return retv832
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__202 *ref_Vec_6string_x) *_goml_vec_string {
    var retv835 *_goml_vec_string
    var t836 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__202)
    retv835 = t836
    return retv835
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__122 *_goml_vec_string, elem__123 string) *_goml_vec_string {
    var retv838 *_goml_vec_string
    var result__124 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop840:
    for {
        var t841 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
        var t842 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__122)
        var t843 bool = t841 < t842
        if t843 {
            var t844 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t845 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__122, t844)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__124, t845)
            var t846 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t847 int32 = t846 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t847)
            continue
        } else {
            break Loop_loop840
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__124, elem__123)
    retv838 = result__124
    return retv838
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__203 *ref_Vec_6string_x, value__204 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv851 *_goml_vec_Value
    var t852 *_goml_vec_Value = vec_new__Vec_5Value()
    retv851 = t852
    return retv851
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__201 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv854 *ref_Vec_5Value_x
    var t855 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__201)
    retv854 = t855
    return retv854
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__202 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv857 *_goml_vec_Value
    var t858 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__202)
    retv857 = t858
    return retv857
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__122 *_goml_vec_Value, elem__123 Value) *_goml_vec_Value {
    var retv860 *_goml_vec_Value
    var result__124 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var index__125 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop862:
    for {
        var t863 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
        var t864 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__122)
        var t865 bool = t863 < t864
        if t865 {
            var t866 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t867 Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__122, t866)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__124, t867)
            var t868 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__125)
            var t869 int32 = t868 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__125, t869)
            continue
        } else {
            break Loop_loop862
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__124, elem__123)
    retv860 = result__124
    return retv860
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__203 *ref_Vec_5Value_x, value__204 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__203, value__204)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__131 *_goml_vec_Value) int32 {
    var retv873 int32
    var t874 int32 = vec_len__Vec_5Value(self__131)
    retv873 = t874
    return retv873
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__201 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv876 *ref_Vec_7Binding_x
    var t877 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__201)
    retv876 = t877
    return retv876
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__131 *_goml_vec_string) int32 {
    var retv879 int32
    var t880 int32 = vec_len__Vec_6string(self__131)
    retv879 = t880
    return retv879
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv882 *_goml_vec_Binding
    var t883 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv882 = t883
    return retv882
}

func println__T_string(value__1 string) struct{} {
    var t885 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t885)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__120 *_goml_vec_Token, elem__121 Token) struct{} {
    vec_push__Vec_5Token(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__126 *_goml_vec_Token, index__127 int32) Token {
    var retv890 Token
    var t891 Token = vec_get__Vec_5Token(self__126, index__127)
    retv890 = t891
    return retv890
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__120 *_goml_vec_SExpr, elem__121 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__126 *_goml_vec_SExpr, index__127 int32) SExpr {
    var retv895 SExpr
    var t896 SExpr = vec_get__Vec_5SExpr(self__126, index__127)
    retv895 = t896
    return retv895
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__120 *_goml_vec_Binding, elem__121 Binding) struct{} {
    vec_push__Vec_7Binding(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__126 *_goml_vec_Binding, index__127 int32) Binding {
    var retv900 Binding
    var t901 Binding = vec_get__Vec_7Binding(self__126, index__127)
    retv900 = t901
    return retv900
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__120 *_goml_vec_string, elem__121 string) struct{} {
    vec_push__Vec_6string(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__126 *_goml_vec_string, index__127 int32) string {
    var retv905 string
    var t906 string = vec_get__Vec_6string(self__126, index__127)
    retv905 = t906
    return retv905
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__120 *_goml_vec_Value, elem__121 Value) struct{} {
    vec_push__Vec_5Value(self__120, elem__121)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__126 *_goml_vec_Value, index__127 int32) Value {
    var retv910 Value
    var t911 Value = vec_get__Vec_5Value(self__126, index__127)
    retv910 = t911
    return retv910
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv913 string
    retv913 = self__34
    return retv913
}

func main() {
    main0()
}
