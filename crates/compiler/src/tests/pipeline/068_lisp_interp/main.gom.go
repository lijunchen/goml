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
    var retv210 bool
    var t213 bool = ch__0 >= 48
    var jp212 bool
    if t213 {
        var t214 bool = ch__0 <= 57
        jp212 = t214
    } else {
        jp212 = false
    }
    retv210 = jp212
    return retv210
}

func digit_value(ch__1 rune) int32 {
    var retv216 int32
    var jp218 int32
    switch ch__1 {
    case 48:
        jp218 = 0
    case 49:
        jp218 = 1
    case 50:
        jp218 = 2
    case 51:
        jp218 = 3
    case 52:
        jp218 = 4
    case 53:
        jp218 = 5
    case 54:
        jp218 = 6
    case 55:
        jp218 = 7
    case 56:
        jp218 = 8
    case 57:
        jp218 = 9
    default:
        jp218 = 0
    }
    retv216 = jp218
    return retv216
}

func is_int_text(text__2 string) bool {
    var retv220 bool
    var len__3 int32 = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t223 bool = len__3 == 0
    var jp222 bool
    if t223 {
        jp222 = false
        retv220 = jp222
        return retv220
    } else {
        var i__4 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var saw_digit__5 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        var ok__6 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(true)
        var started__7 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
        Loop_loop229:
        for {
            var t248 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
            var jp231 bool
            if t248 {
                var t249 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var t250 bool = t249 < len__3
                jp231 = t250
            } else {
                jp231 = false
            }
            if jp231 {
                var t232 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                var ch__8 rune = _goml_m_inherent_i_string_i_string_i_get(text__2, t232)
                var t245 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__7)
                var t246 bool = !t245
                var jp235 bool
                if t246 {
                    var t247 bool = ch__8 == 45
                    jp235 = t247
                } else {
                    jp235 = false
                }
                if jp235 {
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                    var t236 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                    var t237 int32 = t236 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t237)
                } else {
                    var t240 bool = is_digit(ch__8)
                    if t240 {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__7, true)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(saw_digit__5, true)
                        var t241 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__4)
                        var t242 int32 = t241 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__4, t242)
                    } else {
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(ok__6, false)
                    }
                }
                continue
            } else {
                break Loop_loop229
            }
        }
        var t227 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(ok__6)
        var jp226 bool
        if t227 {
            var t228 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(saw_digit__5)
            jp226 = t228
        } else {
            jp226 = false
        }
        jp222 = jp226
        retv220 = jp222
        return retv220
    }
}

func parse_int32(text__9 string) int32 {
    var retv252 int32
    var len__10 int32 = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop260:
    for {
        var t261 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
        var t262 bool = t261 < len__10
        if t262 {
            var t263 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
            var ch__15 rune = _goml_m_inherent_i_string_i_string_i_get(text__9, t263)
            var t276 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(started__13)
            var t277 bool = !t276
            var jp266 bool
            if t277 {
                var t278 bool = ch__15 == 45
                jp266 = t278
            } else {
                jp266 = false
            }
            if jp266 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(negative__12, true)
                var t267 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t268 int32 = t267 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t268)
            } else {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(started__13, true)
                var d__16 int32 = digit_value(ch__15)
                var t270 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
                var t271 int32 = t270 * 10
                var t272 int32 = t271 + d__16
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__14, t272)
                var t273 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__11)
                var t274 int32 = t273 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__11, t274)
            }
            continue
        } else {
            break Loop_loop260
        }
    }
    var t256 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(negative__12)
    var jp255 int32
    if t256 {
        var t257 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        var t258 int32 = 0 - t257
        jp255 = t258
    } else {
        var t259 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__14)
        jp255 = t259
    }
    retv252 = jp255
    return retv252
}

func is_delim(ch__17 rune) bool {
    var retv280 bool
    var t286 bool = ch__17 == 40
    var jp284 bool
    if t286 {
        jp284 = true
    } else {
        var t287 bool = ch__17 == 41
        jp284 = t287
    }
    var jp282 bool
    if jp284 {
        jp282 = true
    } else {
        var t285 bool = ch__17 == 32
        jp282 = t285
    }
    retv280 = jp282
    return retv280
}

func lex_atom(source__18 string, start__19 int32) Tuple2_5Token_5int32 {
    var retv289 Tuple2_5Token_5int32
    var len__20 int32 = _goml_m_inherent_i_string_i_string_i_len(source__18)
    var text__21 *ref_string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__string("")
    var i__22 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__19)
    var done__23 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop303:
    for {
        var t316 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__23)
        var t317 bool = !t316
        var jp305 bool
        if t317 {
            var t318 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var t319 bool = t318 < len__20
            jp305 = t319
        } else {
            jp305 = false
        }
        if jp305 {
            var t306 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
            var ch__24 rune = _goml_m_inherent_i_string_i_string_i_get(source__18, t306)
            var t308 bool = is_delim(ch__24)
            if t308 {
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__23, true)
            } else {
                var t310 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
                var t311 string = _goml_m_inherent_i_char_i_char_i_to__string(ch__24)
                var t312 string = t310 + t311
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(text__21, t312)
                var t313 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
                var t314 int32 = t313 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__22, t314)
            }
            continue
        } else {
            break Loop_loop303
        }
    }
    var atom__25 string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(text__21)
    var jp292 Token
    switch atom__25 {
    case "true":
        var t295 Token = Token_Bool{
            _0: true,
        }
        jp292 = t295
    case "false":
        var t296 Token = Token_Bool{
            _0: false,
        }
        jp292 = t296
    default:
        var t299 bool = is_int_text(atom__25)
        var jp298 Token
        if t299 {
            var t300 int32 = parse_int32(atom__25)
            var t301 Token = Token_Int{
                _0: t300,
            }
            jp298 = t301
        } else {
            var t302 Token = Token_Sym{
                _0: atom__25,
            }
            jp298 = t302
        }
        jp292 = jp298
    }
    var token__26 Token = jp292
    var t293 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__22)
    var t294 Tuple2_5Token_5int32 = Tuple2_5Token_5int32{
        _0: token__26,
        _1: t293,
    }
    retv289 = t294
    return retv289
}

func lex(source__27 string) *_goml_vec_Token {
    var retv321 *_goml_vec_Token
    var len__28 int32 = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var toks__30 *ref_Vec_5Token_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(toks0__29)
    var i__31 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop324:
    for {
        var t325 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
        var t326 bool = t325 < len__28
        if t326 {
            var t327 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
            var ch__32 rune = _goml_m_inherent_i_string_i_string_i_get(source__27, t327)
            var t329 bool = ch__32 == 40
            if t329 {
                var t330 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                var t331 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t330, LParen{})
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t331)
                var t332 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                var t333 int32 = t332 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t333)
            } else {
                var t336 bool = ch__32 == 41
                if t336 {
                    var t337 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                    var t338 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t337, RParen{})
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t338)
                    var t339 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                    var t340 int32 = t339 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t340)
                } else {
                    var t343 bool = ch__32 == 32
                    if t343 {
                        var t344 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var t345 int32 = t344 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, t345)
                    } else {
                        var t347 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__31)
                        var mtmp74 Tuple2_5Token_5int32 = lex_atom(source__27, t347)
                        var x75 Token = mtmp74._0
                        var x76 int32 = mtmp74._1
                        var next__34 int32 = x76
                        var tok__33 Token = x75
                        var t348 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
                        var t349 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t348, tok__33)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(toks__30, t349)
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__31, next__34)
                    }
                }
            }
            continue
        } else {
            break Loop_loop324
        }
    }
    var t323 *_goml_vec_Token = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(toks__30)
    retv321 = t323
    return retv321
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var retv352 Value
    var t353 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(env__35)
    var t354 int32 = t353 - 1
    var i__37 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(t354)
    var result__38 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    var done__39 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop357:
    for {
        var t369 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__39)
        var t370 bool = !t369
        var jp359 bool
        if t370 {
            var t371 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var t372 bool = t371 >= 0
            jp359 = t372
        } else {
            jp359 = false
        }
        if jp359 {
            var t360 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t360)
            var t362 string = binding__40.name
            var t363 bool = t362 == name__36
            if t363 {
                var t364 Value = binding__40.value
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(result__38, t364)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__39, true)
            } else {
                var t366 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__37)
                var t367 int32 = t366 - 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__37, t367)
            }
            continue
        } else {
            break Loop_loop357
        }
    }
    var t356 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(result__38)
    retv352 = t356
    return retv352
}

func lookup(local__41 *_goml_vec_Binding, global__42 *_goml_vec_Binding, name__43 string) Value {
    var retv374 Value
    var mtmp81 Value = env_lookup(local__41, name__43)
    var jp376 Value
    switch mtmp81.(type) {
    case Value_Int:
        var other__44 Value = mtmp81
        jp376 = other__44
    case Value_Bool:
        var other__44 Value = mtmp81
        jp376 = other__44
    case Func:
        var other__44 Value = mtmp81
        jp376 = other__44
    case Nil:
        var t377 Value = env_lookup(global__42, name__43)
        jp376 = t377
    default:
        panic("non-exhaustive match")
    }
    retv374 = jp376
    return retv374
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int32) Tuple2_10Vec_5SExpr_5int32 {
    var retv379 Tuple2_10Vec_5SExpr_5int32
    var acc__47 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__48 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__47)
    var i__49 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__46)
    var done__50 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    Loop_loop384:
    for {
        var t408 bool = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(done__50)
        var t409 bool = !t408
        var jp386 bool
        if t409 {
            var t410 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var t411 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__45)
            var t412 bool = t410 < t411
            jp386 = t412
        } else {
            jp386 = false
        }
        if jp386 {
            var t387 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
            var mtmp85 Token = vec_get__Vec_5Token(tokens__45, t387)
            switch mtmp85.(type) {
            case LParen:
                var t389 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp89 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t389)
                var x90 SExpr = mtmp89._0
                var x91 int32 = mtmp89._1
                var next__52 int32 = x91
                var expr__51 SExpr = x90
                var t390 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t391 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t390, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t391)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case RParen:
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__bool(done__50, true)
                var t393 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var t394 int32 = t393 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, t394)
            case Token_Sym:
                var t396 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp94 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t396)
                var x95 SExpr = mtmp94._0
                var x96 int32 = mtmp94._1
                var next__52 int32 = x96
                var expr__51 SExpr = x95
                var t397 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t398 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t397, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t398)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case Token_Int:
                var t400 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp98 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t400)
                var x99 SExpr = mtmp98._0
                var x100 int32 = mtmp98._1
                var next__52 int32 = x100
                var expr__51 SExpr = x99
                var t401 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t402 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t401, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t402)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            case Token_Bool:
                var t404 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
                var mtmp102 Tuple2_5SExpr_5int32 = parse_expr(tokens__45, t404)
                var x103 SExpr = mtmp102._0
                var x104 int32 = mtmp102._1
                var next__52 int32 = x104
                var expr__51 SExpr = x103
                var t405 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
                var t406 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t405, expr__51)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__48, t406)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__49, next__52)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop384
        }
    }
    var t381 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__48)
    var t382 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__49)
    var t383 Tuple2_10Vec_5SExpr_5int32 = Tuple2_10Vec_5SExpr_5int32{
        _0: t381,
        _1: t382,
    }
    retv379 = t383
    return retv379
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int32) Tuple2_5SExpr_5int32 {
    var retv414 Tuple2_5SExpr_5int32
    var mtmp107 Token = vec_get__Vec_5Token(tokens__53, start__54)
    var jp416 Tuple2_5SExpr_5int32
    switch mtmp107.(type) {
    case LParen:
        var t417 int32 = start__54 + 1
        var mtmp111 Tuple2_10Vec_5SExpr_5int32 = parse_list(tokens__53, t417)
        var x112 *_goml_vec_SExpr = mtmp111._0
        var x113 int32 = mtmp111._1
        var next__56 int32 = x113
        var items__55 *_goml_vec_SExpr = x112
        var t418 SExpr = List{
            _0: items__55,
        }
        var t419 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t418,
            _1: next__56,
        }
        jp416 = t419
    case RParen:
        var t420 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t421 int32 = start__54 + 1
        var t422 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t420,
            _1: t421,
        }
        jp416 = t422
    case Token_Sym:
        var x108 string = mtmp107.(Token_Sym)._0
        var name__59 string = x108
        var t423 SExpr = SExpr_Sym{
            _0: name__59,
        }
        var t424 int32 = start__54 + 1
        var t425 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t423,
            _1: t424,
        }
        jp416 = t425
    case Token_Int:
        var x109 int32 = mtmp107.(Token_Int)._0
        var n__58 int32 = x109
        var t426 SExpr = SExpr_Int{
            _0: n__58,
        }
        var t427 int32 = start__54 + 1
        var t428 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t426,
            _1: t427,
        }
        jp416 = t428
    case Token_Bool:
        var x110 bool = mtmp107.(Token_Bool)._0
        var b__57 bool = x110
        var t429 SExpr = SExpr_Bool{
            _0: b__57,
        }
        var t430 int32 = start__54 + 1
        var t431 Tuple2_5SExpr_5int32 = Tuple2_5SExpr_5int32{
            _0: t429,
            _1: t430,
        }
        jp416 = t431
    default:
        panic("non-exhaustive match")
    }
    retv414 = jp416
    return retv414
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var retv433 *_goml_vec_SExpr
    var i__61 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__62 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var exprs__63 *ref_Vec_5SExpr_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(acc__62)
    Loop_loop436:
    for {
        var t437 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
        var t438 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(tokens__60)
        var t439 bool = t437 < t438
        if t439 {
            var t440 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__61)
            var mtmp114 Tuple2_5SExpr_5int32 = parse_expr(tokens__60, t440)
            var x115 SExpr = mtmp114._0
            var x116 int32 = mtmp114._1
            var next__65 int32 = x116
            var expr__64 SExpr = x115
            var t441 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
            var t442 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t441, expr__64)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(exprs__63, t442)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__61, next__65)
            continue
        } else {
            break Loop_loop436
        }
    }
    var t435 *_goml_vec_SExpr = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(exprs__63)
    retv433 = t435
    return retv433
}

func value_to_string(value__66 Value) string {
    var retv445 string
    var jp447 string
    switch value__66.(type) {
    case Value_Int:
        var x119 int32 = value__66.(Value_Int)._0
        var n__67 int32 = x119
        var t448 string = _goml_m_inherent_i_int32_i_int32_i_to__string(n__67)
        jp447 = t448
    case Value_Bool:
        var x120 bool = value__66.(Value_Bool)._0
        var b__68 bool = x120
        var t449 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(b__68)
        jp447 = t449
    case Func:
        jp447 = "<lambda>"
    case Nil:
        jp447 = "nil"
    default:
        panic("non-exhaustive match")
    }
    retv445 = jp447
    return retv445
}

func truthy(value__69 Value) bool {
    var retv451 bool
    var jp453 bool
    switch value__69.(type) {
    case Value_Int:
        var x122 int32 = value__69.(Value_Int)._0
        var n__71 int32 = x122
        var t454 bool = n__71 != 0
        jp453 = t454
    case Value_Bool:
        var x123 bool = value__69.(Value_Bool)._0
        var b__70 bool = x123
        jp453 = b__70
    case Func:
        jp453 = true
    case Nil:
        jp453 = false
    default:
        panic("non-exhaustive match")
    }
    retv451 = jp453
    return retv451
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    var retv456 Value
    var jp458 Value
    switch expr__72.(type) {
    case SExpr_Int:
        var x125 int32 = expr__72.(SExpr_Int)._0
        var n__75 int32 = x125
        var t459 Value = Value_Int{
            _0: n__75,
        }
        jp458 = t459
    case SExpr_Bool:
        var x126 bool = expr__72.(SExpr_Bool)._0
        var b__76 bool = x126
        var t460 Value = Value_Bool{
            _0: b__76,
        }
        jp458 = t460
    case SExpr_Sym:
        var x127 string = expr__72.(SExpr_Sym)._0
        var name__77 string = x127
        var t461 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__74)
        var t462 Value = lookup(local__73, t461, name__77)
        jp458 = t462
    case List:
        var x128 *_goml_vec_SExpr = expr__72.(List)._0
        var items__78 *_goml_vec_SExpr = x128
        var t463 Value = eval_list(items__78, local__73, global__74)
        jp458 = t463
    default:
        panic("non-exhaustive match")
    }
    retv456 = jp458
    return retv456
}

func eval_list(items__79 *_goml_vec_SExpr, local__80 *_goml_vec_Binding, global__81 *ref_Vec_7Binding_x) Value {
    var retv465 Value
    var t468 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__79)
    var t469 bool = t468 == 0
    var jp467 Value
    if t469 {
        jp467 = Nil{}
    } else {
        var head__82 SExpr = vec_get__Vec_5SExpr(items__79, 0)
        var jp471 Value
        switch head__82.(type) {
        case SExpr_Int:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t472 Value = apply(f__84, args__85, global__81)
            jp471 = t472
        case SExpr_Bool:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t473 Value = apply(f__84, args__85, global__81)
            jp471 = t473
        case SExpr_Sym:
            var x131 string = head__82.(SExpr_Sym)._0
            var name__83 string = x131
            var t474 Value = eval_list_sym(name__83, items__79, local__80, global__81)
            jp471 = t474
        case List:
            var f__84 Value = eval(head__82, local__80, global__81)
            var args__85 *_goml_vec_Value = eval_args(items__79, 1, local__80, global__81)
            var t475 Value = apply(f__84, args__85, global__81)
            jp471 = t475
        default:
            panic("non-exhaustive match")
        }
        jp467 = jp471
    }
    retv465 = jp467
    return retv465
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    var retv477 Value
    var jp479 Value
    switch name__86 {
    case "begin":
        var t480 Value = eval_begin(items__87, 1, local__88, global__89)
        jp479 = t480
    case "define":
        var t483 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t484 bool = t483 == 3
        var jp482 Value
        if t484 {
            var mtmp133 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp486 Value
            switch mtmp133.(type) {
            case SExpr_Int:
                jp486 = Nil{}
            case SExpr_Bool:
                jp486 = Nil{}
            case SExpr_Sym:
                var x136 string = mtmp133.(SExpr_Sym)._0
                var var__90 string = x136
                var t487 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t487, local__88, global__89)
                var env__92 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(global__89)
                var t488 Binding = Binding{
                    name: var__90,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t488)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(global__89, updated__93)
                jp486 = value__91
            case List:
                jp486 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp482 = jp486
        } else {
            jp482 = Nil{}
        }
        jp479 = jp482
    case "if":
        var t491 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t492 bool = t491 == 4
        var jp490 Value
        if t492 {
            var t493 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t493, local__88, global__89)
            var t496 bool = truthy(cond__94)
            var jp495 Value
            if t496 {
                var t497 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t498 Value = eval(t497, local__88, global__89)
                jp495 = t498
            } else {
                var t499 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t500 Value = eval(t499, local__88, global__89)
                jp495 = t500
            }
            jp490 = jp495
        } else {
            jp490 = Nil{}
        }
        jp479 = jp490
    case "lambda":
        var t503 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__87)
        var t504 bool = t503 == 3
        var jp502 Value
        if t504 {
            var mtmp139 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var jp506 Value
            switch mtmp139.(type) {
            case SExpr_Int:
                jp506 = Nil{}
            case SExpr_Bool:
                jp506 = Nil{}
            case SExpr_Sym:
                jp506 = Nil{}
            case List:
                var x143 *_goml_vec_SExpr = mtmp139.(List)._0
                var params_exprs__95 *_goml_vec_SExpr = x143
                var params__96 *_goml_vec_string = params_from_sexprs(params_exprs__95)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t507 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t508 Value = Func{
                    _0: t507,
                }
                jp506 = t508
            default:
                panic("non-exhaustive match")
            }
            jp502 = jp506
        } else {
            jp502 = Nil{}
        }
        jp479 = jp502
    case "+":
        var t509 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t510 Value = apply_builtin("+", t509)
        jp479 = t510
    case "-":
        var t511 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t512 Value = apply_builtin("-", t511)
        jp479 = t512
    case "*":
        var t513 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t514 Value = apply_builtin("*", t513)
        jp479 = t514
    case "/":
        var t515 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t516 Value = apply_builtin("/", t515)
        jp479 = t516
    case "=":
        var t517 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t518 Value = apply_builtin("=", t517)
        jp479 = t518
    default:
        var t519 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t519, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t520 Value = apply(f__98, args__99, global__89)
        jp479 = t520
    }
    retv477 = jp479
    return retv477
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int32, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var retv522 Value
    var i__104 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__101)
    var last__105 *ref_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(Nil{})
    Loop_loop525:
    for {
        var t526 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
        var t527 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__100)
        var t528 bool = t526 < t527
        if t528 {
            var t529 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t530 SExpr = vec_get__Vec_5SExpr(items__100, t529)
            var v__106 Value = eval(t530, local__102, global__103)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(last__105, v__106)
            var t531 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__104)
            var t532 int32 = t531 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__104, t532)
            continue
        } else {
            break Loop_loop525
        }
    }
    var t524 Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(last__105)
    retv522 = t524
    return retv522
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var retv535 *_goml_vec_string
    var i__108 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    var acc__109 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var params__110 *ref_Vec_6string_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(acc__109)
    Loop_loop538:
    for {
        var t539 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
        var t540 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__107)
        var t541 bool = t539 < t540
        if t541 {
            var t542 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
            var mtmp146 SExpr = vec_get__Vec_5SExpr(items__107, t542)
            switch mtmp146.(type) {
            case SExpr_Int:
                var t544 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t545 int32 = t544 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t545)
            case SExpr_Bool:
                var t547 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t548 int32 = t547 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t548)
            case SExpr_Sym:
                var x149 string = mtmp146.(SExpr_Sym)._0
                var name__111 string = x149
                var t550 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
                var t551 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t550, name__111)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(params__110, t551)
                var t552 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t553 int32 = t552 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t553)
            case List:
                var t555 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__108)
                var t556 int32 = t555 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__108, t556)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop538
        }
    }
    var t537 *_goml_vec_string = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(params__110)
    retv535 = t537
    return retv535
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int32, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var retv559 *_goml_vec_Value
    var i__116 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(start__113)
    var acc__117 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var args__118 *ref_Vec_5Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(acc__117)
    Loop_loop562:
    for {
        var t563 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
        var t564 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(items__112)
        var t565 bool = t563 < t564
        if t565 {
            var t566 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t567 SExpr = vec_get__Vec_5SExpr(items__112, t566)
            var v__119 Value = eval(t567, local__114, global__115)
            var t568 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
            var t569 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t568, v__119)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(args__118, t569)
            var t570 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__116)
            var t571 int32 = t570 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__116, t571)
            continue
        } else {
            break Loop_loop562
        }
    }
    var t561 *_goml_vec_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(args__118)
    retv559 = t561
    return retv559
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    var retv574 Value
    var jp576 Value
    switch name__120 {
    case "=":
        var t579 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t580 bool = t579 == 2
        var jp578 Value
        if t580 {
            var t581 Value = vec_get__Vec_5Value(args__121, 0)
            var t582 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp155 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t581,
                _1: t582,
            }
            var x156 Value = mtmp155._0
            var x157 Value = mtmp155._1
            var jp584 Value
            switch x157.(type) {
            case Value_Int:
                var x158 int32 = x157.(Value_Int)._0
                var jp586 Value
                switch x156.(type) {
                case Value_Int:
                    var x161 int32 = x156.(Value_Int)._0
                    var a__122 int32 = x161
                    var b__123 int32 = x158
                    var t587 bool = a__122 == b__123
                    var t588 Value = Value_Bool{
                        _0: t587,
                    }
                    jp586 = t588
                case Value_Bool:
                    var t589 Value = Value_Bool{
                        _0: false,
                    }
                    jp586 = t589
                case Func:
                    var t590 Value = Value_Bool{
                        _0: false,
                    }
                    jp586 = t590
                case Nil:
                    var t591 Value = Value_Bool{
                        _0: false,
                    }
                    jp586 = t591
                default:
                    panic("non-exhaustive match")
                }
                jp584 = jp586
            case Value_Bool:
                var x159 bool = x157.(Value_Bool)._0
                var jp593 Value
                switch x156.(type) {
                case Value_Int:
                    var t594 Value = Value_Bool{
                        _0: false,
                    }
                    jp593 = t594
                case Value_Bool:
                    var x165 bool = x156.(Value_Bool)._0
                    var a__124 bool = x165
                    var b__125 bool = x159
                    var t595 bool = a__124 == b__125
                    var t596 Value = Value_Bool{
                        _0: t595,
                    }
                    jp593 = t596
                case Func:
                    var t597 Value = Value_Bool{
                        _0: false,
                    }
                    jp593 = t597
                case Nil:
                    var t598 Value = Value_Bool{
                        _0: false,
                    }
                    jp593 = t598
                default:
                    panic("non-exhaustive match")
                }
                jp584 = jp593
            case Func:
                var t599 Value = Value_Bool{
                    _0: false,
                }
                jp584 = t599
            case Nil:
                var t600 Value = Value_Bool{
                    _0: false,
                }
                jp584 = t600
            default:
                panic("non-exhaustive match")
            }
            jp578 = jp584
        } else {
            var t601 Value = Value_Bool{
                _0: false,
            }
            jp578 = t601
        }
        jp576 = jp578
        retv574 = jp576
        return retv574
    case "+":
        var i__126 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__127 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        Loop_loop605:
        for {
            var t606 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
            var t607 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t608 bool = t606 < t607
            if t608 {
                var t609 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                var mtmp167 Value = vec_get__Vec_5Value(args__121, t609)
                switch mtmp167.(type) {
                case Value_Int:
                    var x168 int32 = mtmp167.(Value_Int)._0
                    var n__128 int32 = x168
                    var t611 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
                    var t612 int32 = t611 + n__128
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__127, t612)
                    var t613 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t614 int32 = t613 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t614)
                case Value_Bool:
                    var t616 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t617 int32 = t616 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t617)
                case Func:
                    var t619 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t620 int32 = t619 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t620)
                case Nil:
                    var t622 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__126)
                    var t623 int32 = t622 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__126, t623)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop605
            }
        }
        var t603 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__127)
        var t604 Value = Value_Int{
            _0: t603,
        }
        jp576 = t604
        retv574 = jp576
        return retv574
    case "*":
        var i__129 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop628:
        for {
            var t629 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
            var t630 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
            var t631 bool = t629 < t630
            if t631 {
                var t632 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                var mtmp173 Value = vec_get__Vec_5Value(args__121, t632)
                switch mtmp173.(type) {
                case Value_Int:
                    var x174 int32 = mtmp173.(Value_Int)._0
                    var n__131 int32 = x174
                    var t634 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
                    var t635 int32 = t634 * n__131
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(acc__130, t635)
                    var t636 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t637 int32 = t636 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t637)
                case Value_Bool:
                    var t639 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t640 int32 = t639 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t640)
                case Func:
                    var t642 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t643 int32 = t642 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t643)
                case Nil:
                    var t645 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__129)
                    var t646 int32 = t645 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__129, t646)
                default:
                    panic("non-exhaustive match")
                }
                continue
            } else {
                break Loop_loop628
            }
        }
        var t626 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t627 Value = Value_Int{
            _0: t626,
        }
        jp576 = t627
        retv574 = jp576
        return retv574
    case "-":
        var mtmp179 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var jp649 Value
        switch mtmp179 {
        case 1:
            var mtmp180 Value = vec_get__Vec_5Value(args__121, 0)
            var jp651 Value
            switch mtmp180.(type) {
            case Value_Int:
                var x181 int32 = mtmp180.(Value_Int)._0
                var n__132 int32 = x181
                var t652 int32 = 0 - n__132
                var t653 Value = Value_Int{
                    _0: t652,
                }
                jp651 = t653
            case Value_Bool:
                jp651 = Nil{}
            case Func:
                jp651 = Nil{}
            case Nil:
                jp651 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp649 = jp651
        case 2:
            var t654 Value = vec_get__Vec_5Value(args__121, 0)
            var t655 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp184 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t654,
                _1: t655,
            }
            var x185 Value = mtmp184._0
            var x186 Value = mtmp184._1
            var jp657 Value
            switch x186.(type) {
            case Value_Int:
                var x187 int32 = x186.(Value_Int)._0
                var jp659 Value
                switch x185.(type) {
                case Value_Int:
                    var x190 int32 = x185.(Value_Int)._0
                    var a__133 int32 = x190
                    var b__134 int32 = x187
                    var t660 int32 = a__133 - b__134
                    var t661 Value = Value_Int{
                        _0: t660,
                    }
                    jp659 = t661
                case Value_Bool:
                    jp659 = Nil{}
                case Func:
                    jp659 = Nil{}
                case Nil:
                    jp659 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp657 = jp659
            case Value_Bool:
                jp657 = Nil{}
            case Func:
                jp657 = Nil{}
            case Nil:
                jp657 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp649 = jp657
        default:
            jp649 = Nil{}
        }
        jp576 = jp649
        retv574 = jp576
        return retv574
    case "/":
        var t664 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t665 bool = t664 == 2
        var jp663 Value
        if t665 {
            var t666 Value = vec_get__Vec_5Value(args__121, 0)
            var t667 Value = vec_get__Vec_5Value(args__121, 1)
            var mtmp193 Tuple2_5Value_5Value = Tuple2_5Value_5Value{
                _0: t666,
                _1: t667,
            }
            var x194 Value = mtmp193._0
            var x195 Value = mtmp193._1
            var jp669 Value
            switch x195.(type) {
            case Value_Int:
                var x196 int32 = x195.(Value_Int)._0
                var jp671 Value
                switch x194.(type) {
                case Value_Int:
                    var x199 int32 = x194.(Value_Int)._0
                    var a__135 int32 = x199
                    var b__136 int32 = x196
                    var t672 int32 = a__135 / b__136
                    var t673 Value = Value_Int{
                        _0: t672,
                    }
                    jp671 = t673
                case Value_Bool:
                    jp671 = Nil{}
                case Func:
                    jp671 = Nil{}
                case Nil:
                    jp671 = Nil{}
                default:
                    panic("non-exhaustive match")
                }
                jp669 = jp671
            case Value_Bool:
                jp669 = Nil{}
            case Func:
                jp669 = Nil{}
            case Nil:
                jp669 = Nil{}
            default:
                panic("non-exhaustive match")
            }
            jp663 = jp669
        } else {
            jp663 = Nil{}
        }
        jp576 = jp663
        retv574 = jp576
        return retv574
    default:
        jp576 = Nil{}
        retv574 = jp576
        return retv574
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    var retv675 Value
    var jp677 Value
    switch func__137.(type) {
    case Value_Int:
        jp677 = Nil{}
    case Value_Bool:
        jp677 = Nil{}
    case Func:
        var x204 Lambda = func__137.(Func)._0
        var fun__140 Lambda = x204
        var t678 Value = apply_lambda(fun__140, args__138)
        jp677 = t678
    case Nil:
        jp677 = Nil{}
    default:
        panic("non-exhaustive match")
    }
    retv675 = jp677
    return retv675
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var retv680 Value
    var t681 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t681)
    var i__144 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop687:
    for {
        var t698 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
        var t699 *_goml_vec_string = lambda__141.params
        var t700 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t699)
        var t701 bool = t698 < t700
        var jp689 bool
        if t701 {
            var t702 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t703 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__142)
            var t704 bool = t702 < t703
            jp689 = t704
        } else {
            jp689 = false
        }
        if jp689 {
            var t690 *_goml_vec_string = lambda__141.params
            var t691 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var name__145 string = vec_get__Vec_6string(t690, t691)
            var t692 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var value__146 Value = vec_get__Vec_5Value(args__142, t692)
            var t693 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
            var t694 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t693, t694)
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(env__143, updated__147)
            var t695 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(i__144)
            var t696 int32 = t695 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(i__144, t696)
            continue
        } else {
            break Loop_loop687
        }
    }
    var t683 SExpr = lambda__141.body
    var t684 *_goml_vec_Binding = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(env__143)
    var t685 *ref_Vec_7Binding_x = lambda__141.global
    var t686 Value = eval(t683, t684, t685)
    retv680 = t686
    return retv680
}

func main0() struct{} {
    var t706 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var global__148 *ref_Vec_7Binding_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(t706)
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t707 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t707)
    var t708 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t709 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result__151 Value = eval(t708, t709, global__148)
    var t710 string = value_to_string(result__151)
    println__T_string(t710)
    var t711 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t711)
    var t712 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t713 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var result2__153 Value = eval(t712, t713, global__148)
    var t714 string = value_to_string(result2__153)
    println__T_string(t714)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__7 string) int32 {
    var retv716 int32
    var t717 int32 = _goml_runtime_core_string_len(self__7)
    retv716 = t717
    return retv716
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__204 int32) *ref_int32_x {
    var retv719 *ref_int32_x
    var t720 *ref_int32_x = ref__Ref_5int32(value__204)
    retv719 = t720
    return retv719
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__204 bool) *ref_bool_x {
    var retv722 *ref_bool_x
    var t723 *ref_bool_x = ref__Ref_4bool(value__204)
    retv722 = t723
    return retv722
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__bool(self__205 *ref_bool_x) bool {
    var retv725 bool
    var t726 bool = ref_get__Ref_4bool(self__205)
    retv725 = t726
    return retv725
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__205 *ref_int32_x) int32 {
    var retv728 int32
    var t729 int32 = ref_get__Ref_5int32(self__205)
    retv728 = t729
    return retv728
}

func _goml_m_inherent_i_string_i_string_i_get(self__9 string, index__10 int32) rune {
    var retv731 rune
    var t732 rune = _goml_runtime_core_string_get(self__9, index__10)
    retv731 = t732
    return retv731
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
    var retv738 *ref_string_x
    var t739 *ref_string_x = ref__Ref_6string(value__204)
    retv738 = t739
    return retv738
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__string(self__205 *ref_string_x) string {
    var retv741 string
    var t742 string = ref_get__Ref_6string(self__205)
    retv741 = t742
    return retv741
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__6 rune) string {
    var retv744 string
    var t745 string = _goml_runtime_core_char_to_string(self__6)
    retv744 = t745
    return retv744
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__string(self__206 *ref_string_x, value__207 string) struct{} {
    ref_set__Ref_6string(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token() *_goml_vec_Token {
    var retv749 *_goml_vec_Token
    var t750 *_goml_vec_Token = vec_new__Vec_5Token()
    retv749 = t750
    return retv749
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Token_r_(value__204 *_goml_vec_Token) *ref_Vec_5Token_x {
    var retv752 *ref_Vec_5Token_x
    var t753 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(value__204)
    retv752 = t753
    return retv752
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Token_r_(self__205 *ref_Vec_5Token_x) *_goml_vec_Token {
    var retv755 *_goml_vec_Token
    var t756 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(self__205)
    retv755 = t756
    return retv755
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__125 *_goml_vec_Token, elem__126 Token) *_goml_vec_Token {
    var retv758 *_goml_vec_Token
    var result__127 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Token()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop760:
    for {
        var t761 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t762 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__125)
        var t763 bool = t761 < t762
        if t763 {
            var t764 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t765 Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__125, t764)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__127, t765)
            var t766 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t767 int32 = t766 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t767)
            continue
        } else {
            break Loop_loop760
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(result__127, elem__126)
    retv758 = result__127
    return retv758
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Token_r_(self__206 *ref_Vec_5Token_x, value__207 *_goml_vec_Token) struct{} {
    ref_set__Ref_10Vec_5Token(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__134 *_goml_vec_Binding) int32 {
    var retv771 int32
    var t772 int32 = vec_len__Vec_7Binding(self__134)
    retv771 = t772
    return retv771
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Value(value__204 Value) *ref_Value_x {
    var retv774 *ref_Value_x
    var t775 *ref_Value_x = ref__Ref_5Value(value__204)
    retv774 = t775
    return retv774
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Value(self__206 *ref_Value_x, value__207 Value) struct{} {
    ref_set__Ref_5Value(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Value(self__205 *ref_Value_x) Value {
    var retv779 Value
    var t780 Value = ref_get__Ref_5Value(self__205)
    retv779 = t780
    return retv779
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr() *_goml_vec_SExpr {
    var retv782 *_goml_vec_SExpr
    var t783 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    retv782 = t783
    return retv782
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_SExpr_r_(value__204 *_goml_vec_SExpr) *ref_Vec_5SExpr_x {
    var retv785 *ref_Vec_5SExpr_x
    var t786 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(value__204)
    retv785 = t786
    return retv785
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Token(self__134 *_goml_vec_Token) int32 {
    var retv788 int32
    var t789 int32 = vec_len__Vec_5Token(self__134)
    retv788 = t789
    return retv788
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_SExpr_r_(self__205 *ref_Vec_5SExpr_x) *_goml_vec_SExpr {
    var retv791 *_goml_vec_SExpr
    var t792 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(self__205)
    retv791 = t792
    return retv791
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__125 *_goml_vec_SExpr, elem__126 SExpr) *_goml_vec_SExpr {
    var retv794 *_goml_vec_SExpr
    var result__127 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__SExpr()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop796:
    for {
        var t797 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t798 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__125)
        var t799 bool = t797 < t798
        if t799 {
            var t800 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t801 SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__125, t800)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__127, t801)
            var t802 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t803 int32 = t802 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t803)
            continue
        } else {
            break Loop_loop796
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(result__127, elem__126)
    retv794 = result__127
    return retv794
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_SExpr_r_(self__206 *ref_Vec_5SExpr_x, value__207 *_goml_vec_SExpr) struct{} {
    ref_set__Ref_10Vec_5SExpr(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv807 string
    var t808 string = _goml_runtime_core_int32_to_string(self__5)
    retv807 = t808
    return retv807
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv810 string
    var t811 string = _goml_runtime_core_bool_to_string(self__36)
    retv810 = t811
    return retv810
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Binding_r_(self__205 *ref_Vec_7Binding_x) *_goml_vec_Binding {
    var retv813 *_goml_vec_Binding
    var t814 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(self__205)
    retv813 = t814
    return retv813
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__134 *_goml_vec_SExpr) int32 {
    var retv816 int32
    var t817 int32 = vec_len__Vec_5SExpr(self__134)
    retv816 = t817
    return retv816
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__125 *_goml_vec_Binding, elem__126 Binding) *_goml_vec_Binding {
    var retv819 *_goml_vec_Binding
    var result__127 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop821:
    for {
        var t822 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t823 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Binding(self__125)
        var t824 bool = t822 < t823
        if t824 {
            var t825 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t826 Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__125, t825)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__127, t826)
            var t827 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t828 int32 = t827 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t828)
            continue
        } else {
            break Loop_loop821
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(result__127, elem__126)
    retv819 = result__127
    return retv819
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Binding_r_(self__206 *ref_Vec_7Binding_x, value__207 *_goml_vec_Binding) struct{} {
    ref_set__Ref_12Vec_7Binding(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv832 *_goml_vec_string
    var t833 *_goml_vec_string = vec_new__Vec_6string()
    retv832 = t833
    return retv832
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_string_r_(value__204 *_goml_vec_string) *ref_Vec_6string_x {
    var retv835 *ref_Vec_6string_x
    var t836 *ref_Vec_6string_x = ref__Ref_11Vec_6string(value__204)
    retv835 = t836
    return retv835
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_string_r_(self__205 *ref_Vec_6string_x) *_goml_vec_string {
    var retv838 *_goml_vec_string
    var t839 *_goml_vec_string = ref_get__Ref_11Vec_6string(self__205)
    retv838 = t839
    return retv838
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__125 *_goml_vec_string, elem__126 string) *_goml_vec_string {
    var retv841 *_goml_vec_string
    var result__127 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop843:
    for {
        var t844 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t845 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__125)
        var t846 bool = t844 < t845
        if t846 {
            var t847 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t848 string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__125, t847)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__127, t848)
            var t849 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t850 int32 = t849 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t850)
            continue
        } else {
            break Loop_loop843
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(result__127, elem__126)
    retv841 = result__127
    return retv841
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_string_r_(self__206 *ref_Vec_6string_x, value__207 *_goml_vec_string) struct{} {
    ref_set__Ref_11Vec_6string(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value() *_goml_vec_Value {
    var retv854 *_goml_vec_Value
    var t855 *_goml_vec_Value = vec_new__Vec_5Value()
    retv854 = t855
    return retv854
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Value_r_(value__204 *_goml_vec_Value) *ref_Vec_5Value_x {
    var retv857 *ref_Vec_5Value_x
    var t858 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(value__204)
    retv857 = t858
    return retv857
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Vec_l_Value_r_(self__205 *ref_Vec_5Value_x) *_goml_vec_Value {
    var retv860 *_goml_vec_Value
    var t861 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(self__205)
    retv860 = t861
    return retv860
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__125 *_goml_vec_Value, elem__126 Value) *_goml_vec_Value {
    var retv863 *_goml_vec_Value
    var result__127 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Value()
    var index__128 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(0)
    Loop_loop865:
    for {
        var t866 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
        var t867 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__125)
        var t868 bool = t866 < t867
        if t868 {
            var t869 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t870 Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__125, t869)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__127, t870)
            var t871 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(index__128)
            var t872 int32 = t871 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int32(index__128, t872)
            continue
        } else {
            break Loop_loop865
        }
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(result__127, elem__126)
    retv863 = result__127
    return retv863
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Vec_l_Value_r_(self__206 *ref_Vec_5Value_x, value__207 *_goml_vec_Value) struct{} {
    ref_set__Ref_10Vec_5Value(self__206, value__207)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__134 *_goml_vec_Value) int32 {
    var retv876 int32
    var t877 int32 = vec_len__Vec_5Value(self__134)
    retv876 = t877
    return retv876
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Vec_l_Binding_r_(value__204 *_goml_vec_Binding) *ref_Vec_7Binding_x {
    var retv879 *ref_Vec_7Binding_x
    var t880 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(value__204)
    retv879 = t880
    return retv879
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__134 *_goml_vec_string) int32 {
    var retv882 int32
    var t883 int32 = vec_len__Vec_6string(self__134)
    retv882 = t883
    return retv882
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Binding() *_goml_vec_Binding {
    var retv885 *_goml_vec_Binding
    var t886 *_goml_vec_Binding = vec_new__Vec_7Binding()
    retv885 = t886
    return retv885
}

func println__T_string(value__1 string) struct{} {
    var t888 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t888)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Token(self__123 *_goml_vec_Token, elem__124 Token) struct{} {
    vec_push__Vec_5Token(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Token(self__129 *_goml_vec_Token, index__130 int32) Token {
    var retv893 Token
    var t894 Token = vec_get__Vec_5Token(self__129, index__130)
    retv893 = t894
    return retv893
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__SExpr(self__123 *_goml_vec_SExpr, elem__124 SExpr) struct{} {
    vec_push__Vec_5SExpr(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__SExpr(self__129 *_goml_vec_SExpr, index__130 int32) SExpr {
    var retv898 SExpr
    var t899 SExpr = vec_get__Vec_5SExpr(self__129, index__130)
    retv898 = t899
    return retv898
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Binding(self__123 *_goml_vec_Binding, elem__124 Binding) struct{} {
    vec_push__Vec_7Binding(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Binding(self__129 *_goml_vec_Binding, index__130 int32) Binding {
    var retv903 Binding
    var t904 Binding = vec_get__Vec_7Binding(self__129, index__130)
    retv903 = t904
    return retv903
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__123 *_goml_vec_string, elem__124 string) struct{} {
    vec_push__Vec_6string(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__string(self__129 *_goml_vec_string, index__130 int32) string {
    var retv908 string
    var t909 string = vec_get__Vec_6string(self__129, index__130)
    retv908 = t909
    return retv908
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Value(self__123 *_goml_vec_Value, elem__124 Value) struct{} {
    vec_push__Vec_5Value(self__123, elem__124)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__Value(self__129 *_goml_vec_Value, index__130 int32) Value {
    var retv913 Value
    var t914 Value = vec_get__Vec_5Value(self__129, index__130)
    retv913 = t914
    return retv913
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv916 string
    retv916 = self__37
    return retv916
}

func main() {
    main0()
}
