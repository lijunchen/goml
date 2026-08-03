package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_get(s string, i int) rune {
    return rune(s[i])
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_char_to_string(x rune) string {
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
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

func vec_with_capacity__Vec_5Token(capacity int) *_goml_vec_Token {
    return &_goml_vec_Token{
        items: _goml_slices.Grow([]Token{}, int(capacity)),
    }
}

func vec_push__Vec_5Token(vec *_goml_vec_Token, elem Token) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5Token(vec *_goml_vec_Token, index int) Token {
    return vec.items[index]
}

func vec_len__Vec_5Token(vec *_goml_vec_Token) int {
    return int(len(vec.items))
}

type _goml_vec_Binding struct {
    items []Binding
}

func vec_new__Vec_7Binding() *_goml_vec_Binding {
    return &_goml_vec_Binding{
        items: nil,
    }
}

func vec_with_capacity__Vec_7Binding(capacity int) *_goml_vec_Binding {
    return &_goml_vec_Binding{
        items: _goml_slices.Grow([]Binding{}, int(capacity)),
    }
}

func vec_push__Vec_7Binding(vec *_goml_vec_Binding, elem Binding) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_7Binding(vec *_goml_vec_Binding, index int) Binding {
    return vec.items[index]
}

func vec_len__Vec_7Binding(vec *_goml_vec_Binding) int {
    return int(len(vec.items))
}

type _goml_vec_SExpr struct {
    items []SExpr
}

func vec_new__Vec_5SExpr() *_goml_vec_SExpr {
    return &_goml_vec_SExpr{
        items: nil,
    }
}

func vec_with_capacity__Vec_5SExpr(capacity int) *_goml_vec_SExpr {
    return &_goml_vec_SExpr{
        items: _goml_slices.Grow([]SExpr{}, int(capacity)),
    }
}

func vec_push__Vec_5SExpr(vec *_goml_vec_SExpr, elem SExpr) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5SExpr(vec *_goml_vec_SExpr, index int) SExpr {
    return vec.items[index]
}

func vec_len__Vec_5SExpr(vec *_goml_vec_SExpr) int {
    return int(len(vec.items))
}

type _goml_vec_Value struct {
    items []Value
}

func vec_new__Vec_5Value() *_goml_vec_Value {
    return &_goml_vec_Value{
        items: nil,
    }
}

func vec_with_capacity__Vec_5Value(capacity int) *_goml_vec_Value {
    return &_goml_vec_Value{
        items: _goml_slices.Grow([]Value{}, int(capacity)),
    }
}

func vec_push__Vec_5Value(vec *_goml_vec_Value, elem Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5Value(vec *_goml_vec_Value, index int) Value {
    return vec.items[index]
}

func vec_len__Vec_5Value(vec *_goml_vec_Value) int {
    return int(len(vec.items))
}

type _goml_vec_string struct {
    items []string
}

func vec_new__Vec_6string() *_goml_vec_string {
    return &_goml_vec_string{
        items: nil,
    }
}

func vec_with_capacity__Vec_6string(capacity int) *_goml_vec_string {
    return &_goml_vec_string{
        items: _goml_slices.Grow([]string{}, int(capacity)),
    }
}

func vec_push__Vec_6string(vec *_goml_vec_string, elem string) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_6string(vec *_goml_vec_string, index int) string {
    return vec.items[index]
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
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

type Tuple2_5Token_3int struct {
    _0 Token
    _1 int
}

type Tuple2_10Vec_5SExpr_3int struct {
    _0 *_goml_vec_SExpr
    _1 int
}

type Tuple2_5SExpr_3int struct {
    _0 SExpr
    _1 int
}

type Tuple2_5Value_5Value struct {
    _0 Value
    _1 Value
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
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

type Option__char interface {
    isOption__char()
}

type None struct {}

func (_ None) isOption__char() {}

type Some struct {
    _0 rune
}

func (_ Some) isOption__char() {}

func is_int_text(text__2 string) bool {
    var len__3 int = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t327 bool
    var inline1246 int = 0
    var inline1247 bool = len__3 == inline1246
    t327 = inline1247
    if t327 {
        return false
    } else {
        var i__4 *ref_int_x
        var inline1243 int = 0
        var inline1244 *ref_int_x = ref__Ref_3int(inline1243)
        i__4 = inline1244
        var saw_digit__5 *ref_bool_x
        var inline1240 bool = false
        var inline1241 *ref_bool_x = ref__Ref_4bool(inline1240)
        saw_digit__5 = inline1241
        var ok__6 *ref_bool_x
        var inline1237 bool = true
        var inline1238 *ref_bool_x = ref__Ref_4bool(inline1237)
        ok__6 = inline1238
        var started__7 *ref_bool_x
        var inline1234 bool = false
        var inline1235 *ref_bool_x = ref__Ref_4bool(inline1234)
        started__7 = inline1235
        Loop_loop333:
        for {
            var t352 bool
            var inline1228 bool = ref_get__Ref_4bool(ok__6)
            t352 = inline1228
            var jp335 bool
            if t352 {
                var t353 int
                var inline1194 int = ref_get__Ref_3int(i__4)
                t353 = inline1194
                var t354 bool = t353 < len__3
                jp335 = t354
            } else {
                jp335 = false
            }
            if jp335 {
                var t336 int
                var inline1226 int = ref_get__Ref_3int(i__4)
                t336 = inline1226
                var ch__8 rune
                var inline1224 rune = string_get(text__2, t336)
                ch__8 = inline1224
                var t349 bool
                var inline1222 bool = ref_get__Ref_4bool(started__7)
                t349 = inline1222
                var t350 bool = !t349
                var jp339 bool
                if t350 {
                    var inline1196 rune = 45
                    var inline1197 bool = ch__8 == inline1196
                    jp339 = inline1197
                } else {
                    jp339 = false
                }
                if jp339 {
                    var inline1203 bool = true
                    ref_set__Ref_4bool(started__7, inline1203)
                    var t340 int
                    var inline1201 int = ref_get__Ref_3int(i__4)
                    t340 = inline1201
                    var t341 int = t340 + 1
                    ref_set__Ref_3int(i__4, t341)
                    continue
                } else {
                    var t344 bool
                    var inline1219 bool = ch__8 >= 48
                    if inline1219 {
                        var inline1220 bool = ch__8 <= 57
                        t344 = inline1220
                    } else {
                        t344 = false
                    }
                    if t344 {
                        var inline1213 bool = true
                        ref_set__Ref_4bool(started__7, inline1213)
                        var inline1210 bool = true
                        ref_set__Ref_4bool(saw_digit__5, inline1210)
                        var t345 int
                        var inline1208 int = ref_get__Ref_3int(i__4)
                        t345 = inline1208
                        var t346 int = t345 + 1
                        ref_set__Ref_3int(i__4, t346)
                        continue
                    } else {
                        var inline1216 bool = false
                        ref_set__Ref_4bool(ok__6, inline1216)
                        continue
                    }
                }
            } else {
                break Loop_loop333
            }
        }
        var t331 bool
        var inline1232 bool = ref_get__Ref_4bool(ok__6)
        t331 = inline1232
        if t331 {
            var inline1230 bool = ref_get__Ref_4bool(saw_digit__5)
            return inline1230
        } else {
            return false
        }
    }
}

func parse_int32(text__9 string) int32 {
    var len__10 int = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var acc__14 *ref_int32_x
    var inline1288 int32 = 0
    var inline1289 *ref_int32_x = ref__Ref_5int32(inline1288)
    acc__14 = inline1289
    Loop_loop364:
    for {
        var t365 int
        var inline1280 int = ref_get__Ref_3int(i__11)
        t365 = inline1280
        var t366 bool = t365 < len__10
        if t366 {
            var t367 int
            var inline1278 int = ref_get__Ref_3int(i__11)
            t367 = inline1278
            var ch__15 rune
            var inline1276 rune = string_get(text__9, t367)
            ch__15 = inline1276
            var t380 bool
            var inline1274 bool = ref_get__Ref_4bool(started__13)
            t380 = inline1274
            var t381 bool = !t380
            var jp370 bool
            if t381 {
                var inline1249 rune = 45
                var inline1250 bool = ch__15 == inline1249
                jp370 = inline1250
            } else {
                jp370 = false
            }
            if jp370 {
                var inline1259 bool = true
                ref_set__Ref_4bool(started__13, inline1259)
                var inline1256 bool = true
                ref_set__Ref_4bool(negative__12, inline1256)
                var t371 int
                var inline1254 int = ref_get__Ref_3int(i__11)
                t371 = inline1254
                var t372 int = t371 + 1
                ref_set__Ref_3int(i__11, t372)
                continue
            } else {
                var inline1271 bool = true
                ref_set__Ref_4bool(started__13, inline1271)
                var d__16 int32
                switch ch__15 {
                case 48:
                    d__16 = 0
                case 49:
                    d__16 = 1
                case 50:
                    d__16 = 2
                case 51:
                    d__16 = 3
                case 52:
                    d__16 = 4
                case 53:
                    d__16 = 5
                case 54:
                    d__16 = 6
                case 55:
                    d__16 = 7
                case 56:
                    d__16 = 8
                case 57:
                    d__16 = 9
                default:
                    d__16 = 0
                }
                var t374 int32
                var inline1268 int32 = ref_get__Ref_5int32(acc__14)
                t374 = inline1268
                var t375 int32 = t374 * 10
                var t376 int32 = t375 + d__16
                ref_set__Ref_5int32(acc__14, t376)
                var t377 int
                var inline1264 int = ref_get__Ref_3int(i__11)
                t377 = inline1264
                var t378 int = t377 + 1
                ref_set__Ref_3int(i__11, t378)
                continue
            }
        } else {
            break Loop_loop364
        }
    }
    var t360 bool
    var inline1286 bool = ref_get__Ref_4bool(negative__12)
    t360 = inline1286
    if t360 {
        var t361 int32
        var inline1282 int32 = ref_get__Ref_5int32(acc__14)
        t361 = inline1282
        var t362 int32 = 0 - t361
        return t362
    } else {
        var inline1284 int32 = ref_get__Ref_5int32(acc__14)
        return inline1284
    }
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var len__20 int
    var inline1339 int = _goml_runtime_core_string_len(source__18)
    len__20 = inline1339
    var text__21 *ref_string_x
    var inline1336 string = ""
    var inline1337 *ref_string_x = ref__Ref_6string(inline1336)
    text__21 = inline1337
    var i__22 *ref_int_x
    var inline1334 *ref_int_x = ref__Ref_3int(start__19)
    i__22 = inline1334
    var done__23 *ref_bool_x
    var inline1331 bool = false
    var inline1332 *ref_bool_x = ref__Ref_4bool(inline1331)
    done__23 = inline1332
    Loop_loop407:
    for {
        var t420 bool
        var inline1325 bool = ref_get__Ref_4bool(done__23)
        t420 = inline1325
        var t421 bool = !t420
        var jp409 bool
        if t421 {
            var t422 int
            var inline1300 int = ref_get__Ref_3int(i__22)
            t422 = inline1300
            var t423 bool = t422 < len__20
            jp409 = t423
        } else {
            jp409 = false
        }
        if jp409 {
            var t410 int
            var inline1323 int = ref_get__Ref_3int(i__22)
            t410 = inline1323
            var ch__24 rune
            var inline1321 rune = string_get(source__18, t410)
            ch__24 = inline1321
            var t412 bool
            var inline1315 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__24, 40)
            var inline1317 bool
            if inline1315 {
                inline1317 = true
            } else {
                var inline1319 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__24, 41)
                inline1317 = inline1319
            }
            if inline1317 {
                t412 = true
                if t412 {
                    var inline1302 bool = true
                    ref_set__Ref_4bool(done__23, inline1302)
                    continue
                } else {
                    var t414 string
                    var inline1313 string = ref_get__Ref_6string(text__21)
                    t414 = inline1313
                    var t415 string
                    var inline1311 string = char_to_string(ch__24)
                    t415 = inline1311
                    var t416 string = t414 + t415
                    ref_set__Ref_6string(text__21, t416)
                    var t417 int
                    var inline1307 int = ref_get__Ref_3int(i__22)
                    t417 = inline1307
                    var t418 int = t417 + 1
                    ref_set__Ref_3int(i__22, t418)
                    continue
                }
            } else {
                var inline1318 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__24, 32)
                t412 = inline1318
                if t412 {
                    var inline1302 bool = true
                    ref_set__Ref_4bool(done__23, inline1302)
                    continue
                } else {
                    var t414 string
                    var inline1313 string = ref_get__Ref_6string(text__21)
                    t414 = inline1313
                    var t415 string
                    var inline1311 string = char_to_string(ch__24)
                    t415 = inline1311
                    var t416 string = t414 + t415
                    ref_set__Ref_6string(text__21, t416)
                    var t417 int
                    var inline1307 int = ref_get__Ref_3int(i__22)
                    t417 = inline1307
                    var t418 int = t417 + 1
                    ref_set__Ref_3int(i__22, t418)
                    continue
                }
            }
        } else {
            break Loop_loop407
        }
    }
    var atom__25 string
    var inline1329 string = ref_get__Ref_6string(text__21)
    atom__25 = inline1329
    var jp396 Token
    switch atom__25 {
    case "true":
        var t399 Token = Token_Bool{
            _0: true,
        }
        jp396 = t399
    case "false":
        var t400 Token = Token_Bool{
            _0: false,
        }
        jp396 = t400
    default:
        var t403 bool = is_int_text(atom__25)
        if t403 {
            var t404 int32 = parse_int32(atom__25)
            var t405 Token = Token_Int{
                _0: t404,
            }
            jp396 = t405
        } else {
            var t406 Token = Token_Sym{
                _0: atom__25,
            }
            jp396 = t406
        }
    }
    var t397 int
    var inline1327 int = ref_get__Ref_3int(i__22)
    t397 = inline1327
    var t398 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: jp396,
        _1: t397,
    }
    return t398
}

func lex(source__27 string) *_goml_vec_Token {
    var len__28 int = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token
    var inline1391 *_goml_vec_Token = vec_new__Vec_5Token()
    toks0__29 = inline1391
    var toks__30 *ref_Vec_5Token_x
    var inline1389 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    toks__30 = inline1389
    var i__31 *ref_int_x
    var inline1386 int = 0
    var inline1387 *ref_int_x = ref__Ref_3int(inline1386)
    i__31 = inline1387
    Loop_loop428:
    for {
        var t429 int
        var inline1382 int = ref_get__Ref_3int(i__31)
        t429 = inline1382
        var t430 bool = t429 < len__28
        if t430 {
            var t431 int
            var inline1380 int = ref_get__Ref_3int(i__31)
            t431 = inline1380
            var ch__32 rune
            var inline1378 rune = string_get(source__27, t431)
            ch__32 = inline1378
            var t433 bool
            var inline1375 rune = 40
            var inline1376 bool = ch__32 == inline1375
            t433 = inline1376
            if t433 {
                var t434 *_goml_vec_Token
                var inline1347 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                t434 = inline1347
                var t435 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t434, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t435)
                var t436 int
                var inline1343 int = ref_get__Ref_3int(i__31)
                t436 = inline1343
                var t437 int = t436 + 1
                ref_set__Ref_3int(i__31, t437)
                continue
            } else {
                var t440 bool
                var inline1372 rune = 41
                var inline1373 bool = ch__32 == inline1372
                t440 = inline1373
                if t440 {
                    var t441 *_goml_vec_Token
                    var inline1355 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                    t441 = inline1355
                    var t442 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t441, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t442)
                    var t443 int
                    var inline1351 int = ref_get__Ref_3int(i__31)
                    t443 = inline1351
                    var t444 int = t443 + 1
                    ref_set__Ref_3int(i__31, t444)
                    continue
                } else {
                    var t447 bool
                    var inline1369 rune = 32
                    var inline1370 bool = ch__32 == inline1369
                    t447 = inline1370
                    if t447 {
                        var t448 int
                        var inline1359 int = ref_get__Ref_3int(i__31)
                        t448 = inline1359
                        var t449 int = t448 + 1
                        ref_set__Ref_3int(i__31, t449)
                        continue
                    } else {
                        var t451 int
                        var inline1367 int = ref_get__Ref_3int(i__31)
                        t451 = inline1367
                        var mtmp190 Tuple2_5Token_3int = lex_atom(source__27, t451)
                        var x191 Token = mtmp190._0
                        var x192 int = mtmp190._1
                        var t452 *_goml_vec_Token
                        var inline1365 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                        t452 = inline1365
                        var t453 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t452, x191)
                        ref_set__Ref_10Vec_5Token(toks__30, t453)
                        ref_set__Ref_3int(i__31, x192)
                        continue
                    }
                }
            }
        } else {
            break Loop_loop428
        }
    }
    var inline1384 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
    return inline1384
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var t457 int
    var inline1419 int = vec_len__Vec_7Binding(env__35)
    t457 = inline1419
    var t458 int = t457 - 1
    var i__37 *ref_int_x
    var inline1417 *ref_int_x = ref__Ref_3int(t458)
    i__37 = inline1417
    var result__38 *ref_Value_x
    var inline1415 *ref_Value_x = ref__Ref_5Value(Nil{})
    result__38 = inline1415
    var done__39 *ref_bool_x
    var inline1412 bool = false
    var inline1413 *ref_bool_x = ref__Ref_4bool(inline1412)
    done__39 = inline1413
    Loop_loop461:
    for {
        var t473 bool
        var inline1408 bool = ref_get__Ref_4bool(done__39)
        t473 = inline1408
        var t474 bool = !t473
        var jp463 bool
        if t474 {
            var t475 int
            var inline1393 int = ref_get__Ref_3int(i__37)
            t475 = inline1393
            var t476 bool = t475 >= 0
            jp463 = t476
        } else {
            jp463 = false
        }
        if jp463 {
            var t464 int
            var inline1406 int = ref_get__Ref_3int(i__37)
            t464 = inline1406
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t464)
            var t466 string = binding__40.name
            var t467 bool
            var inline1404 bool = t466 == name__36
            t467 = inline1404
            if t467 {
                var t468 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t468)
                var inline1395 bool = true
                ref_set__Ref_4bool(done__39, inline1395)
                continue
            } else {
                var t470 int
                var inline1402 int = ref_get__Ref_3int(i__37)
                t470 = inline1402
                var t471 int = t470 - 1
                ref_set__Ref_3int(i__37, t471)
                continue
            }
        } else {
            break Loop_loop461
        }
    }
    var inline1410 Value = ref_get__Ref_5Value(result__38)
    return inline1410
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var acc__47 *_goml_vec_SExpr
    var inline1455 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__47 = inline1455
    var exprs__48 *ref_Vec_5SExpr_x
    var inline1453 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    exprs__48 = inline1453
    var i__49 *ref_int_x
    var inline1451 *ref_int_x = ref__Ref_3int(start__46)
    i__49 = inline1451
    var done__50 *ref_bool_x
    var inline1448 bool = false
    var inline1449 *ref_bool_x = ref__Ref_4bool(inline1448)
    done__50 = inline1449
    Loop_loop488:
    for {
        var t500 bool
        var inline1442 bool = ref_get__Ref_4bool(done__50)
        t500 = inline1442
        var t501 bool = !t500
        var jp490 bool
        if t501 {
            var t502 int
            var inline1423 int = ref_get__Ref_3int(i__49)
            t502 = inline1423
            var t503 int
            var inline1421 int = vec_len__Vec_5Token(tokens__45)
            t503 = inline1421
            var t504 bool = t502 < t503
            jp490 = t504
        } else {
            jp490 = false
        }
        if jp490 {
            var t491 int
            var inline1440 int = ref_get__Ref_3int(i__49)
            t491 = inline1440
            var mtmp201 Token = vec_get__Vec_5Token(tokens__45, t491)
            switch mtmp201.(type) {
            case RParen:
                var inline1429 bool = true
                ref_set__Ref_4bool(done__50, inline1429)
                var t493 int
                var inline1427 int = ref_get__Ref_3int(i__49)
                t493 = inline1427
                var t494 int = t493 + 1
                ref_set__Ref_3int(i__49, t494)
                continue
            default:
                var t496 int
                var inline1438 int = ref_get__Ref_3int(i__49)
                t496 = inline1438
                var mtmp206 Tuple2_5SExpr_3int = parse_expr(tokens__45, t496)
                var x207 SExpr = mtmp206._0
                var x208 int = mtmp206._1
                var t497 *_goml_vec_SExpr
                var inline1436 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                t497 = inline1436
                var t498 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t497, x207)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t498)
                ref_set__Ref_3int(i__49, x208)
                continue
            }
        } else {
            break Loop_loop488
        }
    }
    var t485 *_goml_vec_SExpr
    var inline1446 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    t485 = inline1446
    var t486 int
    var inline1444 int = ref_get__Ref_3int(i__49)
    t486 = inline1444
    var t487 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t485,
        _1: t486,
    }
    return t487
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var mtmp211 Token = vec_get__Vec_5Token(tokens__53, start__54)
    switch mtmp211.(type) {
    case LParen:
        var t509 int = start__54 + 1
        var mtmp215 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t509)
        var x216 *_goml_vec_SExpr = mtmp215._0
        var x217 int = mtmp215._1
        var t510 SExpr = List{
            _0: x216,
        }
        var t511 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t510,
            _1: x217,
        }
        return t511
    case RParen:
        var t512 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t513 int = start__54 + 1
        var t514 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t512,
            _1: t513,
        }
        return t514
    case Token_Sym:
        var x212 string = mtmp211.(Token_Sym)._0
        var t515 SExpr = SExpr_Sym{
            _0: x212,
        }
        var t516 int = start__54 + 1
        var t517 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t515,
            _1: t516,
        }
        return t517
    case Token_Int:
        var x213 int32 = mtmp211.(Token_Int)._0
        var t518 SExpr = SExpr_Int{
            _0: x213,
        }
        var t519 int = start__54 + 1
        var t520 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t518,
            _1: t519,
        }
        return t520
    case Token_Bool:
        var x214 bool = mtmp211.(Token_Bool)._0
        var t521 SExpr = SExpr_Bool{
            _0: x214,
        }
        var t522 int = start__54 + 1
        var t523 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t521,
            _1: t522,
        }
        return t523
    default:
        panic("non-exhaustive match")
    }
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var i__61 *ref_int_x
    var inline1475 int = 0
    var inline1476 *ref_int_x = ref__Ref_3int(inline1475)
    i__61 = inline1476
    var acc__62 *_goml_vec_SExpr
    var inline1473 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__62 = inline1473
    var exprs__63 *ref_Vec_5SExpr_x
    var inline1471 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    exprs__63 = inline1471
    Loop_loop528:
    for {
        var t529 int
        var inline1467 int = ref_get__Ref_3int(i__61)
        t529 = inline1467
        var t530 int
        var inline1465 int = vec_len__Vec_5Token(tokens__60)
        t530 = inline1465
        var t531 bool = t529 < t530
        if t531 {
            var t532 int
            var inline1463 int = ref_get__Ref_3int(i__61)
            t532 = inline1463
            var mtmp218 Tuple2_5SExpr_3int = parse_expr(tokens__60, t532)
            var x219 SExpr = mtmp218._0
            var x220 int = mtmp218._1
            var t533 *_goml_vec_SExpr
            var inline1461 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            t533 = inline1461
            var t534 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t533, x219)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t534)
            ref_set__Ref_3int(i__61, x220)
            continue
        } else {
            break Loop_loop528
        }
    }
    var inline1469 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    return inline1469
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    switch expr__72.(type) {
    case SExpr_Int:
        var x229 int32 = expr__72.(SExpr_Int)._0
        var t552 Value = Value_Int{
            _0: x229,
        }
        return t552
    case SExpr_Bool:
        var x230 bool = expr__72.(SExpr_Bool)._0
        var t553 Value = Value_Bool{
            _0: x230,
        }
        return t553
    case SExpr_Sym:
        var x231 string = expr__72.(SExpr_Sym)._0
        var t554 *_goml_vec_Binding
        var inline1489 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__74)
        t554 = inline1489
        var inline1485 Value = env_lookup(local__73, x231)
        switch inline1485.(type) {
        case Nil:
            var inline1486 Value = env_lookup(t554, x231)
            return inline1486
        default:
            return inline1485
        }
    case List:
        var x232 *_goml_vec_SExpr = expr__72.(List)._0
        var inline1491 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(x232)
        var inline1492 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(inline1491, 0)
        if inline1492 {
            return Nil{}
        } else {
            var inline1493 SExpr = vec_get__Vec_5SExpr(x232, 0)
            switch inline1493.(type) {
            case SExpr_Sym:
                var inline1494 string = inline1493.(SExpr_Sym)._0
                var inline1496 Value = eval_list_sym(inline1494, x232, local__73, global__74)
                return inline1496
            default:
                var inline1497 Value = eval(inline1493, local__73, global__74)
                var inline1498 *_goml_vec_Value = eval_args(x232, 1, local__73, global__74)
                var inline1499 Value = apply(inline1497, inline1498, global__74)
                return inline1499
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    switch name__86 {
    case "begin":
        var t571 Value = eval_begin(items__87, 1, local__88, global__89)
        return t571
    case "define":
        var t574 int
        var inline1517 int = vec_len__Vec_5SExpr(items__87)
        t574 = inline1517
        var t575 bool
        var inline1514 int = 3
        var inline1515 bool = t574 == inline1514
        t575 = inline1515
        if t575 {
            var mtmp237 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp237.(type) {
            case SExpr_Sym:
                var x240 string = mtmp237.(SExpr_Sym)._0
                var t578 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t578, local__88, global__89)
                var env__92 *_goml_vec_Binding
                var inline1512 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__89)
                env__92 = inline1512
                var t579 Binding = Binding{
                    name: x240,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t579)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                return value__91
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "if":
        var t582 int
        var inline1530 int = vec_len__Vec_5SExpr(items__87)
        t582 = inline1530
        var t583 bool
        var inline1527 int = 4
        var inline1528 bool = t582 == inline1527
        t583 = inline1528
        if t583 {
            var t584 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t584, local__88, global__89)
            var t587 bool
            switch cond__94.(type) {
            case Value_Int:
                var inline1519 int32 = cond__94.(Value_Int)._0
                var inline1521 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline1519, 0)
                var inline1522 bool = !inline1521
                t587 = inline1522
            case Value_Bool:
                var inline1523 bool = cond__94.(Value_Bool)._0
                t587 = inline1523
            case Func:
                t587 = true
            case Nil:
                t587 = false
            default:
                panic("non-exhaustive match")
            }
            if t587 {
                var t588 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t589 Value = eval(t588, local__88, global__89)
                return t589
            } else {
                var t590 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t591 Value = eval(t590, local__88, global__89)
                return t591
            }
        } else {
            return Nil{}
        }
    case "lambda":
        var t594 int
        var inline1535 int = vec_len__Vec_5SExpr(items__87)
        t594 = inline1535
        var t595 bool
        var inline1532 int = 3
        var inline1533 bool = t594 == inline1532
        t595 = inline1533
        if t595 {
            var mtmp243 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp243.(type) {
            case List:
                var x247 *_goml_vec_SExpr = mtmp243.(List)._0
                var params__96 *_goml_vec_string = params_from_sexprs(x247)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t598 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t599 Value = Func{
                    _0: t598,
                }
                return t599
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "+":
        var t600 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t601 Value = apply_builtin("+", t600)
        return t601
    case "-":
        var t602 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t603 Value = apply_builtin("-", t602)
        return t603
    case "*":
        var t604 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t605 Value = apply_builtin("*", t604)
        return t605
    case "/":
        var t606 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t607 Value = apply_builtin("/", t606)
        return t607
    case "=":
        var t608 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t609 Value = apply_builtin("=", t608)
        return t609
    default:
        var t610 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t610, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        switch f__98.(type) {
        case Func:
            var inline1537 Lambda = f__98.(Func)._0
            var inline1539 Value = apply_lambda(inline1537, args__99)
            return inline1539
        default:
            return Nil{}
        }
    }
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var i__104 *ref_int_x
    var inline1557 *ref_int_x = ref__Ref_3int(start__101)
    i__104 = inline1557
    var last__105 *ref_Value_x
    var inline1555 *ref_Value_x = ref__Ref_5Value(Nil{})
    last__105 = inline1555
    Loop_loop616:
    for {
        var t617 int
        var inline1551 int = ref_get__Ref_3int(i__104)
        t617 = inline1551
        var t618 int
        var inline1549 int = vec_len__Vec_5SExpr(items__100)
        t618 = inline1549
        var t619 bool = t617 < t618
        if t619 {
            var t620 int
            var inline1547 int = ref_get__Ref_3int(i__104)
            t620 = inline1547
            var t621 SExpr = vec_get__Vec_5SExpr(items__100, t620)
            var v__106 Value = eval(t621, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t622 int
            var inline1543 int = ref_get__Ref_3int(i__104)
            t622 = inline1543
            var t623 int = t622 + 1
            ref_set__Ref_3int(i__104, t623)
            continue
        } else {
            break Loop_loop616
        }
    }
    var inline1553 Value = ref_get__Ref_5Value(last__105)
    return inline1553
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var i__108 *ref_int_x
    var inline1583 int = 0
    var inline1584 *ref_int_x = ref__Ref_3int(inline1583)
    i__108 = inline1584
    var acc__109 *_goml_vec_string
    var inline1581 *_goml_vec_string = vec_new__Vec_6string()
    acc__109 = inline1581
    var params__110 *ref_Vec_6string_x
    var inline1579 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    params__110 = inline1579
    Loop_loop629:
    for {
        var t630 int
        var inline1575 int = ref_get__Ref_3int(i__108)
        t630 = inline1575
        var t631 int
        var inline1573 int = vec_len__Vec_5SExpr(items__107)
        t631 = inline1573
        var t632 bool = t630 < t631
        if t632 {
            var t633 int
            var inline1571 int = ref_get__Ref_3int(i__108)
            t633 = inline1571
            var mtmp250 SExpr = vec_get__Vec_5SExpr(items__107, t633)
            switch mtmp250.(type) {
            case SExpr_Sym:
                var x253 string = mtmp250.(SExpr_Sym)._0
                var t635 *_goml_vec_string
                var inline1565 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
                t635 = inline1565
                var t636 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t635, x253)
                ref_set__Ref_11Vec_6string(params__110, t636)
                var t637 int
                var inline1561 int = ref_get__Ref_3int(i__108)
                t637 = inline1561
                var t638 int = t637 + 1
                ref_set__Ref_3int(i__108, t638)
                continue
            default:
                var t640 int
                var inline1569 int = ref_get__Ref_3int(i__108)
                t640 = inline1569
                var t641 int = t640 + 1
                ref_set__Ref_3int(i__108, t641)
                continue
            }
        } else {
            break Loop_loop629
        }
    }
    var inline1577 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
    return inline1577
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var i__116 *ref_int_x
    var inline1606 *ref_int_x = ref__Ref_3int(start__113)
    i__116 = inline1606
    var acc__117 *_goml_vec_Value
    var inline1604 *_goml_vec_Value = vec_new__Vec_5Value()
    acc__117 = inline1604
    var args__118 *ref_Vec_5Value_x
    var inline1602 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    args__118 = inline1602
    Loop_loop647:
    for {
        var t648 int
        var inline1598 int = ref_get__Ref_3int(i__116)
        t648 = inline1598
        var t649 int
        var inline1596 int = vec_len__Vec_5SExpr(items__112)
        t649 = inline1596
        var t650 bool = t648 < t649
        if t650 {
            var t651 int
            var inline1594 int = ref_get__Ref_3int(i__116)
            t651 = inline1594
            var t652 SExpr = vec_get__Vec_5SExpr(items__112, t651)
            var v__119 Value = eval(t652, local__114, global__115)
            var t653 *_goml_vec_Value
            var inline1592 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
            t653 = inline1592
            var t654 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t653, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t654)
            var t655 int
            var inline1588 int = ref_get__Ref_3int(i__116)
            t655 = inline1588
            var t656 int = t655 + 1
            ref_set__Ref_3int(i__116, t656)
            continue
        } else {
            break Loop_loop647
        }
    }
    var inline1600 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
    return inline1600
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    switch name__120 {
    case "=":
        var t664 int
        var inline1615 int = vec_len__Vec_5Value(args__121)
        t664 = inline1615
        var t665 bool
        var inline1612 int = 2
        var inline1613 bool = t664 == inline1612
        t665 = inline1613
        if t665 {
            var t666 Value = vec_get__Vec_5Value(args__121, 0)
            var t667 Value = vec_get__Vec_5Value(args__121, 1)
            switch t667.(type) {
            case Value_Int:
                var x262 int32 = t667.(Value_Int)._0
                switch t666.(type) {
                case Value_Int:
                    var x265 int32 = t666.(Value_Int)._0
                    var t672 bool
                    var inline1608 bool = x265 == x262
                    t672 = inline1608
                    var t673 Value = Value_Bool{
                        _0: t672,
                    }
                    return t673
                default:
                    var t674 Value = Value_Bool{
                        _0: false,
                    }
                    return t674
                }
            case Value_Bool:
                var x263 bool = t667.(Value_Bool)._0
                switch t666.(type) {
                case Value_Bool:
                    var x269 bool = t666.(Value_Bool)._0
                    var t677 bool
                    var inline1610 bool = x269 == x263
                    t677 = inline1610
                    var t678 Value = Value_Bool{
                        _0: t677,
                    }
                    return t678
                default:
                    var t679 Value = Value_Bool{
                        _0: false,
                    }
                    return t679
                }
            default:
                var t680 Value = Value_Bool{
                    _0: false,
                }
                return t680
            }
        } else {
            var t681 Value = Value_Bool{
                _0: false,
            }
            return t681
        }
    case "+":
        var i__126 *ref_int_x
        var inline1640 int = 0
        var inline1641 *ref_int_x = ref__Ref_3int(inline1640)
        i__126 = inline1641
        var acc__127 *ref_int32_x
        var inline1637 int32 = 0
        var inline1638 *ref_int32_x = ref__Ref_5int32(inline1637)
        acc__127 = inline1638
        Loop_loop685:
        for {
            var t686 int
            var inline1633 int = ref_get__Ref_3int(i__126)
            t686 = inline1633
            var t687 int
            var inline1631 int = vec_len__Vec_5Value(args__121)
            t687 = inline1631
            var t688 bool = t686 < t687
            if t688 {
                var t689 int
                var inline1629 int = ref_get__Ref_3int(i__126)
                t689 = inline1629
                var mtmp271 Value = vec_get__Vec_5Value(args__121, t689)
                switch mtmp271.(type) {
                case Value_Int:
                    var x272 int32 = mtmp271.(Value_Int)._0
                    var t691 int32
                    var inline1623 int32 = ref_get__Ref_5int32(acc__127)
                    t691 = inline1623
                    var t692 int32 = t691 + x272
                    ref_set__Ref_5int32(acc__127, t692)
                    var t693 int
                    var inline1619 int = ref_get__Ref_3int(i__126)
                    t693 = inline1619
                    var t694 int = t693 + 1
                    ref_set__Ref_3int(i__126, t694)
                    continue
                default:
                    var t696 int
                    var inline1627 int = ref_get__Ref_3int(i__126)
                    t696 = inline1627
                    var t697 int = t696 + 1
                    ref_set__Ref_3int(i__126, t697)
                    continue
                }
            } else {
                break Loop_loop685
            }
        }
        var t683 int32
        var inline1635 int32 = ref_get__Ref_5int32(acc__127)
        t683 = inline1635
        var t684 Value = Value_Int{
            _0: t683,
        }
        return t684
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop702:
        for {
            var t703 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
            var t704 int
            var inline1657 int = vec_len__Vec_5Value(args__121)
            t704 = inline1657
            var t705 bool = t703 < t704
            if t705 {
                var t706 int
                var inline1655 int = ref_get__Ref_3int(i__129)
                t706 = inline1655
                var mtmp277 Value = vec_get__Vec_5Value(args__121, t706)
                switch mtmp277.(type) {
                case Value_Int:
                    var x278 int32 = mtmp277.(Value_Int)._0
                    var t708 int32
                    var inline1649 int32 = ref_get__Ref_5int32(acc__130)
                    t708 = inline1649
                    var t709 int32 = t708 * x278
                    ref_set__Ref_5int32(acc__130, t709)
                    var t710 int
                    var inline1645 int = ref_get__Ref_3int(i__129)
                    t710 = inline1645
                    var t711 int = t710 + 1
                    ref_set__Ref_3int(i__129, t711)
                    continue
                default:
                    var t713 int
                    var inline1653 int = ref_get__Ref_3int(i__129)
                    t713 = inline1653
                    var t714 int = t713 + 1
                    ref_set__Ref_3int(i__129, t714)
                    continue
                }
            } else {
                break Loop_loop702
            }
        }
        var t700 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t701 Value = Value_Int{
            _0: t700,
        }
        return t701
    case "-":
        var mtmp283 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        switch mtmp283 {
        case 1:
            var mtmp284 Value = vec_get__Vec_5Value(args__121, 0)
            switch mtmp284.(type) {
            case Value_Int:
                var x285 int32 = mtmp284.(Value_Int)._0
                var t720 int32 = 0 - x285
                var t721 Value = Value_Int{
                    _0: t720,
                }
                return t721
            default:
                return Nil{}
            }
        case 2:
            var t722 Value = vec_get__Vec_5Value(args__121, 0)
            var t723 Value = vec_get__Vec_5Value(args__121, 1)
            switch t723.(type) {
            case Value_Int:
                var x291 int32 = t723.(Value_Int)._0
                switch t722.(type) {
                case Value_Int:
                    var x294 int32 = t722.(Value_Int)._0
                    var t728 int32 = x294 - x291
                    var t729 Value = Value_Int{
                        _0: t728,
                    }
                    return t729
                default:
                    return Nil{}
                }
            default:
                return Nil{}
            }
        default:
            return Nil{}
        }
    case "/":
        var t732 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t733 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t732, 2)
        if t733 {
            var t734 Value = vec_get__Vec_5Value(args__121, 0)
            var t735 Value = vec_get__Vec_5Value(args__121, 1)
            switch t735.(type) {
            case Value_Int:
                var x300 int32 = t735.(Value_Int)._0
                switch t734.(type) {
                case Value_Int:
                    var x303 int32 = t734.(Value_Int)._0
                    var t740 int32 = x303 / x300
                    var t741 Value = Value_Int{
                        _0: t740,
                    }
                    return t741
                default:
                    return Nil{}
                }
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    default:
        return Nil{}
    }
}

func apply(func__137 Value, args__138 *_goml_vec_Value, global__139 *ref_Vec_7Binding_x) Value {
    switch func__137.(type) {
    case Func:
        var x308 Lambda = func__137.(Func)._0
        var t746 Value = apply_lambda(x308, args__138)
        return t746
    default:
        return Nil{}
    }
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var t749 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x
    var inline1684 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t749)
    env__143 = inline1684
    var i__144 *ref_int_x
    var inline1681 int = 0
    var inline1682 *ref_int_x = ref__Ref_3int(inline1681)
    i__144 = inline1682
    Loop_loop755:
    for {
        var t766 int
        var inline1677 int = ref_get__Ref_3int(i__144)
        t766 = inline1677
        var t767 *_goml_vec_string = lambda__141.params
        var t768 int
        var inline1675 int = vec_len__Vec_6string(t767)
        t768 = inline1675
        var t769 bool = t766 < t768
        var jp757 bool
        if t769 {
            var t770 int
            var inline1661 int = ref_get__Ref_3int(i__144)
            t770 = inline1661
            var t771 int
            var inline1659 int = vec_len__Vec_5Value(args__142)
            t771 = inline1659
            var t772 bool = t770 < t771
            jp757 = t772
        } else {
            jp757 = false
        }
        if jp757 {
            var t758 *_goml_vec_string = lambda__141.params
            var t759 int
            var inline1673 int = ref_get__Ref_3int(i__144)
            t759 = inline1673
            var name__145 string = vec_get__Vec_6string(t758, t759)
            var t760 int
            var inline1671 int = ref_get__Ref_3int(i__144)
            t760 = inline1671
            var value__146 Value = vec_get__Vec_5Value(args__142, t760)
            var t761 *_goml_vec_Binding
            var inline1669 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
            t761 = inline1669
            var t762 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t761, t762)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t763 int
            var inline1665 int = ref_get__Ref_3int(i__144)
            t763 = inline1665
            var t764 int = t763 + 1
            ref_set__Ref_3int(i__144, t764)
            continue
        } else {
            break Loop_loop755
        }
    }
    var t751 SExpr = lambda__141.body
    var t752 *_goml_vec_Binding
    var inline1679 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
    t752 = inline1679
    var t753 *ref_Vec_7Binding_x = lambda__141.global
    var t754 Value = eval(t751, t752, t753)
    return t754
}

func main0() struct{} {
    var t774 *_goml_vec_Binding
    var inline1714 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t774 = inline1714
    var global__148 *ref_Vec_7Binding_x
    var inline1712 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t774)
    global__148 = inline1712
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t775 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t775)
    var t776 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t777 *_goml_vec_Binding
    var inline1710 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t777 = inline1710
    var result__151 Value = eval(t776, t777, global__148)
    var t778 string
    switch result__151.(type) {
    case Value_Int:
        var inline1702 int32 = result__151.(Value_Int)._0
        var inline1704 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1702)
        t778 = inline1704
    case Value_Bool:
        var inline1705 bool = result__151.(Value_Bool)._0
        var inline1707 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1705)
        t778 = inline1707
    case Func:
        t778 = "<lambda>"
    case Nil:
        t778 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1699 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t778)
    _goml_runtime_core_string_println(inline1699)
    var t779 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t779)
    var t780 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t781 *_goml_vec_Binding
    var inline1697 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t781 = inline1697
    var result2__153 Value = eval(t780, t781, global__148)
    var t782 string
    switch result2__153.(type) {
    case Value_Int:
        var inline1689 int32 = result2__153.(Value_Int)._0
        var inline1691 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1689)
        t782 = inline1691
    case Value_Bool:
        var inline1692 bool = result2__153.(Value_Bool)._0
        var inline1694 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1692)
        t782 = inline1694
    case Func:
        t782 = "<lambda>"
    case Nil:
        t782 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1686 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t782)
    _goml_runtime_core_string_println(inline1686)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__37 string) int {
    var t785 int = _goml_runtime_core_string_len(self__37)
    return t785
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__88 int, other__89 int) bool {
    var t788 bool = self__88 == other__89
    return t788
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__236 int) *ref_int_x {
    var t791 *ref_int_x = ref__Ref_3int(value__236)
    return t791
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__236 bool) *ref_bool_x {
    var t794 *ref_bool_x = ref__Ref_4bool(value__236)
    return t794
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__237 *ref_int_x) int {
    var t800 int = ref_get__Ref_3int(self__237)
    return t800
}

func _goml_m_trait__impl_i_Eq_i_char_i_eq(self__86 rune, other__87 rune) bool {
    var t806 bool = self__86 == other__87
    return t806
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__236 int32) *ref_int32_x {
    var t813 *ref_int32_x = ref__Ref_5int32(value__236)
    return t813
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__237 *ref_int32_x) int32 {
    var t816 int32 = ref_get__Ref_5int32(self__237)
    return t816
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__157 *_goml_vec_Token, elem__158 Token) *_goml_vec_Token {
    var t841 int
    var inline1737 int = vec_len__Vec_5Token(self__157)
    t841 = inline1737
    var t842 int = t841 + 1
    var result__159 *_goml_vec_Token
    var inline1735 *_goml_vec_Token = vec_with_capacity__Vec_5Token(t842)
    result__159 = inline1735
    var index__160 int = 0
    Loop_loop844:
    for {
        var t845 int
        var inline1731 int = vec_len__Vec_5Token(self__157)
        t845 = inline1731
        var t846 bool = index__160 < t845
        if t846 {
            var t847 Token = vec_get__Vec_5Token(self__157, index__160)
            vec_push__Vec_5Token(result__159, t847)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t848 int = compound_old60 + compound_value61
            index__160 = t848
            continue
        } else {
            break Loop_loop844
        }
    }
    vec_push__Vec_5Token(result__159, elem__158)
    return result__159
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__157 *_goml_vec_SExpr, elem__158 SExpr) *_goml_vec_SExpr {
    var t880 int
    var inline1747 int = vec_len__Vec_5SExpr(self__157)
    t880 = inline1747
    var t881 int = t880 + 1
    var result__159 *_goml_vec_SExpr
    var inline1745 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(t881)
    result__159 = inline1745
    var index__160 int = 0
    Loop_loop883:
    for {
        var t884 int
        var inline1741 int = vec_len__Vec_5SExpr(self__157)
        t884 = inline1741
        var t885 bool = index__160 < t884
        if t885 {
            var t886 SExpr = vec_get__Vec_5SExpr(self__157, index__160)
            vec_push__Vec_5SExpr(result__159, t886)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t887 int = compound_old60 + compound_value61
            index__160 = t887
            continue
        } else {
            break Loop_loop883
        }
    }
    vec_push__Vec_5SExpr(result__159, elem__158)
    return result__159
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t893 string = _goml_runtime_core_int32_to_string(self__35)
    return t893
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t896 string = _goml_runtime_core_bool_to_string(self__66)
    return t896
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__94 int32, other__95 int32) bool {
    var t899 bool = self__94 == other__95
    return t899
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__166 *_goml_vec_SExpr) int {
    var t905 int = vec_len__Vec_5SExpr(self__166)
    return t905
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__157 *_goml_vec_Binding, elem__158 Binding) *_goml_vec_Binding {
    var t908 int
    var inline1757 int = vec_len__Vec_7Binding(self__157)
    t908 = inline1757
    var t909 int = t908 + 1
    var result__159 *_goml_vec_Binding
    var inline1755 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(t909)
    result__159 = inline1755
    var index__160 int = 0
    Loop_loop911:
    for {
        var t912 int
        var inline1751 int = vec_len__Vec_7Binding(self__157)
        t912 = inline1751
        var t913 bool = index__160 < t912
        if t913 {
            var t914 Binding = vec_get__Vec_7Binding(self__157, index__160)
            vec_push__Vec_7Binding(result__159, t914)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t915 int = compound_old60 + compound_value61
            index__160 = t915
            continue
        } else {
            break Loop_loop911
        }
    }
    vec_push__Vec_7Binding(result__159, elem__158)
    return result__159
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__157 *_goml_vec_string, elem__158 string) *_goml_vec_string {
    var t930 int
    var inline1767 int = vec_len__Vec_6string(self__157)
    t930 = inline1767
    var t931 int = t930 + 1
    var result__159 *_goml_vec_string
    var inline1765 *_goml_vec_string = vec_with_capacity__Vec_6string(t931)
    result__159 = inline1765
    var index__160 int = 0
    Loop_loop933:
    for {
        var t934 int
        var inline1761 int = vec_len__Vec_6string(self__157)
        t934 = inline1761
        var t935 bool = index__160 < t934
        if t935 {
            var t936 string = vec_get__Vec_6string(self__157, index__160)
            vec_push__Vec_6string(result__159, t936)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t937 int = compound_old60 + compound_value61
            index__160 = t937
            continue
        } else {
            break Loop_loop933
        }
    }
    vec_push__Vec_6string(result__159, elem__158)
    return result__159
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__157 *_goml_vec_Value, elem__158 Value) *_goml_vec_Value {
    var t952 int
    var inline1777 int = vec_len__Vec_5Value(self__157)
    t952 = inline1777
    var t953 int = t952 + 1
    var result__159 *_goml_vec_Value
    var inline1775 *_goml_vec_Value = vec_with_capacity__Vec_5Value(t953)
    result__159 = inline1775
    var index__160 int = 0
    Loop_loop955:
    for {
        var t956 int
        var inline1771 int = vec_len__Vec_5Value(self__157)
        t956 = inline1771
        var t957 bool = index__160 < t956
        if t957 {
            var t958 Value = vec_get__Vec_5Value(self__157, index__160)
            vec_push__Vec_5Value(result__159, t958)
            var compound_old60 int = index__160
            var compound_value61 int = 1
            var t959 int = compound_old60 + compound_value61
            index__160 = t959
            continue
        } else {
            break Loop_loop955
        }
    }
    vec_push__Vec_5Value(result__159, elem__158)
    return result__159
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__166 *_goml_vec_Value) int {
    var t965 int = vec_len__Vec_5Value(self__166)
    return t965
}

func string_get(value__17 string, index__18 int) rune {
    var mtmp6 Tuple3_4bool_4char_3int = string_decode_utf8_at(value__17, index__18)
    var x7 bool = mtmp6._0
    var x8 rune = mtmp6._1
    if x7 {
        return x8
    } else {
        var t985 rune = _goml_runtime_core_string_get("", -1)
        return t985
    }
}

func char_to_string(value__29 rune) string {
    var t990 uint32 = uint32(rune(value__29))
    var t991 bool
    var inline1780 bool = t990 <= 1114111
    if inline1780 {
        var inline1781 bool = t990 >= 55296
        var inline1783 bool
        if inline1781 {
            var inline1785 bool = t990 <= 57343
            inline1783 = inline1785
        } else {
            inline1783 = false
        }
        var inline1784 bool = !inline1783
        t991 = inline1784
    } else {
        t991 = false
    }
    if t991 {
        var t992 string = _goml_runtime_core_char_to_string(value__29)
        return t992
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1138 bool = index__6 < 0
    var jp1136 bool
    if t1138 {
        jp1136 = true
    } else {
        var t1139 bool = index__6 >= length__7
        jp1136 = t1139
    }
    if jp1136 {
        var inline1787 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1787
    } else {
        var t1023 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1023))
        var t1026 bool = first__8 < 128
        if t1026 {
            var inline1789 int = 1
            var inline1790 Option__char = char_from_uint32(first__8)
            switch inline1790.(type) {
            case None:
                var inline1791 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1791
            case Some:
                var inline1792 rune = inline1790.(Some)._0
                var inline1794 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1792,
                    _2: inline1789,
                }
                return inline1794
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1030 bool = first__8 < 194
            if t1030 {
                var inline1796 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1796
            } else {
                var t1034 bool = first__8 < 224
                if t1034 {
                    var t1047 int = length__7 - index__6
                    var t1048 bool = t1047 < 2
                    if t1048 {
                        var inline1798 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1798
                    } else {
                        var t1036 int = index__6 + 1
                        var t1037 uint8
                        var inline1812 uint8 = _goml_runtime_core_string_byte_get(value__5, t1036)
                        t1037 = inline1812
                        var second__9 uint32 = uint32(uint8(t1037))
                        var t1040 bool
                        var inline1809 bool = second__9 < 128
                        if inline1809 {
                            t1040 = true
                        } else {
                            var inline1810 bool = second__9 > 191
                            t1040 = inline1810
                        }
                        if t1040 {
                            var inline1800 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1800
                        } else {
                            var t1042_rhs uint32 = 31
                            var t1042 uint32 = first__8 & t1042_rhs
                            var t1043_rhs int = 6
                            var t1043 uint32 = t1042 << t1043_rhs
                            var t1044_rhs uint32 = 63
                            var t1044 uint32 = second__9 & t1044_rhs
                            var t1045 uint32 = t1043 | t1044
                            var inline1802 int = 2
                            var inline1803 Option__char = char_from_uint32(t1045)
                            switch inline1803.(type) {
                            case None:
                                var inline1804 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1804
                            case Some:
                                var inline1805 rune = inline1803.(Some)._0
                                var inline1807 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1805,
                                    _2: inline1802,
                                }
                                return inline1807
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1052 bool = first__8 < 240
                    if t1052 {
                        var t1085 int = length__7 - index__6
                        var t1086 bool = t1085 < 3
                        if t1086 {
                            var inline1814 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1814
                        } else {
                            var t1054 int = index__6 + 1
                            var t1055 uint8
                            var inline1829 uint8 = _goml_runtime_core_string_byte_get(value__5, t1054)
                            t1055 = inline1829
                            var second__10 uint32 = uint32(uint8(t1055))
                            var t1056 int = index__6 + 2
                            var t1057 uint8
                            var inline1827 uint8 = _goml_runtime_core_string_byte_get(value__5, t1056)
                            t1057 = inline1827
                            var third__11 uint32 = uint32(uint8(t1057))
                            var t1083 bool = utf8_invalid_continuation(second__10)
                            var jp1078 bool
                            if t1083 {
                                jp1078 = true
                            } else {
                                var inline1816 bool = third__11 < 128
                                if inline1816 {
                                    jp1078 = true
                                } else {
                                    var inline1817 bool = third__11 > 191
                                    jp1078 = inline1817
                                }
                            }
                            var jp1072 bool
                            if jp1078 {
                                jp1072 = true
                            } else {
                                var t1081 bool
                                var inline1819 uint32 = 224
                                var inline1820 bool = first__8 == inline1819
                                t1081 = inline1820
                                if t1081 {
                                    var t1082 bool = second__10 < 160
                                    jp1072 = t1082
                                } else {
                                    jp1072 = false
                                }
                            }
                            var jp1061 bool
                            if jp1072 {
                                jp1061 = true
                            } else {
                                var t1075 bool
                                var inline1822 uint32 = 237
                                var inline1823 bool = first__8 == inline1822
                                t1075 = inline1823
                                if t1075 {
                                    var t1076 bool = second__10 >= 160
                                    jp1061 = t1076
                                } else {
                                    jp1061 = false
                                }
                            }
                            if jp1061 {
                                var inline1825 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1825
                            } else {
                                var t1063_rhs uint32 = 15
                                var t1063 uint32 = first__8 & t1063_rhs
                                var t1064_rhs int = 12
                                var t1064 uint32 = t1063 << t1064_rhs
                                var t1065_rhs uint32 = 63
                                var t1065 uint32 = second__10 & t1065_rhs
                                var t1066_rhs int = 6
                                var t1066 uint32 = t1065 << t1066_rhs
                                var t1067 uint32 = t1064 | t1066
                                var t1068_rhs uint32 = 63
                                var t1068 uint32 = third__11 & t1068_rhs
                                var t1069 uint32 = t1067 | t1068
                                var t1070 Tuple3_4bool_4char_3int = utf8_valid_decode(t1069, 3)
                                return t1070
                            }
                        }
                    } else {
                        var t1090 bool = first__8 < 245
                        if t1090 {
                            var t1131 int = length__7 - index__6
                            var t1132 bool = t1131 < 4
                            if t1132 {
                                var t1133 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1133
                            } else {
                                var t1092 int = index__6 + 1
                                var t1093 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1092)
                                var second__12 uint32 = uint32(uint8(t1093))
                                var t1094 int = index__6 + 2
                                var t1095 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1094)
                                var third__13 uint32 = uint32(uint8(t1095))
                                var t1096 int = index__6 + 3
                                var t1097 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1096)
                                var fourth__14 uint32 = uint32(uint8(t1097))
                                var t1129 bool = utf8_invalid_continuation(second__12)
                                var jp1127 bool
                                if t1129 {
                                    jp1127 = true
                                } else {
                                    var t1130 bool = utf8_invalid_continuation(third__13)
                                    jp1127 = t1130
                                }
                                var jp1121 bool
                                if jp1127 {
                                    jp1121 = true
                                } else {
                                    var t1128 bool = utf8_invalid_continuation(fourth__14)
                                    jp1121 = t1128
                                }
                                var jp1115 bool
                                if jp1121 {
                                    jp1115 = true
                                } else {
                                    var t1124 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t1124 {
                                        var t1125 bool = second__12 < 144
                                        jp1115 = t1125
                                    } else {
                                        jp1115 = false
                                    }
                                }
                                var jp1101 bool
                                if jp1115 {
                                    jp1101 = true
                                } else {
                                    var t1118 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t1118 {
                                        var t1119 bool = second__12 > 143
                                        jp1101 = t1119
                                    } else {
                                        jp1101 = false
                                    }
                                }
                                if jp1101 {
                                    var t1102 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1102
                                } else {
                                    var t1103_rhs uint32 = 7
                                    var t1103 uint32 = first__8 & t1103_rhs
                                    var t1104_rhs int = 18
                                    var t1104 uint32 = t1103 << t1104_rhs
                                    var t1105_rhs uint32 = 63
                                    var t1105 uint32 = second__12 & t1105_rhs
                                    var t1106_rhs int = 12
                                    var t1106 uint32 = t1105 << t1106_rhs
                                    var t1107 uint32 = t1104 | t1106
                                    var t1108_rhs uint32 = 63
                                    var t1108 uint32 = third__13 & t1108_rhs
                                    var t1109_rhs int = 6
                                    var t1109 uint32 = t1108 << t1109_rhs
                                    var t1110 uint32 = t1107 | t1109
                                    var t1111_rhs uint32 = 63
                                    var t1111 uint32 = fourth__14 & t1111_rhs
                                    var t1112 uint32 = t1110 | t1111
                                    var t1113 Tuple3_4bool_4char_3int = utf8_valid_decode(t1112, 4)
                                    return t1113
                                }
                            }
                        } else {
                            var t1134 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1134
                        }
                    }
                }
            }
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1144 bool = value__4 <= 1114111
    if t1144 {
        var t1148 bool = value__4 >= 55296
        var jp1146 bool
        if t1148 {
            var t1149 bool = value__4 <= 57343
            jp1146 = t1149
        } else {
            jp1146 = false
        }
        var t1147 bool = !jp1146
        return t1147
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t1152 int = _goml_runtime_core_string_len(self__38)
    return t1152
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1155 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1155
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t1158 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t1158
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1847 rune
    var inline1833 bool = utf8_valid_scalar(value__0)
    if inline1833 {
        var inline1834 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1836 rune = inline1834._1
        commute_field1847 = inline1836
        var t1164 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1847,
            _2: width__1,
        }
        return t1164
    } else {
        var inline1831 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1831
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1169 bool = value__3 < 128
    if t1169 {
        return true
    } else {
        var t1170 bool = value__3 > 191
        return t1170
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t1173 bool = self__102 == other__103
    return t1173
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1178 bool
    var inline1840 bool = value__32 <= 1114111
    if inline1840 {
        var inline1841 bool = value__32 >= 55296
        var inline1843 bool
        if inline1841 {
            var inline1845 bool = value__32 <= 57343
            inline1843 = inline1845
        } else {
            inline1843 = false
        }
        var inline1844 bool = !inline1843
        t1178 = inline1844
    } else {
        t1178 = false
    }
    if t1178 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1179 Option__char = Some{
            _0: x24,
        }
        return t1179
    } else {
        return None{}
    }
}

func main() {
    main0()
}
