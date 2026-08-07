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
    var t322 bool
    var inline1271 int = 0
    var inline1272 bool = len__3 == inline1271
    t322 = inline1272
    if t322 {
        return false
    } else {
        var i__4 *ref_int_x
        var inline1268 int = 0
        var inline1269 *ref_int_x = ref__Ref_3int(inline1268)
        i__4 = inline1269
        var saw_digit__5 *ref_bool_x
        var inline1265 bool = false
        var inline1266 *ref_bool_x = ref__Ref_4bool(inline1265)
        saw_digit__5 = inline1266
        var ok__6 *ref_bool_x
        var inline1262 bool = true
        var inline1263 *ref_bool_x = ref__Ref_4bool(inline1262)
        ok__6 = inline1263
        var started__7 *ref_bool_x
        var inline1259 bool = false
        var inline1260 *ref_bool_x = ref__Ref_4bool(inline1259)
        started__7 = inline1260
        Loop_loop328:
        for {
            var t347 bool
            var inline1253 bool = ref_get__Ref_4bool(ok__6)
            t347 = inline1253
            var jp330 bool
            if t347 {
                var t348 int
                var inline1219 int = ref_get__Ref_3int(i__4)
                t348 = inline1219
                var t349 bool = t348 < len__3
                jp330 = t349
            } else {
                jp330 = false
            }
            if jp330 {
                var t331 int
                var inline1251 int = ref_get__Ref_3int(i__4)
                t331 = inline1251
                var ch__8 rune
                var inline1249 rune = string_get(text__2, t331)
                ch__8 = inline1249
                var t344 bool
                var inline1247 bool = ref_get__Ref_4bool(started__7)
                t344 = inline1247
                var t345 bool = !t344
                var jp334 bool
                if t345 {
                    var inline1221 rune = 45
                    var inline1222 bool = ch__8 == inline1221
                    jp334 = inline1222
                } else {
                    jp334 = false
                }
                if jp334 {
                    var inline1228 bool = true
                    ref_set__Ref_4bool(started__7, inline1228)
                    var t335 int
                    var inline1226 int = ref_get__Ref_3int(i__4)
                    t335 = inline1226
                    var t336 int = t335 + 1
                    ref_set__Ref_3int(i__4, t336)
                    continue
                } else {
                    var t339 bool
                    var inline1244 bool = ch__8 >= 48
                    if inline1244 {
                        var inline1245 bool = ch__8 <= 57
                        t339 = inline1245
                    } else {
                        t339 = false
                    }
                    if t339 {
                        var inline1238 bool = true
                        ref_set__Ref_4bool(started__7, inline1238)
                        var inline1235 bool = true
                        ref_set__Ref_4bool(saw_digit__5, inline1235)
                        var t340 int
                        var inline1233 int = ref_get__Ref_3int(i__4)
                        t340 = inline1233
                        var t341 int = t340 + 1
                        ref_set__Ref_3int(i__4, t341)
                        continue
                    } else {
                        var inline1241 bool = false
                        ref_set__Ref_4bool(ok__6, inline1241)
                        continue
                    }
                }
            } else {
                break Loop_loop328
            }
        }
        var t326 bool
        var inline1257 bool = ref_get__Ref_4bool(ok__6)
        t326 = inline1257
        if t326 {
            var inline1255 bool = ref_get__Ref_4bool(saw_digit__5)
            return inline1255
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
    var inline1313 int32 = 0
    var inline1314 *ref_int32_x = ref__Ref_5int32(inline1313)
    acc__14 = inline1314
    Loop_loop359:
    for {
        var t360 int
        var inline1305 int = ref_get__Ref_3int(i__11)
        t360 = inline1305
        var t361 bool = t360 < len__10
        if t361 {
            var t362 int
            var inline1303 int = ref_get__Ref_3int(i__11)
            t362 = inline1303
            var ch__15 rune
            var inline1301 rune = string_get(text__9, t362)
            ch__15 = inline1301
            var t375 bool
            var inline1299 bool = ref_get__Ref_4bool(started__13)
            t375 = inline1299
            var t376 bool = !t375
            var jp365 bool
            if t376 {
                var inline1274 rune = 45
                var inline1275 bool = ch__15 == inline1274
                jp365 = inline1275
            } else {
                jp365 = false
            }
            if jp365 {
                var inline1284 bool = true
                ref_set__Ref_4bool(started__13, inline1284)
                var inline1281 bool = true
                ref_set__Ref_4bool(negative__12, inline1281)
                var t366 int
                var inline1279 int = ref_get__Ref_3int(i__11)
                t366 = inline1279
                var t367 int = t366 + 1
                ref_set__Ref_3int(i__11, t367)
                continue
            } else {
                var inline1296 bool = true
                ref_set__Ref_4bool(started__13, inline1296)
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
                var t369 int32
                var inline1293 int32 = ref_get__Ref_5int32(acc__14)
                t369 = inline1293
                var t370 int32 = t369 * 10
                var t371 int32 = t370 + d__16
                ref_set__Ref_5int32(acc__14, t371)
                var t372 int
                var inline1289 int = ref_get__Ref_3int(i__11)
                t372 = inline1289
                var t373 int = t372 + 1
                ref_set__Ref_3int(i__11, t373)
                continue
            }
        } else {
            break Loop_loop359
        }
    }
    var t355 bool
    var inline1311 bool = ref_get__Ref_4bool(negative__12)
    t355 = inline1311
    if t355 {
        var t356 int32
        var inline1307 int32 = ref_get__Ref_5int32(acc__14)
        t356 = inline1307
        var t357 int32 = 0 - t356
        return t357
    } else {
        var inline1309 int32 = ref_get__Ref_5int32(acc__14)
        return inline1309
    }
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var len__20 int
    var inline1364 int = _goml_runtime_core_string_len(source__18)
    len__20 = inline1364
    var text__21 *ref_string_x
    var inline1361 string = ""
    var inline1362 *ref_string_x = ref__Ref_6string(inline1361)
    text__21 = inline1362
    var i__22 *ref_int_x
    var inline1359 *ref_int_x = ref__Ref_3int(start__19)
    i__22 = inline1359
    var done__23 *ref_bool_x
    var inline1356 bool = false
    var inline1357 *ref_bool_x = ref__Ref_4bool(inline1356)
    done__23 = inline1357
    Loop_loop402:
    for {
        var t415 bool
        var inline1350 bool = ref_get__Ref_4bool(done__23)
        t415 = inline1350
        var t416 bool = !t415
        var jp404 bool
        if t416 {
            var t417 int
            var inline1325 int = ref_get__Ref_3int(i__22)
            t417 = inline1325
            var t418 bool = t417 < len__20
            jp404 = t418
        } else {
            jp404 = false
        }
        if jp404 {
            var t405 int
            var inline1348 int = ref_get__Ref_3int(i__22)
            t405 = inline1348
            var ch__24 rune
            var inline1346 rune = string_get(source__18, t405)
            ch__24 = inline1346
            var t407 bool
            var inline1340 bool = _goml_m_trait__impl_i_PartialEq_i_char_i_eq(ch__24, 40)
            var inline1342 bool
            if inline1340 {
                inline1342 = true
            } else {
                var inline1344 bool = _goml_m_trait__impl_i_PartialEq_i_char_i_eq(ch__24, 41)
                inline1342 = inline1344
            }
            if inline1342 {
                t407 = true
                if t407 {
                    var inline1327 bool = true
                    ref_set__Ref_4bool(done__23, inline1327)
                    continue
                } else {
                    var t409 string
                    var inline1338 string = ref_get__Ref_6string(text__21)
                    t409 = inline1338
                    var t410 string
                    var inline1336 string = char_to_string(ch__24)
                    t410 = inline1336
                    var t411 string = t409 + t410
                    ref_set__Ref_6string(text__21, t411)
                    var t412 int
                    var inline1332 int = ref_get__Ref_3int(i__22)
                    t412 = inline1332
                    var t413 int = t412 + 1
                    ref_set__Ref_3int(i__22, t413)
                    continue
                }
            } else {
                var inline1343 bool = _goml_m_trait__impl_i_PartialEq_i_char_i_eq(ch__24, 32)
                t407 = inline1343
                if t407 {
                    var inline1327 bool = true
                    ref_set__Ref_4bool(done__23, inline1327)
                    continue
                } else {
                    var t409 string
                    var inline1338 string = ref_get__Ref_6string(text__21)
                    t409 = inline1338
                    var t410 string
                    var inline1336 string = char_to_string(ch__24)
                    t410 = inline1336
                    var t411 string = t409 + t410
                    ref_set__Ref_6string(text__21, t411)
                    var t412 int
                    var inline1332 int = ref_get__Ref_3int(i__22)
                    t412 = inline1332
                    var t413 int = t412 + 1
                    ref_set__Ref_3int(i__22, t413)
                    continue
                }
            }
        } else {
            break Loop_loop402
        }
    }
    var atom__25 string
    var inline1354 string = ref_get__Ref_6string(text__21)
    atom__25 = inline1354
    var jp391 Token
    switch atom__25 {
    case "true":
        var t394 Token = Token_Bool{
            _0: true,
        }
        jp391 = t394
    case "false":
        var t395 Token = Token_Bool{
            _0: false,
        }
        jp391 = t395
    default:
        var t398 bool = is_int_text(atom__25)
        if t398 {
            var t399 int32 = parse_int32(atom__25)
            var t400 Token = Token_Int{
                _0: t399,
            }
            jp391 = t400
        } else {
            var t401 Token = Token_Sym{
                _0: atom__25,
            }
            jp391 = t401
        }
    }
    var t392 int
    var inline1352 int = ref_get__Ref_3int(i__22)
    t392 = inline1352
    var t393 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: jp391,
        _1: t392,
    }
    return t393
}

func lex(source__27 string) *_goml_vec_Token {
    var len__28 int = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token
    var inline1416 *_goml_vec_Token = vec_new__Vec_5Token()
    toks0__29 = inline1416
    var toks__30 *ref_Vec_5Token_x
    var inline1414 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    toks__30 = inline1414
    var i__31 *ref_int_x
    var inline1411 int = 0
    var inline1412 *ref_int_x = ref__Ref_3int(inline1411)
    i__31 = inline1412
    Loop_loop423:
    for {
        var t424 int
        var inline1407 int = ref_get__Ref_3int(i__31)
        t424 = inline1407
        var t425 bool = t424 < len__28
        if t425 {
            var t426 int
            var inline1405 int = ref_get__Ref_3int(i__31)
            t426 = inline1405
            var ch__32 rune
            var inline1403 rune = string_get(source__27, t426)
            ch__32 = inline1403
            var t428 bool
            var inline1400 rune = 40
            var inline1401 bool = ch__32 == inline1400
            t428 = inline1401
            if t428 {
                var t429 *_goml_vec_Token
                var inline1372 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                t429 = inline1372
                var t430 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t429, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t430)
                var t431 int
                var inline1368 int = ref_get__Ref_3int(i__31)
                t431 = inline1368
                var t432 int = t431 + 1
                ref_set__Ref_3int(i__31, t432)
                continue
            } else {
                var t435 bool
                var inline1397 rune = 41
                var inline1398 bool = ch__32 == inline1397
                t435 = inline1398
                if t435 {
                    var t436 *_goml_vec_Token
                    var inline1380 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                    t436 = inline1380
                    var t437 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t436, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t437)
                    var t438 int
                    var inline1376 int = ref_get__Ref_3int(i__31)
                    t438 = inline1376
                    var t439 int = t438 + 1
                    ref_set__Ref_3int(i__31, t439)
                    continue
                } else {
                    var t442 bool
                    var inline1394 rune = 32
                    var inline1395 bool = ch__32 == inline1394
                    t442 = inline1395
                    if t442 {
                        var t443 int
                        var inline1384 int = ref_get__Ref_3int(i__31)
                        t443 = inline1384
                        var t444 int = t443 + 1
                        ref_set__Ref_3int(i__31, t444)
                        continue
                    } else {
                        var t446 int
                        var inline1392 int = ref_get__Ref_3int(i__31)
                        t446 = inline1392
                        var mtmp185 Tuple2_5Token_3int = lex_atom(source__27, t446)
                        var x186 Token = mtmp185._0
                        var x187 int = mtmp185._1
                        var t447 *_goml_vec_Token
                        var inline1390 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                        t447 = inline1390
                        var t448 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t447, x186)
                        ref_set__Ref_10Vec_5Token(toks__30, t448)
                        ref_set__Ref_3int(i__31, x187)
                        continue
                    }
                }
            }
        } else {
            break Loop_loop423
        }
    }
    var inline1409 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
    return inline1409
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var t452 int
    var inline1444 int = vec_len__Vec_7Binding(env__35)
    t452 = inline1444
    var t453 int = t452 - 1
    var i__37 *ref_int_x
    var inline1442 *ref_int_x = ref__Ref_3int(t453)
    i__37 = inline1442
    var result__38 *ref_Value_x
    var inline1440 *ref_Value_x = ref__Ref_5Value(Nil{})
    result__38 = inline1440
    var done__39 *ref_bool_x
    var inline1437 bool = false
    var inline1438 *ref_bool_x = ref__Ref_4bool(inline1437)
    done__39 = inline1438
    Loop_loop456:
    for {
        var t468 bool
        var inline1433 bool = ref_get__Ref_4bool(done__39)
        t468 = inline1433
        var t469 bool = !t468
        var jp458 bool
        if t469 {
            var t470 int
            var inline1418 int = ref_get__Ref_3int(i__37)
            t470 = inline1418
            var t471 bool = t470 >= 0
            jp458 = t471
        } else {
            jp458 = false
        }
        if jp458 {
            var t459 int
            var inline1431 int = ref_get__Ref_3int(i__37)
            t459 = inline1431
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t459)
            var t461 string = binding__40.name
            var t462 bool
            var inline1429 bool = t461 == name__36
            t462 = inline1429
            if t462 {
                var t463 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t463)
                var inline1420 bool = true
                ref_set__Ref_4bool(done__39, inline1420)
                continue
            } else {
                var t465 int
                var inline1427 int = ref_get__Ref_3int(i__37)
                t465 = inline1427
                var t466 int = t465 - 1
                ref_set__Ref_3int(i__37, t466)
                continue
            }
        } else {
            break Loop_loop456
        }
    }
    var inline1435 Value = ref_get__Ref_5Value(result__38)
    return inline1435
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var acc__47 *_goml_vec_SExpr
    var inline1480 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__47 = inline1480
    var exprs__48 *ref_Vec_5SExpr_x
    var inline1478 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    exprs__48 = inline1478
    var i__49 *ref_int_x
    var inline1476 *ref_int_x = ref__Ref_3int(start__46)
    i__49 = inline1476
    var done__50 *ref_bool_x
    var inline1473 bool = false
    var inline1474 *ref_bool_x = ref__Ref_4bool(inline1473)
    done__50 = inline1474
    Loop_loop483:
    for {
        var t495 bool
        var inline1467 bool = ref_get__Ref_4bool(done__50)
        t495 = inline1467
        var t496 bool = !t495
        var jp485 bool
        if t496 {
            var t497 int
            var inline1448 int = ref_get__Ref_3int(i__49)
            t497 = inline1448
            var t498 int
            var inline1446 int = vec_len__Vec_5Token(tokens__45)
            t498 = inline1446
            var t499 bool = t497 < t498
            jp485 = t499
        } else {
            jp485 = false
        }
        if jp485 {
            var t486 int
            var inline1465 int = ref_get__Ref_3int(i__49)
            t486 = inline1465
            var mtmp196 Token = vec_get__Vec_5Token(tokens__45, t486)
            switch mtmp196.(type) {
            case RParen:
                var inline1454 bool = true
                ref_set__Ref_4bool(done__50, inline1454)
                var t488 int
                var inline1452 int = ref_get__Ref_3int(i__49)
                t488 = inline1452
                var t489 int = t488 + 1
                ref_set__Ref_3int(i__49, t489)
                continue
            default:
                var t491 int
                var inline1463 int = ref_get__Ref_3int(i__49)
                t491 = inline1463
                var mtmp201 Tuple2_5SExpr_3int = parse_expr(tokens__45, t491)
                var x202 SExpr = mtmp201._0
                var x203 int = mtmp201._1
                var t492 *_goml_vec_SExpr
                var inline1461 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                t492 = inline1461
                var t493 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t492, x202)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t493)
                ref_set__Ref_3int(i__49, x203)
                continue
            }
        } else {
            break Loop_loop483
        }
    }
    var t480 *_goml_vec_SExpr
    var inline1471 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    t480 = inline1471
    var t481 int
    var inline1469 int = ref_get__Ref_3int(i__49)
    t481 = inline1469
    var t482 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t480,
        _1: t481,
    }
    return t482
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var mtmp206 Token = vec_get__Vec_5Token(tokens__53, start__54)
    switch mtmp206.(type) {
    case LParen:
        var t504 int = start__54 + 1
        var mtmp210 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t504)
        var x211 *_goml_vec_SExpr = mtmp210._0
        var x212 int = mtmp210._1
        var t505 SExpr = List{
            _0: x211,
        }
        var t506 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t505,
            _1: x212,
        }
        return t506
    case RParen:
        var t507 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t508 int = start__54 + 1
        var t509 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t507,
            _1: t508,
        }
        return t509
    case Token_Sym:
        var x207 string = mtmp206.(Token_Sym)._0
        var t510 SExpr = SExpr_Sym{
            _0: x207,
        }
        var t511 int = start__54 + 1
        var t512 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t510,
            _1: t511,
        }
        return t512
    case Token_Int:
        var x208 int32 = mtmp206.(Token_Int)._0
        var t513 SExpr = SExpr_Int{
            _0: x208,
        }
        var t514 int = start__54 + 1
        var t515 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t513,
            _1: t514,
        }
        return t515
    case Token_Bool:
        var x209 bool = mtmp206.(Token_Bool)._0
        var t516 SExpr = SExpr_Bool{
            _0: x209,
        }
        var t517 int = start__54 + 1
        var t518 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t516,
            _1: t517,
        }
        return t518
    default:
        panic("non-exhaustive match")
    }
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var i__61 *ref_int_x
    var inline1500 int = 0
    var inline1501 *ref_int_x = ref__Ref_3int(inline1500)
    i__61 = inline1501
    var acc__62 *_goml_vec_SExpr
    var inline1498 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__62 = inline1498
    var exprs__63 *ref_Vec_5SExpr_x
    var inline1496 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    exprs__63 = inline1496
    Loop_loop523:
    for {
        var t524 int
        var inline1492 int = ref_get__Ref_3int(i__61)
        t524 = inline1492
        var t525 int
        var inline1490 int = vec_len__Vec_5Token(tokens__60)
        t525 = inline1490
        var t526 bool = t524 < t525
        if t526 {
            var t527 int
            var inline1488 int = ref_get__Ref_3int(i__61)
            t527 = inline1488
            var mtmp213 Tuple2_5SExpr_3int = parse_expr(tokens__60, t527)
            var x214 SExpr = mtmp213._0
            var x215 int = mtmp213._1
            var t528 *_goml_vec_SExpr
            var inline1486 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            t528 = inline1486
            var t529 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t528, x214)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t529)
            ref_set__Ref_3int(i__61, x215)
            continue
        } else {
            break Loop_loop523
        }
    }
    var inline1494 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    return inline1494
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    switch expr__72.(type) {
    case SExpr_Int:
        var x224 int32 = expr__72.(SExpr_Int)._0
        var t547 Value = Value_Int{
            _0: x224,
        }
        return t547
    case SExpr_Bool:
        var x225 bool = expr__72.(SExpr_Bool)._0
        var t548 Value = Value_Bool{
            _0: x225,
        }
        return t548
    case SExpr_Sym:
        var x226 string = expr__72.(SExpr_Sym)._0
        var t549 *_goml_vec_Binding
        var inline1514 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__74)
        t549 = inline1514
        var inline1510 Value = env_lookup(local__73, x226)
        switch inline1510.(type) {
        case Nil:
            var inline1511 Value = env_lookup(t549, x226)
            return inline1511
        default:
            return inline1510
        }
    case List:
        var x227 *_goml_vec_SExpr = expr__72.(List)._0
        var inline1516 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(x227)
        var inline1517 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(inline1516, 0)
        if inline1517 {
            return Nil{}
        } else {
            var inline1518 SExpr = vec_get__Vec_5SExpr(x227, 0)
            switch inline1518.(type) {
            case SExpr_Sym:
                var inline1519 string = inline1518.(SExpr_Sym)._0
                var inline1521 Value = eval_list_sym(inline1519, x227, local__73, global__74)
                return inline1521
            default:
                var inline1522 Value = eval(inline1518, local__73, global__74)
                var inline1523 *_goml_vec_Value = eval_args(x227, 1, local__73, global__74)
                var inline1524 Value = apply(inline1522, inline1523, global__74)
                return inline1524
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    switch name__86 {
    case "begin":
        var t566 Value = eval_begin(items__87, 1, local__88, global__89)
        return t566
    case "define":
        var t569 int
        var inline1542 int = vec_len__Vec_5SExpr(items__87)
        t569 = inline1542
        var t570 bool
        var inline1539 int = 3
        var inline1540 bool = t569 == inline1539
        t570 = inline1540
        if t570 {
            var mtmp232 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp232.(type) {
            case SExpr_Sym:
                var x235 string = mtmp232.(SExpr_Sym)._0
                var t573 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t573, local__88, global__89)
                var env__92 *_goml_vec_Binding
                var inline1537 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__89)
                env__92 = inline1537
                var t574 Binding = Binding{
                    name: x235,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t574)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                return value__91
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "if":
        var t577 int
        var inline1555 int = vec_len__Vec_5SExpr(items__87)
        t577 = inline1555
        var t578 bool
        var inline1552 int = 4
        var inline1553 bool = t577 == inline1552
        t578 = inline1553
        if t578 {
            var t579 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t579, local__88, global__89)
            var t582 bool
            switch cond__94.(type) {
            case Value_Int:
                var inline1544 int32 = cond__94.(Value_Int)._0
                var inline1546 bool = _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(inline1544, 0)
                var inline1547 bool = !inline1546
                t582 = inline1547
            case Value_Bool:
                var inline1548 bool = cond__94.(Value_Bool)._0
                t582 = inline1548
            case Func:
                t582 = true
            case Nil:
                t582 = false
            default:
                panic("non-exhaustive match")
            }
            if t582 {
                var t583 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t584 Value = eval(t583, local__88, global__89)
                return t584
            } else {
                var t585 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t586 Value = eval(t585, local__88, global__89)
                return t586
            }
        } else {
            return Nil{}
        }
    case "lambda":
        var t589 int
        var inline1560 int = vec_len__Vec_5SExpr(items__87)
        t589 = inline1560
        var t590 bool
        var inline1557 int = 3
        var inline1558 bool = t589 == inline1557
        t590 = inline1558
        if t590 {
            var mtmp238 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp238.(type) {
            case List:
                var x242 *_goml_vec_SExpr = mtmp238.(List)._0
                var params__96 *_goml_vec_string = params_from_sexprs(x242)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t593 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t594 Value = Func{
                    _0: t593,
                }
                return t594
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "+":
        var t595 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t596 Value = apply_builtin("+", t595)
        return t596
    case "-":
        var t597 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t598 Value = apply_builtin("-", t597)
        return t598
    case "*":
        var t599 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t600 Value = apply_builtin("*", t599)
        return t600
    case "/":
        var t601 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t602 Value = apply_builtin("/", t601)
        return t602
    case "=":
        var t603 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t604 Value = apply_builtin("=", t603)
        return t604
    default:
        var t605 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t605, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        switch f__98.(type) {
        case Func:
            var inline1562 Lambda = f__98.(Func)._0
            var inline1564 Value = apply_lambda(inline1562, args__99)
            return inline1564
        default:
            return Nil{}
        }
    }
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var i__104 *ref_int_x
    var inline1582 *ref_int_x = ref__Ref_3int(start__101)
    i__104 = inline1582
    var last__105 *ref_Value_x
    var inline1580 *ref_Value_x = ref__Ref_5Value(Nil{})
    last__105 = inline1580
    Loop_loop611:
    for {
        var t612 int
        var inline1576 int = ref_get__Ref_3int(i__104)
        t612 = inline1576
        var t613 int
        var inline1574 int = vec_len__Vec_5SExpr(items__100)
        t613 = inline1574
        var t614 bool = t612 < t613
        if t614 {
            var t615 int
            var inline1572 int = ref_get__Ref_3int(i__104)
            t615 = inline1572
            var t616 SExpr = vec_get__Vec_5SExpr(items__100, t615)
            var v__106 Value = eval(t616, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t617 int
            var inline1568 int = ref_get__Ref_3int(i__104)
            t617 = inline1568
            var t618 int = t617 + 1
            ref_set__Ref_3int(i__104, t618)
            continue
        } else {
            break Loop_loop611
        }
    }
    var inline1578 Value = ref_get__Ref_5Value(last__105)
    return inline1578
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var i__108 *ref_int_x
    var inline1608 int = 0
    var inline1609 *ref_int_x = ref__Ref_3int(inline1608)
    i__108 = inline1609
    var acc__109 *_goml_vec_string
    var inline1606 *_goml_vec_string = vec_new__Vec_6string()
    acc__109 = inline1606
    var params__110 *ref_Vec_6string_x
    var inline1604 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    params__110 = inline1604
    Loop_loop624:
    for {
        var t625 int
        var inline1600 int = ref_get__Ref_3int(i__108)
        t625 = inline1600
        var t626 int
        var inline1598 int = vec_len__Vec_5SExpr(items__107)
        t626 = inline1598
        var t627 bool = t625 < t626
        if t627 {
            var t628 int
            var inline1596 int = ref_get__Ref_3int(i__108)
            t628 = inline1596
            var mtmp245 SExpr = vec_get__Vec_5SExpr(items__107, t628)
            switch mtmp245.(type) {
            case SExpr_Sym:
                var x248 string = mtmp245.(SExpr_Sym)._0
                var t630 *_goml_vec_string
                var inline1590 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
                t630 = inline1590
                var t631 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t630, x248)
                ref_set__Ref_11Vec_6string(params__110, t631)
                var t632 int
                var inline1586 int = ref_get__Ref_3int(i__108)
                t632 = inline1586
                var t633 int = t632 + 1
                ref_set__Ref_3int(i__108, t633)
                continue
            default:
                var t635 int
                var inline1594 int = ref_get__Ref_3int(i__108)
                t635 = inline1594
                var t636 int = t635 + 1
                ref_set__Ref_3int(i__108, t636)
                continue
            }
        } else {
            break Loop_loop624
        }
    }
    var inline1602 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
    return inline1602
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var i__116 *ref_int_x
    var inline1631 *ref_int_x = ref__Ref_3int(start__113)
    i__116 = inline1631
    var acc__117 *_goml_vec_Value
    var inline1629 *_goml_vec_Value = vec_new__Vec_5Value()
    acc__117 = inline1629
    var args__118 *ref_Vec_5Value_x
    var inline1627 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    args__118 = inline1627
    Loop_loop642:
    for {
        var t643 int
        var inline1623 int = ref_get__Ref_3int(i__116)
        t643 = inline1623
        var t644 int
        var inline1621 int = vec_len__Vec_5SExpr(items__112)
        t644 = inline1621
        var t645 bool = t643 < t644
        if t645 {
            var t646 int
            var inline1619 int = ref_get__Ref_3int(i__116)
            t646 = inline1619
            var t647 SExpr = vec_get__Vec_5SExpr(items__112, t646)
            var v__119 Value = eval(t647, local__114, global__115)
            var t648 *_goml_vec_Value
            var inline1617 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
            t648 = inline1617
            var t649 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t648, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t649)
            var t650 int
            var inline1613 int = ref_get__Ref_3int(i__116)
            t650 = inline1613
            var t651 int = t650 + 1
            ref_set__Ref_3int(i__116, t651)
            continue
        } else {
            break Loop_loop642
        }
    }
    var inline1625 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
    return inline1625
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    switch name__120 {
    case "=":
        var t659 int
        var inline1640 int = vec_len__Vec_5Value(args__121)
        t659 = inline1640
        var t660 bool
        var inline1637 int = 2
        var inline1638 bool = t659 == inline1637
        t660 = inline1638
        if t660 {
            var t661 Value = vec_get__Vec_5Value(args__121, 0)
            var t662 Value = vec_get__Vec_5Value(args__121, 1)
            switch t662.(type) {
            case Value_Int:
                var x257 int32 = t662.(Value_Int)._0
                switch t661.(type) {
                case Value_Int:
                    var x260 int32 = t661.(Value_Int)._0
                    var t667 bool
                    var inline1633 bool = x260 == x257
                    t667 = inline1633
                    var t668 Value = Value_Bool{
                        _0: t667,
                    }
                    return t668
                default:
                    var t669 Value = Value_Bool{
                        _0: false,
                    }
                    return t669
                }
            case Value_Bool:
                var x258 bool = t662.(Value_Bool)._0
                switch t661.(type) {
                case Value_Bool:
                    var x264 bool = t661.(Value_Bool)._0
                    var t672 bool
                    var inline1635 bool = x264 == x258
                    t672 = inline1635
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
            default:
                var t675 Value = Value_Bool{
                    _0: false,
                }
                return t675
            }
        } else {
            var t676 Value = Value_Bool{
                _0: false,
            }
            return t676
        }
    case "+":
        var i__126 *ref_int_x
        var inline1665 int = 0
        var inline1666 *ref_int_x = ref__Ref_3int(inline1665)
        i__126 = inline1666
        var acc__127 *ref_int32_x
        var inline1662 int32 = 0
        var inline1663 *ref_int32_x = ref__Ref_5int32(inline1662)
        acc__127 = inline1663
        Loop_loop680:
        for {
            var t681 int
            var inline1658 int = ref_get__Ref_3int(i__126)
            t681 = inline1658
            var t682 int
            var inline1656 int = vec_len__Vec_5Value(args__121)
            t682 = inline1656
            var t683 bool = t681 < t682
            if t683 {
                var t684 int
                var inline1654 int = ref_get__Ref_3int(i__126)
                t684 = inline1654
                var mtmp266 Value = vec_get__Vec_5Value(args__121, t684)
                switch mtmp266.(type) {
                case Value_Int:
                    var x267 int32 = mtmp266.(Value_Int)._0
                    var t686 int32
                    var inline1648 int32 = ref_get__Ref_5int32(acc__127)
                    t686 = inline1648
                    var t687 int32 = t686 + x267
                    ref_set__Ref_5int32(acc__127, t687)
                    var t688 int
                    var inline1644 int = ref_get__Ref_3int(i__126)
                    t688 = inline1644
                    var t689 int = t688 + 1
                    ref_set__Ref_3int(i__126, t689)
                    continue
                default:
                    var t691 int
                    var inline1652 int = ref_get__Ref_3int(i__126)
                    t691 = inline1652
                    var t692 int = t691 + 1
                    ref_set__Ref_3int(i__126, t692)
                    continue
                }
            } else {
                break Loop_loop680
            }
        }
        var t678 int32
        var inline1660 int32 = ref_get__Ref_5int32(acc__127)
        t678 = inline1660
        var t679 Value = Value_Int{
            _0: t678,
        }
        return t679
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop697:
        for {
            var t698 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
            var t699 int
            var inline1682 int = vec_len__Vec_5Value(args__121)
            t699 = inline1682
            var t700 bool = t698 < t699
            if t700 {
                var t701 int
                var inline1680 int = ref_get__Ref_3int(i__129)
                t701 = inline1680
                var mtmp272 Value = vec_get__Vec_5Value(args__121, t701)
                switch mtmp272.(type) {
                case Value_Int:
                    var x273 int32 = mtmp272.(Value_Int)._0
                    var t703 int32
                    var inline1674 int32 = ref_get__Ref_5int32(acc__130)
                    t703 = inline1674
                    var t704 int32 = t703 * x273
                    ref_set__Ref_5int32(acc__130, t704)
                    var t705 int
                    var inline1670 int = ref_get__Ref_3int(i__129)
                    t705 = inline1670
                    var t706 int = t705 + 1
                    ref_set__Ref_3int(i__129, t706)
                    continue
                default:
                    var t708 int
                    var inline1678 int = ref_get__Ref_3int(i__129)
                    t708 = inline1678
                    var t709 int = t708 + 1
                    ref_set__Ref_3int(i__129, t709)
                    continue
                }
            } else {
                break Loop_loop697
            }
        }
        var t695 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t696 Value = Value_Int{
            _0: t695,
        }
        return t696
    case "-":
        var mtmp278 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        switch mtmp278 {
        case 1:
            var mtmp279 Value = vec_get__Vec_5Value(args__121, 0)
            switch mtmp279.(type) {
            case Value_Int:
                var x280 int32 = mtmp279.(Value_Int)._0
                var t715 int32 = 0 - x280
                var t716 Value = Value_Int{
                    _0: t715,
                }
                return t716
            default:
                return Nil{}
            }
        case 2:
            var t717 Value = vec_get__Vec_5Value(args__121, 0)
            var t718 Value = vec_get__Vec_5Value(args__121, 1)
            switch t718.(type) {
            case Value_Int:
                var x286 int32 = t718.(Value_Int)._0
                switch t717.(type) {
                case Value_Int:
                    var x289 int32 = t717.(Value_Int)._0
                    var t723 int32 = x289 - x286
                    var t724 Value = Value_Int{
                        _0: t723,
                    }
                    return t724
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
        var t727 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t728 bool = _goml_m_trait__impl_i_PartialEq_i_int_i_eq(t727, 2)
        if t728 {
            var t729 Value = vec_get__Vec_5Value(args__121, 0)
            var t730 Value = vec_get__Vec_5Value(args__121, 1)
            switch t730.(type) {
            case Value_Int:
                var x295 int32 = t730.(Value_Int)._0
                switch t729.(type) {
                case Value_Int:
                    var x298 int32 = t729.(Value_Int)._0
                    var t735 int32 = x298 / x295
                    var t736 Value = Value_Int{
                        _0: t735,
                    }
                    return t736
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
        var x303 Lambda = func__137.(Func)._0
        var t741 Value = apply_lambda(x303, args__138)
        return t741
    default:
        return Nil{}
    }
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var t744 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x
    var inline1709 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t744)
    env__143 = inline1709
    var i__144 *ref_int_x
    var inline1706 int = 0
    var inline1707 *ref_int_x = ref__Ref_3int(inline1706)
    i__144 = inline1707
    Loop_loop750:
    for {
        var t761 int
        var inline1702 int = ref_get__Ref_3int(i__144)
        t761 = inline1702
        var t762 *_goml_vec_string = lambda__141.params
        var t763 int
        var inline1700 int = vec_len__Vec_6string(t762)
        t763 = inline1700
        var t764 bool = t761 < t763
        var jp752 bool
        if t764 {
            var t765 int
            var inline1686 int = ref_get__Ref_3int(i__144)
            t765 = inline1686
            var t766 int
            var inline1684 int = vec_len__Vec_5Value(args__142)
            t766 = inline1684
            var t767 bool = t765 < t766
            jp752 = t767
        } else {
            jp752 = false
        }
        if jp752 {
            var t753 *_goml_vec_string = lambda__141.params
            var t754 int
            var inline1698 int = ref_get__Ref_3int(i__144)
            t754 = inline1698
            var name__145 string = vec_get__Vec_6string(t753, t754)
            var t755 int
            var inline1696 int = ref_get__Ref_3int(i__144)
            t755 = inline1696
            var value__146 Value = vec_get__Vec_5Value(args__142, t755)
            var t756 *_goml_vec_Binding
            var inline1694 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
            t756 = inline1694
            var t757 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t756, t757)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t758 int
            var inline1690 int = ref_get__Ref_3int(i__144)
            t758 = inline1690
            var t759 int = t758 + 1
            ref_set__Ref_3int(i__144, t759)
            continue
        } else {
            break Loop_loop750
        }
    }
    var t746 SExpr = lambda__141.body
    var t747 *_goml_vec_Binding
    var inline1704 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
    t747 = inline1704
    var t748 *ref_Vec_7Binding_x = lambda__141.global
    var t749 Value = eval(t746, t747, t748)
    return t749
}

func main0() struct{} {
    var t769 *_goml_vec_Binding
    var inline1739 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t769 = inline1739
    var global__148 *ref_Vec_7Binding_x
    var inline1737 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t769)
    global__148 = inline1737
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t770 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t770)
    var t771 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t772 *_goml_vec_Binding
    var inline1735 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t772 = inline1735
    var result__151 Value = eval(t771, t772, global__148)
    var t773 string
    switch result__151.(type) {
    case Value_Int:
        var inline1727 int32 = result__151.(Value_Int)._0
        var inline1729 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1727)
        t773 = inline1729
    case Value_Bool:
        var inline1730 bool = result__151.(Value_Bool)._0
        var inline1732 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1730)
        t773 = inline1732
    case Func:
        t773 = "<lambda>"
    case Nil:
        t773 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1724 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t773)
    _goml_runtime_core_string_println(inline1724)
    var t774 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t774)
    var t775 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t776 *_goml_vec_Binding
    var inline1722 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t776 = inline1722
    var result2__153 Value = eval(t775, t776, global__148)
    var t777 string
    switch result2__153.(type) {
    case Value_Int:
        var inline1714 int32 = result2__153.(Value_Int)._0
        var inline1716 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1714)
        t777 = inline1716
    case Value_Bool:
        var inline1717 bool = result2__153.(Value_Bool)._0
        var inline1719 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1717)
        t777 = inline1719
    case Func:
        t777 = "<lambda>"
    case Nil:
        t777 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1711 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t777)
    _goml_runtime_core_string_println(inline1711)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__37 string) int {
    var t780 int = _goml_runtime_core_string_len(self__37)
    return t780
}

func _goml_m_trait__impl_i_PartialEq_i_int_i_eq(self__103 int, other__104 int) bool {
    var t783 bool = self__103 == other__104
    return t783
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t786 *ref_int_x = ref__Ref_3int(value__257)
    return t786
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__257 bool) *ref_bool_x {
    var t789 *ref_bool_x = ref__Ref_4bool(value__257)
    return t789
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t795 int = ref_get__Ref_3int(self__258)
    return t795
}

func _goml_m_trait__impl_i_PartialEq_i_char_i_eq(self__101 rune, other__102 rune) bool {
    var t801 bool = self__101 == other__102
    return t801
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__257 int32) *ref_int32_x {
    var t808 *ref_int32_x = ref__Ref_5int32(value__257)
    return t808
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__258 *ref_int32_x) int32 {
    var t811 int32 = ref_get__Ref_5int32(self__258)
    return t811
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__178 *_goml_vec_Token, elem__179 Token) *_goml_vec_Token {
    var t836 int
    var inline1762 int = vec_len__Vec_5Token(self__178)
    t836 = inline1762
    var t837 int = t836 + 1
    var result__180 *_goml_vec_Token
    var inline1760 *_goml_vec_Token = vec_with_capacity__Vec_5Token(t837)
    result__180 = inline1760
    var index__181 int = 0
    Loop_loop839:
    for {
        var t840 int
        var inline1756 int = vec_len__Vec_5Token(self__178)
        t840 = inline1756
        var t841 bool = index__181 < t840
        if t841 {
            var t842 Token = vec_get__Vec_5Token(self__178, index__181)
            vec_push__Vec_5Token(result__180, t842)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t843 int = compound_old80 + compound_value81
            index__181 = t843
            continue
        } else {
            break Loop_loop839
        }
    }
    vec_push__Vec_5Token(result__180, elem__179)
    return result__180
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__178 *_goml_vec_SExpr, elem__179 SExpr) *_goml_vec_SExpr {
    var t875 int
    var inline1772 int = vec_len__Vec_5SExpr(self__178)
    t875 = inline1772
    var t876 int = t875 + 1
    var result__180 *_goml_vec_SExpr
    var inline1770 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(t876)
    result__180 = inline1770
    var index__181 int = 0
    Loop_loop878:
    for {
        var t879 int
        var inline1766 int = vec_len__Vec_5SExpr(self__178)
        t879 = inline1766
        var t880 bool = index__181 < t879
        if t880 {
            var t881 SExpr = vec_get__Vec_5SExpr(self__178, index__181)
            vec_push__Vec_5SExpr(result__180, t881)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t882 int = compound_old80 + compound_value81
            index__181 = t882
            continue
        } else {
            break Loop_loop878
        }
    }
    vec_push__Vec_5SExpr(result__180, elem__179)
    return result__180
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t888 string = _goml_runtime_core_int32_to_string(self__35)
    return t888
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t891 string = _goml_runtime_core_bool_to_string(self__66)
    return t891
}

func _goml_m_trait__impl_i_PartialEq_i_int32_i_eq(self__109 int32, other__110 int32) bool {
    var t894 bool = self__109 == other__110
    return t894
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__187 *_goml_vec_SExpr) int {
    var t900 int = vec_len__Vec_5SExpr(self__187)
    return t900
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__178 *_goml_vec_Binding, elem__179 Binding) *_goml_vec_Binding {
    var t903 int
    var inline1782 int = vec_len__Vec_7Binding(self__178)
    t903 = inline1782
    var t904 int = t903 + 1
    var result__180 *_goml_vec_Binding
    var inline1780 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(t904)
    result__180 = inline1780
    var index__181 int = 0
    Loop_loop906:
    for {
        var t907 int
        var inline1776 int = vec_len__Vec_7Binding(self__178)
        t907 = inline1776
        var t908 bool = index__181 < t907
        if t908 {
            var t909 Binding = vec_get__Vec_7Binding(self__178, index__181)
            vec_push__Vec_7Binding(result__180, t909)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t910 int = compound_old80 + compound_value81
            index__181 = t910
            continue
        } else {
            break Loop_loop906
        }
    }
    vec_push__Vec_7Binding(result__180, elem__179)
    return result__180
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__178 *_goml_vec_string, elem__179 string) *_goml_vec_string {
    var t925 int
    var inline1792 int = vec_len__Vec_6string(self__178)
    t925 = inline1792
    var t926 int = t925 + 1
    var result__180 *_goml_vec_string
    var inline1790 *_goml_vec_string = vec_with_capacity__Vec_6string(t926)
    result__180 = inline1790
    var index__181 int = 0
    Loop_loop928:
    for {
        var t929 int
        var inline1786 int = vec_len__Vec_6string(self__178)
        t929 = inline1786
        var t930 bool = index__181 < t929
        if t930 {
            var t931 string = vec_get__Vec_6string(self__178, index__181)
            vec_push__Vec_6string(result__180, t931)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t932 int = compound_old80 + compound_value81
            index__181 = t932
            continue
        } else {
            break Loop_loop928
        }
    }
    vec_push__Vec_6string(result__180, elem__179)
    return result__180
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__178 *_goml_vec_Value, elem__179 Value) *_goml_vec_Value {
    var t947 int
    var inline1802 int = vec_len__Vec_5Value(self__178)
    t947 = inline1802
    var t948 int = t947 + 1
    var result__180 *_goml_vec_Value
    var inline1800 *_goml_vec_Value = vec_with_capacity__Vec_5Value(t948)
    result__180 = inline1800
    var index__181 int = 0
    Loop_loop950:
    for {
        var t951 int
        var inline1796 int = vec_len__Vec_5Value(self__178)
        t951 = inline1796
        var t952 bool = index__181 < t951
        if t952 {
            var t953 Value = vec_get__Vec_5Value(self__178, index__181)
            vec_push__Vec_5Value(result__180, t953)
            var compound_old80 int = index__181
            var compound_value81 int = 1
            var t954 int = compound_old80 + compound_value81
            index__181 = t954
            continue
        } else {
            break Loop_loop950
        }
    }
    vec_push__Vec_5Value(result__180, elem__179)
    return result__180
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__187 *_goml_vec_Value) int {
    var t960 int = vec_len__Vec_5Value(self__187)
    return t960
}

func string_get(value__17 string, index__18 int) rune {
    var mtmp6 Tuple3_4bool_4char_3int = string_decode_utf8_at(value__17, index__18)
    var x7 bool = mtmp6._0
    var x8 rune = mtmp6._1
    if x7 {
        return x8
    } else {
        var t980 rune = _goml_runtime_core_string_get("", -1)
        return t980
    }
}

func char_to_string(value__29 rune) string {
    var t985 uint32 = uint32(rune(value__29))
    var t986 bool
    var inline1805 bool = t985 <= 1114111
    if inline1805 {
        var inline1806 bool = t985 >= 55296
        var inline1808 bool
        if inline1806 {
            var inline1810 bool = t985 <= 57343
            inline1808 = inline1810
        } else {
            inline1808 = false
        }
        var inline1809 bool = !inline1808
        t986 = inline1809
    } else {
        t986 = false
    }
    if t986 {
        var t987 string = _goml_runtime_core_char_to_string(value__29)
        return t987
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
    var t1133 bool = index__6 < 0
    var jp1131 bool
    if t1133 {
        jp1131 = true
    } else {
        var t1134 bool = index__6 >= length__7
        jp1131 = t1134
    }
    if jp1131 {
        var inline1812 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1812
    } else {
        var t1018 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1018))
        var t1021 bool = first__8 < 128
        if t1021 {
            var inline1814 int = 1
            var inline1815 Option__char = char_from_uint32(first__8)
            switch inline1815.(type) {
            case None:
                var inline1816 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1816
            case Some:
                var inline1817 rune = inline1815.(Some)._0
                var inline1819 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1817,
                    _2: inline1814,
                }
                return inline1819
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1025 bool = first__8 < 194
            if t1025 {
                var inline1821 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1821
            } else {
                var t1029 bool = first__8 < 224
                if t1029 {
                    var t1042 int = length__7 - index__6
                    var t1043 bool = t1042 < 2
                    if t1043 {
                        var inline1823 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1823
                    } else {
                        var t1031 int = index__6 + 1
                        var t1032 uint8
                        var inline1837 uint8 = _goml_runtime_core_string_byte_get(value__5, t1031)
                        t1032 = inline1837
                        var second__9 uint32 = uint32(uint8(t1032))
                        var t1035 bool
                        var inline1834 bool = second__9 < 128
                        if inline1834 {
                            t1035 = true
                        } else {
                            var inline1835 bool = second__9 > 191
                            t1035 = inline1835
                        }
                        if t1035 {
                            var inline1825 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1825
                        } else {
                            var t1037_rhs uint32 = 31
                            var t1037 uint32 = first__8 & t1037_rhs
                            var t1038_rhs int = 6
                            var t1038 uint32 = t1037 << t1038_rhs
                            var t1039_rhs uint32 = 63
                            var t1039 uint32 = second__9 & t1039_rhs
                            var t1040 uint32 = t1038 | t1039
                            var inline1827 int = 2
                            var inline1828 Option__char = char_from_uint32(t1040)
                            switch inline1828.(type) {
                            case None:
                                var inline1829 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1829
                            case Some:
                                var inline1830 rune = inline1828.(Some)._0
                                var inline1832 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1830,
                                    _2: inline1827,
                                }
                                return inline1832
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1047 bool = first__8 < 240
                    if t1047 {
                        var t1080 int = length__7 - index__6
                        var t1081 bool = t1080 < 3
                        if t1081 {
                            var inline1839 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1839
                        } else {
                            var t1049 int = index__6 + 1
                            var t1050 uint8
                            var inline1854 uint8 = _goml_runtime_core_string_byte_get(value__5, t1049)
                            t1050 = inline1854
                            var second__10 uint32 = uint32(uint8(t1050))
                            var t1051 int = index__6 + 2
                            var t1052 uint8
                            var inline1852 uint8 = _goml_runtime_core_string_byte_get(value__5, t1051)
                            t1052 = inline1852
                            var third__11 uint32 = uint32(uint8(t1052))
                            var t1078 bool = utf8_invalid_continuation(second__10)
                            var jp1073 bool
                            if t1078 {
                                jp1073 = true
                            } else {
                                var inline1841 bool = third__11 < 128
                                if inline1841 {
                                    jp1073 = true
                                } else {
                                    var inline1842 bool = third__11 > 191
                                    jp1073 = inline1842
                                }
                            }
                            var jp1067 bool
                            if jp1073 {
                                jp1067 = true
                            } else {
                                var t1076 bool
                                var inline1844 uint32 = 224
                                var inline1845 bool = first__8 == inline1844
                                t1076 = inline1845
                                if t1076 {
                                    var t1077 bool = second__10 < 160
                                    jp1067 = t1077
                                } else {
                                    jp1067 = false
                                }
                            }
                            var jp1056 bool
                            if jp1067 {
                                jp1056 = true
                            } else {
                                var t1070 bool
                                var inline1847 uint32 = 237
                                var inline1848 bool = first__8 == inline1847
                                t1070 = inline1848
                                if t1070 {
                                    var t1071 bool = second__10 >= 160
                                    jp1056 = t1071
                                } else {
                                    jp1056 = false
                                }
                            }
                            if jp1056 {
                                var inline1850 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1850
                            } else {
                                var t1058_rhs uint32 = 15
                                var t1058 uint32 = first__8 & t1058_rhs
                                var t1059_rhs int = 12
                                var t1059 uint32 = t1058 << t1059_rhs
                                var t1060_rhs uint32 = 63
                                var t1060 uint32 = second__10 & t1060_rhs
                                var t1061_rhs int = 6
                                var t1061 uint32 = t1060 << t1061_rhs
                                var t1062 uint32 = t1059 | t1061
                                var t1063_rhs uint32 = 63
                                var t1063 uint32 = third__11 & t1063_rhs
                                var t1064 uint32 = t1062 | t1063
                                var t1065 Tuple3_4bool_4char_3int = utf8_valid_decode(t1064, 3)
                                return t1065
                            }
                        }
                    } else {
                        var t1085 bool = first__8 < 245
                        if t1085 {
                            var t1126 int = length__7 - index__6
                            var t1127 bool = t1126 < 4
                            if t1127 {
                                var t1128 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1128
                            } else {
                                var t1087 int = index__6 + 1
                                var t1088 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1087)
                                var second__12 uint32 = uint32(uint8(t1088))
                                var t1089 int = index__6 + 2
                                var t1090 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1089)
                                var third__13 uint32 = uint32(uint8(t1090))
                                var t1091 int = index__6 + 3
                                var t1092 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1091)
                                var fourth__14 uint32 = uint32(uint8(t1092))
                                var t1124 bool = utf8_invalid_continuation(second__12)
                                var jp1122 bool
                                if t1124 {
                                    jp1122 = true
                                } else {
                                    var t1125 bool = utf8_invalid_continuation(third__13)
                                    jp1122 = t1125
                                }
                                var jp1116 bool
                                if jp1122 {
                                    jp1116 = true
                                } else {
                                    var t1123 bool = utf8_invalid_continuation(fourth__14)
                                    jp1116 = t1123
                                }
                                var jp1110 bool
                                if jp1116 {
                                    jp1110 = true
                                } else {
                                    var t1119 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t1119 {
                                        var t1120 bool = second__12 < 144
                                        jp1110 = t1120
                                    } else {
                                        jp1110 = false
                                    }
                                }
                                var jp1096 bool
                                if jp1110 {
                                    jp1096 = true
                                } else {
                                    var t1113 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t1113 {
                                        var t1114 bool = second__12 > 143
                                        jp1096 = t1114
                                    } else {
                                        jp1096 = false
                                    }
                                }
                                if jp1096 {
                                    var t1097 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1097
                                } else {
                                    var t1098_rhs uint32 = 7
                                    var t1098 uint32 = first__8 & t1098_rhs
                                    var t1099_rhs int = 18
                                    var t1099 uint32 = t1098 << t1099_rhs
                                    var t1100_rhs uint32 = 63
                                    var t1100 uint32 = second__12 & t1100_rhs
                                    var t1101_rhs int = 12
                                    var t1101 uint32 = t1100 << t1101_rhs
                                    var t1102 uint32 = t1099 | t1101
                                    var t1103_rhs uint32 = 63
                                    var t1103 uint32 = third__13 & t1103_rhs
                                    var t1104_rhs int = 6
                                    var t1104 uint32 = t1103 << t1104_rhs
                                    var t1105 uint32 = t1102 | t1104
                                    var t1106_rhs uint32 = 63
                                    var t1106 uint32 = fourth__14 & t1106_rhs
                                    var t1107 uint32 = t1105 | t1106
                                    var t1108 Tuple3_4bool_4char_3int = utf8_valid_decode(t1107, 4)
                                    return t1108
                                }
                            }
                        } else {
                            var t1129 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1129
                        }
                    }
                }
            }
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1139 bool = value__4 <= 1114111
    if t1139 {
        var t1143 bool = value__4 >= 55296
        var jp1141 bool
        if t1143 {
            var t1144 bool = value__4 <= 57343
            jp1141 = t1144
        } else {
            jp1141 = false
        }
        var t1142 bool = !jp1141
        return t1142
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t1147 int = _goml_runtime_core_string_len(self__38)
    return t1147
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1150 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1150
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t1153 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t1153
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1886 rune
    var inline1858 bool = utf8_valid_scalar(value__0)
    if inline1858 {
        var inline1859 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1861 rune = inline1859._1
        commute_field1886 = inline1861
        var t1159 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1886,
            _2: width__1,
        }
        return t1159
    } else {
        var inline1856 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1856
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1164 bool = value__3 < 128
    if t1164 {
        return true
    } else {
        var t1165 bool = value__3 > 191
        return t1165
    }
}

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t1168 bool = self__117 == other__118
    return t1168
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1173 bool
    var inline1865 bool = value__32 <= 1114111
    if inline1865 {
        var inline1866 bool = value__32 >= 55296
        var inline1868 bool
        if inline1866 {
            var inline1870 bool = value__32 <= 57343
            inline1868 = inline1870
        } else {
            inline1868 = false
        }
        var inline1869 bool = !inline1868
        t1173 = inline1869
    } else {
        t1173 = false
    }
    if t1173 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1174 Option__char = Some{
            _0: x24,
        }
        return t1174
    } else {
        return None{}
    }
}

func main() {
    main0()
}
