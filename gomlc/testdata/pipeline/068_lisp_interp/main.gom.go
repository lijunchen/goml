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
    var len__3 int
    var inline1270 int = _goml_runtime_core_string_len(text__2)
    len__3 = inline1270
    var t332 bool = len__3 == 0
    if t332 {
        return false
    } else {
        var i__4 *ref_int_x
        var inline1267 int = 0
        var inline1268 *ref_int_x = ref__Ref_3int(inline1267)
        i__4 = inline1268
        var saw_digit__5 *ref_bool_x
        var inline1264 bool = false
        var inline1265 *ref_bool_x = ref__Ref_4bool(inline1264)
        saw_digit__5 = inline1265
        var ok__6 *ref_bool_x
        var inline1261 bool = true
        var inline1262 *ref_bool_x = ref__Ref_4bool(inline1261)
        ok__6 = inline1262
        var started__7 *ref_bool_x
        var inline1258 bool = false
        var inline1259 *ref_bool_x = ref__Ref_4bool(inline1258)
        started__7 = inline1259
        Loop_loop338:
        for {
            var t357 bool
            var inline1252 bool = ref_get__Ref_4bool(ok__6)
            t357 = inline1252
            var jp340 bool
            if t357 {
                var t358 int
                var inline1221 int = ref_get__Ref_3int(i__4)
                t358 = inline1221
                var t359 bool = t358 < len__3
                jp340 = t359
            } else {
                jp340 = false
            }
            if jp340 {
                var t341 int
                var inline1250 int = ref_get__Ref_3int(i__4)
                t341 = inline1250
                var ch__8 rune
                var inline1248 rune = string_get(text__2, t341)
                ch__8 = inline1248
                var t354 bool
                var inline1246 bool = ref_get__Ref_4bool(started__7)
                t354 = inline1246
                var t355 bool = !t354
                var jp344 bool
                if t355 {
                    var t356 bool = ch__8 == 45
                    jp344 = t356
                } else {
                    jp344 = false
                }
                if jp344 {
                    var inline1227 bool = true
                    ref_set__Ref_4bool(started__7, inline1227)
                    var t345 int
                    var inline1225 int = ref_get__Ref_3int(i__4)
                    t345 = inline1225
                    var t346 int = t345 + 1
                    ref_set__Ref_3int(i__4, t346)
                    continue
                } else {
                    var t349 bool
                    var inline1243 bool = ch__8 >= 48
                    if inline1243 {
                        var inline1244 bool = ch__8 <= 57
                        t349 = inline1244
                    } else {
                        t349 = false
                    }
                    if t349 {
                        var inline1237 bool = true
                        ref_set__Ref_4bool(started__7, inline1237)
                        var inline1234 bool = true
                        ref_set__Ref_4bool(saw_digit__5, inline1234)
                        var t350 int
                        var inline1232 int = ref_get__Ref_3int(i__4)
                        t350 = inline1232
                        var t351 int = t350 + 1
                        ref_set__Ref_3int(i__4, t351)
                        continue
                    } else {
                        var inline1240 bool = false
                        ref_set__Ref_4bool(ok__6, inline1240)
                        continue
                    }
                }
            } else {
                break Loop_loop338
            }
        }
        var t336 bool
        var inline1256 bool = ref_get__Ref_4bool(ok__6)
        t336 = inline1256
        if t336 {
            var inline1254 bool = ref_get__Ref_4bool(saw_digit__5)
            return inline1254
        } else {
            return false
        }
    }
}

func parse_int32(text__9 string) int32 {
    var len__10 int = _goml_m_inherent_i_string_i_string_i_len(text__9)
    var i__11 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var negative__12 *ref_bool_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(false)
    var started__13 *ref_bool_x
    var inline1311 bool = false
    var inline1312 *ref_bool_x = ref__Ref_4bool(inline1311)
    started__13 = inline1312
    var acc__14 *ref_int32_x
    var inline1308 int32 = 0
    var inline1309 *ref_int32_x = ref__Ref_5int32(inline1308)
    acc__14 = inline1309
    Loop_loop369:
    for {
        var t370 int
        var inline1300 int = ref_get__Ref_3int(i__11)
        t370 = inline1300
        var t371 bool = t370 < len__10
        if t371 {
            var t372 int
            var inline1298 int = ref_get__Ref_3int(i__11)
            t372 = inline1298
            var ch__15 rune
            var inline1296 rune = string_get(text__9, t372)
            ch__15 = inline1296
            var t385 bool
            var inline1294 bool = ref_get__Ref_4bool(started__13)
            t385 = inline1294
            var t386 bool = !t385
            var jp375 bool
            if t386 {
                var t387 bool = ch__15 == 45
                jp375 = t387
            } else {
                jp375 = false
            }
            if jp375 {
                var inline1279 bool = true
                ref_set__Ref_4bool(started__13, inline1279)
                var inline1276 bool = true
                ref_set__Ref_4bool(negative__12, inline1276)
                var t376 int
                var inline1274 int = ref_get__Ref_3int(i__11)
                t376 = inline1274
                var t377 int = t376 + 1
                ref_set__Ref_3int(i__11, t377)
                continue
            } else {
                var inline1291 bool = true
                ref_set__Ref_4bool(started__13, inline1291)
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
                var t379 int32
                var inline1288 int32 = ref_get__Ref_5int32(acc__14)
                t379 = inline1288
                var t380 int32 = t379 * 10
                var t381 int32 = t380 + d__16
                ref_set__Ref_5int32(acc__14, t381)
                var t382 int
                var inline1284 int = ref_get__Ref_3int(i__11)
                t382 = inline1284
                var t383 int = t382 + 1
                ref_set__Ref_3int(i__11, t383)
                continue
            }
        } else {
            break Loop_loop369
        }
    }
    var t365 bool
    var inline1306 bool = ref_get__Ref_4bool(negative__12)
    t365 = inline1306
    if t365 {
        var t366 int32
        var inline1302 int32 = ref_get__Ref_5int32(acc__14)
        t366 = inline1302
        var t367 int32 = 0 - t366
        return t367
    } else {
        var inline1304 int32 = ref_get__Ref_5int32(acc__14)
        return inline1304
    }
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var len__20 int
    var inline1353 int = _goml_runtime_core_string_len(source__18)
    len__20 = inline1353
    var text__21 *ref_string_x
    var inline1350 string = ""
    var inline1351 *ref_string_x = ref__Ref_6string(inline1350)
    text__21 = inline1351
    var i__22 *ref_int_x
    var inline1348 *ref_int_x = ref__Ref_3int(start__19)
    i__22 = inline1348
    var done__23 *ref_bool_x
    var inline1345 bool = false
    var inline1346 *ref_bool_x = ref__Ref_4bool(inline1345)
    done__23 = inline1346
    Loop_loop412:
    for {
        var t425 bool
        var inline1339 bool = ref_get__Ref_4bool(done__23)
        t425 = inline1339
        var t426 bool = !t425
        var jp414 bool
        if t426 {
            var t427 int
            var inline1314 int = ref_get__Ref_3int(i__22)
            t427 = inline1314
            var t428 bool = t427 < len__20
            jp414 = t428
        } else {
            jp414 = false
        }
        if jp414 {
            var t415 int
            var inline1337 int = ref_get__Ref_3int(i__22)
            t415 = inline1337
            var ch__24 rune
            var inline1335 rune = string_get(source__18, t415)
            ch__24 = inline1335
            var t417 bool
            var inline1329 bool = ch__24 == 40
            var inline1331 bool
            if inline1329 {
                inline1331 = true
            } else {
                var inline1333 bool = ch__24 == 41
                inline1331 = inline1333
            }
            if inline1331 {
                t417 = true
                if t417 {
                    var inline1316 bool = true
                    ref_set__Ref_4bool(done__23, inline1316)
                    continue
                } else {
                    var t419 string
                    var inline1327 string = ref_get__Ref_6string(text__21)
                    t419 = inline1327
                    var t420 string
                    var inline1325 string = char_to_string(ch__24)
                    t420 = inline1325
                    var t421 string = t419 + t420
                    ref_set__Ref_6string(text__21, t421)
                    var t422 int
                    var inline1321 int = ref_get__Ref_3int(i__22)
                    t422 = inline1321
                    var t423 int = t422 + 1
                    ref_set__Ref_3int(i__22, t423)
                    continue
                }
            } else {
                var inline1332 bool = ch__24 == 32
                t417 = inline1332
                if t417 {
                    var inline1316 bool = true
                    ref_set__Ref_4bool(done__23, inline1316)
                    continue
                } else {
                    var t419 string
                    var inline1327 string = ref_get__Ref_6string(text__21)
                    t419 = inline1327
                    var t420 string
                    var inline1325 string = char_to_string(ch__24)
                    t420 = inline1325
                    var t421 string = t419 + t420
                    ref_set__Ref_6string(text__21, t421)
                    var t422 int
                    var inline1321 int = ref_get__Ref_3int(i__22)
                    t422 = inline1321
                    var t423 int = t422 + 1
                    ref_set__Ref_3int(i__22, t423)
                    continue
                }
            }
        } else {
            break Loop_loop412
        }
    }
    var atom__25 string
    var inline1343 string = ref_get__Ref_6string(text__21)
    atom__25 = inline1343
    var jp401 Token
    switch atom__25 {
    case "true":
        var t404 Token = Token_Bool{
            _0: true,
        }
        jp401 = t404
    case "false":
        var t405 Token = Token_Bool{
            _0: false,
        }
        jp401 = t405
    default:
        var t408 bool = is_int_text(atom__25)
        if t408 {
            var t409 int32 = parse_int32(atom__25)
            var t410 Token = Token_Int{
                _0: t409,
            }
            jp401 = t410
        } else {
            var t411 Token = Token_Sym{
                _0: atom__25,
            }
            jp401 = t411
        }
    }
    var t402 int
    var inline1341 int = ref_get__Ref_3int(i__22)
    t402 = inline1341
    var t403 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: jp401,
        _1: t402,
    }
    return t403
}

func lex(source__27 string) *_goml_vec_Token {
    var len__28 int
    var inline1398 int = _goml_runtime_core_string_len(source__27)
    len__28 = inline1398
    var toks0__29 *_goml_vec_Token
    var inline1396 *_goml_vec_Token = vec_new__Vec_5Token()
    toks0__29 = inline1396
    var toks__30 *ref_Vec_5Token_x
    var inline1394 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    toks__30 = inline1394
    var i__31 *ref_int_x
    var inline1391 int = 0
    var inline1392 *ref_int_x = ref__Ref_3int(inline1391)
    i__31 = inline1392
    Loop_loop433:
    for {
        var t434 int
        var inline1387 int = ref_get__Ref_3int(i__31)
        t434 = inline1387
        var t435 bool = t434 < len__28
        if t435 {
            var t436 int
            var inline1385 int = ref_get__Ref_3int(i__31)
            t436 = inline1385
            var ch__32 rune
            var inline1383 rune = string_get(source__27, t436)
            ch__32 = inline1383
            var t438 bool = ch__32 == 40
            if t438 {
                var t439 *_goml_vec_Token
                var inline1361 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                t439 = inline1361
                var t440 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t439, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t440)
                var t441 int
                var inline1357 int = ref_get__Ref_3int(i__31)
                t441 = inline1357
                var t442 int = t441 + 1
                ref_set__Ref_3int(i__31, t442)
                continue
            } else {
                var t445 bool = ch__32 == 41
                if t445 {
                    var t446 *_goml_vec_Token
                    var inline1369 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                    t446 = inline1369
                    var t447 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t446, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t447)
                    var t448 int
                    var inline1365 int = ref_get__Ref_3int(i__31)
                    t448 = inline1365
                    var t449 int = t448 + 1
                    ref_set__Ref_3int(i__31, t449)
                    continue
                } else {
                    var t452 bool = ch__32 == 32
                    if t452 {
                        var t453 int
                        var inline1373 int = ref_get__Ref_3int(i__31)
                        t453 = inline1373
                        var t454 int = t453 + 1
                        ref_set__Ref_3int(i__31, t454)
                        continue
                    } else {
                        var t456 int
                        var inline1381 int = ref_get__Ref_3int(i__31)
                        t456 = inline1381
                        var mtmp195 Tuple2_5Token_3int = lex_atom(source__27, t456)
                        var x196 Token = mtmp195._0
                        var x197 int = mtmp195._1
                        var t457 *_goml_vec_Token
                        var inline1379 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                        t457 = inline1379
                        var t458 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t457, x196)
                        ref_set__Ref_10Vec_5Token(toks__30, t458)
                        ref_set__Ref_3int(i__31, x197)
                        continue
                    }
                }
            }
        } else {
            break Loop_loop433
        }
    }
    var inline1389 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
    return inline1389
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var t462 int
    var inline1424 int = vec_len__Vec_7Binding(env__35)
    t462 = inline1424
    var t463 int = t462 - 1
    var i__37 *ref_int_x
    var inline1422 *ref_int_x = ref__Ref_3int(t463)
    i__37 = inline1422
    var result__38 *ref_Value_x
    var inline1420 *ref_Value_x = ref__Ref_5Value(Nil{})
    result__38 = inline1420
    var done__39 *ref_bool_x
    var inline1417 bool = false
    var inline1418 *ref_bool_x = ref__Ref_4bool(inline1417)
    done__39 = inline1418
    Loop_loop466:
    for {
        var t478 bool
        var inline1413 bool = ref_get__Ref_4bool(done__39)
        t478 = inline1413
        var t479 bool = !t478
        var jp468 bool
        if t479 {
            var t480 int
            var inline1400 int = ref_get__Ref_3int(i__37)
            t480 = inline1400
            var t481 bool = t480 >= 0
            jp468 = t481
        } else {
            jp468 = false
        }
        if jp468 {
            var t469 int
            var inline1411 int = ref_get__Ref_3int(i__37)
            t469 = inline1411
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t469)
            var t471 string = binding__40.name
            var t472 bool = t471 == name__36
            if t472 {
                var t473 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t473)
                var inline1402 bool = true
                ref_set__Ref_4bool(done__39, inline1402)
                continue
            } else {
                var t475 int
                var inline1409 int = ref_get__Ref_3int(i__37)
                t475 = inline1409
                var t476 int = t475 - 1
                ref_set__Ref_3int(i__37, t476)
                continue
            }
        } else {
            break Loop_loop466
        }
    }
    var inline1415 Value = ref_get__Ref_5Value(result__38)
    return inline1415
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var acc__47 *_goml_vec_SExpr
    var inline1460 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__47 = inline1460
    var exprs__48 *ref_Vec_5SExpr_x
    var inline1458 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    exprs__48 = inline1458
    var i__49 *ref_int_x
    var inline1456 *ref_int_x = ref__Ref_3int(start__46)
    i__49 = inline1456
    var done__50 *ref_bool_x
    var inline1453 bool = false
    var inline1454 *ref_bool_x = ref__Ref_4bool(inline1453)
    done__50 = inline1454
    Loop_loop493:
    for {
        var t505 bool
        var inline1447 bool = ref_get__Ref_4bool(done__50)
        t505 = inline1447
        var t506 bool = !t505
        var jp495 bool
        if t506 {
            var t507 int
            var inline1428 int = ref_get__Ref_3int(i__49)
            t507 = inline1428
            var t508 int
            var inline1426 int = vec_len__Vec_5Token(tokens__45)
            t508 = inline1426
            var t509 bool = t507 < t508
            jp495 = t509
        } else {
            jp495 = false
        }
        if jp495 {
            var t496 int
            var inline1445 int = ref_get__Ref_3int(i__49)
            t496 = inline1445
            var mtmp206 Token = vec_get__Vec_5Token(tokens__45, t496)
            switch mtmp206.(type) {
            case RParen:
                var inline1434 bool = true
                ref_set__Ref_4bool(done__50, inline1434)
                var t498 int
                var inline1432 int = ref_get__Ref_3int(i__49)
                t498 = inline1432
                var t499 int = t498 + 1
                ref_set__Ref_3int(i__49, t499)
                continue
            default:
                var t501 int
                var inline1443 int = ref_get__Ref_3int(i__49)
                t501 = inline1443
                var mtmp211 Tuple2_5SExpr_3int = parse_expr(tokens__45, t501)
                var x212 SExpr = mtmp211._0
                var x213 int = mtmp211._1
                var t502 *_goml_vec_SExpr
                var inline1441 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                t502 = inline1441
                var t503 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t502, x212)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t503)
                ref_set__Ref_3int(i__49, x213)
                continue
            }
        } else {
            break Loop_loop493
        }
    }
    var t490 *_goml_vec_SExpr
    var inline1451 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    t490 = inline1451
    var t491 int
    var inline1449 int = ref_get__Ref_3int(i__49)
    t491 = inline1449
    var t492 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t490,
        _1: t491,
    }
    return t492
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var mtmp216 Token = vec_get__Vec_5Token(tokens__53, start__54)
    switch mtmp216.(type) {
    case LParen:
        var t514 int = start__54 + 1
        var mtmp220 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t514)
        var x221 *_goml_vec_SExpr = mtmp220._0
        var x222 int = mtmp220._1
        var t515 SExpr = List{
            _0: x221,
        }
        var t516 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t515,
            _1: x222,
        }
        return t516
    case RParen:
        var t517 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t518 int = start__54 + 1
        var t519 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t517,
            _1: t518,
        }
        return t519
    case Token_Sym:
        var x217 string = mtmp216.(Token_Sym)._0
        var t520 SExpr = SExpr_Sym{
            _0: x217,
        }
        var t521 int = start__54 + 1
        var t522 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t520,
            _1: t521,
        }
        return t522
    case Token_Int:
        var x218 int32 = mtmp216.(Token_Int)._0
        var t523 SExpr = SExpr_Int{
            _0: x218,
        }
        var t524 int = start__54 + 1
        var t525 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t523,
            _1: t524,
        }
        return t525
    case Token_Bool:
        var x219 bool = mtmp216.(Token_Bool)._0
        var t526 SExpr = SExpr_Bool{
            _0: x219,
        }
        var t527 int = start__54 + 1
        var t528 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t526,
            _1: t527,
        }
        return t528
    default:
        panic("non-exhaustive match")
    }
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var i__61 *ref_int_x
    var inline1480 int = 0
    var inline1481 *ref_int_x = ref__Ref_3int(inline1480)
    i__61 = inline1481
    var acc__62 *_goml_vec_SExpr
    var inline1478 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__62 = inline1478
    var exprs__63 *ref_Vec_5SExpr_x
    var inline1476 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    exprs__63 = inline1476
    Loop_loop533:
    for {
        var t534 int
        var inline1472 int = ref_get__Ref_3int(i__61)
        t534 = inline1472
        var t535 int
        var inline1470 int = vec_len__Vec_5Token(tokens__60)
        t535 = inline1470
        var t536 bool = t534 < t535
        if t536 {
            var t537 int
            var inline1468 int = ref_get__Ref_3int(i__61)
            t537 = inline1468
            var mtmp223 Tuple2_5SExpr_3int = parse_expr(tokens__60, t537)
            var x224 SExpr = mtmp223._0
            var x225 int = mtmp223._1
            var t538 *_goml_vec_SExpr
            var inline1466 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            t538 = inline1466
            var t539 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t538, x224)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t539)
            ref_set__Ref_3int(i__61, x225)
            continue
        } else {
            break Loop_loop533
        }
    }
    var inline1474 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    return inline1474
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    switch expr__72.(type) {
    case SExpr_Int:
        var x234 int32 = expr__72.(SExpr_Int)._0
        var t556 Value = Value_Int{
            _0: x234,
        }
        return t556
    case SExpr_Bool:
        var x235 bool = expr__72.(SExpr_Bool)._0
        var t557 Value = Value_Bool{
            _0: x235,
        }
        return t557
    case SExpr_Sym:
        var x236 string = expr__72.(SExpr_Sym)._0
        var t558 *_goml_vec_Binding
        var inline1491 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__74)
        t558 = inline1491
        var inline1487 Value = env_lookup(local__73, x236)
        switch inline1487.(type) {
        case Nil:
            var inline1488 Value = env_lookup(t558, x236)
            return inline1488
        default:
            return inline1487
        }
    case List:
        var x237 *_goml_vec_SExpr = expr__72.(List)._0
        var inline1493 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(x237)
        var inline1494 bool = inline1493 == 0
        if inline1494 {
            return Nil{}
        } else {
            var inline1495 SExpr = vec_get__Vec_5SExpr(x237, 0)
            switch inline1495.(type) {
            case SExpr_Sym:
                var inline1496 string = inline1495.(SExpr_Sym)._0
                var inline1498 Value = eval_list_sym(inline1496, x237, local__73, global__74)
                return inline1498
            default:
                var inline1499 Value = eval(inline1495, local__73, global__74)
                var inline1500 *_goml_vec_Value = eval_args(x237, 1, local__73, global__74)
                var inline1501 Value = apply(inline1499, inline1500, global__74)
                return inline1501
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    switch name__86 {
    case "begin":
        var t575 Value = eval_begin(items__87, 1, local__88, global__89)
        return t575
    case "define":
        var t578 int
        var inline1513 int = vec_len__Vec_5SExpr(items__87)
        t578 = inline1513
        var t579 bool = t578 == 3
        if t579 {
            var mtmp242 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp242.(type) {
            case SExpr_Sym:
                var x245 string = mtmp242.(SExpr_Sym)._0
                var t582 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t582, local__88, global__89)
                var env__92 *_goml_vec_Binding
                var inline1511 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__89)
                env__92 = inline1511
                var t583 Binding = Binding{
                    name: x245,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t583)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                return value__91
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "if":
        var t586 int
        var inline1521 int = vec_len__Vec_5SExpr(items__87)
        t586 = inline1521
        var t587 bool = t586 == 4
        if t587 {
            var t588 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t588, local__88, global__89)
            var t591 bool
            switch cond__94.(type) {
            case Value_Int:
                var inline1515 int32 = cond__94.(Value_Int)._0
                var inline1517 bool = inline1515 != 0
                t591 = inline1517
            case Value_Bool:
                var inline1518 bool = cond__94.(Value_Bool)._0
                t591 = inline1518
            case Func:
                t591 = true
            case Nil:
                t591 = false
            default:
                panic("non-exhaustive match")
            }
            if t591 {
                var t592 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t593 Value = eval(t592, local__88, global__89)
                return t593
            } else {
                var t594 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t595 Value = eval(t594, local__88, global__89)
                return t595
            }
        } else {
            return Nil{}
        }
    case "lambda":
        var t598 int
        var inline1523 int = vec_len__Vec_5SExpr(items__87)
        t598 = inline1523
        var t599 bool = t598 == 3
        if t599 {
            var mtmp248 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp248.(type) {
            case List:
                var x252 *_goml_vec_SExpr = mtmp248.(List)._0
                var params__96 *_goml_vec_string = params_from_sexprs(x252)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t602 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t603 Value = Func{
                    _0: t602,
                }
                return t603
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "+":
        var t604 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t605 Value = apply_builtin("+", t604)
        return t605
    case "-":
        var t606 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t607 Value = apply_builtin("-", t606)
        return t607
    case "*":
        var t608 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t609 Value = apply_builtin("*", t608)
        return t609
    case "/":
        var t610 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t611 Value = apply_builtin("/", t610)
        return t611
    case "=":
        var t612 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t613 Value = apply_builtin("=", t612)
        return t613
    default:
        var t614 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t614, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        switch f__98.(type) {
        case Func:
            var inline1525 Lambda = f__98.(Func)._0
            var inline1527 Value = apply_lambda(inline1525, args__99)
            return inline1527
        default:
            return Nil{}
        }
    }
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var i__104 *ref_int_x
    var inline1545 *ref_int_x = ref__Ref_3int(start__101)
    i__104 = inline1545
    var last__105 *ref_Value_x
    var inline1543 *ref_Value_x = ref__Ref_5Value(Nil{})
    last__105 = inline1543
    Loop_loop620:
    for {
        var t621 int
        var inline1539 int = ref_get__Ref_3int(i__104)
        t621 = inline1539
        var t622 int
        var inline1537 int = vec_len__Vec_5SExpr(items__100)
        t622 = inline1537
        var t623 bool = t621 < t622
        if t623 {
            var t624 int
            var inline1535 int = ref_get__Ref_3int(i__104)
            t624 = inline1535
            var t625 SExpr = vec_get__Vec_5SExpr(items__100, t624)
            var v__106 Value = eval(t625, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t626 int
            var inline1531 int = ref_get__Ref_3int(i__104)
            t626 = inline1531
            var t627 int = t626 + 1
            ref_set__Ref_3int(i__104, t627)
            continue
        } else {
            break Loop_loop620
        }
    }
    var inline1541 Value = ref_get__Ref_5Value(last__105)
    return inline1541
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var i__108 *ref_int_x
    var inline1571 int = 0
    var inline1572 *ref_int_x = ref__Ref_3int(inline1571)
    i__108 = inline1572
    var acc__109 *_goml_vec_string
    var inline1569 *_goml_vec_string = vec_new__Vec_6string()
    acc__109 = inline1569
    var params__110 *ref_Vec_6string_x
    var inline1567 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    params__110 = inline1567
    Loop_loop633:
    for {
        var t634 int
        var inline1563 int = ref_get__Ref_3int(i__108)
        t634 = inline1563
        var t635 int
        var inline1561 int = vec_len__Vec_5SExpr(items__107)
        t635 = inline1561
        var t636 bool = t634 < t635
        if t636 {
            var t637 int
            var inline1559 int = ref_get__Ref_3int(i__108)
            t637 = inline1559
            var mtmp255 SExpr = vec_get__Vec_5SExpr(items__107, t637)
            switch mtmp255.(type) {
            case SExpr_Sym:
                var x258 string = mtmp255.(SExpr_Sym)._0
                var t639 *_goml_vec_string
                var inline1553 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
                t639 = inline1553
                var t640 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t639, x258)
                ref_set__Ref_11Vec_6string(params__110, t640)
                var t641 int
                var inline1549 int = ref_get__Ref_3int(i__108)
                t641 = inline1549
                var t642 int = t641 + 1
                ref_set__Ref_3int(i__108, t642)
                continue
            default:
                var t644 int
                var inline1557 int = ref_get__Ref_3int(i__108)
                t644 = inline1557
                var t645 int = t644 + 1
                ref_set__Ref_3int(i__108, t645)
                continue
            }
        } else {
            break Loop_loop633
        }
    }
    var inline1565 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
    return inline1565
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var i__116 *ref_int_x
    var inline1594 *ref_int_x = ref__Ref_3int(start__113)
    i__116 = inline1594
    var acc__117 *_goml_vec_Value
    var inline1592 *_goml_vec_Value = vec_new__Vec_5Value()
    acc__117 = inline1592
    var args__118 *ref_Vec_5Value_x
    var inline1590 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    args__118 = inline1590
    Loop_loop651:
    for {
        var t652 int
        var inline1586 int = ref_get__Ref_3int(i__116)
        t652 = inline1586
        var t653 int
        var inline1584 int = vec_len__Vec_5SExpr(items__112)
        t653 = inline1584
        var t654 bool = t652 < t653
        if t654 {
            var t655 int
            var inline1582 int = ref_get__Ref_3int(i__116)
            t655 = inline1582
            var t656 SExpr = vec_get__Vec_5SExpr(items__112, t655)
            var v__119 Value = eval(t656, local__114, global__115)
            var t657 *_goml_vec_Value
            var inline1580 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
            t657 = inline1580
            var t658 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t657, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t658)
            var t659 int
            var inline1576 int = ref_get__Ref_3int(i__116)
            t659 = inline1576
            var t660 int = t659 + 1
            ref_set__Ref_3int(i__116, t660)
            continue
        } else {
            break Loop_loop651
        }
    }
    var inline1588 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
    return inline1588
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    switch name__120 {
    case "=":
        var t668 int
        var inline1596 int = vec_len__Vec_5Value(args__121)
        t668 = inline1596
        var t669 bool = t668 == 2
        if t669 {
            var t670 Value = vec_get__Vec_5Value(args__121, 0)
            var t671 Value = vec_get__Vec_5Value(args__121, 1)
            switch t671.(type) {
            case Value_Int:
                var x267 int32 = t671.(Value_Int)._0
                switch t670.(type) {
                case Value_Int:
                    var x270 int32 = t670.(Value_Int)._0
                    var t676 bool = x270 == x267
                    var t677 Value = Value_Bool{
                        _0: t676,
                    }
                    return t677
                default:
                    var t678 Value = Value_Bool{
                        _0: false,
                    }
                    return t678
                }
            case Value_Bool:
                var x268 bool = t671.(Value_Bool)._0
                switch t670.(type) {
                case Value_Bool:
                    var x274 bool = t670.(Value_Bool)._0
                    var t681 bool = x274 == x268
                    var t682 Value = Value_Bool{
                        _0: t681,
                    }
                    return t682
                default:
                    var t683 Value = Value_Bool{
                        _0: false,
                    }
                    return t683
                }
            default:
                var t684 Value = Value_Bool{
                    _0: false,
                }
                return t684
            }
        } else {
            var t685 Value = Value_Bool{
                _0: false,
            }
            return t685
        }
    case "+":
        var i__126 *ref_int_x
        var inline1621 int = 0
        var inline1622 *ref_int_x = ref__Ref_3int(inline1621)
        i__126 = inline1622
        var acc__127 *ref_int32_x
        var inline1618 int32 = 0
        var inline1619 *ref_int32_x = ref__Ref_5int32(inline1618)
        acc__127 = inline1619
        Loop_loop689:
        for {
            var t690 int
            var inline1614 int = ref_get__Ref_3int(i__126)
            t690 = inline1614
            var t691 int
            var inline1612 int = vec_len__Vec_5Value(args__121)
            t691 = inline1612
            var t692 bool = t690 < t691
            if t692 {
                var t693 int
                var inline1610 int = ref_get__Ref_3int(i__126)
                t693 = inline1610
                var mtmp276 Value = vec_get__Vec_5Value(args__121, t693)
                switch mtmp276.(type) {
                case Value_Int:
                    var x277 int32 = mtmp276.(Value_Int)._0
                    var t695 int32
                    var inline1604 int32 = ref_get__Ref_5int32(acc__127)
                    t695 = inline1604
                    var t696 int32 = t695 + x277
                    ref_set__Ref_5int32(acc__127, t696)
                    var t697 int
                    var inline1600 int = ref_get__Ref_3int(i__126)
                    t697 = inline1600
                    var t698 int = t697 + 1
                    ref_set__Ref_3int(i__126, t698)
                    continue
                default:
                    var t700 int
                    var inline1608 int = ref_get__Ref_3int(i__126)
                    t700 = inline1608
                    var t701 int = t700 + 1
                    ref_set__Ref_3int(i__126, t701)
                    continue
                }
            } else {
                break Loop_loop689
            }
        }
        var t687 int32
        var inline1616 int32 = ref_get__Ref_5int32(acc__127)
        t687 = inline1616
        var t688 Value = Value_Int{
            _0: t687,
        }
        return t688
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x
        var inline1644 int32 = 1
        var inline1645 *ref_int32_x = ref__Ref_5int32(inline1644)
        acc__130 = inline1645
        Loop_loop706:
        for {
            var t707 int
            var inline1640 int = ref_get__Ref_3int(i__129)
            t707 = inline1640
            var t708 int
            var inline1638 int = vec_len__Vec_5Value(args__121)
            t708 = inline1638
            var t709 bool = t707 < t708
            if t709 {
                var t710 int
                var inline1636 int = ref_get__Ref_3int(i__129)
                t710 = inline1636
                var mtmp282 Value = vec_get__Vec_5Value(args__121, t710)
                switch mtmp282.(type) {
                case Value_Int:
                    var x283 int32 = mtmp282.(Value_Int)._0
                    var t712 int32
                    var inline1630 int32 = ref_get__Ref_5int32(acc__130)
                    t712 = inline1630
                    var t713 int32 = t712 * x283
                    ref_set__Ref_5int32(acc__130, t713)
                    var t714 int
                    var inline1626 int = ref_get__Ref_3int(i__129)
                    t714 = inline1626
                    var t715 int = t714 + 1
                    ref_set__Ref_3int(i__129, t715)
                    continue
                default:
                    var t717 int
                    var inline1634 int = ref_get__Ref_3int(i__129)
                    t717 = inline1634
                    var t718 int = t717 + 1
                    ref_set__Ref_3int(i__129, t718)
                    continue
                }
            } else {
                break Loop_loop706
            }
        }
        var t704 int32
        var inline1642 int32 = ref_get__Ref_5int32(acc__130)
        t704 = inline1642
        var t705 Value = Value_Int{
            _0: t704,
        }
        return t705
    case "-":
        var mtmp288 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        switch mtmp288 {
        case 1:
            var mtmp289 Value = vec_get__Vec_5Value(args__121, 0)
            switch mtmp289.(type) {
            case Value_Int:
                var x290 int32 = mtmp289.(Value_Int)._0
                var t724 int32 = 0 - x290
                var t725 Value = Value_Int{
                    _0: t724,
                }
                return t725
            default:
                return Nil{}
            }
        case 2:
            var t726 Value = vec_get__Vec_5Value(args__121, 0)
            var t727 Value = vec_get__Vec_5Value(args__121, 1)
            switch t727.(type) {
            case Value_Int:
                var x296 int32 = t727.(Value_Int)._0
                switch t726.(type) {
                case Value_Int:
                    var x299 int32 = t726.(Value_Int)._0
                    var t732 int32 = x299 - x296
                    var t733 Value = Value_Int{
                        _0: t732,
                    }
                    return t733
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
        var t736 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t737 bool = t736 == 2
        if t737 {
            var t738 Value = vec_get__Vec_5Value(args__121, 0)
            var t739 Value = vec_get__Vec_5Value(args__121, 1)
            switch t739.(type) {
            case Value_Int:
                var x305 int32 = t739.(Value_Int)._0
                switch t738.(type) {
                case Value_Int:
                    var x308 int32 = t738.(Value_Int)._0
                    var t744 int32 = x308 / x305
                    var t745 Value = Value_Int{
                        _0: t744,
                    }
                    return t745
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
        var x313 Lambda = func__137.(Func)._0
        var t750 Value = apply_lambda(x313, args__138)
        return t750
    default:
        return Nil{}
    }
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var t753 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x
    var inline1672 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t753)
    env__143 = inline1672
    var i__144 *ref_int_x
    var inline1669 int = 0
    var inline1670 *ref_int_x = ref__Ref_3int(inline1669)
    i__144 = inline1670
    Loop_loop759:
    for {
        var t770 int
        var inline1665 int = ref_get__Ref_3int(i__144)
        t770 = inline1665
        var t771 *_goml_vec_string = lambda__141.params
        var t772 int
        var inline1663 int = vec_len__Vec_6string(t771)
        t772 = inline1663
        var t773 bool = t770 < t772
        var jp761 bool
        if t773 {
            var t774 int
            var inline1649 int = ref_get__Ref_3int(i__144)
            t774 = inline1649
            var t775 int
            var inline1647 int = vec_len__Vec_5Value(args__142)
            t775 = inline1647
            var t776 bool = t774 < t775
            jp761 = t776
        } else {
            jp761 = false
        }
        if jp761 {
            var t762 *_goml_vec_string = lambda__141.params
            var t763 int
            var inline1661 int = ref_get__Ref_3int(i__144)
            t763 = inline1661
            var name__145 string = vec_get__Vec_6string(t762, t763)
            var t764 int
            var inline1659 int = ref_get__Ref_3int(i__144)
            t764 = inline1659
            var value__146 Value = vec_get__Vec_5Value(args__142, t764)
            var t765 *_goml_vec_Binding
            var inline1657 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
            t765 = inline1657
            var t766 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t765, t766)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t767 int
            var inline1653 int = ref_get__Ref_3int(i__144)
            t767 = inline1653
            var t768 int = t767 + 1
            ref_set__Ref_3int(i__144, t768)
            continue
        } else {
            break Loop_loop759
        }
    }
    var t755 SExpr = lambda__141.body
    var t756 *_goml_vec_Binding
    var inline1667 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
    t756 = inline1667
    var t757 *ref_Vec_7Binding_x = lambda__141.global
    var t758 Value = eval(t755, t756, t757)
    return t758
}

func main0() struct{} {
    var t778 *_goml_vec_Binding
    var inline1700 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t778 = inline1700
    var global__148 *ref_Vec_7Binding_x
    var inline1698 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t778)
    global__148 = inline1698
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t779 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t779)
    var t780 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t781 *_goml_vec_Binding
    var inline1696 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t781 = inline1696
    var result__151 Value = eval(t780, t781, global__148)
    var t782 string
    switch result__151.(type) {
    case Value_Int:
        var inline1689 int32 = result__151.(Value_Int)._0
        var inline1691 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1689)
        t782 = inline1691
    case Value_Bool:
        var inline1692 bool = result__151.(Value_Bool)._0
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
    var t783 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t783)
    var t784 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t785 *_goml_vec_Binding
    var inline1684 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t785 = inline1684
    var result2__153 Value = eval(t784, t785, global__148)
    var t786 string
    switch result2__153.(type) {
    case Value_Int:
        var inline1677 int32 = result2__153.(Value_Int)._0
        var inline1679 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1677)
        t786 = inline1679
    case Value_Bool:
        var inline1680 bool = result2__153.(Value_Bool)._0
        var inline1682 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1680)
        t786 = inline1682
    case Func:
        t786 = "<lambda>"
    case Nil:
        t786 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1674 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t786)
    _goml_runtime_core_string_println(inline1674)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__35 string) int {
    var t789 int = _goml_runtime_core_string_len(self__35)
    return t789
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__270 int) *ref_int_x {
    var t792 *ref_int_x = ref__Ref_3int(value__270)
    return t792
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__270 bool) *ref_bool_x {
    var t795 *ref_bool_x = ref__Ref_4bool(value__270)
    return t795
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__176 *_goml_vec_Token, elem__177 Token) *_goml_vec_Token {
    var t839 int
    var inline1722 int = vec_len__Vec_5Token(self__176)
    t839 = inline1722
    var t840 int = t839 + 1
    var result__178 *_goml_vec_Token
    var inline1720 *_goml_vec_Token = vec_with_capacity__Vec_5Token(t840)
    result__178 = inline1720
    var index__179 int = 0
    Loop_loop842:
    for {
        var t843 int
        var inline1716 int = vec_len__Vec_5Token(self__176)
        t843 = inline1716
        var t844 bool = index__179 < t843
        if t844 {
            var t845 Token = vec_get__Vec_5Token(self__176, index__179)
            vec_push__Vec_5Token(result__178, t845)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t846 int = compound_old80 + compound_value81
            index__179 = t846
            continue
        } else {
            break Loop_loop842
        }
    }
    vec_push__Vec_5Token(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__176 *_goml_vec_SExpr, elem__177 SExpr) *_goml_vec_SExpr {
    var t875 int
    var inline1732 int = vec_len__Vec_5SExpr(self__176)
    t875 = inline1732
    var t876 int = t875 + 1
    var result__178 *_goml_vec_SExpr
    var inline1730 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(t876)
    result__178 = inline1730
    var index__179 int = 0
    Loop_loop878:
    for {
        var t879 int
        var inline1726 int = vec_len__Vec_5SExpr(self__176)
        t879 = inline1726
        var t880 bool = index__179 < t879
        if t880 {
            var t881 SExpr = vec_get__Vec_5SExpr(self__176, index__179)
            vec_push__Vec_5SExpr(result__178, t881)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t882 int = compound_old80 + compound_value81
            index__179 = t882
            continue
        } else {
            break Loop_loop878
        }
    }
    vec_push__Vec_5SExpr(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t888 string = _goml_runtime_core_int32_to_string(self__33)
    return t888
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t891 string = _goml_runtime_core_bool_to_string(self__64)
    return t891
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__189 *_goml_vec_SExpr) int {
    var t897 int = vec_len__Vec_5SExpr(self__189)
    return t897
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__176 *_goml_vec_Binding, elem__177 Binding) *_goml_vec_Binding {
    var t900 int
    var inline1742 int = vec_len__Vec_7Binding(self__176)
    t900 = inline1742
    var t901 int = t900 + 1
    var result__178 *_goml_vec_Binding
    var inline1740 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(t901)
    result__178 = inline1740
    var index__179 int = 0
    Loop_loop903:
    for {
        var t904 int
        var inline1736 int = vec_len__Vec_7Binding(self__176)
        t904 = inline1736
        var t905 bool = index__179 < t904
        if t905 {
            var t906 Binding = vec_get__Vec_7Binding(self__176, index__179)
            vec_push__Vec_7Binding(result__178, t906)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t907 int = compound_old80 + compound_value81
            index__179 = t907
            continue
        } else {
            break Loop_loop903
        }
    }
    vec_push__Vec_7Binding(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__176 *_goml_vec_string, elem__177 string) *_goml_vec_string {
    var t922 int
    var inline1752 int = vec_len__Vec_6string(self__176)
    t922 = inline1752
    var t923 int = t922 + 1
    var result__178 *_goml_vec_string
    var inline1750 *_goml_vec_string = vec_with_capacity__Vec_6string(t923)
    result__178 = inline1750
    var index__179 int = 0
    Loop_loop925:
    for {
        var t926 int
        var inline1746 int = vec_len__Vec_6string(self__176)
        t926 = inline1746
        var t927 bool = index__179 < t926
        if t927 {
            var t928 string = vec_get__Vec_6string(self__176, index__179)
            vec_push__Vec_6string(result__178, t928)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t929 int = compound_old80 + compound_value81
            index__179 = t929
            continue
        } else {
            break Loop_loop925
        }
    }
    vec_push__Vec_6string(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__176 *_goml_vec_Value, elem__177 Value) *_goml_vec_Value {
    var t944 int
    var inline1762 int = vec_len__Vec_5Value(self__176)
    t944 = inline1762
    var t945 int = t944 + 1
    var result__178 *_goml_vec_Value
    var inline1760 *_goml_vec_Value = vec_with_capacity__Vec_5Value(t945)
    result__178 = inline1760
    var index__179 int = 0
    Loop_loop947:
    for {
        var t948 int
        var inline1756 int = vec_len__Vec_5Value(self__176)
        t948 = inline1756
        var t949 bool = index__179 < t948
        if t949 {
            var t950 Value = vec_get__Vec_5Value(self__176, index__179)
            vec_push__Vec_5Value(result__178, t950)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t951 int = compound_old80 + compound_value81
            index__179 = t951
            continue
        } else {
            break Loop_loop947
        }
    }
    vec_push__Vec_5Value(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__189 *_goml_vec_Value) int {
    var t957 int = vec_len__Vec_5Value(self__189)
    return t957
}

func string_get(value__17 string, index__18 int) rune {
    var mtmp6 Tuple3_4bool_4char_3int = string_decode_utf8_at(value__17, index__18)
    var x7 bool = mtmp6._0
    var x8 rune = mtmp6._1
    if x7 {
        return x8
    } else {
        var t974 rune = _goml_runtime_core_string_get("", -1)
        return t974
    }
}

func char_to_string(value__29 rune) string {
    var t979 uint32 = uint32(rune(value__29))
    var t980 bool
    var inline1765 bool = t979 <= 1114111
    if inline1765 {
        var inline1766 bool = t979 >= 55296
        var inline1768 bool
        if inline1766 {
            var inline1770 bool = t979 <= 57343
            inline1768 = inline1770
        } else {
            inline1768 = false
        }
        var inline1769 bool = !inline1768
        t980 = inline1769
    } else {
        t980 = false
    }
    if t980 {
        var t981 string = _goml_runtime_core_char_to_string(value__29)
        return t981
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1127 bool = index__6 < 0
    var jp1125 bool
    if t1127 {
        jp1125 = true
    } else {
        var t1128 bool = index__6 >= length__7
        jp1125 = t1128
    }
    if jp1125 {
        var inline1772 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1772
    } else {
        var t1012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1012))
        var t1015 bool = first__8 < 128
        if t1015 {
            var inline1774 int = 1
            var inline1775 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1775.(type) {
            case None:
                var inline1776 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1776
            case Some:
                var inline1777 rune = inline1775.(Some)._0
                var inline1779 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1777,
                    _2: inline1774,
                }
                return inline1779
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1019 bool = first__8 < 194
            if t1019 {
                var inline1781 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1781
            } else {
                var t1023 bool = first__8 < 224
                if t1023 {
                    var t1036 int = length__7 - index__6
                    var t1037 bool = t1036 < 2
                    if t1037 {
                        var inline1783 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1783
                    } else {
                        var t1025 int = index__6 + 1
                        var t1026 uint8
                        var inline1797 uint8 = _goml_runtime_core_string_byte_get(value__5, t1025)
                        t1026 = inline1797
                        var second__9 uint32 = uint32(uint8(t1026))
                        var t1029 bool
                        var inline1794 bool = second__9 < 128
                        if inline1794 {
                            t1029 = true
                        } else {
                            var inline1795 bool = second__9 > 191
                            t1029 = inline1795
                        }
                        if t1029 {
                            var inline1785 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1785
                        } else {
                            var t1031_rhs uint32 = 31
                            var t1031 uint32 = first__8 & t1031_rhs
                            var t1032_rhs int = 6
                            var t1032 uint32 = t1031 << t1032_rhs
                            var t1033_rhs uint32 = 63
                            var t1033 uint32 = second__9 & t1033_rhs
                            var t1034 uint32 = t1032 | t1033
                            var inline1787 int = 2
                            var inline1788 Option__char = __goml_builtin_char_from_uint32(t1034)
                            switch inline1788.(type) {
                            case None:
                                var inline1789 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1789
                            case Some:
                                var inline1790 rune = inline1788.(Some)._0
                                var inline1792 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1790,
                                    _2: inline1787,
                                }
                                return inline1792
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1041 bool = first__8 < 240
                    if t1041 {
                        var t1074 int = length__7 - index__6
                        var t1075 bool = t1074 < 3
                        if t1075 {
                            var inline1799 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1799
                        } else {
                            var t1043 int = index__6 + 1
                            var t1044 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1043)
                            var second__10 uint32 = uint32(uint8(t1044))
                            var t1045 int = index__6 + 2
                            var t1046 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1045)
                            var third__11 uint32 = uint32(uint8(t1046))
                            var t1072 bool = utf8_invalid_continuation(second__10)
                            var jp1067 bool
                            if t1072 {
                                jp1067 = true
                            } else {
                                var inline1801 bool = third__11 < 128
                                if inline1801 {
                                    jp1067 = true
                                } else {
                                    var inline1802 bool = third__11 > 191
                                    jp1067 = inline1802
                                }
                            }
                            var jp1061 bool
                            if jp1067 {
                                jp1061 = true
                            } else {
                                var t1070 bool = first__8 == 224
                                if t1070 {
                                    var t1071 bool = second__10 < 160
                                    jp1061 = t1071
                                } else {
                                    jp1061 = false
                                }
                            }
                            var jp1050 bool
                            if jp1061 {
                                jp1050 = true
                            } else {
                                var t1064 bool = first__8 == 237
                                if t1064 {
                                    var t1065 bool = second__10 >= 160
                                    jp1050 = t1065
                                } else {
                                    jp1050 = false
                                }
                            }
                            if jp1050 {
                                var inline1804 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1804
                            } else {
                                var t1052_rhs uint32 = 15
                                var t1052 uint32 = first__8 & t1052_rhs
                                var t1053_rhs int = 12
                                var t1053 uint32 = t1052 << t1053_rhs
                                var t1054_rhs uint32 = 63
                                var t1054 uint32 = second__10 & t1054_rhs
                                var t1055_rhs int = 6
                                var t1055 uint32 = t1054 << t1055_rhs
                                var t1056 uint32 = t1053 | t1055
                                var t1057_rhs uint32 = 63
                                var t1057 uint32 = third__11 & t1057_rhs
                                var t1058 uint32 = t1056 | t1057
                                var inline1806 int = 3
                                var inline1807 Option__char = __goml_builtin_char_from_uint32(t1058)
                                switch inline1807.(type) {
                                case None:
                                    var inline1808 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1808
                                case Some:
                                    var inline1809 rune = inline1807.(Some)._0
                                    var inline1811 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1809,
                                        _2: inline1806,
                                    }
                                    return inline1811
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1079 bool = first__8 < 245
                        if t1079 {
                            var t1120 int = length__7 - index__6
                            var t1121 bool = t1120 < 4
                            if t1121 {
                                var t1122 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1122
                            } else {
                                var t1081 int = index__6 + 1
                                var t1082 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1081)
                                var second__12 uint32 = uint32(uint8(t1082))
                                var t1083 int = index__6 + 2
                                var t1084 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1083)
                                var third__13 uint32 = uint32(uint8(t1084))
                                var t1085 int = index__6 + 3
                                var t1086 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1085)
                                var fourth__14 uint32 = uint32(uint8(t1086))
                                var t1118 bool = utf8_invalid_continuation(second__12)
                                var jp1116 bool
                                if t1118 {
                                    jp1116 = true
                                } else {
                                    var t1119 bool = utf8_invalid_continuation(third__13)
                                    jp1116 = t1119
                                }
                                var jp1110 bool
                                if jp1116 {
                                    jp1110 = true
                                } else {
                                    var t1117 bool = utf8_invalid_continuation(fourth__14)
                                    jp1110 = t1117
                                }
                                var jp1104 bool
                                if jp1110 {
                                    jp1104 = true
                                } else {
                                    var t1113 bool = first__8 == 240
                                    if t1113 {
                                        var t1114 bool = second__12 < 144
                                        jp1104 = t1114
                                    } else {
                                        jp1104 = false
                                    }
                                }
                                var jp1090 bool
                                if jp1104 {
                                    jp1090 = true
                                } else {
                                    var t1107 bool = first__8 == 244
                                    if t1107 {
                                        var t1108 bool = second__12 > 143
                                        jp1090 = t1108
                                    } else {
                                        jp1090 = false
                                    }
                                }
                                if jp1090 {
                                    var t1091 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1091
                                } else {
                                    var t1092_rhs uint32 = 7
                                    var t1092 uint32 = first__8 & t1092_rhs
                                    var t1093_rhs int = 18
                                    var t1093 uint32 = t1092 << t1093_rhs
                                    var t1094_rhs uint32 = 63
                                    var t1094 uint32 = second__12 & t1094_rhs
                                    var t1095_rhs int = 12
                                    var t1095 uint32 = t1094 << t1095_rhs
                                    var t1096 uint32 = t1093 | t1095
                                    var t1097_rhs uint32 = 63
                                    var t1097 uint32 = third__13 & t1097_rhs
                                    var t1098_rhs int = 6
                                    var t1098 uint32 = t1097 << t1098_rhs
                                    var t1099 uint32 = t1096 | t1098
                                    var t1100_rhs uint32 = 63
                                    var t1100 uint32 = fourth__14 & t1100_rhs
                                    var t1101 uint32 = t1099 | t1100
                                    var t1102 Tuple3_4bool_4char_3int = utf8_valid_decode(t1101, 4)
                                    return t1102
                                }
                            }
                        } else {
                            var t1123 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1123
                        }
                    }
                }
            }
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1133 bool = value__4 <= 1114111
    if t1133 {
        var t1137 bool = value__4 >= 55296
        var jp1135 bool
        if t1137 {
            var t1138 bool = value__4 <= 57343
            jp1135 = t1138
        } else {
            jp1135 = false
        }
        var t1136 bool = !jp1135
        return t1136
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1141 int = _goml_runtime_core_string_len(self__36)
    return t1141
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1144 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1144
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1147 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1147
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1840 rune
    var inline1815 bool = utf8_valid_scalar(value__0)
    if inline1815 {
        var inline1816 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1817 rune = inline1816._1
        commute_field1840 = inline1817
        var t1153 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1840,
            _2: width__1,
        }
        return t1153
    } else {
        var inline1813 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1813
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1158 bool = value__3 < 128
    if t1158 {
        return true
    } else {
        var t1159 bool = value__3 > 191
        return t1159
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1164 bool
    var inline1821 bool = value__30 <= 1114111
    if inline1821 {
        var inline1822 bool = value__30 >= 55296
        var inline1824 bool
        if inline1822 {
            var inline1826 bool = value__30 <= 57343
            inline1824 = inline1826
        } else {
            inline1824 = false
        }
        var inline1825 bool = !inline1824
        t1164 = inline1825
    } else {
        t1164 = false
    }
    if t1164 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1165 Option__char = Some{
            _0: x24,
        }
        return t1165
    } else {
        return None{}
    }
}

func main() {
    main0()
}
