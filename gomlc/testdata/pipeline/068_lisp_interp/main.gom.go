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
    var inline1275 int = _goml_runtime_core_string_len(text__2)
    len__3 = inline1275
    var t337 bool = len__3 == 0
    if t337 {
        return false
    } else {
        var i__4 *ref_int_x
        var inline1272 int = 0
        var inline1273 *ref_int_x = ref__Ref_3int(inline1272)
        i__4 = inline1273
        var saw_digit__5 *ref_bool_x
        var inline1269 bool = false
        var inline1270 *ref_bool_x = ref__Ref_4bool(inline1269)
        saw_digit__5 = inline1270
        var ok__6 *ref_bool_x
        var inline1266 bool = true
        var inline1267 *ref_bool_x = ref__Ref_4bool(inline1266)
        ok__6 = inline1267
        var started__7 *ref_bool_x
        var inline1263 bool = false
        var inline1264 *ref_bool_x = ref__Ref_4bool(inline1263)
        started__7 = inline1264
        Loop_loop343:
        for {
            var t362 bool
            var inline1257 bool = ref_get__Ref_4bool(ok__6)
            t362 = inline1257
            var jp345 bool
            if t362 {
                var t363 int
                var inline1226 int = ref_get__Ref_3int(i__4)
                t363 = inline1226
                var t364 bool = t363 < len__3
                jp345 = t364
            } else {
                jp345 = false
            }
            if jp345 {
                var t346 int
                var inline1255 int = ref_get__Ref_3int(i__4)
                t346 = inline1255
                var ch__8 rune
                var inline1253 rune = string_get(text__2, t346)
                ch__8 = inline1253
                var t359 bool
                var inline1251 bool = ref_get__Ref_4bool(started__7)
                t359 = inline1251
                var t360 bool = !t359
                var jp349 bool
                if t360 {
                    var t361 bool = ch__8 == 45
                    jp349 = t361
                } else {
                    jp349 = false
                }
                if jp349 {
                    var inline1232 bool = true
                    ref_set__Ref_4bool(started__7, inline1232)
                    var t350 int
                    var inline1230 int = ref_get__Ref_3int(i__4)
                    t350 = inline1230
                    var t351 int = t350 + 1
                    ref_set__Ref_3int(i__4, t351)
                    continue
                } else {
                    var t354 bool
                    var inline1248 bool = ch__8 >= 48
                    if inline1248 {
                        var inline1249 bool = ch__8 <= 57
                        t354 = inline1249
                    } else {
                        t354 = false
                    }
                    if t354 {
                        var inline1242 bool = true
                        ref_set__Ref_4bool(started__7, inline1242)
                        var inline1239 bool = true
                        ref_set__Ref_4bool(saw_digit__5, inline1239)
                        var t355 int
                        var inline1237 int = ref_get__Ref_3int(i__4)
                        t355 = inline1237
                        var t356 int = t355 + 1
                        ref_set__Ref_3int(i__4, t356)
                        continue
                    } else {
                        var inline1245 bool = false
                        ref_set__Ref_4bool(ok__6, inline1245)
                        continue
                    }
                }
            } else {
                break Loop_loop343
            }
        }
        var t341 bool
        var inline1261 bool = ref_get__Ref_4bool(ok__6)
        t341 = inline1261
        if t341 {
            var inline1259 bool = ref_get__Ref_4bool(saw_digit__5)
            return inline1259
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
    var inline1316 bool = false
    var inline1317 *ref_bool_x = ref__Ref_4bool(inline1316)
    started__13 = inline1317
    var acc__14 *ref_int32_x
    var inline1313 int32 = 0
    var inline1314 *ref_int32_x = ref__Ref_5int32(inline1313)
    acc__14 = inline1314
    Loop_loop374:
    for {
        var t375 int
        var inline1305 int = ref_get__Ref_3int(i__11)
        t375 = inline1305
        var t376 bool = t375 < len__10
        if t376 {
            var t377 int
            var inline1303 int = ref_get__Ref_3int(i__11)
            t377 = inline1303
            var ch__15 rune
            var inline1301 rune = string_get(text__9, t377)
            ch__15 = inline1301
            var t390 bool
            var inline1299 bool = ref_get__Ref_4bool(started__13)
            t390 = inline1299
            var t391 bool = !t390
            var jp380 bool
            if t391 {
                var t392 bool = ch__15 == 45
                jp380 = t392
            } else {
                jp380 = false
            }
            if jp380 {
                var inline1284 bool = true
                ref_set__Ref_4bool(started__13, inline1284)
                var inline1281 bool = true
                ref_set__Ref_4bool(negative__12, inline1281)
                var t381 int
                var inline1279 int = ref_get__Ref_3int(i__11)
                t381 = inline1279
                var t382 int = t381 + 1
                ref_set__Ref_3int(i__11, t382)
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
                var t384 int32
                var inline1293 int32 = ref_get__Ref_5int32(acc__14)
                t384 = inline1293
                var t385 int32 = t384 * 10
                var t386 int32 = t385 + d__16
                ref_set__Ref_5int32(acc__14, t386)
                var t387 int
                var inline1289 int = ref_get__Ref_3int(i__11)
                t387 = inline1289
                var t388 int = t387 + 1
                ref_set__Ref_3int(i__11, t388)
                continue
            }
        } else {
            break Loop_loop374
        }
    }
    var t370 bool
    var inline1311 bool = ref_get__Ref_4bool(negative__12)
    t370 = inline1311
    if t370 {
        var t371 int32
        var inline1307 int32 = ref_get__Ref_5int32(acc__14)
        t371 = inline1307
        var t372 int32 = 0 - t371
        return t372
    } else {
        var inline1309 int32 = ref_get__Ref_5int32(acc__14)
        return inline1309
    }
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var len__20 int
    var inline1358 int = _goml_runtime_core_string_len(source__18)
    len__20 = inline1358
    var text__21 *ref_string_x
    var inline1355 string = ""
    var inline1356 *ref_string_x = ref__Ref_6string(inline1355)
    text__21 = inline1356
    var i__22 *ref_int_x
    var inline1353 *ref_int_x = ref__Ref_3int(start__19)
    i__22 = inline1353
    var done__23 *ref_bool_x
    var inline1350 bool = false
    var inline1351 *ref_bool_x = ref__Ref_4bool(inline1350)
    done__23 = inline1351
    Loop_loop417:
    for {
        var t430 bool
        var inline1344 bool = ref_get__Ref_4bool(done__23)
        t430 = inline1344
        var t431 bool = !t430
        var jp419 bool
        if t431 {
            var t432 int
            var inline1319 int = ref_get__Ref_3int(i__22)
            t432 = inline1319
            var t433 bool = t432 < len__20
            jp419 = t433
        } else {
            jp419 = false
        }
        if jp419 {
            var t420 int
            var inline1342 int = ref_get__Ref_3int(i__22)
            t420 = inline1342
            var ch__24 rune
            var inline1340 rune = string_get(source__18, t420)
            ch__24 = inline1340
            var t422 bool
            var inline1334 bool = ch__24 == 40
            var inline1336 bool
            if inline1334 {
                inline1336 = true
            } else {
                var inline1338 bool = ch__24 == 41
                inline1336 = inline1338
            }
            if inline1336 {
                t422 = true
                if t422 {
                    var inline1321 bool = true
                    ref_set__Ref_4bool(done__23, inline1321)
                    continue
                } else {
                    var t424 string
                    var inline1332 string = ref_get__Ref_6string(text__21)
                    t424 = inline1332
                    var t425 string
                    var inline1330 string = char_to_string(ch__24)
                    t425 = inline1330
                    var t426 string = t424 + t425
                    ref_set__Ref_6string(text__21, t426)
                    var t427 int
                    var inline1326 int = ref_get__Ref_3int(i__22)
                    t427 = inline1326
                    var t428 int = t427 + 1
                    ref_set__Ref_3int(i__22, t428)
                    continue
                }
            } else {
                var inline1337 bool = ch__24 == 32
                t422 = inline1337
                if t422 {
                    var inline1321 bool = true
                    ref_set__Ref_4bool(done__23, inline1321)
                    continue
                } else {
                    var t424 string
                    var inline1332 string = ref_get__Ref_6string(text__21)
                    t424 = inline1332
                    var t425 string
                    var inline1330 string = char_to_string(ch__24)
                    t425 = inline1330
                    var t426 string = t424 + t425
                    ref_set__Ref_6string(text__21, t426)
                    var t427 int
                    var inline1326 int = ref_get__Ref_3int(i__22)
                    t427 = inline1326
                    var t428 int = t427 + 1
                    ref_set__Ref_3int(i__22, t428)
                    continue
                }
            }
        } else {
            break Loop_loop417
        }
    }
    var atom__25 string
    var inline1348 string = ref_get__Ref_6string(text__21)
    atom__25 = inline1348
    var jp406 Token
    switch atom__25 {
    case "true":
        var t409 Token = Token_Bool{
            _0: true,
        }
        jp406 = t409
    case "false":
        var t410 Token = Token_Bool{
            _0: false,
        }
        jp406 = t410
    default:
        var t413 bool = is_int_text(atom__25)
        if t413 {
            var t414 int32 = parse_int32(atom__25)
            var t415 Token = Token_Int{
                _0: t414,
            }
            jp406 = t415
        } else {
            var t416 Token = Token_Sym{
                _0: atom__25,
            }
            jp406 = t416
        }
    }
    var t407 int
    var inline1346 int = ref_get__Ref_3int(i__22)
    t407 = inline1346
    var t408 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: jp406,
        _1: t407,
    }
    return t408
}

func lex(source__27 string) *_goml_vec_Token {
    var len__28 int
    var inline1403 int = _goml_runtime_core_string_len(source__27)
    len__28 = inline1403
    var toks0__29 *_goml_vec_Token
    var inline1401 *_goml_vec_Token = vec_new__Vec_5Token()
    toks0__29 = inline1401
    var toks__30 *ref_Vec_5Token_x
    var inline1399 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    toks__30 = inline1399
    var i__31 *ref_int_x
    var inline1396 int = 0
    var inline1397 *ref_int_x = ref__Ref_3int(inline1396)
    i__31 = inline1397
    Loop_loop438:
    for {
        var t439 int
        var inline1392 int = ref_get__Ref_3int(i__31)
        t439 = inline1392
        var t440 bool = t439 < len__28
        if t440 {
            var t441 int
            var inline1390 int = ref_get__Ref_3int(i__31)
            t441 = inline1390
            var ch__32 rune
            var inline1388 rune = string_get(source__27, t441)
            ch__32 = inline1388
            var t443 bool = ch__32 == 40
            if t443 {
                var t444 *_goml_vec_Token
                var inline1366 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                t444 = inline1366
                var t445 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t444, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t445)
                var t446 int
                var inline1362 int = ref_get__Ref_3int(i__31)
                t446 = inline1362
                var t447 int = t446 + 1
                ref_set__Ref_3int(i__31, t447)
                continue
            } else {
                var t450 bool = ch__32 == 41
                if t450 {
                    var t451 *_goml_vec_Token
                    var inline1374 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                    t451 = inline1374
                    var t452 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t451, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t452)
                    var t453 int
                    var inline1370 int = ref_get__Ref_3int(i__31)
                    t453 = inline1370
                    var t454 int = t453 + 1
                    ref_set__Ref_3int(i__31, t454)
                    continue
                } else {
                    var t457 bool = ch__32 == 32
                    if t457 {
                        var t458 int
                        var inline1378 int = ref_get__Ref_3int(i__31)
                        t458 = inline1378
                        var t459 int = t458 + 1
                        ref_set__Ref_3int(i__31, t459)
                        continue
                    } else {
                        var t461 int
                        var inline1386 int = ref_get__Ref_3int(i__31)
                        t461 = inline1386
                        var mtmp200 Tuple2_5Token_3int = lex_atom(source__27, t461)
                        var x201 Token = mtmp200._0
                        var x202 int = mtmp200._1
                        var t462 *_goml_vec_Token
                        var inline1384 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                        t462 = inline1384
                        var t463 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t462, x201)
                        ref_set__Ref_10Vec_5Token(toks__30, t463)
                        ref_set__Ref_3int(i__31, x202)
                        continue
                    }
                }
            }
        } else {
            break Loop_loop438
        }
    }
    var inline1394 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
    return inline1394
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var t467 int
    var inline1429 int = vec_len__Vec_7Binding(env__35)
    t467 = inline1429
    var t468 int = t467 - 1
    var i__37 *ref_int_x
    var inline1427 *ref_int_x = ref__Ref_3int(t468)
    i__37 = inline1427
    var result__38 *ref_Value_x
    var inline1425 *ref_Value_x = ref__Ref_5Value(Nil{})
    result__38 = inline1425
    var done__39 *ref_bool_x
    var inline1422 bool = false
    var inline1423 *ref_bool_x = ref__Ref_4bool(inline1422)
    done__39 = inline1423
    Loop_loop471:
    for {
        var t483 bool
        var inline1418 bool = ref_get__Ref_4bool(done__39)
        t483 = inline1418
        var t484 bool = !t483
        var jp473 bool
        if t484 {
            var t485 int
            var inline1405 int = ref_get__Ref_3int(i__37)
            t485 = inline1405
            var t486 bool = t485 >= 0
            jp473 = t486
        } else {
            jp473 = false
        }
        if jp473 {
            var t474 int
            var inline1416 int = ref_get__Ref_3int(i__37)
            t474 = inline1416
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t474)
            var t476 string = binding__40.name
            var t477 bool = t476 == name__36
            if t477 {
                var t478 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t478)
                var inline1407 bool = true
                ref_set__Ref_4bool(done__39, inline1407)
                continue
            } else {
                var t480 int
                var inline1414 int = ref_get__Ref_3int(i__37)
                t480 = inline1414
                var t481 int = t480 - 1
                ref_set__Ref_3int(i__37, t481)
                continue
            }
        } else {
            break Loop_loop471
        }
    }
    var inline1420 Value = ref_get__Ref_5Value(result__38)
    return inline1420
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var acc__47 *_goml_vec_SExpr
    var inline1465 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__47 = inline1465
    var exprs__48 *ref_Vec_5SExpr_x
    var inline1463 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    exprs__48 = inline1463
    var i__49 *ref_int_x
    var inline1461 *ref_int_x = ref__Ref_3int(start__46)
    i__49 = inline1461
    var done__50 *ref_bool_x
    var inline1458 bool = false
    var inline1459 *ref_bool_x = ref__Ref_4bool(inline1458)
    done__50 = inline1459
    Loop_loop498:
    for {
        var t510 bool
        var inline1452 bool = ref_get__Ref_4bool(done__50)
        t510 = inline1452
        var t511 bool = !t510
        var jp500 bool
        if t511 {
            var t512 int
            var inline1433 int = ref_get__Ref_3int(i__49)
            t512 = inline1433
            var t513 int
            var inline1431 int = vec_len__Vec_5Token(tokens__45)
            t513 = inline1431
            var t514 bool = t512 < t513
            jp500 = t514
        } else {
            jp500 = false
        }
        if jp500 {
            var t501 int
            var inline1450 int = ref_get__Ref_3int(i__49)
            t501 = inline1450
            var mtmp211 Token = vec_get__Vec_5Token(tokens__45, t501)
            switch mtmp211.(type) {
            case RParen:
                var inline1439 bool = true
                ref_set__Ref_4bool(done__50, inline1439)
                var t503 int
                var inline1437 int = ref_get__Ref_3int(i__49)
                t503 = inline1437
                var t504 int = t503 + 1
                ref_set__Ref_3int(i__49, t504)
                continue
            default:
                var t506 int
                var inline1448 int = ref_get__Ref_3int(i__49)
                t506 = inline1448
                var mtmp216 Tuple2_5SExpr_3int = parse_expr(tokens__45, t506)
                var x217 SExpr = mtmp216._0
                var x218 int = mtmp216._1
                var t507 *_goml_vec_SExpr
                var inline1446 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                t507 = inline1446
                var t508 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t507, x217)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t508)
                ref_set__Ref_3int(i__49, x218)
                continue
            }
        } else {
            break Loop_loop498
        }
    }
    var t495 *_goml_vec_SExpr
    var inline1456 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    t495 = inline1456
    var t496 int
    var inline1454 int = ref_get__Ref_3int(i__49)
    t496 = inline1454
    var t497 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t495,
        _1: t496,
    }
    return t497
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var mtmp221 Token = vec_get__Vec_5Token(tokens__53, start__54)
    switch mtmp221.(type) {
    case LParen:
        var t519 int = start__54 + 1
        var mtmp225 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t519)
        var x226 *_goml_vec_SExpr = mtmp225._0
        var x227 int = mtmp225._1
        var t520 SExpr = List{
            _0: x226,
        }
        var t521 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t520,
            _1: x227,
        }
        return t521
    case RParen:
        var t522 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t523 int = start__54 + 1
        var t524 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t522,
            _1: t523,
        }
        return t524
    case Token_Sym:
        var x222 string = mtmp221.(Token_Sym)._0
        var t525 SExpr = SExpr_Sym{
            _0: x222,
        }
        var t526 int = start__54 + 1
        var t527 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t525,
            _1: t526,
        }
        return t527
    case Token_Int:
        var x223 int32 = mtmp221.(Token_Int)._0
        var t528 SExpr = SExpr_Int{
            _0: x223,
        }
        var t529 int = start__54 + 1
        var t530 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t528,
            _1: t529,
        }
        return t530
    case Token_Bool:
        var x224 bool = mtmp221.(Token_Bool)._0
        var t531 SExpr = SExpr_Bool{
            _0: x224,
        }
        var t532 int = start__54 + 1
        var t533 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t531,
            _1: t532,
        }
        return t533
    default:
        panic("non-exhaustive match")
    }
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var i__61 *ref_int_x
    var inline1485 int = 0
    var inline1486 *ref_int_x = ref__Ref_3int(inline1485)
    i__61 = inline1486
    var acc__62 *_goml_vec_SExpr
    var inline1483 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__62 = inline1483
    var exprs__63 *ref_Vec_5SExpr_x
    var inline1481 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    exprs__63 = inline1481
    Loop_loop538:
    for {
        var t539 int
        var inline1477 int = ref_get__Ref_3int(i__61)
        t539 = inline1477
        var t540 int
        var inline1475 int = vec_len__Vec_5Token(tokens__60)
        t540 = inline1475
        var t541 bool = t539 < t540
        if t541 {
            var t542 int
            var inline1473 int = ref_get__Ref_3int(i__61)
            t542 = inline1473
            var mtmp228 Tuple2_5SExpr_3int = parse_expr(tokens__60, t542)
            var x229 SExpr = mtmp228._0
            var x230 int = mtmp228._1
            var t543 *_goml_vec_SExpr
            var inline1471 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            t543 = inline1471
            var t544 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t543, x229)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t544)
            ref_set__Ref_3int(i__61, x230)
            continue
        } else {
            break Loop_loop538
        }
    }
    var inline1479 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    return inline1479
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    switch expr__72.(type) {
    case SExpr_Int:
        var x239 int32 = expr__72.(SExpr_Int)._0
        var t561 Value = Value_Int{
            _0: x239,
        }
        return t561
    case SExpr_Bool:
        var x240 bool = expr__72.(SExpr_Bool)._0
        var t562 Value = Value_Bool{
            _0: x240,
        }
        return t562
    case SExpr_Sym:
        var x241 string = expr__72.(SExpr_Sym)._0
        var t563 *_goml_vec_Binding
        var inline1496 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__74)
        t563 = inline1496
        var inline1492 Value = env_lookup(local__73, x241)
        switch inline1492.(type) {
        case Nil:
            var inline1493 Value = env_lookup(t563, x241)
            return inline1493
        default:
            return inline1492
        }
    case List:
        var x242 *_goml_vec_SExpr = expr__72.(List)._0
        var inline1498 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(x242)
        var inline1499 bool = inline1498 == 0
        if inline1499 {
            return Nil{}
        } else {
            var inline1500 SExpr = vec_get__Vec_5SExpr(x242, 0)
            switch inline1500.(type) {
            case SExpr_Sym:
                var inline1501 string = inline1500.(SExpr_Sym)._0
                var inline1503 Value = eval_list_sym(inline1501, x242, local__73, global__74)
                return inline1503
            default:
                var inline1504 Value = eval(inline1500, local__73, global__74)
                var inline1505 *_goml_vec_Value = eval_args(x242, 1, local__73, global__74)
                var inline1506 Value = apply(inline1504, inline1505, global__74)
                return inline1506
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    switch name__86 {
    case "begin":
        var t580 Value = eval_begin(items__87, 1, local__88, global__89)
        return t580
    case "define":
        var t583 int
        var inline1518 int = vec_len__Vec_5SExpr(items__87)
        t583 = inline1518
        var t584 bool = t583 == 3
        if t584 {
            var mtmp247 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp247.(type) {
            case SExpr_Sym:
                var x250 string = mtmp247.(SExpr_Sym)._0
                var t587 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t587, local__88, global__89)
                var env__92 *_goml_vec_Binding
                var inline1516 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__89)
                env__92 = inline1516
                var t588 Binding = Binding{
                    name: x250,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t588)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                return value__91
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "if":
        var t591 int
        var inline1526 int = vec_len__Vec_5SExpr(items__87)
        t591 = inline1526
        var t592 bool = t591 == 4
        if t592 {
            var t593 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t593, local__88, global__89)
            var t596 bool
            switch cond__94.(type) {
            case Value_Int:
                var inline1520 int32 = cond__94.(Value_Int)._0
                var inline1522 bool = inline1520 != 0
                t596 = inline1522
            case Value_Bool:
                var inline1523 bool = cond__94.(Value_Bool)._0
                t596 = inline1523
            case Func:
                t596 = true
            case Nil:
                t596 = false
            default:
                panic("non-exhaustive match")
            }
            if t596 {
                var t597 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t598 Value = eval(t597, local__88, global__89)
                return t598
            } else {
                var t599 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t600 Value = eval(t599, local__88, global__89)
                return t600
            }
        } else {
            return Nil{}
        }
    case "lambda":
        var t603 int
        var inline1528 int = vec_len__Vec_5SExpr(items__87)
        t603 = inline1528
        var t604 bool = t603 == 3
        if t604 {
            var mtmp253 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp253.(type) {
            case List:
                var x257 *_goml_vec_SExpr = mtmp253.(List)._0
                var params__96 *_goml_vec_string = params_from_sexprs(x257)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t607 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t608 Value = Func{
                    _0: t607,
                }
                return t608
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "+":
        var t609 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t610 Value = apply_builtin("+", t609)
        return t610
    case "-":
        var t611 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t612 Value = apply_builtin("-", t611)
        return t612
    case "*":
        var t613 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t614 Value = apply_builtin("*", t613)
        return t614
    case "/":
        var t615 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t616 Value = apply_builtin("/", t615)
        return t616
    case "=":
        var t617 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t618 Value = apply_builtin("=", t617)
        return t618
    default:
        var t619 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t619, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        switch f__98.(type) {
        case Func:
            var inline1530 Lambda = f__98.(Func)._0
            var inline1532 Value = apply_lambda(inline1530, args__99)
            return inline1532
        default:
            return Nil{}
        }
    }
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var i__104 *ref_int_x
    var inline1550 *ref_int_x = ref__Ref_3int(start__101)
    i__104 = inline1550
    var last__105 *ref_Value_x
    var inline1548 *ref_Value_x = ref__Ref_5Value(Nil{})
    last__105 = inline1548
    Loop_loop625:
    for {
        var t626 int
        var inline1544 int = ref_get__Ref_3int(i__104)
        t626 = inline1544
        var t627 int
        var inline1542 int = vec_len__Vec_5SExpr(items__100)
        t627 = inline1542
        var t628 bool = t626 < t627
        if t628 {
            var t629 int
            var inline1540 int = ref_get__Ref_3int(i__104)
            t629 = inline1540
            var t630 SExpr = vec_get__Vec_5SExpr(items__100, t629)
            var v__106 Value = eval(t630, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t631 int
            var inline1536 int = ref_get__Ref_3int(i__104)
            t631 = inline1536
            var t632 int = t631 + 1
            ref_set__Ref_3int(i__104, t632)
            continue
        } else {
            break Loop_loop625
        }
    }
    var inline1546 Value = ref_get__Ref_5Value(last__105)
    return inline1546
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var i__108 *ref_int_x
    var inline1576 int = 0
    var inline1577 *ref_int_x = ref__Ref_3int(inline1576)
    i__108 = inline1577
    var acc__109 *_goml_vec_string
    var inline1574 *_goml_vec_string = vec_new__Vec_6string()
    acc__109 = inline1574
    var params__110 *ref_Vec_6string_x
    var inline1572 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    params__110 = inline1572
    Loop_loop638:
    for {
        var t639 int
        var inline1568 int = ref_get__Ref_3int(i__108)
        t639 = inline1568
        var t640 int
        var inline1566 int = vec_len__Vec_5SExpr(items__107)
        t640 = inline1566
        var t641 bool = t639 < t640
        if t641 {
            var t642 int
            var inline1564 int = ref_get__Ref_3int(i__108)
            t642 = inline1564
            var mtmp260 SExpr = vec_get__Vec_5SExpr(items__107, t642)
            switch mtmp260.(type) {
            case SExpr_Sym:
                var x263 string = mtmp260.(SExpr_Sym)._0
                var t644 *_goml_vec_string
                var inline1558 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
                t644 = inline1558
                var t645 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t644, x263)
                ref_set__Ref_11Vec_6string(params__110, t645)
                var t646 int
                var inline1554 int = ref_get__Ref_3int(i__108)
                t646 = inline1554
                var t647 int = t646 + 1
                ref_set__Ref_3int(i__108, t647)
                continue
            default:
                var t649 int
                var inline1562 int = ref_get__Ref_3int(i__108)
                t649 = inline1562
                var t650 int = t649 + 1
                ref_set__Ref_3int(i__108, t650)
                continue
            }
        } else {
            break Loop_loop638
        }
    }
    var inline1570 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
    return inline1570
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var i__116 *ref_int_x
    var inline1599 *ref_int_x = ref__Ref_3int(start__113)
    i__116 = inline1599
    var acc__117 *_goml_vec_Value
    var inline1597 *_goml_vec_Value = vec_new__Vec_5Value()
    acc__117 = inline1597
    var args__118 *ref_Vec_5Value_x
    var inline1595 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    args__118 = inline1595
    Loop_loop656:
    for {
        var t657 int
        var inline1591 int = ref_get__Ref_3int(i__116)
        t657 = inline1591
        var t658 int
        var inline1589 int = vec_len__Vec_5SExpr(items__112)
        t658 = inline1589
        var t659 bool = t657 < t658
        if t659 {
            var t660 int
            var inline1587 int = ref_get__Ref_3int(i__116)
            t660 = inline1587
            var t661 SExpr = vec_get__Vec_5SExpr(items__112, t660)
            var v__119 Value = eval(t661, local__114, global__115)
            var t662 *_goml_vec_Value
            var inline1585 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
            t662 = inline1585
            var t663 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t662, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t663)
            var t664 int
            var inline1581 int = ref_get__Ref_3int(i__116)
            t664 = inline1581
            var t665 int = t664 + 1
            ref_set__Ref_3int(i__116, t665)
            continue
        } else {
            break Loop_loop656
        }
    }
    var inline1593 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
    return inline1593
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    switch name__120 {
    case "=":
        var t673 int
        var inline1601 int = vec_len__Vec_5Value(args__121)
        t673 = inline1601
        var t674 bool = t673 == 2
        if t674 {
            var t675 Value = vec_get__Vec_5Value(args__121, 0)
            var t676 Value = vec_get__Vec_5Value(args__121, 1)
            switch t676.(type) {
            case Value_Int:
                var x272 int32 = t676.(Value_Int)._0
                switch t675.(type) {
                case Value_Int:
                    var x275 int32 = t675.(Value_Int)._0
                    var t681 bool = x275 == x272
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
            case Value_Bool:
                var x273 bool = t676.(Value_Bool)._0
                switch t675.(type) {
                case Value_Bool:
                    var x279 bool = t675.(Value_Bool)._0
                    var t686 bool = x279 == x273
                    var t687 Value = Value_Bool{
                        _0: t686,
                    }
                    return t687
                default:
                    var t688 Value = Value_Bool{
                        _0: false,
                    }
                    return t688
                }
            default:
                var t689 Value = Value_Bool{
                    _0: false,
                }
                return t689
            }
        } else {
            var t690 Value = Value_Bool{
                _0: false,
            }
            return t690
        }
    case "+":
        var i__126 *ref_int_x
        var inline1626 int = 0
        var inline1627 *ref_int_x = ref__Ref_3int(inline1626)
        i__126 = inline1627
        var acc__127 *ref_int32_x
        var inline1623 int32 = 0
        var inline1624 *ref_int32_x = ref__Ref_5int32(inline1623)
        acc__127 = inline1624
        Loop_loop694:
        for {
            var t695 int
            var inline1619 int = ref_get__Ref_3int(i__126)
            t695 = inline1619
            var t696 int
            var inline1617 int = vec_len__Vec_5Value(args__121)
            t696 = inline1617
            var t697 bool = t695 < t696
            if t697 {
                var t698 int
                var inline1615 int = ref_get__Ref_3int(i__126)
                t698 = inline1615
                var mtmp281 Value = vec_get__Vec_5Value(args__121, t698)
                switch mtmp281.(type) {
                case Value_Int:
                    var x282 int32 = mtmp281.(Value_Int)._0
                    var t700 int32
                    var inline1609 int32 = ref_get__Ref_5int32(acc__127)
                    t700 = inline1609
                    var t701 int32 = t700 + x282
                    ref_set__Ref_5int32(acc__127, t701)
                    var t702 int
                    var inline1605 int = ref_get__Ref_3int(i__126)
                    t702 = inline1605
                    var t703 int = t702 + 1
                    ref_set__Ref_3int(i__126, t703)
                    continue
                default:
                    var t705 int
                    var inline1613 int = ref_get__Ref_3int(i__126)
                    t705 = inline1613
                    var t706 int = t705 + 1
                    ref_set__Ref_3int(i__126, t706)
                    continue
                }
            } else {
                break Loop_loop694
            }
        }
        var t692 int32
        var inline1621 int32 = ref_get__Ref_5int32(acc__127)
        t692 = inline1621
        var t693 Value = Value_Int{
            _0: t692,
        }
        return t693
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x
        var inline1649 int32 = 1
        var inline1650 *ref_int32_x = ref__Ref_5int32(inline1649)
        acc__130 = inline1650
        Loop_loop711:
        for {
            var t712 int
            var inline1645 int = ref_get__Ref_3int(i__129)
            t712 = inline1645
            var t713 int
            var inline1643 int = vec_len__Vec_5Value(args__121)
            t713 = inline1643
            var t714 bool = t712 < t713
            if t714 {
                var t715 int
                var inline1641 int = ref_get__Ref_3int(i__129)
                t715 = inline1641
                var mtmp287 Value = vec_get__Vec_5Value(args__121, t715)
                switch mtmp287.(type) {
                case Value_Int:
                    var x288 int32 = mtmp287.(Value_Int)._0
                    var t717 int32
                    var inline1635 int32 = ref_get__Ref_5int32(acc__130)
                    t717 = inline1635
                    var t718 int32 = t717 * x288
                    ref_set__Ref_5int32(acc__130, t718)
                    var t719 int
                    var inline1631 int = ref_get__Ref_3int(i__129)
                    t719 = inline1631
                    var t720 int = t719 + 1
                    ref_set__Ref_3int(i__129, t720)
                    continue
                default:
                    var t722 int
                    var inline1639 int = ref_get__Ref_3int(i__129)
                    t722 = inline1639
                    var t723 int = t722 + 1
                    ref_set__Ref_3int(i__129, t723)
                    continue
                }
            } else {
                break Loop_loop711
            }
        }
        var t709 int32
        var inline1647 int32 = ref_get__Ref_5int32(acc__130)
        t709 = inline1647
        var t710 Value = Value_Int{
            _0: t709,
        }
        return t710
    case "-":
        var mtmp293 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        switch mtmp293 {
        case 1:
            var mtmp294 Value = vec_get__Vec_5Value(args__121, 0)
            switch mtmp294.(type) {
            case Value_Int:
                var x295 int32 = mtmp294.(Value_Int)._0
                var t729 int32 = 0 - x295
                var t730 Value = Value_Int{
                    _0: t729,
                }
                return t730
            default:
                return Nil{}
            }
        case 2:
            var t731 Value = vec_get__Vec_5Value(args__121, 0)
            var t732 Value = vec_get__Vec_5Value(args__121, 1)
            switch t732.(type) {
            case Value_Int:
                var x301 int32 = t732.(Value_Int)._0
                switch t731.(type) {
                case Value_Int:
                    var x304 int32 = t731.(Value_Int)._0
                    var t737 int32 = x304 - x301
                    var t738 Value = Value_Int{
                        _0: t737,
                    }
                    return t738
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
        var t741 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t742 bool = t741 == 2
        if t742 {
            var t743 Value = vec_get__Vec_5Value(args__121, 0)
            var t744 Value = vec_get__Vec_5Value(args__121, 1)
            switch t744.(type) {
            case Value_Int:
                var x310 int32 = t744.(Value_Int)._0
                switch t743.(type) {
                case Value_Int:
                    var x313 int32 = t743.(Value_Int)._0
                    var t749 int32 = x313 / x310
                    var t750 Value = Value_Int{
                        _0: t749,
                    }
                    return t750
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
        var x318 Lambda = func__137.(Func)._0
        var t755 Value = apply_lambda(x318, args__138)
        return t755
    default:
        return Nil{}
    }
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var t758 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x
    var inline1677 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t758)
    env__143 = inline1677
    var i__144 *ref_int_x
    var inline1674 int = 0
    var inline1675 *ref_int_x = ref__Ref_3int(inline1674)
    i__144 = inline1675
    Loop_loop764:
    for {
        var t775 int
        var inline1670 int = ref_get__Ref_3int(i__144)
        t775 = inline1670
        var t776 *_goml_vec_string = lambda__141.params
        var t777 int
        var inline1668 int = vec_len__Vec_6string(t776)
        t777 = inline1668
        var t778 bool = t775 < t777
        var jp766 bool
        if t778 {
            var t779 int
            var inline1654 int = ref_get__Ref_3int(i__144)
            t779 = inline1654
            var t780 int
            var inline1652 int = vec_len__Vec_5Value(args__142)
            t780 = inline1652
            var t781 bool = t779 < t780
            jp766 = t781
        } else {
            jp766 = false
        }
        if jp766 {
            var t767 *_goml_vec_string = lambda__141.params
            var t768 int
            var inline1666 int = ref_get__Ref_3int(i__144)
            t768 = inline1666
            var name__145 string = vec_get__Vec_6string(t767, t768)
            var t769 int
            var inline1664 int = ref_get__Ref_3int(i__144)
            t769 = inline1664
            var value__146 Value = vec_get__Vec_5Value(args__142, t769)
            var t770 *_goml_vec_Binding
            var inline1662 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
            t770 = inline1662
            var t771 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t770, t771)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t772 int
            var inline1658 int = ref_get__Ref_3int(i__144)
            t772 = inline1658
            var t773 int = t772 + 1
            ref_set__Ref_3int(i__144, t773)
            continue
        } else {
            break Loop_loop764
        }
    }
    var t760 SExpr = lambda__141.body
    var t761 *_goml_vec_Binding
    var inline1672 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
    t761 = inline1672
    var t762 *ref_Vec_7Binding_x = lambda__141.global
    var t763 Value = eval(t760, t761, t762)
    return t763
}

func main0() struct{} {
    var t783 *_goml_vec_Binding
    var inline1705 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t783 = inline1705
    var global__148 *ref_Vec_7Binding_x
    var inline1703 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t783)
    global__148 = inline1703
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t784 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t784)
    var t785 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t786 *_goml_vec_Binding
    var inline1701 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t786 = inline1701
    var result__151 Value = eval(t785, t786, global__148)
    var t787 string
    switch result__151.(type) {
    case Value_Int:
        var inline1694 int32 = result__151.(Value_Int)._0
        var inline1696 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1694)
        t787 = inline1696
    case Value_Bool:
        var inline1697 bool = result__151.(Value_Bool)._0
        var inline1699 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1697)
        t787 = inline1699
    case Func:
        t787 = "<lambda>"
    case Nil:
        t787 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1691 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t787)
    _goml_runtime_core_string_println(inline1691)
    var t788 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t788)
    var t789 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t790 *_goml_vec_Binding
    var inline1689 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t790 = inline1689
    var result2__153 Value = eval(t789, t790, global__148)
    var t791 string
    switch result2__153.(type) {
    case Value_Int:
        var inline1682 int32 = result2__153.(Value_Int)._0
        var inline1684 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1682)
        t791 = inline1684
    case Value_Bool:
        var inline1685 bool = result2__153.(Value_Bool)._0
        var inline1687 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1685)
        t791 = inline1687
    case Func:
        t791 = "<lambda>"
    case Nil:
        t791 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1679 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t791)
    _goml_runtime_core_string_println(inline1679)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__35 string) int {
    var t794 int = _goml_runtime_core_string_len(self__35)
    return t794
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__273 int) *ref_int_x {
    var t797 *ref_int_x = ref__Ref_3int(value__273)
    return t797
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__273 bool) *ref_bool_x {
    var t800 *ref_bool_x = ref__Ref_4bool(value__273)
    return t800
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__176 *_goml_vec_Token, elem__177 Token) *_goml_vec_Token {
    var t844 int
    var inline1727 int = vec_len__Vec_5Token(self__176)
    t844 = inline1727
    var t845 int = t844 + 1
    var result__178 *_goml_vec_Token
    var inline1725 *_goml_vec_Token = vec_with_capacity__Vec_5Token(t845)
    result__178 = inline1725
    var index__179 int = 0
    Loop_loop847:
    for {
        var t848 int
        var inline1721 int = vec_len__Vec_5Token(self__176)
        t848 = inline1721
        var t849 bool = index__179 < t848
        if t849 {
            var t850 Token = vec_get__Vec_5Token(self__176, index__179)
            vec_push__Vec_5Token(result__178, t850)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t851 int = compound_old80 + compound_value81
            index__179 = t851
            continue
        } else {
            break Loop_loop847
        }
    }
    vec_push__Vec_5Token(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__176 *_goml_vec_SExpr, elem__177 SExpr) *_goml_vec_SExpr {
    var t880 int
    var inline1737 int = vec_len__Vec_5SExpr(self__176)
    t880 = inline1737
    var t881 int = t880 + 1
    var result__178 *_goml_vec_SExpr
    var inline1735 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(t881)
    result__178 = inline1735
    var index__179 int = 0
    Loop_loop883:
    for {
        var t884 int
        var inline1731 int = vec_len__Vec_5SExpr(self__176)
        t884 = inline1731
        var t885 bool = index__179 < t884
        if t885 {
            var t886 SExpr = vec_get__Vec_5SExpr(self__176, index__179)
            vec_push__Vec_5SExpr(result__178, t886)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t887 int = compound_old80 + compound_value81
            index__179 = t887
            continue
        } else {
            break Loop_loop883
        }
    }
    vec_push__Vec_5SExpr(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t893 string = _goml_runtime_core_int32_to_string(self__33)
    return t893
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t896 string = _goml_runtime_core_bool_to_string(self__64)
    return t896
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__189 *_goml_vec_SExpr) int {
    var t902 int = vec_len__Vec_5SExpr(self__189)
    return t902
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__176 *_goml_vec_Binding, elem__177 Binding) *_goml_vec_Binding {
    var t905 int
    var inline1747 int = vec_len__Vec_7Binding(self__176)
    t905 = inline1747
    var t906 int = t905 + 1
    var result__178 *_goml_vec_Binding
    var inline1745 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(t906)
    result__178 = inline1745
    var index__179 int = 0
    Loop_loop908:
    for {
        var t909 int
        var inline1741 int = vec_len__Vec_7Binding(self__176)
        t909 = inline1741
        var t910 bool = index__179 < t909
        if t910 {
            var t911 Binding = vec_get__Vec_7Binding(self__176, index__179)
            vec_push__Vec_7Binding(result__178, t911)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t912 int = compound_old80 + compound_value81
            index__179 = t912
            continue
        } else {
            break Loop_loop908
        }
    }
    vec_push__Vec_7Binding(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__176 *_goml_vec_string, elem__177 string) *_goml_vec_string {
    var t927 int
    var inline1757 int = vec_len__Vec_6string(self__176)
    t927 = inline1757
    var t928 int = t927 + 1
    var result__178 *_goml_vec_string
    var inline1755 *_goml_vec_string = vec_with_capacity__Vec_6string(t928)
    result__178 = inline1755
    var index__179 int = 0
    Loop_loop930:
    for {
        var t931 int
        var inline1751 int = vec_len__Vec_6string(self__176)
        t931 = inline1751
        var t932 bool = index__179 < t931
        if t932 {
            var t933 string = vec_get__Vec_6string(self__176, index__179)
            vec_push__Vec_6string(result__178, t933)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t934 int = compound_old80 + compound_value81
            index__179 = t934
            continue
        } else {
            break Loop_loop930
        }
    }
    vec_push__Vec_6string(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__176 *_goml_vec_Value, elem__177 Value) *_goml_vec_Value {
    var t949 int
    var inline1767 int = vec_len__Vec_5Value(self__176)
    t949 = inline1767
    var t950 int = t949 + 1
    var result__178 *_goml_vec_Value
    var inline1765 *_goml_vec_Value = vec_with_capacity__Vec_5Value(t950)
    result__178 = inline1765
    var index__179 int = 0
    Loop_loop952:
    for {
        var t953 int
        var inline1761 int = vec_len__Vec_5Value(self__176)
        t953 = inline1761
        var t954 bool = index__179 < t953
        if t954 {
            var t955 Value = vec_get__Vec_5Value(self__176, index__179)
            vec_push__Vec_5Value(result__178, t955)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t956 int = compound_old80 + compound_value81
            index__179 = t956
            continue
        } else {
            break Loop_loop952
        }
    }
    vec_push__Vec_5Value(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__189 *_goml_vec_Value) int {
    var t962 int = vec_len__Vec_5Value(self__189)
    return t962
}

func string_get(value__17 string, index__18 int) rune {
    var mtmp6 Tuple3_4bool_4char_3int = string_decode_utf8_at(value__17, index__18)
    var x7 bool = mtmp6._0
    var x8 rune = mtmp6._1
    if x7 {
        return x8
    } else {
        var t979 rune = _goml_runtime_core_string_get("", -1)
        return t979
    }
}

func char_to_string(value__29 rune) string {
    var t984 uint32 = uint32(rune(value__29))
    var t985 bool
    var inline1770 bool = t984 <= 1114111
    if inline1770 {
        var inline1771 bool = t984 >= 55296
        var inline1773 bool
        if inline1771 {
            var inline1775 bool = t984 <= 57343
            inline1773 = inline1775
        } else {
            inline1773 = false
        }
        var inline1774 bool = !inline1773
        t985 = inline1774
    } else {
        t985 = false
    }
    if t985 {
        var t986 string = _goml_runtime_core_char_to_string(value__29)
        return t986
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
    var t1132 bool = index__6 < 0
    var jp1130 bool
    if t1132 {
        jp1130 = true
    } else {
        var t1133 bool = index__6 >= length__7
        jp1130 = t1133
    }
    if jp1130 {
        var inline1777 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1777
    } else {
        var t1017 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1017))
        var t1020 bool = first__8 < 128
        if t1020 {
            var inline1779 int = 1
            var inline1780 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1780.(type) {
            case None:
                var inline1781 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1781
            case Some:
                var inline1782 rune = inline1780.(Some)._0
                var inline1784 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1782,
                    _2: inline1779,
                }
                return inline1784
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1024 bool = first__8 < 194
            if t1024 {
                var inline1786 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1786
            } else {
                var t1028 bool = first__8 < 224
                if t1028 {
                    var t1041 int = length__7 - index__6
                    var t1042 bool = t1041 < 2
                    if t1042 {
                        var inline1788 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1788
                    } else {
                        var t1030 int = index__6 + 1
                        var t1031 uint8
                        var inline1802 uint8 = _goml_runtime_core_string_byte_get(value__5, t1030)
                        t1031 = inline1802
                        var second__9 uint32 = uint32(uint8(t1031))
                        var t1034 bool
                        var inline1799 bool = second__9 < 128
                        if inline1799 {
                            t1034 = true
                        } else {
                            var inline1800 bool = second__9 > 191
                            t1034 = inline1800
                        }
                        if t1034 {
                            var inline1790 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1790
                        } else {
                            var t1036_rhs uint32 = 31
                            var t1036 uint32 = first__8 & t1036_rhs
                            var t1037_rhs int = 6
                            var t1037 uint32 = t1036 << t1037_rhs
                            var t1038_rhs uint32 = 63
                            var t1038 uint32 = second__9 & t1038_rhs
                            var t1039 uint32 = t1037 | t1038
                            var inline1792 int = 2
                            var inline1793 Option__char = __goml_builtin_char_from_uint32(t1039)
                            switch inline1793.(type) {
                            case None:
                                var inline1794 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1794
                            case Some:
                                var inline1795 rune = inline1793.(Some)._0
                                var inline1797 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1795,
                                    _2: inline1792,
                                }
                                return inline1797
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1046 bool = first__8 < 240
                    if t1046 {
                        var t1079 int = length__7 - index__6
                        var t1080 bool = t1079 < 3
                        if t1080 {
                            var inline1804 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1804
                        } else {
                            var t1048 int = index__6 + 1
                            var t1049 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1048)
                            var second__10 uint32 = uint32(uint8(t1049))
                            var t1050 int = index__6 + 2
                            var t1051 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1050)
                            var third__11 uint32 = uint32(uint8(t1051))
                            var t1077 bool = utf8_invalid_continuation(second__10)
                            var jp1072 bool
                            if t1077 {
                                jp1072 = true
                            } else {
                                var inline1806 bool = third__11 < 128
                                if inline1806 {
                                    jp1072 = true
                                } else {
                                    var inline1807 bool = third__11 > 191
                                    jp1072 = inline1807
                                }
                            }
                            var jp1066 bool
                            if jp1072 {
                                jp1066 = true
                            } else {
                                var t1075 bool = first__8 == 224
                                if t1075 {
                                    var t1076 bool = second__10 < 160
                                    jp1066 = t1076
                                } else {
                                    jp1066 = false
                                }
                            }
                            var jp1055 bool
                            if jp1066 {
                                jp1055 = true
                            } else {
                                var t1069 bool = first__8 == 237
                                if t1069 {
                                    var t1070 bool = second__10 >= 160
                                    jp1055 = t1070
                                } else {
                                    jp1055 = false
                                }
                            }
                            if jp1055 {
                                var inline1809 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1809
                            } else {
                                var t1057_rhs uint32 = 15
                                var t1057 uint32 = first__8 & t1057_rhs
                                var t1058_rhs int = 12
                                var t1058 uint32 = t1057 << t1058_rhs
                                var t1059_rhs uint32 = 63
                                var t1059 uint32 = second__10 & t1059_rhs
                                var t1060_rhs int = 6
                                var t1060 uint32 = t1059 << t1060_rhs
                                var t1061 uint32 = t1058 | t1060
                                var t1062_rhs uint32 = 63
                                var t1062 uint32 = third__11 & t1062_rhs
                                var t1063 uint32 = t1061 | t1062
                                var inline1811 int = 3
                                var inline1812 Option__char = __goml_builtin_char_from_uint32(t1063)
                                switch inline1812.(type) {
                                case None:
                                    var inline1813 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1813
                                case Some:
                                    var inline1814 rune = inline1812.(Some)._0
                                    var inline1816 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1814,
                                        _2: inline1811,
                                    }
                                    return inline1816
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1084 bool = first__8 < 245
                        if t1084 {
                            var t1125 int = length__7 - index__6
                            var t1126 bool = t1125 < 4
                            if t1126 {
                                var t1127 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1127
                            } else {
                                var t1086 int = index__6 + 1
                                var t1087 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1086)
                                var second__12 uint32 = uint32(uint8(t1087))
                                var t1088 int = index__6 + 2
                                var t1089 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1088)
                                var third__13 uint32 = uint32(uint8(t1089))
                                var t1090 int = index__6 + 3
                                var t1091 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1090)
                                var fourth__14 uint32 = uint32(uint8(t1091))
                                var t1123 bool = utf8_invalid_continuation(second__12)
                                var jp1121 bool
                                if t1123 {
                                    jp1121 = true
                                } else {
                                    var t1124 bool = utf8_invalid_continuation(third__13)
                                    jp1121 = t1124
                                }
                                var jp1115 bool
                                if jp1121 {
                                    jp1115 = true
                                } else {
                                    var t1122 bool = utf8_invalid_continuation(fourth__14)
                                    jp1115 = t1122
                                }
                                var jp1109 bool
                                if jp1115 {
                                    jp1109 = true
                                } else {
                                    var t1118 bool = first__8 == 240
                                    if t1118 {
                                        var t1119 bool = second__12 < 144
                                        jp1109 = t1119
                                    } else {
                                        jp1109 = false
                                    }
                                }
                                var jp1095 bool
                                if jp1109 {
                                    jp1095 = true
                                } else {
                                    var t1112 bool = first__8 == 244
                                    if t1112 {
                                        var t1113 bool = second__12 > 143
                                        jp1095 = t1113
                                    } else {
                                        jp1095 = false
                                    }
                                }
                                if jp1095 {
                                    var t1096 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1096
                                } else {
                                    var t1097_rhs uint32 = 7
                                    var t1097 uint32 = first__8 & t1097_rhs
                                    var t1098_rhs int = 18
                                    var t1098 uint32 = t1097 << t1098_rhs
                                    var t1099_rhs uint32 = 63
                                    var t1099 uint32 = second__12 & t1099_rhs
                                    var t1100_rhs int = 12
                                    var t1100 uint32 = t1099 << t1100_rhs
                                    var t1101 uint32 = t1098 | t1100
                                    var t1102_rhs uint32 = 63
                                    var t1102 uint32 = third__13 & t1102_rhs
                                    var t1103_rhs int = 6
                                    var t1103 uint32 = t1102 << t1103_rhs
                                    var t1104 uint32 = t1101 | t1103
                                    var t1105_rhs uint32 = 63
                                    var t1105 uint32 = fourth__14 & t1105_rhs
                                    var t1106 uint32 = t1104 | t1105
                                    var t1107 Tuple3_4bool_4char_3int = utf8_valid_decode(t1106, 4)
                                    return t1107
                                }
                            }
                        } else {
                            var t1128 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1128
                        }
                    }
                }
            }
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1138 bool = value__4 <= 1114111
    if t1138 {
        var t1142 bool = value__4 >= 55296
        var jp1140 bool
        if t1142 {
            var t1143 bool = value__4 <= 57343
            jp1140 = t1143
        } else {
            jp1140 = false
        }
        var t1141 bool = !jp1140
        return t1141
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1146 int = _goml_runtime_core_string_len(self__36)
    return t1146
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1149 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1149
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1152 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1152
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1845 rune
    var inline1820 bool = utf8_valid_scalar(value__0)
    if inline1820 {
        var inline1821 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1822 rune = inline1821._1
        commute_field1845 = inline1822
        var t1158 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1845,
            _2: width__1,
        }
        return t1158
    } else {
        var inline1818 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1818
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1163 bool = value__3 < 128
    if t1163 {
        return true
    } else {
        var t1164 bool = value__3 > 191
        return t1164
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1169 bool
    var inline1826 bool = value__30 <= 1114111
    if inline1826 {
        var inline1827 bool = value__30 >= 55296
        var inline1829 bool
        if inline1827 {
            var inline1831 bool = value__30 <= 57343
            inline1829 = inline1831
        } else {
            inline1829 = false
        }
        var inline1830 bool = !inline1829
        t1169 = inline1830
    } else {
        t1169 = false
    }
    if t1169 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1170 Option__char = Some{
            _0: x24,
        }
        return t1170
    } else {
        return None{}
    }
}

func main() {
    main0()
}
