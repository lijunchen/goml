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
    var inline1260 int = _goml_runtime_core_string_len(text__2)
    len__3 = inline1260
    var t322 bool = len__3 == 0
    if t322 {
        return false
    } else {
        var i__4 *ref_int_x
        var inline1257 int = 0
        var inline1258 *ref_int_x = ref__Ref_3int(inline1257)
        i__4 = inline1258
        var saw_digit__5 *ref_bool_x
        var inline1254 bool = false
        var inline1255 *ref_bool_x = ref__Ref_4bool(inline1254)
        saw_digit__5 = inline1255
        var ok__6 *ref_bool_x
        var inline1251 bool = true
        var inline1252 *ref_bool_x = ref__Ref_4bool(inline1251)
        ok__6 = inline1252
        var started__7 *ref_bool_x
        var inline1248 bool = false
        var inline1249 *ref_bool_x = ref__Ref_4bool(inline1248)
        started__7 = inline1249
        Loop_loop328:
        for {
            var t347 bool
            var inline1242 bool = ref_get__Ref_4bool(ok__6)
            t347 = inline1242
            var jp330 bool
            if t347 {
                var t348 int
                var inline1211 int = ref_get__Ref_3int(i__4)
                t348 = inline1211
                var t349 bool = t348 < len__3
                jp330 = t349
            } else {
                jp330 = false
            }
            if jp330 {
                var t331 int
                var inline1240 int = ref_get__Ref_3int(i__4)
                t331 = inline1240
                var ch__8 rune
                var inline1238 rune = string_get(text__2, t331)
                ch__8 = inline1238
                var t344 bool
                var inline1236 bool = ref_get__Ref_4bool(started__7)
                t344 = inline1236
                var t345 bool = !t344
                var jp334 bool
                if t345 {
                    var t346 bool = ch__8 == 45
                    jp334 = t346
                } else {
                    jp334 = false
                }
                if jp334 {
                    var inline1217 bool = true
                    ref_set__Ref_4bool(started__7, inline1217)
                    var t335 int
                    var inline1215 int = ref_get__Ref_3int(i__4)
                    t335 = inline1215
                    var t336 int = t335 + 1
                    ref_set__Ref_3int(i__4, t336)
                    continue
                } else {
                    var t339 bool
                    var inline1233 bool = ch__8 >= 48
                    if inline1233 {
                        var inline1234 bool = ch__8 <= 57
                        t339 = inline1234
                    } else {
                        t339 = false
                    }
                    if t339 {
                        var inline1227 bool = true
                        ref_set__Ref_4bool(started__7, inline1227)
                        var inline1224 bool = true
                        ref_set__Ref_4bool(saw_digit__5, inline1224)
                        var t340 int
                        var inline1222 int = ref_get__Ref_3int(i__4)
                        t340 = inline1222
                        var t341 int = t340 + 1
                        ref_set__Ref_3int(i__4, t341)
                        continue
                    } else {
                        var inline1230 bool = false
                        ref_set__Ref_4bool(ok__6, inline1230)
                        continue
                    }
                }
            } else {
                break Loop_loop328
            }
        }
        var t326 bool
        var inline1246 bool = ref_get__Ref_4bool(ok__6)
        t326 = inline1246
        if t326 {
            var inline1244 bool = ref_get__Ref_4bool(saw_digit__5)
            return inline1244
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
    var inline1301 bool = false
    var inline1302 *ref_bool_x = ref__Ref_4bool(inline1301)
    started__13 = inline1302
    var acc__14 *ref_int32_x
    var inline1298 int32 = 0
    var inline1299 *ref_int32_x = ref__Ref_5int32(inline1298)
    acc__14 = inline1299
    Loop_loop359:
    for {
        var t360 int
        var inline1290 int = ref_get__Ref_3int(i__11)
        t360 = inline1290
        var t361 bool = t360 < len__10
        if t361 {
            var t362 int
            var inline1288 int = ref_get__Ref_3int(i__11)
            t362 = inline1288
            var ch__15 rune
            var inline1286 rune = string_get(text__9, t362)
            ch__15 = inline1286
            var t375 bool
            var inline1284 bool = ref_get__Ref_4bool(started__13)
            t375 = inline1284
            var t376 bool = !t375
            var jp365 bool
            if t376 {
                var t377 bool = ch__15 == 45
                jp365 = t377
            } else {
                jp365 = false
            }
            if jp365 {
                var inline1269 bool = true
                ref_set__Ref_4bool(started__13, inline1269)
                var inline1266 bool = true
                ref_set__Ref_4bool(negative__12, inline1266)
                var t366 int
                var inline1264 int = ref_get__Ref_3int(i__11)
                t366 = inline1264
                var t367 int = t366 + 1
                ref_set__Ref_3int(i__11, t367)
                continue
            } else {
                var inline1281 bool = true
                ref_set__Ref_4bool(started__13, inline1281)
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
                var inline1278 int32 = ref_get__Ref_5int32(acc__14)
                t369 = inline1278
                var t370 int32 = t369 * 10
                var t371 int32 = t370 + d__16
                ref_set__Ref_5int32(acc__14, t371)
                var t372 int
                var inline1274 int = ref_get__Ref_3int(i__11)
                t372 = inline1274
                var t373 int = t372 + 1
                ref_set__Ref_3int(i__11, t373)
                continue
            }
        } else {
            break Loop_loop359
        }
    }
    var t355 bool
    var inline1296 bool = ref_get__Ref_4bool(negative__12)
    t355 = inline1296
    if t355 {
        var t356 int32
        var inline1292 int32 = ref_get__Ref_5int32(acc__14)
        t356 = inline1292
        var t357 int32 = 0 - t356
        return t357
    } else {
        var inline1294 int32 = ref_get__Ref_5int32(acc__14)
        return inline1294
    }
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var len__20 int
    var inline1343 int = _goml_runtime_core_string_len(source__18)
    len__20 = inline1343
    var text__21 *ref_string_x
    var inline1340 string = ""
    var inline1341 *ref_string_x = ref__Ref_6string(inline1340)
    text__21 = inline1341
    var i__22 *ref_int_x
    var inline1338 *ref_int_x = ref__Ref_3int(start__19)
    i__22 = inline1338
    var done__23 *ref_bool_x
    var inline1335 bool = false
    var inline1336 *ref_bool_x = ref__Ref_4bool(inline1335)
    done__23 = inline1336
    Loop_loop402:
    for {
        var t415 bool
        var inline1329 bool = ref_get__Ref_4bool(done__23)
        t415 = inline1329
        var t416 bool = !t415
        var jp404 bool
        if t416 {
            var t417 int
            var inline1304 int = ref_get__Ref_3int(i__22)
            t417 = inline1304
            var t418 bool = t417 < len__20
            jp404 = t418
        } else {
            jp404 = false
        }
        if jp404 {
            var t405 int
            var inline1327 int = ref_get__Ref_3int(i__22)
            t405 = inline1327
            var ch__24 rune
            var inline1325 rune = string_get(source__18, t405)
            ch__24 = inline1325
            var t407 bool
            var inline1319 bool = ch__24 == 40
            var inline1321 bool
            if inline1319 {
                inline1321 = true
            } else {
                var inline1323 bool = ch__24 == 41
                inline1321 = inline1323
            }
            if inline1321 {
                t407 = true
                if t407 {
                    var inline1306 bool = true
                    ref_set__Ref_4bool(done__23, inline1306)
                    continue
                } else {
                    var t409 string
                    var inline1317 string = ref_get__Ref_6string(text__21)
                    t409 = inline1317
                    var t410 string
                    var inline1315 string = char_to_string(ch__24)
                    t410 = inline1315
                    var t411 string = t409 + t410
                    ref_set__Ref_6string(text__21, t411)
                    var t412 int
                    var inline1311 int = ref_get__Ref_3int(i__22)
                    t412 = inline1311
                    var t413 int = t412 + 1
                    ref_set__Ref_3int(i__22, t413)
                    continue
                }
            } else {
                var inline1322 bool = ch__24 == 32
                t407 = inline1322
                if t407 {
                    var inline1306 bool = true
                    ref_set__Ref_4bool(done__23, inline1306)
                    continue
                } else {
                    var t409 string
                    var inline1317 string = ref_get__Ref_6string(text__21)
                    t409 = inline1317
                    var t410 string
                    var inline1315 string = char_to_string(ch__24)
                    t410 = inline1315
                    var t411 string = t409 + t410
                    ref_set__Ref_6string(text__21, t411)
                    var t412 int
                    var inline1311 int = ref_get__Ref_3int(i__22)
                    t412 = inline1311
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
    var inline1333 string = ref_get__Ref_6string(text__21)
    atom__25 = inline1333
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
    var inline1331 int = ref_get__Ref_3int(i__22)
    t392 = inline1331
    var t393 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: jp391,
        _1: t392,
    }
    return t393
}

func lex(source__27 string) *_goml_vec_Token {
    var len__28 int
    var inline1388 int = _goml_runtime_core_string_len(source__27)
    len__28 = inline1388
    var toks0__29 *_goml_vec_Token
    var inline1386 *_goml_vec_Token = vec_new__Vec_5Token()
    toks0__29 = inline1386
    var toks__30 *ref_Vec_5Token_x
    var inline1384 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    toks__30 = inline1384
    var i__31 *ref_int_x
    var inline1381 int = 0
    var inline1382 *ref_int_x = ref__Ref_3int(inline1381)
    i__31 = inline1382
    Loop_loop423:
    for {
        var t424 int
        var inline1377 int = ref_get__Ref_3int(i__31)
        t424 = inline1377
        var t425 bool = t424 < len__28
        if t425 {
            var t426 int
            var inline1375 int = ref_get__Ref_3int(i__31)
            t426 = inline1375
            var ch__32 rune
            var inline1373 rune = string_get(source__27, t426)
            ch__32 = inline1373
            var t428 bool = ch__32 == 40
            if t428 {
                var t429 *_goml_vec_Token
                var inline1351 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                t429 = inline1351
                var t430 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t429, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t430)
                var t431 int
                var inline1347 int = ref_get__Ref_3int(i__31)
                t431 = inline1347
                var t432 int = t431 + 1
                ref_set__Ref_3int(i__31, t432)
                continue
            } else {
                var t435 bool = ch__32 == 41
                if t435 {
                    var t436 *_goml_vec_Token
                    var inline1359 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                    t436 = inline1359
                    var t437 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t436, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t437)
                    var t438 int
                    var inline1355 int = ref_get__Ref_3int(i__31)
                    t438 = inline1355
                    var t439 int = t438 + 1
                    ref_set__Ref_3int(i__31, t439)
                    continue
                } else {
                    var t442 bool = ch__32 == 32
                    if t442 {
                        var t443 int
                        var inline1363 int = ref_get__Ref_3int(i__31)
                        t443 = inline1363
                        var t444 int = t443 + 1
                        ref_set__Ref_3int(i__31, t444)
                        continue
                    } else {
                        var t446 int
                        var inline1371 int = ref_get__Ref_3int(i__31)
                        t446 = inline1371
                        var mtmp185 Tuple2_5Token_3int = lex_atom(source__27, t446)
                        var x186 Token = mtmp185._0
                        var x187 int = mtmp185._1
                        var t447 *_goml_vec_Token
                        var inline1369 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                        t447 = inline1369
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
    var inline1379 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
    return inline1379
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var t452 int
    var inline1414 int = vec_len__Vec_7Binding(env__35)
    t452 = inline1414
    var t453 int = t452 - 1
    var i__37 *ref_int_x
    var inline1412 *ref_int_x = ref__Ref_3int(t453)
    i__37 = inline1412
    var result__38 *ref_Value_x
    var inline1410 *ref_Value_x = ref__Ref_5Value(Nil{})
    result__38 = inline1410
    var done__39 *ref_bool_x
    var inline1407 bool = false
    var inline1408 *ref_bool_x = ref__Ref_4bool(inline1407)
    done__39 = inline1408
    Loop_loop456:
    for {
        var t468 bool
        var inline1403 bool = ref_get__Ref_4bool(done__39)
        t468 = inline1403
        var t469 bool = !t468
        var jp458 bool
        if t469 {
            var t470 int
            var inline1390 int = ref_get__Ref_3int(i__37)
            t470 = inline1390
            var t471 bool = t470 >= 0
            jp458 = t471
        } else {
            jp458 = false
        }
        if jp458 {
            var t459 int
            var inline1401 int = ref_get__Ref_3int(i__37)
            t459 = inline1401
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t459)
            var t461 string = binding__40.name
            var t462 bool = t461 == name__36
            if t462 {
                var t463 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t463)
                var inline1392 bool = true
                ref_set__Ref_4bool(done__39, inline1392)
                continue
            } else {
                var t465 int
                var inline1399 int = ref_get__Ref_3int(i__37)
                t465 = inline1399
                var t466 int = t465 - 1
                ref_set__Ref_3int(i__37, t466)
                continue
            }
        } else {
            break Loop_loop456
        }
    }
    var inline1405 Value = ref_get__Ref_5Value(result__38)
    return inline1405
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var acc__47 *_goml_vec_SExpr
    var inline1450 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__47 = inline1450
    var exprs__48 *ref_Vec_5SExpr_x
    var inline1448 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    exprs__48 = inline1448
    var i__49 *ref_int_x
    var inline1446 *ref_int_x = ref__Ref_3int(start__46)
    i__49 = inline1446
    var done__50 *ref_bool_x
    var inline1443 bool = false
    var inline1444 *ref_bool_x = ref__Ref_4bool(inline1443)
    done__50 = inline1444
    Loop_loop483:
    for {
        var t495 bool
        var inline1437 bool = ref_get__Ref_4bool(done__50)
        t495 = inline1437
        var t496 bool = !t495
        var jp485 bool
        if t496 {
            var t497 int
            var inline1418 int = ref_get__Ref_3int(i__49)
            t497 = inline1418
            var t498 int
            var inline1416 int = vec_len__Vec_5Token(tokens__45)
            t498 = inline1416
            var t499 bool = t497 < t498
            jp485 = t499
        } else {
            jp485 = false
        }
        if jp485 {
            var t486 int
            var inline1435 int = ref_get__Ref_3int(i__49)
            t486 = inline1435
            var mtmp196 Token = vec_get__Vec_5Token(tokens__45, t486)
            switch mtmp196.(type) {
            case RParen:
                var inline1424 bool = true
                ref_set__Ref_4bool(done__50, inline1424)
                var t488 int
                var inline1422 int = ref_get__Ref_3int(i__49)
                t488 = inline1422
                var t489 int = t488 + 1
                ref_set__Ref_3int(i__49, t489)
                continue
            default:
                var t491 int
                var inline1433 int = ref_get__Ref_3int(i__49)
                t491 = inline1433
                var mtmp201 Tuple2_5SExpr_3int = parse_expr(tokens__45, t491)
                var x202 SExpr = mtmp201._0
                var x203 int = mtmp201._1
                var t492 *_goml_vec_SExpr
                var inline1431 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                t492 = inline1431
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
    var inline1441 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    t480 = inline1441
    var t481 int
    var inline1439 int = ref_get__Ref_3int(i__49)
    t481 = inline1439
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
    var inline1470 int = 0
    var inline1471 *ref_int_x = ref__Ref_3int(inline1470)
    i__61 = inline1471
    var acc__62 *_goml_vec_SExpr
    var inline1468 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__62 = inline1468
    var exprs__63 *ref_Vec_5SExpr_x
    var inline1466 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    exprs__63 = inline1466
    Loop_loop523:
    for {
        var t524 int
        var inline1462 int = ref_get__Ref_3int(i__61)
        t524 = inline1462
        var t525 int
        var inline1460 int = vec_len__Vec_5Token(tokens__60)
        t525 = inline1460
        var t526 bool = t524 < t525
        if t526 {
            var t527 int
            var inline1458 int = ref_get__Ref_3int(i__61)
            t527 = inline1458
            var mtmp213 Tuple2_5SExpr_3int = parse_expr(tokens__60, t527)
            var x214 SExpr = mtmp213._0
            var x215 int = mtmp213._1
            var t528 *_goml_vec_SExpr
            var inline1456 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            t528 = inline1456
            var t529 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t528, x214)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t529)
            ref_set__Ref_3int(i__61, x215)
            continue
        } else {
            break Loop_loop523
        }
    }
    var inline1464 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    return inline1464
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    switch expr__72.(type) {
    case SExpr_Int:
        var x224 int32 = expr__72.(SExpr_Int)._0
        var t546 Value = Value_Int{
            _0: x224,
        }
        return t546
    case SExpr_Bool:
        var x225 bool = expr__72.(SExpr_Bool)._0
        var t547 Value = Value_Bool{
            _0: x225,
        }
        return t547
    case SExpr_Sym:
        var x226 string = expr__72.(SExpr_Sym)._0
        var t548 *_goml_vec_Binding
        var inline1481 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__74)
        t548 = inline1481
        var inline1477 Value = env_lookup(local__73, x226)
        switch inline1477.(type) {
        case Nil:
            var inline1478 Value = env_lookup(t548, x226)
            return inline1478
        default:
            return inline1477
        }
    case List:
        var x227 *_goml_vec_SExpr = expr__72.(List)._0
        var inline1483 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(x227)
        var inline1484 bool = inline1483 == 0
        if inline1484 {
            return Nil{}
        } else {
            var inline1485 SExpr = vec_get__Vec_5SExpr(x227, 0)
            switch inline1485.(type) {
            case SExpr_Sym:
                var inline1486 string = inline1485.(SExpr_Sym)._0
                var inline1488 Value = eval_list_sym(inline1486, x227, local__73, global__74)
                return inline1488
            default:
                var inline1489 Value = eval(inline1485, local__73, global__74)
                var inline1490 *_goml_vec_Value = eval_args(x227, 1, local__73, global__74)
                var inline1491 Value = apply(inline1489, inline1490, global__74)
                return inline1491
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    switch name__86 {
    case "begin":
        var t565 Value = eval_begin(items__87, 1, local__88, global__89)
        return t565
    case "define":
        var t568 int
        var inline1503 int = vec_len__Vec_5SExpr(items__87)
        t568 = inline1503
        var t569 bool = t568 == 3
        if t569 {
            var mtmp232 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp232.(type) {
            case SExpr_Sym:
                var x235 string = mtmp232.(SExpr_Sym)._0
                var t572 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t572, local__88, global__89)
                var env__92 *_goml_vec_Binding
                var inline1501 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__89)
                env__92 = inline1501
                var t573 Binding = Binding{
                    name: x235,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t573)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                return value__91
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "if":
        var t576 int
        var inline1511 int = vec_len__Vec_5SExpr(items__87)
        t576 = inline1511
        var t577 bool = t576 == 4
        if t577 {
            var t578 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t578, local__88, global__89)
            var t581 bool
            switch cond__94.(type) {
            case Value_Int:
                var inline1505 int32 = cond__94.(Value_Int)._0
                var inline1507 bool = inline1505 != 0
                t581 = inline1507
            case Value_Bool:
                var inline1508 bool = cond__94.(Value_Bool)._0
                t581 = inline1508
            case Func:
                t581 = true
            case Nil:
                t581 = false
            default:
                panic("non-exhaustive match")
            }
            if t581 {
                var t582 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t583 Value = eval(t582, local__88, global__89)
                return t583
            } else {
                var t584 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t585 Value = eval(t584, local__88, global__89)
                return t585
            }
        } else {
            return Nil{}
        }
    case "lambda":
        var t588 int
        var inline1513 int = vec_len__Vec_5SExpr(items__87)
        t588 = inline1513
        var t589 bool = t588 == 3
        if t589 {
            var mtmp238 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp238.(type) {
            case List:
                var x242 *_goml_vec_SExpr = mtmp238.(List)._0
                var params__96 *_goml_vec_string = params_from_sexprs(x242)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t592 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t593 Value = Func{
                    _0: t592,
                }
                return t593
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "+":
        var t594 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t595 Value = apply_builtin("+", t594)
        return t595
    case "-":
        var t596 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t597 Value = apply_builtin("-", t596)
        return t597
    case "*":
        var t598 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t599 Value = apply_builtin("*", t598)
        return t599
    case "/":
        var t600 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t601 Value = apply_builtin("/", t600)
        return t601
    case "=":
        var t602 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t603 Value = apply_builtin("=", t602)
        return t603
    default:
        var t604 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t604, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        switch f__98.(type) {
        case Func:
            var inline1515 Lambda = f__98.(Func)._0
            var inline1517 Value = apply_lambda(inline1515, args__99)
            return inline1517
        default:
            return Nil{}
        }
    }
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var i__104 *ref_int_x
    var inline1535 *ref_int_x = ref__Ref_3int(start__101)
    i__104 = inline1535
    var last__105 *ref_Value_x
    var inline1533 *ref_Value_x = ref__Ref_5Value(Nil{})
    last__105 = inline1533
    Loop_loop610:
    for {
        var t611 int
        var inline1529 int = ref_get__Ref_3int(i__104)
        t611 = inline1529
        var t612 int
        var inline1527 int = vec_len__Vec_5SExpr(items__100)
        t612 = inline1527
        var t613 bool = t611 < t612
        if t613 {
            var t614 int
            var inline1525 int = ref_get__Ref_3int(i__104)
            t614 = inline1525
            var t615 SExpr = vec_get__Vec_5SExpr(items__100, t614)
            var v__106 Value = eval(t615, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t616 int
            var inline1521 int = ref_get__Ref_3int(i__104)
            t616 = inline1521
            var t617 int = t616 + 1
            ref_set__Ref_3int(i__104, t617)
            continue
        } else {
            break Loop_loop610
        }
    }
    var inline1531 Value = ref_get__Ref_5Value(last__105)
    return inline1531
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var i__108 *ref_int_x
    var inline1561 int = 0
    var inline1562 *ref_int_x = ref__Ref_3int(inline1561)
    i__108 = inline1562
    var acc__109 *_goml_vec_string
    var inline1559 *_goml_vec_string = vec_new__Vec_6string()
    acc__109 = inline1559
    var params__110 *ref_Vec_6string_x
    var inline1557 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    params__110 = inline1557
    Loop_loop623:
    for {
        var t624 int
        var inline1553 int = ref_get__Ref_3int(i__108)
        t624 = inline1553
        var t625 int
        var inline1551 int = vec_len__Vec_5SExpr(items__107)
        t625 = inline1551
        var t626 bool = t624 < t625
        if t626 {
            var t627 int
            var inline1549 int = ref_get__Ref_3int(i__108)
            t627 = inline1549
            var mtmp245 SExpr = vec_get__Vec_5SExpr(items__107, t627)
            switch mtmp245.(type) {
            case SExpr_Sym:
                var x248 string = mtmp245.(SExpr_Sym)._0
                var t629 *_goml_vec_string
                var inline1543 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
                t629 = inline1543
                var t630 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t629, x248)
                ref_set__Ref_11Vec_6string(params__110, t630)
                var t631 int
                var inline1539 int = ref_get__Ref_3int(i__108)
                t631 = inline1539
                var t632 int = t631 + 1
                ref_set__Ref_3int(i__108, t632)
                continue
            default:
                var t634 int
                var inline1547 int = ref_get__Ref_3int(i__108)
                t634 = inline1547
                var t635 int = t634 + 1
                ref_set__Ref_3int(i__108, t635)
                continue
            }
        } else {
            break Loop_loop623
        }
    }
    var inline1555 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
    return inline1555
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var i__116 *ref_int_x
    var inline1584 *ref_int_x = ref__Ref_3int(start__113)
    i__116 = inline1584
    var acc__117 *_goml_vec_Value
    var inline1582 *_goml_vec_Value = vec_new__Vec_5Value()
    acc__117 = inline1582
    var args__118 *ref_Vec_5Value_x
    var inline1580 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    args__118 = inline1580
    Loop_loop641:
    for {
        var t642 int
        var inline1576 int = ref_get__Ref_3int(i__116)
        t642 = inline1576
        var t643 int
        var inline1574 int = vec_len__Vec_5SExpr(items__112)
        t643 = inline1574
        var t644 bool = t642 < t643
        if t644 {
            var t645 int
            var inline1572 int = ref_get__Ref_3int(i__116)
            t645 = inline1572
            var t646 SExpr = vec_get__Vec_5SExpr(items__112, t645)
            var v__119 Value = eval(t646, local__114, global__115)
            var t647 *_goml_vec_Value
            var inline1570 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
            t647 = inline1570
            var t648 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t647, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t648)
            var t649 int
            var inline1566 int = ref_get__Ref_3int(i__116)
            t649 = inline1566
            var t650 int = t649 + 1
            ref_set__Ref_3int(i__116, t650)
            continue
        } else {
            break Loop_loop641
        }
    }
    var inline1578 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
    return inline1578
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    switch name__120 {
    case "=":
        var t658 int
        var inline1586 int = vec_len__Vec_5Value(args__121)
        t658 = inline1586
        var t659 bool = t658 == 2
        if t659 {
            var t660 Value = vec_get__Vec_5Value(args__121, 0)
            var t661 Value = vec_get__Vec_5Value(args__121, 1)
            switch t661.(type) {
            case Value_Int:
                var x257 int32 = t661.(Value_Int)._0
                switch t660.(type) {
                case Value_Int:
                    var x260 int32 = t660.(Value_Int)._0
                    var t666 bool = x260 == x257
                    var t667 Value = Value_Bool{
                        _0: t666,
                    }
                    return t667
                default:
                    var t668 Value = Value_Bool{
                        _0: false,
                    }
                    return t668
                }
            case Value_Bool:
                var x258 bool = t661.(Value_Bool)._0
                switch t660.(type) {
                case Value_Bool:
                    var x264 bool = t660.(Value_Bool)._0
                    var t671 bool = x264 == x258
                    var t672 Value = Value_Bool{
                        _0: t671,
                    }
                    return t672
                default:
                    var t673 Value = Value_Bool{
                        _0: false,
                    }
                    return t673
                }
            default:
                var t674 Value = Value_Bool{
                    _0: false,
                }
                return t674
            }
        } else {
            var t675 Value = Value_Bool{
                _0: false,
            }
            return t675
        }
    case "+":
        var i__126 *ref_int_x
        var inline1611 int = 0
        var inline1612 *ref_int_x = ref__Ref_3int(inline1611)
        i__126 = inline1612
        var acc__127 *ref_int32_x
        var inline1608 int32 = 0
        var inline1609 *ref_int32_x = ref__Ref_5int32(inline1608)
        acc__127 = inline1609
        Loop_loop679:
        for {
            var t680 int
            var inline1604 int = ref_get__Ref_3int(i__126)
            t680 = inline1604
            var t681 int
            var inline1602 int = vec_len__Vec_5Value(args__121)
            t681 = inline1602
            var t682 bool = t680 < t681
            if t682 {
                var t683 int
                var inline1600 int = ref_get__Ref_3int(i__126)
                t683 = inline1600
                var mtmp266 Value = vec_get__Vec_5Value(args__121, t683)
                switch mtmp266.(type) {
                case Value_Int:
                    var x267 int32 = mtmp266.(Value_Int)._0
                    var t685 int32
                    var inline1594 int32 = ref_get__Ref_5int32(acc__127)
                    t685 = inline1594
                    var t686 int32 = t685 + x267
                    ref_set__Ref_5int32(acc__127, t686)
                    var t687 int
                    var inline1590 int = ref_get__Ref_3int(i__126)
                    t687 = inline1590
                    var t688 int = t687 + 1
                    ref_set__Ref_3int(i__126, t688)
                    continue
                default:
                    var t690 int
                    var inline1598 int = ref_get__Ref_3int(i__126)
                    t690 = inline1598
                    var t691 int = t690 + 1
                    ref_set__Ref_3int(i__126, t691)
                    continue
                }
            } else {
                break Loop_loop679
            }
        }
        var t677 int32
        var inline1606 int32 = ref_get__Ref_5int32(acc__127)
        t677 = inline1606
        var t678 Value = Value_Int{
            _0: t677,
        }
        return t678
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x
        var inline1634 int32 = 1
        var inline1635 *ref_int32_x = ref__Ref_5int32(inline1634)
        acc__130 = inline1635
        Loop_loop696:
        for {
            var t697 int
            var inline1630 int = ref_get__Ref_3int(i__129)
            t697 = inline1630
            var t698 int
            var inline1628 int = vec_len__Vec_5Value(args__121)
            t698 = inline1628
            var t699 bool = t697 < t698
            if t699 {
                var t700 int
                var inline1626 int = ref_get__Ref_3int(i__129)
                t700 = inline1626
                var mtmp272 Value = vec_get__Vec_5Value(args__121, t700)
                switch mtmp272.(type) {
                case Value_Int:
                    var x273 int32 = mtmp272.(Value_Int)._0
                    var t702 int32
                    var inline1620 int32 = ref_get__Ref_5int32(acc__130)
                    t702 = inline1620
                    var t703 int32 = t702 * x273
                    ref_set__Ref_5int32(acc__130, t703)
                    var t704 int
                    var inline1616 int = ref_get__Ref_3int(i__129)
                    t704 = inline1616
                    var t705 int = t704 + 1
                    ref_set__Ref_3int(i__129, t705)
                    continue
                default:
                    var t707 int
                    var inline1624 int = ref_get__Ref_3int(i__129)
                    t707 = inline1624
                    var t708 int = t707 + 1
                    ref_set__Ref_3int(i__129, t708)
                    continue
                }
            } else {
                break Loop_loop696
            }
        }
        var t694 int32
        var inline1632 int32 = ref_get__Ref_5int32(acc__130)
        t694 = inline1632
        var t695 Value = Value_Int{
            _0: t694,
        }
        return t695
    case "-":
        var mtmp278 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        switch mtmp278 {
        case 1:
            var mtmp279 Value = vec_get__Vec_5Value(args__121, 0)
            switch mtmp279.(type) {
            case Value_Int:
                var x280 int32 = mtmp279.(Value_Int)._0
                var t714 int32 = 0 - x280
                var t715 Value = Value_Int{
                    _0: t714,
                }
                return t715
            default:
                return Nil{}
            }
        case 2:
            var t716 Value = vec_get__Vec_5Value(args__121, 0)
            var t717 Value = vec_get__Vec_5Value(args__121, 1)
            switch t717.(type) {
            case Value_Int:
                var x286 int32 = t717.(Value_Int)._0
                switch t716.(type) {
                case Value_Int:
                    var x289 int32 = t716.(Value_Int)._0
                    var t722 int32 = x289 - x286
                    var t723 Value = Value_Int{
                        _0: t722,
                    }
                    return t723
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
        var t726 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t727 bool = t726 == 2
        if t727 {
            var t728 Value = vec_get__Vec_5Value(args__121, 0)
            var t729 Value = vec_get__Vec_5Value(args__121, 1)
            switch t729.(type) {
            case Value_Int:
                var x295 int32 = t729.(Value_Int)._0
                switch t728.(type) {
                case Value_Int:
                    var x298 int32 = t728.(Value_Int)._0
                    var t734 int32 = x298 / x295
                    var t735 Value = Value_Int{
                        _0: t734,
                    }
                    return t735
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
        var t740 Value = apply_lambda(x303, args__138)
        return t740
    default:
        return Nil{}
    }
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var t743 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x
    var inline1662 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t743)
    env__143 = inline1662
    var i__144 *ref_int_x
    var inline1659 int = 0
    var inline1660 *ref_int_x = ref__Ref_3int(inline1659)
    i__144 = inline1660
    Loop_loop749:
    for {
        var t760 int
        var inline1655 int = ref_get__Ref_3int(i__144)
        t760 = inline1655
        var t761 *_goml_vec_string = lambda__141.params
        var t762 int
        var inline1653 int = vec_len__Vec_6string(t761)
        t762 = inline1653
        var t763 bool = t760 < t762
        var jp751 bool
        if t763 {
            var t764 int
            var inline1639 int = ref_get__Ref_3int(i__144)
            t764 = inline1639
            var t765 int
            var inline1637 int = vec_len__Vec_5Value(args__142)
            t765 = inline1637
            var t766 bool = t764 < t765
            jp751 = t766
        } else {
            jp751 = false
        }
        if jp751 {
            var t752 *_goml_vec_string = lambda__141.params
            var t753 int
            var inline1651 int = ref_get__Ref_3int(i__144)
            t753 = inline1651
            var name__145 string = vec_get__Vec_6string(t752, t753)
            var t754 int
            var inline1649 int = ref_get__Ref_3int(i__144)
            t754 = inline1649
            var value__146 Value = vec_get__Vec_5Value(args__142, t754)
            var t755 *_goml_vec_Binding
            var inline1647 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
            t755 = inline1647
            var t756 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t755, t756)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t757 int
            var inline1643 int = ref_get__Ref_3int(i__144)
            t757 = inline1643
            var t758 int = t757 + 1
            ref_set__Ref_3int(i__144, t758)
            continue
        } else {
            break Loop_loop749
        }
    }
    var t745 SExpr = lambda__141.body
    var t746 *_goml_vec_Binding
    var inline1657 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
    t746 = inline1657
    var t747 *ref_Vec_7Binding_x = lambda__141.global
    var t748 Value = eval(t745, t746, t747)
    return t748
}

func main0() struct{} {
    var t768 *_goml_vec_Binding
    var inline1690 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t768 = inline1690
    var global__148 *ref_Vec_7Binding_x
    var inline1688 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t768)
    global__148 = inline1688
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t769 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t769)
    var t770 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t771 *_goml_vec_Binding
    var inline1686 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t771 = inline1686
    var result__151 Value = eval(t770, t771, global__148)
    var t772 string
    switch result__151.(type) {
    case Value_Int:
        var inline1679 int32 = result__151.(Value_Int)._0
        var inline1681 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1679)
        t772 = inline1681
    case Value_Bool:
        var inline1682 bool = result__151.(Value_Bool)._0
        var inline1684 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1682)
        t772 = inline1684
    case Func:
        t772 = "<lambda>"
    case Nil:
        t772 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1676 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t772)
    _goml_runtime_core_string_println(inline1676)
    var t773 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t773)
    var t774 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t775 *_goml_vec_Binding
    var inline1674 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t775 = inline1674
    var result2__153 Value = eval(t774, t775, global__148)
    var t776 string
    switch result2__153.(type) {
    case Value_Int:
        var inline1667 int32 = result2__153.(Value_Int)._0
        var inline1669 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1667)
        t776 = inline1669
    case Value_Bool:
        var inline1670 bool = result2__153.(Value_Bool)._0
        var inline1672 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1670)
        t776 = inline1672
    case Func:
        t776 = "<lambda>"
    case Nil:
        t776 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1664 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t776)
    _goml_runtime_core_string_println(inline1664)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__35 string) int {
    var t779 int = _goml_runtime_core_string_len(self__35)
    return t779
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__255 int) *ref_int_x {
    var t782 *ref_int_x = ref__Ref_3int(value__255)
    return t782
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__255 bool) *ref_bool_x {
    var t785 *ref_bool_x = ref__Ref_4bool(value__255)
    return t785
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__176 *_goml_vec_Token, elem__177 Token) *_goml_vec_Token {
    var t829 int
    var inline1712 int = vec_len__Vec_5Token(self__176)
    t829 = inline1712
    var t830 int = t829 + 1
    var result__178 *_goml_vec_Token
    var inline1710 *_goml_vec_Token = vec_with_capacity__Vec_5Token(t830)
    result__178 = inline1710
    var index__179 int = 0
    Loop_loop832:
    for {
        var t833 int
        var inline1706 int = vec_len__Vec_5Token(self__176)
        t833 = inline1706
        var t834 bool = index__179 < t833
        if t834 {
            var t835 Token = vec_get__Vec_5Token(self__176, index__179)
            vec_push__Vec_5Token(result__178, t835)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t836 int = compound_old80 + compound_value81
            index__179 = t836
            continue
        } else {
            break Loop_loop832
        }
    }
    vec_push__Vec_5Token(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__176 *_goml_vec_SExpr, elem__177 SExpr) *_goml_vec_SExpr {
    var t865 int
    var inline1722 int = vec_len__Vec_5SExpr(self__176)
    t865 = inline1722
    var t866 int = t865 + 1
    var result__178 *_goml_vec_SExpr
    var inline1720 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(t866)
    result__178 = inline1720
    var index__179 int = 0
    Loop_loop868:
    for {
        var t869 int
        var inline1716 int = vec_len__Vec_5SExpr(self__176)
        t869 = inline1716
        var t870 bool = index__179 < t869
        if t870 {
            var t871 SExpr = vec_get__Vec_5SExpr(self__176, index__179)
            vec_push__Vec_5SExpr(result__178, t871)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t872 int = compound_old80 + compound_value81
            index__179 = t872
            continue
        } else {
            break Loop_loop868
        }
    }
    vec_push__Vec_5SExpr(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t878 string = _goml_runtime_core_int32_to_string(self__33)
    return t878
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t881 string = _goml_runtime_core_bool_to_string(self__64)
    return t881
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__185 *_goml_vec_SExpr) int {
    var t887 int = vec_len__Vec_5SExpr(self__185)
    return t887
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__176 *_goml_vec_Binding, elem__177 Binding) *_goml_vec_Binding {
    var t890 int
    var inline1732 int = vec_len__Vec_7Binding(self__176)
    t890 = inline1732
    var t891 int = t890 + 1
    var result__178 *_goml_vec_Binding
    var inline1730 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(t891)
    result__178 = inline1730
    var index__179 int = 0
    Loop_loop893:
    for {
        var t894 int
        var inline1726 int = vec_len__Vec_7Binding(self__176)
        t894 = inline1726
        var t895 bool = index__179 < t894
        if t895 {
            var t896 Binding = vec_get__Vec_7Binding(self__176, index__179)
            vec_push__Vec_7Binding(result__178, t896)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t897 int = compound_old80 + compound_value81
            index__179 = t897
            continue
        } else {
            break Loop_loop893
        }
    }
    vec_push__Vec_7Binding(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__176 *_goml_vec_string, elem__177 string) *_goml_vec_string {
    var t912 int
    var inline1742 int = vec_len__Vec_6string(self__176)
    t912 = inline1742
    var t913 int = t912 + 1
    var result__178 *_goml_vec_string
    var inline1740 *_goml_vec_string = vec_with_capacity__Vec_6string(t913)
    result__178 = inline1740
    var index__179 int = 0
    Loop_loop915:
    for {
        var t916 int
        var inline1736 int = vec_len__Vec_6string(self__176)
        t916 = inline1736
        var t917 bool = index__179 < t916
        if t917 {
            var t918 string = vec_get__Vec_6string(self__176, index__179)
            vec_push__Vec_6string(result__178, t918)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t919 int = compound_old80 + compound_value81
            index__179 = t919
            continue
        } else {
            break Loop_loop915
        }
    }
    vec_push__Vec_6string(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__176 *_goml_vec_Value, elem__177 Value) *_goml_vec_Value {
    var t934 int
    var inline1752 int = vec_len__Vec_5Value(self__176)
    t934 = inline1752
    var t935 int = t934 + 1
    var result__178 *_goml_vec_Value
    var inline1750 *_goml_vec_Value = vec_with_capacity__Vec_5Value(t935)
    result__178 = inline1750
    var index__179 int = 0
    Loop_loop937:
    for {
        var t938 int
        var inline1746 int = vec_len__Vec_5Value(self__176)
        t938 = inline1746
        var t939 bool = index__179 < t938
        if t939 {
            var t940 Value = vec_get__Vec_5Value(self__176, index__179)
            vec_push__Vec_5Value(result__178, t940)
            var compound_old80 int = index__179
            var compound_value81 int = 1
            var t941 int = compound_old80 + compound_value81
            index__179 = t941
            continue
        } else {
            break Loop_loop937
        }
    }
    vec_push__Vec_5Value(result__178, elem__177)
    return result__178
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__185 *_goml_vec_Value) int {
    var t947 int = vec_len__Vec_5Value(self__185)
    return t947
}

func string_get(value__17 string, index__18 int) rune {
    var mtmp6 Tuple3_4bool_4char_3int = string_decode_utf8_at(value__17, index__18)
    var x7 bool = mtmp6._0
    var x8 rune = mtmp6._1
    if x7 {
        return x8
    } else {
        var t964 rune = _goml_runtime_core_string_get("", -1)
        return t964
    }
}

func char_to_string(value__29 rune) string {
    var t969 uint32 = uint32(rune(value__29))
    var t970 bool
    var inline1755 bool = t969 <= 1114111
    if inline1755 {
        var inline1756 bool = t969 >= 55296
        var inline1758 bool
        if inline1756 {
            var inline1760 bool = t969 <= 57343
            inline1758 = inline1760
        } else {
            inline1758 = false
        }
        var inline1759 bool = !inline1758
        t970 = inline1759
    } else {
        t970 = false
    }
    if t970 {
        var t971 string = _goml_runtime_core_char_to_string(value__29)
        return t971
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
    var t1117 bool = index__6 < 0
    var jp1115 bool
    if t1117 {
        jp1115 = true
    } else {
        var t1118 bool = index__6 >= length__7
        jp1115 = t1118
    }
    if jp1115 {
        var inline1762 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1762
    } else {
        var t1002 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1002))
        var t1005 bool = first__8 < 128
        if t1005 {
            var inline1764 int = 1
            var inline1765 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1765.(type) {
            case None:
                var inline1766 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1766
            case Some:
                var inline1767 rune = inline1765.(Some)._0
                var inline1769 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1767,
                    _2: inline1764,
                }
                return inline1769
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1009 bool = first__8 < 194
            if t1009 {
                var inline1771 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1771
            } else {
                var t1013 bool = first__8 < 224
                if t1013 {
                    var t1026 int = length__7 - index__6
                    var t1027 bool = t1026 < 2
                    if t1027 {
                        var inline1773 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1773
                    } else {
                        var t1015 int = index__6 + 1
                        var t1016 uint8
                        var inline1787 uint8 = _goml_runtime_core_string_byte_get(value__5, t1015)
                        t1016 = inline1787
                        var second__9 uint32 = uint32(uint8(t1016))
                        var t1019 bool
                        var inline1784 bool = second__9 < 128
                        if inline1784 {
                            t1019 = true
                        } else {
                            var inline1785 bool = second__9 > 191
                            t1019 = inline1785
                        }
                        if t1019 {
                            var inline1775 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1775
                        } else {
                            var t1021_rhs uint32 = 31
                            var t1021 uint32 = first__8 & t1021_rhs
                            var t1022_rhs int = 6
                            var t1022 uint32 = t1021 << t1022_rhs
                            var t1023_rhs uint32 = 63
                            var t1023 uint32 = second__9 & t1023_rhs
                            var t1024 uint32 = t1022 | t1023
                            var inline1777 int = 2
                            var inline1778 Option__char = __goml_builtin_char_from_uint32(t1024)
                            switch inline1778.(type) {
                            case None:
                                var inline1779 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1779
                            case Some:
                                var inline1780 rune = inline1778.(Some)._0
                                var inline1782 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1780,
                                    _2: inline1777,
                                }
                                return inline1782
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1031 bool = first__8 < 240
                    if t1031 {
                        var t1064 int = length__7 - index__6
                        var t1065 bool = t1064 < 3
                        if t1065 {
                            var inline1789 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1789
                        } else {
                            var t1033 int = index__6 + 1
                            var t1034 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1033)
                            var second__10 uint32 = uint32(uint8(t1034))
                            var t1035 int = index__6 + 2
                            var t1036 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1035)
                            var third__11 uint32 = uint32(uint8(t1036))
                            var t1062 bool = utf8_invalid_continuation(second__10)
                            var jp1057 bool
                            if t1062 {
                                jp1057 = true
                            } else {
                                var inline1791 bool = third__11 < 128
                                if inline1791 {
                                    jp1057 = true
                                } else {
                                    var inline1792 bool = third__11 > 191
                                    jp1057 = inline1792
                                }
                            }
                            var jp1051 bool
                            if jp1057 {
                                jp1051 = true
                            } else {
                                var t1060 bool = first__8 == 224
                                if t1060 {
                                    var t1061 bool = second__10 < 160
                                    jp1051 = t1061
                                } else {
                                    jp1051 = false
                                }
                            }
                            var jp1040 bool
                            if jp1051 {
                                jp1040 = true
                            } else {
                                var t1054 bool = first__8 == 237
                                if t1054 {
                                    var t1055 bool = second__10 >= 160
                                    jp1040 = t1055
                                } else {
                                    jp1040 = false
                                }
                            }
                            if jp1040 {
                                var inline1794 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1794
                            } else {
                                var t1042_rhs uint32 = 15
                                var t1042 uint32 = first__8 & t1042_rhs
                                var t1043_rhs int = 12
                                var t1043 uint32 = t1042 << t1043_rhs
                                var t1044_rhs uint32 = 63
                                var t1044 uint32 = second__10 & t1044_rhs
                                var t1045_rhs int = 6
                                var t1045 uint32 = t1044 << t1045_rhs
                                var t1046 uint32 = t1043 | t1045
                                var t1047_rhs uint32 = 63
                                var t1047 uint32 = third__11 & t1047_rhs
                                var t1048 uint32 = t1046 | t1047
                                var inline1796 int = 3
                                var inline1797 Option__char = __goml_builtin_char_from_uint32(t1048)
                                switch inline1797.(type) {
                                case None:
                                    var inline1798 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1798
                                case Some:
                                    var inline1799 rune = inline1797.(Some)._0
                                    var inline1801 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1799,
                                        _2: inline1796,
                                    }
                                    return inline1801
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1069 bool = first__8 < 245
                        if t1069 {
                            var t1110 int = length__7 - index__6
                            var t1111 bool = t1110 < 4
                            if t1111 {
                                var t1112 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1112
                            } else {
                                var t1071 int = index__6 + 1
                                var t1072 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1071)
                                var second__12 uint32 = uint32(uint8(t1072))
                                var t1073 int = index__6 + 2
                                var t1074 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1073)
                                var third__13 uint32 = uint32(uint8(t1074))
                                var t1075 int = index__6 + 3
                                var t1076 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1075)
                                var fourth__14 uint32 = uint32(uint8(t1076))
                                var t1108 bool = utf8_invalid_continuation(second__12)
                                var jp1106 bool
                                if t1108 {
                                    jp1106 = true
                                } else {
                                    var t1109 bool = utf8_invalid_continuation(third__13)
                                    jp1106 = t1109
                                }
                                var jp1100 bool
                                if jp1106 {
                                    jp1100 = true
                                } else {
                                    var t1107 bool = utf8_invalid_continuation(fourth__14)
                                    jp1100 = t1107
                                }
                                var jp1094 bool
                                if jp1100 {
                                    jp1094 = true
                                } else {
                                    var t1103 bool = first__8 == 240
                                    if t1103 {
                                        var t1104 bool = second__12 < 144
                                        jp1094 = t1104
                                    } else {
                                        jp1094 = false
                                    }
                                }
                                var jp1080 bool
                                if jp1094 {
                                    jp1080 = true
                                } else {
                                    var t1097 bool = first__8 == 244
                                    if t1097 {
                                        var t1098 bool = second__12 > 143
                                        jp1080 = t1098
                                    } else {
                                        jp1080 = false
                                    }
                                }
                                if jp1080 {
                                    var t1081 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1081
                                } else {
                                    var t1082_rhs uint32 = 7
                                    var t1082 uint32 = first__8 & t1082_rhs
                                    var t1083_rhs int = 18
                                    var t1083 uint32 = t1082 << t1083_rhs
                                    var t1084_rhs uint32 = 63
                                    var t1084 uint32 = second__12 & t1084_rhs
                                    var t1085_rhs int = 12
                                    var t1085 uint32 = t1084 << t1085_rhs
                                    var t1086 uint32 = t1083 | t1085
                                    var t1087_rhs uint32 = 63
                                    var t1087 uint32 = third__13 & t1087_rhs
                                    var t1088_rhs int = 6
                                    var t1088 uint32 = t1087 << t1088_rhs
                                    var t1089 uint32 = t1086 | t1088
                                    var t1090_rhs uint32 = 63
                                    var t1090 uint32 = fourth__14 & t1090_rhs
                                    var t1091 uint32 = t1089 | t1090
                                    var t1092 Tuple3_4bool_4char_3int = utf8_valid_decode(t1091, 4)
                                    return t1092
                                }
                            }
                        } else {
                            var t1113 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1113
                        }
                    }
                }
            }
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1123 bool = value__4 <= 1114111
    if t1123 {
        var t1127 bool = value__4 >= 55296
        var jp1125 bool
        if t1127 {
            var t1128 bool = value__4 <= 57343
            jp1125 = t1128
        } else {
            jp1125 = false
        }
        var t1126 bool = !jp1125
        return t1126
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1131 int = _goml_runtime_core_string_len(self__36)
    return t1131
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1134 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1134
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1137 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1137
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1830 rune
    var inline1805 bool = utf8_valid_scalar(value__0)
    if inline1805 {
        var inline1806 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1807 rune = inline1806._1
        commute_field1830 = inline1807
        var t1143 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1830,
            _2: width__1,
        }
        return t1143
    } else {
        var inline1803 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1803
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1148 bool = value__3 < 128
    if t1148 {
        return true
    } else {
        var t1149 bool = value__3 > 191
        return t1149
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1154 bool
    var inline1811 bool = value__30 <= 1114111
    if inline1811 {
        var inline1812 bool = value__30 >= 55296
        var inline1814 bool
        if inline1812 {
            var inline1816 bool = value__30 <= 57343
            inline1814 = inline1816
        } else {
            inline1814 = false
        }
        var inline1815 bool = !inline1814
        t1154 = inline1815
    } else {
        t1154 = false
    }
    if t1154 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1155 Option__char = Some{
            _0: x24,
        }
        return t1155
    } else {
        return None{}
    }
}

func main() {
    main0()
}
