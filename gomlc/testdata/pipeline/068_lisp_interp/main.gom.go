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
    var t286 bool
    var inline1205 int = 0
    var inline1206 bool = len__3 == inline1205
    t286 = inline1206
    if t286 {
        return false
    } else {
        var i__4 *ref_int_x
        var inline1202 int = 0
        var inline1203 *ref_int_x = ref__Ref_3int(inline1202)
        i__4 = inline1203
        var saw_digit__5 *ref_bool_x
        var inline1199 bool = false
        var inline1200 *ref_bool_x = ref__Ref_4bool(inline1199)
        saw_digit__5 = inline1200
        var ok__6 *ref_bool_x
        var inline1196 bool = true
        var inline1197 *ref_bool_x = ref__Ref_4bool(inline1196)
        ok__6 = inline1197
        var started__7 *ref_bool_x
        var inline1193 bool = false
        var inline1194 *ref_bool_x = ref__Ref_4bool(inline1193)
        started__7 = inline1194
        Loop_loop292:
        for {
            var t311 bool
            var inline1187 bool = ref_get__Ref_4bool(ok__6)
            t311 = inline1187
            var jp294 bool
            if t311 {
                var t312 int
                var inline1153 int = ref_get__Ref_3int(i__4)
                t312 = inline1153
                var t313 bool = t312 < len__3
                jp294 = t313
            } else {
                jp294 = false
            }
            if jp294 {
                var t295 int
                var inline1185 int = ref_get__Ref_3int(i__4)
                t295 = inline1185
                var ch__8 rune
                var inline1183 rune = string_get(text__2, t295)
                ch__8 = inline1183
                var t308 bool
                var inline1181 bool = ref_get__Ref_4bool(started__7)
                t308 = inline1181
                var t309 bool = !t308
                var jp298 bool
                if t309 {
                    var inline1155 rune = 45
                    var inline1156 bool = ch__8 == inline1155
                    jp298 = inline1156
                } else {
                    jp298 = false
                }
                if jp298 {
                    var inline1162 bool = true
                    ref_set__Ref_4bool(started__7, inline1162)
                    var t299 int
                    var inline1160 int = ref_get__Ref_3int(i__4)
                    t299 = inline1160
                    var t300 int = t299 + 1
                    ref_set__Ref_3int(i__4, t300)
                    continue
                } else {
                    var t303 bool
                    var inline1178 bool = ch__8 >= 48
                    if inline1178 {
                        var inline1179 bool = ch__8 <= 57
                        t303 = inline1179
                    } else {
                        t303 = false
                    }
                    if t303 {
                        var inline1172 bool = true
                        ref_set__Ref_4bool(started__7, inline1172)
                        var inline1169 bool = true
                        ref_set__Ref_4bool(saw_digit__5, inline1169)
                        var t304 int
                        var inline1167 int = ref_get__Ref_3int(i__4)
                        t304 = inline1167
                        var t305 int = t304 + 1
                        ref_set__Ref_3int(i__4, t305)
                        continue
                    } else {
                        var inline1175 bool = false
                        ref_set__Ref_4bool(ok__6, inline1175)
                        continue
                    }
                }
            } else {
                break Loop_loop292
            }
        }
        var t290 bool
        var inline1191 bool = ref_get__Ref_4bool(ok__6)
        t290 = inline1191
        if t290 {
            var inline1189 bool = ref_get__Ref_4bool(saw_digit__5)
            return inline1189
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
    var inline1247 int32 = 0
    var inline1248 *ref_int32_x = ref__Ref_5int32(inline1247)
    acc__14 = inline1248
    Loop_loop323:
    for {
        var t324 int
        var inline1239 int = ref_get__Ref_3int(i__11)
        t324 = inline1239
        var t325 bool = t324 < len__10
        if t325 {
            var t326 int
            var inline1237 int = ref_get__Ref_3int(i__11)
            t326 = inline1237
            var ch__15 rune
            var inline1235 rune = string_get(text__9, t326)
            ch__15 = inline1235
            var t339 bool
            var inline1233 bool = ref_get__Ref_4bool(started__13)
            t339 = inline1233
            var t340 bool = !t339
            var jp329 bool
            if t340 {
                var inline1208 rune = 45
                var inline1209 bool = ch__15 == inline1208
                jp329 = inline1209
            } else {
                jp329 = false
            }
            if jp329 {
                var inline1218 bool = true
                ref_set__Ref_4bool(started__13, inline1218)
                var inline1215 bool = true
                ref_set__Ref_4bool(negative__12, inline1215)
                var t330 int
                var inline1213 int = ref_get__Ref_3int(i__11)
                t330 = inline1213
                var t331 int = t330 + 1
                ref_set__Ref_3int(i__11, t331)
                continue
            } else {
                var inline1230 bool = true
                ref_set__Ref_4bool(started__13, inline1230)
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
                var t333 int32
                var inline1227 int32 = ref_get__Ref_5int32(acc__14)
                t333 = inline1227
                var t334 int32 = t333 * 10
                var t335 int32 = t334 + d__16
                ref_set__Ref_5int32(acc__14, t335)
                var t336 int
                var inline1223 int = ref_get__Ref_3int(i__11)
                t336 = inline1223
                var t337 int = t336 + 1
                ref_set__Ref_3int(i__11, t337)
                continue
            }
        } else {
            break Loop_loop323
        }
    }
    var t319 bool
    var inline1245 bool = ref_get__Ref_4bool(negative__12)
    t319 = inline1245
    if t319 {
        var t320 int32
        var inline1241 int32 = ref_get__Ref_5int32(acc__14)
        t320 = inline1241
        var t321 int32 = 0 - t320
        return t321
    } else {
        var inline1243 int32 = ref_get__Ref_5int32(acc__14)
        return inline1243
    }
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var len__20 int
    var inline1298 int = _goml_runtime_core_string_len(source__18)
    len__20 = inline1298
    var text__21 *ref_string_x
    var inline1295 string = ""
    var inline1296 *ref_string_x = ref__Ref_6string(inline1295)
    text__21 = inline1296
    var i__22 *ref_int_x
    var inline1293 *ref_int_x = ref__Ref_3int(start__19)
    i__22 = inline1293
    var done__23 *ref_bool_x
    var inline1290 bool = false
    var inline1291 *ref_bool_x = ref__Ref_4bool(inline1290)
    done__23 = inline1291
    Loop_loop366:
    for {
        var t379 bool
        var inline1284 bool = ref_get__Ref_4bool(done__23)
        t379 = inline1284
        var t380 bool = !t379
        var jp368 bool
        if t380 {
            var t381 int
            var inline1259 int = ref_get__Ref_3int(i__22)
            t381 = inline1259
            var t382 bool = t381 < len__20
            jp368 = t382
        } else {
            jp368 = false
        }
        if jp368 {
            var t369 int
            var inline1282 int = ref_get__Ref_3int(i__22)
            t369 = inline1282
            var ch__24 rune
            var inline1280 rune = string_get(source__18, t369)
            ch__24 = inline1280
            var t371 bool
            var inline1274 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__24, 40)
            var inline1276 bool
            if inline1274 {
                inline1276 = true
            } else {
                var inline1278 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__24, 41)
                inline1276 = inline1278
            }
            if inline1276 {
                t371 = true
                if t371 {
                    var inline1261 bool = true
                    ref_set__Ref_4bool(done__23, inline1261)
                    continue
                } else {
                    var t373 string
                    var inline1272 string = ref_get__Ref_6string(text__21)
                    t373 = inline1272
                    var t374 string
                    var inline1270 string = char_to_string(ch__24)
                    t374 = inline1270
                    var t375 string = t373 + t374
                    ref_set__Ref_6string(text__21, t375)
                    var t376 int
                    var inline1266 int = ref_get__Ref_3int(i__22)
                    t376 = inline1266
                    var t377 int = t376 + 1
                    ref_set__Ref_3int(i__22, t377)
                    continue
                }
            } else {
                var inline1277 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__24, 32)
                t371 = inline1277
                if t371 {
                    var inline1261 bool = true
                    ref_set__Ref_4bool(done__23, inline1261)
                    continue
                } else {
                    var t373 string
                    var inline1272 string = ref_get__Ref_6string(text__21)
                    t373 = inline1272
                    var t374 string
                    var inline1270 string = char_to_string(ch__24)
                    t374 = inline1270
                    var t375 string = t373 + t374
                    ref_set__Ref_6string(text__21, t375)
                    var t376 int
                    var inline1266 int = ref_get__Ref_3int(i__22)
                    t376 = inline1266
                    var t377 int = t376 + 1
                    ref_set__Ref_3int(i__22, t377)
                    continue
                }
            }
        } else {
            break Loop_loop366
        }
    }
    var atom__25 string
    var inline1288 string = ref_get__Ref_6string(text__21)
    atom__25 = inline1288
    var jp355 Token
    switch atom__25 {
    case "true":
        var t358 Token = Token_Bool{
            _0: true,
        }
        jp355 = t358
    case "false":
        var t359 Token = Token_Bool{
            _0: false,
        }
        jp355 = t359
    default:
        var t362 bool = is_int_text(atom__25)
        if t362 {
            var t363 int32 = parse_int32(atom__25)
            var t364 Token = Token_Int{
                _0: t363,
            }
            jp355 = t364
        } else {
            var t365 Token = Token_Sym{
                _0: atom__25,
            }
            jp355 = t365
        }
    }
    var t356 int
    var inline1286 int = ref_get__Ref_3int(i__22)
    t356 = inline1286
    var t357 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: jp355,
        _1: t356,
    }
    return t357
}

func lex(source__27 string) *_goml_vec_Token {
    var len__28 int = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token
    var inline1350 *_goml_vec_Token = vec_new__Vec_5Token()
    toks0__29 = inline1350
    var toks__30 *ref_Vec_5Token_x
    var inline1348 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    toks__30 = inline1348
    var i__31 *ref_int_x
    var inline1345 int = 0
    var inline1346 *ref_int_x = ref__Ref_3int(inline1345)
    i__31 = inline1346
    Loop_loop387:
    for {
        var t388 int
        var inline1341 int = ref_get__Ref_3int(i__31)
        t388 = inline1341
        var t389 bool = t388 < len__28
        if t389 {
            var t390 int
            var inline1339 int = ref_get__Ref_3int(i__31)
            t390 = inline1339
            var ch__32 rune
            var inline1337 rune = string_get(source__27, t390)
            ch__32 = inline1337
            var t392 bool
            var inline1334 rune = 40
            var inline1335 bool = ch__32 == inline1334
            t392 = inline1335
            if t392 {
                var t393 *_goml_vec_Token
                var inline1306 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                t393 = inline1306
                var t394 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t393, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t394)
                var t395 int
                var inline1302 int = ref_get__Ref_3int(i__31)
                t395 = inline1302
                var t396 int = t395 + 1
                ref_set__Ref_3int(i__31, t396)
                continue
            } else {
                var t399 bool
                var inline1331 rune = 41
                var inline1332 bool = ch__32 == inline1331
                t399 = inline1332
                if t399 {
                    var t400 *_goml_vec_Token
                    var inline1314 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                    t400 = inline1314
                    var t401 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t400, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t401)
                    var t402 int
                    var inline1310 int = ref_get__Ref_3int(i__31)
                    t402 = inline1310
                    var t403 int = t402 + 1
                    ref_set__Ref_3int(i__31, t403)
                    continue
                } else {
                    var t406 bool
                    var inline1328 rune = 32
                    var inline1329 bool = ch__32 == inline1328
                    t406 = inline1329
                    if t406 {
                        var t407 int
                        var inline1318 int = ref_get__Ref_3int(i__31)
                        t407 = inline1318
                        var t408 int = t407 + 1
                        ref_set__Ref_3int(i__31, t408)
                        continue
                    } else {
                        var t410 int
                        var inline1326 int = ref_get__Ref_3int(i__31)
                        t410 = inline1326
                        var mtmp149 Tuple2_5Token_3int = lex_atom(source__27, t410)
                        var x150 Token = mtmp149._0
                        var x151 int = mtmp149._1
                        var t411 *_goml_vec_Token
                        var inline1324 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                        t411 = inline1324
                        var t412 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t411, x150)
                        ref_set__Ref_10Vec_5Token(toks__30, t412)
                        ref_set__Ref_3int(i__31, x151)
                        continue
                    }
                }
            }
        } else {
            break Loop_loop387
        }
    }
    var inline1343 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
    return inline1343
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var t416 int
    var inline1378 int = vec_len__Vec_7Binding(env__35)
    t416 = inline1378
    var t417 int = t416 - 1
    var i__37 *ref_int_x
    var inline1376 *ref_int_x = ref__Ref_3int(t417)
    i__37 = inline1376
    var result__38 *ref_Value_x
    var inline1374 *ref_Value_x = ref__Ref_5Value(Nil{})
    result__38 = inline1374
    var done__39 *ref_bool_x
    var inline1371 bool = false
    var inline1372 *ref_bool_x = ref__Ref_4bool(inline1371)
    done__39 = inline1372
    Loop_loop420:
    for {
        var t432 bool
        var inline1367 bool = ref_get__Ref_4bool(done__39)
        t432 = inline1367
        var t433 bool = !t432
        var jp422 bool
        if t433 {
            var t434 int
            var inline1352 int = ref_get__Ref_3int(i__37)
            t434 = inline1352
            var t435 bool = t434 >= 0
            jp422 = t435
        } else {
            jp422 = false
        }
        if jp422 {
            var t423 int
            var inline1365 int = ref_get__Ref_3int(i__37)
            t423 = inline1365
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t423)
            var t425 string = binding__40.name
            var t426 bool
            var inline1363 bool = t425 == name__36
            t426 = inline1363
            if t426 {
                var t427 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t427)
                var inline1354 bool = true
                ref_set__Ref_4bool(done__39, inline1354)
                continue
            } else {
                var t429 int
                var inline1361 int = ref_get__Ref_3int(i__37)
                t429 = inline1361
                var t430 int = t429 - 1
                ref_set__Ref_3int(i__37, t430)
                continue
            }
        } else {
            break Loop_loop420
        }
    }
    var inline1369 Value = ref_get__Ref_5Value(result__38)
    return inline1369
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var acc__47 *_goml_vec_SExpr
    var inline1414 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__47 = inline1414
    var exprs__48 *ref_Vec_5SExpr_x
    var inline1412 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    exprs__48 = inline1412
    var i__49 *ref_int_x
    var inline1410 *ref_int_x = ref__Ref_3int(start__46)
    i__49 = inline1410
    var done__50 *ref_bool_x
    var inline1407 bool = false
    var inline1408 *ref_bool_x = ref__Ref_4bool(inline1407)
    done__50 = inline1408
    Loop_loop447:
    for {
        var t459 bool
        var inline1401 bool = ref_get__Ref_4bool(done__50)
        t459 = inline1401
        var t460 bool = !t459
        var jp449 bool
        if t460 {
            var t461 int
            var inline1382 int = ref_get__Ref_3int(i__49)
            t461 = inline1382
            var t462 int
            var inline1380 int = vec_len__Vec_5Token(tokens__45)
            t462 = inline1380
            var t463 bool = t461 < t462
            jp449 = t463
        } else {
            jp449 = false
        }
        if jp449 {
            var t450 int
            var inline1399 int = ref_get__Ref_3int(i__49)
            t450 = inline1399
            var mtmp160 Token = vec_get__Vec_5Token(tokens__45, t450)
            switch mtmp160.(type) {
            case RParen:
                var inline1388 bool = true
                ref_set__Ref_4bool(done__50, inline1388)
                var t452 int
                var inline1386 int = ref_get__Ref_3int(i__49)
                t452 = inline1386
                var t453 int = t452 + 1
                ref_set__Ref_3int(i__49, t453)
                continue
            default:
                var t455 int
                var inline1397 int = ref_get__Ref_3int(i__49)
                t455 = inline1397
                var mtmp165 Tuple2_5SExpr_3int = parse_expr(tokens__45, t455)
                var x166 SExpr = mtmp165._0
                var x167 int = mtmp165._1
                var t456 *_goml_vec_SExpr
                var inline1395 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                t456 = inline1395
                var t457 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t456, x166)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t457)
                ref_set__Ref_3int(i__49, x167)
                continue
            }
        } else {
            break Loop_loop447
        }
    }
    var t444 *_goml_vec_SExpr
    var inline1405 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    t444 = inline1405
    var t445 int
    var inline1403 int = ref_get__Ref_3int(i__49)
    t445 = inline1403
    var t446 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t444,
        _1: t445,
    }
    return t446
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var mtmp170 Token = vec_get__Vec_5Token(tokens__53, start__54)
    switch mtmp170.(type) {
    case LParen:
        var t468 int = start__54 + 1
        var mtmp174 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t468)
        var x175 *_goml_vec_SExpr = mtmp174._0
        var x176 int = mtmp174._1
        var t469 SExpr = List{
            _0: x175,
        }
        var t470 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t469,
            _1: x176,
        }
        return t470
    case RParen:
        var t471 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t472 int = start__54 + 1
        var t473 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t471,
            _1: t472,
        }
        return t473
    case Token_Sym:
        var x171 string = mtmp170.(Token_Sym)._0
        var t474 SExpr = SExpr_Sym{
            _0: x171,
        }
        var t475 int = start__54 + 1
        var t476 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t474,
            _1: t475,
        }
        return t476
    case Token_Int:
        var x172 int32 = mtmp170.(Token_Int)._0
        var t477 SExpr = SExpr_Int{
            _0: x172,
        }
        var t478 int = start__54 + 1
        var t479 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t477,
            _1: t478,
        }
        return t479
    case Token_Bool:
        var x173 bool = mtmp170.(Token_Bool)._0
        var t480 SExpr = SExpr_Bool{
            _0: x173,
        }
        var t481 int = start__54 + 1
        var t482 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t480,
            _1: t481,
        }
        return t482
    default:
        panic("non-exhaustive match")
    }
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var i__61 *ref_int_x
    var inline1434 int = 0
    var inline1435 *ref_int_x = ref__Ref_3int(inline1434)
    i__61 = inline1435
    var acc__62 *_goml_vec_SExpr
    var inline1432 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__62 = inline1432
    var exprs__63 *ref_Vec_5SExpr_x
    var inline1430 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    exprs__63 = inline1430
    Loop_loop487:
    for {
        var t488 int
        var inline1426 int = ref_get__Ref_3int(i__61)
        t488 = inline1426
        var t489 int
        var inline1424 int = vec_len__Vec_5Token(tokens__60)
        t489 = inline1424
        var t490 bool = t488 < t489
        if t490 {
            var t491 int
            var inline1422 int = ref_get__Ref_3int(i__61)
            t491 = inline1422
            var mtmp177 Tuple2_5SExpr_3int = parse_expr(tokens__60, t491)
            var x178 SExpr = mtmp177._0
            var x179 int = mtmp177._1
            var t492 *_goml_vec_SExpr
            var inline1420 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            t492 = inline1420
            var t493 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t492, x178)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t493)
            ref_set__Ref_3int(i__61, x179)
            continue
        } else {
            break Loop_loop487
        }
    }
    var inline1428 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    return inline1428
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    switch expr__72.(type) {
    case SExpr_Int:
        var x188 int32 = expr__72.(SExpr_Int)._0
        var t511 Value = Value_Int{
            _0: x188,
        }
        return t511
    case SExpr_Bool:
        var x189 bool = expr__72.(SExpr_Bool)._0
        var t512 Value = Value_Bool{
            _0: x189,
        }
        return t512
    case SExpr_Sym:
        var x190 string = expr__72.(SExpr_Sym)._0
        var t513 *_goml_vec_Binding
        var inline1448 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__74)
        t513 = inline1448
        var inline1444 Value = env_lookup(local__73, x190)
        switch inline1444.(type) {
        case Nil:
            var inline1445 Value = env_lookup(t513, x190)
            return inline1445
        default:
            return inline1444
        }
    case List:
        var x191 *_goml_vec_SExpr = expr__72.(List)._0
        var inline1450 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(x191)
        var inline1451 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(inline1450, 0)
        if inline1451 {
            return Nil{}
        } else {
            var inline1452 SExpr = vec_get__Vec_5SExpr(x191, 0)
            switch inline1452.(type) {
            case SExpr_Sym:
                var inline1453 string = inline1452.(SExpr_Sym)._0
                var inline1455 Value = eval_list_sym(inline1453, x191, local__73, global__74)
                return inline1455
            default:
                var inline1456 Value = eval(inline1452, local__73, global__74)
                var inline1457 *_goml_vec_Value = eval_args(x191, 1, local__73, global__74)
                var inline1458 Value = apply(inline1456, inline1457, global__74)
                return inline1458
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    switch name__86 {
    case "begin":
        var t530 Value = eval_begin(items__87, 1, local__88, global__89)
        return t530
    case "define":
        var t533 int
        var inline1476 int = vec_len__Vec_5SExpr(items__87)
        t533 = inline1476
        var t534 bool
        var inline1473 int = 3
        var inline1474 bool = t533 == inline1473
        t534 = inline1474
        if t534 {
            var mtmp196 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp196.(type) {
            case SExpr_Sym:
                var x199 string = mtmp196.(SExpr_Sym)._0
                var t537 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t537, local__88, global__89)
                var env__92 *_goml_vec_Binding
                var inline1471 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__89)
                env__92 = inline1471
                var t538 Binding = Binding{
                    name: x199,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t538)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                return value__91
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "if":
        var t541 int
        var inline1489 int = vec_len__Vec_5SExpr(items__87)
        t541 = inline1489
        var t542 bool
        var inline1486 int = 4
        var inline1487 bool = t541 == inline1486
        t542 = inline1487
        if t542 {
            var t543 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t543, local__88, global__89)
            var t546 bool
            switch cond__94.(type) {
            case Value_Int:
                var inline1478 int32 = cond__94.(Value_Int)._0
                var inline1480 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline1478, 0)
                var inline1481 bool = !inline1480
                t546 = inline1481
            case Value_Bool:
                var inline1482 bool = cond__94.(Value_Bool)._0
                t546 = inline1482
            case Func:
                t546 = true
            case Nil:
                t546 = false
            default:
                panic("non-exhaustive match")
            }
            if t546 {
                var t547 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t548 Value = eval(t547, local__88, global__89)
                return t548
            } else {
                var t549 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t550 Value = eval(t549, local__88, global__89)
                return t550
            }
        } else {
            return Nil{}
        }
    case "lambda":
        var t553 int
        var inline1494 int = vec_len__Vec_5SExpr(items__87)
        t553 = inline1494
        var t554 bool
        var inline1491 int = 3
        var inline1492 bool = t553 == inline1491
        t554 = inline1492
        if t554 {
            var mtmp202 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp202.(type) {
            case List:
                var x206 *_goml_vec_SExpr = mtmp202.(List)._0
                var params__96 *_goml_vec_string = params_from_sexprs(x206)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t557 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t558 Value = Func{
                    _0: t557,
                }
                return t558
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "+":
        var t559 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t560 Value = apply_builtin("+", t559)
        return t560
    case "-":
        var t561 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t562 Value = apply_builtin("-", t561)
        return t562
    case "*":
        var t563 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t564 Value = apply_builtin("*", t563)
        return t564
    case "/":
        var t565 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t566 Value = apply_builtin("/", t565)
        return t566
    case "=":
        var t567 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t568 Value = apply_builtin("=", t567)
        return t568
    default:
        var t569 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t569, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        switch f__98.(type) {
        case Func:
            var inline1496 Lambda = f__98.(Func)._0
            var inline1498 Value = apply_lambda(inline1496, args__99)
            return inline1498
        default:
            return Nil{}
        }
    }
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var i__104 *ref_int_x
    var inline1516 *ref_int_x = ref__Ref_3int(start__101)
    i__104 = inline1516
    var last__105 *ref_Value_x
    var inline1514 *ref_Value_x = ref__Ref_5Value(Nil{})
    last__105 = inline1514
    Loop_loop575:
    for {
        var t576 int
        var inline1510 int = ref_get__Ref_3int(i__104)
        t576 = inline1510
        var t577 int
        var inline1508 int = vec_len__Vec_5SExpr(items__100)
        t577 = inline1508
        var t578 bool = t576 < t577
        if t578 {
            var t579 int
            var inline1506 int = ref_get__Ref_3int(i__104)
            t579 = inline1506
            var t580 SExpr = vec_get__Vec_5SExpr(items__100, t579)
            var v__106 Value = eval(t580, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t581 int
            var inline1502 int = ref_get__Ref_3int(i__104)
            t581 = inline1502
            var t582 int = t581 + 1
            ref_set__Ref_3int(i__104, t582)
            continue
        } else {
            break Loop_loop575
        }
    }
    var inline1512 Value = ref_get__Ref_5Value(last__105)
    return inline1512
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var i__108 *ref_int_x
    var inline1542 int = 0
    var inline1543 *ref_int_x = ref__Ref_3int(inline1542)
    i__108 = inline1543
    var acc__109 *_goml_vec_string
    var inline1540 *_goml_vec_string = vec_new__Vec_6string()
    acc__109 = inline1540
    var params__110 *ref_Vec_6string_x
    var inline1538 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    params__110 = inline1538
    Loop_loop588:
    for {
        var t589 int
        var inline1534 int = ref_get__Ref_3int(i__108)
        t589 = inline1534
        var t590 int
        var inline1532 int = vec_len__Vec_5SExpr(items__107)
        t590 = inline1532
        var t591 bool = t589 < t590
        if t591 {
            var t592 int
            var inline1530 int = ref_get__Ref_3int(i__108)
            t592 = inline1530
            var mtmp209 SExpr = vec_get__Vec_5SExpr(items__107, t592)
            switch mtmp209.(type) {
            case SExpr_Sym:
                var x212 string = mtmp209.(SExpr_Sym)._0
                var t594 *_goml_vec_string
                var inline1524 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
                t594 = inline1524
                var t595 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t594, x212)
                ref_set__Ref_11Vec_6string(params__110, t595)
                var t596 int
                var inline1520 int = ref_get__Ref_3int(i__108)
                t596 = inline1520
                var t597 int = t596 + 1
                ref_set__Ref_3int(i__108, t597)
                continue
            default:
                var t599 int
                var inline1528 int = ref_get__Ref_3int(i__108)
                t599 = inline1528
                var t600 int = t599 + 1
                ref_set__Ref_3int(i__108, t600)
                continue
            }
        } else {
            break Loop_loop588
        }
    }
    var inline1536 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
    return inline1536
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var i__116 *ref_int_x
    var inline1565 *ref_int_x = ref__Ref_3int(start__113)
    i__116 = inline1565
    var acc__117 *_goml_vec_Value
    var inline1563 *_goml_vec_Value = vec_new__Vec_5Value()
    acc__117 = inline1563
    var args__118 *ref_Vec_5Value_x
    var inline1561 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    args__118 = inline1561
    Loop_loop606:
    for {
        var t607 int
        var inline1557 int = ref_get__Ref_3int(i__116)
        t607 = inline1557
        var t608 int
        var inline1555 int = vec_len__Vec_5SExpr(items__112)
        t608 = inline1555
        var t609 bool = t607 < t608
        if t609 {
            var t610 int
            var inline1553 int = ref_get__Ref_3int(i__116)
            t610 = inline1553
            var t611 SExpr = vec_get__Vec_5SExpr(items__112, t610)
            var v__119 Value = eval(t611, local__114, global__115)
            var t612 *_goml_vec_Value
            var inline1551 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
            t612 = inline1551
            var t613 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t612, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t613)
            var t614 int
            var inline1547 int = ref_get__Ref_3int(i__116)
            t614 = inline1547
            var t615 int = t614 + 1
            ref_set__Ref_3int(i__116, t615)
            continue
        } else {
            break Loop_loop606
        }
    }
    var inline1559 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
    return inline1559
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    switch name__120 {
    case "=":
        var t623 int
        var inline1574 int = vec_len__Vec_5Value(args__121)
        t623 = inline1574
        var t624 bool
        var inline1571 int = 2
        var inline1572 bool = t623 == inline1571
        t624 = inline1572
        if t624 {
            var t625 Value = vec_get__Vec_5Value(args__121, 0)
            var t626 Value = vec_get__Vec_5Value(args__121, 1)
            switch t626.(type) {
            case Value_Int:
                var x221 int32 = t626.(Value_Int)._0
                switch t625.(type) {
                case Value_Int:
                    var x224 int32 = t625.(Value_Int)._0
                    var t631 bool
                    var inline1567 bool = x224 == x221
                    t631 = inline1567
                    var t632 Value = Value_Bool{
                        _0: t631,
                    }
                    return t632
                default:
                    var t633 Value = Value_Bool{
                        _0: false,
                    }
                    return t633
                }
            case Value_Bool:
                var x222 bool = t626.(Value_Bool)._0
                switch t625.(type) {
                case Value_Bool:
                    var x228 bool = t625.(Value_Bool)._0
                    var t636 bool
                    var inline1569 bool = x228 == x222
                    t636 = inline1569
                    var t637 Value = Value_Bool{
                        _0: t636,
                    }
                    return t637
                default:
                    var t638 Value = Value_Bool{
                        _0: false,
                    }
                    return t638
                }
            default:
                var t639 Value = Value_Bool{
                    _0: false,
                }
                return t639
            }
        } else {
            var t640 Value = Value_Bool{
                _0: false,
            }
            return t640
        }
    case "+":
        var i__126 *ref_int_x
        var inline1599 int = 0
        var inline1600 *ref_int_x = ref__Ref_3int(inline1599)
        i__126 = inline1600
        var acc__127 *ref_int32_x
        var inline1596 int32 = 0
        var inline1597 *ref_int32_x = ref__Ref_5int32(inline1596)
        acc__127 = inline1597
        Loop_loop644:
        for {
            var t645 int
            var inline1592 int = ref_get__Ref_3int(i__126)
            t645 = inline1592
            var t646 int
            var inline1590 int = vec_len__Vec_5Value(args__121)
            t646 = inline1590
            var t647 bool = t645 < t646
            if t647 {
                var t648 int
                var inline1588 int = ref_get__Ref_3int(i__126)
                t648 = inline1588
                var mtmp230 Value = vec_get__Vec_5Value(args__121, t648)
                switch mtmp230.(type) {
                case Value_Int:
                    var x231 int32 = mtmp230.(Value_Int)._0
                    var t650 int32
                    var inline1582 int32 = ref_get__Ref_5int32(acc__127)
                    t650 = inline1582
                    var t651 int32 = t650 + x231
                    ref_set__Ref_5int32(acc__127, t651)
                    var t652 int
                    var inline1578 int = ref_get__Ref_3int(i__126)
                    t652 = inline1578
                    var t653 int = t652 + 1
                    ref_set__Ref_3int(i__126, t653)
                    continue
                default:
                    var t655 int
                    var inline1586 int = ref_get__Ref_3int(i__126)
                    t655 = inline1586
                    var t656 int = t655 + 1
                    ref_set__Ref_3int(i__126, t656)
                    continue
                }
            } else {
                break Loop_loop644
            }
        }
        var t642 int32
        var inline1594 int32 = ref_get__Ref_5int32(acc__127)
        t642 = inline1594
        var t643 Value = Value_Int{
            _0: t642,
        }
        return t643
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop661:
        for {
            var t662 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
            var t663 int
            var inline1616 int = vec_len__Vec_5Value(args__121)
            t663 = inline1616
            var t664 bool = t662 < t663
            if t664 {
                var t665 int
                var inline1614 int = ref_get__Ref_3int(i__129)
                t665 = inline1614
                var mtmp236 Value = vec_get__Vec_5Value(args__121, t665)
                switch mtmp236.(type) {
                case Value_Int:
                    var x237 int32 = mtmp236.(Value_Int)._0
                    var t667 int32
                    var inline1608 int32 = ref_get__Ref_5int32(acc__130)
                    t667 = inline1608
                    var t668 int32 = t667 * x237
                    ref_set__Ref_5int32(acc__130, t668)
                    var t669 int
                    var inline1604 int = ref_get__Ref_3int(i__129)
                    t669 = inline1604
                    var t670 int = t669 + 1
                    ref_set__Ref_3int(i__129, t670)
                    continue
                default:
                    var t672 int
                    var inline1612 int = ref_get__Ref_3int(i__129)
                    t672 = inline1612
                    var t673 int = t672 + 1
                    ref_set__Ref_3int(i__129, t673)
                    continue
                }
            } else {
                break Loop_loop661
            }
        }
        var t659 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t660 Value = Value_Int{
            _0: t659,
        }
        return t660
    case "-":
        var mtmp242 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        switch mtmp242 {
        case 1:
            var mtmp243 Value = vec_get__Vec_5Value(args__121, 0)
            switch mtmp243.(type) {
            case Value_Int:
                var x244 int32 = mtmp243.(Value_Int)._0
                var t679 int32 = 0 - x244
                var t680 Value = Value_Int{
                    _0: t679,
                }
                return t680
            default:
                return Nil{}
            }
        case 2:
            var t681 Value = vec_get__Vec_5Value(args__121, 0)
            var t682 Value = vec_get__Vec_5Value(args__121, 1)
            switch t682.(type) {
            case Value_Int:
                var x250 int32 = t682.(Value_Int)._0
                switch t681.(type) {
                case Value_Int:
                    var x253 int32 = t681.(Value_Int)._0
                    var t687 int32 = x253 - x250
                    var t688 Value = Value_Int{
                        _0: t687,
                    }
                    return t688
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
        var t691 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t692 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t691, 2)
        if t692 {
            var t693 Value = vec_get__Vec_5Value(args__121, 0)
            var t694 Value = vec_get__Vec_5Value(args__121, 1)
            switch t694.(type) {
            case Value_Int:
                var x259 int32 = t694.(Value_Int)._0
                switch t693.(type) {
                case Value_Int:
                    var x262 int32 = t693.(Value_Int)._0
                    var t699 int32 = x262 / x259
                    var t700 Value = Value_Int{
                        _0: t699,
                    }
                    return t700
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
        var x267 Lambda = func__137.(Func)._0
        var t705 Value = apply_lambda(x267, args__138)
        return t705
    default:
        return Nil{}
    }
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var t708 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x
    var inline1643 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t708)
    env__143 = inline1643
    var i__144 *ref_int_x
    var inline1640 int = 0
    var inline1641 *ref_int_x = ref__Ref_3int(inline1640)
    i__144 = inline1641
    Loop_loop714:
    for {
        var t725 int
        var inline1636 int = ref_get__Ref_3int(i__144)
        t725 = inline1636
        var t726 *_goml_vec_string = lambda__141.params
        var t727 int
        var inline1634 int = vec_len__Vec_6string(t726)
        t727 = inline1634
        var t728 bool = t725 < t727
        var jp716 bool
        if t728 {
            var t729 int
            var inline1620 int = ref_get__Ref_3int(i__144)
            t729 = inline1620
            var t730 int
            var inline1618 int = vec_len__Vec_5Value(args__142)
            t730 = inline1618
            var t731 bool = t729 < t730
            jp716 = t731
        } else {
            jp716 = false
        }
        if jp716 {
            var t717 *_goml_vec_string = lambda__141.params
            var t718 int
            var inline1632 int = ref_get__Ref_3int(i__144)
            t718 = inline1632
            var name__145 string = vec_get__Vec_6string(t717, t718)
            var t719 int
            var inline1630 int = ref_get__Ref_3int(i__144)
            t719 = inline1630
            var value__146 Value = vec_get__Vec_5Value(args__142, t719)
            var t720 *_goml_vec_Binding
            var inline1628 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
            t720 = inline1628
            var t721 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t720, t721)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t722 int
            var inline1624 int = ref_get__Ref_3int(i__144)
            t722 = inline1624
            var t723 int = t722 + 1
            ref_set__Ref_3int(i__144, t723)
            continue
        } else {
            break Loop_loop714
        }
    }
    var t710 SExpr = lambda__141.body
    var t711 *_goml_vec_Binding
    var inline1638 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
    t711 = inline1638
    var t712 *ref_Vec_7Binding_x = lambda__141.global
    var t713 Value = eval(t710, t711, t712)
    return t713
}

func main0() struct{} {
    var t733 *_goml_vec_Binding
    var inline1673 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t733 = inline1673
    var global__148 *ref_Vec_7Binding_x
    var inline1671 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t733)
    global__148 = inline1671
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t734 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t734)
    var t735 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t736 *_goml_vec_Binding
    var inline1669 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t736 = inline1669
    var result__151 Value = eval(t735, t736, global__148)
    var t737 string
    switch result__151.(type) {
    case Value_Int:
        var inline1661 int32 = result__151.(Value_Int)._0
        var inline1663 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1661)
        t737 = inline1663
    case Value_Bool:
        var inline1664 bool = result__151.(Value_Bool)._0
        var inline1666 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1664)
        t737 = inline1666
    case Func:
        t737 = "<lambda>"
    case Nil:
        t737 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1658 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t737)
    _goml_runtime_core_string_println(inline1658)
    var t738 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t738)
    var t739 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t740 *_goml_vec_Binding
    var inline1656 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t740 = inline1656
    var result2__153 Value = eval(t739, t740, global__148)
    var t741 string
    switch result2__153.(type) {
    case Value_Int:
        var inline1648 int32 = result2__153.(Value_Int)._0
        var inline1650 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1648)
        t741 = inline1650
    case Value_Bool:
        var inline1651 bool = result2__153.(Value_Bool)._0
        var inline1653 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1651)
        t741 = inline1653
    case Func:
        t741 = "<lambda>"
    case Nil:
        t741 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1645 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t741)
    _goml_runtime_core_string_println(inline1645)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__37 string) int {
    var t744 int = _goml_runtime_core_string_len(self__37)
    return t744
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__103 int, other__104 int) bool {
    var t747 bool = self__103 == other__104
    return t747
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__232 int) *ref_int_x {
    var t750 *ref_int_x = ref__Ref_3int(value__232)
    return t750
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__232 bool) *ref_bool_x {
    var t753 *ref_bool_x = ref__Ref_4bool(value__232)
    return t753
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__233 *ref_int_x) int {
    var t759 int = ref_get__Ref_3int(self__233)
    return t759
}

func _goml_m_trait__impl_i_Eq_i_char_i_eq(self__101 rune, other__102 rune) bool {
    var t765 bool = self__101 == other__102
    return t765
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__232 int32) *ref_int32_x {
    var t772 *ref_int32_x = ref__Ref_5int32(value__232)
    return t772
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__233 *ref_int32_x) int32 {
    var t775 int32 = ref_get__Ref_5int32(self__233)
    return t775
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__153 *_goml_vec_Token, elem__154 Token) *_goml_vec_Token {
    var t800 int
    var inline1696 int = vec_len__Vec_5Token(self__153)
    t800 = inline1696
    var t801 int = t800 + 1
    var result__155 *_goml_vec_Token
    var inline1694 *_goml_vec_Token = vec_with_capacity__Vec_5Token(t801)
    result__155 = inline1694
    var index__156 int = 0
    Loop_loop803:
    for {
        var t804 int
        var inline1690 int = vec_len__Vec_5Token(self__153)
        t804 = inline1690
        var t805 bool = index__156 < t804
        if t805 {
            var t806 Token = vec_get__Vec_5Token(self__153, index__156)
            vec_push__Vec_5Token(result__155, t806)
            var compound_old44 int = index__156
            var compound_value45 int = 1
            var t807 int = compound_old44 + compound_value45
            index__156 = t807
            continue
        } else {
            break Loop_loop803
        }
    }
    vec_push__Vec_5Token(result__155, elem__154)
    return result__155
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__153 *_goml_vec_SExpr, elem__154 SExpr) *_goml_vec_SExpr {
    var t839 int
    var inline1706 int = vec_len__Vec_5SExpr(self__153)
    t839 = inline1706
    var t840 int = t839 + 1
    var result__155 *_goml_vec_SExpr
    var inline1704 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(t840)
    result__155 = inline1704
    var index__156 int = 0
    Loop_loop842:
    for {
        var t843 int
        var inline1700 int = vec_len__Vec_5SExpr(self__153)
        t843 = inline1700
        var t844 bool = index__156 < t843
        if t844 {
            var t845 SExpr = vec_get__Vec_5SExpr(self__153, index__156)
            vec_push__Vec_5SExpr(result__155, t845)
            var compound_old44 int = index__156
            var compound_value45 int = 1
            var t846 int = compound_old44 + compound_value45
            index__156 = t846
            continue
        } else {
            break Loop_loop842
        }
    }
    vec_push__Vec_5SExpr(result__155, elem__154)
    return result__155
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t852 string = _goml_runtime_core_int32_to_string(self__35)
    return t852
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t855 string = _goml_runtime_core_bool_to_string(self__66)
    return t855
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__109 int32, other__110 int32) bool {
    var t858 bool = self__109 == other__110
    return t858
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__162 *_goml_vec_SExpr) int {
    var t864 int = vec_len__Vec_5SExpr(self__162)
    return t864
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__153 *_goml_vec_Binding, elem__154 Binding) *_goml_vec_Binding {
    var t867 int
    var inline1716 int = vec_len__Vec_7Binding(self__153)
    t867 = inline1716
    var t868 int = t867 + 1
    var result__155 *_goml_vec_Binding
    var inline1714 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(t868)
    result__155 = inline1714
    var index__156 int = 0
    Loop_loop870:
    for {
        var t871 int
        var inline1710 int = vec_len__Vec_7Binding(self__153)
        t871 = inline1710
        var t872 bool = index__156 < t871
        if t872 {
            var t873 Binding = vec_get__Vec_7Binding(self__153, index__156)
            vec_push__Vec_7Binding(result__155, t873)
            var compound_old44 int = index__156
            var compound_value45 int = 1
            var t874 int = compound_old44 + compound_value45
            index__156 = t874
            continue
        } else {
            break Loop_loop870
        }
    }
    vec_push__Vec_7Binding(result__155, elem__154)
    return result__155
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__153 *_goml_vec_string, elem__154 string) *_goml_vec_string {
    var t889 int
    var inline1726 int = vec_len__Vec_6string(self__153)
    t889 = inline1726
    var t890 int = t889 + 1
    var result__155 *_goml_vec_string
    var inline1724 *_goml_vec_string = vec_with_capacity__Vec_6string(t890)
    result__155 = inline1724
    var index__156 int = 0
    Loop_loop892:
    for {
        var t893 int
        var inline1720 int = vec_len__Vec_6string(self__153)
        t893 = inline1720
        var t894 bool = index__156 < t893
        if t894 {
            var t895 string = vec_get__Vec_6string(self__153, index__156)
            vec_push__Vec_6string(result__155, t895)
            var compound_old44 int = index__156
            var compound_value45 int = 1
            var t896 int = compound_old44 + compound_value45
            index__156 = t896
            continue
        } else {
            break Loop_loop892
        }
    }
    vec_push__Vec_6string(result__155, elem__154)
    return result__155
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__153 *_goml_vec_Value, elem__154 Value) *_goml_vec_Value {
    var t911 int
    var inline1736 int = vec_len__Vec_5Value(self__153)
    t911 = inline1736
    var t912 int = t911 + 1
    var result__155 *_goml_vec_Value
    var inline1734 *_goml_vec_Value = vec_with_capacity__Vec_5Value(t912)
    result__155 = inline1734
    var index__156 int = 0
    Loop_loop914:
    for {
        var t915 int
        var inline1730 int = vec_len__Vec_5Value(self__153)
        t915 = inline1730
        var t916 bool = index__156 < t915
        if t916 {
            var t917 Value = vec_get__Vec_5Value(self__153, index__156)
            vec_push__Vec_5Value(result__155, t917)
            var compound_old44 int = index__156
            var compound_value45 int = 1
            var t918 int = compound_old44 + compound_value45
            index__156 = t918
            continue
        } else {
            break Loop_loop914
        }
    }
    vec_push__Vec_5Value(result__155, elem__154)
    return result__155
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__162 *_goml_vec_Value) int {
    var t924 int = vec_len__Vec_5Value(self__162)
    return t924
}

func string_get(value__17 string, index__18 int) rune {
    var mtmp6 Tuple3_4bool_4char_3int = string_decode_utf8_at(value__17, index__18)
    var x7 bool = mtmp6._0
    var x8 rune = mtmp6._1
    if x7 {
        return x8
    } else {
        var t944 rune = _goml_runtime_core_string_get("", -1)
        return t944
    }
}

func char_to_string(value__29 rune) string {
    var t949 uint32 = uint32(rune(value__29))
    var t950 bool
    var inline1739 bool = t949 <= 1114111
    if inline1739 {
        var inline1740 bool = t949 >= 55296
        var inline1742 bool
        if inline1740 {
            var inline1744 bool = t949 <= 57343
            inline1742 = inline1744
        } else {
            inline1742 = false
        }
        var inline1743 bool = !inline1742
        t950 = inline1743
    } else {
        t950 = false
    }
    if t950 {
        var t951 string = _goml_runtime_core_char_to_string(value__29)
        return t951
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
    var t1097 bool = index__6 < 0
    var jp1095 bool
    if t1097 {
        jp1095 = true
    } else {
        var t1098 bool = index__6 >= length__7
        jp1095 = t1098
    }
    if jp1095 {
        var inline1746 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1746
    } else {
        var t982 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t982))
        var t985 bool = first__8 < 128
        if t985 {
            var inline1748 int = 1
            var inline1749 Option__char = char_from_uint32(first__8)
            switch inline1749.(type) {
            case None:
                var inline1750 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1750
            case Some:
                var inline1751 rune = inline1749.(Some)._0
                var inline1753 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1751,
                    _2: inline1748,
                }
                return inline1753
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t989 bool = first__8 < 194
            if t989 {
                var inline1755 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1755
            } else {
                var t993 bool = first__8 < 224
                if t993 {
                    var t1006 int = length__7 - index__6
                    var t1007 bool = t1006 < 2
                    if t1007 {
                        var inline1757 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1757
                    } else {
                        var t995 int = index__6 + 1
                        var t996 uint8
                        var inline1771 uint8 = _goml_runtime_core_string_byte_get(value__5, t995)
                        t996 = inline1771
                        var second__9 uint32 = uint32(uint8(t996))
                        var t999 bool
                        var inline1768 bool = second__9 < 128
                        if inline1768 {
                            t999 = true
                        } else {
                            var inline1769 bool = second__9 > 191
                            t999 = inline1769
                        }
                        if t999 {
                            var inline1759 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1759
                        } else {
                            var t1001_rhs uint32 = 31
                            var t1001 uint32 = first__8 & t1001_rhs
                            var t1002_rhs int = 6
                            var t1002 uint32 = t1001 << t1002_rhs
                            var t1003_rhs uint32 = 63
                            var t1003 uint32 = second__9 & t1003_rhs
                            var t1004 uint32 = t1002 | t1003
                            var inline1761 int = 2
                            var inline1762 Option__char = char_from_uint32(t1004)
                            switch inline1762.(type) {
                            case None:
                                var inline1763 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1763
                            case Some:
                                var inline1764 rune = inline1762.(Some)._0
                                var inline1766 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1764,
                                    _2: inline1761,
                                }
                                return inline1766
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1011 bool = first__8 < 240
                    if t1011 {
                        var t1044 int = length__7 - index__6
                        var t1045 bool = t1044 < 3
                        if t1045 {
                            var inline1773 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1773
                        } else {
                            var t1013 int = index__6 + 1
                            var t1014 uint8
                            var inline1788 uint8 = _goml_runtime_core_string_byte_get(value__5, t1013)
                            t1014 = inline1788
                            var second__10 uint32 = uint32(uint8(t1014))
                            var t1015 int = index__6 + 2
                            var t1016 uint8
                            var inline1786 uint8 = _goml_runtime_core_string_byte_get(value__5, t1015)
                            t1016 = inline1786
                            var third__11 uint32 = uint32(uint8(t1016))
                            var t1042 bool = utf8_invalid_continuation(second__10)
                            var jp1037 bool
                            if t1042 {
                                jp1037 = true
                            } else {
                                var inline1775 bool = third__11 < 128
                                if inline1775 {
                                    jp1037 = true
                                } else {
                                    var inline1776 bool = third__11 > 191
                                    jp1037 = inline1776
                                }
                            }
                            var jp1031 bool
                            if jp1037 {
                                jp1031 = true
                            } else {
                                var t1040 bool
                                var inline1778 uint32 = 224
                                var inline1779 bool = first__8 == inline1778
                                t1040 = inline1779
                                if t1040 {
                                    var t1041 bool = second__10 < 160
                                    jp1031 = t1041
                                } else {
                                    jp1031 = false
                                }
                            }
                            var jp1020 bool
                            if jp1031 {
                                jp1020 = true
                            } else {
                                var t1034 bool
                                var inline1781 uint32 = 237
                                var inline1782 bool = first__8 == inline1781
                                t1034 = inline1782
                                if t1034 {
                                    var t1035 bool = second__10 >= 160
                                    jp1020 = t1035
                                } else {
                                    jp1020 = false
                                }
                            }
                            if jp1020 {
                                var inline1784 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1784
                            } else {
                                var t1022_rhs uint32 = 15
                                var t1022 uint32 = first__8 & t1022_rhs
                                var t1023_rhs int = 12
                                var t1023 uint32 = t1022 << t1023_rhs
                                var t1024_rhs uint32 = 63
                                var t1024 uint32 = second__10 & t1024_rhs
                                var t1025_rhs int = 6
                                var t1025 uint32 = t1024 << t1025_rhs
                                var t1026 uint32 = t1023 | t1025
                                var t1027_rhs uint32 = 63
                                var t1027 uint32 = third__11 & t1027_rhs
                                var t1028 uint32 = t1026 | t1027
                                var t1029 Tuple3_4bool_4char_3int = utf8_valid_decode(t1028, 3)
                                return t1029
                            }
                        }
                    } else {
                        var t1049 bool = first__8 < 245
                        if t1049 {
                            var t1090 int = length__7 - index__6
                            var t1091 bool = t1090 < 4
                            if t1091 {
                                var t1092 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1092
                            } else {
                                var t1051 int = index__6 + 1
                                var t1052 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1051)
                                var second__12 uint32 = uint32(uint8(t1052))
                                var t1053 int = index__6 + 2
                                var t1054 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1053)
                                var third__13 uint32 = uint32(uint8(t1054))
                                var t1055 int = index__6 + 3
                                var t1056 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1055)
                                var fourth__14 uint32 = uint32(uint8(t1056))
                                var t1088 bool = utf8_invalid_continuation(second__12)
                                var jp1086 bool
                                if t1088 {
                                    jp1086 = true
                                } else {
                                    var t1089 bool = utf8_invalid_continuation(third__13)
                                    jp1086 = t1089
                                }
                                var jp1080 bool
                                if jp1086 {
                                    jp1080 = true
                                } else {
                                    var t1087 bool = utf8_invalid_continuation(fourth__14)
                                    jp1080 = t1087
                                }
                                var jp1074 bool
                                if jp1080 {
                                    jp1074 = true
                                } else {
                                    var t1083 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t1083 {
                                        var t1084 bool = second__12 < 144
                                        jp1074 = t1084
                                    } else {
                                        jp1074 = false
                                    }
                                }
                                var jp1060 bool
                                if jp1074 {
                                    jp1060 = true
                                } else {
                                    var t1077 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t1077 {
                                        var t1078 bool = second__12 > 143
                                        jp1060 = t1078
                                    } else {
                                        jp1060 = false
                                    }
                                }
                                if jp1060 {
                                    var t1061 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1061
                                } else {
                                    var t1062_rhs uint32 = 7
                                    var t1062 uint32 = first__8 & t1062_rhs
                                    var t1063_rhs int = 18
                                    var t1063 uint32 = t1062 << t1063_rhs
                                    var t1064_rhs uint32 = 63
                                    var t1064 uint32 = second__12 & t1064_rhs
                                    var t1065_rhs int = 12
                                    var t1065 uint32 = t1064 << t1065_rhs
                                    var t1066 uint32 = t1063 | t1065
                                    var t1067_rhs uint32 = 63
                                    var t1067 uint32 = third__13 & t1067_rhs
                                    var t1068_rhs int = 6
                                    var t1068 uint32 = t1067 << t1068_rhs
                                    var t1069 uint32 = t1066 | t1068
                                    var t1070_rhs uint32 = 63
                                    var t1070 uint32 = fourth__14 & t1070_rhs
                                    var t1071 uint32 = t1069 | t1070
                                    var t1072 Tuple3_4bool_4char_3int = utf8_valid_decode(t1071, 4)
                                    return t1072
                                }
                            }
                        } else {
                            var t1093 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1093
                        }
                    }
                }
            }
        }
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1103 bool = value__4 <= 1114111
    if t1103 {
        var t1107 bool = value__4 >= 55296
        var jp1105 bool
        if t1107 {
            var t1108 bool = value__4 <= 57343
            jp1105 = t1108
        } else {
            jp1105 = false
        }
        var t1106 bool = !jp1105
        return t1106
    } else {
        return false
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t1111 int = _goml_runtime_core_string_len(self__38)
    return t1111
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1114 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1114
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t1117 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t1117
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1806 rune
    var inline1792 bool = utf8_valid_scalar(value__0)
    if inline1792 {
        var inline1793 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1795 rune = inline1793._1
        commute_field1806 = inline1795
        var t1123 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1806,
            _2: width__1,
        }
        return t1123
    } else {
        var inline1790 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1790
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1128 bool = value__3 < 128
    if t1128 {
        return true
    } else {
        var t1129 bool = value__3 > 191
        return t1129
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t1132 bool = self__117 == other__118
    return t1132
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1137 bool
    var inline1799 bool = value__32 <= 1114111
    if inline1799 {
        var inline1800 bool = value__32 >= 55296
        var inline1802 bool
        if inline1800 {
            var inline1804 bool = value__32 <= 57343
            inline1802 = inline1804
        } else {
            inline1802 = false
        }
        var inline1803 bool = !inline1802
        t1137 = inline1803
    } else {
        t1137 = false
    }
    if t1137 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1138 Option__char = Some{
            _0: x24,
        }
        return t1138
    } else {
        return None{}
    }
}

func main() {
    main0()
}
