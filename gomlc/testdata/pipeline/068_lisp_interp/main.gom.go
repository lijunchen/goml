package main

import (
    _goml_fmt "fmt"
    _goml_slices "slices"
    _goml_utf8 "unicode/utf8"
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

func _goml_runtime_string_decode_utf8_at_native(s string, i int) (bool, rune, int) {
    if i < 0 || i >= int(len(s)) {
        return false, 0, 0
    }
    var value rune
    var width int
    value, width = _goml_utf8.DecodeRuneInString(s[i:int(len(s))])
    if value == _goml_utf8.RuneError && width == 1 {
        return false, 0, 0
    }
    return true, value, int(width)
}

func _goml_runtime_core_string_get(s string, i int) rune {
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

func is_int_text(text__2 string) bool {
    var len__3 int = _goml_m_inherent_i_string_i_string_i_len(text__2)
    var t305 bool
    var inline1052 int = 0
    var inline1053 bool = len__3 == inline1052
    t305 = inline1053
    if t305 {
        return false
    } else {
        var i__4 *ref_int_x
        var inline1049 int = 0
        var inline1050 *ref_int_x = ref__Ref_3int(inline1049)
        i__4 = inline1050
        var saw_digit__5 *ref_bool_x
        var inline1046 bool = false
        var inline1047 *ref_bool_x = ref__Ref_4bool(inline1046)
        saw_digit__5 = inline1047
        var ok__6 *ref_bool_x
        var inline1043 bool = true
        var inline1044 *ref_bool_x = ref__Ref_4bool(inline1043)
        ok__6 = inline1044
        var started__7 *ref_bool_x
        var inline1040 bool = false
        var inline1041 *ref_bool_x = ref__Ref_4bool(inline1040)
        started__7 = inline1041
        Loop_loop311:
        for {
            var t330 bool
            var inline1034 bool = ref_get__Ref_4bool(ok__6)
            t330 = inline1034
            var jp313 bool
            if t330 {
                var t331 int
                var inline1000 int = ref_get__Ref_3int(i__4)
                t331 = inline1000
                var t332 bool = t331 < len__3
                jp313 = t332
            } else {
                jp313 = false
            }
            if jp313 {
                var t314 int
                var inline1032 int = ref_get__Ref_3int(i__4)
                t314 = inline1032
                var ch__8 rune
                var inline1030 rune = _goml_runtime_core_string_get(text__2, t314)
                ch__8 = inline1030
                var t327 bool
                var inline1028 bool = ref_get__Ref_4bool(started__7)
                t327 = inline1028
                var t328 bool = !t327
                var jp317 bool
                if t328 {
                    var inline1002 rune = 45
                    var inline1003 bool = ch__8 == inline1002
                    jp317 = inline1003
                } else {
                    jp317 = false
                }
                if jp317 {
                    var inline1009 bool = true
                    ref_set__Ref_4bool(started__7, inline1009)
                    var t318 int
                    var inline1007 int = ref_get__Ref_3int(i__4)
                    t318 = inline1007
                    var t319 int = t318 + 1
                    ref_set__Ref_3int(i__4, t319)
                    continue
                } else {
                    var t322 bool
                    var inline1025 bool = ch__8 >= 48
                    if inline1025 {
                        var inline1026 bool = ch__8 <= 57
                        t322 = inline1026
                    } else {
                        t322 = false
                    }
                    if t322 {
                        var inline1019 bool = true
                        ref_set__Ref_4bool(started__7, inline1019)
                        var inline1016 bool = true
                        ref_set__Ref_4bool(saw_digit__5, inline1016)
                        var t323 int
                        var inline1014 int = ref_get__Ref_3int(i__4)
                        t323 = inline1014
                        var t324 int = t323 + 1
                        ref_set__Ref_3int(i__4, t324)
                        continue
                    } else {
                        var inline1022 bool = false
                        ref_set__Ref_4bool(ok__6, inline1022)
                        continue
                    }
                }
            } else {
                break Loop_loop311
            }
        }
        var t309 bool
        var inline1038 bool = ref_get__Ref_4bool(ok__6)
        t309 = inline1038
        if t309 {
            var inline1036 bool = ref_get__Ref_4bool(saw_digit__5)
            return inline1036
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
    var inline1094 int32 = 0
    var inline1095 *ref_int32_x = ref__Ref_5int32(inline1094)
    acc__14 = inline1095
    Loop_loop342:
    for {
        var t343 int
        var inline1086 int = ref_get__Ref_3int(i__11)
        t343 = inline1086
        var t344 bool = t343 < len__10
        if t344 {
            var t345 int
            var inline1084 int = ref_get__Ref_3int(i__11)
            t345 = inline1084
            var ch__15 rune
            var inline1082 rune = _goml_runtime_core_string_get(text__9, t345)
            ch__15 = inline1082
            var t358 bool
            var inline1080 bool = ref_get__Ref_4bool(started__13)
            t358 = inline1080
            var t359 bool = !t358
            var jp348 bool
            if t359 {
                var inline1055 rune = 45
                var inline1056 bool = ch__15 == inline1055
                jp348 = inline1056
            } else {
                jp348 = false
            }
            if jp348 {
                var inline1065 bool = true
                ref_set__Ref_4bool(started__13, inline1065)
                var inline1062 bool = true
                ref_set__Ref_4bool(negative__12, inline1062)
                var t349 int
                var inline1060 int = ref_get__Ref_3int(i__11)
                t349 = inline1060
                var t350 int = t349 + 1
                ref_set__Ref_3int(i__11, t350)
                continue
            } else {
                var inline1077 bool = true
                ref_set__Ref_4bool(started__13, inline1077)
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
                var t352 int32
                var inline1074 int32 = ref_get__Ref_5int32(acc__14)
                t352 = inline1074
                var t353 int32 = t352 * 10
                var t354 int32 = t353 + d__16
                ref_set__Ref_5int32(acc__14, t354)
                var t355 int
                var inline1070 int = ref_get__Ref_3int(i__11)
                t355 = inline1070
                var t356 int = t355 + 1
                ref_set__Ref_3int(i__11, t356)
                continue
            }
        } else {
            break Loop_loop342
        }
    }
    var t338 bool
    var inline1092 bool = ref_get__Ref_4bool(negative__12)
    t338 = inline1092
    if t338 {
        var t339 int32
        var inline1088 int32 = ref_get__Ref_5int32(acc__14)
        t339 = inline1088
        var t340 int32 = 0 - t339
        return t340
    } else {
        var inline1090 int32 = ref_get__Ref_5int32(acc__14)
        return inline1090
    }
}

func lex_atom(source__18 string, start__19 int) Tuple2_5Token_3int {
    var len__20 int
    var inline1145 int = _goml_runtime_core_string_len(source__18)
    len__20 = inline1145
    var text__21 *ref_string_x
    var inline1142 string = ""
    var inline1143 *ref_string_x = ref__Ref_6string(inline1142)
    text__21 = inline1143
    var i__22 *ref_int_x
    var inline1140 *ref_int_x = ref__Ref_3int(start__19)
    i__22 = inline1140
    var done__23 *ref_bool_x
    var inline1137 bool = false
    var inline1138 *ref_bool_x = ref__Ref_4bool(inline1137)
    done__23 = inline1138
    Loop_loop385:
    for {
        var t398 bool
        var inline1131 bool = ref_get__Ref_4bool(done__23)
        t398 = inline1131
        var t399 bool = !t398
        var jp387 bool
        if t399 {
            var t400 int
            var inline1106 int = ref_get__Ref_3int(i__22)
            t400 = inline1106
            var t401 bool = t400 < len__20
            jp387 = t401
        } else {
            jp387 = false
        }
        if jp387 {
            var t388 int
            var inline1129 int = ref_get__Ref_3int(i__22)
            t388 = inline1129
            var ch__24 rune
            var inline1127 rune = _goml_runtime_core_string_get(source__18, t388)
            ch__24 = inline1127
            var t390 bool
            var inline1121 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__24, 40)
            var inline1123 bool
            if inline1121 {
                inline1123 = true
            } else {
                var inline1125 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__24, 41)
                inline1123 = inline1125
            }
            if inline1123 {
                t390 = true
                if t390 {
                    var inline1108 bool = true
                    ref_set__Ref_4bool(done__23, inline1108)
                    continue
                } else {
                    var t392 string
                    var inline1119 string = ref_get__Ref_6string(text__21)
                    t392 = inline1119
                    var t393 string
                    var inline1117 string = _goml_runtime_core_char_to_string(ch__24)
                    t393 = inline1117
                    var t394 string = t392 + t393
                    ref_set__Ref_6string(text__21, t394)
                    var t395 int
                    var inline1113 int = ref_get__Ref_3int(i__22)
                    t395 = inline1113
                    var t396 int = t395 + 1
                    ref_set__Ref_3int(i__22, t396)
                    continue
                }
            } else {
                var inline1124 bool = _goml_m_trait__impl_i_Eq_i_char_i_eq(ch__24, 32)
                t390 = inline1124
                if t390 {
                    var inline1108 bool = true
                    ref_set__Ref_4bool(done__23, inline1108)
                    continue
                } else {
                    var t392 string
                    var inline1119 string = ref_get__Ref_6string(text__21)
                    t392 = inline1119
                    var t393 string
                    var inline1117 string = _goml_runtime_core_char_to_string(ch__24)
                    t393 = inline1117
                    var t394 string = t392 + t393
                    ref_set__Ref_6string(text__21, t394)
                    var t395 int
                    var inline1113 int = ref_get__Ref_3int(i__22)
                    t395 = inline1113
                    var t396 int = t395 + 1
                    ref_set__Ref_3int(i__22, t396)
                    continue
                }
            }
        } else {
            break Loop_loop385
        }
    }
    var atom__25 string
    var inline1135 string = ref_get__Ref_6string(text__21)
    atom__25 = inline1135
    var jp374 Token
    switch atom__25 {
    case "true":
        var t377 Token = Token_Bool{
            _0: true,
        }
        jp374 = t377
    case "false":
        var t378 Token = Token_Bool{
            _0: false,
        }
        jp374 = t378
    default:
        var t381 bool = is_int_text(atom__25)
        if t381 {
            var t382 int32 = parse_int32(atom__25)
            var t383 Token = Token_Int{
                _0: t382,
            }
            jp374 = t383
        } else {
            var t384 Token = Token_Sym{
                _0: atom__25,
            }
            jp374 = t384
        }
    }
    var t375 int
    var inline1133 int = ref_get__Ref_3int(i__22)
    t375 = inline1133
    var t376 Tuple2_5Token_3int = Tuple2_5Token_3int{
        _0: jp374,
        _1: t375,
    }
    return t376
}

func lex(source__27 string) *_goml_vec_Token {
    var len__28 int = _goml_m_inherent_i_string_i_string_i_len(source__27)
    var toks0__29 *_goml_vec_Token
    var inline1197 *_goml_vec_Token = vec_new__Vec_5Token()
    toks0__29 = inline1197
    var toks__30 *ref_Vec_5Token_x
    var inline1195 *ref_Vec_5Token_x = ref__Ref_10Vec_5Token(toks0__29)
    toks__30 = inline1195
    var i__31 *ref_int_x
    var inline1192 int = 0
    var inline1193 *ref_int_x = ref__Ref_3int(inline1192)
    i__31 = inline1193
    Loop_loop406:
    for {
        var t407 int
        var inline1188 int = ref_get__Ref_3int(i__31)
        t407 = inline1188
        var t408 bool = t407 < len__28
        if t408 {
            var t409 int
            var inline1186 int = ref_get__Ref_3int(i__31)
            t409 = inline1186
            var ch__32 rune
            var inline1184 rune = _goml_runtime_core_string_get(source__27, t409)
            ch__32 = inline1184
            var t411 bool
            var inline1181 rune = 40
            var inline1182 bool = ch__32 == inline1181
            t411 = inline1182
            if t411 {
                var t412 *_goml_vec_Token
                var inline1153 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                t412 = inline1153
                var t413 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t412, LParen{})
                ref_set__Ref_10Vec_5Token(toks__30, t413)
                var t414 int
                var inline1149 int = ref_get__Ref_3int(i__31)
                t414 = inline1149
                var t415 int = t414 + 1
                ref_set__Ref_3int(i__31, t415)
                continue
            } else {
                var t418 bool
                var inline1178 rune = 41
                var inline1179 bool = ch__32 == inline1178
                t418 = inline1179
                if t418 {
                    var t419 *_goml_vec_Token
                    var inline1161 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                    t419 = inline1161
                    var t420 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t419, RParen{})
                    ref_set__Ref_10Vec_5Token(toks__30, t420)
                    var t421 int
                    var inline1157 int = ref_get__Ref_3int(i__31)
                    t421 = inline1157
                    var t422 int = t421 + 1
                    ref_set__Ref_3int(i__31, t422)
                    continue
                } else {
                    var t425 bool
                    var inline1175 rune = 32
                    var inline1176 bool = ch__32 == inline1175
                    t425 = inline1176
                    if t425 {
                        var t426 int
                        var inline1165 int = ref_get__Ref_3int(i__31)
                        t426 = inline1165
                        var t427 int = t426 + 1
                        ref_set__Ref_3int(i__31, t427)
                        continue
                    } else {
                        var t429 int
                        var inline1173 int = ref_get__Ref_3int(i__31)
                        t429 = inline1173
                        var mtmp168 Tuple2_5Token_3int = lex_atom(source__27, t429)
                        var x169 Token = mtmp168._0
                        var x170 int = mtmp168._1
                        var t430 *_goml_vec_Token
                        var inline1171 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
                        t430 = inline1171
                        var t431 *_goml_vec_Token = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(t430, x169)
                        ref_set__Ref_10Vec_5Token(toks__30, t431)
                        ref_set__Ref_3int(i__31, x170)
                        continue
                    }
                }
            }
        } else {
            break Loop_loop406
        }
    }
    var inline1190 *_goml_vec_Token = ref_get__Ref_10Vec_5Token(toks__30)
    return inline1190
}

func env_lookup(env__35 *_goml_vec_Binding, name__36 string) Value {
    var t435 int
    var inline1225 int = vec_len__Vec_7Binding(env__35)
    t435 = inline1225
    var t436 int = t435 - 1
    var i__37 *ref_int_x
    var inline1223 *ref_int_x = ref__Ref_3int(t436)
    i__37 = inline1223
    var result__38 *ref_Value_x
    var inline1221 *ref_Value_x = ref__Ref_5Value(Nil{})
    result__38 = inline1221
    var done__39 *ref_bool_x
    var inline1218 bool = false
    var inline1219 *ref_bool_x = ref__Ref_4bool(inline1218)
    done__39 = inline1219
    Loop_loop439:
    for {
        var t451 bool
        var inline1214 bool = ref_get__Ref_4bool(done__39)
        t451 = inline1214
        var t452 bool = !t451
        var jp441 bool
        if t452 {
            var t453 int
            var inline1199 int = ref_get__Ref_3int(i__37)
            t453 = inline1199
            var t454 bool = t453 >= 0
            jp441 = t454
        } else {
            jp441 = false
        }
        if jp441 {
            var t442 int
            var inline1212 int = ref_get__Ref_3int(i__37)
            t442 = inline1212
            var binding__40 Binding = vec_get__Vec_7Binding(env__35, t442)
            var t444 string = binding__40.name
            var t445 bool
            var inline1210 bool = t444 == name__36
            t445 = inline1210
            if t445 {
                var t446 Value = binding__40.value
                ref_set__Ref_5Value(result__38, t446)
                var inline1201 bool = true
                ref_set__Ref_4bool(done__39, inline1201)
                continue
            } else {
                var t448 int
                var inline1208 int = ref_get__Ref_3int(i__37)
                t448 = inline1208
                var t449 int = t448 - 1
                ref_set__Ref_3int(i__37, t449)
                continue
            }
        } else {
            break Loop_loop439
        }
    }
    var inline1216 Value = ref_get__Ref_5Value(result__38)
    return inline1216
}

func parse_list(tokens__45 *_goml_vec_Token, start__46 int) Tuple2_10Vec_5SExpr_3int {
    var acc__47 *_goml_vec_SExpr
    var inline1261 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__47 = inline1261
    var exprs__48 *ref_Vec_5SExpr_x
    var inline1259 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__47)
    exprs__48 = inline1259
    var i__49 *ref_int_x
    var inline1257 *ref_int_x = ref__Ref_3int(start__46)
    i__49 = inline1257
    var done__50 *ref_bool_x
    var inline1254 bool = false
    var inline1255 *ref_bool_x = ref__Ref_4bool(inline1254)
    done__50 = inline1255
    Loop_loop466:
    for {
        var t478 bool
        var inline1248 bool = ref_get__Ref_4bool(done__50)
        t478 = inline1248
        var t479 bool = !t478
        var jp468 bool
        if t479 {
            var t480 int
            var inline1229 int = ref_get__Ref_3int(i__49)
            t480 = inline1229
            var t481 int
            var inline1227 int = vec_len__Vec_5Token(tokens__45)
            t481 = inline1227
            var t482 bool = t480 < t481
            jp468 = t482
        } else {
            jp468 = false
        }
        if jp468 {
            var t469 int
            var inline1246 int = ref_get__Ref_3int(i__49)
            t469 = inline1246
            var mtmp179 Token = vec_get__Vec_5Token(tokens__45, t469)
            switch mtmp179.(type) {
            case RParen:
                var inline1235 bool = true
                ref_set__Ref_4bool(done__50, inline1235)
                var t471 int
                var inline1233 int = ref_get__Ref_3int(i__49)
                t471 = inline1233
                var t472 int = t471 + 1
                ref_set__Ref_3int(i__49, t472)
                continue
            default:
                var t474 int
                var inline1244 int = ref_get__Ref_3int(i__49)
                t474 = inline1244
                var mtmp184 Tuple2_5SExpr_3int = parse_expr(tokens__45, t474)
                var x185 SExpr = mtmp184._0
                var x186 int = mtmp184._1
                var t475 *_goml_vec_SExpr
                var inline1242 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
                t475 = inline1242
                var t476 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t475, x185)
                ref_set__Ref_10Vec_5SExpr(exprs__48, t476)
                ref_set__Ref_3int(i__49, x186)
                continue
            }
        } else {
            break Loop_loop466
        }
    }
    var t463 *_goml_vec_SExpr
    var inline1252 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__48)
    t463 = inline1252
    var t464 int
    var inline1250 int = ref_get__Ref_3int(i__49)
    t464 = inline1250
    var t465 Tuple2_10Vec_5SExpr_3int = Tuple2_10Vec_5SExpr_3int{
        _0: t463,
        _1: t464,
    }
    return t465
}

func parse_expr(tokens__53 *_goml_vec_Token, start__54 int) Tuple2_5SExpr_3int {
    var mtmp189 Token = vec_get__Vec_5Token(tokens__53, start__54)
    switch mtmp189.(type) {
    case LParen:
        var t487 int = start__54 + 1
        var mtmp193 Tuple2_10Vec_5SExpr_3int = parse_list(tokens__53, t487)
        var x194 *_goml_vec_SExpr = mtmp193._0
        var x195 int = mtmp193._1
        var t488 SExpr = List{
            _0: x194,
        }
        var t489 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t488,
            _1: x195,
        }
        return t489
    case RParen:
        var t490 SExpr = SExpr_Sym{
            _0: ")",
        }
        var t491 int = start__54 + 1
        var t492 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t490,
            _1: t491,
        }
        return t492
    case Token_Sym:
        var x190 string = mtmp189.(Token_Sym)._0
        var t493 SExpr = SExpr_Sym{
            _0: x190,
        }
        var t494 int = start__54 + 1
        var t495 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t493,
            _1: t494,
        }
        return t495
    case Token_Int:
        var x191 int32 = mtmp189.(Token_Int)._0
        var t496 SExpr = SExpr_Int{
            _0: x191,
        }
        var t497 int = start__54 + 1
        var t498 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t496,
            _1: t497,
        }
        return t498
    case Token_Bool:
        var x192 bool = mtmp189.(Token_Bool)._0
        var t499 SExpr = SExpr_Bool{
            _0: x192,
        }
        var t500 int = start__54 + 1
        var t501 Tuple2_5SExpr_3int = Tuple2_5SExpr_3int{
            _0: t499,
            _1: t500,
        }
        return t501
    default:
        panic("non-exhaustive match")
    }
}

func parse_program(tokens__60 *_goml_vec_Token) *_goml_vec_SExpr {
    var i__61 *ref_int_x
    var inline1281 int = 0
    var inline1282 *ref_int_x = ref__Ref_3int(inline1281)
    i__61 = inline1282
    var acc__62 *_goml_vec_SExpr
    var inline1279 *_goml_vec_SExpr = vec_new__Vec_5SExpr()
    acc__62 = inline1279
    var exprs__63 *ref_Vec_5SExpr_x
    var inline1277 *ref_Vec_5SExpr_x = ref__Ref_10Vec_5SExpr(acc__62)
    exprs__63 = inline1277
    Loop_loop506:
    for {
        var t507 int
        var inline1273 int = ref_get__Ref_3int(i__61)
        t507 = inline1273
        var t508 int
        var inline1271 int = vec_len__Vec_5Token(tokens__60)
        t508 = inline1271
        var t509 bool = t507 < t508
        if t509 {
            var t510 int
            var inline1269 int = ref_get__Ref_3int(i__61)
            t510 = inline1269
            var mtmp196 Tuple2_5SExpr_3int = parse_expr(tokens__60, t510)
            var x197 SExpr = mtmp196._0
            var x198 int = mtmp196._1
            var t511 *_goml_vec_SExpr
            var inline1267 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
            t511 = inline1267
            var t512 *_goml_vec_SExpr = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(t511, x197)
            ref_set__Ref_10Vec_5SExpr(exprs__63, t512)
            ref_set__Ref_3int(i__61, x198)
            continue
        } else {
            break Loop_loop506
        }
    }
    var inline1275 *_goml_vec_SExpr = ref_get__Ref_10Vec_5SExpr(exprs__63)
    return inline1275
}

func eval(expr__72 SExpr, local__73 *_goml_vec_Binding, global__74 *ref_Vec_7Binding_x) Value {
    switch expr__72.(type) {
    case SExpr_Int:
        var x207 int32 = expr__72.(SExpr_Int)._0
        var t530 Value = Value_Int{
            _0: x207,
        }
        return t530
    case SExpr_Bool:
        var x208 bool = expr__72.(SExpr_Bool)._0
        var t531 Value = Value_Bool{
            _0: x208,
        }
        return t531
    case SExpr_Sym:
        var x209 string = expr__72.(SExpr_Sym)._0
        var t532 *_goml_vec_Binding
        var inline1295 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__74)
        t532 = inline1295
        var inline1291 Value = env_lookup(local__73, x209)
        switch inline1291.(type) {
        case Nil:
            var inline1292 Value = env_lookup(t532, x209)
            return inline1292
        default:
            return inline1291
        }
    case List:
        var x210 *_goml_vec_SExpr = expr__72.(List)._0
        var inline1297 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(x210)
        var inline1298 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(inline1297, 0)
        if inline1298 {
            return Nil{}
        } else {
            var inline1299 SExpr = vec_get__Vec_5SExpr(x210, 0)
            switch inline1299.(type) {
            case SExpr_Sym:
                var inline1300 string = inline1299.(SExpr_Sym)._0
                var inline1302 Value = eval_list_sym(inline1300, x210, local__73, global__74)
                return inline1302
            default:
                var inline1303 Value = eval(inline1299, local__73, global__74)
                var inline1304 *_goml_vec_Value = eval_args(x210, 1, local__73, global__74)
                var inline1305 Value = apply(inline1303, inline1304, global__74)
                return inline1305
            }
        }
    default:
        panic("non-exhaustive match")
    }
}

func eval_list_sym(name__86 string, items__87 *_goml_vec_SExpr, local__88 *_goml_vec_Binding, global__89 *ref_Vec_7Binding_x) Value {
    switch name__86 {
    case "begin":
        var t549 Value = eval_begin(items__87, 1, local__88, global__89)
        return t549
    case "define":
        var t552 int
        var inline1323 int = vec_len__Vec_5SExpr(items__87)
        t552 = inline1323
        var t553 bool
        var inline1320 int = 3
        var inline1321 bool = t552 == inline1320
        t553 = inline1321
        if t553 {
            var mtmp215 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp215.(type) {
            case SExpr_Sym:
                var x218 string = mtmp215.(SExpr_Sym)._0
                var t556 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var value__91 Value = eval(t556, local__88, global__89)
                var env__92 *_goml_vec_Binding
                var inline1318 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(global__89)
                env__92 = inline1318
                var t557 Binding = Binding{
                    name: x218,
                    value: value__91,
                }
                var updated__93 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(env__92, t557)
                ref_set__Ref_12Vec_7Binding(global__89, updated__93)
                return value__91
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "if":
        var t560 int
        var inline1336 int = vec_len__Vec_5SExpr(items__87)
        t560 = inline1336
        var t561 bool
        var inline1333 int = 4
        var inline1334 bool = t560 == inline1333
        t561 = inline1334
        if t561 {
            var t562 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            var cond__94 Value = eval(t562, local__88, global__89)
            var t565 bool
            switch cond__94.(type) {
            case Value_Int:
                var inline1325 int32 = cond__94.(Value_Int)._0
                var inline1327 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(inline1325, 0)
                var inline1328 bool = !inline1327
                t565 = inline1328
            case Value_Bool:
                var inline1329 bool = cond__94.(Value_Bool)._0
                t565 = inline1329
            case Func:
                t565 = true
            case Nil:
                t565 = false
            default:
                panic("non-exhaustive match")
            }
            if t565 {
                var t566 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t567 Value = eval(t566, local__88, global__89)
                return t567
            } else {
                var t568 SExpr = vec_get__Vec_5SExpr(items__87, 3)
                var t569 Value = eval(t568, local__88, global__89)
                return t569
            }
        } else {
            return Nil{}
        }
    case "lambda":
        var t572 int
        var inline1341 int = vec_len__Vec_5SExpr(items__87)
        t572 = inline1341
        var t573 bool
        var inline1338 int = 3
        var inline1339 bool = t572 == inline1338
        t573 = inline1339
        if t573 {
            var mtmp221 SExpr = vec_get__Vec_5SExpr(items__87, 1)
            switch mtmp221.(type) {
            case List:
                var x225 *_goml_vec_SExpr = mtmp221.(List)._0
                var params__96 *_goml_vec_string = params_from_sexprs(x225)
                var body__97 SExpr = vec_get__Vec_5SExpr(items__87, 2)
                var t576 Lambda = Lambda{
                    params: params__96,
                    body: body__97,
                    env: local__88,
                    global: global__89,
                }
                var t577 Value = Func{
                    _0: t576,
                }
                return t577
            default:
                return Nil{}
            }
        } else {
            return Nil{}
        }
    case "+":
        var t578 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t579 Value = apply_builtin("+", t578)
        return t579
    case "-":
        var t580 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t581 Value = apply_builtin("-", t580)
        return t581
    case "*":
        var t582 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t583 Value = apply_builtin("*", t582)
        return t583
    case "/":
        var t584 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t585 Value = apply_builtin("/", t584)
        return t585
    case "=":
        var t586 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        var t587 Value = apply_builtin("=", t586)
        return t587
    default:
        var t588 SExpr = SExpr_Sym{
            _0: name__86,
        }
        var f__98 Value = eval(t588, local__88, global__89)
        var args__99 *_goml_vec_Value = eval_args(items__87, 1, local__88, global__89)
        switch f__98.(type) {
        case Func:
            var inline1343 Lambda = f__98.(Func)._0
            var inline1345 Value = apply_lambda(inline1343, args__99)
            return inline1345
        default:
            return Nil{}
        }
    }
}

func eval_begin(items__100 *_goml_vec_SExpr, start__101 int, local__102 *_goml_vec_Binding, global__103 *ref_Vec_7Binding_x) Value {
    var i__104 *ref_int_x
    var inline1363 *ref_int_x = ref__Ref_3int(start__101)
    i__104 = inline1363
    var last__105 *ref_Value_x
    var inline1361 *ref_Value_x = ref__Ref_5Value(Nil{})
    last__105 = inline1361
    Loop_loop594:
    for {
        var t595 int
        var inline1357 int = ref_get__Ref_3int(i__104)
        t595 = inline1357
        var t596 int
        var inline1355 int = vec_len__Vec_5SExpr(items__100)
        t596 = inline1355
        var t597 bool = t595 < t596
        if t597 {
            var t598 int
            var inline1353 int = ref_get__Ref_3int(i__104)
            t598 = inline1353
            var t599 SExpr = vec_get__Vec_5SExpr(items__100, t598)
            var v__106 Value = eval(t599, local__102, global__103)
            ref_set__Ref_5Value(last__105, v__106)
            var t600 int
            var inline1349 int = ref_get__Ref_3int(i__104)
            t600 = inline1349
            var t601 int = t600 + 1
            ref_set__Ref_3int(i__104, t601)
            continue
        } else {
            break Loop_loop594
        }
    }
    var inline1359 Value = ref_get__Ref_5Value(last__105)
    return inline1359
}

func params_from_sexprs(items__107 *_goml_vec_SExpr) *_goml_vec_string {
    var i__108 *ref_int_x
    var inline1389 int = 0
    var inline1390 *ref_int_x = ref__Ref_3int(inline1389)
    i__108 = inline1390
    var acc__109 *_goml_vec_string
    var inline1387 *_goml_vec_string = vec_new__Vec_6string()
    acc__109 = inline1387
    var params__110 *ref_Vec_6string_x
    var inline1385 *ref_Vec_6string_x = ref__Ref_11Vec_6string(acc__109)
    params__110 = inline1385
    Loop_loop607:
    for {
        var t608 int
        var inline1381 int = ref_get__Ref_3int(i__108)
        t608 = inline1381
        var t609 int
        var inline1379 int = vec_len__Vec_5SExpr(items__107)
        t609 = inline1379
        var t610 bool = t608 < t609
        if t610 {
            var t611 int
            var inline1377 int = ref_get__Ref_3int(i__108)
            t611 = inline1377
            var mtmp228 SExpr = vec_get__Vec_5SExpr(items__107, t611)
            switch mtmp228.(type) {
            case SExpr_Sym:
                var x231 string = mtmp228.(SExpr_Sym)._0
                var t613 *_goml_vec_string
                var inline1371 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
                t613 = inline1371
                var t614 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(t613, x231)
                ref_set__Ref_11Vec_6string(params__110, t614)
                var t615 int
                var inline1367 int = ref_get__Ref_3int(i__108)
                t615 = inline1367
                var t616 int = t615 + 1
                ref_set__Ref_3int(i__108, t616)
                continue
            default:
                var t618 int
                var inline1375 int = ref_get__Ref_3int(i__108)
                t618 = inline1375
                var t619 int = t618 + 1
                ref_set__Ref_3int(i__108, t619)
                continue
            }
        } else {
            break Loop_loop607
        }
    }
    var inline1383 *_goml_vec_string = ref_get__Ref_11Vec_6string(params__110)
    return inline1383
}

func eval_args(items__112 *_goml_vec_SExpr, start__113 int, local__114 *_goml_vec_Binding, global__115 *ref_Vec_7Binding_x) *_goml_vec_Value {
    var i__116 *ref_int_x
    var inline1412 *ref_int_x = ref__Ref_3int(start__113)
    i__116 = inline1412
    var acc__117 *_goml_vec_Value
    var inline1410 *_goml_vec_Value = vec_new__Vec_5Value()
    acc__117 = inline1410
    var args__118 *ref_Vec_5Value_x
    var inline1408 *ref_Vec_5Value_x = ref__Ref_10Vec_5Value(acc__117)
    args__118 = inline1408
    Loop_loop625:
    for {
        var t626 int
        var inline1404 int = ref_get__Ref_3int(i__116)
        t626 = inline1404
        var t627 int
        var inline1402 int = vec_len__Vec_5SExpr(items__112)
        t627 = inline1402
        var t628 bool = t626 < t627
        if t628 {
            var t629 int
            var inline1400 int = ref_get__Ref_3int(i__116)
            t629 = inline1400
            var t630 SExpr = vec_get__Vec_5SExpr(items__112, t629)
            var v__119 Value = eval(t630, local__114, global__115)
            var t631 *_goml_vec_Value
            var inline1398 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
            t631 = inline1398
            var t632 *_goml_vec_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(t631, v__119)
            ref_set__Ref_10Vec_5Value(args__118, t632)
            var t633 int
            var inline1394 int = ref_get__Ref_3int(i__116)
            t633 = inline1394
            var t634 int = t633 + 1
            ref_set__Ref_3int(i__116, t634)
            continue
        } else {
            break Loop_loop625
        }
    }
    var inline1406 *_goml_vec_Value = ref_get__Ref_10Vec_5Value(args__118)
    return inline1406
}

func apply_builtin(name__120 string, args__121 *_goml_vec_Value) Value {
    switch name__120 {
    case "=":
        var t642 int
        var inline1421 int = vec_len__Vec_5Value(args__121)
        t642 = inline1421
        var t643 bool
        var inline1418 int = 2
        var inline1419 bool = t642 == inline1418
        t643 = inline1419
        if t643 {
            var t644 Value = vec_get__Vec_5Value(args__121, 0)
            var t645 Value = vec_get__Vec_5Value(args__121, 1)
            switch t645.(type) {
            case Value_Int:
                var x240 int32 = t645.(Value_Int)._0
                switch t644.(type) {
                case Value_Int:
                    var x243 int32 = t644.(Value_Int)._0
                    var t650 bool
                    var inline1414 bool = x243 == x240
                    t650 = inline1414
                    var t651 Value = Value_Bool{
                        _0: t650,
                    }
                    return t651
                default:
                    var t652 Value = Value_Bool{
                        _0: false,
                    }
                    return t652
                }
            case Value_Bool:
                var x241 bool = t645.(Value_Bool)._0
                switch t644.(type) {
                case Value_Bool:
                    var x247 bool = t644.(Value_Bool)._0
                    var t655 bool
                    var inline1416 bool = x247 == x241
                    t655 = inline1416
                    var t656 Value = Value_Bool{
                        _0: t655,
                    }
                    return t656
                default:
                    var t657 Value = Value_Bool{
                        _0: false,
                    }
                    return t657
                }
            default:
                var t658 Value = Value_Bool{
                    _0: false,
                }
                return t658
            }
        } else {
            var t659 Value = Value_Bool{
                _0: false,
            }
            return t659
        }
    case "+":
        var i__126 *ref_int_x
        var inline1446 int = 0
        var inline1447 *ref_int_x = ref__Ref_3int(inline1446)
        i__126 = inline1447
        var acc__127 *ref_int32_x
        var inline1443 int32 = 0
        var inline1444 *ref_int32_x = ref__Ref_5int32(inline1443)
        acc__127 = inline1444
        Loop_loop663:
        for {
            var t664 int
            var inline1439 int = ref_get__Ref_3int(i__126)
            t664 = inline1439
            var t665 int
            var inline1437 int = vec_len__Vec_5Value(args__121)
            t665 = inline1437
            var t666 bool = t664 < t665
            if t666 {
                var t667 int
                var inline1435 int = ref_get__Ref_3int(i__126)
                t667 = inline1435
                var mtmp249 Value = vec_get__Vec_5Value(args__121, t667)
                switch mtmp249.(type) {
                case Value_Int:
                    var x250 int32 = mtmp249.(Value_Int)._0
                    var t669 int32
                    var inline1429 int32 = ref_get__Ref_5int32(acc__127)
                    t669 = inline1429
                    var t670 int32 = t669 + x250
                    ref_set__Ref_5int32(acc__127, t670)
                    var t671 int
                    var inline1425 int = ref_get__Ref_3int(i__126)
                    t671 = inline1425
                    var t672 int = t671 + 1
                    ref_set__Ref_3int(i__126, t672)
                    continue
                default:
                    var t674 int
                    var inline1433 int = ref_get__Ref_3int(i__126)
                    t674 = inline1433
                    var t675 int = t674 + 1
                    ref_set__Ref_3int(i__126, t675)
                    continue
                }
            } else {
                break Loop_loop663
            }
        }
        var t661 int32
        var inline1441 int32 = ref_get__Ref_5int32(acc__127)
        t661 = inline1441
        var t662 Value = Value_Int{
            _0: t661,
        }
        return t662
    case "*":
        var i__129 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var acc__130 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(1)
        Loop_loop680:
        for {
            var t681 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(i__129)
            var t682 int
            var inline1463 int = vec_len__Vec_5Value(args__121)
            t682 = inline1463
            var t683 bool = t681 < t682
            if t683 {
                var t684 int
                var inline1461 int = ref_get__Ref_3int(i__129)
                t684 = inline1461
                var mtmp255 Value = vec_get__Vec_5Value(args__121, t684)
                switch mtmp255.(type) {
                case Value_Int:
                    var x256 int32 = mtmp255.(Value_Int)._0
                    var t686 int32
                    var inline1455 int32 = ref_get__Ref_5int32(acc__130)
                    t686 = inline1455
                    var t687 int32 = t686 * x256
                    ref_set__Ref_5int32(acc__130, t687)
                    var t688 int
                    var inline1451 int = ref_get__Ref_3int(i__129)
                    t688 = inline1451
                    var t689 int = t688 + 1
                    ref_set__Ref_3int(i__129, t689)
                    continue
                default:
                    var t691 int
                    var inline1459 int = ref_get__Ref_3int(i__129)
                    t691 = inline1459
                    var t692 int = t691 + 1
                    ref_set__Ref_3int(i__129, t692)
                    continue
                }
            } else {
                break Loop_loop680
            }
        }
        var t678 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(acc__130)
        var t679 Value = Value_Int{
            _0: t678,
        }
        return t679
    case "-":
        var mtmp261 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        switch mtmp261 {
        case 1:
            var mtmp262 Value = vec_get__Vec_5Value(args__121, 0)
            switch mtmp262.(type) {
            case Value_Int:
                var x263 int32 = mtmp262.(Value_Int)._0
                var t698 int32 = 0 - x263
                var t699 Value = Value_Int{
                    _0: t698,
                }
                return t699
            default:
                return Nil{}
            }
        case 2:
            var t700 Value = vec_get__Vec_5Value(args__121, 0)
            var t701 Value = vec_get__Vec_5Value(args__121, 1)
            switch t701.(type) {
            case Value_Int:
                var x269 int32 = t701.(Value_Int)._0
                switch t700.(type) {
                case Value_Int:
                    var x272 int32 = t700.(Value_Int)._0
                    var t706 int32 = x272 - x269
                    var t707 Value = Value_Int{
                        _0: t706,
                    }
                    return t707
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
        var t710 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(args__121)
        var t711 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t710, 2)
        if t711 {
            var t712 Value = vec_get__Vec_5Value(args__121, 0)
            var t713 Value = vec_get__Vec_5Value(args__121, 1)
            switch t713.(type) {
            case Value_Int:
                var x278 int32 = t713.(Value_Int)._0
                switch t712.(type) {
                case Value_Int:
                    var x281 int32 = t712.(Value_Int)._0
                    var t718 int32 = x281 / x278
                    var t719 Value = Value_Int{
                        _0: t718,
                    }
                    return t719
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
        var x286 Lambda = func__137.(Func)._0
        var t724 Value = apply_lambda(x286, args__138)
        return t724
    default:
        return Nil{}
    }
}

func apply_lambda(lambda__141 Lambda, args__142 *_goml_vec_Value) Value {
    var t727 *_goml_vec_Binding = lambda__141.env
    var env__143 *ref_Vec_7Binding_x
    var inline1490 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t727)
    env__143 = inline1490
    var i__144 *ref_int_x
    var inline1487 int = 0
    var inline1488 *ref_int_x = ref__Ref_3int(inline1487)
    i__144 = inline1488
    Loop_loop733:
    for {
        var t744 int
        var inline1483 int = ref_get__Ref_3int(i__144)
        t744 = inline1483
        var t745 *_goml_vec_string = lambda__141.params
        var t746 int
        var inline1481 int = vec_len__Vec_6string(t745)
        t746 = inline1481
        var t747 bool = t744 < t746
        var jp735 bool
        if t747 {
            var t748 int
            var inline1467 int = ref_get__Ref_3int(i__144)
            t748 = inline1467
            var t749 int
            var inline1465 int = vec_len__Vec_5Value(args__142)
            t749 = inline1465
            var t750 bool = t748 < t749
            jp735 = t750
        } else {
            jp735 = false
        }
        if jp735 {
            var t736 *_goml_vec_string = lambda__141.params
            var t737 int
            var inline1479 int = ref_get__Ref_3int(i__144)
            t737 = inline1479
            var name__145 string = vec_get__Vec_6string(t736, t737)
            var t738 int
            var inline1477 int = ref_get__Ref_3int(i__144)
            t738 = inline1477
            var value__146 Value = vec_get__Vec_5Value(args__142, t738)
            var t739 *_goml_vec_Binding
            var inline1475 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
            t739 = inline1475
            var t740 Binding = Binding{
                name: name__145,
                value: value__146,
            }
            var updated__147 *_goml_vec_Binding = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(t739, t740)
            ref_set__Ref_12Vec_7Binding(env__143, updated__147)
            var t741 int
            var inline1471 int = ref_get__Ref_3int(i__144)
            t741 = inline1471
            var t742 int = t741 + 1
            ref_set__Ref_3int(i__144, t742)
            continue
        } else {
            break Loop_loop733
        }
    }
    var t729 SExpr = lambda__141.body
    var t730 *_goml_vec_Binding
    var inline1485 *_goml_vec_Binding = ref_get__Ref_12Vec_7Binding(env__143)
    t730 = inline1485
    var t731 *ref_Vec_7Binding_x = lambda__141.global
    var t732 Value = eval(t729, t730, t731)
    return t732
}

func main0() struct{} {
    var t752 *_goml_vec_Binding
    var inline1520 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t752 = inline1520
    var global__148 *ref_Vec_7Binding_x
    var inline1518 *ref_Vec_7Binding_x = ref__Ref_12Vec_7Binding(t752)
    global__148 = inline1518
    var program__149 string = "(begin (define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) (define add3 (lambda (a b c) (+ a (+ b c)))) (fact 6))"
    var t753 *_goml_vec_Token = lex(program__149)
    var exprs__150 *_goml_vec_SExpr = parse_program(t753)
    var t754 SExpr = vec_get__Vec_5SExpr(exprs__150, 0)
    var t755 *_goml_vec_Binding
    var inline1516 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t755 = inline1516
    var result__151 Value = eval(t754, t755, global__148)
    var t756 string
    switch result__151.(type) {
    case Value_Int:
        var inline1508 int32 = result__151.(Value_Int)._0
        var inline1510 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1508)
        t756 = inline1510
    case Value_Bool:
        var inline1511 bool = result__151.(Value_Bool)._0
        var inline1513 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1511)
        t756 = inline1513
    case Func:
        t756 = "<lambda>"
    case Nil:
        t756 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1505 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t756)
    _goml_runtime_core_string_println(inline1505)
    var t757 *_goml_vec_Token = lex("(add3 10 20 30)")
    var exprs2__152 *_goml_vec_SExpr = parse_program(t757)
    var t758 SExpr = vec_get__Vec_5SExpr(exprs2__152, 0)
    var t759 *_goml_vec_Binding
    var inline1503 *_goml_vec_Binding = vec_new__Vec_7Binding()
    t759 = inline1503
    var result2__153 Value = eval(t758, t759, global__148)
    var t760 string
    switch result2__153.(type) {
    case Value_Int:
        var inline1495 int32 = result2__153.(Value_Int)._0
        var inline1497 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline1495)
        t760 = inline1497
    case Value_Bool:
        var inline1498 bool = result2__153.(Value_Bool)._0
        var inline1500 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1498)
        t760 = inline1500
    case Func:
        t760 = "<lambda>"
    case Nil:
        t760 = "nil"
    default:
        panic("non-exhaustive match")
    }
    var inline1492 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t760)
    _goml_runtime_core_string_println(inline1492)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_len(self__8 string) int {
    var t763 int = _goml_runtime_core_string_len(self__8)
    return t763
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var t766 bool = self__59 == other__60
    return t766
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t769 *ref_int_x = ref__Ref_3int(value__207)
    return t769
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__bool(value__207 bool) *ref_bool_x {
    var t772 *ref_bool_x = ref__Ref_4bool(value__207)
    return t772
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t778 int = ref_get__Ref_3int(self__208)
    return t778
}

func _goml_m_trait__impl_i_Eq_i_char_i_eq(self__57 rune, other__58 rune) bool {
    var t784 bool = self__57 == other__58
    return t784
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__207 int32) *ref_int32_x {
    var t791 *ref_int32_x = ref__Ref_5int32(value__207)
    return t791
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__208 *ref_int32_x) int32 {
    var t794 int32 = ref_get__Ref_5int32(self__208)
    return t794
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Token(self__128 *_goml_vec_Token, elem__129 Token) *_goml_vec_Token {
    var t819 int
    var inline1530 int = vec_len__Vec_5Token(self__128)
    t819 = inline1530
    var t820 int = t819 + 1
    var result__130 *_goml_vec_Token
    var inline1528 *_goml_vec_Token = vec_with_capacity__Vec_5Token(t820)
    result__130 = inline1528
    var index__131 int = 0
    Loop_loop822:
    for {
        var t823 int
        var inline1524 int = vec_len__Vec_5Token(self__128)
        t823 = inline1524
        var t824 bool = index__131 < t823
        if t824 {
            var t825 Token = vec_get__Vec_5Token(self__128, index__131)
            vec_push__Vec_5Token(result__130, t825)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t826 int = compound_old38 + compound_value39
            index__131 = t826
            continue
        } else {
            break Loop_loop822
        }
    }
    vec_push__Vec_5Token(result__130, elem__129)
    return result__130
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__SExpr(self__128 *_goml_vec_SExpr, elem__129 SExpr) *_goml_vec_SExpr {
    var t858 int
    var inline1540 int = vec_len__Vec_5SExpr(self__128)
    t858 = inline1540
    var t859 int = t858 + 1
    var result__130 *_goml_vec_SExpr
    var inline1538 *_goml_vec_SExpr = vec_with_capacity__Vec_5SExpr(t859)
    result__130 = inline1538
    var index__131 int = 0
    Loop_loop861:
    for {
        var t862 int
        var inline1534 int = vec_len__Vec_5SExpr(self__128)
        t862 = inline1534
        var t863 bool = index__131 < t862
        if t863 {
            var t864 SExpr = vec_get__Vec_5SExpr(self__128, index__131)
            vec_push__Vec_5SExpr(result__130, t864)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t865 int = compound_old38 + compound_value39
            index__131 = t865
            continue
        } else {
            break Loop_loop861
        }
    }
    vec_push__Vec_5SExpr(result__130, elem__129)
    return result__130
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t871 string = _goml_runtime_core_int32_to_string(self__6)
    return t871
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t874 string = _goml_runtime_core_bool_to_string(self__37)
    return t874
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var t877 bool = self__65 == other__66
    return t877
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__SExpr(self__137 *_goml_vec_SExpr) int {
    var t883 int = vec_len__Vec_5SExpr(self__137)
    return t883
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Binding(self__128 *_goml_vec_Binding, elem__129 Binding) *_goml_vec_Binding {
    var t886 int
    var inline1550 int = vec_len__Vec_7Binding(self__128)
    t886 = inline1550
    var t887 int = t886 + 1
    var result__130 *_goml_vec_Binding
    var inline1548 *_goml_vec_Binding = vec_with_capacity__Vec_7Binding(t887)
    result__130 = inline1548
    var index__131 int = 0
    Loop_loop889:
    for {
        var t890 int
        var inline1544 int = vec_len__Vec_7Binding(self__128)
        t890 = inline1544
        var t891 bool = index__131 < t890
        if t891 {
            var t892 Binding = vec_get__Vec_7Binding(self__128, index__131)
            vec_push__Vec_7Binding(result__130, t892)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t893 int = compound_old38 + compound_value39
            index__131 = t893
            continue
        } else {
            break Loop_loop889
        }
    }
    vec_push__Vec_7Binding(result__130, elem__129)
    return result__130
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__string(self__128 *_goml_vec_string, elem__129 string) *_goml_vec_string {
    var t908 int
    var inline1560 int = vec_len__Vec_6string(self__128)
    t908 = inline1560
    var t909 int = t908 + 1
    var result__130 *_goml_vec_string
    var inline1558 *_goml_vec_string = vec_with_capacity__Vec_6string(t909)
    result__130 = inline1558
    var index__131 int = 0
    Loop_loop911:
    for {
        var t912 int
        var inline1554 int = vec_len__Vec_6string(self__128)
        t912 = inline1554
        var t913 bool = index__131 < t912
        if t913 {
            var t914 string = vec_get__Vec_6string(self__128, index__131)
            vec_push__Vec_6string(result__130, t914)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t915 int = compound_old38 + compound_value39
            index__131 = t915
            continue
        } else {
            break Loop_loop911
        }
    }
    vec_push__Vec_6string(result__130, elem__129)
    return result__130
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_pushed____T__Value(self__128 *_goml_vec_Value, elem__129 Value) *_goml_vec_Value {
    var t930 int
    var inline1570 int = vec_len__Vec_5Value(self__128)
    t930 = inline1570
    var t931 int = t930 + 1
    var result__130 *_goml_vec_Value
    var inline1568 *_goml_vec_Value = vec_with_capacity__Vec_5Value(t931)
    result__130 = inline1568
    var index__131 int = 0
    Loop_loop933:
    for {
        var t934 int
        var inline1564 int = vec_len__Vec_5Value(self__128)
        t934 = inline1564
        var t935 bool = index__131 < t934
        if t935 {
            var t936 Value = vec_get__Vec_5Value(self__128, index__131)
            vec_push__Vec_5Value(result__130, t936)
            var compound_old38 int = index__131
            var compound_value39 int = 1
            var t937 int = compound_old38 + compound_value39
            index__131 = t937
            continue
        } else {
            break Loop_loop933
        }
    }
    vec_push__Vec_5Value(result__130, elem__129)
    return result__130
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__Value(self__137 *_goml_vec_Value) int {
    var t943 int = vec_len__Vec_5Value(self__137)
    return t943
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
