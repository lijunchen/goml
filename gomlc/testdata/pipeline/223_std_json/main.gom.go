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

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    if !_goml_runtime_core_string_is_char_boundary(s, start) && _goml_runtime_core_string_is_char_boundary(s, end) {
        panic("invalid string byte slice")
    }
    return s[start:end]
}

func _goml_runtime_core_string_is_char_boundary(s string, i int) bool {
    if i < 0 || i > int(len(s)) {
        return false
    }
    if i == int(len(s)) {
        return true
    }
    return _goml_utf8.RuneStart(s[i])
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    if !_goml_utf8.Valid(bytes.items) {
        return Tuple2_4bool_6string{
            _0: false,
            _1: "",
        }
    }
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_to_string(x rune) string {
    if !_goml_utf8.ValidRune(x) {
        panic("invalid char")
    }
    return string(x)
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    if value > 1114111 || value >= 55296 && value <= 57343 {
        return Tuple2_4bool_4char{
            _0: false,
            _1: 0,
        }
    }
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_new__Vec_5uint8() *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: nil,
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

type _goml_vec__goml_m_std_p_json_p_Value struct {
    items []_goml_m_std_p_json_p_Value
}

func vec_new___goml_m_Vec__16std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    return &_goml_vec__goml_m_std_p_json_p_Value{
        items: nil,
    }
}

func vec_push___goml_m_Vec__16std_p_json_p_Value(vec *_goml_vec__goml_m_std_p_json_p_Value, elem _goml_m_std_p_json_p_Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get___goml_m_Vec__16std_p_json_p_Value(vec *_goml_vec__goml_m_std_p_json_p_Value, index int) _goml_m_std_p_json_p_Value {
    return vec.items[index]
}

func vec_len___goml_m_Vec__16std_p_json_p_Value(vec *_goml_vec__goml_m_std_p_json_p_Value) int {
    return int(len(vec.items))
}

type _goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value struct {
    items []Tuple2_6string_26_goml_m_std_p_json_p_Value
}

func vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    return &_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value{
        items: nil,
    }
}

func vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(vec *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(vec *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, index int) Tuple2_6string_26_goml_m_std_p_json_p_Value {
    return vec.items[index]
}

func vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(vec *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value) int {
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

type ref_uint32_x struct {
    value uint32
}

func ref__Ref_6uint32(value uint32) *ref_uint32_x {
    return &ref_uint32_x{
        value: value,
    }
}

func ref_get__Ref_6uint32(reference *ref_uint32_x) uint32 {
    return reference.value
}

func ref_set__Ref_6uint32(reference *ref_uint32_x, value uint32) struct{} {
    reference.value = value
    return struct{}{}
}

type ref__goml_m_Option____std_p_json_p_Value_x struct {
    value _goml_m_Option____std_p_json_p_Value
}

func ref___goml_m_Ref__24Option____std_p_json_p_Value(value _goml_m_Option____std_p_json_p_Value) *ref__goml_m_Option____std_p_json_p_Value_x {
    return &ref__goml_m_Option____std_p_json_p_Value_x{
        value: value,
    }
}

func ref_get___goml_m_Ref__24Option____std_p_json_p_Value(reference *ref__goml_m_Option____std_p_json_p_Value_x) _goml_m_Option____std_p_json_p_Value {
    return reference.value
}

func ref_set___goml_m_Ref__24Option____std_p_json_p_Value(reference *ref__goml_m_Option____std_p_json_p_Value_x, value _goml_m_Option____std_p_json_p_Value) struct{} {
    reference.value = value
    return struct{}{}
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_6string_26_goml_m_std_p_json_p_Value struct {
    _0 string
    _1 _goml_m_std_p_json_p_Value
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_text_p_StringBuilder struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_json_p_JsonParser struct {
    input string
    index *ref_int_x
}

type _goml_m_std_p_json_p_Value interface {
    is_goml_m_std_p_json_p_Value()
}

type Object struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
}

func (_ Object) is_goml_m_std_p_json_p_Value() {}

type Array struct {
    _0 *_goml_vec__goml_m_std_p_json_p_Value
}

func (_ Array) is_goml_m_std_p_json_p_Value() {}

type String struct {
    _0 string
}

func (_ String) is_goml_m_std_p_json_p_Value() {}

type Number struct {
    _0 string
}

func (_ Number) is_goml_m_std_p_json_p_Value() {}

type Bool struct {
    _0 bool
}

func (_ Bool) is_goml_m_std_p_json_p_Value() {}

type Null struct {}

func (_ Null) is_goml_m_std_p_json_p_Value() {}

type Option__uint8 interface {
    isOption__uint8()
}

type Option__uint8_None struct {}

func (_ Option__uint8_None) isOption__uint8() {}

type Option__uint8_Some struct {
    _0 uint8
}

func (_ Option__uint8_Some) isOption__uint8() {}

type Result__string__string interface {
    isResult__string__string()
}

type Result__string__string_Ok struct {
    _0 string
}

func (_ Result__string__string_Ok) isResult__string__string() {}

type Result__string__string_Err struct {
    _0 string
}

func (_ Result__string__string_Err) isResult__string__string() {}

type Result__unit__string interface {
    isResult__unit__string()
}

type Result__unit__string_Ok struct {
    _0 struct{}
}

func (_ Result__unit__string_Ok) isResult__unit__string() {}

type Result__unit__string_Err struct {
    _0 string
}

func (_ Result__unit__string_Err) isResult__unit__string() {}

type Option__uint32 interface {
    isOption__uint32()
}

type Option__uint32_None struct {}

func (_ Option__uint32_None) isOption__uint32() {}

type Option__uint32_Some struct {
    _0 uint32
}

func (_ Option__uint32_Some) isOption__uint32() {}

type Result__uint32__string interface {
    isResult__uint32__string()
}

type Result__uint32__string_Ok struct {
    _0 uint32
}

func (_ Result__uint32__string_Ok) isResult__uint32__string() {}

type Result__uint32__string_Err struct {
    _0 string
}

func (_ Result__uint32__string_Err) isResult__uint32__string() {}

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

type _goml_m_Result____std_p_json_p_Value____string interface {
    is_goml_m_Result____std_p_json_p_Value____string()
}

type _goml_m_Result____std_p_json_p_Value____string_Ok struct {
    _0 _goml_m_std_p_json_p_Value
}

func (_ _goml_m_Result____std_p_json_p_Value____string_Ok) is_goml_m_Result____std_p_json_p_Value____string() {}

type _goml_m_Result____std_p_json_p_Value____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_json_p_Value____string_Err) is_goml_m_Result____std_p_json_p_Value____string() {}

type _goml_m_Option____std_p_json_p_Value interface {
    is_goml_m_Option____std_p_json_p_Value()
}

type _goml_m_Option____std_p_json_p_Value_None struct {}

func (_ _goml_m_Option____std_p_json_p_Value_None) is_goml_m_Option____std_p_json_p_Value() {}

type _goml_m_Option____std_p_json_p_Value_Some struct {
    _0 _goml_m_std_p_json_p_Value
}

func (_ _goml_m_Option____std_p_json_p_Value_Some) is_goml_m_Option____std_p_json_p_Value() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

type Option__bool interface {
    isOption__bool()
}

type Option__bool_None struct {}

func (_ Option__bool_None) isOption__bool() {}

type Option__bool_Some struct {
    _0 bool
}

func (_ Option__bool_Some) isOption__bool() {}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r_ interface {
    is_goml_m_Option____Vec_l_std_p_json_p_Value_r_()
}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r__None struct {}

func (_ _goml_m_Option____Vec_l_std_p_json_p_Value_r__None) is_goml_m_Option____Vec_l_std_p_json_p_Value_r_() {}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r__Some struct {
    _0 *_goml_vec__goml_m_std_p_json_p_Value
}

func (_ _goml_m_Option____Vec_l_std_p_json_p_Value_r__Some) is_goml_m_Option____Vec_l_std_p_json_p_Value_r_() {}

func _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new() _goml_m_std_p_text_p_StringBuilder {
    var retv148 _goml_m_std_p_text_p_StringBuilder
    var t149 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var t150 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: t149,
    }
    retv148 = t150
    return retv148
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var for_index0 int = 0
    var for_limit1 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    Loop_loop166:
    for {
        var t167 bool = for_index0 < for_limit1
        if t167 {
            var for_item2 int = for_index0
            var t168 int = for_index0 + 1
            for_index0 = t168
            var index__5 int = for_item2
            var t169 *_goml_vec_uint8 = self__3.values
            var t170 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__4, index__5)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(t169, t170)
            continue
        } else {
            break Loop_loop166
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__6 _goml_m_std_p_text_p_StringBuilder, value__7 rune) struct{} {
    var t173 string = _goml_m_inherent_i_char_i_char_i_to__string(value__7)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__6, t173)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(self__15 _goml_m_std_p_text_p_StringBuilder) string {
    var retv187 string
    var t188 *_goml_vec_uint8 = self__15.values
    var mtmp9 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t188)
    var x11 string = mtmp9._1
    var value__16 string = x11
    retv187 = value__16
    return retv187
}

func _goml_m_std_p_json_p_json__parser(input__0 string) _goml_m_std_p_json_p_JsonParser {
    var retv193 _goml_m_std_p_json_p_JsonParser
    var t194 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t195 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__0,
        index: t194,
    }
    retv193 = t195
    return retv193
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var retv197 string
    var t198 string = message__2 + " at byte "
    var t199 *ref_int_x = value__1.index
    var t200 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t199)
    var t201 string = _goml_m_inherent_i_int_i_int_i_to__string(t200)
    var t202 string = t198 + t201
    retv197 = t202
    return retv197
}

func _goml_m_std_p_json_p_json__whitespace(value__3 uint8) bool {
    var retv204 bool
    var t213 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 9)
    var jp211 bool
    if t213 {
        jp211 = true
    } else {
        var t214 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 10)
        jp211 = t214
    }
    var jp208 bool
    if jp211 {
        jp208 = true
    } else {
        var t212 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 13)
        jp208 = t212
    }
    var jp206 bool
    if jp208 {
        jp206 = true
    } else {
        var t209 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 32)
        jp206 = t209
    }
    retv204 = jp206
    return retv204
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop217:
    for {
        var t225 *ref_int_x = value__4.index
        var t226 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t225)
        var t227 string = value__4.input
        var t228 int = _goml_m_inherent_i_string_i_string_i_byte__len(t227)
        var t229 bool = t226 < t228
        var jp219 bool
        if t229 {
            var t230 string = value__4.input
            var t231 *ref_int_x = value__4.index
            var t232 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t231)
            var t233 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t230, t232)
            var t234 bool = _goml_m_std_p_json_p_json__whitespace(t233)
            jp219 = t234
        } else {
            jp219 = false
        }
        if jp219 {
            var t220 *ref_int_x = value__4.index
            var t221 *ref_int_x = value__4.index
            var t222 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t221)
            var t223 int = t222 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t220, t223)
            continue
        } else {
            break Loop_loop217
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var retv236 Option__uint32
    var t262 bool = value__5 >= 48
    var jp240 bool
    if t262 {
        var t263 bool = value__5 <= 57
        jp240 = t263
    } else {
        jp240 = false
    }
    var jp238 Option__uint32
    if jp240 {
        var t241 uint8 = value__5 - 48
        var t242 uint32 = uint32(uint8(t241))
        var t243 Option__uint32 = Option__uint32_Some{
            _0: t242,
        }
        jp238 = t243
    } else {
        var t260 bool = value__5 >= 65
        var jp247 bool
        if t260 {
            var t261 bool = value__5 <= 70
            jp247 = t261
        } else {
            jp247 = false
        }
        var jp245 Option__uint32
        if jp247 {
            var t248 uint8 = value__5 - 55
            var t249 uint32 = uint32(uint8(t248))
            var t250 Option__uint32 = Option__uint32_Some{
                _0: t249,
            }
            jp245 = t250
        } else {
            var t258 bool = value__5 >= 97
            var jp254 bool
            if t258 {
                var t259 bool = value__5 <= 102
                jp254 = t259
            } else {
                jp254 = false
            }
            var jp252 Option__uint32
            if jp254 {
                var t255 uint8 = value__5 - 87
                var t256 uint32 = uint32(uint8(t255))
                var t257 Option__uint32 = Option__uint32_Some{
                    _0: t256,
                }
                jp252 = t257
            } else {
                jp252 = Option__uint32_None{}
            }
            jp245 = jp252
        }
        jp238 = jp245
    }
    retv236 = jp238
    return retv236
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var retv265 Result__uint32__string
    var t268 *ref_int_x = value__6.index
    var t269 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t268)
    var t270 int = t269 + 4
    var t271 string = value__6.input
    var t272 int = _goml_m_inherent_i_string_i_string_i_byte__len(t271)
    var t273 bool = t270 > t272
    var jp267 Result__uint32__string
    if t273 {
        var t274 string = _goml_m_std_p_json_p_json__error(value__6, "incomplete unicode escape")
        var t275 Result__uint32__string = Result__uint32__string_Err{
            _0: t274,
        }
        jp267 = t275
        retv265 = jp267
        return retv265
    } else {
        var t276_source int = 0
        var t276 uint32 = uint32(int(t276_source))
        var result__7 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(t276)
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop284:
        for {
            var t285 bool = for_index0 < for_limit1
            if t285 {
                var for_item2 int = for_index0
                var t286 int = for_index0 + 1
                for_index0 = t286
                var offset__8 int = for_item2
                var t287 string = value__6.input
                var t288 *ref_int_x = value__6.index
                var t289 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t288)
                var t290 int = t289 + offset__8
                var t291 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t287, t290)
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t291)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t293 string = _goml_m_std_p_json_p_json__error(value__6, "invalid unicode escape")
                    var t294 Result__uint32__string = Result__uint32__string_Err{
                        _0: t293,
                    }
                    retv265 = t294
                    return retv265
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var digit__9 uint32 = x5
                    var t295 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
                    var t296 uint32 = t295 * 16
                    var t297 uint32 = t296 + digit__9
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(result__7, t297)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop284
            }
        }
        var t278 *ref_int_x = value__6.index
        var t279 *ref_int_x = value__6.index
        var t280 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t279)
        var t281 int = t280 + 4
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t278, t281)
        var t282 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
        var t283 Result__uint32__string = Result__uint32__string_Ok{
            _0: t282,
        }
        jp267 = t283
        retv265 = jp267
        return retv265
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var retv300 Result__unit__string
    var mtmp8 Option__char = char_from_uint32(codepoint__12)
    var jp302 Result__unit__string
    switch mtmp8.(type) {
    case Option__char_None:
        var t303 string = _goml_m_std_p_json_p_json__error(value__10, "invalid unicode codepoint")
        var t304 Result__unit__string = Result__unit__string_Err{
            _0: t303,
        }
        jp302 = t304
    case Option__char_Some:
        var x9 rune = mtmp8.(Option__char_Some)._0
        var character__13 rune = x9
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__11, character__13)
        var t305 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp302 = t305
    default:
        panic("non-exhaustive match")
    }
    retv300 = jp302
    return retv300
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var retv307 Result__unit__string
    var mtmp11 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp309 uint32
    switch mtmp11.(type) {
    case Result__uint32__string_Ok:
        var x12 uint32 = mtmp11.(Result__uint32__string_Ok)._0
        var codepoint__16 uint32 = x12
        jp309 = codepoint__16
        var first__18 uint32 = jp309
        var t371 bool = first__18 >= 55296
        var jp313 bool
        if t371 {
            var t372 bool = first__18 <= 56319
            jp313 = t372
        } else {
            jp313 = false
        }
        var jp311 Result__unit__string
        if jp313 {
            var t350 *ref_int_x = value__14.index
            var t351 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t350)
            var t352 int = t351 + 2
            var t353 string = value__14.input
            var t354 int = _goml_m_inherent_i_string_i_string_i_byte__len(t353)
            var t355 bool = t352 > t354
            var jp342 bool
            if t355 {
                jp342 = true
            } else {
                var t356 string = value__14.input
                var t357 *ref_int_x = value__14.index
                var t358 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t357)
                var t359 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t356, t358)
                var t360 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t359, 92)
                var t361 bool = !t360
                jp342 = t361
            }
            var jp317 bool
            if jp342 {
                jp317 = true
            } else {
                var t343 string = value__14.input
                var t344 *ref_int_x = value__14.index
                var t345 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t344)
                var t346 int = t345 + 1
                var t347 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t343, t346)
                var t348 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t347, 117)
                var t349 bool = !t348
                jp317 = t349
            }
            var jp315 Result__unit__string
            if jp317 {
                var t318 string = _goml_m_std_p_json_p_json__error(value__14, "missing low surrogate")
                var t319 Result__unit__string = Result__unit__string_Err{
                    _0: t318,
                }
                jp315 = t319
                jp311 = jp315
                retv307 = jp311
                return retv307
            } else {
                var t320 *ref_int_x = value__14.index
                var t321 *ref_int_x = value__14.index
                var t322 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t321)
                var t323 int = t322 + 2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t320, t323)
                var mtmp15 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp325 uint32
                switch mtmp15.(type) {
                case Result__uint32__string_Ok:
                    var x16 uint32 = mtmp15.(Result__uint32__string_Ok)._0
                    var codepoint__19 uint32 = x16
                    jp325 = codepoint__19
                    var second__21 uint32 = jp325
                    var t338 bool = second__21 < 56320
                    var jp329 bool
                    if t338 {
                        jp329 = true
                    } else {
                        var t339 bool = second__21 > 57343
                        jp329 = t339
                    }
                    var jp327 Result__unit__string
                    if jp329 {
                        var t330 string = _goml_m_std_p_json_p_json__error(value__14, "invalid low surrogate")
                        var t331 Result__unit__string = Result__unit__string_Err{
                            _0: t330,
                        }
                        jp327 = t331
                    } else {
                        var t332 uint32 = first__18 - 55296
                        var t333 uint32 = t332 * 1024
                        var t334 uint32 = 65536 + t333
                        var t335 uint32 = t334 + second__21
                        var t336 uint32 = t335 - 56320
                        var t337 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, t336)
                        jp327 = t337
                    }
                    jp315 = jp327
                    jp311 = jp315
                    retv307 = jp311
                    return retv307
                case Result__uint32__string_Err:
                    var x17 string = mtmp15.(Result__uint32__string_Err)._0
                    var error__20 string = x17
                    var t340 Result__unit__string = Result__unit__string_Err{
                        _0: error__20,
                    }
                    retv307 = t340
                    return retv307
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t369 bool = first__18 >= 56320
            var jp365 bool
            if t369 {
                var t370 bool = first__18 <= 57343
                jp365 = t370
            } else {
                jp365 = false
            }
            var jp363 Result__unit__string
            if jp365 {
                var t366 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t367 Result__unit__string = Result__unit__string_Err{
                    _0: t366,
                }
                jp363 = t367
            } else {
                var t368 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, first__18)
                jp363 = t368
            }
            jp311 = jp363
            retv307 = jp311
            return retv307
        }
    case Result__uint32__string_Err:
        var x13 string = mtmp11.(Result__uint32__string_Err)._0
        var error__17 string = x13
        var t373 Result__unit__string = Result__unit__string_Err{
            _0: error__17,
        }
        retv307 = t373
        return retv307
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__22 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var retv375 Result__string__string
    var t496 *ref_int_x = value__22.index
    var t497 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t496)
    var t498 string = value__22.input
    var t499 int = _goml_m_inherent_i_string_i_string_i_byte__len(t498)
    var t500 bool = t497 >= t499
    var jp488 bool
    if t500 {
        jp488 = true
    } else {
        var t501 string = value__22.input
        var t502 *ref_int_x = value__22.index
        var t503 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t502)
        var t504 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t501, t503)
        var t505 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t504, 34)
        var t506 bool = !t505
        jp488 = t506
    }
    if jp488 {
        var t489 string = _goml_m_std_p_json_p_json__error(value__22, "expected string")
        var t490 Result__string__string = Result__string__string_Err{
            _0: t489,
        }
        retv375 = t490
        return retv375
    } else {
        var t491 *ref_int_x = value__22.index
        var t492 *ref_int_x = value__22.index
        var t493 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t492)
        var t494 int = t493 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t491, t494)
        var builder__23 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t377 *ref_int_x = value__22.index
        var t378 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t377)
        var segment__24 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t378)
        Loop_loop382:
        for {
            var t383 *ref_int_x = value__22.index
            var t384 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t383)
            var t385 string = value__22.input
            var t386 int = _goml_m_inherent_i_string_i_string_i_byte__len(t385)
            var t387 bool = t384 < t386
            if t387 {
                var t388 string = value__22.input
                var t389 *ref_int_x = value__22.index
                var t390 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t389)
                var byte__25 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t388, t390)
                var t392 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 34)
                if t392 {
                    var t400 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                    var t401 *ref_int_x = value__22.index
                    var t402 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t401)
                    var t403 bool = t400 < t402
                    if t403 {
                        var t404 string = value__22.input
                        var t405 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t406 *ref_int_x = value__22.index
                        var t407 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t406)
                        var t408 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t404, t405, t407)
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t408)
                    } else {}
                    var t394 *ref_int_x = value__22.index
                    var t395 *ref_int_x = value__22.index
                    var t396 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t395)
                    var t397 int = t396 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t394, t397)
                    var t398 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__23)
                    var t399 Result__string__string = Result__string__string_Ok{
                        _0: t398,
                    }
                    retv375 = t399
                    return retv375
                } else {
                    var t411 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 92)
                    if t411 {
                        var t468 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t469 *ref_int_x = value__22.index
                        var t470 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t469)
                        var t471 bool = t468 < t470
                        if t471 {
                            var t472 string = value__22.input
                            var t473 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                            var t474 *ref_int_x = value__22.index
                            var t475 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t474)
                            var t476 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t472, t473, t475)
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t476)
                        } else {}
                        var t413 *ref_int_x = value__22.index
                        var t414 *ref_int_x = value__22.index
                        var t415 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t414)
                        var t416 int = t415 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t413, t416)
                        var t461 *ref_int_x = value__22.index
                        var t462 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t461)
                        var t463 string = value__22.input
                        var t464 int = _goml_m_inherent_i_string_i_string_i_byte__len(t463)
                        var t465 bool = t462 >= t464
                        if t465 {
                            var t466 string = _goml_m_std_p_json_p_json__error(value__22, "incomplete escape")
                            var t467 Result__string__string = Result__string__string_Err{
                                _0: t466,
                            }
                            retv375 = t467
                            return retv375
                        } else {
                            var t418 string = value__22.input
                            var t419 *ref_int_x = value__22.index
                            var t420 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t419)
                            var escape__26 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t418, t420)
                            var t421 *ref_int_x = value__22.index
                            var t422 *ref_int_x = value__22.index
                            var t423 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t422)
                            var t424 int = t423 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t421, t424)
                            var t429 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 34)
                            if t429 {
                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 34)
                                var t426 *ref_int_x = value__22.index
                                var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t427)
                                continue
                            } else {
                                var t432 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 92)
                                if t432 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 92)
                                    var t426 *ref_int_x = value__22.index
                                    var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t427)
                                    continue
                                } else {
                                    var t435 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 47)
                                    if t435 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 47)
                                        var t426 *ref_int_x = value__22.index
                                        var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t427)
                                        continue
                                    } else {
                                        var t438 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 98)
                                        if t438 {
                                            var mtmp25 Option__char = char_from_uint32(8)
                                            switch mtmp25.(type) {
                                            case Option__char_None:
                                            case Option__char_Some:
                                                var x26 rune = mtmp25.(Option__char_Some)._0
                                                var character__27 rune = x26
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, character__27)
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                            var t426 *ref_int_x = value__22.index
                                            var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t427)
                                            continue
                                        } else {
                                            var t442 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 102)
                                            if t442 {
                                                var mtmp27 Option__char = char_from_uint32(12)
                                                switch mtmp27.(type) {
                                                case Option__char_None:
                                                case Option__char_Some:
                                                    var x28 rune = mtmp27.(Option__char_Some)._0
                                                    var character__28 rune = x28
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, character__28)
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                                var t426 *ref_int_x = value__22.index
                                                var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t427)
                                                continue
                                            } else {
                                                var t446 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 110)
                                                if t446 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 10)
                                                    var t426 *ref_int_x = value__22.index
                                                    var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t427)
                                                    continue
                                                } else {
                                                    var t449 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 114)
                                                    if t449 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 13)
                                                        var t426 *ref_int_x = value__22.index
                                                        var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t427)
                                                        continue
                                                    } else {
                                                        var t452 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 116)
                                                        if t452 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 9)
                                                            var t426 *ref_int_x = value__22.index
                                                            var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t427)
                                                            continue
                                                        } else {
                                                            var t455 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 117)
                                                            if t455 {
                                                                var mtmp29 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__22, builder__23)
                                                                switch mtmp29.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t426 *ref_int_x = value__22.index
                                                                    var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t427)
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x31 string = mtmp29.(Result__unit__string_Err)._0
                                                                    var error__29 string = x31
                                                                    var t458 Result__string__string = Result__string__string_Err{
                                                                        _0: error__29,
                                                                    }
                                                                    retv375 = t458
                                                                    return retv375
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t459 string = _goml_m_std_p_json_p_json__error(value__22, "invalid escape")
                                                                var t460 Result__string__string = Result__string__string_Err{
                                                                    _0: t459,
                                                                }
                                                                retv375 = t460
                                                                return retv375
                                                            }
                                                        }
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    } else {
                        var t479 bool = byte__25 < 32
                        if t479 {
                            var t480 string = _goml_m_std_p_json_p_json__error(value__22, "unescaped control character")
                            var t481 Result__string__string = Result__string__string_Err{
                                _0: t480,
                            }
                            retv375 = t481
                            return retv375
                        } else {
                            var t482 *ref_int_x = value__22.index
                            var t483 *ref_int_x = value__22.index
                            var t484 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t483)
                            var t485 int = t484 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t482, t485)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop382
            }
        }
        var t380 string = _goml_m_std_p_json_p_json__error(value__22, "unterminated string")
        var t381 Result__string__string = Result__string__string_Err{
            _0: t380,
        }
        retv375 = t381
        return retv375
    }
}

func _goml_m_std_p_json_p_json__digit(value__30 uint8) bool {
    var retv508 bool
    var t511 bool = value__30 >= 48
    var jp510 bool
    if t511 {
        var t512 bool = value__30 <= 57
        jp510 = t512
    } else {
        jp510 = false
    }
    retv508 = jp510
    return retv508
}

func _goml_m_std_p_json_p_parse__digits(value__31 _goml_m_std_p_json_p_JsonParser) bool {
    var retv514 bool
    var t515 *ref_int_x = value__31.index
    var start__32 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t515)
    Loop_loop520:
    for {
        var t528 *ref_int_x = value__31.index
        var t529 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t528)
        var t530 string = value__31.input
        var t531 int = _goml_m_inherent_i_string_i_string_i_byte__len(t530)
        var t532 bool = t529 < t531
        var jp522 bool
        if t532 {
            var t533 string = value__31.input
            var t534 *ref_int_x = value__31.index
            var t535 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t534)
            var t536 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t533, t535)
            var t537 bool = _goml_m_std_p_json_p_json__digit(t536)
            jp522 = t537
        } else {
            jp522 = false
        }
        if jp522 {
            var t523 *ref_int_x = value__31.index
            var t524 *ref_int_x = value__31.index
            var t525 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t524)
            var t526 int = t525 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t523, t526)
            continue
        } else {
            break Loop_loop520
        }
    }
    var t517 *ref_int_x = value__31.index
    var t518 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t517)
    var t519 bool = t518 > start__32
    retv514 = t519
    return retv514
}

func _goml_m_std_p_json_p_parse__json__number(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv539 _goml_m_Result____std_p_json_p_Value____string
    var t540 *ref_int_x = value__33.index
    var start__34 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t540)
    var t662 string = value__33.input
    var t663 *ref_int_x = value__33.index
    var t664 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t663)
    var t665 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t662, t664)
    var t666 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t665, 45)
    if t666 {
        var t667 *ref_int_x = value__33.index
        var t668 *ref_int_x = value__33.index
        var t669 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t668)
        var t670 int = t669 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t667, t670)
    } else {}
    var t625 *ref_int_x = value__33.index
    var t626 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t625)
    var t627 string = value__33.input
    var t628 int = _goml_m_inherent_i_string_i_string_i_byte__len(t627)
    var t629 bool = t626 >= t628
    if t629 {
        var t630 string = _goml_m_std_p_json_p_json__error(value__33, "incomplete number")
        var t631 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t630,
        }
        retv539 = t631
        return retv539
    } else {
        var t633 string = value__33.input
        var t634 *ref_int_x = value__33.index
        var t635 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t634)
        var t636 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t633, t635)
        var t637 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t636, 48)
        if t637 {
            var t638 *ref_int_x = value__33.index
            var t639 *ref_int_x = value__33.index
            var t640 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t639)
            var t641 int = t640 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t638, t641)
            var t647 *ref_int_x = value__33.index
            var t648 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t647)
            var t649 string = value__33.input
            var t650 int = _goml_m_inherent_i_string_i_string_i_byte__len(t649)
            var t651 bool = t648 < t650
            var jp644 bool
            if t651 {
                var t652 string = value__33.input
                var t653 *ref_int_x = value__33.index
                var t654 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t653)
                var t655 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t652, t654)
                var t656 bool = _goml_m_std_p_json_p_json__digit(t655)
                jp644 = t656
            } else {
                jp644 = false
            }
            if jp644 {
                var t645 string = _goml_m_std_p_json_p_json__error(value__33, "invalid leading zero")
                var t646 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t645,
                }
                retv539 = t646
                return retv539
            } else {
                var t615 *ref_int_x = value__33.index
                var t616 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t615)
                var t617 string = value__33.input
                var t618 int = _goml_m_inherent_i_string_i_string_i_byte__len(t617)
                var t619 bool = t616 < t618
                var jp605 bool
                if t619 {
                    var t620 string = value__33.input
                    var t621 *ref_int_x = value__33.index
                    var t622 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t621)
                    var t623 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t620, t622)
                    var t624 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t623, 46)
                    jp605 = t624
                } else {
                    jp605 = false
                }
                if jp605 {
                    var t606 *ref_int_x = value__33.index
                    var t607 *ref_int_x = value__33.index
                    var t608 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t607)
                    var t609 int = t608 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t606, t609)
                    var t611 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t612 bool = !t611
                    if t612 {
                        var t613 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t614 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t613,
                        }
                        retv539 = t614
                        return retv539
                    } else {
                        var t587 *ref_int_x = value__33.index
                        var t588 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t587)
                        var t589 string = value__33.input
                        var t590 int = _goml_m_inherent_i_string_i_string_i_byte__len(t589)
                        var t591 bool = t588 < t590
                        var jp552 bool
                        if t591 {
                            var t594 string = value__33.input
                            var t595 *ref_int_x = value__33.index
                            var t596 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t595)
                            var t597 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t594, t596)
                            var t598 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t597, 101)
                            var jp593 bool
                            if t598 {
                                jp593 = true
                            } else {
                                var t599 string = value__33.input
                                var t600 *ref_int_x = value__33.index
                                var t601 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t600)
                                var t602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t599, t601)
                                var t603 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t602, 69)
                                jp593 = t603
                            }
                            jp552 = jp593
                        } else {
                            jp552 = false
                        }
                        if jp552 {
                            var t553 *ref_int_x = value__33.index
                            var t554 *ref_int_x = value__33.index
                            var t555 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t554)
                            var t556 int = t555 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t553, t556)
                            var t570 *ref_int_x = value__33.index
                            var t571 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t570)
                            var t572 string = value__33.input
                            var t573 int = _goml_m_inherent_i_string_i_string_i_byte__len(t572)
                            var t574 bool = t571 < t573
                            var jp564 bool
                            if t574 {
                                var t577 string = value__33.input
                                var t578 *ref_int_x = value__33.index
                                var t579 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t578)
                                var t580 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t577, t579)
                                var t581 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t580, 43)
                                var jp576 bool
                                if t581 {
                                    jp576 = true
                                } else {
                                    var t582 string = value__33.input
                                    var t583 *ref_int_x = value__33.index
                                    var t584 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t583)
                                    var t585 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t582, t584)
                                    var t586 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t585, 45)
                                    jp576 = t586
                                }
                                jp564 = jp576
                            } else {
                                jp564 = false
                            }
                            if jp564 {
                                var t565 *ref_int_x = value__33.index
                                var t566 *ref_int_x = value__33.index
                                var t567 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t566)
                                var t568 int = t567 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t565, t568)
                            } else {}
                            var t559 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t560 bool = !t559
                            if t560 {
                                var t561 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t562 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t561,
                                }
                                retv539 = t562
                                return retv539
                            } else {
                                var t545 string = value__33.input
                                var t546 *ref_int_x = value__33.index
                                var t547 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t546)
                                var t548 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t545, start__34, t547)
                                var t549 _goml_m_std_p_json_p_Value = Number{
                                    _0: t548,
                                }
                                var t550 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t549,
                                }
                                retv539 = t550
                                return retv539
                            }
                        } else {
                            var t545 string = value__33.input
                            var t546 *ref_int_x = value__33.index
                            var t547 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t546)
                            var t548 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t545, start__34, t547)
                            var t549 _goml_m_std_p_json_p_Value = Number{
                                _0: t548,
                            }
                            var t550 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t549,
                            }
                            retv539 = t550
                            return retv539
                        }
                    }
                } else {
                    var t587 *ref_int_x = value__33.index
                    var t588 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t587)
                    var t589 string = value__33.input
                    var t590 int = _goml_m_inherent_i_string_i_string_i_byte__len(t589)
                    var t591 bool = t588 < t590
                    var jp552 bool
                    if t591 {
                        var t594 string = value__33.input
                        var t595 *ref_int_x = value__33.index
                        var t596 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t595)
                        var t597 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t594, t596)
                        var t598 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t597, 101)
                        var jp593 bool
                        if t598 {
                            jp593 = true
                        } else {
                            var t599 string = value__33.input
                            var t600 *ref_int_x = value__33.index
                            var t601 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t600)
                            var t602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t599, t601)
                            var t603 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t602, 69)
                            jp593 = t603
                        }
                        jp552 = jp593
                    } else {
                        jp552 = false
                    }
                    if jp552 {
                        var t553 *ref_int_x = value__33.index
                        var t554 *ref_int_x = value__33.index
                        var t555 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t554)
                        var t556 int = t555 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t553, t556)
                        var t570 *ref_int_x = value__33.index
                        var t571 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t570)
                        var t572 string = value__33.input
                        var t573 int = _goml_m_inherent_i_string_i_string_i_byte__len(t572)
                        var t574 bool = t571 < t573
                        var jp564 bool
                        if t574 {
                            var t577 string = value__33.input
                            var t578 *ref_int_x = value__33.index
                            var t579 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t578)
                            var t580 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t577, t579)
                            var t581 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t580, 43)
                            var jp576 bool
                            if t581 {
                                jp576 = true
                            } else {
                                var t582 string = value__33.input
                                var t583 *ref_int_x = value__33.index
                                var t584 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t583)
                                var t585 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t582, t584)
                                var t586 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t585, 45)
                                jp576 = t586
                            }
                            jp564 = jp576
                        } else {
                            jp564 = false
                        }
                        if jp564 {
                            var t565 *ref_int_x = value__33.index
                            var t566 *ref_int_x = value__33.index
                            var t567 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t566)
                            var t568 int = t567 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t565, t568)
                        } else {}
                        var t559 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t560 bool = !t559
                        if t560 {
                            var t561 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t562 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t561,
                            }
                            retv539 = t562
                            return retv539
                        } else {
                            var t545 string = value__33.input
                            var t546 *ref_int_x = value__33.index
                            var t547 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t546)
                            var t548 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t545, start__34, t547)
                            var t549 _goml_m_std_p_json_p_Value = Number{
                                _0: t548,
                            }
                            var t550 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t549,
                            }
                            retv539 = t550
                            return retv539
                        }
                    } else {
                        var t545 string = value__33.input
                        var t546 *ref_int_x = value__33.index
                        var t547 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t546)
                        var t548 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t545, start__34, t547)
                        var t549 _goml_m_std_p_json_p_Value = Number{
                            _0: t548,
                        }
                        var t550 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t549,
                        }
                        retv539 = t550
                        return retv539
                    }
                }
            }
        } else {
            var t658 bool = _goml_m_std_p_json_p_parse__digits(value__33)
            var t659 bool = !t658
            if t659 {
                var t660 string = _goml_m_std_p_json_p_json__error(value__33, "expected number")
                var t661 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t660,
                }
                retv539 = t661
                return retv539
            } else {
                var t615 *ref_int_x = value__33.index
                var t616 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t615)
                var t617 string = value__33.input
                var t618 int = _goml_m_inherent_i_string_i_string_i_byte__len(t617)
                var t619 bool = t616 < t618
                var jp605 bool
                if t619 {
                    var t620 string = value__33.input
                    var t621 *ref_int_x = value__33.index
                    var t622 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t621)
                    var t623 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t620, t622)
                    var t624 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t623, 46)
                    jp605 = t624
                } else {
                    jp605 = false
                }
                if jp605 {
                    var t606 *ref_int_x = value__33.index
                    var t607 *ref_int_x = value__33.index
                    var t608 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t607)
                    var t609 int = t608 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t606, t609)
                    var t611 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t612 bool = !t611
                    if t612 {
                        var t613 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t614 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t613,
                        }
                        retv539 = t614
                        return retv539
                    } else {
                        var t587 *ref_int_x = value__33.index
                        var t588 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t587)
                        var t589 string = value__33.input
                        var t590 int = _goml_m_inherent_i_string_i_string_i_byte__len(t589)
                        var t591 bool = t588 < t590
                        var jp552 bool
                        if t591 {
                            var t594 string = value__33.input
                            var t595 *ref_int_x = value__33.index
                            var t596 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t595)
                            var t597 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t594, t596)
                            var t598 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t597, 101)
                            var jp593 bool
                            if t598 {
                                jp593 = true
                            } else {
                                var t599 string = value__33.input
                                var t600 *ref_int_x = value__33.index
                                var t601 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t600)
                                var t602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t599, t601)
                                var t603 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t602, 69)
                                jp593 = t603
                            }
                            jp552 = jp593
                        } else {
                            jp552 = false
                        }
                        if jp552 {
                            var t553 *ref_int_x = value__33.index
                            var t554 *ref_int_x = value__33.index
                            var t555 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t554)
                            var t556 int = t555 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t553, t556)
                            var t570 *ref_int_x = value__33.index
                            var t571 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t570)
                            var t572 string = value__33.input
                            var t573 int = _goml_m_inherent_i_string_i_string_i_byte__len(t572)
                            var t574 bool = t571 < t573
                            var jp564 bool
                            if t574 {
                                var t577 string = value__33.input
                                var t578 *ref_int_x = value__33.index
                                var t579 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t578)
                                var t580 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t577, t579)
                                var t581 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t580, 43)
                                var jp576 bool
                                if t581 {
                                    jp576 = true
                                } else {
                                    var t582 string = value__33.input
                                    var t583 *ref_int_x = value__33.index
                                    var t584 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t583)
                                    var t585 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t582, t584)
                                    var t586 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t585, 45)
                                    jp576 = t586
                                }
                                jp564 = jp576
                            } else {
                                jp564 = false
                            }
                            if jp564 {
                                var t565 *ref_int_x = value__33.index
                                var t566 *ref_int_x = value__33.index
                                var t567 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t566)
                                var t568 int = t567 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t565, t568)
                            } else {}
                            var t559 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t560 bool = !t559
                            if t560 {
                                var t561 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t562 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t561,
                                }
                                retv539 = t562
                                return retv539
                            } else {
                                var t545 string = value__33.input
                                var t546 *ref_int_x = value__33.index
                                var t547 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t546)
                                var t548 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t545, start__34, t547)
                                var t549 _goml_m_std_p_json_p_Value = Number{
                                    _0: t548,
                                }
                                var t550 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t549,
                                }
                                retv539 = t550
                                return retv539
                            }
                        } else {
                            var t545 string = value__33.input
                            var t546 *ref_int_x = value__33.index
                            var t547 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t546)
                            var t548 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t545, start__34, t547)
                            var t549 _goml_m_std_p_json_p_Value = Number{
                                _0: t548,
                            }
                            var t550 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t549,
                            }
                            retv539 = t550
                            return retv539
                        }
                    }
                } else {
                    var t587 *ref_int_x = value__33.index
                    var t588 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t587)
                    var t589 string = value__33.input
                    var t590 int = _goml_m_inherent_i_string_i_string_i_byte__len(t589)
                    var t591 bool = t588 < t590
                    var jp552 bool
                    if t591 {
                        var t594 string = value__33.input
                        var t595 *ref_int_x = value__33.index
                        var t596 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t595)
                        var t597 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t594, t596)
                        var t598 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t597, 101)
                        var jp593 bool
                        if t598 {
                            jp593 = true
                        } else {
                            var t599 string = value__33.input
                            var t600 *ref_int_x = value__33.index
                            var t601 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t600)
                            var t602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t599, t601)
                            var t603 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t602, 69)
                            jp593 = t603
                        }
                        jp552 = jp593
                    } else {
                        jp552 = false
                    }
                    if jp552 {
                        var t553 *ref_int_x = value__33.index
                        var t554 *ref_int_x = value__33.index
                        var t555 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t554)
                        var t556 int = t555 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t553, t556)
                        var t570 *ref_int_x = value__33.index
                        var t571 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t570)
                        var t572 string = value__33.input
                        var t573 int = _goml_m_inherent_i_string_i_string_i_byte__len(t572)
                        var t574 bool = t571 < t573
                        var jp564 bool
                        if t574 {
                            var t577 string = value__33.input
                            var t578 *ref_int_x = value__33.index
                            var t579 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t578)
                            var t580 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t577, t579)
                            var t581 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t580, 43)
                            var jp576 bool
                            if t581 {
                                jp576 = true
                            } else {
                                var t582 string = value__33.input
                                var t583 *ref_int_x = value__33.index
                                var t584 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t583)
                                var t585 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t582, t584)
                                var t586 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t585, 45)
                                jp576 = t586
                            }
                            jp564 = jp576
                        } else {
                            jp564 = false
                        }
                        if jp564 {
                            var t565 *ref_int_x = value__33.index
                            var t566 *ref_int_x = value__33.index
                            var t567 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t566)
                            var t568 int = t567 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t565, t568)
                        } else {}
                        var t559 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t560 bool = !t559
                        if t560 {
                            var t561 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t562 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t561,
                            }
                            retv539 = t562
                            return retv539
                        } else {
                            var t545 string = value__33.input
                            var t546 *ref_int_x = value__33.index
                            var t547 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t546)
                            var t548 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t545, start__34, t547)
                            var t549 _goml_m_std_p_json_p_Value = Number{
                                _0: t548,
                            }
                            var t550 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t549,
                            }
                            retv539 = t550
                            return retv539
                        }
                    } else {
                        var t545 string = value__33.input
                        var t546 *ref_int_x = value__33.index
                        var t547 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t546)
                        var t548 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t545, start__34, t547)
                        var t549 _goml_m_std_p_json_p_Value = Number{
                            _0: t548,
                        }
                        var t550 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t549,
                        }
                        retv539 = t550
                        return retv539
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__35 _goml_m_std_p_json_p_JsonParser, expected__36 string, result__37 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var retv673 _goml_m_Result____std_p_json_p_Value____string
    var t686 *ref_int_x = value__35.index
    var t687 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t686)
    var t688 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
    var t689 int = t687 + t688
    var t690 string = value__35.input
    var t691 int = _goml_m_inherent_i_string_i_string_i_byte__len(t690)
    var t692 bool = t689 <= t691
    var jp677 bool
    if t692 {
        var t693 string = value__35.input
        var t694 *ref_int_x = value__35.index
        var t695 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t694)
        var t696 *ref_int_x = value__35.index
        var t697 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t696)
        var t698 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t699 int = t697 + t698
        var t700 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t693, t695, t699)
        var t701 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t700, expected__36)
        jp677 = t701
    } else {
        jp677 = false
    }
    var jp675 _goml_m_Result____std_p_json_p_Value____string
    if jp677 {
        var t678 *ref_int_x = value__35.index
        var t679 *ref_int_x = value__35.index
        var t680 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t679)
        var t681 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t682 int = t680 + t681
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t678, t682)
        var t683 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__37,
        }
        jp675 = t683
    } else {
        var t684 string = _goml_m_std_p_json_p_json__error(value__35, "invalid literal")
        var t685 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t684,
        }
        jp675 = t685
    }
    retv673 = jp675
    return retv673
}

func _goml_m_std_p_json_p_parse__json__array(value__38 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv703 _goml_m_Result____std_p_json_p_Value____string
    var t704 *ref_int_x = value__38.index
    var t705 *ref_int_x = value__38.index
    var t706 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t705)
    var t707 int = t706 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t704, t707)
    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
    var result__39 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t762 *ref_int_x = value__38.index
    var t763 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t762)
    var t764 string = value__38.input
    var t765 int = _goml_m_inherent_i_string_i_string_i_byte__len(t764)
    var t766 bool = t763 < t765
    var jp755 bool
    if t766 {
        var t767 string = value__38.input
        var t768 *ref_int_x = value__38.index
        var t769 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t768)
        var t770 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t767, t769)
        var t771 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t770, 93)
        jp755 = t771
    } else {
        jp755 = false
    }
    if jp755 {
        var t756 *ref_int_x = value__38.index
        var t757 *ref_int_x = value__38.index
        var t758 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t757)
        var t759 int = t758 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t756, t759)
        var t760 _goml_m_std_p_json_p_Value = Array{
            _0: result__39,
        }
        var t761 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t760,
        }
        retv703 = t761
        return retv703
    } else {
        Loop_loop712:
        for {
            var t713 *ref_int_x = value__38.index
            var t714 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t713)
            var t715 string = value__38.input
            var t716 int = _goml_m_inherent_i_string_i_string_i_byte__len(t715)
            var t717 bool = t714 < t716
            if t717 {
                var mtmp48 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__38)
                switch mtmp48.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x49 _goml_m_std_p_json_p_Value = mtmp48.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    var item__40 _goml_m_std_p_json_p_Value = x49
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(result__39, item__40)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                    var t720 *ref_int_x = value__38.index
                    var t721 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t720)
                    var t722 string = value__38.input
                    var t723 int = _goml_m_inherent_i_string_i_string_i_byte__len(t722)
                    var t724 bool = t721 >= t723
                    if t724 {
                        var t725 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
                        var t726 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t725,
                        }
                        retv703 = t726
                        return retv703
                    } else {
                        var t728 string = value__38.input
                        var t729 *ref_int_x = value__38.index
                        var t730 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t729)
                        var t731 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t728, t730)
                        var t732 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t731, 93)
                        if t732 {
                            var t733 *ref_int_x = value__38.index
                            var t734 *ref_int_x = value__38.index
                            var t735 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t734)
                            var t736 int = t735 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t733, t736)
                            var t737 _goml_m_std_p_json_p_Value = Array{
                                _0: result__39,
                            }
                            var t738 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t737,
                            }
                            retv703 = t738
                            return retv703
                        } else {
                            var t740 string = value__38.input
                            var t741 *ref_int_x = value__38.index
                            var t742 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t741)
                            var t743 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t740, t742)
                            var t744 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t743, 44)
                            if t744 {
                                var t745 *ref_int_x = value__38.index
                                var t746 *ref_int_x = value__38.index
                                var t747 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t746)
                                var t748 int = t747 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t745, t748)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                                continue
                            } else {
                                var t750 string = _goml_m_std_p_json_p_json__error(value__38, "expected array separator")
                                var t751 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t750,
                                }
                                retv703 = t751
                                return retv703
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x50 string = mtmp48.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var error__41 string = x50
                    var t753 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__41,
                    }
                    retv703 = t753
                    return retv703
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop712
            }
        }
        var t710 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
        var t711 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t710,
        }
        retv703 = t711
        return retv703
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__42 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv773 _goml_m_Result____std_p_json_p_Value____string
    var t774 *ref_int_x = value__42.index
    var t775 *ref_int_x = value__42.index
    var t776 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t775)
    var t777 int = t776 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t774, t777)
    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
    var result__43 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t857 *ref_int_x = value__42.index
    var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
    var t859 string = value__42.input
    var t860 int = _goml_m_inherent_i_string_i_string_i_byte__len(t859)
    var t861 bool = t858 < t860
    var jp850 bool
    if t861 {
        var t862 string = value__42.input
        var t863 *ref_int_x = value__42.index
        var t864 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t863)
        var t865 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t862, t864)
        var t866 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t865, 125)
        jp850 = t866
    } else {
        jp850 = false
    }
    if jp850 {
        var t851 *ref_int_x = value__42.index
        var t852 *ref_int_x = value__42.index
        var t853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t852)
        var t854 int = t853 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t851, t854)
        var t855 _goml_m_std_p_json_p_Value = Object{
            _0: result__43,
        }
        var t856 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t855,
        }
        retv773 = t856
        return retv773
    } else {
        Loop_loop782:
        for {
            var t783 *ref_int_x = value__42.index
            var t784 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t783)
            var t785 string = value__42.input
            var t786 int = _goml_m_inherent_i_string_i_string_i_byte__len(t785)
            var t787 bool = t784 < t786
            if t787 {
                var mtmp60 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__42)
                var jp789 string
                switch mtmp60.(type) {
                case Result__string__string_Ok:
                    var x61 string = mtmp60.(Result__string__string_Ok)._0
                    var name__44 string = x61
                    jp789 = name__44
                    var name__46 string = jp789
                    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                    var t837 *ref_int_x = value__42.index
                    var t838 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t837)
                    var t839 string = value__42.input
                    var t840 int = _goml_m_inherent_i_string_i_string_i_byte__len(t839)
                    var t841 bool = t838 >= t840
                    var jp829 bool
                    if t841 {
                        jp829 = true
                    } else {
                        var t842 string = value__42.input
                        var t843 *ref_int_x = value__42.index
                        var t844 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t843)
                        var t845 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t842, t844)
                        var t846 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t845, 58)
                        var t847 bool = !t846
                        jp829 = t847
                    }
                    if jp829 {
                        var t830 string = _goml_m_std_p_json_p_json__error(value__42, "expected object colon")
                        var t831 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t830,
                        }
                        retv773 = t831
                        return retv773
                    } else {
                        var t832 *ref_int_x = value__42.index
                        var t833 *ref_int_x = value__42.index
                        var t834 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t833)
                        var t835 int = t834 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t832, t835)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                        var mtmp66 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__42)
                        switch mtmp66.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x67 _goml_m_std_p_json_p_Value = mtmp66.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            var item__47 _goml_m_std_p_json_p_Value = x67
                            var t825 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: name__46,
                                _1: item__47,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__43, t825)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                            var t793 *ref_int_x = value__42.index
                            var t794 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t793)
                            var t795 string = value__42.input
                            var t796 int = _goml_m_inherent_i_string_i_string_i_byte__len(t795)
                            var t797 bool = t794 >= t796
                            if t797 {
                                var t798 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
                                var t799 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t798,
                                }
                                retv773 = t799
                                return retv773
                            } else {
                                var t801 string = value__42.input
                                var t802 *ref_int_x = value__42.index
                                var t803 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t802)
                                var t804 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t801, t803)
                                var t805 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t804, 125)
                                if t805 {
                                    var t806 *ref_int_x = value__42.index
                                    var t807 *ref_int_x = value__42.index
                                    var t808 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t807)
                                    var t809 int = t808 + 1
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t806, t809)
                                    var t810 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__43,
                                    }
                                    var t811 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t810,
                                    }
                                    retv773 = t811
                                    return retv773
                                } else {
                                    var t813 string = value__42.input
                                    var t814 *ref_int_x = value__42.index
                                    var t815 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t814)
                                    var t816 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t813, t815)
                                    var t817 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t816, 44)
                                    if t817 {
                                        var t818 *ref_int_x = value__42.index
                                        var t819 *ref_int_x = value__42.index
                                        var t820 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t819)
                                        var t821 int = t820 + 1
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t818, t821)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                                        continue
                                    } else {
                                        var t823 string = _goml_m_std_p_json_p_json__error(value__42, "expected object separator")
                                        var t824 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t823,
                                        }
                                        retv773 = t824
                                        return retv773
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x68 string = mtmp66.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var error__48 string = x68
                            var t827 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: error__48,
                            }
                            retv773 = t827
                            return retv773
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x62 string = mtmp60.(Result__string__string_Err)._0
                    var error__45 string = x62
                    var t848 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__45,
                    }
                    retv773 = t848
                    return retv773
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop782
            }
        }
        var t780 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
        var t781 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t780,
        }
        retv773 = t781
        return retv773
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__49 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv868 _goml_m_Result____std_p_json_p_Value____string
    _goml_m_std_p_json_p_skip__json__whitespace(value__49)
    var t871 *ref_int_x = value__49.index
    var t872 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t871)
    var t873 string = value__49.input
    var t874 int = _goml_m_inherent_i_string_i_string_i_byte__len(t873)
    var t875 bool = t872 >= t874
    var jp870 _goml_m_Result____std_p_json_p_Value____string
    if t875 {
        var t876 string = _goml_m_std_p_json_p_json__error(value__49, "expected JSON value")
        var t877 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t876,
        }
        jp870 = t877
    } else {
        var t878 string = value__49.input
        var t879 *ref_int_x = value__49.index
        var t880 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t879)
        var mtmp75 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t878, t880)
        var jp882 _goml_m_Result____std_p_json_p_Value____string
        switch mtmp75 {
        case 123:
            var t883 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__49)
            jp882 = t883
        case 91:
            var t884 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__49)
            jp882 = t884
        case 34:
            var mtmp76 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__49)
            var jp886 _goml_m_Result____std_p_json_p_Value____string
            switch mtmp76.(type) {
            case Result__string__string_Ok:
                var x77 string = mtmp76.(Result__string__string_Ok)._0
                var text__50 string = x77
                var t887 _goml_m_std_p_json_p_Value = String{
                    _0: text__50,
                }
                var t888 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t887,
                }
                jp886 = t888
            case Result__string__string_Err:
                var x78 string = mtmp76.(Result__string__string_Err)._0
                var error__51 string = x78
                var t889 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: error__51,
                }
                jp886 = t889
            default:
                panic("non-exhaustive match")
            }
            jp882 = jp886
        case 116:
            var t890 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t891 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "true", t890)
            jp882 = t891
        case 102:
            var t892 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t893 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "false", t892)
            jp882 = t893
        case 110:
            var t894 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "null", Null{})
            jp882 = t894
        default:
            var byte__52 uint8 = mtmp75
            var t902 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 45)
            var jp898 bool
            if t902 {
                jp898 = true
            } else {
                var t903 bool = _goml_m_std_p_json_p_json__digit(byte__52)
                jp898 = t903
            }
            var jp896 _goml_m_Result____std_p_json_p_Value____string
            if jp898 {
                var t899 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__49)
                jp896 = t899
            } else {
                var t900 string = _goml_m_std_p_json_p_json__error(value__49, "unexpected JSON token")
                var t901 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t900,
                }
                jp896 = t901
            }
            jp882 = jp896
        }
        jp870 = jp882
    }
    retv868 = jp870
    return retv868
}

func _goml_m_std_p_json_p_parse(input__53 string) _goml_m_Result____std_p_json_p_Value____string {
    var retv905 _goml_m_Result____std_p_json_p_Value____string
    var parser__54 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_json__parser(input__53)
    var mtmp79 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__54)
    var jp907 _goml_m_std_p_json_p_Value
    switch mtmp79.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x80 _goml_m_std_p_json_p_Value = mtmp79.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__55 _goml_m_std_p_json_p_Value = x80
        jp907 = value__55
        var result__57 _goml_m_std_p_json_p_Value = jp907
        _goml_m_std_p_json_p_skip__json__whitespace(parser__54)
        var t910 *ref_int_x = parser__54.index
        var t911 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t910)
        var t912 int = _goml_m_inherent_i_string_i_string_i_byte__len(input__53)
        var t913 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t911, t912)
        var jp909 _goml_m_Result____std_p_json_p_Value____string
        if t913 {
            var t914 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: result__57,
            }
            jp909 = t914
        } else {
            var t915 string = _goml_m_std_p_json_p_json__error(parser__54, "trailing JSON data")
            var t916 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t915,
            }
            jp909 = t916
        }
        retv905 = jp909
        return retv905
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x81 string = mtmp79.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var error__56 string = x81
        var t917 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: error__56,
        }
        retv905 = t917
        return retv905
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_json__hex__digit(value__58 uint8) rune {
    var retv919 rune
    var t920 int = int(uint8(value__58))
    var t921 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", t920)
    retv919 = t921
    return retv919
}

func _goml_m_std_p_json_p_write__json__string(builder__59 _goml_m_std_p_text_p_StringBuilder, value__60 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, 34)
    var start__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var for_index84 int = 0
    var for_limit85 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    Loop_loop933:
    for {
        var t934 bool = for_index84 < for_limit85
        if t934 {
            var for_item86 int = for_index84
            var t935 int = for_index84 + 1
            for_index84 = t935
            var index__62 int = for_item86
            var byte__63 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__60, index__62)
            var t991 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
            var jp989 bool
            if t991 {
                jp989 = true
            } else {
                var t992 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                jp989 = t992
            }
            var jp986 bool
            if jp989 {
                jp986 = true
            } else {
                var t990 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                jp986 = t990
            }
            var jp983 bool
            if jp986 {
                jp983 = true
            } else {
                var t987 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                jp983 = t987
            }
            var jp980 bool
            if jp983 {
                jp980 = true
            } else {
                var t984 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                jp980 = t984
            }
            var jp977 bool
            if jp980 {
                jp977 = true
            } else {
                var t981 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                jp977 = t981
            }
            var jp974 bool
            if jp977 {
                jp974 = true
            } else {
                var t978 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                jp974 = t978
            }
            var jp938 bool
            if jp974 {
                jp938 = true
            } else {
                var t975 bool = byte__63 < 32
                jp938 = t975
            }
            if jp938 {
                var t968 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                var t969 bool = t968 < index__62
                if t969 {
                    var t970 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                    var t971 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t970, index__62)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t971)
                } else {}
                var t943 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
                if t943 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\"")
                } else {
                    var t946 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                    if t946 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\\")
                    } else {
                        var t949 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                        if t949 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\b")
                        } else {
                            var t952 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                            if t952 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\t")
                            } else {
                                var t955 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                                if t955 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\n")
                                } else {
                                    var t958 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                                    if t958 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\f")
                                    } else {
                                        var t961 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                                        if t961 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\u00")
                                            var t963 uint8 = byte__63 / 16
                                            var t964 rune = _goml_m_std_p_json_p_json__hex__digit(t963)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t964)
                                            var t965_rhs uint8 = 16
                                            var t965 uint8 = byte__63 % t965_rhs
                                            var t966 rune = _goml_m_std_p_json_p_json__hex__digit(t965)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t966)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t941 int = index__62 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(start__61, t941)
            } else {}
            continue
        } else {
            break Loop_loop933
        }
    }
    var t926 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
    var t927 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    var t928 bool = t926 < t927
    if t928 {
        var t929 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
        var t930 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
        var t931 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t929, t930)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t931)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__64 _goml_m_std_p_text_p_StringBuilder, value__65 _goml_m_std_p_json_p_Value) struct{} {
    switch value__65.(type) {
    case Object:
        var x94 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__65.(Object)._0
        var fields__66 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x94
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 123)
        var index__67 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var for_source100 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = fields__66
        var for_limit101 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source100)
        var for_index102 int = 0
        Loop_loop997:
        for {
            var t998 bool = for_index102 < for_limit101
            if t998 {
                var for_item103 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source100, for_index102)
                var t999 int = for_index102 + 1
                for_index102 = t999
                var field__68 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item103
                var t1006 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                var t1007 bool = t1006 > 0
                if t1007 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                } else {}
                var t1001 string = field__68._0
                _goml_m_std_p_json_p_write__json__string(builder__64, t1001)
                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 58)
                var t1002 _goml_m_std_p_json_p_Value = field__68._1
                _goml_m_std_p_json_p_write__json__value(builder__64, t1002)
                var t1003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                var t1004 int = t1003 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__67, t1004)
                continue
            } else {
                break Loop_loop997
            }
        }
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 125)
        return struct{}{}
    case Array:
        var x95 *_goml_vec__goml_m_std_p_json_p_Value = value__65.(Array)._0
        var items__69 *_goml_vec__goml_m_std_p_json_p_Value = x95
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 91)
        var index__70 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var for_source111 *_goml_vec__goml_m_std_p_json_p_Value = items__69
        var for_limit112 int = vec_len___goml_m_Vec__16std_p_json_p_Value(for_source111)
        var for_index113 int = 0
        Loop_loop1011:
        for {
            var t1012 bool = for_index113 < for_limit112
            if t1012 {
                var for_item114 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(for_source111, for_index113)
                var t1013 int = for_index113 + 1
                for_index113 = t1013
                var item__71 _goml_m_std_p_json_p_Value = for_item114
                var t1018 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__70)
                var t1019 bool = t1018 > 0
                if t1019 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__64, item__71)
                var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__70)
                var t1016 int = t1015 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__70, t1016)
                continue
            } else {
                break Loop_loop1011
            }
        }
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 93)
        return struct{}{}
    case String:
        var x96 string = value__65.(String)._0
        var text__72 string = x96
        _goml_m_std_p_json_p_write__json__string(builder__64, text__72)
        return struct{}{}
    case Number:
        var x97 string = value__65.(Number)._0
        var number__73 string = x97
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, number__73)
        return struct{}{}
    case Bool:
        var x98 bool = value__65.(Bool)._0
        var value__74 bool = x98
        var jp1024 string
        if value__74 {
            jp1024 = "true"
        } else {
            jp1024 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, jp1024)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__75 _goml_m_std_p_json_p_Value) string {
    var retv1028 string
    var builder__76 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
    _goml_m_std_p_json_p_write__json__value(builder__76, value__75)
    var t1029 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__76)
    retv1028 = t1029
    return retv1028
}

func _goml_m_std_p_json_p_field(value__77 _goml_m_std_p_json_p_Value, name__78 string) _goml_m_Option____std_p_json_p_Value {
    var retv1031 _goml_m_Option____std_p_json_p_Value
    var jp1033 _goml_m_Option____std_p_json_p_Value
    switch value__77.(type) {
    case Object:
        var x120 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__77.(Object)._0
        var fields__79 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x120
        var result__80 *ref__goml_m_Option____std_p_json_p_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(_goml_m_Option____std_p_json_p_Value_None{})
        var for_source125 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = fields__79
        var for_limit126 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source125)
        var for_index127 int = 0
        Loop_loop1036:
        for {
            var t1037 bool = for_index127 < for_limit126
            if t1037 {
                var for_item128 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source125, for_index127)
                var t1038 int = for_index127 + 1
                for_index127 = t1038
                var field__81 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item128
                var t1040 string = field__81._0
                var t1041 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1040, name__78)
                if t1041 {
                    var t1042 _goml_m_std_p_json_p_Value = field__81._1
                    var t1043 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1042,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(result__80, t1043)
                } else {}
                continue
            } else {
                break Loop_loop1036
            }
        }
        var t1035 _goml_m_Option____std_p_json_p_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(result__80)
        jp1033 = t1035
        retv1031 = jp1033
        return retv1031
    default:
        jp1033 = _goml_m_Option____std_p_json_p_Value_None{}
        retv1031 = jp1033
        return retv1031
    }
}

func _goml_m_std_p_json_p_as__string(value__82 _goml_m_std_p_json_p_Value) Option__string {
    var retv1046 Option__string
    var jp1048 Option__string
    switch value__82.(type) {
    case String:
        var x133 string = value__82.(String)._0
        var text__83 string = x133
        var t1049 Option__string = Option__string_Some{
            _0: text__83,
        }
        jp1048 = t1049
    default:
        jp1048 = Option__string_None{}
    }
    retv1046 = jp1048
    return retv1046
}

func _goml_m_std_p_json_p_parse__json__int__text(value__84 string) Option__int {
    var retv1051 Option__int
    var t1054 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
    var t1055 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1054, 0)
    var jp1053 Option__int
    if t1055 {
        jp1053 = Option__int_None{}
        retv1051 = jp1053
        return retv1051
    } else {
        var t1056 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, 0)
        var negative__85 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1056, 45)
        var jp1058 int
        if negative__85 {
            jp1058 = 1
        } else {
            jp1058 = 0
        }
        var index__86 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(jp1058)
        var result__87 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var t1086 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
        var t1087 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1088 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1086, t1087)
        if t1088 {
            retv1051 = Option__int_None{}
            return retv1051
        } else {
            Loop_loop1067:
            for {
                var t1068 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                var t1069 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                var t1070 bool = t1068 < t1069
                if t1070 {
                    var t1071 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                    var byte__88 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1071)
                    var t1084 bool = byte__88 < 48
                    var jp1077 bool
                    if t1084 {
                        jp1077 = true
                    } else {
                        var t1085 bool = byte__88 > 57
                        jp1077 = t1085
                    }
                    if jp1077 {
                        retv1051 = Option__int_None{}
                        return retv1051
                    } else {
                        var t1078 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                        var t1079 int = t1078 * 10
                        var t1080 uint8 = byte__88 - 48
                        var t1081 int = int(uint8(t1080))
                        var t1082 int = t1079 + t1081
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__87, t1082)
                        var t1073 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                        var t1074 int = t1073 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__86, t1074)
                        continue
                    }
                } else {
                    break Loop_loop1067
                }
            }
            var jp1062 int
            if negative__85 {
                var t1064 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                var t1065 int = 0 - t1064
                jp1062 = t1065
            } else {
                var t1066 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                jp1062 = t1066
            }
            var t1063 Option__int = Option__int_Some{
                _0: jp1062,
            }
            jp1053 = t1063
            retv1051 = jp1053
            return retv1051
        }
    }
}

func _goml_m_std_p_json_p_as__int(value__89 _goml_m_std_p_json_p_Value) Option__int {
    var retv1090 Option__int
    var jp1092 Option__int
    switch value__89.(type) {
    case Number:
        var x142 string = value__89.(Number)._0
        var number__90 string = x142
        var t1093 Option__int = _goml_m_std_p_json_p_parse__json__int__text(number__90)
        jp1092 = t1093
    default:
        jp1092 = Option__int_None{}
    }
    retv1090 = jp1092
    return retv1090
}

func _goml_m_std_p_json_p_as__bool(value__91 _goml_m_std_p_json_p_Value) Option__bool {
    var retv1095 Option__bool
    var jp1097 Option__bool
    switch value__91.(type) {
    case Bool:
        var x148 bool = value__91.(Bool)._0
        var result__92 bool = x148
        var t1098 Option__bool = Option__bool_Some{
            _0: result__92,
        }
        jp1097 = t1098
    default:
        jp1097 = Option__bool_None{}
    }
    retv1095 = jp1097
    return retv1095
}

func main0() struct{} {
    var mtmp64 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1109 _goml_m_std_p_json_p_Value
    switch mtmp64.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x65 _goml_m_std_p_json_p_Value = mtmp64.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__0 _goml_m_std_p_json_p_Value = x65
        jp1109 = value__0
        var value__2 _goml_m_std_p_json_p_Value = jp1109
        var mtmp68 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "name")
        switch mtmp68.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing name")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__3 _goml_m_std_p_json_p_Value = x69
            var mtmp70 Option__string = _goml_m_std_p_json_p_as__string(field__3)
            switch mtmp70.(type) {
            case Option__string_None:
                println__T_string("invalid name")
            case Option__string_Some:
                var x71 string = mtmp70.(Option__string_Some)._0
                var name__4 string = x71
                println__T_string(name__4)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var mtmp73 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "version")
        switch mtmp73.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing version")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x74 _goml_m_std_p_json_p_Value = mtmp73.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__5 _goml_m_std_p_json_p_Value = x74
            var mtmp75 Option__int = _goml_m_std_p_json_p_as__int(field__5)
            switch mtmp75.(type) {
            case Option__int_None:
                println__T_string("invalid version")
            case Option__int_Some:
                var x76 int = mtmp75.(Option__int_Some)._0
                var version__6 int = x76
                println__T_int(version__6)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var mtmp78 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "stable")
        switch mtmp78.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing stable")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x79 _goml_m_std_p_json_p_Value = mtmp78.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__7 _goml_m_std_p_json_p_Value = x79
            var mtmp80 Option__bool = _goml_m_std_p_json_p_as__bool(field__7)
            switch mtmp80.(type) {
            case Option__bool_None:
                println__T_string("invalid stable")
            case Option__bool_Some:
                var x81 bool = mtmp80.(Option__bool_Some)._0
                var stable__8 bool = x81
                println__T_bool(stable__8)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var t1113 string = _goml_m_std_p_json_p_encode(value__2)
        println__T_string(t1113)
        return struct{}{}
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x66 string = mtmp64.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var error__1 string = x66
        println__T_string(error__1)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv1128 *_goml_vec_uint8
    var t1129 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv1128 = t1129
    return retv1128
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__128 *_goml_vec_uint8, elem__129 uint8) struct{} {
    vec_push__Vec_5uint8(self__128, elem__129)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv1171 bool
    var t1172 bool = self__59 == other__60
    retv1171 = t1172
    return retv1171
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv1174 int
    var t1175 int = _goml_runtime_core_string_len(self__9)
    retv1174 = t1175
    return retv1174
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv1177 uint8
    var t1178 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv1177 = t1178
    return retv1177
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv1180 string
    var t1181 string = _goml_runtime_core_char_to_string(self__7)
    retv1180 = t1181
    return retv1180
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv1183 *ref_int_x
    var t1184 *ref_int_x = ref__Ref_3int(value__209)
    retv1183 = t1184
    return retv1183
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv1186 int
    var t1187 int = ref_get__Ref_3int(self__210)
    retv1186 = t1187
    return retv1186
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv1189 string
    var t1190 string = _goml_runtime_core_int_to_string(self__5)
    retv1189 = t1190
    return retv1189
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv1192 bool
    var t1193 bool = self__69 == other__70
    retv1192 = t1193
    return retv1192
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__209 uint32) *ref_uint32_x {
    var retv1197 *ref_uint32_x
    var t1198 *ref_uint32_x = ref__Ref_6uint32(value__209)
    retv1197 = t1198
    return retv1197
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__210 *ref_uint32_x) uint32 {
    var retv1200 uint32
    var t1201 uint32 = ref_get__Ref_6uint32(self__210)
    retv1200 = t1201
    return retv1200
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(self__211 *ref_uint32_x, value__212 uint32) struct{} {
    ref_set__Ref_6uint32(self__211, value__212)
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv1205 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp1207 Option__char
    if valid__3 {
        var t1208 Option__char = Option__char_Some{
            _0: value__4,
        }
        jp1207 = t1208
    } else {
        jp1207 = Option__char_None{}
    }
    retv1205 = jp1207
    return retv1205
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv1210 string
    var t1211 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv1210 = t1211
    return retv1210
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv1213 bool
    var t1214 bool = self__55 == other__56
    retv1213 = t1214
    return retv1213
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var retv1216 *_goml_vec__goml_m_std_p_json_p_Value
    var t1217 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    retv1216 = t1217
    return retv1216
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__128 *_goml_vec__goml_m_std_p_json_p_Value, elem__129 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var retv1221 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
    var t1222 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    retv1221 = t1222
    return retv1221
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__128 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__129 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv1226 rune
    var t1227 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv1226 = t1227
    return retv1226
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(value__209 _goml_m_Option____std_p_json_p_Value) *ref__goml_m_Option____std_p_json_p_Value_x {
    var retv1229 *ref__goml_m_Option____std_p_json_p_Value_x
    var t1230 *ref__goml_m_Option____std_p_json_p_Value_x = ref___goml_m_Ref__24Option____std_p_json_p_Value(value__209)
    retv1229 = t1230
    return retv1229
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(self__211 *ref__goml_m_Option____std_p_json_p_Value_x, value__212 _goml_m_Option____std_p_json_p_Value) struct{} {
    ref_set___goml_m_Ref__24Option____std_p_json_p_Value(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(self__210 *ref__goml_m_Option____std_p_json_p_Value_x) _goml_m_Option____std_p_json_p_Value {
    var retv1234 _goml_m_Option____std_p_json_p_Value
    var t1235 _goml_m_Option____std_p_json_p_Value = ref_get___goml_m_Ref__24Option____std_p_json_p_Value(self__210)
    retv1234 = t1235
    return retv1234
}

func println__T_string(value__1 string) struct{} {
    var t1237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t1237)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t1240 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t1240)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t1243 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t1243)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv1246 string
    retv1246 = self__38
    return retv1246
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv1248 string
    var t1249 string = _goml_runtime_core_int_to_string(self__40)
    retv1248 = t1249
    return retv1248
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv1251 string
    var t1252 string = _goml_runtime_core_bool_to_string(self__37)
    retv1251 = t1252
    return retv1251
}

func main() {
    main0()
}
