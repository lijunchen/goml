package main

import (
    _goml_fmt "fmt"
    _goml_strings "strings"
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

func _goml_runtime_core_string_concat(values *_goml_vec_string) string {
    return _goml_strings.Join(values.items, "")
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
    parts *_goml_vec_string
    byte_len *ref_int_x
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
    var t149 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var t150 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t151 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        parts: t149,
        byte_len: t150,
    }
    retv148 = t151
    return retv148
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var t167 *_goml_vec_string = self__3.parts
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(t167, value__4)
    var t168 *ref_int_x = self__3.byte_len
    var t169 *ref_int_x = self__3.byte_len
    var t170 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t169)
    var t171 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    var t172 int = t170 + t171
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t168, t172)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__5 _goml_m_std_p_text_p_StringBuilder, value__6 rune) struct{} {
    var t175 string = _goml_m_inherent_i_char_i_char_i_to__string(value__6)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__5, t175)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(self__14 _goml_m_std_p_text_p_StringBuilder) string {
    var retv190 string
    var t191 *_goml_vec_string = self__14.parts
    var t192 string = _goml_runtime_core_string_concat(t191)
    retv190 = t192
    return retv190
}

func _goml_m_std_p_json_p_json__parser(input__0 string) _goml_m_std_p_json_p_JsonParser {
    var retv197 _goml_m_std_p_json_p_JsonParser
    var t198 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t199 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__0,
        index: t198,
    }
    retv197 = t199
    return retv197
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var retv201 string
    var t202 string = message__2 + " at byte "
    var t203 *ref_int_x = value__1.index
    var t204 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t203)
    var t205 string = _goml_m_inherent_i_int_i_int_i_to__string(t204)
    var t206 string = t202 + t205
    retv201 = t206
    return retv201
}

func _goml_m_std_p_json_p_json__whitespace(value__3 uint8) bool {
    var retv208 bool
    var t217 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 9)
    var jp215 bool
    if t217 {
        jp215 = true
    } else {
        var t218 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 10)
        jp215 = t218
    }
    var jp212 bool
    if jp215 {
        jp212 = true
    } else {
        var t216 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 13)
        jp212 = t216
    }
    var jp210 bool
    if jp212 {
        jp210 = true
    } else {
        var t213 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 32)
        jp210 = t213
    }
    retv208 = jp210
    return retv208
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop221:
    for {
        var t229 *ref_int_x = value__4.index
        var t230 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t229)
        var t231 string = value__4.input
        var t232 int = _goml_m_inherent_i_string_i_string_i_byte__len(t231)
        var t233 bool = t230 < t232
        var jp223 bool
        if t233 {
            var t234 string = value__4.input
            var t235 *ref_int_x = value__4.index
            var t236 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t235)
            var t237 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t234, t236)
            var t238 bool = _goml_m_std_p_json_p_json__whitespace(t237)
            jp223 = t238
        } else {
            jp223 = false
        }
        if jp223 {
            var t224 *ref_int_x = value__4.index
            var t225 *ref_int_x = value__4.index
            var t226 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t225)
            var t227 int = t226 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t224, t227)
            continue
        } else {
            break Loop_loop221
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var retv240 Option__uint32
    var t266 bool = value__5 >= 48
    var jp244 bool
    if t266 {
        var t267 bool = value__5 <= 57
        jp244 = t267
    } else {
        jp244 = false
    }
    var jp242 Option__uint32
    if jp244 {
        var t245 uint8 = value__5 - 48
        var t246 uint32 = uint32(uint8(t245))
        var t247 Option__uint32 = Option__uint32_Some{
            _0: t246,
        }
        jp242 = t247
    } else {
        var t264 bool = value__5 >= 65
        var jp251 bool
        if t264 {
            var t265 bool = value__5 <= 70
            jp251 = t265
        } else {
            jp251 = false
        }
        var jp249 Option__uint32
        if jp251 {
            var t252 uint8 = value__5 - 55
            var t253 uint32 = uint32(uint8(t252))
            var t254 Option__uint32 = Option__uint32_Some{
                _0: t253,
            }
            jp249 = t254
        } else {
            var t262 bool = value__5 >= 97
            var jp258 bool
            if t262 {
                var t263 bool = value__5 <= 102
                jp258 = t263
            } else {
                jp258 = false
            }
            var jp256 Option__uint32
            if jp258 {
                var t259 uint8 = value__5 - 87
                var t260 uint32 = uint32(uint8(t259))
                var t261 Option__uint32 = Option__uint32_Some{
                    _0: t260,
                }
                jp256 = t261
            } else {
                jp256 = Option__uint32_None{}
            }
            jp249 = jp256
        }
        jp242 = jp249
    }
    retv240 = jp242
    return retv240
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var retv269 Result__uint32__string
    var t272 *ref_int_x = value__6.index
    var t273 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t272)
    var t274 int = t273 + 4
    var t275 string = value__6.input
    var t276 int = _goml_m_inherent_i_string_i_string_i_byte__len(t275)
    var t277 bool = t274 > t276
    var jp271 Result__uint32__string
    if t277 {
        var t278 string = _goml_m_std_p_json_p_json__error(value__6, "incomplete unicode escape")
        var t279 Result__uint32__string = Result__uint32__string_Err{
            _0: t278,
        }
        jp271 = t279
        retv269 = jp271
        return retv269
    } else {
        var t280_source int = 0
        var t280 uint32 = uint32(int(t280_source))
        var result__7 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(t280)
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop288:
        for {
            var t289 bool = for_index0 < for_limit1
            if t289 {
                var for_item2 int = for_index0
                var t290 int = for_index0 + 1
                for_index0 = t290
                var offset__8 int = for_item2
                var t291 string = value__6.input
                var t292 *ref_int_x = value__6.index
                var t293 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t292)
                var t294 int = t293 + offset__8
                var t295 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t291, t294)
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t295)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t297 string = _goml_m_std_p_json_p_json__error(value__6, "invalid unicode escape")
                    var t298 Result__uint32__string = Result__uint32__string_Err{
                        _0: t297,
                    }
                    retv269 = t298
                    return retv269
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var digit__9 uint32 = x5
                    var t299 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
                    var t300 uint32 = t299 * 16
                    var t301 uint32 = t300 + digit__9
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(result__7, t301)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop288
            }
        }
        var t282 *ref_int_x = value__6.index
        var t283 *ref_int_x = value__6.index
        var t284 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t283)
        var t285 int = t284 + 4
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t282, t285)
        var t286 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
        var t287 Result__uint32__string = Result__uint32__string_Ok{
            _0: t286,
        }
        jp271 = t287
        retv269 = jp271
        return retv269
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var retv304 Result__unit__string
    var mtmp8 Option__char = char_from_uint32(codepoint__12)
    var jp306 Result__unit__string
    switch mtmp8.(type) {
    case Option__char_None:
        var t307 string = _goml_m_std_p_json_p_json__error(value__10, "invalid unicode codepoint")
        var t308 Result__unit__string = Result__unit__string_Err{
            _0: t307,
        }
        jp306 = t308
    case Option__char_Some:
        var x9 rune = mtmp8.(Option__char_Some)._0
        var character__13 rune = x9
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__11, character__13)
        var t309 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp306 = t309
    default:
        panic("non-exhaustive match")
    }
    retv304 = jp306
    return retv304
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var retv311 Result__unit__string
    var mtmp11 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp313 uint32
    switch mtmp11.(type) {
    case Result__uint32__string_Ok:
        var x12 uint32 = mtmp11.(Result__uint32__string_Ok)._0
        var codepoint__16 uint32 = x12
        jp313 = codepoint__16
        var first__18 uint32 = jp313
        var t375 bool = first__18 >= 55296
        var jp317 bool
        if t375 {
            var t376 bool = first__18 <= 56319
            jp317 = t376
        } else {
            jp317 = false
        }
        var jp315 Result__unit__string
        if jp317 {
            var t354 *ref_int_x = value__14.index
            var t355 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t354)
            var t356 int = t355 + 2
            var t357 string = value__14.input
            var t358 int = _goml_m_inherent_i_string_i_string_i_byte__len(t357)
            var t359 bool = t356 > t358
            var jp346 bool
            if t359 {
                jp346 = true
            } else {
                var t360 string = value__14.input
                var t361 *ref_int_x = value__14.index
                var t362 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t361)
                var t363 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t360, t362)
                var t364 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t363, 92)
                var t365 bool = !t364
                jp346 = t365
            }
            var jp321 bool
            if jp346 {
                jp321 = true
            } else {
                var t347 string = value__14.input
                var t348 *ref_int_x = value__14.index
                var t349 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t348)
                var t350 int = t349 + 1
                var t351 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t347, t350)
                var t352 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t351, 117)
                var t353 bool = !t352
                jp321 = t353
            }
            var jp319 Result__unit__string
            if jp321 {
                var t322 string = _goml_m_std_p_json_p_json__error(value__14, "missing low surrogate")
                var t323 Result__unit__string = Result__unit__string_Err{
                    _0: t322,
                }
                jp319 = t323
                jp315 = jp319
                retv311 = jp315
                return retv311
            } else {
                var t324 *ref_int_x = value__14.index
                var t325 *ref_int_x = value__14.index
                var t326 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t325)
                var t327 int = t326 + 2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t324, t327)
                var mtmp15 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp329 uint32
                switch mtmp15.(type) {
                case Result__uint32__string_Ok:
                    var x16 uint32 = mtmp15.(Result__uint32__string_Ok)._0
                    var codepoint__19 uint32 = x16
                    jp329 = codepoint__19
                    var second__21 uint32 = jp329
                    var t342 bool = second__21 < 56320
                    var jp333 bool
                    if t342 {
                        jp333 = true
                    } else {
                        var t343 bool = second__21 > 57343
                        jp333 = t343
                    }
                    var jp331 Result__unit__string
                    if jp333 {
                        var t334 string = _goml_m_std_p_json_p_json__error(value__14, "invalid low surrogate")
                        var t335 Result__unit__string = Result__unit__string_Err{
                            _0: t334,
                        }
                        jp331 = t335
                    } else {
                        var t336 uint32 = first__18 - 55296
                        var t337 uint32 = t336 * 1024
                        var t338 uint32 = 65536 + t337
                        var t339 uint32 = t338 + second__21
                        var t340 uint32 = t339 - 56320
                        var t341 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, t340)
                        jp331 = t341
                    }
                    jp319 = jp331
                    jp315 = jp319
                    retv311 = jp315
                    return retv311
                case Result__uint32__string_Err:
                    var x17 string = mtmp15.(Result__uint32__string_Err)._0
                    var error__20 string = x17
                    var t344 Result__unit__string = Result__unit__string_Err{
                        _0: error__20,
                    }
                    retv311 = t344
                    return retv311
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t373 bool = first__18 >= 56320
            var jp369 bool
            if t373 {
                var t374 bool = first__18 <= 57343
                jp369 = t374
            } else {
                jp369 = false
            }
            var jp367 Result__unit__string
            if jp369 {
                var t370 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t371 Result__unit__string = Result__unit__string_Err{
                    _0: t370,
                }
                jp367 = t371
            } else {
                var t372 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, first__18)
                jp367 = t372
            }
            jp315 = jp367
            retv311 = jp315
            return retv311
        }
    case Result__uint32__string_Err:
        var x13 string = mtmp11.(Result__uint32__string_Err)._0
        var error__17 string = x13
        var t377 Result__unit__string = Result__unit__string_Err{
            _0: error__17,
        }
        retv311 = t377
        return retv311
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__22 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var retv379 Result__string__string
    var t500 *ref_int_x = value__22.index
    var t501 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t500)
    var t502 string = value__22.input
    var t503 int = _goml_m_inherent_i_string_i_string_i_byte__len(t502)
    var t504 bool = t501 >= t503
    var jp492 bool
    if t504 {
        jp492 = true
    } else {
        var t505 string = value__22.input
        var t506 *ref_int_x = value__22.index
        var t507 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t506)
        var t508 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t505, t507)
        var t509 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t508, 34)
        var t510 bool = !t509
        jp492 = t510
    }
    if jp492 {
        var t493 string = _goml_m_std_p_json_p_json__error(value__22, "expected string")
        var t494 Result__string__string = Result__string__string_Err{
            _0: t493,
        }
        retv379 = t494
        return retv379
    } else {
        var t495 *ref_int_x = value__22.index
        var t496 *ref_int_x = value__22.index
        var t497 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t496)
        var t498 int = t497 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t495, t498)
        var builder__23 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t381 *ref_int_x = value__22.index
        var t382 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t381)
        var segment__24 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t382)
        Loop_loop386:
        for {
            var t387 *ref_int_x = value__22.index
            var t388 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t387)
            var t389 string = value__22.input
            var t390 int = _goml_m_inherent_i_string_i_string_i_byte__len(t389)
            var t391 bool = t388 < t390
            if t391 {
                var t392 string = value__22.input
                var t393 *ref_int_x = value__22.index
                var t394 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t393)
                var byte__25 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t392, t394)
                var t396 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 34)
                if t396 {
                    var t404 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                    var t405 *ref_int_x = value__22.index
                    var t406 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t405)
                    var t407 bool = t404 < t406
                    if t407 {
                        var t408 string = value__22.input
                        var t409 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t410 *ref_int_x = value__22.index
                        var t411 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t410)
                        var t412 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t408, t409, t411)
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t412)
                    } else {}
                    var t398 *ref_int_x = value__22.index
                    var t399 *ref_int_x = value__22.index
                    var t400 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t399)
                    var t401 int = t400 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t398, t401)
                    var t402 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__23)
                    var t403 Result__string__string = Result__string__string_Ok{
                        _0: t402,
                    }
                    retv379 = t403
                    return retv379
                } else {
                    var t415 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 92)
                    if t415 {
                        var t472 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t473 *ref_int_x = value__22.index
                        var t474 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t473)
                        var t475 bool = t472 < t474
                        if t475 {
                            var t476 string = value__22.input
                            var t477 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                            var t478 *ref_int_x = value__22.index
                            var t479 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t478)
                            var t480 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t476, t477, t479)
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t480)
                        } else {}
                        var t417 *ref_int_x = value__22.index
                        var t418 *ref_int_x = value__22.index
                        var t419 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t418)
                        var t420 int = t419 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t417, t420)
                        var t465 *ref_int_x = value__22.index
                        var t466 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t465)
                        var t467 string = value__22.input
                        var t468 int = _goml_m_inherent_i_string_i_string_i_byte__len(t467)
                        var t469 bool = t466 >= t468
                        if t469 {
                            var t470 string = _goml_m_std_p_json_p_json__error(value__22, "incomplete escape")
                            var t471 Result__string__string = Result__string__string_Err{
                                _0: t470,
                            }
                            retv379 = t471
                            return retv379
                        } else {
                            var t422 string = value__22.input
                            var t423 *ref_int_x = value__22.index
                            var t424 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t423)
                            var escape__26 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t422, t424)
                            var t425 *ref_int_x = value__22.index
                            var t426 *ref_int_x = value__22.index
                            var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                            var t428 int = t427 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t425, t428)
                            var t433 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 34)
                            if t433 {
                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 34)
                                var t430 *ref_int_x = value__22.index
                                var t431 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t430)
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t431)
                                continue
                            } else {
                                var t436 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 92)
                                if t436 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 92)
                                    var t430 *ref_int_x = value__22.index
                                    var t431 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t430)
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t431)
                                    continue
                                } else {
                                    var t439 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 47)
                                    if t439 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 47)
                                        var t430 *ref_int_x = value__22.index
                                        var t431 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t430)
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t431)
                                        continue
                                    } else {
                                        var t442 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 98)
                                        if t442 {
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
                                            var t430 *ref_int_x = value__22.index
                                            var t431 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t430)
                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t431)
                                            continue
                                        } else {
                                            var t446 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 102)
                                            if t446 {
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
                                                var t430 *ref_int_x = value__22.index
                                                var t431 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t430)
                                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t431)
                                                continue
                                            } else {
                                                var t450 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 110)
                                                if t450 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 10)
                                                    var t430 *ref_int_x = value__22.index
                                                    var t431 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t430)
                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t431)
                                                    continue
                                                } else {
                                                    var t453 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 114)
                                                    if t453 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 13)
                                                        var t430 *ref_int_x = value__22.index
                                                        var t431 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t430)
                                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t431)
                                                        continue
                                                    } else {
                                                        var t456 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 116)
                                                        if t456 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 9)
                                                            var t430 *ref_int_x = value__22.index
                                                            var t431 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t430)
                                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t431)
                                                            continue
                                                        } else {
                                                            var t459 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 117)
                                                            if t459 {
                                                                var mtmp29 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__22, builder__23)
                                                                switch mtmp29.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t430 *ref_int_x = value__22.index
                                                                    var t431 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t430)
                                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t431)
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x31 string = mtmp29.(Result__unit__string_Err)._0
                                                                    var error__29 string = x31
                                                                    var t462 Result__string__string = Result__string__string_Err{
                                                                        _0: error__29,
                                                                    }
                                                                    retv379 = t462
                                                                    return retv379
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t463 string = _goml_m_std_p_json_p_json__error(value__22, "invalid escape")
                                                                var t464 Result__string__string = Result__string__string_Err{
                                                                    _0: t463,
                                                                }
                                                                retv379 = t464
                                                                return retv379
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
                        var t483 bool = byte__25 < 32
                        if t483 {
                            var t484 string = _goml_m_std_p_json_p_json__error(value__22, "unescaped control character")
                            var t485 Result__string__string = Result__string__string_Err{
                                _0: t484,
                            }
                            retv379 = t485
                            return retv379
                        } else {
                            var t486 *ref_int_x = value__22.index
                            var t487 *ref_int_x = value__22.index
                            var t488 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t487)
                            var t489 int = t488 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t486, t489)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop386
            }
        }
        var t384 string = _goml_m_std_p_json_p_json__error(value__22, "unterminated string")
        var t385 Result__string__string = Result__string__string_Err{
            _0: t384,
        }
        retv379 = t385
        return retv379
    }
}

func _goml_m_std_p_json_p_json__digit(value__30 uint8) bool {
    var retv512 bool
    var t515 bool = value__30 >= 48
    var jp514 bool
    if t515 {
        var t516 bool = value__30 <= 57
        jp514 = t516
    } else {
        jp514 = false
    }
    retv512 = jp514
    return retv512
}

func _goml_m_std_p_json_p_parse__digits(value__31 _goml_m_std_p_json_p_JsonParser) bool {
    var retv518 bool
    var t519 *ref_int_x = value__31.index
    var start__32 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t519)
    Loop_loop524:
    for {
        var t532 *ref_int_x = value__31.index
        var t533 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t532)
        var t534 string = value__31.input
        var t535 int = _goml_m_inherent_i_string_i_string_i_byte__len(t534)
        var t536 bool = t533 < t535
        var jp526 bool
        if t536 {
            var t537 string = value__31.input
            var t538 *ref_int_x = value__31.index
            var t539 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t538)
            var t540 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t537, t539)
            var t541 bool = _goml_m_std_p_json_p_json__digit(t540)
            jp526 = t541
        } else {
            jp526 = false
        }
        if jp526 {
            var t527 *ref_int_x = value__31.index
            var t528 *ref_int_x = value__31.index
            var t529 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t528)
            var t530 int = t529 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t527, t530)
            continue
        } else {
            break Loop_loop524
        }
    }
    var t521 *ref_int_x = value__31.index
    var t522 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t521)
    var t523 bool = t522 > start__32
    retv518 = t523
    return retv518
}

func _goml_m_std_p_json_p_parse__json__number(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv543 _goml_m_Result____std_p_json_p_Value____string
    var t544 *ref_int_x = value__33.index
    var start__34 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t544)
    var t666 string = value__33.input
    var t667 *ref_int_x = value__33.index
    var t668 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t667)
    var t669 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t666, t668)
    var t670 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t669, 45)
    if t670 {
        var t671 *ref_int_x = value__33.index
        var t672 *ref_int_x = value__33.index
        var t673 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t672)
        var t674 int = t673 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t671, t674)
    } else {}
    var t629 *ref_int_x = value__33.index
    var t630 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t629)
    var t631 string = value__33.input
    var t632 int = _goml_m_inherent_i_string_i_string_i_byte__len(t631)
    var t633 bool = t630 >= t632
    if t633 {
        var t634 string = _goml_m_std_p_json_p_json__error(value__33, "incomplete number")
        var t635 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t634,
        }
        retv543 = t635
        return retv543
    } else {
        var t637 string = value__33.input
        var t638 *ref_int_x = value__33.index
        var t639 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t638)
        var t640 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t637, t639)
        var t641 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t640, 48)
        if t641 {
            var t642 *ref_int_x = value__33.index
            var t643 *ref_int_x = value__33.index
            var t644 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t643)
            var t645 int = t644 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t642, t645)
            var t651 *ref_int_x = value__33.index
            var t652 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t651)
            var t653 string = value__33.input
            var t654 int = _goml_m_inherent_i_string_i_string_i_byte__len(t653)
            var t655 bool = t652 < t654
            var jp648 bool
            if t655 {
                var t656 string = value__33.input
                var t657 *ref_int_x = value__33.index
                var t658 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t657)
                var t659 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t656, t658)
                var t660 bool = _goml_m_std_p_json_p_json__digit(t659)
                jp648 = t660
            } else {
                jp648 = false
            }
            if jp648 {
                var t649 string = _goml_m_std_p_json_p_json__error(value__33, "invalid leading zero")
                var t650 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t649,
                }
                retv543 = t650
                return retv543
            } else {
                var t619 *ref_int_x = value__33.index
                var t620 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t619)
                var t621 string = value__33.input
                var t622 int = _goml_m_inherent_i_string_i_string_i_byte__len(t621)
                var t623 bool = t620 < t622
                var jp609 bool
                if t623 {
                    var t624 string = value__33.input
                    var t625 *ref_int_x = value__33.index
                    var t626 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t625)
                    var t627 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t624, t626)
                    var t628 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t627, 46)
                    jp609 = t628
                } else {
                    jp609 = false
                }
                if jp609 {
                    var t610 *ref_int_x = value__33.index
                    var t611 *ref_int_x = value__33.index
                    var t612 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t611)
                    var t613 int = t612 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t610, t613)
                    var t615 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t616 bool = !t615
                    if t616 {
                        var t617 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t618 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t617,
                        }
                        retv543 = t618
                        return retv543
                    } else {
                        var t591 *ref_int_x = value__33.index
                        var t592 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t591)
                        var t593 string = value__33.input
                        var t594 int = _goml_m_inherent_i_string_i_string_i_byte__len(t593)
                        var t595 bool = t592 < t594
                        var jp556 bool
                        if t595 {
                            var t598 string = value__33.input
                            var t599 *ref_int_x = value__33.index
                            var t600 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t599)
                            var t601 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t598, t600)
                            var t602 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t601, 101)
                            var jp597 bool
                            if t602 {
                                jp597 = true
                            } else {
                                var t603 string = value__33.input
                                var t604 *ref_int_x = value__33.index
                                var t605 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t604)
                                var t606 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t603, t605)
                                var t607 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t606, 69)
                                jp597 = t607
                            }
                            jp556 = jp597
                        } else {
                            jp556 = false
                        }
                        if jp556 {
                            var t557 *ref_int_x = value__33.index
                            var t558 *ref_int_x = value__33.index
                            var t559 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t558)
                            var t560 int = t559 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t557, t560)
                            var t574 *ref_int_x = value__33.index
                            var t575 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t574)
                            var t576 string = value__33.input
                            var t577 int = _goml_m_inherent_i_string_i_string_i_byte__len(t576)
                            var t578 bool = t575 < t577
                            var jp568 bool
                            if t578 {
                                var t581 string = value__33.input
                                var t582 *ref_int_x = value__33.index
                                var t583 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t582)
                                var t584 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t581, t583)
                                var t585 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t584, 43)
                                var jp580 bool
                                if t585 {
                                    jp580 = true
                                } else {
                                    var t586 string = value__33.input
                                    var t587 *ref_int_x = value__33.index
                                    var t588 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t587)
                                    var t589 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t586, t588)
                                    var t590 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t589, 45)
                                    jp580 = t590
                                }
                                jp568 = jp580
                            } else {
                                jp568 = false
                            }
                            if jp568 {
                                var t569 *ref_int_x = value__33.index
                                var t570 *ref_int_x = value__33.index
                                var t571 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t570)
                                var t572 int = t571 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t569, t572)
                            } else {}
                            var t563 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t564 bool = !t563
                            if t564 {
                                var t565 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t566 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t565,
                                }
                                retv543 = t566
                                return retv543
                            } else {
                                var t549 string = value__33.input
                                var t550 *ref_int_x = value__33.index
                                var t551 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t550)
                                var t552 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t549, start__34, t551)
                                var t553 _goml_m_std_p_json_p_Value = Number{
                                    _0: t552,
                                }
                                var t554 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t553,
                                }
                                retv543 = t554
                                return retv543
                            }
                        } else {
                            var t549 string = value__33.input
                            var t550 *ref_int_x = value__33.index
                            var t551 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t550)
                            var t552 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t549, start__34, t551)
                            var t553 _goml_m_std_p_json_p_Value = Number{
                                _0: t552,
                            }
                            var t554 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t553,
                            }
                            retv543 = t554
                            return retv543
                        }
                    }
                } else {
                    var t591 *ref_int_x = value__33.index
                    var t592 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t591)
                    var t593 string = value__33.input
                    var t594 int = _goml_m_inherent_i_string_i_string_i_byte__len(t593)
                    var t595 bool = t592 < t594
                    var jp556 bool
                    if t595 {
                        var t598 string = value__33.input
                        var t599 *ref_int_x = value__33.index
                        var t600 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t599)
                        var t601 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t598, t600)
                        var t602 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t601, 101)
                        var jp597 bool
                        if t602 {
                            jp597 = true
                        } else {
                            var t603 string = value__33.input
                            var t604 *ref_int_x = value__33.index
                            var t605 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t604)
                            var t606 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t603, t605)
                            var t607 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t606, 69)
                            jp597 = t607
                        }
                        jp556 = jp597
                    } else {
                        jp556 = false
                    }
                    if jp556 {
                        var t557 *ref_int_x = value__33.index
                        var t558 *ref_int_x = value__33.index
                        var t559 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t558)
                        var t560 int = t559 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t557, t560)
                        var t574 *ref_int_x = value__33.index
                        var t575 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t574)
                        var t576 string = value__33.input
                        var t577 int = _goml_m_inherent_i_string_i_string_i_byte__len(t576)
                        var t578 bool = t575 < t577
                        var jp568 bool
                        if t578 {
                            var t581 string = value__33.input
                            var t582 *ref_int_x = value__33.index
                            var t583 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t582)
                            var t584 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t581, t583)
                            var t585 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t584, 43)
                            var jp580 bool
                            if t585 {
                                jp580 = true
                            } else {
                                var t586 string = value__33.input
                                var t587 *ref_int_x = value__33.index
                                var t588 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t587)
                                var t589 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t586, t588)
                                var t590 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t589, 45)
                                jp580 = t590
                            }
                            jp568 = jp580
                        } else {
                            jp568 = false
                        }
                        if jp568 {
                            var t569 *ref_int_x = value__33.index
                            var t570 *ref_int_x = value__33.index
                            var t571 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t570)
                            var t572 int = t571 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t569, t572)
                        } else {}
                        var t563 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t564 bool = !t563
                        if t564 {
                            var t565 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t566 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t565,
                            }
                            retv543 = t566
                            return retv543
                        } else {
                            var t549 string = value__33.input
                            var t550 *ref_int_x = value__33.index
                            var t551 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t550)
                            var t552 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t549, start__34, t551)
                            var t553 _goml_m_std_p_json_p_Value = Number{
                                _0: t552,
                            }
                            var t554 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t553,
                            }
                            retv543 = t554
                            return retv543
                        }
                    } else {
                        var t549 string = value__33.input
                        var t550 *ref_int_x = value__33.index
                        var t551 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t550)
                        var t552 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t549, start__34, t551)
                        var t553 _goml_m_std_p_json_p_Value = Number{
                            _0: t552,
                        }
                        var t554 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t553,
                        }
                        retv543 = t554
                        return retv543
                    }
                }
            }
        } else {
            var t662 bool = _goml_m_std_p_json_p_parse__digits(value__33)
            var t663 bool = !t662
            if t663 {
                var t664 string = _goml_m_std_p_json_p_json__error(value__33, "expected number")
                var t665 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t664,
                }
                retv543 = t665
                return retv543
            } else {
                var t619 *ref_int_x = value__33.index
                var t620 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t619)
                var t621 string = value__33.input
                var t622 int = _goml_m_inherent_i_string_i_string_i_byte__len(t621)
                var t623 bool = t620 < t622
                var jp609 bool
                if t623 {
                    var t624 string = value__33.input
                    var t625 *ref_int_x = value__33.index
                    var t626 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t625)
                    var t627 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t624, t626)
                    var t628 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t627, 46)
                    jp609 = t628
                } else {
                    jp609 = false
                }
                if jp609 {
                    var t610 *ref_int_x = value__33.index
                    var t611 *ref_int_x = value__33.index
                    var t612 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t611)
                    var t613 int = t612 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t610, t613)
                    var t615 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t616 bool = !t615
                    if t616 {
                        var t617 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t618 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t617,
                        }
                        retv543 = t618
                        return retv543
                    } else {
                        var t591 *ref_int_x = value__33.index
                        var t592 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t591)
                        var t593 string = value__33.input
                        var t594 int = _goml_m_inherent_i_string_i_string_i_byte__len(t593)
                        var t595 bool = t592 < t594
                        var jp556 bool
                        if t595 {
                            var t598 string = value__33.input
                            var t599 *ref_int_x = value__33.index
                            var t600 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t599)
                            var t601 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t598, t600)
                            var t602 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t601, 101)
                            var jp597 bool
                            if t602 {
                                jp597 = true
                            } else {
                                var t603 string = value__33.input
                                var t604 *ref_int_x = value__33.index
                                var t605 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t604)
                                var t606 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t603, t605)
                                var t607 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t606, 69)
                                jp597 = t607
                            }
                            jp556 = jp597
                        } else {
                            jp556 = false
                        }
                        if jp556 {
                            var t557 *ref_int_x = value__33.index
                            var t558 *ref_int_x = value__33.index
                            var t559 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t558)
                            var t560 int = t559 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t557, t560)
                            var t574 *ref_int_x = value__33.index
                            var t575 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t574)
                            var t576 string = value__33.input
                            var t577 int = _goml_m_inherent_i_string_i_string_i_byte__len(t576)
                            var t578 bool = t575 < t577
                            var jp568 bool
                            if t578 {
                                var t581 string = value__33.input
                                var t582 *ref_int_x = value__33.index
                                var t583 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t582)
                                var t584 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t581, t583)
                                var t585 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t584, 43)
                                var jp580 bool
                                if t585 {
                                    jp580 = true
                                } else {
                                    var t586 string = value__33.input
                                    var t587 *ref_int_x = value__33.index
                                    var t588 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t587)
                                    var t589 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t586, t588)
                                    var t590 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t589, 45)
                                    jp580 = t590
                                }
                                jp568 = jp580
                            } else {
                                jp568 = false
                            }
                            if jp568 {
                                var t569 *ref_int_x = value__33.index
                                var t570 *ref_int_x = value__33.index
                                var t571 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t570)
                                var t572 int = t571 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t569, t572)
                            } else {}
                            var t563 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t564 bool = !t563
                            if t564 {
                                var t565 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t566 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t565,
                                }
                                retv543 = t566
                                return retv543
                            } else {
                                var t549 string = value__33.input
                                var t550 *ref_int_x = value__33.index
                                var t551 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t550)
                                var t552 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t549, start__34, t551)
                                var t553 _goml_m_std_p_json_p_Value = Number{
                                    _0: t552,
                                }
                                var t554 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t553,
                                }
                                retv543 = t554
                                return retv543
                            }
                        } else {
                            var t549 string = value__33.input
                            var t550 *ref_int_x = value__33.index
                            var t551 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t550)
                            var t552 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t549, start__34, t551)
                            var t553 _goml_m_std_p_json_p_Value = Number{
                                _0: t552,
                            }
                            var t554 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t553,
                            }
                            retv543 = t554
                            return retv543
                        }
                    }
                } else {
                    var t591 *ref_int_x = value__33.index
                    var t592 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t591)
                    var t593 string = value__33.input
                    var t594 int = _goml_m_inherent_i_string_i_string_i_byte__len(t593)
                    var t595 bool = t592 < t594
                    var jp556 bool
                    if t595 {
                        var t598 string = value__33.input
                        var t599 *ref_int_x = value__33.index
                        var t600 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t599)
                        var t601 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t598, t600)
                        var t602 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t601, 101)
                        var jp597 bool
                        if t602 {
                            jp597 = true
                        } else {
                            var t603 string = value__33.input
                            var t604 *ref_int_x = value__33.index
                            var t605 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t604)
                            var t606 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t603, t605)
                            var t607 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t606, 69)
                            jp597 = t607
                        }
                        jp556 = jp597
                    } else {
                        jp556 = false
                    }
                    if jp556 {
                        var t557 *ref_int_x = value__33.index
                        var t558 *ref_int_x = value__33.index
                        var t559 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t558)
                        var t560 int = t559 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t557, t560)
                        var t574 *ref_int_x = value__33.index
                        var t575 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t574)
                        var t576 string = value__33.input
                        var t577 int = _goml_m_inherent_i_string_i_string_i_byte__len(t576)
                        var t578 bool = t575 < t577
                        var jp568 bool
                        if t578 {
                            var t581 string = value__33.input
                            var t582 *ref_int_x = value__33.index
                            var t583 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t582)
                            var t584 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t581, t583)
                            var t585 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t584, 43)
                            var jp580 bool
                            if t585 {
                                jp580 = true
                            } else {
                                var t586 string = value__33.input
                                var t587 *ref_int_x = value__33.index
                                var t588 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t587)
                                var t589 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t586, t588)
                                var t590 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t589, 45)
                                jp580 = t590
                            }
                            jp568 = jp580
                        } else {
                            jp568 = false
                        }
                        if jp568 {
                            var t569 *ref_int_x = value__33.index
                            var t570 *ref_int_x = value__33.index
                            var t571 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t570)
                            var t572 int = t571 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t569, t572)
                        } else {}
                        var t563 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t564 bool = !t563
                        if t564 {
                            var t565 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t566 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t565,
                            }
                            retv543 = t566
                            return retv543
                        } else {
                            var t549 string = value__33.input
                            var t550 *ref_int_x = value__33.index
                            var t551 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t550)
                            var t552 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t549, start__34, t551)
                            var t553 _goml_m_std_p_json_p_Value = Number{
                                _0: t552,
                            }
                            var t554 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t553,
                            }
                            retv543 = t554
                            return retv543
                        }
                    } else {
                        var t549 string = value__33.input
                        var t550 *ref_int_x = value__33.index
                        var t551 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t550)
                        var t552 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t549, start__34, t551)
                        var t553 _goml_m_std_p_json_p_Value = Number{
                            _0: t552,
                        }
                        var t554 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t553,
                        }
                        retv543 = t554
                        return retv543
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__35 _goml_m_std_p_json_p_JsonParser, expected__36 string, result__37 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var retv677 _goml_m_Result____std_p_json_p_Value____string
    var t690 *ref_int_x = value__35.index
    var t691 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t690)
    var t692 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
    var t693 int = t691 + t692
    var t694 string = value__35.input
    var t695 int = _goml_m_inherent_i_string_i_string_i_byte__len(t694)
    var t696 bool = t693 <= t695
    var jp681 bool
    if t696 {
        var t697 string = value__35.input
        var t698 *ref_int_x = value__35.index
        var t699 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t698)
        var t700 *ref_int_x = value__35.index
        var t701 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t700)
        var t702 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t703 int = t701 + t702
        var t704 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t697, t699, t703)
        var t705 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t704, expected__36)
        jp681 = t705
    } else {
        jp681 = false
    }
    var jp679 _goml_m_Result____std_p_json_p_Value____string
    if jp681 {
        var t682 *ref_int_x = value__35.index
        var t683 *ref_int_x = value__35.index
        var t684 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t683)
        var t685 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t686 int = t684 + t685
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t682, t686)
        var t687 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__37,
        }
        jp679 = t687
    } else {
        var t688 string = _goml_m_std_p_json_p_json__error(value__35, "invalid literal")
        var t689 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t688,
        }
        jp679 = t689
    }
    retv677 = jp679
    return retv677
}

func _goml_m_std_p_json_p_parse__json__array(value__38 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv707 _goml_m_Result____std_p_json_p_Value____string
    var t708 *ref_int_x = value__38.index
    var t709 *ref_int_x = value__38.index
    var t710 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t709)
    var t711 int = t710 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t708, t711)
    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
    var result__39 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t766 *ref_int_x = value__38.index
    var t767 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t766)
    var t768 string = value__38.input
    var t769 int = _goml_m_inherent_i_string_i_string_i_byte__len(t768)
    var t770 bool = t767 < t769
    var jp759 bool
    if t770 {
        var t771 string = value__38.input
        var t772 *ref_int_x = value__38.index
        var t773 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t772)
        var t774 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t771, t773)
        var t775 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t774, 93)
        jp759 = t775
    } else {
        jp759 = false
    }
    if jp759 {
        var t760 *ref_int_x = value__38.index
        var t761 *ref_int_x = value__38.index
        var t762 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t761)
        var t763 int = t762 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t760, t763)
        var t764 _goml_m_std_p_json_p_Value = Array{
            _0: result__39,
        }
        var t765 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t764,
        }
        retv707 = t765
        return retv707
    } else {
        Loop_loop716:
        for {
            var t717 *ref_int_x = value__38.index
            var t718 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t717)
            var t719 string = value__38.input
            var t720 int = _goml_m_inherent_i_string_i_string_i_byte__len(t719)
            var t721 bool = t718 < t720
            if t721 {
                var mtmp48 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__38)
                switch mtmp48.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x49 _goml_m_std_p_json_p_Value = mtmp48.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    var item__40 _goml_m_std_p_json_p_Value = x49
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(result__39, item__40)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                    var t724 *ref_int_x = value__38.index
                    var t725 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t724)
                    var t726 string = value__38.input
                    var t727 int = _goml_m_inherent_i_string_i_string_i_byte__len(t726)
                    var t728 bool = t725 >= t727
                    if t728 {
                        var t729 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
                        var t730 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t729,
                        }
                        retv707 = t730
                        return retv707
                    } else {
                        var t732 string = value__38.input
                        var t733 *ref_int_x = value__38.index
                        var t734 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t733)
                        var t735 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t732, t734)
                        var t736 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t735, 93)
                        if t736 {
                            var t737 *ref_int_x = value__38.index
                            var t738 *ref_int_x = value__38.index
                            var t739 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t738)
                            var t740 int = t739 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t737, t740)
                            var t741 _goml_m_std_p_json_p_Value = Array{
                                _0: result__39,
                            }
                            var t742 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t741,
                            }
                            retv707 = t742
                            return retv707
                        } else {
                            var t744 string = value__38.input
                            var t745 *ref_int_x = value__38.index
                            var t746 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t745)
                            var t747 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t744, t746)
                            var t748 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t747, 44)
                            if t748 {
                                var t749 *ref_int_x = value__38.index
                                var t750 *ref_int_x = value__38.index
                                var t751 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t750)
                                var t752 int = t751 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t749, t752)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                                continue
                            } else {
                                var t754 string = _goml_m_std_p_json_p_json__error(value__38, "expected array separator")
                                var t755 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t754,
                                }
                                retv707 = t755
                                return retv707
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x50 string = mtmp48.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var error__41 string = x50
                    var t757 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__41,
                    }
                    retv707 = t757
                    return retv707
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop716
            }
        }
        var t714 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
        var t715 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t714,
        }
        retv707 = t715
        return retv707
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__42 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv777 _goml_m_Result____std_p_json_p_Value____string
    var t778 *ref_int_x = value__42.index
    var t779 *ref_int_x = value__42.index
    var t780 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t779)
    var t781 int = t780 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t778, t781)
    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
    var result__43 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t861 *ref_int_x = value__42.index
    var t862 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t861)
    var t863 string = value__42.input
    var t864 int = _goml_m_inherent_i_string_i_string_i_byte__len(t863)
    var t865 bool = t862 < t864
    var jp854 bool
    if t865 {
        var t866 string = value__42.input
        var t867 *ref_int_x = value__42.index
        var t868 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t867)
        var t869 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t866, t868)
        var t870 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t869, 125)
        jp854 = t870
    } else {
        jp854 = false
    }
    if jp854 {
        var t855 *ref_int_x = value__42.index
        var t856 *ref_int_x = value__42.index
        var t857 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t856)
        var t858 int = t857 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t855, t858)
        var t859 _goml_m_std_p_json_p_Value = Object{
            _0: result__43,
        }
        var t860 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t859,
        }
        retv777 = t860
        return retv777
    } else {
        Loop_loop786:
        for {
            var t787 *ref_int_x = value__42.index
            var t788 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t787)
            var t789 string = value__42.input
            var t790 int = _goml_m_inherent_i_string_i_string_i_byte__len(t789)
            var t791 bool = t788 < t790
            if t791 {
                var mtmp60 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__42)
                var jp793 string
                switch mtmp60.(type) {
                case Result__string__string_Ok:
                    var x61 string = mtmp60.(Result__string__string_Ok)._0
                    var name__44 string = x61
                    jp793 = name__44
                    var name__46 string = jp793
                    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                    var t841 *ref_int_x = value__42.index
                    var t842 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t841)
                    var t843 string = value__42.input
                    var t844 int = _goml_m_inherent_i_string_i_string_i_byte__len(t843)
                    var t845 bool = t842 >= t844
                    var jp833 bool
                    if t845 {
                        jp833 = true
                    } else {
                        var t846 string = value__42.input
                        var t847 *ref_int_x = value__42.index
                        var t848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t847)
                        var t849 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t846, t848)
                        var t850 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t849, 58)
                        var t851 bool = !t850
                        jp833 = t851
                    }
                    if jp833 {
                        var t834 string = _goml_m_std_p_json_p_json__error(value__42, "expected object colon")
                        var t835 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t834,
                        }
                        retv777 = t835
                        return retv777
                    } else {
                        var t836 *ref_int_x = value__42.index
                        var t837 *ref_int_x = value__42.index
                        var t838 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t837)
                        var t839 int = t838 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t836, t839)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                        var mtmp66 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__42)
                        switch mtmp66.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x67 _goml_m_std_p_json_p_Value = mtmp66.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            var item__47 _goml_m_std_p_json_p_Value = x67
                            var t829 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: name__46,
                                _1: item__47,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__43, t829)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                            var t797 *ref_int_x = value__42.index
                            var t798 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t797)
                            var t799 string = value__42.input
                            var t800 int = _goml_m_inherent_i_string_i_string_i_byte__len(t799)
                            var t801 bool = t798 >= t800
                            if t801 {
                                var t802 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
                                var t803 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t802,
                                }
                                retv777 = t803
                                return retv777
                            } else {
                                var t805 string = value__42.input
                                var t806 *ref_int_x = value__42.index
                                var t807 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t806)
                                var t808 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t805, t807)
                                var t809 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t808, 125)
                                if t809 {
                                    var t810 *ref_int_x = value__42.index
                                    var t811 *ref_int_x = value__42.index
                                    var t812 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t811)
                                    var t813 int = t812 + 1
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t810, t813)
                                    var t814 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__43,
                                    }
                                    var t815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t814,
                                    }
                                    retv777 = t815
                                    return retv777
                                } else {
                                    var t817 string = value__42.input
                                    var t818 *ref_int_x = value__42.index
                                    var t819 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t818)
                                    var t820 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t817, t819)
                                    var t821 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t820, 44)
                                    if t821 {
                                        var t822 *ref_int_x = value__42.index
                                        var t823 *ref_int_x = value__42.index
                                        var t824 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t823)
                                        var t825 int = t824 + 1
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t822, t825)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                                        continue
                                    } else {
                                        var t827 string = _goml_m_std_p_json_p_json__error(value__42, "expected object separator")
                                        var t828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t827,
                                        }
                                        retv777 = t828
                                        return retv777
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x68 string = mtmp66.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var error__48 string = x68
                            var t831 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: error__48,
                            }
                            retv777 = t831
                            return retv777
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x62 string = mtmp60.(Result__string__string_Err)._0
                    var error__45 string = x62
                    var t852 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__45,
                    }
                    retv777 = t852
                    return retv777
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop786
            }
        }
        var t784 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
        var t785 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t784,
        }
        retv777 = t785
        return retv777
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__49 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv872 _goml_m_Result____std_p_json_p_Value____string
    _goml_m_std_p_json_p_skip__json__whitespace(value__49)
    var t875 *ref_int_x = value__49.index
    var t876 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t875)
    var t877 string = value__49.input
    var t878 int = _goml_m_inherent_i_string_i_string_i_byte__len(t877)
    var t879 bool = t876 >= t878
    var jp874 _goml_m_Result____std_p_json_p_Value____string
    if t879 {
        var t880 string = _goml_m_std_p_json_p_json__error(value__49, "expected JSON value")
        var t881 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t880,
        }
        jp874 = t881
    } else {
        var t882 string = value__49.input
        var t883 *ref_int_x = value__49.index
        var t884 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t883)
        var mtmp75 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t882, t884)
        var jp886 _goml_m_Result____std_p_json_p_Value____string
        switch mtmp75 {
        case 123:
            var t887 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__49)
            jp886 = t887
        case 91:
            var t888 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__49)
            jp886 = t888
        case 34:
            var mtmp76 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__49)
            var jp890 _goml_m_Result____std_p_json_p_Value____string
            switch mtmp76.(type) {
            case Result__string__string_Ok:
                var x77 string = mtmp76.(Result__string__string_Ok)._0
                var text__50 string = x77
                var t891 _goml_m_std_p_json_p_Value = String{
                    _0: text__50,
                }
                var t892 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t891,
                }
                jp890 = t892
            case Result__string__string_Err:
                var x78 string = mtmp76.(Result__string__string_Err)._0
                var error__51 string = x78
                var t893 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: error__51,
                }
                jp890 = t893
            default:
                panic("non-exhaustive match")
            }
            jp886 = jp890
        case 116:
            var t894 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t895 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "true", t894)
            jp886 = t895
        case 102:
            var t896 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t897 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "false", t896)
            jp886 = t897
        case 110:
            var t898 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "null", Null{})
            jp886 = t898
        default:
            var byte__52 uint8 = mtmp75
            var t906 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 45)
            var jp902 bool
            if t906 {
                jp902 = true
            } else {
                var t907 bool = _goml_m_std_p_json_p_json__digit(byte__52)
                jp902 = t907
            }
            var jp900 _goml_m_Result____std_p_json_p_Value____string
            if jp902 {
                var t903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__49)
                jp900 = t903
            } else {
                var t904 string = _goml_m_std_p_json_p_json__error(value__49, "unexpected JSON token")
                var t905 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t904,
                }
                jp900 = t905
            }
            jp886 = jp900
        }
        jp874 = jp886
    }
    retv872 = jp874
    return retv872
}

func _goml_m_std_p_json_p_parse(input__53 string) _goml_m_Result____std_p_json_p_Value____string {
    var retv909 _goml_m_Result____std_p_json_p_Value____string
    var parser__54 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_json__parser(input__53)
    var mtmp79 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__54)
    var jp911 _goml_m_std_p_json_p_Value
    switch mtmp79.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x80 _goml_m_std_p_json_p_Value = mtmp79.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__55 _goml_m_std_p_json_p_Value = x80
        jp911 = value__55
        var result__57 _goml_m_std_p_json_p_Value = jp911
        _goml_m_std_p_json_p_skip__json__whitespace(parser__54)
        var t914 *ref_int_x = parser__54.index
        var t915 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t914)
        var t916 int = _goml_m_inherent_i_string_i_string_i_byte__len(input__53)
        var t917 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t915, t916)
        var jp913 _goml_m_Result____std_p_json_p_Value____string
        if t917 {
            var t918 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: result__57,
            }
            jp913 = t918
        } else {
            var t919 string = _goml_m_std_p_json_p_json__error(parser__54, "trailing JSON data")
            var t920 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t919,
            }
            jp913 = t920
        }
        retv909 = jp913
        return retv909
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x81 string = mtmp79.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var error__56 string = x81
        var t921 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: error__56,
        }
        retv909 = t921
        return retv909
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_json__hex__digit(value__58 uint8) rune {
    var retv923 rune
    var t924 int = int(uint8(value__58))
    var t925 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", t924)
    retv923 = t925
    return retv923
}

func _goml_m_std_p_json_p_write__json__string(builder__59 _goml_m_std_p_text_p_StringBuilder, value__60 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, 34)
    var start__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var for_index84 int = 0
    var for_limit85 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    Loop_loop937:
    for {
        var t938 bool = for_index84 < for_limit85
        if t938 {
            var for_item86 int = for_index84
            var t939 int = for_index84 + 1
            for_index84 = t939
            var index__62 int = for_item86
            var byte__63 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__60, index__62)
            var t995 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
            var jp993 bool
            if t995 {
                jp993 = true
            } else {
                var t996 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                jp993 = t996
            }
            var jp990 bool
            if jp993 {
                jp990 = true
            } else {
                var t994 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                jp990 = t994
            }
            var jp987 bool
            if jp990 {
                jp987 = true
            } else {
                var t991 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                jp987 = t991
            }
            var jp984 bool
            if jp987 {
                jp984 = true
            } else {
                var t988 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                jp984 = t988
            }
            var jp981 bool
            if jp984 {
                jp981 = true
            } else {
                var t985 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                jp981 = t985
            }
            var jp978 bool
            if jp981 {
                jp978 = true
            } else {
                var t982 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                jp978 = t982
            }
            var jp942 bool
            if jp978 {
                jp942 = true
            } else {
                var t979 bool = byte__63 < 32
                jp942 = t979
            }
            if jp942 {
                var t972 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                var t973 bool = t972 < index__62
                if t973 {
                    var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                    var t975 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t974, index__62)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t975)
                } else {}
                var t947 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
                if t947 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\"")
                } else {
                    var t950 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                    if t950 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\\")
                    } else {
                        var t953 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                        if t953 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\b")
                        } else {
                            var t956 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                            if t956 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\t")
                            } else {
                                var t959 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                                if t959 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\n")
                                } else {
                                    var t962 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                                    if t962 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\f")
                                    } else {
                                        var t965 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                                        if t965 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\u00")
                                            var t967 uint8 = byte__63 / 16
                                            var t968 rune = _goml_m_std_p_json_p_json__hex__digit(t967)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t968)
                                            var t969_rhs uint8 = 16
                                            var t969 uint8 = byte__63 % t969_rhs
                                            var t970 rune = _goml_m_std_p_json_p_json__hex__digit(t969)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t970)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t945 int = index__62 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(start__61, t945)
            } else {}
            continue
        } else {
            break Loop_loop937
        }
    }
    var t930 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
    var t931 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    var t932 bool = t930 < t931
    if t932 {
        var t933 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
        var t934 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
        var t935 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t933, t934)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t935)
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
        Loop_loop1001:
        for {
            var t1002 bool = for_index102 < for_limit101
            if t1002 {
                var for_item103 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source100, for_index102)
                var t1003 int = for_index102 + 1
                for_index102 = t1003
                var field__68 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item103
                var t1010 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                var t1011 bool = t1010 > 0
                if t1011 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                } else {}
                var t1005 string = field__68._0
                _goml_m_std_p_json_p_write__json__string(builder__64, t1005)
                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 58)
                var t1006 _goml_m_std_p_json_p_Value = field__68._1
                _goml_m_std_p_json_p_write__json__value(builder__64, t1006)
                var t1007 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                var t1008 int = t1007 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__67, t1008)
                continue
            } else {
                break Loop_loop1001
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
        Loop_loop1015:
        for {
            var t1016 bool = for_index113 < for_limit112
            if t1016 {
                var for_item114 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(for_source111, for_index113)
                var t1017 int = for_index113 + 1
                for_index113 = t1017
                var item__71 _goml_m_std_p_json_p_Value = for_item114
                var t1022 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__70)
                var t1023 bool = t1022 > 0
                if t1023 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__64, item__71)
                var t1019 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__70)
                var t1020 int = t1019 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__70, t1020)
                continue
            } else {
                break Loop_loop1015
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
        var jp1028 string
        if value__74 {
            jp1028 = "true"
        } else {
            jp1028 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, jp1028)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__75 _goml_m_std_p_json_p_Value) string {
    var retv1032 string
    var builder__76 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
    _goml_m_std_p_json_p_write__json__value(builder__76, value__75)
    var t1033 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__76)
    retv1032 = t1033
    return retv1032
}

func _goml_m_std_p_json_p_field(value__77 _goml_m_std_p_json_p_Value, name__78 string) _goml_m_Option____std_p_json_p_Value {
    var retv1035 _goml_m_Option____std_p_json_p_Value
    var jp1037 _goml_m_Option____std_p_json_p_Value
    switch value__77.(type) {
    case Object:
        var x120 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__77.(Object)._0
        var fields__79 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x120
        var result__80 *ref__goml_m_Option____std_p_json_p_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(_goml_m_Option____std_p_json_p_Value_None{})
        var for_source125 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = fields__79
        var for_limit126 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source125)
        var for_index127 int = 0
        Loop_loop1040:
        for {
            var t1041 bool = for_index127 < for_limit126
            if t1041 {
                var for_item128 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source125, for_index127)
                var t1042 int = for_index127 + 1
                for_index127 = t1042
                var field__81 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item128
                var t1044 string = field__81._0
                var t1045 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1044, name__78)
                if t1045 {
                    var t1046 _goml_m_std_p_json_p_Value = field__81._1
                    var t1047 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1046,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(result__80, t1047)
                } else {}
                continue
            } else {
                break Loop_loop1040
            }
        }
        var t1039 _goml_m_Option____std_p_json_p_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(result__80)
        jp1037 = t1039
        retv1035 = jp1037
        return retv1035
    default:
        jp1037 = _goml_m_Option____std_p_json_p_Value_None{}
        retv1035 = jp1037
        return retv1035
    }
}

func _goml_m_std_p_json_p_as__string(value__82 _goml_m_std_p_json_p_Value) Option__string {
    var retv1050 Option__string
    var jp1052 Option__string
    switch value__82.(type) {
    case String:
        var x133 string = value__82.(String)._0
        var text__83 string = x133
        var t1053 Option__string = Option__string_Some{
            _0: text__83,
        }
        jp1052 = t1053
    default:
        jp1052 = Option__string_None{}
    }
    retv1050 = jp1052
    return retv1050
}

func _goml_m_std_p_json_p_parse__json__int__text(value__84 string) Option__int {
    var retv1055 Option__int
    var t1058 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
    var t1059 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1058, 0)
    var jp1057 Option__int
    if t1059 {
        jp1057 = Option__int_None{}
        retv1055 = jp1057
        return retv1055
    } else {
        var t1060 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, 0)
        var negative__85 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1060, 45)
        var jp1062 int
        if negative__85 {
            jp1062 = 1
        } else {
            jp1062 = 0
        }
        var index__86 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(jp1062)
        var result__87 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var t1090 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
        var t1091 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1092 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1090, t1091)
        if t1092 {
            retv1055 = Option__int_None{}
            return retv1055
        } else {
            Loop_loop1071:
            for {
                var t1072 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                var t1073 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                var t1074 bool = t1072 < t1073
                if t1074 {
                    var t1075 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                    var byte__88 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1075)
                    var t1088 bool = byte__88 < 48
                    var jp1081 bool
                    if t1088 {
                        jp1081 = true
                    } else {
                        var t1089 bool = byte__88 > 57
                        jp1081 = t1089
                    }
                    if jp1081 {
                        retv1055 = Option__int_None{}
                        return retv1055
                    } else {
                        var t1082 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                        var t1083 int = t1082 * 10
                        var t1084 uint8 = byte__88 - 48
                        var t1085 int = int(uint8(t1084))
                        var t1086 int = t1083 + t1085
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__87, t1086)
                        var t1077 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                        var t1078 int = t1077 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__86, t1078)
                        continue
                    }
                } else {
                    break Loop_loop1071
                }
            }
            var jp1066 int
            if negative__85 {
                var t1068 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                var t1069 int = 0 - t1068
                jp1066 = t1069
            } else {
                var t1070 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                jp1066 = t1070
            }
            var t1067 Option__int = Option__int_Some{
                _0: jp1066,
            }
            jp1057 = t1067
            retv1055 = jp1057
            return retv1055
        }
    }
}

func _goml_m_std_p_json_p_as__int(value__89 _goml_m_std_p_json_p_Value) Option__int {
    var retv1094 Option__int
    var jp1096 Option__int
    switch value__89.(type) {
    case Number:
        var x142 string = value__89.(Number)._0
        var number__90 string = x142
        var t1097 Option__int = _goml_m_std_p_json_p_parse__json__int__text(number__90)
        jp1096 = t1097
    default:
        jp1096 = Option__int_None{}
    }
    retv1094 = jp1096
    return retv1094
}

func _goml_m_std_p_json_p_as__bool(value__91 _goml_m_std_p_json_p_Value) Option__bool {
    var retv1099 Option__bool
    var jp1101 Option__bool
    switch value__91.(type) {
    case Bool:
        var x148 bool = value__91.(Bool)._0
        var result__92 bool = x148
        var t1102 Option__bool = Option__bool_Some{
            _0: result__92,
        }
        jp1101 = t1102
    default:
        jp1101 = Option__bool_None{}
    }
    retv1099 = jp1101
    return retv1099
}

func main0() struct{} {
    var mtmp64 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1113 _goml_m_std_p_json_p_Value
    switch mtmp64.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x65 _goml_m_std_p_json_p_Value = mtmp64.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__0 _goml_m_std_p_json_p_Value = x65
        jp1113 = value__0
        var value__2 _goml_m_std_p_json_p_Value = jp1113
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
        var t1117 string = _goml_m_std_p_json_p_encode(value__2)
        println__T_string(t1117)
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string() *_goml_vec_string {
    var retv1175 *_goml_vec_string
    var t1176 *_goml_vec_string = vec_new__Vec_6string()
    retv1175 = t1176
    return retv1175
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv1178 *ref_int_x
    var t1179 *ref_int_x = ref__Ref_3int(value__209)
    retv1178 = t1179
    return retv1178
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv1184 int
    var t1185 int = ref_get__Ref_3int(self__210)
    retv1184 = t1185
    return retv1184
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv1187 bool
    var t1188 bool = self__59 == other__60
    retv1187 = t1188
    return retv1187
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__128 *_goml_vec_string, elem__129 string) struct{} {
    vec_push__Vec_6string(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv1192 int
    var t1193 int = _goml_runtime_core_string_len(self__9)
    retv1192 = t1193
    return retv1192
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv1197 string
    var t1198 string = _goml_runtime_core_char_to_string(self__7)
    retv1197 = t1198
    return retv1197
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv1202 string
    var t1203 string = _goml_runtime_core_int_to_string(self__5)
    retv1202 = t1203
    return retv1202
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv1205 bool
    var t1206 bool = self__69 == other__70
    retv1205 = t1206
    return retv1205
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv1208 uint8
    var t1209 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv1208 = t1209
    return retv1208
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__209 uint32) *ref_uint32_x {
    var retv1211 *ref_uint32_x
    var t1212 *ref_uint32_x = ref__Ref_6uint32(value__209)
    retv1211 = t1212
    return retv1211
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__210 *ref_uint32_x) uint32 {
    var retv1214 uint32
    var t1215 uint32 = ref_get__Ref_6uint32(self__210)
    retv1214 = t1215
    return retv1214
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(self__211 *ref_uint32_x, value__212 uint32) struct{} {
    ref_set__Ref_6uint32(self__211, value__212)
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv1219 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp1221 Option__char
    if valid__3 {
        var t1222 Option__char = Option__char_Some{
            _0: value__4,
        }
        jp1221 = t1222
    } else {
        jp1221 = Option__char_None{}
    }
    retv1219 = jp1221
    return retv1219
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv1224 string
    var t1225 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv1224 = t1225
    return retv1224
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv1227 bool
    var t1228 bool = self__55 == other__56
    retv1227 = t1228
    return retv1227
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var retv1230 *_goml_vec__goml_m_std_p_json_p_Value
    var t1231 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    retv1230 = t1231
    return retv1230
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__128 *_goml_vec__goml_m_std_p_json_p_Value, elem__129 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var retv1235 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
    var t1236 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    retv1235 = t1236
    return retv1235
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__128 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__129 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv1240 rune
    var t1241 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv1240 = t1241
    return retv1240
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(value__209 _goml_m_Option____std_p_json_p_Value) *ref__goml_m_Option____std_p_json_p_Value_x {
    var retv1243 *ref__goml_m_Option____std_p_json_p_Value_x
    var t1244 *ref__goml_m_Option____std_p_json_p_Value_x = ref___goml_m_Ref__24Option____std_p_json_p_Value(value__209)
    retv1243 = t1244
    return retv1243
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(self__211 *ref__goml_m_Option____std_p_json_p_Value_x, value__212 _goml_m_Option____std_p_json_p_Value) struct{} {
    ref_set___goml_m_Ref__24Option____std_p_json_p_Value(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(self__210 *ref__goml_m_Option____std_p_json_p_Value_x) _goml_m_Option____std_p_json_p_Value {
    var retv1248 _goml_m_Option____std_p_json_p_Value
    var t1249 _goml_m_Option____std_p_json_p_Value = ref_get___goml_m_Ref__24Option____std_p_json_p_Value(self__210)
    retv1248 = t1249
    return retv1248
}

func println__T_string(value__1 string) struct{} {
    var t1251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t1251)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t1254 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t1254)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t1257 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t1257)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv1262 string
    retv1262 = self__38
    return retv1262
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv1264 string
    var t1265 string = _goml_runtime_core_int_to_string(self__40)
    retv1264 = t1265
    return retv1264
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv1267 string
    var t1268 string = _goml_runtime_core_bool_to_string(self__37)
    retv1267 = t1268
    return retv1267
}

func main() {
    main0()
}
