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

type FnIterator__int struct {
    next_fn func() Option__int
}

type _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_ struct {
    next_fn func() _goml_m_Option_____o_string_c_std_p_json_p_Value_q_
}

type _goml_m_FnIterator____std_p_json_p_Value struct {
    next_fn func() _goml_m_Option____std_p_json_p_Value
}

type closure_env_goml_builtin_range_0 struct {
    current_0 *ref_int_x
    end_1 int
}

type closure_env_inherent_Vec_Vec_T_iter_T_string_std_json_Value_1 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
}

type closure_env_inherent_Vec_Vec_T_iter_T_std_json_Value_2 struct {
    index_0 *ref_int_x
    len_1 int
    self_2 *_goml_vec__goml_m_std_p_json_p_Value
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

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

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

type _goml_m_Option_____o_string_c_std_p_json_p_Value_q_ interface {
    is_goml_m_Option_____o_string_c_std_p_json_p_Value_q_()
}

type _goml_m_Option_____o_string_c_std_p_json_p_Value_q__None struct {}

func (_ _goml_m_Option_____o_string_c_std_p_json_p_Value_q__None) is_goml_m_Option_____o_string_c_std_p_json_p_Value_q_() {}

type _goml_m_Option_____o_string_c_std_p_json_p_Value_q__Some struct {
    _0 Tuple2_6string_26_goml_m_std_p_json_p_Value
}

func (_ _goml_m_Option_____o_string_c_std_p_json_p_Value_q__Some) is_goml_m_Option_____o_string_c_std_p_json_p_Value_q_() {}

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
    var retv151 _goml_m_std_p_text_p_StringBuilder
    var t152 *_goml_vec_string = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__string()
    var t153 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t154 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        parts: t152,
        byte_len: t153,
    }
    retv151 = t154
    return retv151
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var t170 *_goml_vec_string = self__3.parts
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(t170, value__4)
    var t171 *ref_int_x = self__3.byte_len
    var t172 *ref_int_x = self__3.byte_len
    var t173 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t172)
    var t174 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    var t175 int = t173 + t174
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t171, t175)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__5 _goml_m_std_p_text_p_StringBuilder, value__6 rune) struct{} {
    var t178 string = _goml_m_inherent_i_char_i_char_i_to__string(value__6)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__5, t178)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(self__14 _goml_m_std_p_text_p_StringBuilder) string {
    var retv193 string
    var t194 *_goml_vec_string = self__14.parts
    var t195 string = _goml_runtime_core_string_concat(t194)
    retv193 = t195
    return retv193
}

func _goml_m_std_p_json_p_json__parser(input__0 string) _goml_m_std_p_json_p_JsonParser {
    var retv200 _goml_m_std_p_json_p_JsonParser
    var t201 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t202 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__0,
        index: t201,
    }
    retv200 = t202
    return retv200
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var retv204 string
    var t205 string = message__2 + " at byte "
    var t206 *ref_int_x = value__1.index
    var t207 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t206)
    var t208 string = _goml_m_inherent_i_int_i_int_i_to__string(t207)
    var t209 string = t205 + t208
    retv204 = t209
    return retv204
}

func _goml_m_std_p_json_p_json__whitespace(value__3 uint8) bool {
    var retv211 bool
    var t220 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 9)
    var jp218 bool
    if t220 {
        jp218 = true
    } else {
        var t221 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 10)
        jp218 = t221
    }
    var jp215 bool
    if jp218 {
        jp215 = true
    } else {
        var t219 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 13)
        jp215 = t219
    }
    var jp213 bool
    if jp215 {
        jp213 = true
    } else {
        var t216 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 32)
        jp213 = t216
    }
    retv211 = jp213
    return retv211
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop224:
    for {
        var t232 *ref_int_x = value__4.index
        var t233 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t232)
        var t234 string = value__4.input
        var t235 int = _goml_m_inherent_i_string_i_string_i_byte__len(t234)
        var t236 bool = t233 < t235
        var jp226 bool
        if t236 {
            var t237 string = value__4.input
            var t238 *ref_int_x = value__4.index
            var t239 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t238)
            var t240 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t237, t239)
            var t241 bool = _goml_m_std_p_json_p_json__whitespace(t240)
            jp226 = t241
        } else {
            jp226 = false
        }
        if jp226 {
            var t227 *ref_int_x = value__4.index
            var t228 *ref_int_x = value__4.index
            var t229 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t228)
            var t230 int = t229 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t227, t230)
            continue
        } else {
            break Loop_loop224
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var retv243 Option__uint32
    var t269 bool = value__5 >= 48
    var jp247 bool
    if t269 {
        var t270 bool = value__5 <= 57
        jp247 = t270
    } else {
        jp247 = false
    }
    var jp245 Option__uint32
    if jp247 {
        var t248 uint8 = value__5 - 48
        var t249 uint32 = uint32(uint8(t248))
        var t250 Option__uint32 = Option__uint32_Some{
            _0: t249,
        }
        jp245 = t250
    } else {
        var t267 bool = value__5 >= 65
        var jp254 bool
        if t267 {
            var t268 bool = value__5 <= 70
            jp254 = t268
        } else {
            jp254 = false
        }
        var jp252 Option__uint32
        if jp254 {
            var t255 uint8 = value__5 - 55
            var t256 uint32 = uint32(uint8(t255))
            var t257 Option__uint32 = Option__uint32_Some{
                _0: t256,
            }
            jp252 = t257
        } else {
            var t265 bool = value__5 >= 97
            var jp261 bool
            if t265 {
                var t266 bool = value__5 <= 102
                jp261 = t266
            } else {
                jp261 = false
            }
            var jp259 Option__uint32
            if jp261 {
                var t262 uint8 = value__5 - 87
                var t263 uint32 = uint32(uint8(t262))
                var t264 Option__uint32 = Option__uint32_Some{
                    _0: t263,
                }
                jp259 = t264
            } else {
                jp259 = Option__uint32_None{}
            }
            jp252 = jp259
        }
        jp245 = jp252
    }
    retv243 = jp245
    return retv243
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var retv272 Result__uint32__string
    var t275 *ref_int_x = value__6.index
    var t276 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t275)
    var t277 int = t276 + 4
    var t278 string = value__6.input
    var t279 int = _goml_m_inherent_i_string_i_string_i_byte__len(t278)
    var t280 bool = t277 > t279
    var jp274 Result__uint32__string
    if t280 {
        var t281 string = _goml_m_std_p_json_p_json__error(value__6, "incomplete unicode escape")
        var t282 Result__uint32__string = Result__uint32__string_Err{
            _0: t281,
        }
        jp274 = t282
        retv272 = jp274
        return retv272
    } else {
        var t283_source int = 0
        var t283 uint32 = uint32(int(t283_source))
        var result__7 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(t283)
        var t284 FnIterator__int = _goml_m_range(0, 4)
        var for_iter0 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t284)
        Loop_loop292:
        for {
            if true {
                var for_next1 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter0)
                switch for_next1.(type) {
                case Option__int_None:
                    break Loop_loop292
                case Option__int_Some:
                    var x2 int = for_next1.(Option__int_Some)._0
                    var offset__8 int = x2
                    var t294 string = value__6.input
                    var t295 *ref_int_x = value__6.index
                    var t296 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t295)
                    var t297 int = t296 + offset__8
                    var t298 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t294, t297)
                    var mtmp3 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t298)
                    switch mtmp3.(type) {
                    case Option__uint32_None:
                        var t300 string = _goml_m_std_p_json_p_json__error(value__6, "invalid unicode escape")
                        var t301 Result__uint32__string = Result__uint32__string_Err{
                            _0: t300,
                        }
                        retv272 = t301
                        return retv272
                    case Option__uint32_Some:
                        var x4 uint32 = mtmp3.(Option__uint32_Some)._0
                        var digit__9 uint32 = x4
                        var t302 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
                        var t303 uint32 = t302 * 16
                        var t304 uint32 = t303 + digit__9
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(result__7, t304)
                        continue
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop292
            }
        }
        var t286 *ref_int_x = value__6.index
        var t287 *ref_int_x = value__6.index
        var t288 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t287)
        var t289 int = t288 + 4
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t286, t289)
        var t290 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
        var t291 Result__uint32__string = Result__uint32__string_Ok{
            _0: t290,
        }
        jp274 = t291
        retv272 = jp274
        return retv272
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var retv307 Result__unit__string
    var mtmp7 Option__char = char_from_uint32(codepoint__12)
    var jp309 Result__unit__string
    switch mtmp7.(type) {
    case Option__char_None:
        var t310 string = _goml_m_std_p_json_p_json__error(value__10, "invalid unicode codepoint")
        var t311 Result__unit__string = Result__unit__string_Err{
            _0: t310,
        }
        jp309 = t311
    case Option__char_Some:
        var x8 rune = mtmp7.(Option__char_Some)._0
        var character__13 rune = x8
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__11, character__13)
        var t312 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp309 = t312
    default:
        panic("non-exhaustive match")
    }
    retv307 = jp309
    return retv307
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var retv314 Result__unit__string
    var mtmp10 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp316 uint32
    switch mtmp10.(type) {
    case Result__uint32__string_Ok:
        var x11 uint32 = mtmp10.(Result__uint32__string_Ok)._0
        var codepoint__16 uint32 = x11
        jp316 = codepoint__16
        var first__18 uint32 = jp316
        var t378 bool = first__18 >= 55296
        var jp320 bool
        if t378 {
            var t379 bool = first__18 <= 56319
            jp320 = t379
        } else {
            jp320 = false
        }
        var jp318 Result__unit__string
        if jp320 {
            var t357 *ref_int_x = value__14.index
            var t358 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t357)
            var t359 int = t358 + 2
            var t360 string = value__14.input
            var t361 int = _goml_m_inherent_i_string_i_string_i_byte__len(t360)
            var t362 bool = t359 > t361
            var jp349 bool
            if t362 {
                jp349 = true
            } else {
                var t363 string = value__14.input
                var t364 *ref_int_x = value__14.index
                var t365 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t364)
                var t366 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t363, t365)
                var t367 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t366, 92)
                var t368 bool = !t367
                jp349 = t368
            }
            var jp324 bool
            if jp349 {
                jp324 = true
            } else {
                var t350 string = value__14.input
                var t351 *ref_int_x = value__14.index
                var t352 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t351)
                var t353 int = t352 + 1
                var t354 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t350, t353)
                var t355 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t354, 117)
                var t356 bool = !t355
                jp324 = t356
            }
            var jp322 Result__unit__string
            if jp324 {
                var t325 string = _goml_m_std_p_json_p_json__error(value__14, "missing low surrogate")
                var t326 Result__unit__string = Result__unit__string_Err{
                    _0: t325,
                }
                jp322 = t326
                jp318 = jp322
                retv314 = jp318
                return retv314
            } else {
                var t327 *ref_int_x = value__14.index
                var t328 *ref_int_x = value__14.index
                var t329 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t328)
                var t330 int = t329 + 2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t327, t330)
                var mtmp14 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp332 uint32
                switch mtmp14.(type) {
                case Result__uint32__string_Ok:
                    var x15 uint32 = mtmp14.(Result__uint32__string_Ok)._0
                    var codepoint__19 uint32 = x15
                    jp332 = codepoint__19
                    var second__21 uint32 = jp332
                    var t345 bool = second__21 < 56320
                    var jp336 bool
                    if t345 {
                        jp336 = true
                    } else {
                        var t346 bool = second__21 > 57343
                        jp336 = t346
                    }
                    var jp334 Result__unit__string
                    if jp336 {
                        var t337 string = _goml_m_std_p_json_p_json__error(value__14, "invalid low surrogate")
                        var t338 Result__unit__string = Result__unit__string_Err{
                            _0: t337,
                        }
                        jp334 = t338
                    } else {
                        var t339 uint32 = first__18 - 55296
                        var t340 uint32 = t339 * 1024
                        var t341 uint32 = 65536 + t340
                        var t342 uint32 = t341 + second__21
                        var t343 uint32 = t342 - 56320
                        var t344 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, t343)
                        jp334 = t344
                    }
                    jp322 = jp334
                    jp318 = jp322
                    retv314 = jp318
                    return retv314
                case Result__uint32__string_Err:
                    var x16 string = mtmp14.(Result__uint32__string_Err)._0
                    var error__20 string = x16
                    var t347 Result__unit__string = Result__unit__string_Err{
                        _0: error__20,
                    }
                    retv314 = t347
                    return retv314
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t376 bool = first__18 >= 56320
            var jp372 bool
            if t376 {
                var t377 bool = first__18 <= 57343
                jp372 = t377
            } else {
                jp372 = false
            }
            var jp370 Result__unit__string
            if jp372 {
                var t373 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t374 Result__unit__string = Result__unit__string_Err{
                    _0: t373,
                }
                jp370 = t374
            } else {
                var t375 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, first__18)
                jp370 = t375
            }
            jp318 = jp370
            retv314 = jp318
            return retv314
        }
    case Result__uint32__string_Err:
        var x12 string = mtmp10.(Result__uint32__string_Err)._0
        var error__17 string = x12
        var t380 Result__unit__string = Result__unit__string_Err{
            _0: error__17,
        }
        retv314 = t380
        return retv314
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__22 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var retv382 Result__string__string
    var t503 *ref_int_x = value__22.index
    var t504 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t503)
    var t505 string = value__22.input
    var t506 int = _goml_m_inherent_i_string_i_string_i_byte__len(t505)
    var t507 bool = t504 >= t506
    var jp495 bool
    if t507 {
        jp495 = true
    } else {
        var t508 string = value__22.input
        var t509 *ref_int_x = value__22.index
        var t510 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t509)
        var t511 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t508, t510)
        var t512 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t511, 34)
        var t513 bool = !t512
        jp495 = t513
    }
    if jp495 {
        var t496 string = _goml_m_std_p_json_p_json__error(value__22, "expected string")
        var t497 Result__string__string = Result__string__string_Err{
            _0: t496,
        }
        retv382 = t497
        return retv382
    } else {
        var t498 *ref_int_x = value__22.index
        var t499 *ref_int_x = value__22.index
        var t500 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t499)
        var t501 int = t500 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t498, t501)
        var builder__23 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t384 *ref_int_x = value__22.index
        var t385 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t384)
        var segment__24 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t385)
        Loop_loop389:
        for {
            var t390 *ref_int_x = value__22.index
            var t391 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t390)
            var t392 string = value__22.input
            var t393 int = _goml_m_inherent_i_string_i_string_i_byte__len(t392)
            var t394 bool = t391 < t393
            if t394 {
                var t395 string = value__22.input
                var t396 *ref_int_x = value__22.index
                var t397 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t396)
                var byte__25 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t395, t397)
                var t399 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 34)
                if t399 {
                    var t407 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                    var t408 *ref_int_x = value__22.index
                    var t409 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t408)
                    var t410 bool = t407 < t409
                    if t410 {
                        var t411 string = value__22.input
                        var t412 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t413 *ref_int_x = value__22.index
                        var t414 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t413)
                        var t415 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t411, t412, t414)
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t415)
                    } else {}
                    var t401 *ref_int_x = value__22.index
                    var t402 *ref_int_x = value__22.index
                    var t403 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t402)
                    var t404 int = t403 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t401, t404)
                    var t405 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__23)
                    var t406 Result__string__string = Result__string__string_Ok{
                        _0: t405,
                    }
                    retv382 = t406
                    return retv382
                } else {
                    var t418 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 92)
                    if t418 {
                        var t475 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t476 *ref_int_x = value__22.index
                        var t477 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t476)
                        var t478 bool = t475 < t477
                        if t478 {
                            var t479 string = value__22.input
                            var t480 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                            var t481 *ref_int_x = value__22.index
                            var t482 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t481)
                            var t483 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t479, t480, t482)
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t483)
                        } else {}
                        var t420 *ref_int_x = value__22.index
                        var t421 *ref_int_x = value__22.index
                        var t422 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t421)
                        var t423 int = t422 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t420, t423)
                        var t468 *ref_int_x = value__22.index
                        var t469 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t468)
                        var t470 string = value__22.input
                        var t471 int = _goml_m_inherent_i_string_i_string_i_byte__len(t470)
                        var t472 bool = t469 >= t471
                        if t472 {
                            var t473 string = _goml_m_std_p_json_p_json__error(value__22, "incomplete escape")
                            var t474 Result__string__string = Result__string__string_Err{
                                _0: t473,
                            }
                            retv382 = t474
                            return retv382
                        } else {
                            var t425 string = value__22.input
                            var t426 *ref_int_x = value__22.index
                            var t427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t426)
                            var escape__26 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t425, t427)
                            var t428 *ref_int_x = value__22.index
                            var t429 *ref_int_x = value__22.index
                            var t430 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t429)
                            var t431 int = t430 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t428, t431)
                            var t436 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 34)
                            if t436 {
                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 34)
                                var t433 *ref_int_x = value__22.index
                                var t434 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t433)
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t434)
                                continue
                            } else {
                                var t439 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 92)
                                if t439 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 92)
                                    var t433 *ref_int_x = value__22.index
                                    var t434 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t433)
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t434)
                                    continue
                                } else {
                                    var t442 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 47)
                                    if t442 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 47)
                                        var t433 *ref_int_x = value__22.index
                                        var t434 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t433)
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t434)
                                        continue
                                    } else {
                                        var t445 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 98)
                                        if t445 {
                                            var mtmp24 Option__char = char_from_uint32(8)
                                            switch mtmp24.(type) {
                                            case Option__char_None:
                                            case Option__char_Some:
                                                var x25 rune = mtmp24.(Option__char_Some)._0
                                                var character__27 rune = x25
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, character__27)
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                            var t433 *ref_int_x = value__22.index
                                            var t434 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t433)
                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t434)
                                            continue
                                        } else {
                                            var t449 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 102)
                                            if t449 {
                                                var mtmp26 Option__char = char_from_uint32(12)
                                                switch mtmp26.(type) {
                                                case Option__char_None:
                                                case Option__char_Some:
                                                    var x27 rune = mtmp26.(Option__char_Some)._0
                                                    var character__28 rune = x27
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, character__28)
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                                var t433 *ref_int_x = value__22.index
                                                var t434 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t433)
                                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t434)
                                                continue
                                            } else {
                                                var t453 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 110)
                                                if t453 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 10)
                                                    var t433 *ref_int_x = value__22.index
                                                    var t434 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t433)
                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t434)
                                                    continue
                                                } else {
                                                    var t456 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 114)
                                                    if t456 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 13)
                                                        var t433 *ref_int_x = value__22.index
                                                        var t434 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t433)
                                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t434)
                                                        continue
                                                    } else {
                                                        var t459 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 116)
                                                        if t459 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 9)
                                                            var t433 *ref_int_x = value__22.index
                                                            var t434 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t433)
                                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t434)
                                                            continue
                                                        } else {
                                                            var t462 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 117)
                                                            if t462 {
                                                                var mtmp28 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__22, builder__23)
                                                                switch mtmp28.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t433 *ref_int_x = value__22.index
                                                                    var t434 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t433)
                                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t434)
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x30 string = mtmp28.(Result__unit__string_Err)._0
                                                                    var error__29 string = x30
                                                                    var t465 Result__string__string = Result__string__string_Err{
                                                                        _0: error__29,
                                                                    }
                                                                    retv382 = t465
                                                                    return retv382
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t466 string = _goml_m_std_p_json_p_json__error(value__22, "invalid escape")
                                                                var t467 Result__string__string = Result__string__string_Err{
                                                                    _0: t466,
                                                                }
                                                                retv382 = t467
                                                                return retv382
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
                        var t486 bool = byte__25 < 32
                        if t486 {
                            var t487 string = _goml_m_std_p_json_p_json__error(value__22, "unescaped control character")
                            var t488 Result__string__string = Result__string__string_Err{
                                _0: t487,
                            }
                            retv382 = t488
                            return retv382
                        } else {
                            var t489 *ref_int_x = value__22.index
                            var t490 *ref_int_x = value__22.index
                            var t491 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t490)
                            var t492 int = t491 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t489, t492)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop389
            }
        }
        var t387 string = _goml_m_std_p_json_p_json__error(value__22, "unterminated string")
        var t388 Result__string__string = Result__string__string_Err{
            _0: t387,
        }
        retv382 = t388
        return retv382
    }
}

func _goml_m_std_p_json_p_json__digit(value__30 uint8) bool {
    var retv515 bool
    var t518 bool = value__30 >= 48
    var jp517 bool
    if t518 {
        var t519 bool = value__30 <= 57
        jp517 = t519
    } else {
        jp517 = false
    }
    retv515 = jp517
    return retv515
}

func _goml_m_std_p_json_p_parse__digits(value__31 _goml_m_std_p_json_p_JsonParser) bool {
    var retv521 bool
    var t522 *ref_int_x = value__31.index
    var start__32 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t522)
    Loop_loop527:
    for {
        var t535 *ref_int_x = value__31.index
        var t536 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t535)
        var t537 string = value__31.input
        var t538 int = _goml_m_inherent_i_string_i_string_i_byte__len(t537)
        var t539 bool = t536 < t538
        var jp529 bool
        if t539 {
            var t540 string = value__31.input
            var t541 *ref_int_x = value__31.index
            var t542 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t541)
            var t543 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t540, t542)
            var t544 bool = _goml_m_std_p_json_p_json__digit(t543)
            jp529 = t544
        } else {
            jp529 = false
        }
        if jp529 {
            var t530 *ref_int_x = value__31.index
            var t531 *ref_int_x = value__31.index
            var t532 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t531)
            var t533 int = t532 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t530, t533)
            continue
        } else {
            break Loop_loop527
        }
    }
    var t524 *ref_int_x = value__31.index
    var t525 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t524)
    var t526 bool = t525 > start__32
    retv521 = t526
    return retv521
}

func _goml_m_std_p_json_p_parse__json__number(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv546 _goml_m_Result____std_p_json_p_Value____string
    var t547 *ref_int_x = value__33.index
    var start__34 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t547)
    var t669 string = value__33.input
    var t670 *ref_int_x = value__33.index
    var t671 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t670)
    var t672 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t669, t671)
    var t673 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t672, 45)
    if t673 {
        var t674 *ref_int_x = value__33.index
        var t675 *ref_int_x = value__33.index
        var t676 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t675)
        var t677 int = t676 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t674, t677)
    } else {}
    var t632 *ref_int_x = value__33.index
    var t633 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t632)
    var t634 string = value__33.input
    var t635 int = _goml_m_inherent_i_string_i_string_i_byte__len(t634)
    var t636 bool = t633 >= t635
    if t636 {
        var t637 string = _goml_m_std_p_json_p_json__error(value__33, "incomplete number")
        var t638 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t637,
        }
        retv546 = t638
        return retv546
    } else {
        var t640 string = value__33.input
        var t641 *ref_int_x = value__33.index
        var t642 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t641)
        var t643 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t640, t642)
        var t644 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t643, 48)
        if t644 {
            var t645 *ref_int_x = value__33.index
            var t646 *ref_int_x = value__33.index
            var t647 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t646)
            var t648 int = t647 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t645, t648)
            var t654 *ref_int_x = value__33.index
            var t655 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t654)
            var t656 string = value__33.input
            var t657 int = _goml_m_inherent_i_string_i_string_i_byte__len(t656)
            var t658 bool = t655 < t657
            var jp651 bool
            if t658 {
                var t659 string = value__33.input
                var t660 *ref_int_x = value__33.index
                var t661 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t660)
                var t662 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t659, t661)
                var t663 bool = _goml_m_std_p_json_p_json__digit(t662)
                jp651 = t663
            } else {
                jp651 = false
            }
            if jp651 {
                var t652 string = _goml_m_std_p_json_p_json__error(value__33, "invalid leading zero")
                var t653 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t652,
                }
                retv546 = t653
                return retv546
            } else {
                var t622 *ref_int_x = value__33.index
                var t623 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t622)
                var t624 string = value__33.input
                var t625 int = _goml_m_inherent_i_string_i_string_i_byte__len(t624)
                var t626 bool = t623 < t625
                var jp612 bool
                if t626 {
                    var t627 string = value__33.input
                    var t628 *ref_int_x = value__33.index
                    var t629 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t628)
                    var t630 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t627, t629)
                    var t631 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t630, 46)
                    jp612 = t631
                } else {
                    jp612 = false
                }
                if jp612 {
                    var t613 *ref_int_x = value__33.index
                    var t614 *ref_int_x = value__33.index
                    var t615 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t614)
                    var t616 int = t615 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t613, t616)
                    var t618 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t619 bool = !t618
                    if t619 {
                        var t620 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t621 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t620,
                        }
                        retv546 = t621
                        return retv546
                    } else {
                        var t594 *ref_int_x = value__33.index
                        var t595 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t594)
                        var t596 string = value__33.input
                        var t597 int = _goml_m_inherent_i_string_i_string_i_byte__len(t596)
                        var t598 bool = t595 < t597
                        var jp559 bool
                        if t598 {
                            var t601 string = value__33.input
                            var t602 *ref_int_x = value__33.index
                            var t603 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t602)
                            var t604 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t601, t603)
                            var t605 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t604, 101)
                            var jp600 bool
                            if t605 {
                                jp600 = true
                            } else {
                                var t606 string = value__33.input
                                var t607 *ref_int_x = value__33.index
                                var t608 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t607)
                                var t609 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t606, t608)
                                var t610 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t609, 69)
                                jp600 = t610
                            }
                            jp559 = jp600
                        } else {
                            jp559 = false
                        }
                        if jp559 {
                            var t560 *ref_int_x = value__33.index
                            var t561 *ref_int_x = value__33.index
                            var t562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t561)
                            var t563 int = t562 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t560, t563)
                            var t577 *ref_int_x = value__33.index
                            var t578 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t577)
                            var t579 string = value__33.input
                            var t580 int = _goml_m_inherent_i_string_i_string_i_byte__len(t579)
                            var t581 bool = t578 < t580
                            var jp571 bool
                            if t581 {
                                var t584 string = value__33.input
                                var t585 *ref_int_x = value__33.index
                                var t586 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t585)
                                var t587 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t584, t586)
                                var t588 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t587, 43)
                                var jp583 bool
                                if t588 {
                                    jp583 = true
                                } else {
                                    var t589 string = value__33.input
                                    var t590 *ref_int_x = value__33.index
                                    var t591 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t590)
                                    var t592 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t589, t591)
                                    var t593 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t592, 45)
                                    jp583 = t593
                                }
                                jp571 = jp583
                            } else {
                                jp571 = false
                            }
                            if jp571 {
                                var t572 *ref_int_x = value__33.index
                                var t573 *ref_int_x = value__33.index
                                var t574 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t573)
                                var t575 int = t574 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t572, t575)
                            } else {}
                            var t566 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t567 bool = !t566
                            if t567 {
                                var t568 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t569 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t568,
                                }
                                retv546 = t569
                                return retv546
                            } else {
                                var t552 string = value__33.input
                                var t553 *ref_int_x = value__33.index
                                var t554 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t553)
                                var t555 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t552, start__34, t554)
                                var t556 _goml_m_std_p_json_p_Value = Number{
                                    _0: t555,
                                }
                                var t557 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t556,
                                }
                                retv546 = t557
                                return retv546
                            }
                        } else {
                            var t552 string = value__33.input
                            var t553 *ref_int_x = value__33.index
                            var t554 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t553)
                            var t555 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t552, start__34, t554)
                            var t556 _goml_m_std_p_json_p_Value = Number{
                                _0: t555,
                            }
                            var t557 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t556,
                            }
                            retv546 = t557
                            return retv546
                        }
                    }
                } else {
                    var t594 *ref_int_x = value__33.index
                    var t595 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t594)
                    var t596 string = value__33.input
                    var t597 int = _goml_m_inherent_i_string_i_string_i_byte__len(t596)
                    var t598 bool = t595 < t597
                    var jp559 bool
                    if t598 {
                        var t601 string = value__33.input
                        var t602 *ref_int_x = value__33.index
                        var t603 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t602)
                        var t604 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t601, t603)
                        var t605 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t604, 101)
                        var jp600 bool
                        if t605 {
                            jp600 = true
                        } else {
                            var t606 string = value__33.input
                            var t607 *ref_int_x = value__33.index
                            var t608 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t607)
                            var t609 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t606, t608)
                            var t610 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t609, 69)
                            jp600 = t610
                        }
                        jp559 = jp600
                    } else {
                        jp559 = false
                    }
                    if jp559 {
                        var t560 *ref_int_x = value__33.index
                        var t561 *ref_int_x = value__33.index
                        var t562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t561)
                        var t563 int = t562 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t560, t563)
                        var t577 *ref_int_x = value__33.index
                        var t578 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t577)
                        var t579 string = value__33.input
                        var t580 int = _goml_m_inherent_i_string_i_string_i_byte__len(t579)
                        var t581 bool = t578 < t580
                        var jp571 bool
                        if t581 {
                            var t584 string = value__33.input
                            var t585 *ref_int_x = value__33.index
                            var t586 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t585)
                            var t587 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t584, t586)
                            var t588 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t587, 43)
                            var jp583 bool
                            if t588 {
                                jp583 = true
                            } else {
                                var t589 string = value__33.input
                                var t590 *ref_int_x = value__33.index
                                var t591 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t590)
                                var t592 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t589, t591)
                                var t593 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t592, 45)
                                jp583 = t593
                            }
                            jp571 = jp583
                        } else {
                            jp571 = false
                        }
                        if jp571 {
                            var t572 *ref_int_x = value__33.index
                            var t573 *ref_int_x = value__33.index
                            var t574 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t573)
                            var t575 int = t574 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t572, t575)
                        } else {}
                        var t566 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t567 bool = !t566
                        if t567 {
                            var t568 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t569 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t568,
                            }
                            retv546 = t569
                            return retv546
                        } else {
                            var t552 string = value__33.input
                            var t553 *ref_int_x = value__33.index
                            var t554 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t553)
                            var t555 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t552, start__34, t554)
                            var t556 _goml_m_std_p_json_p_Value = Number{
                                _0: t555,
                            }
                            var t557 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t556,
                            }
                            retv546 = t557
                            return retv546
                        }
                    } else {
                        var t552 string = value__33.input
                        var t553 *ref_int_x = value__33.index
                        var t554 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t553)
                        var t555 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t552, start__34, t554)
                        var t556 _goml_m_std_p_json_p_Value = Number{
                            _0: t555,
                        }
                        var t557 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t556,
                        }
                        retv546 = t557
                        return retv546
                    }
                }
            }
        } else {
            var t665 bool = _goml_m_std_p_json_p_parse__digits(value__33)
            var t666 bool = !t665
            if t666 {
                var t667 string = _goml_m_std_p_json_p_json__error(value__33, "expected number")
                var t668 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t667,
                }
                retv546 = t668
                return retv546
            } else {
                var t622 *ref_int_x = value__33.index
                var t623 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t622)
                var t624 string = value__33.input
                var t625 int = _goml_m_inherent_i_string_i_string_i_byte__len(t624)
                var t626 bool = t623 < t625
                var jp612 bool
                if t626 {
                    var t627 string = value__33.input
                    var t628 *ref_int_x = value__33.index
                    var t629 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t628)
                    var t630 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t627, t629)
                    var t631 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t630, 46)
                    jp612 = t631
                } else {
                    jp612 = false
                }
                if jp612 {
                    var t613 *ref_int_x = value__33.index
                    var t614 *ref_int_x = value__33.index
                    var t615 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t614)
                    var t616 int = t615 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t613, t616)
                    var t618 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t619 bool = !t618
                    if t619 {
                        var t620 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t621 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t620,
                        }
                        retv546 = t621
                        return retv546
                    } else {
                        var t594 *ref_int_x = value__33.index
                        var t595 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t594)
                        var t596 string = value__33.input
                        var t597 int = _goml_m_inherent_i_string_i_string_i_byte__len(t596)
                        var t598 bool = t595 < t597
                        var jp559 bool
                        if t598 {
                            var t601 string = value__33.input
                            var t602 *ref_int_x = value__33.index
                            var t603 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t602)
                            var t604 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t601, t603)
                            var t605 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t604, 101)
                            var jp600 bool
                            if t605 {
                                jp600 = true
                            } else {
                                var t606 string = value__33.input
                                var t607 *ref_int_x = value__33.index
                                var t608 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t607)
                                var t609 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t606, t608)
                                var t610 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t609, 69)
                                jp600 = t610
                            }
                            jp559 = jp600
                        } else {
                            jp559 = false
                        }
                        if jp559 {
                            var t560 *ref_int_x = value__33.index
                            var t561 *ref_int_x = value__33.index
                            var t562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t561)
                            var t563 int = t562 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t560, t563)
                            var t577 *ref_int_x = value__33.index
                            var t578 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t577)
                            var t579 string = value__33.input
                            var t580 int = _goml_m_inherent_i_string_i_string_i_byte__len(t579)
                            var t581 bool = t578 < t580
                            var jp571 bool
                            if t581 {
                                var t584 string = value__33.input
                                var t585 *ref_int_x = value__33.index
                                var t586 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t585)
                                var t587 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t584, t586)
                                var t588 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t587, 43)
                                var jp583 bool
                                if t588 {
                                    jp583 = true
                                } else {
                                    var t589 string = value__33.input
                                    var t590 *ref_int_x = value__33.index
                                    var t591 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t590)
                                    var t592 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t589, t591)
                                    var t593 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t592, 45)
                                    jp583 = t593
                                }
                                jp571 = jp583
                            } else {
                                jp571 = false
                            }
                            if jp571 {
                                var t572 *ref_int_x = value__33.index
                                var t573 *ref_int_x = value__33.index
                                var t574 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t573)
                                var t575 int = t574 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t572, t575)
                            } else {}
                            var t566 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t567 bool = !t566
                            if t567 {
                                var t568 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t569 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t568,
                                }
                                retv546 = t569
                                return retv546
                            } else {
                                var t552 string = value__33.input
                                var t553 *ref_int_x = value__33.index
                                var t554 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t553)
                                var t555 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t552, start__34, t554)
                                var t556 _goml_m_std_p_json_p_Value = Number{
                                    _0: t555,
                                }
                                var t557 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t556,
                                }
                                retv546 = t557
                                return retv546
                            }
                        } else {
                            var t552 string = value__33.input
                            var t553 *ref_int_x = value__33.index
                            var t554 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t553)
                            var t555 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t552, start__34, t554)
                            var t556 _goml_m_std_p_json_p_Value = Number{
                                _0: t555,
                            }
                            var t557 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t556,
                            }
                            retv546 = t557
                            return retv546
                        }
                    }
                } else {
                    var t594 *ref_int_x = value__33.index
                    var t595 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t594)
                    var t596 string = value__33.input
                    var t597 int = _goml_m_inherent_i_string_i_string_i_byte__len(t596)
                    var t598 bool = t595 < t597
                    var jp559 bool
                    if t598 {
                        var t601 string = value__33.input
                        var t602 *ref_int_x = value__33.index
                        var t603 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t602)
                        var t604 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t601, t603)
                        var t605 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t604, 101)
                        var jp600 bool
                        if t605 {
                            jp600 = true
                        } else {
                            var t606 string = value__33.input
                            var t607 *ref_int_x = value__33.index
                            var t608 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t607)
                            var t609 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t606, t608)
                            var t610 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t609, 69)
                            jp600 = t610
                        }
                        jp559 = jp600
                    } else {
                        jp559 = false
                    }
                    if jp559 {
                        var t560 *ref_int_x = value__33.index
                        var t561 *ref_int_x = value__33.index
                        var t562 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t561)
                        var t563 int = t562 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t560, t563)
                        var t577 *ref_int_x = value__33.index
                        var t578 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t577)
                        var t579 string = value__33.input
                        var t580 int = _goml_m_inherent_i_string_i_string_i_byte__len(t579)
                        var t581 bool = t578 < t580
                        var jp571 bool
                        if t581 {
                            var t584 string = value__33.input
                            var t585 *ref_int_x = value__33.index
                            var t586 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t585)
                            var t587 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t584, t586)
                            var t588 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t587, 43)
                            var jp583 bool
                            if t588 {
                                jp583 = true
                            } else {
                                var t589 string = value__33.input
                                var t590 *ref_int_x = value__33.index
                                var t591 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t590)
                                var t592 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t589, t591)
                                var t593 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t592, 45)
                                jp583 = t593
                            }
                            jp571 = jp583
                        } else {
                            jp571 = false
                        }
                        if jp571 {
                            var t572 *ref_int_x = value__33.index
                            var t573 *ref_int_x = value__33.index
                            var t574 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t573)
                            var t575 int = t574 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t572, t575)
                        } else {}
                        var t566 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t567 bool = !t566
                        if t567 {
                            var t568 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t569 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t568,
                            }
                            retv546 = t569
                            return retv546
                        } else {
                            var t552 string = value__33.input
                            var t553 *ref_int_x = value__33.index
                            var t554 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t553)
                            var t555 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t552, start__34, t554)
                            var t556 _goml_m_std_p_json_p_Value = Number{
                                _0: t555,
                            }
                            var t557 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t556,
                            }
                            retv546 = t557
                            return retv546
                        }
                    } else {
                        var t552 string = value__33.input
                        var t553 *ref_int_x = value__33.index
                        var t554 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t553)
                        var t555 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t552, start__34, t554)
                        var t556 _goml_m_std_p_json_p_Value = Number{
                            _0: t555,
                        }
                        var t557 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t556,
                        }
                        retv546 = t557
                        return retv546
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__35 _goml_m_std_p_json_p_JsonParser, expected__36 string, result__37 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var retv680 _goml_m_Result____std_p_json_p_Value____string
    var t693 *ref_int_x = value__35.index
    var t694 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t693)
    var t695 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
    var t696 int = t694 + t695
    var t697 string = value__35.input
    var t698 int = _goml_m_inherent_i_string_i_string_i_byte__len(t697)
    var t699 bool = t696 <= t698
    var jp684 bool
    if t699 {
        var t700 string = value__35.input
        var t701 *ref_int_x = value__35.index
        var t702 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t701)
        var t703 *ref_int_x = value__35.index
        var t704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t703)
        var t705 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t706 int = t704 + t705
        var t707 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t700, t702, t706)
        var t708 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t707, expected__36)
        jp684 = t708
    } else {
        jp684 = false
    }
    var jp682 _goml_m_Result____std_p_json_p_Value____string
    if jp684 {
        var t685 *ref_int_x = value__35.index
        var t686 *ref_int_x = value__35.index
        var t687 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t686)
        var t688 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t689 int = t687 + t688
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t685, t689)
        var t690 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__37,
        }
        jp682 = t690
    } else {
        var t691 string = _goml_m_std_p_json_p_json__error(value__35, "invalid literal")
        var t692 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t691,
        }
        jp682 = t692
    }
    retv680 = jp682
    return retv680
}

func _goml_m_std_p_json_p_parse__json__array(value__38 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv710 _goml_m_Result____std_p_json_p_Value____string
    var t711 *ref_int_x = value__38.index
    var t712 *ref_int_x = value__38.index
    var t713 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t712)
    var t714 int = t713 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t711, t714)
    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
    var result__39 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t769 *ref_int_x = value__38.index
    var t770 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t769)
    var t771 string = value__38.input
    var t772 int = _goml_m_inherent_i_string_i_string_i_byte__len(t771)
    var t773 bool = t770 < t772
    var jp762 bool
    if t773 {
        var t774 string = value__38.input
        var t775 *ref_int_x = value__38.index
        var t776 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t775)
        var t777 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t774, t776)
        var t778 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t777, 93)
        jp762 = t778
    } else {
        jp762 = false
    }
    if jp762 {
        var t763 *ref_int_x = value__38.index
        var t764 *ref_int_x = value__38.index
        var t765 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t764)
        var t766 int = t765 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t763, t766)
        var t767 _goml_m_std_p_json_p_Value = Array{
            _0: result__39,
        }
        var t768 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t767,
        }
        retv710 = t768
        return retv710
    } else {
        Loop_loop719:
        for {
            var t720 *ref_int_x = value__38.index
            var t721 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t720)
            var t722 string = value__38.input
            var t723 int = _goml_m_inherent_i_string_i_string_i_byte__len(t722)
            var t724 bool = t721 < t723
            if t724 {
                var mtmp47 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__38)
                switch mtmp47.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x48 _goml_m_std_p_json_p_Value = mtmp47.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    var item__40 _goml_m_std_p_json_p_Value = x48
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(result__39, item__40)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                    var t727 *ref_int_x = value__38.index
                    var t728 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t727)
                    var t729 string = value__38.input
                    var t730 int = _goml_m_inherent_i_string_i_string_i_byte__len(t729)
                    var t731 bool = t728 >= t730
                    if t731 {
                        var t732 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
                        var t733 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t732,
                        }
                        retv710 = t733
                        return retv710
                    } else {
                        var t735 string = value__38.input
                        var t736 *ref_int_x = value__38.index
                        var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                        var t738 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t735, t737)
                        var t739 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t738, 93)
                        if t739 {
                            var t740 *ref_int_x = value__38.index
                            var t741 *ref_int_x = value__38.index
                            var t742 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t741)
                            var t743 int = t742 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t740, t743)
                            var t744 _goml_m_std_p_json_p_Value = Array{
                                _0: result__39,
                            }
                            var t745 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t744,
                            }
                            retv710 = t745
                            return retv710
                        } else {
                            var t747 string = value__38.input
                            var t748 *ref_int_x = value__38.index
                            var t749 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t748)
                            var t750 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t747, t749)
                            var t751 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t750, 44)
                            if t751 {
                                var t752 *ref_int_x = value__38.index
                                var t753 *ref_int_x = value__38.index
                                var t754 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t753)
                                var t755 int = t754 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t752, t755)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                                continue
                            } else {
                                var t757 string = _goml_m_std_p_json_p_json__error(value__38, "expected array separator")
                                var t758 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t757,
                                }
                                retv710 = t758
                                return retv710
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x49 string = mtmp47.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var error__41 string = x49
                    var t760 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__41,
                    }
                    retv710 = t760
                    return retv710
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop719
            }
        }
        var t717 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
        var t718 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t717,
        }
        retv710 = t718
        return retv710
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__42 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv780 _goml_m_Result____std_p_json_p_Value____string
    var t781 *ref_int_x = value__42.index
    var t782 *ref_int_x = value__42.index
    var t783 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t782)
    var t784 int = t783 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t781, t784)
    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
    var result__43 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t864 *ref_int_x = value__42.index
    var t865 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t864)
    var t866 string = value__42.input
    var t867 int = _goml_m_inherent_i_string_i_string_i_byte__len(t866)
    var t868 bool = t865 < t867
    var jp857 bool
    if t868 {
        var t869 string = value__42.input
        var t870 *ref_int_x = value__42.index
        var t871 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t870)
        var t872 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t869, t871)
        var t873 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t872, 125)
        jp857 = t873
    } else {
        jp857 = false
    }
    if jp857 {
        var t858 *ref_int_x = value__42.index
        var t859 *ref_int_x = value__42.index
        var t860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t859)
        var t861 int = t860 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t858, t861)
        var t862 _goml_m_std_p_json_p_Value = Object{
            _0: result__43,
        }
        var t863 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t862,
        }
        retv780 = t863
        return retv780
    } else {
        Loop_loop789:
        for {
            var t790 *ref_int_x = value__42.index
            var t791 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t790)
            var t792 string = value__42.input
            var t793 int = _goml_m_inherent_i_string_i_string_i_byte__len(t792)
            var t794 bool = t791 < t793
            if t794 {
                var mtmp59 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__42)
                var jp796 string
                switch mtmp59.(type) {
                case Result__string__string_Ok:
                    var x60 string = mtmp59.(Result__string__string_Ok)._0
                    var name__44 string = x60
                    jp796 = name__44
                    var name__46 string = jp796
                    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                    var t844 *ref_int_x = value__42.index
                    var t845 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t844)
                    var t846 string = value__42.input
                    var t847 int = _goml_m_inherent_i_string_i_string_i_byte__len(t846)
                    var t848 bool = t845 >= t847
                    var jp836 bool
                    if t848 {
                        jp836 = true
                    } else {
                        var t849 string = value__42.input
                        var t850 *ref_int_x = value__42.index
                        var t851 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t850)
                        var t852 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t849, t851)
                        var t853 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t852, 58)
                        var t854 bool = !t853
                        jp836 = t854
                    }
                    if jp836 {
                        var t837 string = _goml_m_std_p_json_p_json__error(value__42, "expected object colon")
                        var t838 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t837,
                        }
                        retv780 = t838
                        return retv780
                    } else {
                        var t839 *ref_int_x = value__42.index
                        var t840 *ref_int_x = value__42.index
                        var t841 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t840)
                        var t842 int = t841 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t839, t842)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                        var mtmp65 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__42)
                        switch mtmp65.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x66 _goml_m_std_p_json_p_Value = mtmp65.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            var item__47 _goml_m_std_p_json_p_Value = x66
                            var t832 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: name__46,
                                _1: item__47,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__43, t832)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                            var t800 *ref_int_x = value__42.index
                            var t801 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t800)
                            var t802 string = value__42.input
                            var t803 int = _goml_m_inherent_i_string_i_string_i_byte__len(t802)
                            var t804 bool = t801 >= t803
                            if t804 {
                                var t805 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
                                var t806 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t805,
                                }
                                retv780 = t806
                                return retv780
                            } else {
                                var t808 string = value__42.input
                                var t809 *ref_int_x = value__42.index
                                var t810 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t809)
                                var t811 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t808, t810)
                                var t812 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t811, 125)
                                if t812 {
                                    var t813 *ref_int_x = value__42.index
                                    var t814 *ref_int_x = value__42.index
                                    var t815 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t814)
                                    var t816 int = t815 + 1
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t813, t816)
                                    var t817 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__43,
                                    }
                                    var t818 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t817,
                                    }
                                    retv780 = t818
                                    return retv780
                                } else {
                                    var t820 string = value__42.input
                                    var t821 *ref_int_x = value__42.index
                                    var t822 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t821)
                                    var t823 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t820, t822)
                                    var t824 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t823, 44)
                                    if t824 {
                                        var t825 *ref_int_x = value__42.index
                                        var t826 *ref_int_x = value__42.index
                                        var t827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t826)
                                        var t828 int = t827 + 1
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t825, t828)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                                        continue
                                    } else {
                                        var t830 string = _goml_m_std_p_json_p_json__error(value__42, "expected object separator")
                                        var t831 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t830,
                                        }
                                        retv780 = t831
                                        return retv780
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x67 string = mtmp65.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var error__48 string = x67
                            var t834 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: error__48,
                            }
                            retv780 = t834
                            return retv780
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x61 string = mtmp59.(Result__string__string_Err)._0
                    var error__45 string = x61
                    var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__45,
                    }
                    retv780 = t855
                    return retv780
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop789
            }
        }
        var t787 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
        var t788 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t787,
        }
        retv780 = t788
        return retv780
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__49 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv875 _goml_m_Result____std_p_json_p_Value____string
    _goml_m_std_p_json_p_skip__json__whitespace(value__49)
    var t878 *ref_int_x = value__49.index
    var t879 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t878)
    var t880 string = value__49.input
    var t881 int = _goml_m_inherent_i_string_i_string_i_byte__len(t880)
    var t882 bool = t879 >= t881
    var jp877 _goml_m_Result____std_p_json_p_Value____string
    if t882 {
        var t883 string = _goml_m_std_p_json_p_json__error(value__49, "expected JSON value")
        var t884 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t883,
        }
        jp877 = t884
    } else {
        var t885 string = value__49.input
        var t886 *ref_int_x = value__49.index
        var t887 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t886)
        var mtmp74 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t885, t887)
        var jp889 _goml_m_Result____std_p_json_p_Value____string
        switch mtmp74 {
        case 123:
            var t890 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__49)
            jp889 = t890
        case 91:
            var t891 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__49)
            jp889 = t891
        case 34:
            var mtmp75 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__49)
            var jp893 _goml_m_Result____std_p_json_p_Value____string
            switch mtmp75.(type) {
            case Result__string__string_Ok:
                var x76 string = mtmp75.(Result__string__string_Ok)._0
                var text__50 string = x76
                var t894 _goml_m_std_p_json_p_Value = String{
                    _0: text__50,
                }
                var t895 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t894,
                }
                jp893 = t895
            case Result__string__string_Err:
                var x77 string = mtmp75.(Result__string__string_Err)._0
                var error__51 string = x77
                var t896 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: error__51,
                }
                jp893 = t896
            default:
                panic("non-exhaustive match")
            }
            jp889 = jp893
        case 116:
            var t897 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t898 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "true", t897)
            jp889 = t898
        case 102:
            var t899 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t900 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "false", t899)
            jp889 = t900
        case 110:
            var t901 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "null", Null{})
            jp889 = t901
        default:
            var byte__52 uint8 = mtmp74
            var t909 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 45)
            var jp905 bool
            if t909 {
                jp905 = true
            } else {
                var t910 bool = _goml_m_std_p_json_p_json__digit(byte__52)
                jp905 = t910
            }
            var jp903 _goml_m_Result____std_p_json_p_Value____string
            if jp905 {
                var t906 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__49)
                jp903 = t906
            } else {
                var t907 string = _goml_m_std_p_json_p_json__error(value__49, "unexpected JSON token")
                var t908 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t907,
                }
                jp903 = t908
            }
            jp889 = jp903
        }
        jp877 = jp889
    }
    retv875 = jp877
    return retv875
}

func _goml_m_std_p_json_p_parse(input__53 string) _goml_m_Result____std_p_json_p_Value____string {
    var retv912 _goml_m_Result____std_p_json_p_Value____string
    var parser__54 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_json__parser(input__53)
    var mtmp78 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__54)
    var jp914 _goml_m_std_p_json_p_Value
    switch mtmp78.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x79 _goml_m_std_p_json_p_Value = mtmp78.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__55 _goml_m_std_p_json_p_Value = x79
        jp914 = value__55
        var result__57 _goml_m_std_p_json_p_Value = jp914
        _goml_m_std_p_json_p_skip__json__whitespace(parser__54)
        var t917 *ref_int_x = parser__54.index
        var t918 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t917)
        var t919 int = _goml_m_inherent_i_string_i_string_i_byte__len(input__53)
        var t920 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t918, t919)
        var jp916 _goml_m_Result____std_p_json_p_Value____string
        if t920 {
            var t921 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: result__57,
            }
            jp916 = t921
        } else {
            var t922 string = _goml_m_std_p_json_p_json__error(parser__54, "trailing JSON data")
            var t923 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t922,
            }
            jp916 = t923
        }
        retv912 = jp916
        return retv912
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x80 string = mtmp78.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var error__56 string = x80
        var t924 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: error__56,
        }
        retv912 = t924
        return retv912
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_json__hex__digit(value__58 uint8) rune {
    var retv926 rune
    var t927 int = int(uint8(value__58))
    var t928 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", t927)
    retv926 = t928
    return retv926
}

func _goml_m_std_p_json_p_write__json__string(builder__59 _goml_m_std_p_text_p_StringBuilder, value__60 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, 34)
    var start__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t930 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    var t931 FnIterator__int = _goml_m_range(0, t930)
    var for_iter83 FnIterator__int = _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(t931)
    Loop_loop942:
    for {
        if true {
            var for_next84 Option__int = _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(for_iter83)
            switch for_next84.(type) {
            case Option__int_None:
                break Loop_loop942
            case Option__int_Some:
                var x85 int = for_next84.(Option__int_Some)._0
                var index__62 int = x85
                var byte__63 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__60, index__62)
                var t999 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
                var jp997 bool
                if t999 {
                    jp997 = true
                } else {
                    var t1000 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                    jp997 = t1000
                }
                var jp994 bool
                if jp997 {
                    jp994 = true
                } else {
                    var t998 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                    jp994 = t998
                }
                var jp991 bool
                if jp994 {
                    jp991 = true
                } else {
                    var t995 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                    jp991 = t995
                }
                var jp988 bool
                if jp991 {
                    jp988 = true
                } else {
                    var t992 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                    jp988 = t992
                }
                var jp985 bool
                if jp988 {
                    jp985 = true
                } else {
                    var t989 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                    jp985 = t989
                }
                var jp982 bool
                if jp985 {
                    jp982 = true
                } else {
                    var t986 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                    jp982 = t986
                }
                var jp946 bool
                if jp982 {
                    jp946 = true
                } else {
                    var t983 bool = byte__63 < 32
                    jp946 = t983
                }
                if jp946 {
                    var t976 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                    var t977 bool = t976 < index__62
                    if t977 {
                        var t978 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                        var t979 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t978, index__62)
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t979)
                    } else {}
                    var t951 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
                    if t951 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\"")
                    } else {
                        var t954 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                        if t954 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\\")
                        } else {
                            var t957 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                            if t957 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\b")
                            } else {
                                var t960 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                                if t960 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\t")
                                } else {
                                    var t963 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                                    if t963 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\n")
                                    } else {
                                        var t966 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                                        if t966 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\f")
                                        } else {
                                            var t969 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                                            if t969 {
                                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\r")
                                            } else {
                                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\u00")
                                                var t971 uint8 = byte__63 / 16
                                                var t972 rune = _goml_m_std_p_json_p_json__hex__digit(t971)
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t972)
                                                var t973_rhs uint8 = 16
                                                var t973 uint8 = byte__63 % t973_rhs
                                                var t974 rune = _goml_m_std_p_json_p_json__hex__digit(t973)
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t974)
                                            }
                                        }
                                    }
                                }
                            }
                        }
                    }
                    var t949 int = index__62 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(start__61, t949)
                } else {}
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop942
        }
    }
    var t935 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
    var t936 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    var t937 bool = t935 < t936
    if t937 {
        var t938 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
        var t939 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
        var t940 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t938, t939)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t940)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__64 _goml_m_std_p_text_p_StringBuilder, value__65 _goml_m_std_p_json_p_Value) struct{} {
    switch value__65.(type) {
    case Object:
        var x92 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__65.(Object)._0
        var fields__66 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x92
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 123)
        var index__67 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var for_iter98 _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_ = _goml_m_trait__impl_i_IntoIter_hce918e329d488746205fdc863e60371c_r__i_into__iter(fields__66)
        Loop_loop1005:
        for {
            if true {
                var for_next99 _goml_m_Option_____o_string_c_std_p_json_p_Value_q_ = _goml_m_trait__impl_i_Iterator_h994d05b87e2f17b89cf287973a116e36_Value_q__i_next(for_iter98)
                switch for_next99.(type) {
                case _goml_m_Option_____o_string_c_std_p_json_p_Value_q__None:
                    break Loop_loop1005
                case _goml_m_Option_____o_string_c_std_p_json_p_Value_q__Some:
                    var x100 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_next99.(_goml_m_Option_____o_string_c_std_p_json_p_Value_q__Some)._0
                    var field__68 Tuple2_6string_26_goml_m_std_p_json_p_Value = x100
                    var t1013 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                    var t1014 bool = t1013 > 0
                    if t1014 {
                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                    } else {}
                    var t1008 string = field__68._0
                    _goml_m_std_p_json_p_write__json__string(builder__64, t1008)
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 58)
                    var t1009 _goml_m_std_p_json_p_Value = field__68._1
                    _goml_m_std_p_json_p_write__json__value(builder__64, t1009)
                    var t1010 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                    var t1011 int = t1010 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__67, t1011)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1005
            }
        }
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 125)
        return struct{}{}
    case Array:
        var x93 *_goml_vec__goml_m_std_p_json_p_Value = value__65.(Array)._0
        var items__69 *_goml_vec__goml_m_std_p_json_p_Value = x93
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 91)
        var index__70 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var for_iter107 _goml_m_FnIterator____std_p_json_p_Value = _goml_m_trait__impl_i_IntoIterator_i_Vec_l_std_p_json_p_Value_r__i_into__iter(items__69)
        Loop_loop1018:
        for {
            if true {
                var for_next108 _goml_m_Option____std_p_json_p_Value = _goml_m_trait__impl_i_Iterator_i_FnIterator____std_p_json_p_Value_i_next(for_iter107)
                switch for_next108.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    break Loop_loop1018
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x109 _goml_m_std_p_json_p_Value = for_next108.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var item__71 _goml_m_std_p_json_p_Value = x109
                    var t1024 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__70)
                    var t1025 bool = t1024 > 0
                    if t1025 {
                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                    } else {}
                    _goml_m_std_p_json_p_write__json__value(builder__64, item__71)
                    var t1021 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__70)
                    var t1022 int = t1021 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__70, t1022)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1018
            }
        }
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 93)
        return struct{}{}
    case String:
        var x94 string = value__65.(String)._0
        var text__72 string = x94
        _goml_m_std_p_json_p_write__json__string(builder__64, text__72)
        return struct{}{}
    case Number:
        var x95 string = value__65.(Number)._0
        var number__73 string = x95
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, number__73)
        return struct{}{}
    case Bool:
        var x96 bool = value__65.(Bool)._0
        var value__74 bool = x96
        var jp1030 string
        if value__74 {
            jp1030 = "true"
        } else {
            jp1030 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, jp1030)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__75 _goml_m_std_p_json_p_Value) string {
    var retv1034 string
    var builder__76 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
    _goml_m_std_p_json_p_write__json__value(builder__76, value__75)
    var t1035 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__76)
    retv1034 = t1035
    return retv1034
}

func _goml_m_std_p_json_p_field(value__77 _goml_m_std_p_json_p_Value, name__78 string) _goml_m_Option____std_p_json_p_Value {
    var retv1037 _goml_m_Option____std_p_json_p_Value
    var jp1039 _goml_m_Option____std_p_json_p_Value
    switch value__77.(type) {
    case Object:
        var x114 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__77.(Object)._0
        var fields__79 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x114
        var result__80 *ref__goml_m_Option____std_p_json_p_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(_goml_m_Option____std_p_json_p_Value_None{})
        var for_iter119 _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_ = _goml_m_trait__impl_i_IntoIter_hce918e329d488746205fdc863e60371c_r__i_into__iter(fields__79)
        Loop_loop1042:
        for {
            if true {
                var for_next120 _goml_m_Option_____o_string_c_std_p_json_p_Value_q_ = _goml_m_trait__impl_i_Iterator_h994d05b87e2f17b89cf287973a116e36_Value_q__i_next(for_iter119)
                switch for_next120.(type) {
                case _goml_m_Option_____o_string_c_std_p_json_p_Value_q__None:
                    break Loop_loop1042
                case _goml_m_Option_____o_string_c_std_p_json_p_Value_q__Some:
                    var x121 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_next120.(_goml_m_Option_____o_string_c_std_p_json_p_Value_q__Some)._0
                    var field__81 Tuple2_6string_26_goml_m_std_p_json_p_Value = x121
                    var t1045 string = field__81._0
                    var t1046 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1045, name__78)
                    if t1046 {
                        var t1047 _goml_m_std_p_json_p_Value = field__81._1
                        var t1048 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                            _0: t1047,
                        }
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(result__80, t1048)
                    } else {}
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1042
            }
        }
        var t1041 _goml_m_Option____std_p_json_p_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(result__80)
        jp1039 = t1041
        retv1037 = jp1039
        return retv1037
    default:
        jp1039 = _goml_m_Option____std_p_json_p_Value_None{}
        retv1037 = jp1039
        return retv1037
    }
}

func _goml_m_std_p_json_p_as__string(value__82 _goml_m_std_p_json_p_Value) Option__string {
    var retv1051 Option__string
    var jp1053 Option__string
    switch value__82.(type) {
    case String:
        var x125 string = value__82.(String)._0
        var text__83 string = x125
        var t1054 Option__string = Option__string_Some{
            _0: text__83,
        }
        jp1053 = t1054
    default:
        jp1053 = Option__string_None{}
    }
    retv1051 = jp1053
    return retv1051
}

func _goml_m_std_p_json_p_parse__json__int__text(value__84 string) Option__int {
    var retv1056 Option__int
    var t1059 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
    var t1060 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1059, 0)
    var jp1058 Option__int
    if t1060 {
        jp1058 = Option__int_None{}
        retv1056 = jp1058
        return retv1056
    } else {
        var t1061 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, 0)
        var negative__85 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1061, 45)
        var jp1063 int
        if negative__85 {
            jp1063 = 1
        } else {
            jp1063 = 0
        }
        var index__86 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(jp1063)
        var result__87 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var t1091 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
        var t1092 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1093 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1091, t1092)
        if t1093 {
            retv1056 = Option__int_None{}
            return retv1056
        } else {
            Loop_loop1072:
            for {
                var t1073 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                var t1074 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                var t1075 bool = t1073 < t1074
                if t1075 {
                    var t1076 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                    var byte__88 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1076)
                    var t1089 bool = byte__88 < 48
                    var jp1082 bool
                    if t1089 {
                        jp1082 = true
                    } else {
                        var t1090 bool = byte__88 > 57
                        jp1082 = t1090
                    }
                    if jp1082 {
                        retv1056 = Option__int_None{}
                        return retv1056
                    } else {
                        var t1083 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                        var t1084 int = t1083 * 10
                        var t1085 uint8 = byte__88 - 48
                        var t1086 int = int(uint8(t1085))
                        var t1087 int = t1084 + t1086
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__87, t1087)
                        var t1078 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                        var t1079 int = t1078 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__86, t1079)
                        continue
                    }
                } else {
                    break Loop_loop1072
                }
            }
            var jp1067 int
            if negative__85 {
                var t1069 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                var t1070 int = 0 - t1069
                jp1067 = t1070
            } else {
                var t1071 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                jp1067 = t1071
            }
            var t1068 Option__int = Option__int_Some{
                _0: jp1067,
            }
            jp1058 = t1068
            retv1056 = jp1058
            return retv1056
        }
    }
}

func _goml_m_std_p_json_p_as__int(value__89 _goml_m_std_p_json_p_Value) Option__int {
    var retv1095 Option__int
    var jp1097 Option__int
    switch value__89.(type) {
    case Number:
        var x134 string = value__89.(Number)._0
        var number__90 string = x134
        var t1098 Option__int = _goml_m_std_p_json_p_parse__json__int__text(number__90)
        jp1097 = t1098
    default:
        jp1097 = Option__int_None{}
    }
    retv1095 = jp1097
    return retv1095
}

func _goml_m_std_p_json_p_as__bool(value__91 _goml_m_std_p_json_p_Value) Option__bool {
    var retv1100 Option__bool
    var jp1102 Option__bool
    switch value__91.(type) {
    case Bool:
        var x140 bool = value__91.(Bool)._0
        var result__92 bool = x140
        var t1103 Option__bool = Option__bool_Some{
            _0: result__92,
        }
        jp1102 = t1103
    default:
        jp1102 = Option__bool_None{}
    }
    retv1100 = jp1102
    return retv1100
}

func main0() struct{} {
    var mtmp64 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1114 _goml_m_std_p_json_p_Value
    switch mtmp64.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x65 _goml_m_std_p_json_p_Value = mtmp64.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__0 _goml_m_std_p_json_p_Value = x65
        jp1114 = value__0
        var value__2 _goml_m_std_p_json_p_Value = jp1114
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
        var t1118 string = _goml_m_std_p_json_p_encode(value__2)
        println__T_string(t1118)
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
    var retv1176 *_goml_vec_string
    var t1177 *_goml_vec_string = vec_new__Vec_6string()
    retv1176 = t1177
    return retv1176
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv1179 *ref_int_x
    var t1180 *ref_int_x = ref__Ref_3int(value__209)
    retv1179 = t1180
    return retv1179
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv1185 int
    var t1186 int = ref_get__Ref_3int(self__210)
    retv1185 = t1186
    return retv1185
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv1188 bool
    var t1189 bool = self__59 == other__60
    retv1188 = t1189
    return retv1188
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__string(self__128 *_goml_vec_string, elem__129 string) struct{} {
    vec_push__Vec_6string(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv1193 int
    var t1194 int = _goml_runtime_core_string_len(self__9)
    retv1193 = t1194
    return retv1193
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv1198 string
    var t1199 string = _goml_runtime_core_char_to_string(self__7)
    retv1198 = t1199
    return retv1198
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv1203 string
    var t1204 string = _goml_runtime_core_int_to_string(self__5)
    retv1203 = t1204
    return retv1203
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv1206 bool
    var t1207 bool = self__69 == other__70
    retv1206 = t1207
    return retv1206
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv1209 uint8
    var t1210 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv1209 = t1210
    return retv1209
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__209 uint32) *ref_uint32_x {
    var retv1212 *ref_uint32_x
    var t1213 *ref_uint32_x = ref__Ref_6uint32(value__209)
    retv1212 = t1213
    return retv1212
}

func _goml_m_range(start__224 int, end__225 int) FnIterator__int {
    var retv1215 FnIterator__int
    var t1216 FnIterator__int = __goml_builtin_range(start__224, end__225)
    retv1215 = t1216
    return retv1215
}

func _goml_m_trait__impl_i_IntoIterator_i_FnIterator____int_i_into__iter(self__109 FnIterator__int) FnIterator__int {
    var retv1218 FnIterator__int
    retv1218 = self__109
    return retv1218
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____int_i_next(self__102 FnIterator__int) Option__int {
    var retv1220 Option__int
    var t1221 func() Option__int = self__102.next_fn
    var t1222 Option__int = t1221()
    retv1220 = t1222
    return retv1220
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__210 *ref_uint32_x) uint32 {
    var retv1224 uint32
    var t1225 uint32 = ref_get__Ref_6uint32(self__210)
    retv1224 = t1225
    return retv1224
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(self__211 *ref_uint32_x, value__212 uint32) struct{} {
    ref_set__Ref_6uint32(self__211, value__212)
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv1229 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp1231 Option__char
    if valid__3 {
        var t1232 Option__char = Option__char_Some{
            _0: value__4,
        }
        jp1231 = t1232
    } else {
        jp1231 = Option__char_None{}
    }
    retv1229 = jp1231
    return retv1229
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv1234 string
    var t1235 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv1234 = t1235
    return retv1234
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv1237 bool
    var t1238 bool = self__55 == other__56
    retv1237 = t1238
    return retv1237
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var retv1240 *_goml_vec__goml_m_std_p_json_p_Value
    var t1241 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    retv1240 = t1241
    return retv1240
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__128 *_goml_vec__goml_m_std_p_json_p_Value, elem__129 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var retv1245 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
    var t1246 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    retv1245 = t1246
    return retv1245
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__128 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__129 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv1250 rune
    var t1251 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv1250 = t1251
    return retv1250
}

func _goml_m_trait__impl_i_IntoIter_hce918e329d488746205fdc863e60371c_r__i_into__iter(self__185 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value) _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_ {
    var retv1253 _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_
    var t1254 _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_ = _goml_m_inherent_i_Vec_i_Vec_l_h3891a7f9b4b5f4fec59c0bb4214e9af5_json_p_Value_q_(self__185)
    retv1253 = t1254
    return retv1253
}

func _goml_m_trait__impl_i_Iterator_h994d05b87e2f17b89cf287973a116e36_Value_q__i_next(self__102 _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_) _goml_m_Option_____o_string_c_std_p_json_p_Value_q_ {
    var retv1256 _goml_m_Option_____o_string_c_std_p_json_p_Value_q_
    var t1257 func() _goml_m_Option_____o_string_c_std_p_json_p_Value_q_ = self__102.next_fn
    var t1258 _goml_m_Option_____o_string_c_std_p_json_p_Value_q_ = t1257()
    retv1256 = t1258
    return retv1256
}

func _goml_m_trait__impl_i_IntoIterator_i_Vec_l_std_p_json_p_Value_r__i_into__iter(self__185 *_goml_vec__goml_m_std_p_json_p_Value) _goml_m_FnIterator____std_p_json_p_Value {
    var retv1260 _goml_m_FnIterator____std_p_json_p_Value
    var t1261 _goml_m_FnIterator____std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__std_p_json_p_Value(self__185)
    retv1260 = t1261
    return retv1260
}

func _goml_m_trait__impl_i_Iterator_i_FnIterator____std_p_json_p_Value_i_next(self__102 _goml_m_FnIterator____std_p_json_p_Value) _goml_m_Option____std_p_json_p_Value {
    var retv1263 _goml_m_Option____std_p_json_p_Value
    var t1264 func() _goml_m_Option____std_p_json_p_Value = self__102.next_fn
    var t1265 _goml_m_Option____std_p_json_p_Value = t1264()
    retv1263 = t1265
    return retv1263
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(value__209 _goml_m_Option____std_p_json_p_Value) *ref__goml_m_Option____std_p_json_p_Value_x {
    var retv1267 *ref__goml_m_Option____std_p_json_p_Value_x
    var t1268 *ref__goml_m_Option____std_p_json_p_Value_x = ref___goml_m_Ref__24Option____std_p_json_p_Value(value__209)
    retv1267 = t1268
    return retv1267
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(self__211 *ref__goml_m_Option____std_p_json_p_Value_x, value__212 _goml_m_Option____std_p_json_p_Value) struct{} {
    ref_set___goml_m_Ref__24Option____std_p_json_p_Value(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(self__210 *ref__goml_m_Option____std_p_json_p_Value_x) _goml_m_Option____std_p_json_p_Value {
    var retv1272 _goml_m_Option____std_p_json_p_Value
    var t1273 _goml_m_Option____std_p_json_p_Value = ref_get___goml_m_Ref__24Option____std_p_json_p_Value(self__210)
    retv1272 = t1273
    return retv1272
}

func println__T_string(value__1 string) struct{} {
    var t1275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t1275)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t1278 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t1278)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t1281 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t1281)
    return struct{}{}
}

func __goml_builtin_range(start__220 int, end__221 int) FnIterator__int {
    var retv1286 FnIterator__int
    var current__222 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(start__220)
    var t1287 closure_env_goml_builtin_range_0 = closure_env_goml_builtin_range_0{
        current_0: current__222,
        end_1: end__221,
    }
    var t1288 FnIterator__int = _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(func() Option__int {
        return _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(t1287)
    })
    retv1286 = t1288
    return retv1286
}

func _goml_m_inherent_i_Vec_i_Vec_l_h3891a7f9b4b5f4fec59c0bb4214e9af5_json_p_Value_q_(self__180 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value) _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_ {
    var retv1290 _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_
    var index__181 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__182 int = _goml_m_inherent_i_Vec_i_Vec_l_hfeabd5ec0b8d04a16628cec2b1a6daf2_json_p_Value_q_(self__180)
    var t1291 closure_env_inherent_Vec_Vec_T_iter_T_string_std_json_Value_1 = closure_env_inherent_Vec_Vec_T_iter_T_string_std_json_Value_1{
        index_0: index__181,
        len_1: len__182,
        self_2: self__180,
    }
    var t1292 _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_ = _goml_m_inherent_i_FnIterator__hb704bd0d42198a38c1141423913b9d03_json_p_Value_q_(func() _goml_m_Option_____o_string_c_std_p_json_p_Value_q_ {
        return _goml_m_inherent_i_closure__en_h23749ffd5fdf71c3765ceddd191fc804_alue__1_i_apply(t1291)
    })
    retv1290 = t1292
    return retv1290
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_iter____T__std_p_json_p_Value(self__180 *_goml_vec__goml_m_std_p_json_p_Value) _goml_m_FnIterator____std_p_json_p_Value {
    var retv1294 _goml_m_FnIterator____std_p_json_p_Value
    var index__181 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var len__182 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__std_p_json_p_Value(self__180)
    var t1295 closure_env_inherent_Vec_Vec_T_iter_T_std_json_Value_2 = closure_env_inherent_Vec_Vec_T_iter_T_std_json_Value_2{
        index_0: index__181,
        len_1: len__182,
        self_2: self__180,
    }
    var t1296 _goml_m_FnIterator____std_p_json_p_Value = _goml_m_inherent_i_FnIterator__hec8bbc0c2942707d643340b6ec9ed32d__p_json_p_Value(func() _goml_m_Option____std_p_json_p_Value {
        return _goml_m_inherent_i_closure__en_hbdd03344a8a0cb4b58c708dd121c1da4_alue__2_i_apply(t1295)
    })
    retv1294 = t1296
    return retv1294
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv1298 string
    retv1298 = self__38
    return retv1298
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv1300 string
    var t1301 string = _goml_runtime_core_int_to_string(self__40)
    retv1300 = t1301
    return retv1300
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv1303 string
    var t1304 string = _goml_runtime_core_bool_to_string(self__37)
    retv1303 = t1304
    return retv1303
}

func _goml_m_inherent_i_FnIterator_i_FnIterator_l_T_r__i_from__fn____T__int(next_fn__101 func() Option__int) FnIterator__int {
    var retv1306 FnIterator__int
    var t1307 FnIterator__int = FnIterator__int{
        next_fn: next_fn__101,
    }
    retv1306 = t1307
    return retv1306
}

func _goml_m_inherent_i_Vec_i_Vec_l_hfeabd5ec0b8d04a16628cec2b1a6daf2_json_p_Value_q_(self__139 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value) int {
    var retv1309 int
    var t1310 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__139)
    retv1309 = t1310
    return retv1309
}

func _goml_m_inherent_i_Vec_i_Vec_l_he2156be4e181254601ea4457ad787b14_json_p_Value_q_(self__134 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, index__135 int) Tuple2_6string_26_goml_m_std_p_json_p_Value {
    var retv1312 Tuple2_6string_26_goml_m_std_p_json_p_Value
    var t1313 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__134, index__135)
    retv1312 = t1313
    return retv1312
}

func _goml_m_inherent_i_FnIterator__hb704bd0d42198a38c1141423913b9d03_json_p_Value_q_(next_fn__101 func() _goml_m_Option_____o_string_c_std_p_json_p_Value_q_) _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_ {
    var retv1315 _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_
    var t1316 _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_ = _goml_m_FnIterator_____o_string_c_std_p_json_p_Value_q_{
        next_fn: next_fn__101,
    }
    retv1315 = t1316
    return retv1315
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__std_p_json_p_Value(self__139 *_goml_vec__goml_m_std_p_json_p_Value) int {
    var retv1318 int
    var t1319 int = vec_len___goml_m_Vec__16std_p_json_p_Value(self__139)
    retv1318 = t1319
    return retv1318
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__std_p_json_p_Value(self__134 *_goml_vec__goml_m_std_p_json_p_Value, index__135 int) _goml_m_std_p_json_p_Value {
    var retv1321 _goml_m_std_p_json_p_Value
    var t1322 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(self__134, index__135)
    retv1321 = t1322
    return retv1321
}

func _goml_m_inherent_i_FnIterator__hec8bbc0c2942707d643340b6ec9ed32d__p_json_p_Value(next_fn__101 func() _goml_m_Option____std_p_json_p_Value) _goml_m_FnIterator____std_p_json_p_Value {
    var retv1324 _goml_m_FnIterator____std_p_json_p_Value
    var t1325 _goml_m_FnIterator____std_p_json_p_Value = _goml_m_FnIterator____std_p_json_p_Value{
        next_fn: next_fn__101,
    }
    retv1324 = t1325
    return retv1324
}

func _goml_m_inherent_i_closure__en_h5b1fcaf2e23588c4625108f446fe7c51_ange__0_i_apply(env83 closure_env_goml_builtin_range_0) Option__int {
    var retv1336 Option__int
    var current__222 *ref_int_x = env83.current_0
    var end__221 int = env83.end_1
    var value__223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(current__222)
    var t1339 bool = value__223 < end__221
    var jp1338 Option__int
    if t1339 {
        var t1340 int = value__223 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(current__222, t1340)
        var t1341 Option__int = Option__int_Some{
            _0: value__223,
        }
        jp1338 = t1341
    } else {
        jp1338 = Option__int_None{}
    }
    retv1336 = jp1338
    return retv1336
}

func _goml_m_inherent_i_closure__en_h23749ffd5fdf71c3765ceddd191fc804_alue__1_i_apply(env84 closure_env_inherent_Vec_Vec_T_iter_T_string_std_json_Value_1) _goml_m_Option_____o_string_c_std_p_json_p_Value_q_ {
    var retv1343 _goml_m_Option_____o_string_c_std_p_json_p_Value_q_
    var index__181 *ref_int_x = env84.index_0
    var len__182 int = env84.len_1
    var self__180 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = env84.self_2
    var current__183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__181)
    var t1346 bool = current__183 < len__182
    var jp1345 _goml_m_Option_____o_string_c_std_p_json_p_Value_q_
    if t1346 {
        var value__184 Tuple2_6string_26_goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_he2156be4e181254601ea4457ad787b14_json_p_Value_q_(self__180, current__183)
        var t1347 int = current__183 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__181, t1347)
        var t1348 _goml_m_Option_____o_string_c_std_p_json_p_Value_q_ = _goml_m_Option_____o_string_c_std_p_json_p_Value_q__Some{
            _0: value__184,
        }
        jp1345 = t1348
    } else {
        jp1345 = _goml_m_Option_____o_string_c_std_p_json_p_Value_q__None{}
    }
    retv1343 = jp1345
    return retv1343
}

func _goml_m_inherent_i_closure__en_hbdd03344a8a0cb4b58c708dd121c1da4_alue__2_i_apply(env85 closure_env_inherent_Vec_Vec_T_iter_T_std_json_Value_2) _goml_m_Option____std_p_json_p_Value {
    var retv1350 _goml_m_Option____std_p_json_p_Value
    var index__181 *ref_int_x = env85.index_0
    var len__182 int = env85.len_1
    var self__180 *_goml_vec__goml_m_std_p_json_p_Value = env85.self_2
    var current__183 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__181)
    var t1353 bool = current__183 < len__182
    var jp1352 _goml_m_Option____std_p_json_p_Value
    if t1353 {
        var value__184 _goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_get____T__std_p_json_p_Value(self__180, current__183)
        var t1354 int = current__183 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__181, t1354)
        var t1355 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
            _0: value__184,
        }
        jp1352 = t1355
    } else {
        jp1352 = _goml_m_Option____std_p_json_p_Value_None{}
    }
    retv1350 = jp1352
    return retv1350
}

func main() {
    main0()
}
