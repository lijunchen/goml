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

func vec_reserve__Vec_5uint8(vec *_goml_vec_uint8, additional int) struct{} {
    vec.items = _goml_slices.Grow(vec.items, int(additional))
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
    var retv152 _goml_m_std_p_text_p_StringBuilder
    var t153 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var t154 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: t153,
    }
    retv152 = t154
    return retv152
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var t169 *_goml_vec_uint8 = self__3.values
    var t170 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__uint8(t169, t170)
    var for_index1 int = 0
    var for_limit2 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    Loop_loop172:
    for {
        var t173 bool = for_index1 < for_limit2
        if t173 {
            var for_item3 int = for_index1
            var t174 int = for_index1 + 1
            for_index1 = t174
            var index__5 int = for_item3
            var t175 *_goml_vec_uint8 = self__3.values
            var t176 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__4, index__5)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(t175, t176)
            continue
        } else {
            break Loop_loop172
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__6 _goml_m_std_p_text_p_StringBuilder, value__7 rune) struct{} {
    var t179 string = _goml_m_inherent_i_char_i_char_i_to__string(value__7)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__6, t179)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(self__15 _goml_m_std_p_text_p_StringBuilder) string {
    var retv193 string
    var t194 *_goml_vec_uint8 = self__15.values
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t194)
    var x12 string = mtmp10._1
    var value__16 string = x12
    retv193 = value__16
    return retv193
}

func _goml_m_std_p_json_p_json__parser(input__0 string) _goml_m_std_p_json_p_JsonParser {
    var retv199 _goml_m_std_p_json_p_JsonParser
    var t200 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t201 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__0,
        index: t200,
    }
    retv199 = t201
    return retv199
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var retv203 string
    var t204 string = message__2 + " at byte "
    var t205 *ref_int_x = value__1.index
    var t206 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t205)
    var t207 string = _goml_m_inherent_i_int_i_int_i_to__string(t206)
    var t208 string = t204 + t207
    retv203 = t208
    return retv203
}

func _goml_m_std_p_json_p_json__whitespace(value__3 uint8) bool {
    var retv210 bool
    var t219 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 9)
    var jp217 bool
    if t219 {
        jp217 = true
    } else {
        var t220 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 10)
        jp217 = t220
    }
    var jp214 bool
    if jp217 {
        jp214 = true
    } else {
        var t218 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 13)
        jp214 = t218
    }
    var jp212 bool
    if jp214 {
        jp212 = true
    } else {
        var t215 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 32)
        jp212 = t215
    }
    retv210 = jp212
    return retv210
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop223:
    for {
        var t231 *ref_int_x = value__4.index
        var t232 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t231)
        var t233 string = value__4.input
        var t234 int = _goml_m_inherent_i_string_i_string_i_byte__len(t233)
        var t235 bool = t232 < t234
        var jp225 bool
        if t235 {
            var t236 string = value__4.input
            var t237 *ref_int_x = value__4.index
            var t238 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t237)
            var t239 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t236, t238)
            var t240 bool = _goml_m_std_p_json_p_json__whitespace(t239)
            jp225 = t240
        } else {
            jp225 = false
        }
        if jp225 {
            var t226 *ref_int_x = value__4.index
            var t227 *ref_int_x = value__4.index
            var t228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t227)
            var t229 int = t228 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t226, t229)
            continue
        } else {
            break Loop_loop223
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var retv242 Option__uint32
    var t268 bool = value__5 >= 48
    var jp246 bool
    if t268 {
        var t269 bool = value__5 <= 57
        jp246 = t269
    } else {
        jp246 = false
    }
    var jp244 Option__uint32
    if jp246 {
        var t247 uint8 = value__5 - 48
        var t248 uint32 = uint32(uint8(t247))
        var t249 Option__uint32 = Option__uint32_Some{
            _0: t248,
        }
        jp244 = t249
    } else {
        var t266 bool = value__5 >= 65
        var jp253 bool
        if t266 {
            var t267 bool = value__5 <= 70
            jp253 = t267
        } else {
            jp253 = false
        }
        var jp251 Option__uint32
        if jp253 {
            var t254 uint8 = value__5 - 55
            var t255 uint32 = uint32(uint8(t254))
            var t256 Option__uint32 = Option__uint32_Some{
                _0: t255,
            }
            jp251 = t256
        } else {
            var t264 bool = value__5 >= 97
            var jp260 bool
            if t264 {
                var t265 bool = value__5 <= 102
                jp260 = t265
            } else {
                jp260 = false
            }
            var jp258 Option__uint32
            if jp260 {
                var t261 uint8 = value__5 - 87
                var t262 uint32 = uint32(uint8(t261))
                var t263 Option__uint32 = Option__uint32_Some{
                    _0: t262,
                }
                jp258 = t263
            } else {
                jp258 = Option__uint32_None{}
            }
            jp251 = jp258
        }
        jp244 = jp251
    }
    retv242 = jp244
    return retv242
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var retv271 Result__uint32__string
    var t274 *ref_int_x = value__6.index
    var t275 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t274)
    var t276 int = t275 + 4
    var t277 string = value__6.input
    var t278 int = _goml_m_inherent_i_string_i_string_i_byte__len(t277)
    var t279 bool = t276 > t278
    var jp273 Result__uint32__string
    if t279 {
        var t280 string = _goml_m_std_p_json_p_json__error(value__6, "incomplete unicode escape")
        var t281 Result__uint32__string = Result__uint32__string_Err{
            _0: t280,
        }
        jp273 = t281
        retv271 = jp273
        return retv271
    } else {
        var t282_source int = 0
        var t282 uint32 = uint32(int(t282_source))
        var result__7 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(t282)
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop290:
        for {
            var t291 bool = for_index0 < for_limit1
            if t291 {
                var for_item2 int = for_index0
                var t292 int = for_index0 + 1
                for_index0 = t292
                var offset__8 int = for_item2
                var t293 string = value__6.input
                var t294 *ref_int_x = value__6.index
                var t295 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t294)
                var t296 int = t295 + offset__8
                var t297 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t293, t296)
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t297)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t299 string = _goml_m_std_p_json_p_json__error(value__6, "invalid unicode escape")
                    var t300 Result__uint32__string = Result__uint32__string_Err{
                        _0: t299,
                    }
                    retv271 = t300
                    return retv271
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var digit__9 uint32 = x5
                    var t301 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
                    var t302 uint32 = t301 * 16
                    var t303 uint32 = t302 + digit__9
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(result__7, t303)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop290
            }
        }
        var t284 *ref_int_x = value__6.index
        var t285 *ref_int_x = value__6.index
        var t286 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t285)
        var t287 int = t286 + 4
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t284, t287)
        var t288 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
        var t289 Result__uint32__string = Result__uint32__string_Ok{
            _0: t288,
        }
        jp273 = t289
        retv271 = jp273
        return retv271
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var retv306 Result__unit__string
    var mtmp8 Option__char = char_from_uint32(codepoint__12)
    var jp308 Result__unit__string
    switch mtmp8.(type) {
    case Option__char_None:
        var t309 string = _goml_m_std_p_json_p_json__error(value__10, "invalid unicode codepoint")
        var t310 Result__unit__string = Result__unit__string_Err{
            _0: t309,
        }
        jp308 = t310
    case Option__char_Some:
        var x9 rune = mtmp8.(Option__char_Some)._0
        var character__13 rune = x9
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__11, character__13)
        var t311 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp308 = t311
    default:
        panic("non-exhaustive match")
    }
    retv306 = jp308
    return retv306
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var retv313 Result__unit__string
    var mtmp11 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp315 uint32
    switch mtmp11.(type) {
    case Result__uint32__string_Ok:
        var x12 uint32 = mtmp11.(Result__uint32__string_Ok)._0
        var codepoint__16 uint32 = x12
        jp315 = codepoint__16
        var first__18 uint32 = jp315
        var t377 bool = first__18 >= 55296
        var jp319 bool
        if t377 {
            var t378 bool = first__18 <= 56319
            jp319 = t378
        } else {
            jp319 = false
        }
        var jp317 Result__unit__string
        if jp319 {
            var t356 *ref_int_x = value__14.index
            var t357 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t356)
            var t358 int = t357 + 2
            var t359 string = value__14.input
            var t360 int = _goml_m_inherent_i_string_i_string_i_byte__len(t359)
            var t361 bool = t358 > t360
            var jp348 bool
            if t361 {
                jp348 = true
            } else {
                var t362 string = value__14.input
                var t363 *ref_int_x = value__14.index
                var t364 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t363)
                var t365 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t362, t364)
                var t366 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t365, 92)
                var t367 bool = !t366
                jp348 = t367
            }
            var jp323 bool
            if jp348 {
                jp323 = true
            } else {
                var t349 string = value__14.input
                var t350 *ref_int_x = value__14.index
                var t351 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t350)
                var t352 int = t351 + 1
                var t353 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t349, t352)
                var t354 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t353, 117)
                var t355 bool = !t354
                jp323 = t355
            }
            var jp321 Result__unit__string
            if jp323 {
                var t324 string = _goml_m_std_p_json_p_json__error(value__14, "missing low surrogate")
                var t325 Result__unit__string = Result__unit__string_Err{
                    _0: t324,
                }
                jp321 = t325
                jp317 = jp321
                retv313 = jp317
                return retv313
            } else {
                var t326 *ref_int_x = value__14.index
                var t327 *ref_int_x = value__14.index
                var t328 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t327)
                var t329 int = t328 + 2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t326, t329)
                var mtmp15 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp331 uint32
                switch mtmp15.(type) {
                case Result__uint32__string_Ok:
                    var x16 uint32 = mtmp15.(Result__uint32__string_Ok)._0
                    var codepoint__19 uint32 = x16
                    jp331 = codepoint__19
                    var second__21 uint32 = jp331
                    var t344 bool = second__21 < 56320
                    var jp335 bool
                    if t344 {
                        jp335 = true
                    } else {
                        var t345 bool = second__21 > 57343
                        jp335 = t345
                    }
                    var jp333 Result__unit__string
                    if jp335 {
                        var t336 string = _goml_m_std_p_json_p_json__error(value__14, "invalid low surrogate")
                        var t337 Result__unit__string = Result__unit__string_Err{
                            _0: t336,
                        }
                        jp333 = t337
                    } else {
                        var t338 uint32 = first__18 - 55296
                        var t339 uint32 = t338 * 1024
                        var t340 uint32 = 65536 + t339
                        var t341 uint32 = t340 + second__21
                        var t342 uint32 = t341 - 56320
                        var t343 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, t342)
                        jp333 = t343
                    }
                    jp321 = jp333
                    jp317 = jp321
                    retv313 = jp317
                    return retv313
                case Result__uint32__string_Err:
                    var x17 string = mtmp15.(Result__uint32__string_Err)._0
                    var error__20 string = x17
                    var t346 Result__unit__string = Result__unit__string_Err{
                        _0: error__20,
                    }
                    retv313 = t346
                    return retv313
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t375 bool = first__18 >= 56320
            var jp371 bool
            if t375 {
                var t376 bool = first__18 <= 57343
                jp371 = t376
            } else {
                jp371 = false
            }
            var jp369 Result__unit__string
            if jp371 {
                var t372 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t373 Result__unit__string = Result__unit__string_Err{
                    _0: t372,
                }
                jp369 = t373
            } else {
                var t374 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, first__18)
                jp369 = t374
            }
            jp317 = jp369
            retv313 = jp317
            return retv313
        }
    case Result__uint32__string_Err:
        var x13 string = mtmp11.(Result__uint32__string_Err)._0
        var error__17 string = x13
        var t379 Result__unit__string = Result__unit__string_Err{
            _0: error__17,
        }
        retv313 = t379
        return retv313
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__22 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var retv381 Result__string__string
    var t502 *ref_int_x = value__22.index
    var t503 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t502)
    var t504 string = value__22.input
    var t505 int = _goml_m_inherent_i_string_i_string_i_byte__len(t504)
    var t506 bool = t503 >= t505
    var jp494 bool
    if t506 {
        jp494 = true
    } else {
        var t507 string = value__22.input
        var t508 *ref_int_x = value__22.index
        var t509 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t508)
        var t510 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t507, t509)
        var t511 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t510, 34)
        var t512 bool = !t511
        jp494 = t512
    }
    if jp494 {
        var t495 string = _goml_m_std_p_json_p_json__error(value__22, "expected string")
        var t496 Result__string__string = Result__string__string_Err{
            _0: t495,
        }
        retv381 = t496
        return retv381
    } else {
        var t497 *ref_int_x = value__22.index
        var t498 *ref_int_x = value__22.index
        var t499 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t498)
        var t500 int = t499 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t497, t500)
        var builder__23 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t383 *ref_int_x = value__22.index
        var t384 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t383)
        var segment__24 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t384)
        Loop_loop388:
        for {
            var t389 *ref_int_x = value__22.index
            var t390 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t389)
            var t391 string = value__22.input
            var t392 int = _goml_m_inherent_i_string_i_string_i_byte__len(t391)
            var t393 bool = t390 < t392
            if t393 {
                var t394 string = value__22.input
                var t395 *ref_int_x = value__22.index
                var t396 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t395)
                var byte__25 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t394, t396)
                var t398 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 34)
                if t398 {
                    var t406 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                    var t407 *ref_int_x = value__22.index
                    var t408 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t407)
                    var t409 bool = t406 < t408
                    if t409 {
                        var t410 string = value__22.input
                        var t411 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t412 *ref_int_x = value__22.index
                        var t413 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t412)
                        var t414 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t410, t411, t413)
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t414)
                    } else {}
                    var t400 *ref_int_x = value__22.index
                    var t401 *ref_int_x = value__22.index
                    var t402 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t401)
                    var t403 int = t402 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t400, t403)
                    var t404 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__23)
                    var t405 Result__string__string = Result__string__string_Ok{
                        _0: t404,
                    }
                    retv381 = t405
                    return retv381
                } else {
                    var t417 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 92)
                    if t417 {
                        var t474 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t475 *ref_int_x = value__22.index
                        var t476 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t475)
                        var t477 bool = t474 < t476
                        if t477 {
                            var t478 string = value__22.input
                            var t479 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                            var t480 *ref_int_x = value__22.index
                            var t481 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t480)
                            var t482 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t478, t479, t481)
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t482)
                        } else {}
                        var t419 *ref_int_x = value__22.index
                        var t420 *ref_int_x = value__22.index
                        var t421 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t420)
                        var t422 int = t421 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t419, t422)
                        var t467 *ref_int_x = value__22.index
                        var t468 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t467)
                        var t469 string = value__22.input
                        var t470 int = _goml_m_inherent_i_string_i_string_i_byte__len(t469)
                        var t471 bool = t468 >= t470
                        if t471 {
                            var t472 string = _goml_m_std_p_json_p_json__error(value__22, "incomplete escape")
                            var t473 Result__string__string = Result__string__string_Err{
                                _0: t472,
                            }
                            retv381 = t473
                            return retv381
                        } else {
                            var t424 string = value__22.input
                            var t425 *ref_int_x = value__22.index
                            var t426 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t425)
                            var escape__26 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t424, t426)
                            var t427 *ref_int_x = value__22.index
                            var t428 *ref_int_x = value__22.index
                            var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t428)
                            var t430 int = t429 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t427, t430)
                            var t435 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 34)
                            if t435 {
                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 34)
                                var t432 *ref_int_x = value__22.index
                                var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t432)
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t433)
                                continue
                            } else {
                                var t438 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 92)
                                if t438 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 92)
                                    var t432 *ref_int_x = value__22.index
                                    var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t432)
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t433)
                                    continue
                                } else {
                                    var t441 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 47)
                                    if t441 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 47)
                                        var t432 *ref_int_x = value__22.index
                                        var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t432)
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t433)
                                        continue
                                    } else {
                                        var t444 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 98)
                                        if t444 {
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
                                            var t432 *ref_int_x = value__22.index
                                            var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t432)
                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t433)
                                            continue
                                        } else {
                                            var t448 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 102)
                                            if t448 {
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
                                                var t432 *ref_int_x = value__22.index
                                                var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t432)
                                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t433)
                                                continue
                                            } else {
                                                var t452 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 110)
                                                if t452 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 10)
                                                    var t432 *ref_int_x = value__22.index
                                                    var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t432)
                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t433)
                                                    continue
                                                } else {
                                                    var t455 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 114)
                                                    if t455 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 13)
                                                        var t432 *ref_int_x = value__22.index
                                                        var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t432)
                                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t433)
                                                        continue
                                                    } else {
                                                        var t458 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 116)
                                                        if t458 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 9)
                                                            var t432 *ref_int_x = value__22.index
                                                            var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t432)
                                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t433)
                                                            continue
                                                        } else {
                                                            var t461 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 117)
                                                            if t461 {
                                                                var mtmp29 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__22, builder__23)
                                                                switch mtmp29.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t432 *ref_int_x = value__22.index
                                                                    var t433 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t432)
                                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t433)
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x31 string = mtmp29.(Result__unit__string_Err)._0
                                                                    var error__29 string = x31
                                                                    var t464 Result__string__string = Result__string__string_Err{
                                                                        _0: error__29,
                                                                    }
                                                                    retv381 = t464
                                                                    return retv381
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t465 string = _goml_m_std_p_json_p_json__error(value__22, "invalid escape")
                                                                var t466 Result__string__string = Result__string__string_Err{
                                                                    _0: t465,
                                                                }
                                                                retv381 = t466
                                                                return retv381
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
                        var t485 bool = byte__25 < 32
                        if t485 {
                            var t486 string = _goml_m_std_p_json_p_json__error(value__22, "unescaped control character")
                            var t487 Result__string__string = Result__string__string_Err{
                                _0: t486,
                            }
                            retv381 = t487
                            return retv381
                        } else {
                            var t488 *ref_int_x = value__22.index
                            var t489 *ref_int_x = value__22.index
                            var t490 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t489)
                            var t491 int = t490 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t488, t491)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop388
            }
        }
        var t386 string = _goml_m_std_p_json_p_json__error(value__22, "unterminated string")
        var t387 Result__string__string = Result__string__string_Err{
            _0: t386,
        }
        retv381 = t387
        return retv381
    }
}

func _goml_m_std_p_json_p_json__digit(value__30 uint8) bool {
    var retv514 bool
    var t517 bool = value__30 >= 48
    var jp516 bool
    if t517 {
        var t518 bool = value__30 <= 57
        jp516 = t518
    } else {
        jp516 = false
    }
    retv514 = jp516
    return retv514
}

func _goml_m_std_p_json_p_parse__digits(value__31 _goml_m_std_p_json_p_JsonParser) bool {
    var retv520 bool
    var t521 *ref_int_x = value__31.index
    var start__32 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t521)
    Loop_loop526:
    for {
        var t534 *ref_int_x = value__31.index
        var t535 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t534)
        var t536 string = value__31.input
        var t537 int = _goml_m_inherent_i_string_i_string_i_byte__len(t536)
        var t538 bool = t535 < t537
        var jp528 bool
        if t538 {
            var t539 string = value__31.input
            var t540 *ref_int_x = value__31.index
            var t541 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t540)
            var t542 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t539, t541)
            var t543 bool = _goml_m_std_p_json_p_json__digit(t542)
            jp528 = t543
        } else {
            jp528 = false
        }
        if jp528 {
            var t529 *ref_int_x = value__31.index
            var t530 *ref_int_x = value__31.index
            var t531 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t530)
            var t532 int = t531 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t529, t532)
            continue
        } else {
            break Loop_loop526
        }
    }
    var t523 *ref_int_x = value__31.index
    var t524 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t523)
    var t525 bool = t524 > start__32
    retv520 = t525
    return retv520
}

func _goml_m_std_p_json_p_parse__json__number(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv545 _goml_m_Result____std_p_json_p_Value____string
    var t546 *ref_int_x = value__33.index
    var start__34 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t546)
    var t668 string = value__33.input
    var t669 *ref_int_x = value__33.index
    var t670 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t669)
    var t671 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t668, t670)
    var t672 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t671, 45)
    if t672 {
        var t673 *ref_int_x = value__33.index
        var t674 *ref_int_x = value__33.index
        var t675 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t674)
        var t676 int = t675 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t673, t676)
    } else {}
    var t631 *ref_int_x = value__33.index
    var t632 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t631)
    var t633 string = value__33.input
    var t634 int = _goml_m_inherent_i_string_i_string_i_byte__len(t633)
    var t635 bool = t632 >= t634
    if t635 {
        var t636 string = _goml_m_std_p_json_p_json__error(value__33, "incomplete number")
        var t637 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t636,
        }
        retv545 = t637
        return retv545
    } else {
        var t639 string = value__33.input
        var t640 *ref_int_x = value__33.index
        var t641 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t640)
        var t642 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t639, t641)
        var t643 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t642, 48)
        if t643 {
            var t644 *ref_int_x = value__33.index
            var t645 *ref_int_x = value__33.index
            var t646 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t645)
            var t647 int = t646 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t644, t647)
            var t653 *ref_int_x = value__33.index
            var t654 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t653)
            var t655 string = value__33.input
            var t656 int = _goml_m_inherent_i_string_i_string_i_byte__len(t655)
            var t657 bool = t654 < t656
            var jp650 bool
            if t657 {
                var t658 string = value__33.input
                var t659 *ref_int_x = value__33.index
                var t660 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t659)
                var t661 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t658, t660)
                var t662 bool = _goml_m_std_p_json_p_json__digit(t661)
                jp650 = t662
            } else {
                jp650 = false
            }
            if jp650 {
                var t651 string = _goml_m_std_p_json_p_json__error(value__33, "invalid leading zero")
                var t652 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t651,
                }
                retv545 = t652
                return retv545
            } else {
                var t621 *ref_int_x = value__33.index
                var t622 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t621)
                var t623 string = value__33.input
                var t624 int = _goml_m_inherent_i_string_i_string_i_byte__len(t623)
                var t625 bool = t622 < t624
                var jp611 bool
                if t625 {
                    var t626 string = value__33.input
                    var t627 *ref_int_x = value__33.index
                    var t628 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t627)
                    var t629 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t626, t628)
                    var t630 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t629, 46)
                    jp611 = t630
                } else {
                    jp611 = false
                }
                if jp611 {
                    var t612 *ref_int_x = value__33.index
                    var t613 *ref_int_x = value__33.index
                    var t614 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t613)
                    var t615 int = t614 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t612, t615)
                    var t617 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t618 bool = !t617
                    if t618 {
                        var t619 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t620 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t619,
                        }
                        retv545 = t620
                        return retv545
                    } else {
                        var t593 *ref_int_x = value__33.index
                        var t594 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t593)
                        var t595 string = value__33.input
                        var t596 int = _goml_m_inherent_i_string_i_string_i_byte__len(t595)
                        var t597 bool = t594 < t596
                        var jp558 bool
                        if t597 {
                            var t600 string = value__33.input
                            var t601 *ref_int_x = value__33.index
                            var t602 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t601)
                            var t603 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t600, t602)
                            var t604 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t603, 101)
                            var jp599 bool
                            if t604 {
                                jp599 = true
                            } else {
                                var t605 string = value__33.input
                                var t606 *ref_int_x = value__33.index
                                var t607 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t606)
                                var t608 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t605, t607)
                                var t609 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t608, 69)
                                jp599 = t609
                            }
                            jp558 = jp599
                        } else {
                            jp558 = false
                        }
                        if jp558 {
                            var t559 *ref_int_x = value__33.index
                            var t560 *ref_int_x = value__33.index
                            var t561 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t560)
                            var t562 int = t561 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t559, t562)
                            var t576 *ref_int_x = value__33.index
                            var t577 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t576)
                            var t578 string = value__33.input
                            var t579 int = _goml_m_inherent_i_string_i_string_i_byte__len(t578)
                            var t580 bool = t577 < t579
                            var jp570 bool
                            if t580 {
                                var t583 string = value__33.input
                                var t584 *ref_int_x = value__33.index
                                var t585 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t584)
                                var t586 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t583, t585)
                                var t587 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t586, 43)
                                var jp582 bool
                                if t587 {
                                    jp582 = true
                                } else {
                                    var t588 string = value__33.input
                                    var t589 *ref_int_x = value__33.index
                                    var t590 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t589)
                                    var t591 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t588, t590)
                                    var t592 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t591, 45)
                                    jp582 = t592
                                }
                                jp570 = jp582
                            } else {
                                jp570 = false
                            }
                            if jp570 {
                                var t571 *ref_int_x = value__33.index
                                var t572 *ref_int_x = value__33.index
                                var t573 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t572)
                                var t574 int = t573 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t571, t574)
                            } else {}
                            var t565 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t566 bool = !t565
                            if t566 {
                                var t567 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t568 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t567,
                                }
                                retv545 = t568
                                return retv545
                            } else {
                                var t551 string = value__33.input
                                var t552 *ref_int_x = value__33.index
                                var t553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t552)
                                var t554 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t551, start__34, t553)
                                var t555 _goml_m_std_p_json_p_Value = Number{
                                    _0: t554,
                                }
                                var t556 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t555,
                                }
                                retv545 = t556
                                return retv545
                            }
                        } else {
                            var t551 string = value__33.input
                            var t552 *ref_int_x = value__33.index
                            var t553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t552)
                            var t554 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t551, start__34, t553)
                            var t555 _goml_m_std_p_json_p_Value = Number{
                                _0: t554,
                            }
                            var t556 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t555,
                            }
                            retv545 = t556
                            return retv545
                        }
                    }
                } else {
                    var t593 *ref_int_x = value__33.index
                    var t594 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t593)
                    var t595 string = value__33.input
                    var t596 int = _goml_m_inherent_i_string_i_string_i_byte__len(t595)
                    var t597 bool = t594 < t596
                    var jp558 bool
                    if t597 {
                        var t600 string = value__33.input
                        var t601 *ref_int_x = value__33.index
                        var t602 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t601)
                        var t603 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t600, t602)
                        var t604 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t603, 101)
                        var jp599 bool
                        if t604 {
                            jp599 = true
                        } else {
                            var t605 string = value__33.input
                            var t606 *ref_int_x = value__33.index
                            var t607 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t606)
                            var t608 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t605, t607)
                            var t609 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t608, 69)
                            jp599 = t609
                        }
                        jp558 = jp599
                    } else {
                        jp558 = false
                    }
                    if jp558 {
                        var t559 *ref_int_x = value__33.index
                        var t560 *ref_int_x = value__33.index
                        var t561 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t560)
                        var t562 int = t561 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t559, t562)
                        var t576 *ref_int_x = value__33.index
                        var t577 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t576)
                        var t578 string = value__33.input
                        var t579 int = _goml_m_inherent_i_string_i_string_i_byte__len(t578)
                        var t580 bool = t577 < t579
                        var jp570 bool
                        if t580 {
                            var t583 string = value__33.input
                            var t584 *ref_int_x = value__33.index
                            var t585 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t584)
                            var t586 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t583, t585)
                            var t587 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t586, 43)
                            var jp582 bool
                            if t587 {
                                jp582 = true
                            } else {
                                var t588 string = value__33.input
                                var t589 *ref_int_x = value__33.index
                                var t590 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t589)
                                var t591 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t588, t590)
                                var t592 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t591, 45)
                                jp582 = t592
                            }
                            jp570 = jp582
                        } else {
                            jp570 = false
                        }
                        if jp570 {
                            var t571 *ref_int_x = value__33.index
                            var t572 *ref_int_x = value__33.index
                            var t573 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t572)
                            var t574 int = t573 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t571, t574)
                        } else {}
                        var t565 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t566 bool = !t565
                        if t566 {
                            var t567 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t568 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t567,
                            }
                            retv545 = t568
                            return retv545
                        } else {
                            var t551 string = value__33.input
                            var t552 *ref_int_x = value__33.index
                            var t553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t552)
                            var t554 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t551, start__34, t553)
                            var t555 _goml_m_std_p_json_p_Value = Number{
                                _0: t554,
                            }
                            var t556 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t555,
                            }
                            retv545 = t556
                            return retv545
                        }
                    } else {
                        var t551 string = value__33.input
                        var t552 *ref_int_x = value__33.index
                        var t553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t552)
                        var t554 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t551, start__34, t553)
                        var t555 _goml_m_std_p_json_p_Value = Number{
                            _0: t554,
                        }
                        var t556 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t555,
                        }
                        retv545 = t556
                        return retv545
                    }
                }
            }
        } else {
            var t664 bool = _goml_m_std_p_json_p_parse__digits(value__33)
            var t665 bool = !t664
            if t665 {
                var t666 string = _goml_m_std_p_json_p_json__error(value__33, "expected number")
                var t667 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t666,
                }
                retv545 = t667
                return retv545
            } else {
                var t621 *ref_int_x = value__33.index
                var t622 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t621)
                var t623 string = value__33.input
                var t624 int = _goml_m_inherent_i_string_i_string_i_byte__len(t623)
                var t625 bool = t622 < t624
                var jp611 bool
                if t625 {
                    var t626 string = value__33.input
                    var t627 *ref_int_x = value__33.index
                    var t628 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t627)
                    var t629 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t626, t628)
                    var t630 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t629, 46)
                    jp611 = t630
                } else {
                    jp611 = false
                }
                if jp611 {
                    var t612 *ref_int_x = value__33.index
                    var t613 *ref_int_x = value__33.index
                    var t614 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t613)
                    var t615 int = t614 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t612, t615)
                    var t617 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t618 bool = !t617
                    if t618 {
                        var t619 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t620 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t619,
                        }
                        retv545 = t620
                        return retv545
                    } else {
                        var t593 *ref_int_x = value__33.index
                        var t594 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t593)
                        var t595 string = value__33.input
                        var t596 int = _goml_m_inherent_i_string_i_string_i_byte__len(t595)
                        var t597 bool = t594 < t596
                        var jp558 bool
                        if t597 {
                            var t600 string = value__33.input
                            var t601 *ref_int_x = value__33.index
                            var t602 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t601)
                            var t603 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t600, t602)
                            var t604 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t603, 101)
                            var jp599 bool
                            if t604 {
                                jp599 = true
                            } else {
                                var t605 string = value__33.input
                                var t606 *ref_int_x = value__33.index
                                var t607 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t606)
                                var t608 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t605, t607)
                                var t609 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t608, 69)
                                jp599 = t609
                            }
                            jp558 = jp599
                        } else {
                            jp558 = false
                        }
                        if jp558 {
                            var t559 *ref_int_x = value__33.index
                            var t560 *ref_int_x = value__33.index
                            var t561 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t560)
                            var t562 int = t561 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t559, t562)
                            var t576 *ref_int_x = value__33.index
                            var t577 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t576)
                            var t578 string = value__33.input
                            var t579 int = _goml_m_inherent_i_string_i_string_i_byte__len(t578)
                            var t580 bool = t577 < t579
                            var jp570 bool
                            if t580 {
                                var t583 string = value__33.input
                                var t584 *ref_int_x = value__33.index
                                var t585 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t584)
                                var t586 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t583, t585)
                                var t587 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t586, 43)
                                var jp582 bool
                                if t587 {
                                    jp582 = true
                                } else {
                                    var t588 string = value__33.input
                                    var t589 *ref_int_x = value__33.index
                                    var t590 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t589)
                                    var t591 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t588, t590)
                                    var t592 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t591, 45)
                                    jp582 = t592
                                }
                                jp570 = jp582
                            } else {
                                jp570 = false
                            }
                            if jp570 {
                                var t571 *ref_int_x = value__33.index
                                var t572 *ref_int_x = value__33.index
                                var t573 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t572)
                                var t574 int = t573 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t571, t574)
                            } else {}
                            var t565 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t566 bool = !t565
                            if t566 {
                                var t567 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t568 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t567,
                                }
                                retv545 = t568
                                return retv545
                            } else {
                                var t551 string = value__33.input
                                var t552 *ref_int_x = value__33.index
                                var t553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t552)
                                var t554 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t551, start__34, t553)
                                var t555 _goml_m_std_p_json_p_Value = Number{
                                    _0: t554,
                                }
                                var t556 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t555,
                                }
                                retv545 = t556
                                return retv545
                            }
                        } else {
                            var t551 string = value__33.input
                            var t552 *ref_int_x = value__33.index
                            var t553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t552)
                            var t554 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t551, start__34, t553)
                            var t555 _goml_m_std_p_json_p_Value = Number{
                                _0: t554,
                            }
                            var t556 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t555,
                            }
                            retv545 = t556
                            return retv545
                        }
                    }
                } else {
                    var t593 *ref_int_x = value__33.index
                    var t594 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t593)
                    var t595 string = value__33.input
                    var t596 int = _goml_m_inherent_i_string_i_string_i_byte__len(t595)
                    var t597 bool = t594 < t596
                    var jp558 bool
                    if t597 {
                        var t600 string = value__33.input
                        var t601 *ref_int_x = value__33.index
                        var t602 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t601)
                        var t603 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t600, t602)
                        var t604 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t603, 101)
                        var jp599 bool
                        if t604 {
                            jp599 = true
                        } else {
                            var t605 string = value__33.input
                            var t606 *ref_int_x = value__33.index
                            var t607 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t606)
                            var t608 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t605, t607)
                            var t609 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t608, 69)
                            jp599 = t609
                        }
                        jp558 = jp599
                    } else {
                        jp558 = false
                    }
                    if jp558 {
                        var t559 *ref_int_x = value__33.index
                        var t560 *ref_int_x = value__33.index
                        var t561 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t560)
                        var t562 int = t561 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t559, t562)
                        var t576 *ref_int_x = value__33.index
                        var t577 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t576)
                        var t578 string = value__33.input
                        var t579 int = _goml_m_inherent_i_string_i_string_i_byte__len(t578)
                        var t580 bool = t577 < t579
                        var jp570 bool
                        if t580 {
                            var t583 string = value__33.input
                            var t584 *ref_int_x = value__33.index
                            var t585 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t584)
                            var t586 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t583, t585)
                            var t587 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t586, 43)
                            var jp582 bool
                            if t587 {
                                jp582 = true
                            } else {
                                var t588 string = value__33.input
                                var t589 *ref_int_x = value__33.index
                                var t590 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t589)
                                var t591 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t588, t590)
                                var t592 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t591, 45)
                                jp582 = t592
                            }
                            jp570 = jp582
                        } else {
                            jp570 = false
                        }
                        if jp570 {
                            var t571 *ref_int_x = value__33.index
                            var t572 *ref_int_x = value__33.index
                            var t573 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t572)
                            var t574 int = t573 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t571, t574)
                        } else {}
                        var t565 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t566 bool = !t565
                        if t566 {
                            var t567 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t568 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t567,
                            }
                            retv545 = t568
                            return retv545
                        } else {
                            var t551 string = value__33.input
                            var t552 *ref_int_x = value__33.index
                            var t553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t552)
                            var t554 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t551, start__34, t553)
                            var t555 _goml_m_std_p_json_p_Value = Number{
                                _0: t554,
                            }
                            var t556 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t555,
                            }
                            retv545 = t556
                            return retv545
                        }
                    } else {
                        var t551 string = value__33.input
                        var t552 *ref_int_x = value__33.index
                        var t553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t552)
                        var t554 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t551, start__34, t553)
                        var t555 _goml_m_std_p_json_p_Value = Number{
                            _0: t554,
                        }
                        var t556 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t555,
                        }
                        retv545 = t556
                        return retv545
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__35 _goml_m_std_p_json_p_JsonParser, expected__36 string, result__37 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var retv679 _goml_m_Result____std_p_json_p_Value____string
    var t692 *ref_int_x = value__35.index
    var t693 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t692)
    var t694 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
    var t695 int = t693 + t694
    var t696 string = value__35.input
    var t697 int = _goml_m_inherent_i_string_i_string_i_byte__len(t696)
    var t698 bool = t695 <= t697
    var jp683 bool
    if t698 {
        var t699 string = value__35.input
        var t700 *ref_int_x = value__35.index
        var t701 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t700)
        var t702 *ref_int_x = value__35.index
        var t703 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t702)
        var t704 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t705 int = t703 + t704
        var t706 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t699, t701, t705)
        var t707 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t706, expected__36)
        jp683 = t707
    } else {
        jp683 = false
    }
    var jp681 _goml_m_Result____std_p_json_p_Value____string
    if jp683 {
        var t684 *ref_int_x = value__35.index
        var t685 *ref_int_x = value__35.index
        var t686 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t685)
        var t687 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t688 int = t686 + t687
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t684, t688)
        var t689 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__37,
        }
        jp681 = t689
    } else {
        var t690 string = _goml_m_std_p_json_p_json__error(value__35, "invalid literal")
        var t691 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t690,
        }
        jp681 = t691
    }
    retv679 = jp681
    return retv679
}

func _goml_m_std_p_json_p_parse__json__array(value__38 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv709 _goml_m_Result____std_p_json_p_Value____string
    var t710 *ref_int_x = value__38.index
    var t711 *ref_int_x = value__38.index
    var t712 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t711)
    var t713 int = t712 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t710, t713)
    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
    var result__39 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t768 *ref_int_x = value__38.index
    var t769 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t768)
    var t770 string = value__38.input
    var t771 int = _goml_m_inherent_i_string_i_string_i_byte__len(t770)
    var t772 bool = t769 < t771
    var jp761 bool
    if t772 {
        var t773 string = value__38.input
        var t774 *ref_int_x = value__38.index
        var t775 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t774)
        var t776 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t773, t775)
        var t777 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t776, 93)
        jp761 = t777
    } else {
        jp761 = false
    }
    if jp761 {
        var t762 *ref_int_x = value__38.index
        var t763 *ref_int_x = value__38.index
        var t764 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t763)
        var t765 int = t764 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t762, t765)
        var t766 _goml_m_std_p_json_p_Value = Array{
            _0: result__39,
        }
        var t767 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t766,
        }
        retv709 = t767
        return retv709
    } else {
        Loop_loop718:
        for {
            var t719 *ref_int_x = value__38.index
            var t720 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t719)
            var t721 string = value__38.input
            var t722 int = _goml_m_inherent_i_string_i_string_i_byte__len(t721)
            var t723 bool = t720 < t722
            if t723 {
                var mtmp48 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__38)
                switch mtmp48.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x49 _goml_m_std_p_json_p_Value = mtmp48.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    var item__40 _goml_m_std_p_json_p_Value = x49
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(result__39, item__40)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                    var t726 *ref_int_x = value__38.index
                    var t727 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t726)
                    var t728 string = value__38.input
                    var t729 int = _goml_m_inherent_i_string_i_string_i_byte__len(t728)
                    var t730 bool = t727 >= t729
                    if t730 {
                        var t731 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
                        var t732 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t731,
                        }
                        retv709 = t732
                        return retv709
                    } else {
                        var t734 string = value__38.input
                        var t735 *ref_int_x = value__38.index
                        var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                        var t737 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t734, t736)
                        var t738 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t737, 93)
                        if t738 {
                            var t739 *ref_int_x = value__38.index
                            var t740 *ref_int_x = value__38.index
                            var t741 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t740)
                            var t742 int = t741 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t739, t742)
                            var t743 _goml_m_std_p_json_p_Value = Array{
                                _0: result__39,
                            }
                            var t744 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t743,
                            }
                            retv709 = t744
                            return retv709
                        } else {
                            var t746 string = value__38.input
                            var t747 *ref_int_x = value__38.index
                            var t748 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t747)
                            var t749 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t746, t748)
                            var t750 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t749, 44)
                            if t750 {
                                var t751 *ref_int_x = value__38.index
                                var t752 *ref_int_x = value__38.index
                                var t753 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t752)
                                var t754 int = t753 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t751, t754)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                                continue
                            } else {
                                var t756 string = _goml_m_std_p_json_p_json__error(value__38, "expected array separator")
                                var t757 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t756,
                                }
                                retv709 = t757
                                return retv709
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x50 string = mtmp48.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var error__41 string = x50
                    var t759 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__41,
                    }
                    retv709 = t759
                    return retv709
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop718
            }
        }
        var t716 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
        var t717 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t716,
        }
        retv709 = t717
        return retv709
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__42 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv779 _goml_m_Result____std_p_json_p_Value____string
    var t780 *ref_int_x = value__42.index
    var t781 *ref_int_x = value__42.index
    var t782 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t781)
    var t783 int = t782 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t780, t783)
    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
    var result__43 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t863 *ref_int_x = value__42.index
    var t864 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t863)
    var t865 string = value__42.input
    var t866 int = _goml_m_inherent_i_string_i_string_i_byte__len(t865)
    var t867 bool = t864 < t866
    var jp856 bool
    if t867 {
        var t868 string = value__42.input
        var t869 *ref_int_x = value__42.index
        var t870 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t869)
        var t871 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t868, t870)
        var t872 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t871, 125)
        jp856 = t872
    } else {
        jp856 = false
    }
    if jp856 {
        var t857 *ref_int_x = value__42.index
        var t858 *ref_int_x = value__42.index
        var t859 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t858)
        var t860 int = t859 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t857, t860)
        var t861 _goml_m_std_p_json_p_Value = Object{
            _0: result__43,
        }
        var t862 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t861,
        }
        retv779 = t862
        return retv779
    } else {
        Loop_loop788:
        for {
            var t789 *ref_int_x = value__42.index
            var t790 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t789)
            var t791 string = value__42.input
            var t792 int = _goml_m_inherent_i_string_i_string_i_byte__len(t791)
            var t793 bool = t790 < t792
            if t793 {
                var mtmp60 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__42)
                var jp795 string
                switch mtmp60.(type) {
                case Result__string__string_Ok:
                    var x61 string = mtmp60.(Result__string__string_Ok)._0
                    var name__44 string = x61
                    jp795 = name__44
                    var name__46 string = jp795
                    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                    var t843 *ref_int_x = value__42.index
                    var t844 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t843)
                    var t845 string = value__42.input
                    var t846 int = _goml_m_inherent_i_string_i_string_i_byte__len(t845)
                    var t847 bool = t844 >= t846
                    var jp835 bool
                    if t847 {
                        jp835 = true
                    } else {
                        var t848 string = value__42.input
                        var t849 *ref_int_x = value__42.index
                        var t850 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t849)
                        var t851 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t848, t850)
                        var t852 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t851, 58)
                        var t853 bool = !t852
                        jp835 = t853
                    }
                    if jp835 {
                        var t836 string = _goml_m_std_p_json_p_json__error(value__42, "expected object colon")
                        var t837 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t836,
                        }
                        retv779 = t837
                        return retv779
                    } else {
                        var t838 *ref_int_x = value__42.index
                        var t839 *ref_int_x = value__42.index
                        var t840 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t839)
                        var t841 int = t840 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t838, t841)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                        var mtmp66 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__42)
                        switch mtmp66.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x67 _goml_m_std_p_json_p_Value = mtmp66.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            var item__47 _goml_m_std_p_json_p_Value = x67
                            var t831 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: name__46,
                                _1: item__47,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__43, t831)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                            var t799 *ref_int_x = value__42.index
                            var t800 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t799)
                            var t801 string = value__42.input
                            var t802 int = _goml_m_inherent_i_string_i_string_i_byte__len(t801)
                            var t803 bool = t800 >= t802
                            if t803 {
                                var t804 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
                                var t805 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t804,
                                }
                                retv779 = t805
                                return retv779
                            } else {
                                var t807 string = value__42.input
                                var t808 *ref_int_x = value__42.index
                                var t809 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t808)
                                var t810 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t807, t809)
                                var t811 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t810, 125)
                                if t811 {
                                    var t812 *ref_int_x = value__42.index
                                    var t813 *ref_int_x = value__42.index
                                    var t814 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t813)
                                    var t815 int = t814 + 1
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t812, t815)
                                    var t816 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__43,
                                    }
                                    var t817 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t816,
                                    }
                                    retv779 = t817
                                    return retv779
                                } else {
                                    var t819 string = value__42.input
                                    var t820 *ref_int_x = value__42.index
                                    var t821 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t820)
                                    var t822 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t819, t821)
                                    var t823 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t822, 44)
                                    if t823 {
                                        var t824 *ref_int_x = value__42.index
                                        var t825 *ref_int_x = value__42.index
                                        var t826 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t825)
                                        var t827 int = t826 + 1
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t824, t827)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                                        continue
                                    } else {
                                        var t829 string = _goml_m_std_p_json_p_json__error(value__42, "expected object separator")
                                        var t830 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t829,
                                        }
                                        retv779 = t830
                                        return retv779
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x68 string = mtmp66.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var error__48 string = x68
                            var t833 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: error__48,
                            }
                            retv779 = t833
                            return retv779
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x62 string = mtmp60.(Result__string__string_Err)._0
                    var error__45 string = x62
                    var t854 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__45,
                    }
                    retv779 = t854
                    return retv779
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop788
            }
        }
        var t786 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
        var t787 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t786,
        }
        retv779 = t787
        return retv779
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__49 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv874 _goml_m_Result____std_p_json_p_Value____string
    _goml_m_std_p_json_p_skip__json__whitespace(value__49)
    var t877 *ref_int_x = value__49.index
    var t878 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t877)
    var t879 string = value__49.input
    var t880 int = _goml_m_inherent_i_string_i_string_i_byte__len(t879)
    var t881 bool = t878 >= t880
    var jp876 _goml_m_Result____std_p_json_p_Value____string
    if t881 {
        var t882 string = _goml_m_std_p_json_p_json__error(value__49, "expected JSON value")
        var t883 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t882,
        }
        jp876 = t883
    } else {
        var t884 string = value__49.input
        var t885 *ref_int_x = value__49.index
        var t886 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t885)
        var mtmp75 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t884, t886)
        var jp888 _goml_m_Result____std_p_json_p_Value____string
        switch mtmp75 {
        case 123:
            var t889 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__49)
            jp888 = t889
        case 91:
            var t890 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__49)
            jp888 = t890
        case 34:
            var mtmp76 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__49)
            var jp892 _goml_m_Result____std_p_json_p_Value____string
            switch mtmp76.(type) {
            case Result__string__string_Ok:
                var x77 string = mtmp76.(Result__string__string_Ok)._0
                var text__50 string = x77
                var t893 _goml_m_std_p_json_p_Value = String{
                    _0: text__50,
                }
                var t894 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t893,
                }
                jp892 = t894
            case Result__string__string_Err:
                var x78 string = mtmp76.(Result__string__string_Err)._0
                var error__51 string = x78
                var t895 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: error__51,
                }
                jp892 = t895
            default:
                panic("non-exhaustive match")
            }
            jp888 = jp892
        case 116:
            var t896 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t897 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "true", t896)
            jp888 = t897
        case 102:
            var t898 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t899 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "false", t898)
            jp888 = t899
        case 110:
            var t900 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "null", Null{})
            jp888 = t900
        default:
            var byte__52 uint8 = mtmp75
            var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 45)
            var jp904 bool
            if t908 {
                jp904 = true
            } else {
                var t909 bool = _goml_m_std_p_json_p_json__digit(byte__52)
                jp904 = t909
            }
            var jp902 _goml_m_Result____std_p_json_p_Value____string
            if jp904 {
                var t905 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__49)
                jp902 = t905
            } else {
                var t906 string = _goml_m_std_p_json_p_json__error(value__49, "unexpected JSON token")
                var t907 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t906,
                }
                jp902 = t907
            }
            jp888 = jp902
        }
        jp876 = jp888
    }
    retv874 = jp876
    return retv874
}

func _goml_m_std_p_json_p_parse(input__53 string) _goml_m_Result____std_p_json_p_Value____string {
    var retv911 _goml_m_Result____std_p_json_p_Value____string
    var parser__54 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_json__parser(input__53)
    var mtmp79 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__54)
    var jp913 _goml_m_std_p_json_p_Value
    switch mtmp79.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x80 _goml_m_std_p_json_p_Value = mtmp79.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__55 _goml_m_std_p_json_p_Value = x80
        jp913 = value__55
        var result__57 _goml_m_std_p_json_p_Value = jp913
        _goml_m_std_p_json_p_skip__json__whitespace(parser__54)
        var t916 *ref_int_x = parser__54.index
        var t917 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t916)
        var t918 int = _goml_m_inherent_i_string_i_string_i_byte__len(input__53)
        var t919 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t917, t918)
        var jp915 _goml_m_Result____std_p_json_p_Value____string
        if t919 {
            var t920 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: result__57,
            }
            jp915 = t920
        } else {
            var t921 string = _goml_m_std_p_json_p_json__error(parser__54, "trailing JSON data")
            var t922 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t921,
            }
            jp915 = t922
        }
        retv911 = jp915
        return retv911
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x81 string = mtmp79.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var error__56 string = x81
        var t923 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: error__56,
        }
        retv911 = t923
        return retv911
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_json__hex__digit(value__58 uint8) rune {
    var retv925 rune
    var t926 int = int(uint8(value__58))
    var t927 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", t926)
    retv925 = t927
    return retv925
}

func _goml_m_std_p_json_p_write__json__string(builder__59 _goml_m_std_p_text_p_StringBuilder, value__60 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, 34)
    var start__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var for_index84 int = 0
    var for_limit85 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    Loop_loop939:
    for {
        var t940 bool = for_index84 < for_limit85
        if t940 {
            var for_item86 int = for_index84
            var t941 int = for_index84 + 1
            for_index84 = t941
            var index__62 int = for_item86
            var byte__63 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__60, index__62)
            var t997 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
            var jp995 bool
            if t997 {
                jp995 = true
            } else {
                var t998 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                jp995 = t998
            }
            var jp992 bool
            if jp995 {
                jp992 = true
            } else {
                var t996 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                jp992 = t996
            }
            var jp989 bool
            if jp992 {
                jp989 = true
            } else {
                var t993 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                jp989 = t993
            }
            var jp986 bool
            if jp989 {
                jp986 = true
            } else {
                var t990 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                jp986 = t990
            }
            var jp983 bool
            if jp986 {
                jp983 = true
            } else {
                var t987 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                jp983 = t987
            }
            var jp980 bool
            if jp983 {
                jp980 = true
            } else {
                var t984 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                jp980 = t984
            }
            var jp944 bool
            if jp980 {
                jp944 = true
            } else {
                var t981 bool = byte__63 < 32
                jp944 = t981
            }
            if jp944 {
                var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                var t975 bool = t974 < index__62
                if t975 {
                    var t976 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                    var t977 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t976, index__62)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t977)
                } else {}
                var t949 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
                if t949 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\"")
                } else {
                    var t952 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                    if t952 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\\")
                    } else {
                        var t955 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                        if t955 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\b")
                        } else {
                            var t958 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                            if t958 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\t")
                            } else {
                                var t961 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                                if t961 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\n")
                                } else {
                                    var t964 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                                    if t964 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\f")
                                    } else {
                                        var t967 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                                        if t967 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\u00")
                                            var t969 uint8 = byte__63 / 16
                                            var t970 rune = _goml_m_std_p_json_p_json__hex__digit(t969)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t970)
                                            var t971_rhs uint8 = 16
                                            var t971 uint8 = byte__63 % t971_rhs
                                            var t972 rune = _goml_m_std_p_json_p_json__hex__digit(t971)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t972)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t947 int = index__62 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(start__61, t947)
            } else {}
            continue
        } else {
            break Loop_loop939
        }
    }
    var t932 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
    var t933 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    var t934 bool = t932 < t933
    if t934 {
        var t935 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
        var t936 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
        var t937 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t935, t936)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t937)
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
        Loop_loop1003:
        for {
            var t1004 bool = for_index102 < for_limit101
            if t1004 {
                var for_item103 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source100, for_index102)
                var t1005 int = for_index102 + 1
                for_index102 = t1005
                var field__68 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item103
                var t1012 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                var t1013 bool = t1012 > 0
                if t1013 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                } else {}
                var t1007 string = field__68._0
                _goml_m_std_p_json_p_write__json__string(builder__64, t1007)
                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 58)
                var t1008 _goml_m_std_p_json_p_Value = field__68._1
                _goml_m_std_p_json_p_write__json__value(builder__64, t1008)
                var t1009 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                var t1010 int = t1009 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__67, t1010)
                continue
            } else {
                break Loop_loop1003
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
        Loop_loop1017:
        for {
            var t1018 bool = for_index113 < for_limit112
            if t1018 {
                var for_item114 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(for_source111, for_index113)
                var t1019 int = for_index113 + 1
                for_index113 = t1019
                var item__71 _goml_m_std_p_json_p_Value = for_item114
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
            } else {
                break Loop_loop1017
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
        var x120 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__77.(Object)._0
        var fields__79 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x120
        var result__80 *ref__goml_m_Option____std_p_json_p_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(_goml_m_Option____std_p_json_p_Value_None{})
        var for_source125 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = fields__79
        var for_limit126 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source125)
        var for_index127 int = 0
        Loop_loop1042:
        for {
            var t1043 bool = for_index127 < for_limit126
            if t1043 {
                var for_item128 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source125, for_index127)
                var t1044 int = for_index127 + 1
                for_index127 = t1044
                var field__81 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item128
                var t1046 string = field__81._0
                var t1047 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1046, name__78)
                if t1047 {
                    var t1048 _goml_m_std_p_json_p_Value = field__81._1
                    var t1049 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1048,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(result__80, t1049)
                } else {}
                continue
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
    var retv1052 Option__string
    var jp1054 Option__string
    switch value__82.(type) {
    case String:
        var x133 string = value__82.(String)._0
        var text__83 string = x133
        var t1055 Option__string = Option__string_Some{
            _0: text__83,
        }
        jp1054 = t1055
    default:
        jp1054 = Option__string_None{}
    }
    retv1052 = jp1054
    return retv1052
}

func _goml_m_std_p_json_p_parse__json__int__text(value__84 string) Option__int {
    var retv1057 Option__int
    var t1060 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
    var t1061 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1060, 0)
    var jp1059 Option__int
    if t1061 {
        jp1059 = Option__int_None{}
        retv1057 = jp1059
        return retv1057
    } else {
        var t1062 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, 0)
        var negative__85 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1062, 45)
        var jp1064 int
        if negative__85 {
            jp1064 = 1
        } else {
            jp1064 = 0
        }
        var index__86 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(jp1064)
        var result__87 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var t1092 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
        var t1093 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1094 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1092, t1093)
        if t1094 {
            retv1057 = Option__int_None{}
            return retv1057
        } else {
            Loop_loop1073:
            for {
                var t1074 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                var t1075 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                var t1076 bool = t1074 < t1075
                if t1076 {
                    var t1077 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                    var byte__88 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1077)
                    var t1090 bool = byte__88 < 48
                    var jp1083 bool
                    if t1090 {
                        jp1083 = true
                    } else {
                        var t1091 bool = byte__88 > 57
                        jp1083 = t1091
                    }
                    if jp1083 {
                        retv1057 = Option__int_None{}
                        return retv1057
                    } else {
                        var t1084 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                        var t1085 int = t1084 * 10
                        var t1086 uint8 = byte__88 - 48
                        var t1087 int = int(uint8(t1086))
                        var t1088 int = t1085 + t1087
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__87, t1088)
                        var t1079 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                        var t1080 int = t1079 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__86, t1080)
                        continue
                    }
                } else {
                    break Loop_loop1073
                }
            }
            var jp1068 int
            if negative__85 {
                var t1070 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                var t1071 int = 0 - t1070
                jp1068 = t1071
            } else {
                var t1072 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                jp1068 = t1072
            }
            var t1069 Option__int = Option__int_Some{
                _0: jp1068,
            }
            jp1059 = t1069
            retv1057 = jp1059
            return retv1057
        }
    }
}

func _goml_m_std_p_json_p_as__int(value__89 _goml_m_std_p_json_p_Value) Option__int {
    var retv1096 Option__int
    var jp1098 Option__int
    switch value__89.(type) {
    case Number:
        var x142 string = value__89.(Number)._0
        var number__90 string = x142
        var t1099 Option__int = _goml_m_std_p_json_p_parse__json__int__text(number__90)
        jp1098 = t1099
    default:
        jp1098 = Option__int_None{}
    }
    retv1096 = jp1098
    return retv1096
}

func _goml_m_std_p_json_p_as__bool(value__91 _goml_m_std_p_json_p_Value) Option__bool {
    var retv1101 Option__bool
    var jp1103 Option__bool
    switch value__91.(type) {
    case Bool:
        var x148 bool = value__91.(Bool)._0
        var result__92 bool = x148
        var t1104 Option__bool = Option__bool_Some{
            _0: result__92,
        }
        jp1103 = t1104
    default:
        jp1103 = Option__bool_None{}
    }
    retv1101 = jp1103
    return retv1101
}

func main0() struct{} {
    var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1115 _goml_m_std_p_json_p_Value
    switch mtmp68.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__0 _goml_m_std_p_json_p_Value = x69
        jp1115 = value__0
        var value__2 _goml_m_std_p_json_p_Value = jp1115
        var mtmp72 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "name")
        switch mtmp72.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing name")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x73 _goml_m_std_p_json_p_Value = mtmp72.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__3 _goml_m_std_p_json_p_Value = x73
            var mtmp74 Option__string = _goml_m_std_p_json_p_as__string(field__3)
            switch mtmp74.(type) {
            case Option__string_None:
                println__T_string("invalid name")
            case Option__string_Some:
                var x75 string = mtmp74.(Option__string_Some)._0
                var name__4 string = x75
                println__T_string(name__4)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var mtmp77 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "version")
        switch mtmp77.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing version")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x78 _goml_m_std_p_json_p_Value = mtmp77.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__5 _goml_m_std_p_json_p_Value = x78
            var mtmp79 Option__int = _goml_m_std_p_json_p_as__int(field__5)
            switch mtmp79.(type) {
            case Option__int_None:
                println__T_string("invalid version")
            case Option__int_Some:
                var x80 int = mtmp79.(Option__int_Some)._0
                var version__6 int = x80
                println__T_int(version__6)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var mtmp82 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(value__2, "stable")
        switch mtmp82.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            println__T_string("missing stable")
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x83 _goml_m_std_p_json_p_Value = mtmp82.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var field__7 _goml_m_std_p_json_p_Value = x83
            var mtmp84 Option__bool = _goml_m_std_p_json_p_as__bool(field__7)
            switch mtmp84.(type) {
            case Option__bool_None:
                println__T_string("invalid stable")
            case Option__bool_Some:
                var x85 bool = mtmp84.(Option__bool_Some)._0
                var stable__8 bool = x85
                println__T_bool(stable__8)
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
        var t1119 string = _goml_m_std_p_json_p_encode(value__2)
        println__T_string(t1119)
        return struct{}{}
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var error__1 string = x70
        println__T_string(error__1)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var retv1134 *_goml_vec_uint8
    var t1135 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv1134 = t1135
    return retv1134
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__126 *_goml_vec_uint8, elem__127 uint8) struct{} {
    vec_push__Vec_5uint8(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__uint8(self__140 *_goml_vec_uint8, additional__141 int) struct{} {
    vec_reserve__Vec_5uint8(self__140, additional__141)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv1177 bool
    var t1178 bool = self__59 == other__60
    retv1177 = t1178
    return retv1177
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv1180 int
    var t1181 int = _goml_runtime_core_string_len(self__9)
    retv1180 = t1181
    return retv1180
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv1183 uint8
    var t1184 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv1183 = t1184
    return retv1183
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv1186 string
    var t1187 string = _goml_runtime_core_char_to_string(self__7)
    retv1186 = t1187
    return retv1186
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv1189 *ref_int_x
    var t1190 *ref_int_x = ref__Ref_3int(value__207)
    retv1189 = t1190
    return retv1189
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv1192 int
    var t1193 int = ref_get__Ref_3int(self__208)
    retv1192 = t1193
    return retv1192
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv1195 string
    var t1196 string = _goml_runtime_core_int_to_string(self__5)
    retv1195 = t1196
    return retv1195
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv1198 bool
    var t1199 bool = self__69 == other__70
    retv1198 = t1199
    return retv1198
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__207 uint32) *ref_uint32_x {
    var retv1203 *ref_uint32_x
    var t1204 *ref_uint32_x = ref__Ref_6uint32(value__207)
    retv1203 = t1204
    return retv1203
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__208 *ref_uint32_x) uint32 {
    var retv1206 uint32
    var t1207 uint32 = ref_get__Ref_6uint32(self__208)
    retv1206 = t1207
    return retv1206
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(self__209 *ref_uint32_x, value__210 uint32) struct{} {
    ref_set__Ref_6uint32(self__209, value__210)
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv1211 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp1213 Option__char
    if valid__3 {
        var t1214 Option__char = Option__char_Some{
            _0: value__4,
        }
        jp1213 = t1214
    } else {
        jp1213 = Option__char_None{}
    }
    retv1211 = jp1213
    return retv1211
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv1216 string
    var t1217 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv1216 = t1217
    return retv1216
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv1219 bool
    var t1220 bool = self__55 == other__56
    retv1219 = t1220
    return retv1219
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var retv1222 *_goml_vec__goml_m_std_p_json_p_Value
    var t1223 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    retv1222 = t1223
    return retv1222
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__126 *_goml_vec__goml_m_std_p_json_p_Value, elem__127 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var retv1227 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
    var t1228 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    retv1227 = t1228
    return retv1227
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__126 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__127 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv1232 rune
    var t1233 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv1232 = t1233
    return retv1232
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(value__207 _goml_m_Option____std_p_json_p_Value) *ref__goml_m_Option____std_p_json_p_Value_x {
    var retv1235 *ref__goml_m_Option____std_p_json_p_Value_x
    var t1236 *ref__goml_m_Option____std_p_json_p_Value_x = ref___goml_m_Ref__24Option____std_p_json_p_Value(value__207)
    retv1235 = t1236
    return retv1235
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(self__209 *ref__goml_m_Option____std_p_json_p_Value_x, value__210 _goml_m_Option____std_p_json_p_Value) struct{} {
    ref_set___goml_m_Ref__24Option____std_p_json_p_Value(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(self__208 *ref__goml_m_Option____std_p_json_p_Value_x) _goml_m_Option____std_p_json_p_Value {
    var retv1240 _goml_m_Option____std_p_json_p_Value
    var t1241 _goml_m_Option____std_p_json_p_Value = ref_get___goml_m_Ref__24Option____std_p_json_p_Value(self__208)
    retv1240 = t1241
    return retv1240
}

func println__T_string(value__1 string) struct{} {
    var t1243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t1243)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t1246 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t1246)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t1249 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t1249)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv1252 string
    retv1252 = self__38
    return retv1252
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv1254 string
    var t1255 string = _goml_runtime_core_int_to_string(self__40)
    retv1254 = t1255
    return retv1254
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv1257 string
    var t1258 string = _goml_runtime_core_bool_to_string(self__37)
    retv1257 = t1258
    return retv1257
}

func main() {
    main0()
}
