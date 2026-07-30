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
    var retv148 _goml_m_std_p_text_p_StringBuilder
    var t149 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var t150 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: t149,
    }
    retv148 = t150
    return retv148
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var t165 *_goml_vec_uint8 = self__3.values
    var t166 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__uint8(t165, t166)
    var for_index1 int = 0
    var for_limit2 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__4)
    Loop_loop168:
    for {
        var t169 bool = for_index1 < for_limit2
        if t169 {
            var for_item3 int = for_index1
            var t170 int = for_index1 + 1
            for_index1 = t170
            var index__5 int = for_item3
            var t171 *_goml_vec_uint8 = self__3.values
            var t172 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__4, index__5)
            _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(t171, t172)
            continue
        } else {
            break Loop_loop168
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__6 _goml_m_std_p_text_p_StringBuilder, value__7 rune) struct{} {
    var t175 string = _goml_m_inherent_i_char_i_char_i_to__string(value__7)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__6, t175)
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(self__15 _goml_m_std_p_text_p_StringBuilder) string {
    var retv189 string
    var t190 *_goml_vec_uint8 = self__15.values
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t190)
    var x12 string = mtmp10._1
    var value__16 string = x12
    retv189 = value__16
    return retv189
}

func _goml_m_std_p_json_p_json__parser(input__0 string) _goml_m_std_p_json_p_JsonParser {
    var retv195 _goml_m_std_p_json_p_JsonParser
    var t196 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var t197 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__0,
        index: t196,
    }
    retv195 = t197
    return retv195
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var retv199 string
    var t200 string = message__2 + " at byte "
    var t201 *ref_int_x = value__1.index
    var t202 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t201)
    var t203 string = _goml_m_inherent_i_int_i_int_i_to__string(t202)
    var t204 string = t200 + t203
    retv199 = t204
    return retv199
}

func _goml_m_std_p_json_p_json__whitespace(value__3 uint8) bool {
    var retv206 bool
    var t215 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 9)
    var jp213 bool
    if t215 {
        jp213 = true
    } else {
        var t216 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 10)
        jp213 = t216
    }
    var jp210 bool
    if jp213 {
        jp210 = true
    } else {
        var t214 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 13)
        jp210 = t214
    }
    var jp208 bool
    if jp210 {
        jp208 = true
    } else {
        var t211 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(value__3, 32)
        jp208 = t211
    }
    retv206 = jp208
    return retv206
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop219:
    for {
        var t227 *ref_int_x = value__4.index
        var t228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t227)
        var t229 string = value__4.input
        var t230 int = _goml_m_inherent_i_string_i_string_i_byte__len(t229)
        var t231 bool = t228 < t230
        var jp221 bool
        if t231 {
            var t232 string = value__4.input
            var t233 *ref_int_x = value__4.index
            var t234 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t233)
            var t235 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t232, t234)
            var t236 bool = _goml_m_std_p_json_p_json__whitespace(t235)
            jp221 = t236
        } else {
            jp221 = false
        }
        if jp221 {
            var t222 *ref_int_x = value__4.index
            var t223 *ref_int_x = value__4.index
            var t224 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t223)
            var t225 int = t224 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t222, t225)
            continue
        } else {
            break Loop_loop219
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var retv238 Option__uint32
    var t264 bool = value__5 >= 48
    var jp242 bool
    if t264 {
        var t265 bool = value__5 <= 57
        jp242 = t265
    } else {
        jp242 = false
    }
    var jp240 Option__uint32
    if jp242 {
        var t243 uint8 = value__5 - 48
        var t244 uint32 = uint32(uint8(t243))
        var t245 Option__uint32 = Option__uint32_Some{
            _0: t244,
        }
        jp240 = t245
    } else {
        var t262 bool = value__5 >= 65
        var jp249 bool
        if t262 {
            var t263 bool = value__5 <= 70
            jp249 = t263
        } else {
            jp249 = false
        }
        var jp247 Option__uint32
        if jp249 {
            var t250 uint8 = value__5 - 55
            var t251 uint32 = uint32(uint8(t250))
            var t252 Option__uint32 = Option__uint32_Some{
                _0: t251,
            }
            jp247 = t252
        } else {
            var t260 bool = value__5 >= 97
            var jp256 bool
            if t260 {
                var t261 bool = value__5 <= 102
                jp256 = t261
            } else {
                jp256 = false
            }
            var jp254 Option__uint32
            if jp256 {
                var t257 uint8 = value__5 - 87
                var t258 uint32 = uint32(uint8(t257))
                var t259 Option__uint32 = Option__uint32_Some{
                    _0: t258,
                }
                jp254 = t259
            } else {
                jp254 = Option__uint32_None{}
            }
            jp247 = jp254
        }
        jp240 = jp247
    }
    retv238 = jp240
    return retv238
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var retv267 Result__uint32__string
    var t270 *ref_int_x = value__6.index
    var t271 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t270)
    var t272 int = t271 + 4
    var t273 string = value__6.input
    var t274 int = _goml_m_inherent_i_string_i_string_i_byte__len(t273)
    var t275 bool = t272 > t274
    var jp269 Result__uint32__string
    if t275 {
        var t276 string = _goml_m_std_p_json_p_json__error(value__6, "incomplete unicode escape")
        var t277 Result__uint32__string = Result__uint32__string_Err{
            _0: t276,
        }
        jp269 = t277
        retv267 = jp269
        return retv267
    } else {
        var t278_source int = 0
        var t278 uint32 = uint32(int(t278_source))
        var result__7 *ref_uint32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(t278)
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop286:
        for {
            var t287 bool = for_index0 < for_limit1
            if t287 {
                var for_item2 int = for_index0
                var t288 int = for_index0 + 1
                for_index0 = t288
                var offset__8 int = for_item2
                var t289 string = value__6.input
                var t290 *ref_int_x = value__6.index
                var t291 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t290)
                var t292 int = t291 + offset__8
                var t293 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t289, t292)
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t293)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t295 string = _goml_m_std_p_json_p_json__error(value__6, "invalid unicode escape")
                    var t296 Result__uint32__string = Result__uint32__string_Err{
                        _0: t295,
                    }
                    retv267 = t296
                    return retv267
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var digit__9 uint32 = x5
                    var t297 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
                    var t298 uint32 = t297 * 16
                    var t299 uint32 = t298 + digit__9
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(result__7, t299)
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop286
            }
        }
        var t280 *ref_int_x = value__6.index
        var t281 *ref_int_x = value__6.index
        var t282 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t281)
        var t283 int = t282 + 4
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t280, t283)
        var t284 uint32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(result__7)
        var t285 Result__uint32__string = Result__uint32__string_Ok{
            _0: t284,
        }
        jp269 = t285
        retv267 = jp269
        return retv267
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var retv302 Result__unit__string
    var mtmp8 Option__char = char_from_uint32(codepoint__12)
    var jp304 Result__unit__string
    switch mtmp8.(type) {
    case Option__char_None:
        var t305 string = _goml_m_std_p_json_p_json__error(value__10, "invalid unicode codepoint")
        var t306 Result__unit__string = Result__unit__string_Err{
            _0: t305,
        }
        jp304 = t306
    case Option__char_Some:
        var x9 rune = mtmp8.(Option__char_Some)._0
        var character__13 rune = x9
        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__11, character__13)
        var t307 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp304 = t307
    default:
        panic("non-exhaustive match")
    }
    retv302 = jp304
    return retv302
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var retv309 Result__unit__string
    var mtmp11 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp311 uint32
    switch mtmp11.(type) {
    case Result__uint32__string_Ok:
        var x12 uint32 = mtmp11.(Result__uint32__string_Ok)._0
        var codepoint__16 uint32 = x12
        jp311 = codepoint__16
        var first__18 uint32 = jp311
        var t373 bool = first__18 >= 55296
        var jp315 bool
        if t373 {
            var t374 bool = first__18 <= 56319
            jp315 = t374
        } else {
            jp315 = false
        }
        var jp313 Result__unit__string
        if jp315 {
            var t352 *ref_int_x = value__14.index
            var t353 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t352)
            var t354 int = t353 + 2
            var t355 string = value__14.input
            var t356 int = _goml_m_inherent_i_string_i_string_i_byte__len(t355)
            var t357 bool = t354 > t356
            var jp344 bool
            if t357 {
                jp344 = true
            } else {
                var t358 string = value__14.input
                var t359 *ref_int_x = value__14.index
                var t360 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t359)
                var t361 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t358, t360)
                var t362 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t361, 92)
                var t363 bool = !t362
                jp344 = t363
            }
            var jp319 bool
            if jp344 {
                jp319 = true
            } else {
                var t345 string = value__14.input
                var t346 *ref_int_x = value__14.index
                var t347 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t346)
                var t348 int = t347 + 1
                var t349 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t345, t348)
                var t350 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t349, 117)
                var t351 bool = !t350
                jp319 = t351
            }
            var jp317 Result__unit__string
            if jp319 {
                var t320 string = _goml_m_std_p_json_p_json__error(value__14, "missing low surrogate")
                var t321 Result__unit__string = Result__unit__string_Err{
                    _0: t320,
                }
                jp317 = t321
                jp313 = jp317
                retv309 = jp313
                return retv309
            } else {
                var t322 *ref_int_x = value__14.index
                var t323 *ref_int_x = value__14.index
                var t324 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t323)
                var t325 int = t324 + 2
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t322, t325)
                var mtmp15 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp327 uint32
                switch mtmp15.(type) {
                case Result__uint32__string_Ok:
                    var x16 uint32 = mtmp15.(Result__uint32__string_Ok)._0
                    var codepoint__19 uint32 = x16
                    jp327 = codepoint__19
                    var second__21 uint32 = jp327
                    var t340 bool = second__21 < 56320
                    var jp331 bool
                    if t340 {
                        jp331 = true
                    } else {
                        var t341 bool = second__21 > 57343
                        jp331 = t341
                    }
                    var jp329 Result__unit__string
                    if jp331 {
                        var t332 string = _goml_m_std_p_json_p_json__error(value__14, "invalid low surrogate")
                        var t333 Result__unit__string = Result__unit__string_Err{
                            _0: t332,
                        }
                        jp329 = t333
                    } else {
                        var t334 uint32 = first__18 - 55296
                        var t335 uint32 = t334 * 1024
                        var t336 uint32 = 65536 + t335
                        var t337 uint32 = t336 + second__21
                        var t338 uint32 = t337 - 56320
                        var t339 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, t338)
                        jp329 = t339
                    }
                    jp317 = jp329
                    jp313 = jp317
                    retv309 = jp313
                    return retv309
                case Result__uint32__string_Err:
                    var x17 string = mtmp15.(Result__uint32__string_Err)._0
                    var error__20 string = x17
                    var t342 Result__unit__string = Result__unit__string_Err{
                        _0: error__20,
                    }
                    retv309 = t342
                    return retv309
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t371 bool = first__18 >= 56320
            var jp367 bool
            if t371 {
                var t372 bool = first__18 <= 57343
                jp367 = t372
            } else {
                jp367 = false
            }
            var jp365 Result__unit__string
            if jp367 {
                var t368 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t369 Result__unit__string = Result__unit__string_Err{
                    _0: t368,
                }
                jp365 = t369
            } else {
                var t370 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, first__18)
                jp365 = t370
            }
            jp313 = jp365
            retv309 = jp313
            return retv309
        }
    case Result__uint32__string_Err:
        var x13 string = mtmp11.(Result__uint32__string_Err)._0
        var error__17 string = x13
        var t375 Result__unit__string = Result__unit__string_Err{
            _0: error__17,
        }
        retv309 = t375
        return retv309
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__22 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var retv377 Result__string__string
    var t498 *ref_int_x = value__22.index
    var t499 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t498)
    var t500 string = value__22.input
    var t501 int = _goml_m_inherent_i_string_i_string_i_byte__len(t500)
    var t502 bool = t499 >= t501
    var jp490 bool
    if t502 {
        jp490 = true
    } else {
        var t503 string = value__22.input
        var t504 *ref_int_x = value__22.index
        var t505 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t504)
        var t506 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t503, t505)
        var t507 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t506, 34)
        var t508 bool = !t507
        jp490 = t508
    }
    if jp490 {
        var t491 string = _goml_m_std_p_json_p_json__error(value__22, "expected string")
        var t492 Result__string__string = Result__string__string_Err{
            _0: t491,
        }
        retv377 = t492
        return retv377
    } else {
        var t493 *ref_int_x = value__22.index
        var t494 *ref_int_x = value__22.index
        var t495 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t494)
        var t496 int = t495 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t493, t496)
        var builder__23 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t379 *ref_int_x = value__22.index
        var t380 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t379)
        var segment__24 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(t380)
        Loop_loop384:
        for {
            var t385 *ref_int_x = value__22.index
            var t386 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t385)
            var t387 string = value__22.input
            var t388 int = _goml_m_inherent_i_string_i_string_i_byte__len(t387)
            var t389 bool = t386 < t388
            if t389 {
                var t390 string = value__22.input
                var t391 *ref_int_x = value__22.index
                var t392 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t391)
                var byte__25 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t390, t392)
                var t394 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 34)
                if t394 {
                    var t402 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                    var t403 *ref_int_x = value__22.index
                    var t404 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t403)
                    var t405 bool = t402 < t404
                    if t405 {
                        var t406 string = value__22.input
                        var t407 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t408 *ref_int_x = value__22.index
                        var t409 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t408)
                        var t410 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t406, t407, t409)
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t410)
                    } else {}
                    var t396 *ref_int_x = value__22.index
                    var t397 *ref_int_x = value__22.index
                    var t398 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t397)
                    var t399 int = t398 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t396, t399)
                    var t400 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__23)
                    var t401 Result__string__string = Result__string__string_Ok{
                        _0: t400,
                    }
                    retv377 = t401
                    return retv377
                } else {
                    var t413 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__25, 92)
                    if t413 {
                        var t470 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                        var t471 *ref_int_x = value__22.index
                        var t472 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t471)
                        var t473 bool = t470 < t472
                        if t473 {
                            var t474 string = value__22.input
                            var t475 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(segment__24)
                            var t476 *ref_int_x = value__22.index
                            var t477 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t476)
                            var t478 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t474, t475, t477)
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__23, t478)
                        } else {}
                        var t415 *ref_int_x = value__22.index
                        var t416 *ref_int_x = value__22.index
                        var t417 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t416)
                        var t418 int = t417 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t415, t418)
                        var t463 *ref_int_x = value__22.index
                        var t464 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t463)
                        var t465 string = value__22.input
                        var t466 int = _goml_m_inherent_i_string_i_string_i_byte__len(t465)
                        var t467 bool = t464 >= t466
                        if t467 {
                            var t468 string = _goml_m_std_p_json_p_json__error(value__22, "incomplete escape")
                            var t469 Result__string__string = Result__string__string_Err{
                                _0: t468,
                            }
                            retv377 = t469
                            return retv377
                        } else {
                            var t420 string = value__22.input
                            var t421 *ref_int_x = value__22.index
                            var t422 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t421)
                            var escape__26 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t420, t422)
                            var t423 *ref_int_x = value__22.index
                            var t424 *ref_int_x = value__22.index
                            var t425 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t424)
                            var t426 int = t425 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t423, t426)
                            var t431 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 34)
                            if t431 {
                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 34)
                                var t428 *ref_int_x = value__22.index
                                var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t428)
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t429)
                                continue
                            } else {
                                var t434 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 92)
                                if t434 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 92)
                                    var t428 *ref_int_x = value__22.index
                                    var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t428)
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t429)
                                    continue
                                } else {
                                    var t437 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 47)
                                    if t437 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 47)
                                        var t428 *ref_int_x = value__22.index
                                        var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t428)
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t429)
                                        continue
                                    } else {
                                        var t440 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 98)
                                        if t440 {
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
                                            var t428 *ref_int_x = value__22.index
                                            var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t428)
                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t429)
                                            continue
                                        } else {
                                            var t444 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 102)
                                            if t444 {
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
                                                var t428 *ref_int_x = value__22.index
                                                var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t428)
                                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t429)
                                                continue
                                            } else {
                                                var t448 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 110)
                                                if t448 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 10)
                                                    var t428 *ref_int_x = value__22.index
                                                    var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t428)
                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t429)
                                                    continue
                                                } else {
                                                    var t451 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 114)
                                                    if t451 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 13)
                                                        var t428 *ref_int_x = value__22.index
                                                        var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t428)
                                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t429)
                                                        continue
                                                    } else {
                                                        var t454 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 116)
                                                        if t454 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__23, 9)
                                                            var t428 *ref_int_x = value__22.index
                                                            var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t428)
                                                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t429)
                                                            continue
                                                        } else {
                                                            var t457 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__26, 117)
                                                            if t457 {
                                                                var mtmp29 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__22, builder__23)
                                                                switch mtmp29.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t428 *ref_int_x = value__22.index
                                                                    var t429 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t428)
                                                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(segment__24, t429)
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x31 string = mtmp29.(Result__unit__string_Err)._0
                                                                    var error__29 string = x31
                                                                    var t460 Result__string__string = Result__string__string_Err{
                                                                        _0: error__29,
                                                                    }
                                                                    retv377 = t460
                                                                    return retv377
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t461 string = _goml_m_std_p_json_p_json__error(value__22, "invalid escape")
                                                                var t462 Result__string__string = Result__string__string_Err{
                                                                    _0: t461,
                                                                }
                                                                retv377 = t462
                                                                return retv377
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
                        var t481 bool = byte__25 < 32
                        if t481 {
                            var t482 string = _goml_m_std_p_json_p_json__error(value__22, "unescaped control character")
                            var t483 Result__string__string = Result__string__string_Err{
                                _0: t482,
                            }
                            retv377 = t483
                            return retv377
                        } else {
                            var t484 *ref_int_x = value__22.index
                            var t485 *ref_int_x = value__22.index
                            var t486 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t485)
                            var t487 int = t486 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t484, t487)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop384
            }
        }
        var t382 string = _goml_m_std_p_json_p_json__error(value__22, "unterminated string")
        var t383 Result__string__string = Result__string__string_Err{
            _0: t382,
        }
        retv377 = t383
        return retv377
    }
}

func _goml_m_std_p_json_p_json__digit(value__30 uint8) bool {
    var retv510 bool
    var t513 bool = value__30 >= 48
    var jp512 bool
    if t513 {
        var t514 bool = value__30 <= 57
        jp512 = t514
    } else {
        jp512 = false
    }
    retv510 = jp512
    return retv510
}

func _goml_m_std_p_json_p_parse__digits(value__31 _goml_m_std_p_json_p_JsonParser) bool {
    var retv516 bool
    var t517 *ref_int_x = value__31.index
    var start__32 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t517)
    Loop_loop522:
    for {
        var t530 *ref_int_x = value__31.index
        var t531 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t530)
        var t532 string = value__31.input
        var t533 int = _goml_m_inherent_i_string_i_string_i_byte__len(t532)
        var t534 bool = t531 < t533
        var jp524 bool
        if t534 {
            var t535 string = value__31.input
            var t536 *ref_int_x = value__31.index
            var t537 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t536)
            var t538 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t535, t537)
            var t539 bool = _goml_m_std_p_json_p_json__digit(t538)
            jp524 = t539
        } else {
            jp524 = false
        }
        if jp524 {
            var t525 *ref_int_x = value__31.index
            var t526 *ref_int_x = value__31.index
            var t527 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t526)
            var t528 int = t527 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t525, t528)
            continue
        } else {
            break Loop_loop522
        }
    }
    var t519 *ref_int_x = value__31.index
    var t520 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t519)
    var t521 bool = t520 > start__32
    retv516 = t521
    return retv516
}

func _goml_m_std_p_json_p_parse__json__number(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv541 _goml_m_Result____std_p_json_p_Value____string
    var t542 *ref_int_x = value__33.index
    var start__34 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t542)
    var t664 string = value__33.input
    var t665 *ref_int_x = value__33.index
    var t666 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t665)
    var t667 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t664, t666)
    var t668 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t667, 45)
    if t668 {
        var t669 *ref_int_x = value__33.index
        var t670 *ref_int_x = value__33.index
        var t671 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t670)
        var t672 int = t671 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t669, t672)
    } else {}
    var t627 *ref_int_x = value__33.index
    var t628 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t627)
    var t629 string = value__33.input
    var t630 int = _goml_m_inherent_i_string_i_string_i_byte__len(t629)
    var t631 bool = t628 >= t630
    if t631 {
        var t632 string = _goml_m_std_p_json_p_json__error(value__33, "incomplete number")
        var t633 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t632,
        }
        retv541 = t633
        return retv541
    } else {
        var t635 string = value__33.input
        var t636 *ref_int_x = value__33.index
        var t637 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t636)
        var t638 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t635, t637)
        var t639 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t638, 48)
        if t639 {
            var t640 *ref_int_x = value__33.index
            var t641 *ref_int_x = value__33.index
            var t642 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t641)
            var t643 int = t642 + 1
            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t640, t643)
            var t649 *ref_int_x = value__33.index
            var t650 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t649)
            var t651 string = value__33.input
            var t652 int = _goml_m_inherent_i_string_i_string_i_byte__len(t651)
            var t653 bool = t650 < t652
            var jp646 bool
            if t653 {
                var t654 string = value__33.input
                var t655 *ref_int_x = value__33.index
                var t656 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t655)
                var t657 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t654, t656)
                var t658 bool = _goml_m_std_p_json_p_json__digit(t657)
                jp646 = t658
            } else {
                jp646 = false
            }
            if jp646 {
                var t647 string = _goml_m_std_p_json_p_json__error(value__33, "invalid leading zero")
                var t648 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t647,
                }
                retv541 = t648
                return retv541
            } else {
                var t617 *ref_int_x = value__33.index
                var t618 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t617)
                var t619 string = value__33.input
                var t620 int = _goml_m_inherent_i_string_i_string_i_byte__len(t619)
                var t621 bool = t618 < t620
                var jp607 bool
                if t621 {
                    var t622 string = value__33.input
                    var t623 *ref_int_x = value__33.index
                    var t624 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t623)
                    var t625 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t622, t624)
                    var t626 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t625, 46)
                    jp607 = t626
                } else {
                    jp607 = false
                }
                if jp607 {
                    var t608 *ref_int_x = value__33.index
                    var t609 *ref_int_x = value__33.index
                    var t610 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t609)
                    var t611 int = t610 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t608, t611)
                    var t613 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t614 bool = !t613
                    if t614 {
                        var t615 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t616 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t615,
                        }
                        retv541 = t616
                        return retv541
                    } else {
                        var t589 *ref_int_x = value__33.index
                        var t590 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t589)
                        var t591 string = value__33.input
                        var t592 int = _goml_m_inherent_i_string_i_string_i_byte__len(t591)
                        var t593 bool = t590 < t592
                        var jp554 bool
                        if t593 {
                            var t596 string = value__33.input
                            var t597 *ref_int_x = value__33.index
                            var t598 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t597)
                            var t599 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t596, t598)
                            var t600 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t599, 101)
                            var jp595 bool
                            if t600 {
                                jp595 = true
                            } else {
                                var t601 string = value__33.input
                                var t602 *ref_int_x = value__33.index
                                var t603 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t602)
                                var t604 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t601, t603)
                                var t605 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t604, 69)
                                jp595 = t605
                            }
                            jp554 = jp595
                        } else {
                            jp554 = false
                        }
                        if jp554 {
                            var t555 *ref_int_x = value__33.index
                            var t556 *ref_int_x = value__33.index
                            var t557 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t556)
                            var t558 int = t557 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t555, t558)
                            var t572 *ref_int_x = value__33.index
                            var t573 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t572)
                            var t574 string = value__33.input
                            var t575 int = _goml_m_inherent_i_string_i_string_i_byte__len(t574)
                            var t576 bool = t573 < t575
                            var jp566 bool
                            if t576 {
                                var t579 string = value__33.input
                                var t580 *ref_int_x = value__33.index
                                var t581 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t580)
                                var t582 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t579, t581)
                                var t583 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t582, 43)
                                var jp578 bool
                                if t583 {
                                    jp578 = true
                                } else {
                                    var t584 string = value__33.input
                                    var t585 *ref_int_x = value__33.index
                                    var t586 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t585)
                                    var t587 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t584, t586)
                                    var t588 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t587, 45)
                                    jp578 = t588
                                }
                                jp566 = jp578
                            } else {
                                jp566 = false
                            }
                            if jp566 {
                                var t567 *ref_int_x = value__33.index
                                var t568 *ref_int_x = value__33.index
                                var t569 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t568)
                                var t570 int = t569 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t567, t570)
                            } else {}
                            var t561 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t562 bool = !t561
                            if t562 {
                                var t563 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t564 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t563,
                                }
                                retv541 = t564
                                return retv541
                            } else {
                                var t547 string = value__33.input
                                var t548 *ref_int_x = value__33.index
                                var t549 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t548)
                                var t550 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t547, start__34, t549)
                                var t551 _goml_m_std_p_json_p_Value = Number{
                                    _0: t550,
                                }
                                var t552 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t551,
                                }
                                retv541 = t552
                                return retv541
                            }
                        } else {
                            var t547 string = value__33.input
                            var t548 *ref_int_x = value__33.index
                            var t549 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t548)
                            var t550 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t547, start__34, t549)
                            var t551 _goml_m_std_p_json_p_Value = Number{
                                _0: t550,
                            }
                            var t552 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t551,
                            }
                            retv541 = t552
                            return retv541
                        }
                    }
                } else {
                    var t589 *ref_int_x = value__33.index
                    var t590 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t589)
                    var t591 string = value__33.input
                    var t592 int = _goml_m_inherent_i_string_i_string_i_byte__len(t591)
                    var t593 bool = t590 < t592
                    var jp554 bool
                    if t593 {
                        var t596 string = value__33.input
                        var t597 *ref_int_x = value__33.index
                        var t598 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t597)
                        var t599 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t596, t598)
                        var t600 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t599, 101)
                        var jp595 bool
                        if t600 {
                            jp595 = true
                        } else {
                            var t601 string = value__33.input
                            var t602 *ref_int_x = value__33.index
                            var t603 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t602)
                            var t604 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t601, t603)
                            var t605 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t604, 69)
                            jp595 = t605
                        }
                        jp554 = jp595
                    } else {
                        jp554 = false
                    }
                    if jp554 {
                        var t555 *ref_int_x = value__33.index
                        var t556 *ref_int_x = value__33.index
                        var t557 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t556)
                        var t558 int = t557 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t555, t558)
                        var t572 *ref_int_x = value__33.index
                        var t573 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t572)
                        var t574 string = value__33.input
                        var t575 int = _goml_m_inherent_i_string_i_string_i_byte__len(t574)
                        var t576 bool = t573 < t575
                        var jp566 bool
                        if t576 {
                            var t579 string = value__33.input
                            var t580 *ref_int_x = value__33.index
                            var t581 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t580)
                            var t582 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t579, t581)
                            var t583 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t582, 43)
                            var jp578 bool
                            if t583 {
                                jp578 = true
                            } else {
                                var t584 string = value__33.input
                                var t585 *ref_int_x = value__33.index
                                var t586 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t585)
                                var t587 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t584, t586)
                                var t588 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t587, 45)
                                jp578 = t588
                            }
                            jp566 = jp578
                        } else {
                            jp566 = false
                        }
                        if jp566 {
                            var t567 *ref_int_x = value__33.index
                            var t568 *ref_int_x = value__33.index
                            var t569 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t568)
                            var t570 int = t569 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t567, t570)
                        } else {}
                        var t561 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t562 bool = !t561
                        if t562 {
                            var t563 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t564 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t563,
                            }
                            retv541 = t564
                            return retv541
                        } else {
                            var t547 string = value__33.input
                            var t548 *ref_int_x = value__33.index
                            var t549 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t548)
                            var t550 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t547, start__34, t549)
                            var t551 _goml_m_std_p_json_p_Value = Number{
                                _0: t550,
                            }
                            var t552 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t551,
                            }
                            retv541 = t552
                            return retv541
                        }
                    } else {
                        var t547 string = value__33.input
                        var t548 *ref_int_x = value__33.index
                        var t549 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t548)
                        var t550 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t547, start__34, t549)
                        var t551 _goml_m_std_p_json_p_Value = Number{
                            _0: t550,
                        }
                        var t552 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t551,
                        }
                        retv541 = t552
                        return retv541
                    }
                }
            }
        } else {
            var t660 bool = _goml_m_std_p_json_p_parse__digits(value__33)
            var t661 bool = !t660
            if t661 {
                var t662 string = _goml_m_std_p_json_p_json__error(value__33, "expected number")
                var t663 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t662,
                }
                retv541 = t663
                return retv541
            } else {
                var t617 *ref_int_x = value__33.index
                var t618 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t617)
                var t619 string = value__33.input
                var t620 int = _goml_m_inherent_i_string_i_string_i_byte__len(t619)
                var t621 bool = t618 < t620
                var jp607 bool
                if t621 {
                    var t622 string = value__33.input
                    var t623 *ref_int_x = value__33.index
                    var t624 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t623)
                    var t625 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t622, t624)
                    var t626 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t625, 46)
                    jp607 = t626
                } else {
                    jp607 = false
                }
                if jp607 {
                    var t608 *ref_int_x = value__33.index
                    var t609 *ref_int_x = value__33.index
                    var t610 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t609)
                    var t611 int = t610 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t608, t611)
                    var t613 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                    var t614 bool = !t613
                    if t614 {
                        var t615 string = _goml_m_std_p_json_p_json__error(value__33, "missing fraction digits")
                        var t616 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t615,
                        }
                        retv541 = t616
                        return retv541
                    } else {
                        var t589 *ref_int_x = value__33.index
                        var t590 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t589)
                        var t591 string = value__33.input
                        var t592 int = _goml_m_inherent_i_string_i_string_i_byte__len(t591)
                        var t593 bool = t590 < t592
                        var jp554 bool
                        if t593 {
                            var t596 string = value__33.input
                            var t597 *ref_int_x = value__33.index
                            var t598 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t597)
                            var t599 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t596, t598)
                            var t600 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t599, 101)
                            var jp595 bool
                            if t600 {
                                jp595 = true
                            } else {
                                var t601 string = value__33.input
                                var t602 *ref_int_x = value__33.index
                                var t603 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t602)
                                var t604 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t601, t603)
                                var t605 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t604, 69)
                                jp595 = t605
                            }
                            jp554 = jp595
                        } else {
                            jp554 = false
                        }
                        if jp554 {
                            var t555 *ref_int_x = value__33.index
                            var t556 *ref_int_x = value__33.index
                            var t557 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t556)
                            var t558 int = t557 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t555, t558)
                            var t572 *ref_int_x = value__33.index
                            var t573 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t572)
                            var t574 string = value__33.input
                            var t575 int = _goml_m_inherent_i_string_i_string_i_byte__len(t574)
                            var t576 bool = t573 < t575
                            var jp566 bool
                            if t576 {
                                var t579 string = value__33.input
                                var t580 *ref_int_x = value__33.index
                                var t581 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t580)
                                var t582 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t579, t581)
                                var t583 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t582, 43)
                                var jp578 bool
                                if t583 {
                                    jp578 = true
                                } else {
                                    var t584 string = value__33.input
                                    var t585 *ref_int_x = value__33.index
                                    var t586 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t585)
                                    var t587 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t584, t586)
                                    var t588 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t587, 45)
                                    jp578 = t588
                                }
                                jp566 = jp578
                            } else {
                                jp566 = false
                            }
                            if jp566 {
                                var t567 *ref_int_x = value__33.index
                                var t568 *ref_int_x = value__33.index
                                var t569 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t568)
                                var t570 int = t569 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t567, t570)
                            } else {}
                            var t561 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                            var t562 bool = !t561
                            if t562 {
                                var t563 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                                var t564 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t563,
                                }
                                retv541 = t564
                                return retv541
                            } else {
                                var t547 string = value__33.input
                                var t548 *ref_int_x = value__33.index
                                var t549 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t548)
                                var t550 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t547, start__34, t549)
                                var t551 _goml_m_std_p_json_p_Value = Number{
                                    _0: t550,
                                }
                                var t552 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t551,
                                }
                                retv541 = t552
                                return retv541
                            }
                        } else {
                            var t547 string = value__33.input
                            var t548 *ref_int_x = value__33.index
                            var t549 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t548)
                            var t550 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t547, start__34, t549)
                            var t551 _goml_m_std_p_json_p_Value = Number{
                                _0: t550,
                            }
                            var t552 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t551,
                            }
                            retv541 = t552
                            return retv541
                        }
                    }
                } else {
                    var t589 *ref_int_x = value__33.index
                    var t590 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t589)
                    var t591 string = value__33.input
                    var t592 int = _goml_m_inherent_i_string_i_string_i_byte__len(t591)
                    var t593 bool = t590 < t592
                    var jp554 bool
                    if t593 {
                        var t596 string = value__33.input
                        var t597 *ref_int_x = value__33.index
                        var t598 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t597)
                        var t599 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t596, t598)
                        var t600 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t599, 101)
                        var jp595 bool
                        if t600 {
                            jp595 = true
                        } else {
                            var t601 string = value__33.input
                            var t602 *ref_int_x = value__33.index
                            var t603 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t602)
                            var t604 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t601, t603)
                            var t605 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t604, 69)
                            jp595 = t605
                        }
                        jp554 = jp595
                    } else {
                        jp554 = false
                    }
                    if jp554 {
                        var t555 *ref_int_x = value__33.index
                        var t556 *ref_int_x = value__33.index
                        var t557 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t556)
                        var t558 int = t557 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t555, t558)
                        var t572 *ref_int_x = value__33.index
                        var t573 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t572)
                        var t574 string = value__33.input
                        var t575 int = _goml_m_inherent_i_string_i_string_i_byte__len(t574)
                        var t576 bool = t573 < t575
                        var jp566 bool
                        if t576 {
                            var t579 string = value__33.input
                            var t580 *ref_int_x = value__33.index
                            var t581 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t580)
                            var t582 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t579, t581)
                            var t583 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t582, 43)
                            var jp578 bool
                            if t583 {
                                jp578 = true
                            } else {
                                var t584 string = value__33.input
                                var t585 *ref_int_x = value__33.index
                                var t586 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t585)
                                var t587 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t584, t586)
                                var t588 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t587, 45)
                                jp578 = t588
                            }
                            jp566 = jp578
                        } else {
                            jp566 = false
                        }
                        if jp566 {
                            var t567 *ref_int_x = value__33.index
                            var t568 *ref_int_x = value__33.index
                            var t569 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t568)
                            var t570 int = t569 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t567, t570)
                        } else {}
                        var t561 bool = _goml_m_std_p_json_p_parse__digits(value__33)
                        var t562 bool = !t561
                        if t562 {
                            var t563 string = _goml_m_std_p_json_p_json__error(value__33, "missing exponent digits")
                            var t564 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t563,
                            }
                            retv541 = t564
                            return retv541
                        } else {
                            var t547 string = value__33.input
                            var t548 *ref_int_x = value__33.index
                            var t549 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t548)
                            var t550 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t547, start__34, t549)
                            var t551 _goml_m_std_p_json_p_Value = Number{
                                _0: t550,
                            }
                            var t552 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t551,
                            }
                            retv541 = t552
                            return retv541
                        }
                    } else {
                        var t547 string = value__33.input
                        var t548 *ref_int_x = value__33.index
                        var t549 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t548)
                        var t550 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t547, start__34, t549)
                        var t551 _goml_m_std_p_json_p_Value = Number{
                            _0: t550,
                        }
                        var t552 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t551,
                        }
                        retv541 = t552
                        return retv541
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__35 _goml_m_std_p_json_p_JsonParser, expected__36 string, result__37 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var retv675 _goml_m_Result____std_p_json_p_Value____string
    var t688 *ref_int_x = value__35.index
    var t689 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t688)
    var t690 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
    var t691 int = t689 + t690
    var t692 string = value__35.input
    var t693 int = _goml_m_inherent_i_string_i_string_i_byte__len(t692)
    var t694 bool = t691 <= t693
    var jp679 bool
    if t694 {
        var t695 string = value__35.input
        var t696 *ref_int_x = value__35.index
        var t697 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t696)
        var t698 *ref_int_x = value__35.index
        var t699 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t698)
        var t700 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t701 int = t699 + t700
        var t702 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t695, t697, t701)
        var t703 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t702, expected__36)
        jp679 = t703
    } else {
        jp679 = false
    }
    var jp677 _goml_m_Result____std_p_json_p_Value____string
    if jp679 {
        var t680 *ref_int_x = value__35.index
        var t681 *ref_int_x = value__35.index
        var t682 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t681)
        var t683 int = _goml_m_inherent_i_string_i_string_i_byte__len(expected__36)
        var t684 int = t682 + t683
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t680, t684)
        var t685 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__37,
        }
        jp677 = t685
    } else {
        var t686 string = _goml_m_std_p_json_p_json__error(value__35, "invalid literal")
        var t687 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t686,
        }
        jp677 = t687
    }
    retv675 = jp677
    return retv675
}

func _goml_m_std_p_json_p_parse__json__array(value__38 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv705 _goml_m_Result____std_p_json_p_Value____string
    var t706 *ref_int_x = value__38.index
    var t707 *ref_int_x = value__38.index
    var t708 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t707)
    var t709 int = t708 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t706, t709)
    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
    var result__39 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t764 *ref_int_x = value__38.index
    var t765 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t764)
    var t766 string = value__38.input
    var t767 int = _goml_m_inherent_i_string_i_string_i_byte__len(t766)
    var t768 bool = t765 < t767
    var jp757 bool
    if t768 {
        var t769 string = value__38.input
        var t770 *ref_int_x = value__38.index
        var t771 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t770)
        var t772 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t769, t771)
        var t773 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t772, 93)
        jp757 = t773
    } else {
        jp757 = false
    }
    if jp757 {
        var t758 *ref_int_x = value__38.index
        var t759 *ref_int_x = value__38.index
        var t760 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t759)
        var t761 int = t760 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t758, t761)
        var t762 _goml_m_std_p_json_p_Value = Array{
            _0: result__39,
        }
        var t763 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t762,
        }
        retv705 = t763
        return retv705
    } else {
        Loop_loop714:
        for {
            var t715 *ref_int_x = value__38.index
            var t716 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t715)
            var t717 string = value__38.input
            var t718 int = _goml_m_inherent_i_string_i_string_i_byte__len(t717)
            var t719 bool = t716 < t718
            if t719 {
                var mtmp48 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__38)
                switch mtmp48.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x49 _goml_m_std_p_json_p_Value = mtmp48.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    var item__40 _goml_m_std_p_json_p_Value = x49
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(result__39, item__40)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                    var t722 *ref_int_x = value__38.index
                    var t723 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t722)
                    var t724 string = value__38.input
                    var t725 int = _goml_m_inherent_i_string_i_string_i_byte__len(t724)
                    var t726 bool = t723 >= t725
                    if t726 {
                        var t727 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
                        var t728 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t727,
                        }
                        retv705 = t728
                        return retv705
                    } else {
                        var t730 string = value__38.input
                        var t731 *ref_int_x = value__38.index
                        var t732 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t731)
                        var t733 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t730, t732)
                        var t734 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t733, 93)
                        if t734 {
                            var t735 *ref_int_x = value__38.index
                            var t736 *ref_int_x = value__38.index
                            var t737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t736)
                            var t738 int = t737 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t735, t738)
                            var t739 _goml_m_std_p_json_p_Value = Array{
                                _0: result__39,
                            }
                            var t740 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t739,
                            }
                            retv705 = t740
                            return retv705
                        } else {
                            var t742 string = value__38.input
                            var t743 *ref_int_x = value__38.index
                            var t744 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t743)
                            var t745 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t742, t744)
                            var t746 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t745, 44)
                            if t746 {
                                var t747 *ref_int_x = value__38.index
                                var t748 *ref_int_x = value__38.index
                                var t749 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t748)
                                var t750 int = t749 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t747, t750)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__38)
                                continue
                            } else {
                                var t752 string = _goml_m_std_p_json_p_json__error(value__38, "expected array separator")
                                var t753 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t752,
                                }
                                retv705 = t753
                                return retv705
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x50 string = mtmp48.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var error__41 string = x50
                    var t755 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__41,
                    }
                    retv705 = t755
                    return retv705
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop714
            }
        }
        var t712 string = _goml_m_std_p_json_p_json__error(value__38, "unterminated array")
        var t713 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t712,
        }
        retv705 = t713
        return retv705
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__42 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv775 _goml_m_Result____std_p_json_p_Value____string
    var t776 *ref_int_x = value__42.index
    var t777 *ref_int_x = value__42.index
    var t778 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t777)
    var t779 int = t778 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t776, t779)
    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
    var result__43 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t859 *ref_int_x = value__42.index
    var t860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t859)
    var t861 string = value__42.input
    var t862 int = _goml_m_inherent_i_string_i_string_i_byte__len(t861)
    var t863 bool = t860 < t862
    var jp852 bool
    if t863 {
        var t864 string = value__42.input
        var t865 *ref_int_x = value__42.index
        var t866 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t865)
        var t867 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t864, t866)
        var t868 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t867, 125)
        jp852 = t868
    } else {
        jp852 = false
    }
    if jp852 {
        var t853 *ref_int_x = value__42.index
        var t854 *ref_int_x = value__42.index
        var t855 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t854)
        var t856 int = t855 + 1
        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t853, t856)
        var t857 _goml_m_std_p_json_p_Value = Object{
            _0: result__43,
        }
        var t858 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t857,
        }
        retv775 = t858
        return retv775
    } else {
        Loop_loop784:
        for {
            var t785 *ref_int_x = value__42.index
            var t786 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t785)
            var t787 string = value__42.input
            var t788 int = _goml_m_inherent_i_string_i_string_i_byte__len(t787)
            var t789 bool = t786 < t788
            if t789 {
                var mtmp60 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__42)
                var jp791 string
                switch mtmp60.(type) {
                case Result__string__string_Ok:
                    var x61 string = mtmp60.(Result__string__string_Ok)._0
                    var name__44 string = x61
                    jp791 = name__44
                    var name__46 string = jp791
                    _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                    var t839 *ref_int_x = value__42.index
                    var t840 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t839)
                    var t841 string = value__42.input
                    var t842 int = _goml_m_inherent_i_string_i_string_i_byte__len(t841)
                    var t843 bool = t840 >= t842
                    var jp831 bool
                    if t843 {
                        jp831 = true
                    } else {
                        var t844 string = value__42.input
                        var t845 *ref_int_x = value__42.index
                        var t846 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t845)
                        var t847 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t844, t846)
                        var t848 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t847, 58)
                        var t849 bool = !t848
                        jp831 = t849
                    }
                    if jp831 {
                        var t832 string = _goml_m_std_p_json_p_json__error(value__42, "expected object colon")
                        var t833 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t832,
                        }
                        retv775 = t833
                        return retv775
                    } else {
                        var t834 *ref_int_x = value__42.index
                        var t835 *ref_int_x = value__42.index
                        var t836 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t835)
                        var t837 int = t836 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t834, t837)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                        var mtmp66 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__42)
                        switch mtmp66.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x67 _goml_m_std_p_json_p_Value = mtmp66.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            var item__47 _goml_m_std_p_json_p_Value = x67
                            var t827 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: name__46,
                                _1: item__47,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__43, t827)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                            var t795 *ref_int_x = value__42.index
                            var t796 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t795)
                            var t797 string = value__42.input
                            var t798 int = _goml_m_inherent_i_string_i_string_i_byte__len(t797)
                            var t799 bool = t796 >= t798
                            if t799 {
                                var t800 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
                                var t801 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t800,
                                }
                                retv775 = t801
                                return retv775
                            } else {
                                var t803 string = value__42.input
                                var t804 *ref_int_x = value__42.index
                                var t805 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t804)
                                var t806 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t803, t805)
                                var t807 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t806, 125)
                                if t807 {
                                    var t808 *ref_int_x = value__42.index
                                    var t809 *ref_int_x = value__42.index
                                    var t810 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t809)
                                    var t811 int = t810 + 1
                                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t808, t811)
                                    var t812 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__43,
                                    }
                                    var t813 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t812,
                                    }
                                    retv775 = t813
                                    return retv775
                                } else {
                                    var t815 string = value__42.input
                                    var t816 *ref_int_x = value__42.index
                                    var t817 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t816)
                                    var t818 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t815, t817)
                                    var t819 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t818, 44)
                                    if t819 {
                                        var t820 *ref_int_x = value__42.index
                                        var t821 *ref_int_x = value__42.index
                                        var t822 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t821)
                                        var t823 int = t822 + 1
                                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t820, t823)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__42)
                                        continue
                                    } else {
                                        var t825 string = _goml_m_std_p_json_p_json__error(value__42, "expected object separator")
                                        var t826 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t825,
                                        }
                                        retv775 = t826
                                        return retv775
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x68 string = mtmp66.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var error__48 string = x68
                            var t829 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: error__48,
                            }
                            retv775 = t829
                            return retv775
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x62 string = mtmp60.(Result__string__string_Err)._0
                    var error__45 string = x62
                    var t850 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: error__45,
                    }
                    retv775 = t850
                    return retv775
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop784
            }
        }
        var t782 string = _goml_m_std_p_json_p_json__error(value__42, "unterminated object")
        var t783 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t782,
        }
        retv775 = t783
        return retv775
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__49 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var retv870 _goml_m_Result____std_p_json_p_Value____string
    _goml_m_std_p_json_p_skip__json__whitespace(value__49)
    var t873 *ref_int_x = value__49.index
    var t874 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t873)
    var t875 string = value__49.input
    var t876 int = _goml_m_inherent_i_string_i_string_i_byte__len(t875)
    var t877 bool = t874 >= t876
    var jp872 _goml_m_Result____std_p_json_p_Value____string
    if t877 {
        var t878 string = _goml_m_std_p_json_p_json__error(value__49, "expected JSON value")
        var t879 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t878,
        }
        jp872 = t879
    } else {
        var t880 string = value__49.input
        var t881 *ref_int_x = value__49.index
        var t882 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t881)
        var mtmp75 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t880, t882)
        var jp884 _goml_m_Result____std_p_json_p_Value____string
        switch mtmp75 {
        case 123:
            var t885 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__49)
            jp884 = t885
        case 91:
            var t886 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__49)
            jp884 = t886
        case 34:
            var mtmp76 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__49)
            var jp888 _goml_m_Result____std_p_json_p_Value____string
            switch mtmp76.(type) {
            case Result__string__string_Ok:
                var x77 string = mtmp76.(Result__string__string_Ok)._0
                var text__50 string = x77
                var t889 _goml_m_std_p_json_p_Value = String{
                    _0: text__50,
                }
                var t890 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t889,
                }
                jp888 = t890
            case Result__string__string_Err:
                var x78 string = mtmp76.(Result__string__string_Err)._0
                var error__51 string = x78
                var t891 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: error__51,
                }
                jp888 = t891
            default:
                panic("non-exhaustive match")
            }
            jp884 = jp888
        case 116:
            var t892 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t893 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "true", t892)
            jp884 = t893
        case 102:
            var t894 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t895 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "false", t894)
            jp884 = t895
        case 110:
            var t896 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__49, "null", Null{})
            jp884 = t896
        default:
            var byte__52 uint8 = mtmp75
            var t904 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__52, 45)
            var jp900 bool
            if t904 {
                jp900 = true
            } else {
                var t905 bool = _goml_m_std_p_json_p_json__digit(byte__52)
                jp900 = t905
            }
            var jp898 _goml_m_Result____std_p_json_p_Value____string
            if jp900 {
                var t901 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__49)
                jp898 = t901
            } else {
                var t902 string = _goml_m_std_p_json_p_json__error(value__49, "unexpected JSON token")
                var t903 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t902,
                }
                jp898 = t903
            }
            jp884 = jp898
        }
        jp872 = jp884
    }
    retv870 = jp872
    return retv870
}

func _goml_m_std_p_json_p_parse(input__53 string) _goml_m_Result____std_p_json_p_Value____string {
    var retv907 _goml_m_Result____std_p_json_p_Value____string
    var parser__54 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_json__parser(input__53)
    var mtmp79 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__54)
    var jp909 _goml_m_std_p_json_p_Value
    switch mtmp79.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x80 _goml_m_std_p_json_p_Value = mtmp79.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__55 _goml_m_std_p_json_p_Value = x80
        jp909 = value__55
        var result__57 _goml_m_std_p_json_p_Value = jp909
        _goml_m_std_p_json_p_skip__json__whitespace(parser__54)
        var t912 *ref_int_x = parser__54.index
        var t913 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t912)
        var t914 int = _goml_m_inherent_i_string_i_string_i_byte__len(input__53)
        var t915 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t913, t914)
        var jp911 _goml_m_Result____std_p_json_p_Value____string
        if t915 {
            var t916 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: result__57,
            }
            jp911 = t916
        } else {
            var t917 string = _goml_m_std_p_json_p_json__error(parser__54, "trailing JSON data")
            var t918 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t917,
            }
            jp911 = t918
        }
        retv907 = jp911
        return retv907
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x81 string = mtmp79.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var error__56 string = x81
        var t919 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: error__56,
        }
        retv907 = t919
        return retv907
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_json__hex__digit(value__58 uint8) rune {
    var retv921 rune
    var t922 int = int(uint8(value__58))
    var t923 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", t922)
    retv921 = t923
    return retv921
}

func _goml_m_std_p_json_p_write__json__string(builder__59 _goml_m_std_p_text_p_StringBuilder, value__60 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, 34)
    var start__61 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var for_index84 int = 0
    var for_limit85 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    Loop_loop935:
    for {
        var t936 bool = for_index84 < for_limit85
        if t936 {
            var for_item86 int = for_index84
            var t937 int = for_index84 + 1
            for_index84 = t937
            var index__62 int = for_item86
            var byte__63 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__60, index__62)
            var t993 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
            var jp991 bool
            if t993 {
                jp991 = true
            } else {
                var t994 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                jp991 = t994
            }
            var jp988 bool
            if jp991 {
                jp988 = true
            } else {
                var t992 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                jp988 = t992
            }
            var jp985 bool
            if jp988 {
                jp985 = true
            } else {
                var t989 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                jp985 = t989
            }
            var jp982 bool
            if jp985 {
                jp982 = true
            } else {
                var t986 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                jp982 = t986
            }
            var jp979 bool
            if jp982 {
                jp979 = true
            } else {
                var t983 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                jp979 = t983
            }
            var jp976 bool
            if jp979 {
                jp976 = true
            } else {
                var t980 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                jp976 = t980
            }
            var jp940 bool
            if jp976 {
                jp940 = true
            } else {
                var t977 bool = byte__63 < 32
                jp940 = t977
            }
            if jp940 {
                var t970 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                var t971 bool = t970 < index__62
                if t971 {
                    var t972 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
                    var t973 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t972, index__62)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t973)
                } else {}
                var t945 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 34)
                if t945 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\"")
                } else {
                    var t948 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 92)
                    if t948 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\\\")
                    } else {
                        var t951 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 8)
                        if t951 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\b")
                        } else {
                            var t954 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 9)
                            if t954 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\t")
                            } else {
                                var t957 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 10)
                                if t957 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\n")
                                } else {
                                    var t960 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 12)
                                    if t960 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\f")
                                    } else {
                                        var t963 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__63, 13)
                                        if t963 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, "\\u00")
                                            var t965 uint8 = byte__63 / 16
                                            var t966 rune = _goml_m_std_p_json_p_json__hex__digit(t965)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t966)
                                            var t967_rhs uint8 = 16
                                            var t967 uint8 = byte__63 % t967_rhs
                                            var t968 rune = _goml_m_std_p_json_p_json__hex__digit(t967)
                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__59, t968)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t943 int = index__62 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(start__61, t943)
            } else {}
            continue
        } else {
            break Loop_loop935
        }
    }
    var t928 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
    var t929 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
    var t930 bool = t928 < t929
    if t930 {
        var t931 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(start__61)
        var t932 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__60)
        var t933 string = _goml_m_inherent_i_string_i_string_i_byte__slice(value__60, t931, t932)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__59, t933)
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
        Loop_loop999:
        for {
            var t1000 bool = for_index102 < for_limit101
            if t1000 {
                var for_item103 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source100, for_index102)
                var t1001 int = for_index102 + 1
                for_index102 = t1001
                var field__68 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item103
                var t1008 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                var t1009 bool = t1008 > 0
                if t1009 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                } else {}
                var t1003 string = field__68._0
                _goml_m_std_p_json_p_write__json__string(builder__64, t1003)
                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 58)
                var t1004 _goml_m_std_p_json_p_Value = field__68._1
                _goml_m_std_p_json_p_write__json__value(builder__64, t1004)
                var t1005 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__67)
                var t1006 int = t1005 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__67, t1006)
                continue
            } else {
                break Loop_loop999
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
        Loop_loop1013:
        for {
            var t1014 bool = for_index113 < for_limit112
            if t1014 {
                var for_item114 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(for_source111, for_index113)
                var t1015 int = for_index113 + 1
                for_index113 = t1015
                var item__71 _goml_m_std_p_json_p_Value = for_item114
                var t1020 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__70)
                var t1021 bool = t1020 > 0
                if t1021 {
                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__64, 44)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__64, item__71)
                var t1017 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__70)
                var t1018 int = t1017 + 1
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__70, t1018)
                continue
            } else {
                break Loop_loop1013
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
        var jp1026 string
        if value__74 {
            jp1026 = "true"
        } else {
            jp1026 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, jp1026)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__64, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__75 _goml_m_std_p_json_p_Value) string {
    var retv1030 string
    var builder__76 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
    _goml_m_std_p_json_p_write__json__value(builder__76, value__75)
    var t1031 string = _goml_m_inherent_i_std_p_text__h38fb10d4bc2f951b3d4bba4675c0c32c_uilder_i_finish(builder__76)
    retv1030 = t1031
    return retv1030
}

func _goml_m_std_p_json_p_field(value__77 _goml_m_std_p_json_p_Value, name__78 string) _goml_m_Option____std_p_json_p_Value {
    var retv1033 _goml_m_Option____std_p_json_p_Value
    var jp1035 _goml_m_Option____std_p_json_p_Value
    switch value__77.(type) {
    case Object:
        var x120 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__77.(Object)._0
        var fields__79 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = x120
        var result__80 *ref__goml_m_Option____std_p_json_p_Value_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(_goml_m_Option____std_p_json_p_Value_None{})
        var for_source125 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = fields__79
        var for_limit126 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source125)
        var for_index127 int = 0
        Loop_loop1038:
        for {
            var t1039 bool = for_index127 < for_limit126
            if t1039 {
                var for_item128 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(for_source125, for_index127)
                var t1040 int = for_index127 + 1
                for_index127 = t1040
                var field__81 Tuple2_6string_26_goml_m_std_p_json_p_Value = for_item128
                var t1042 string = field__81._0
                var t1043 bool = _goml_m_trait__impl_i_Eq_i_string_i_eq(t1042, name__78)
                if t1043 {
                    var t1044 _goml_m_std_p_json_p_Value = field__81._1
                    var t1045 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1044,
                    }
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(result__80, t1045)
                } else {}
                continue
            } else {
                break Loop_loop1038
            }
        }
        var t1037 _goml_m_Option____std_p_json_p_Value = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(result__80)
        jp1035 = t1037
        retv1033 = jp1035
        return retv1033
    default:
        jp1035 = _goml_m_Option____std_p_json_p_Value_None{}
        retv1033 = jp1035
        return retv1033
    }
}

func _goml_m_std_p_json_p_as__string(value__82 _goml_m_std_p_json_p_Value) Option__string {
    var retv1048 Option__string
    var jp1050 Option__string
    switch value__82.(type) {
    case String:
        var x133 string = value__82.(String)._0
        var text__83 string = x133
        var t1051 Option__string = Option__string_Some{
            _0: text__83,
        }
        jp1050 = t1051
    default:
        jp1050 = Option__string_None{}
    }
    retv1048 = jp1050
    return retv1048
}

func _goml_m_std_p_json_p_parse__json__int__text(value__84 string) Option__int {
    var retv1053 Option__int
    var t1056 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
    var t1057 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1056, 0)
    var jp1055 Option__int
    if t1057 {
        jp1055 = Option__int_None{}
        retv1053 = jp1055
        return retv1053
    } else {
        var t1058 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, 0)
        var negative__85 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1058, 45)
        var jp1060 int
        if negative__85 {
            jp1060 = 1
        } else {
            jp1060 = 0
        }
        var index__86 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(jp1060)
        var result__87 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
        var t1088 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
        var t1089 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
        var t1090 bool = _goml_m_trait__impl_i_Eq_i_int_i_eq(t1088, t1089)
        if t1090 {
            retv1053 = Option__int_None{}
            return retv1053
        } else {
            Loop_loop1069:
            for {
                var t1070 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                var t1071 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__84)
                var t1072 bool = t1070 < t1071
                if t1072 {
                    var t1073 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                    var byte__88 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__84, t1073)
                    var t1086 bool = byte__88 < 48
                    var jp1079 bool
                    if t1086 {
                        jp1079 = true
                    } else {
                        var t1087 bool = byte__88 > 57
                        jp1079 = t1087
                    }
                    if jp1079 {
                        retv1053 = Option__int_None{}
                        return retv1053
                    } else {
                        var t1080 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                        var t1081 int = t1080 * 10
                        var t1082 uint8 = byte__88 - 48
                        var t1083 int = int(uint8(t1082))
                        var t1084 int = t1081 + t1083
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__87, t1084)
                        var t1075 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(index__86)
                        var t1076 int = t1075 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(index__86, t1076)
                        continue
                    }
                } else {
                    break Loop_loop1069
                }
            }
            var jp1064 int
            if negative__85 {
                var t1066 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                var t1067 int = 0 - t1066
                jp1064 = t1067
            } else {
                var t1068 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__87)
                jp1064 = t1068
            }
            var t1065 Option__int = Option__int_Some{
                _0: jp1064,
            }
            jp1055 = t1065
            retv1053 = jp1055
            return retv1053
        }
    }
}

func _goml_m_std_p_json_p_as__int(value__89 _goml_m_std_p_json_p_Value) Option__int {
    var retv1092 Option__int
    var jp1094 Option__int
    switch value__89.(type) {
    case Number:
        var x142 string = value__89.(Number)._0
        var number__90 string = x142
        var t1095 Option__int = _goml_m_std_p_json_p_parse__json__int__text(number__90)
        jp1094 = t1095
    default:
        jp1094 = Option__int_None{}
    }
    retv1092 = jp1094
    return retv1092
}

func _goml_m_std_p_json_p_as__bool(value__91 _goml_m_std_p_json_p_Value) Option__bool {
    var retv1097 Option__bool
    var jp1099 Option__bool
    switch value__91.(type) {
    case Bool:
        var x148 bool = value__91.(Bool)._0
        var result__92 bool = x148
        var t1100 Option__bool = Option__bool_Some{
            _0: result__92,
        }
        jp1099 = t1100
    default:
        jp1099 = Option__bool_None{}
    }
    retv1097 = jp1099
    return retv1097
}

func main0() struct{} {
    var mtmp64 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1111 _goml_m_std_p_json_p_Value
    switch mtmp64.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x65 _goml_m_std_p_json_p_Value = mtmp64.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        var value__0 _goml_m_std_p_json_p_Value = x65
        jp1111 = value__0
        var value__2 _goml_m_std_p_json_p_Value = jp1111
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
        var t1115 string = _goml_m_std_p_json_p_encode(value__2)
        println__T_string(t1115)
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
    var retv1130 *_goml_vec_uint8
    var t1131 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    retv1130 = t1131
    return retv1130
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__uint8(self__128 *_goml_vec_uint8, elem__129 uint8) struct{} {
    vec_push__Vec_5uint8(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_reserve____T__uint8(self__142 *_goml_vec_uint8, additional__143 int) struct{} {
    vec_reserve__Vec_5uint8(self__142, additional__143)
    return struct{}{}
}

func _goml_m_trait__impl_i_Eq_i_int_i_eq(self__59 int, other__60 int) bool {
    var retv1173 bool
    var t1174 bool = self__59 == other__60
    retv1173 = t1174
    return retv1173
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var retv1176 int
    var t1177 int = _goml_runtime_core_string_len(self__9)
    retv1176 = t1177
    return retv1176
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var retv1179 uint8
    var t1180 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    retv1179 = t1180
    return retv1179
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var retv1182 string
    var t1183 string = _goml_runtime_core_char_to_string(self__7)
    retv1182 = t1183
    return retv1182
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv1185 *ref_int_x
    var t1186 *ref_int_x = ref__Ref_3int(value__209)
    retv1185 = t1186
    return retv1185
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv1188 int
    var t1189 int = ref_get__Ref_3int(self__210)
    retv1188 = t1189
    return retv1188
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv1191 string
    var t1192 string = _goml_runtime_core_int_to_string(self__5)
    retv1191 = t1192
    return retv1191
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var retv1194 bool
    var t1195 bool = self__69 == other__70
    retv1194 = t1195
    return retv1194
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__211 *ref_int_x, value__212 int) struct{} {
    ref_set__Ref_3int(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__uint32(value__209 uint32) *ref_uint32_x {
    var retv1199 *ref_uint32_x
    var t1200 *ref_uint32_x = ref__Ref_6uint32(value__209)
    retv1199 = t1200
    return retv1199
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__uint32(self__210 *ref_uint32_x) uint32 {
    var retv1202 uint32
    var t1203 uint32 = ref_get__Ref_6uint32(self__210)
    retv1202 = t1203
    return retv1202
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__uint32(self__211 *ref_uint32_x, value__212 uint32) struct{} {
    ref_set__Ref_6uint32(self__211, value__212)
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var retv1207 Option__char
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    var value__4 rune = x2
    var valid__3 bool = x1
    var jp1209 Option__char
    if valid__3 {
        var t1210 Option__char = Option__char_Some{
            _0: value__4,
        }
        jp1209 = t1210
    } else {
        jp1209 = Option__char_None{}
    }
    retv1207 = jp1209
    return retv1207
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var retv1212 string
    var t1213 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    retv1212 = t1213
    return retv1212
}

func _goml_m_trait__impl_i_Eq_i_string_i_eq(self__55 string, other__56 string) bool {
    var retv1215 bool
    var t1216 bool = self__55 == other__56
    retv1215 = t1216
    return retv1215
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var retv1218 *_goml_vec__goml_m_std_p_json_p_Value
    var t1219 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    retv1218 = t1219
    return retv1218
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__128 *_goml_vec__goml_m_std_p_json_p_Value, elem__129 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var retv1223 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
    var t1224 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    retv1223 = t1224
    return retv1223
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__128 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__129 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__128, elem__129)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var retv1228 rune
    var t1229 rune = _goml_runtime_core_string_get(self__10, index__11)
    retv1228 = t1229
    return retv1228
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_std_p_json_p_Value_r_(value__209 _goml_m_Option____std_p_json_p_Value) *ref__goml_m_Option____std_p_json_p_Value_x {
    var retv1231 *ref__goml_m_Option____std_p_json_p_Value_x
    var t1232 *ref__goml_m_Option____std_p_json_p_Value_x = ref___goml_m_Ref__24Option____std_p_json_p_Value(value__209)
    retv1231 = t1232
    return retv1231
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_std_p_json_p_Value_r_(self__211 *ref__goml_m_Option____std_p_json_p_Value_x, value__212 _goml_m_Option____std_p_json_p_Value) struct{} {
    ref_set___goml_m_Ref__24Option____std_p_json_p_Value(self__211, value__212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_std_p_json_p_Value_r_(self__210 *ref__goml_m_Option____std_p_json_p_Value_x) _goml_m_Option____std_p_json_p_Value {
    var retv1236 _goml_m_Option____std_p_json_p_Value
    var t1237 _goml_m_Option____std_p_json_p_Value = ref_get___goml_m_Ref__24Option____std_p_json_p_Value(self__210)
    retv1236 = t1237
    return retv1236
}

func println__T_string(value__1 string) struct{} {
    var t1239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t1239)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t1242 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t1242)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t1245 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t1245)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv1248 string
    retv1248 = self__38
    return retv1248
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv1250 string
    var t1251 string = _goml_runtime_core_int_to_string(self__40)
    retv1250 = t1251
    return retv1250
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv1253 string
    var t1254 string = _goml_runtime_core_bool_to_string(self__37)
    retv1253 = t1254
    return retv1253
}

func main() {
    main0()
}
