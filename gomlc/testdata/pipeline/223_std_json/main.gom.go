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

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
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

type _goml_vec_string struct {
    items []string
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

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Tuple2_6string_26_goml_m_std_p_json_p_Value struct {
    _0 string
    _1 _goml_m_std_p_json_p_Value
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
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

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

type _goml_m_Option_____o_string_c_string_q_ interface {
    is_goml_m_Option_____o_string_c_string_q_()
}

type _goml_m_Option_____o_string_c_string_q__None struct {}

func (_ _goml_m_Option_____o_string_c_string_q__None) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option_____o_string_c_string_q__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Option_____o_string_c_string_q__Some) is_goml_m_Option_____o_string_c_string_q_() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

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
    var vec_literal__178 *_goml_vec_uint8
    var inline1974 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline1974
    var t345 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t345
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline1989 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline1989
    var t359 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t359, length__5)
    var for_index1 int = 0
    Loop_loop361:
    for {
        var t362 bool = for_index1 < length__5
        if t362 {
            var for_item3 int = for_index1
            var t363 int = for_index1 + 1
            for_index1 = t363
            var t364 *_goml_vec_uint8 = self__3.values
            var t365 uint8
            var inline1985 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t365 = inline1985
            vec_push__Vec_5uint8(t364, t365)
            continue
        } else {
            break Loop_loop361
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t368 string
    var inline1991 string = char_to_string(value__8)
    t368 = inline1991
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t368)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var t633 string = "" + message__2
    var t634 string = t633 + " at byte "
    var t635 *ref_int_x = value__1.index
    var t636 int
    var inline2216 int = ref_get__Ref_3int(t635)
    t636 = inline2216
    var t637 string
    var inline2214 string = _goml_runtime_core_int_to_string(t636)
    t637 = inline2214
    var t638 string = t634 + t637
    return t638
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop653:
    for {
        var t661 *ref_int_x = value__4.index
        var t662 int
        var inline2249 int = ref_get__Ref_3int(t661)
        t662 = inline2249
        var t663 string = value__4.input
        var t664 int
        var inline2247 int = _goml_runtime_core_string_len(t663)
        t664 = inline2247
        var t665 bool = t662 < t664
        var jp655 bool
        if t665 {
            var t666 string = value__4.input
            var t667 *ref_int_x = value__4.index
            var t668 int
            var inline2241 int = ref_get__Ref_3int(t667)
            t668 = inline2241
            var t669 uint8
            var inline2239 uint8 = _goml_runtime_core_string_byte_get(t666, t668)
            t669 = inline2239
            var inline2230 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t669, 9)
            var inline2232 bool
            if inline2230 {
                inline2232 = true
            } else {
                var inline2237 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t669, 10)
                inline2232 = inline2237
            }
            var inline2234 bool
            if inline2232 {
                inline2234 = true
            } else {
                var inline2236 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t669, 13)
                inline2234 = inline2236
            }
            if inline2234 {
                jp655 = true
            } else {
                var inline2235 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t669, 32)
                jp655 = inline2235
            }
        } else {
            jp655 = false
        }
        if jp655 {
            var t656 *ref_int_x = value__4.index
            var t657 *ref_int_x = value__4.index
            var t658 int
            var inline2245 int = ref_get__Ref_3int(t657)
            t658 = inline2245
            var t659 int = t658 + 1
            ref_set__Ref_3int(t656, t659)
            continue
        } else {
            break Loop_loop653
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var t700 bool = value__5 >= 48
    var jp676 bool
    if t700 {
        var t701 bool = value__5 <= 57
        jp676 = t701
    } else {
        jp676 = false
    }
    if jp676 {
        var t677 uint8 = value__5 - 48
        var t678 uint32 = uint32(uint8(t677))
        var t679 Option__uint32 = Option__uint32_Some{
            _0: t678,
        }
        return t679
    } else {
        var t698 bool = value__5 >= 65
        var jp683 bool
        if t698 {
            var t699 bool = value__5 <= 70
            jp683 = t699
        } else {
            jp683 = false
        }
        if jp683 {
            var t684 uint8 = value__5 - 65
            var t685 uint8 = t684 + 10
            var t686 uint32 = uint32(uint8(t685))
            var t687 Option__uint32 = Option__uint32_Some{
                _0: t686,
            }
            return t687
        } else {
            var t696 bool = value__5 >= 97
            var jp691 bool
            if t696 {
                var t697 bool = value__5 <= 102
                jp691 = t697
            } else {
                jp691 = false
            }
            if jp691 {
                var t692 uint8 = value__5 - 97
                var t693 uint8 = t692 + 10
                var t694 uint32 = uint32(uint8(t693))
                var t695 Option__uint32 = Option__uint32_Some{
                    _0: t694,
                }
                return t695
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t706 *ref_int_x = value__6.index
    var t707 int
    var inline2277 int = ref_get__Ref_3int(t706)
    t707 = inline2277
    var t708 int = t707 + 4
    var t709 string = value__6.input
    var t710 int
    var inline2275 int = _goml_runtime_core_string_len(t709)
    t710 = inline2275
    var t711 bool = t708 > t710
    if t711 {
        var t712 string
        var inline2251 string = "incomplete unicode escape"
        var inline2252 string = "" + inline2251
        var inline2253 string = inline2252 + " at byte "
        var inline2254 *ref_int_x = value__6.index
        var inline2255 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2254)
        var inline2256 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2255)
        var inline2257 string = inline2253 + inline2256
        t712 = inline2257
        var t713 Result__uint32__string = Result__uint32__string_Err{
            _0: t712,
        }
        return t713
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop720:
        for {
            var t721 bool = for_index0 < for_limit1
            if t721 {
                var for_item2 int = for_index0
                var t722 int = for_index0 + 1
                for_index0 = t722
                var t723 string = value__6.input
                var t724 *ref_int_x = value__6.index
                var t725 int
                var inline2269 int = ref_get__Ref_3int(t724)
                t725 = inline2269
                var t726 int = t725 + for_item2
                var t727 uint8
                var inline2267 uint8 = _goml_runtime_core_string_byte_get(t723, t726)
                t727 = inline2267
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t727)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t729 string
                    var inline2259 string = "invalid unicode escape"
                    var inline2260 string = "" + inline2259
                    var inline2261 string = inline2260 + " at byte "
                    var inline2262 *ref_int_x = value__6.index
                    var inline2263 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2262)
                    var inline2264 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2263)
                    var inline2265 string = inline2261 + inline2264
                    t729 = inline2265
                    var t730 Result__uint32__string = Result__uint32__string_Err{
                        _0: t729,
                    }
                    return t730
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var t731 uint32 = result__7 * 16
                    var t732 uint32 = t731 + x5
                    result__7 = t732
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop720
            }
        }
        var t715 *ref_int_x = value__6.index
        var t716 *ref_int_x = value__6.index
        var t717 int
        var inline2273 int = ref_get__Ref_3int(t716)
        t717 = inline2273
        var t718 int = t717 + 4
        ref_set__Ref_3int(t715, t718)
        var t719 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        return t719
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var commute_field2952 rune
    var inline2290 bool = utf8_valid_scalar(codepoint__12)
    if inline2290 {
        var inline2291 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__12)
        var inline2293 rune = inline2291._1
        commute_field2952 = inline2293
        var inline2287 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field2952)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__11, inline2287)
        var t739 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t739
    } else {
        var t737 string
        var inline2279 string = "invalid unicode codepoint"
        var inline2280 string = "" + inline2279
        var inline2281 string = inline2280 + " at byte "
        var inline2282 *ref_int_x = value__10.index
        var inline2283 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2282)
        var inline2284 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2283)
        var inline2285 string = inline2281 + inline2284
        t737 = inline2285
        var t738 Result__unit__string = Result__unit__string_Err{
            _0: t737,
        }
        return t738
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp743 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        jp743 = x13
        var t805 bool = jp743 >= 55296
        var jp747 bool
        if t805 {
            var t806 bool = jp743 <= 56319
            jp747 = t806
        } else {
            jp747 = false
        }
        if jp747 {
            var t784 *ref_int_x = value__14.index
            var t785 int
            var inline2341 int = ref_get__Ref_3int(t784)
            t785 = inline2341
            var t786 int = t785 + 2
            var t787 string = value__14.input
            var t788 int
            var inline2339 int = _goml_runtime_core_string_len(t787)
            t788 = inline2339
            var t789 bool = t786 > t788
            var jp776 bool
            if t789 {
                jp776 = true
            } else {
                var t790 string = value__14.input
                var t791 *ref_int_x = value__14.index
                var t792 int
                var inline2302 int = ref_get__Ref_3int(t791)
                t792 = inline2302
                var t793 uint8
                var inline2300 uint8 = _goml_runtime_core_string_byte_get(t790, t792)
                t793 = inline2300
                var t794 bool
                var inline2297 uint8 = 92
                var inline2298 bool = t793 == inline2297
                t794 = inline2298
                var t795 bool = !t794
                jp776 = t795
            }
            var jp751 bool
            if jp776 {
                jp751 = true
            } else {
                var t777 string = value__14.input
                var t778 *ref_int_x = value__14.index
                var t779 int
                var inline2309 int = ref_get__Ref_3int(t778)
                t779 = inline2309
                var t780 int = t779 + 1
                var t781 uint8
                var inline2307 uint8 = _goml_runtime_core_string_byte_get(t777, t780)
                t781 = inline2307
                var t782 bool
                var inline2304 uint8 = 117
                var inline2305 bool = t781 == inline2304
                t782 = inline2305
                var t783 bool = !t782
                jp751 = t783
            }
            if jp751 {
                var t752 string
                var inline2311 string = "missing low surrogate"
                var inline2312 string = "" + inline2311
                var inline2313 string = inline2312 + " at byte "
                var inline2314 *ref_int_x = value__14.index
                var inline2315 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2314)
                var inline2316 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2315)
                var inline2317 string = inline2313 + inline2316
                t752 = inline2317
                var t753 Result__unit__string = Result__unit__string_Err{
                    _0: t752,
                }
                return t753
            } else {
                var t754 *ref_int_x = value__14.index
                var t755 *ref_int_x = value__14.index
                var t756 int
                var inline2337 int = ref_get__Ref_3int(t755)
                t756 = inline2337
                var t757 int = t756 + 2
                ref_set__Ref_3int(t754, t757)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp759 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    jp759 = x17
                    var t772 bool = jp759 < 56320
                    var jp763 bool
                    if t772 {
                        jp763 = true
                    } else {
                        var t773 bool = jp759 > 57343
                        jp763 = t773
                    }
                    if jp763 {
                        var t764 string
                        var inline2319 string = "invalid low surrogate"
                        var inline2320 string = "" + inline2319
                        var inline2321 string = inline2320 + " at byte "
                        var inline2322 *ref_int_x = value__14.index
                        var inline2323 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2322)
                        var inline2324 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2323)
                        var inline2325 string = inline2321 + inline2324
                        t764 = inline2325
                        var t765 Result__unit__string = Result__unit__string_Err{
                            _0: t764,
                        }
                        return t765
                    } else {
                        var t766 uint32 = jp743 - 55296
                        var t767 uint32 = t766 * 1024
                        var t768 uint32 = 65536 + t767
                        var t769 uint32 = t768 + jp759
                        var t770 uint32 = t769 - 56320
                        var inline2327 Option__char = char_from_uint32(t770)
                        switch inline2327.(type) {
                        case Option__char_None:
                            var inline2328 string = _goml_m_std_p_json_p_json__error(value__14, "invalid unicode codepoint")
                            var inline2329 Result__unit__string = Result__unit__string_Err{
                                _0: inline2328,
                            }
                            return inline2329
                        case Option__char_Some:
                            var inline2330 rune = inline2327.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__15, inline2330)
                            var inline2333 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline2333
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var t774 Result__unit__string = Result__unit__string_Err{
                        _0: x18,
                    }
                    return t774
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t803 bool = jp743 >= 56320
            var jp799 bool
            if t803 {
                var t804 bool = jp743 <= 57343
                jp799 = t804
            } else {
                jp799 = false
            }
            if jp799 {
                var t800 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t801 Result__unit__string = Result__unit__string_Err{
                    _0: t800,
                }
                return t801
            } else {
                var t802 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, jp743)
                return t802
            }
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var t807 Result__unit__string = Result__unit__string_Err{
            _0: x14,
        }
        return t807
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t923 *ref_int_x = value__18.index
    var t924 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t923)
    var t925 string = value__18.input
    var t926 int = _goml_m_inherent_i_string_i_string_i_byte__len(t925)
    var t927 bool = t924 >= t926
    var jp915 bool
    if t927 {
        jp915 = true
    } else {
        var t928 string = value__18.input
        var t929 *ref_int_x = value__18.index
        var t930 int
        var inline2348 int = ref_get__Ref_3int(t929)
        t930 = inline2348
        var t931 uint8
        var inline2346 uint8 = _goml_runtime_core_string_byte_get(t928, t930)
        t931 = inline2346
        var t932 bool
        var inline2343 uint8 = 34
        var inline2344 bool = t931 == inline2343
        t932 = inline2344
        var t933 bool = !t932
        jp915 = t933
    }
    if jp915 {
        var t916 string
        var inline2350 string = "expected string"
        var inline2351 string = "" + inline2350
        var inline2352 string = inline2351 + " at byte "
        var inline2353 *ref_int_x = value__18.index
        var inline2354 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2353)
        var inline2355 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2354)
        var inline2356 string = inline2352 + inline2355
        t916 = inline2356
        var t917 Result__string__string = Result__string__string_Err{
            _0: t916,
        }
        return t917
    } else {
        var t918 *ref_int_x = value__18.index
        var t919 *ref_int_x = value__18.index
        var t920 int
        var inline2360 int = ref_get__Ref_3int(t919)
        t920 = inline2360
        var t921 int = t920 + 1
        ref_set__Ref_3int(t918, t921)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t811 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t811)
        Loop_loop815:
        for {
            var t816 *ref_int_x = value__18.index
            var t817 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t816)
            var t818 string = value__18.input
            var t819 int = _goml_m_inherent_i_string_i_string_i_byte__len(t818)
            var t820 bool = t817 < t819
            if t820 {
                var t821 string = value__18.input
                var t822 *ref_int_x = value__18.index
                var t823 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t822)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t821, t823)
                var t825 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 34)
                if t825 {
                    var t833 *ref_int_x = value__18.index
                    var t834 int
                    var inline2376 int = ref_get__Ref_3int(t833)
                    t834 = inline2376
                    var t835 bool = segment__20 < t834
                    if t835 {
                        var t836 string = value__18.input
                        var t837 *ref_int_x = value__18.index
                        var t838 int
                        var inline2364 int = ref_get__Ref_3int(t837)
                        t838 = inline2364
                        var t839 string
                        var inline2362 string = string_byte_slice(t836, segment__20, t838)
                        t839 = inline2362
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t839)
                    } else {}
                    var t827 *ref_int_x = value__18.index
                    var t828 *ref_int_x = value__18.index
                    var t829 int
                    var inline2374 int = ref_get__Ref_3int(t828)
                    t829 = inline2374
                    var t830 int = t829 + 1
                    ref_set__Ref_3int(t827, t830)
                    var t831 string
                    var inline2366 *_goml_vec_uint8 = builder__19.values
                    var inline2367 Tuple2_4bool_6string = string_from_utf8(inline2366)
                    var inline2369 string = inline2367._1
                    t831 = inline2369
                    var t832 Result__string__string = Result__string__string_Ok{
                        _0: t831,
                    }
                    return t832
                } else {
                    var t842 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 92)
                    if t842 {
                        var t897 *ref_int_x = value__18.index
                        var t898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t897)
                        var t899 bool = segment__20 < t898
                        if t899 {
                            var t900 string = value__18.input
                            var t901 *ref_int_x = value__18.index
                            var t902 int
                            var inline2380 int = ref_get__Ref_3int(t901)
                            t902 = inline2380
                            var t903 string
                            var inline2378 string = string_byte_slice(t900, segment__20, t902)
                            t903 = inline2378
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t903)
                        } else {}
                        var t844 *ref_int_x = value__18.index
                        var t845 *ref_int_x = value__18.index
                        var t846 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t845)
                        var t847 int = t846 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t844, t847)
                        var t890 *ref_int_x = value__18.index
                        var t891 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t890)
                        var t892 string = value__18.input
                        var t893 int = _goml_m_inherent_i_string_i_string_i_byte__len(t892)
                        var t894 bool = t891 >= t893
                        if t894 {
                            var t895 string
                            var inline2382 string = "incomplete escape"
                            var inline2383 string = "" + inline2382
                            var inline2384 string = inline2383 + " at byte "
                            var inline2385 *ref_int_x = value__18.index
                            var inline2386 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2385)
                            var inline2387 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2386)
                            var inline2388 string = inline2384 + inline2387
                            t895 = inline2388
                            var t896 Result__string__string = Result__string__string_Err{
                                _0: t895,
                            }
                            return t896
                        } else {
                            var t849 string = value__18.input
                            var t850 *ref_int_x = value__18.index
                            var t851 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t850)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t849, t851)
                            var t852 *ref_int_x = value__18.index
                            var t853 *ref_int_x = value__18.index
                            var t854 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t853)
                            var t855 int = t854 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t852, t855)
                            var t859 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 34)
                            if t859 {
                                var inline2390 rune = 34
                                var inline2391 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2390)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, inline2391)
                                var t857 *ref_int_x = value__18.index
                                var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                segment__20 = t858
                                continue
                            } else {
                                var t862 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 92)
                                if t862 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t857 *ref_int_x = value__18.index
                                    var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                    segment__20 = t858
                                    continue
                                } else {
                                    var t865 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 47)
                                    if t865 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t857 *ref_int_x = value__18.index
                                        var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                        segment__20 = t858
                                        continue
                                    } else {
                                        var t868 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 98)
                                        if t868 {
                                            var mtmp26 Option__char = char_from_uint32(8)
                                            switch mtmp26.(type) {
                                            case Option__char_None:
                                                var t857 *ref_int_x = value__18.index
                                                var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                                segment__20 = t858
                                                continue
                                            case Option__char_Some:
                                                var x27 rune = mtmp26.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x27)
                                                var t857 *ref_int_x = value__18.index
                                                var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                                segment__20 = t858
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t872 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 102)
                                            if t872 {
                                                var mtmp28 Option__char = char_from_uint32(12)
                                                switch mtmp28.(type) {
                                                case Option__char_None:
                                                    var t857 *ref_int_x = value__18.index
                                                    var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                                    segment__20 = t858
                                                    continue
                                                case Option__char_Some:
                                                    var x29 rune = mtmp28.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x29)
                                                    var t857 *ref_int_x = value__18.index
                                                    var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                                    segment__20 = t858
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t876 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 110)
                                                if t876 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t857 *ref_int_x = value__18.index
                                                    var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                                    segment__20 = t858
                                                    continue
                                                } else {
                                                    var t879 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 114)
                                                    if t879 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t857 *ref_int_x = value__18.index
                                                        var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                                        segment__20 = t858
                                                        continue
                                                    } else {
                                                        var t882 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 116)
                                                        if t882 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t857 *ref_int_x = value__18.index
                                                            var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                                            segment__20 = t858
                                                            continue
                                                        } else {
                                                            var t885 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 117)
                                                            if t885 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t857 *ref_int_x = value__18.index
                                                                    var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
                                                                    segment__20 = t858
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var t887 Result__string__string = Result__string__string_Err{
                                                                        _0: x32,
                                                                    }
                                                                    return t887
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t888 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t889 Result__string__string = Result__string__string_Err{
                                                                    _0: t888,
                                                                }
                                                                return t889
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
                        var t906 bool = byte__21 < 32
                        if t906 {
                            var t907 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t908 Result__string__string = Result__string__string_Err{
                                _0: t907,
                            }
                            return t908
                        } else {
                            var t909 *ref_int_x = value__18.index
                            var t910 *ref_int_x = value__18.index
                            var t911 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t910)
                            var t912 int = t911 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t909, t912)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop815
            }
        }
        var t813 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t814 Result__string__string = Result__string__string_Err{
            _0: t813,
        }
        return t814
    }
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var t942 *ref_int_x = value__26.index
    var start__27 int
    var inline2411 int = ref_get__Ref_3int(t942)
    start__27 = inline2411
    Loop_loop947:
    for {
        var t955 *ref_int_x = value__26.index
        var t956 int
        var inline2407 int = ref_get__Ref_3int(t955)
        t956 = inline2407
        var t957 string = value__26.input
        var t958 int
        var inline2405 int = _goml_runtime_core_string_len(t957)
        t958 = inline2405
        var t959 bool = t956 < t958
        var jp949 bool
        if t959 {
            var t960 string = value__26.input
            var t961 *ref_int_x = value__26.index
            var t962 int
            var inline2399 int = ref_get__Ref_3int(t961)
            t962 = inline2399
            var t963 uint8
            var inline2397 uint8 = _goml_runtime_core_string_byte_get(t960, t962)
            t963 = inline2397
            var inline2394 bool = t963 >= 48
            if inline2394 {
                var inline2395 bool = t963 <= 57
                jp949 = inline2395
            } else {
                jp949 = false
            }
        } else {
            jp949 = false
        }
        if jp949 {
            var t950 *ref_int_x = value__26.index
            var t951 *ref_int_x = value__26.index
            var t952 int
            var inline2403 int = ref_get__Ref_3int(t951)
            t952 = inline2403
            var t953 int = t952 + 1
            ref_set__Ref_3int(t950, t953)
            continue
        } else {
            break Loop_loop947
        }
    }
    var t944 *ref_int_x = value__26.index
    var t945 int
    var inline2409 int = ref_get__Ref_3int(t944)
    t945 = inline2409
    var t946 bool = t945 > start__27
    return t946
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t967 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t967)
    var t1089 string = value__28.input
    var t1090 *ref_int_x = value__28.index
    var t1091 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1090)
    var t1092 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1089, t1091)
    var t1093 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1092, 45)
    if t1093 {
        var t1094 *ref_int_x = value__28.index
        var t1095 *ref_int_x = value__28.index
        var t1096 int
        var inline2415 int = ref_get__Ref_3int(t1095)
        t1096 = inline2415
        var t1097 int = t1096 + 1
        ref_set__Ref_3int(t1094, t1097)
    } else {}
    var t1052 *ref_int_x = value__28.index
    var t1053 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1052)
    var t1054 string = value__28.input
    var t1055 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1054)
    var t1056 bool = t1053 >= t1055
    if t1056 {
        var t1057 string
        var inline2417 string = "incomplete number"
        var inline2418 string = "" + inline2417
        var inline2419 string = inline2418 + " at byte "
        var inline2420 *ref_int_x = value__28.index
        var inline2421 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2420)
        var inline2422 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2421)
        var inline2423 string = inline2419 + inline2422
        t1057 = inline2423
        var t1058 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1057,
        }
        return t1058
    } else {
        var t1060 string = value__28.input
        var t1061 *ref_int_x = value__28.index
        var t1062 int
        var inline2461 int = ref_get__Ref_3int(t1061)
        t1062 = inline2461
        var t1063 uint8
        var inline2459 uint8 = _goml_runtime_core_string_byte_get(t1060, t1062)
        t1063 = inline2459
        var t1064 bool
        var inline2456 uint8 = 48
        var inline2457 bool = t1063 == inline2456
        t1064 = inline2457
        if t1064 {
            var t1065 *ref_int_x = value__28.index
            var t1066 *ref_int_x = value__28.index
            var t1067 int
            var inline2446 int = ref_get__Ref_3int(t1066)
            t1067 = inline2446
            var t1068 int = t1067 + 1
            ref_set__Ref_3int(t1065, t1068)
            var t1074 *ref_int_x = value__28.index
            var t1075 int
            var inline2442 int = ref_get__Ref_3int(t1074)
            t1075 = inline2442
            var t1076 string = value__28.input
            var t1077 int
            var inline2440 int = _goml_runtime_core_string_len(t1076)
            t1077 = inline2440
            var t1078 bool = t1075 < t1077
            var jp1071 bool
            if t1078 {
                var t1079 string = value__28.input
                var t1080 *ref_int_x = value__28.index
                var t1081 int
                var inline2430 int = ref_get__Ref_3int(t1080)
                t1081 = inline2430
                var t1082 uint8
                var inline2428 uint8 = _goml_runtime_core_string_byte_get(t1079, t1081)
                t1082 = inline2428
                var inline2425 bool = t1082 >= 48
                if inline2425 {
                    var inline2426 bool = t1082 <= 57
                    jp1071 = inline2426
                } else {
                    jp1071 = false
                }
            } else {
                jp1071 = false
            }
            if jp1071 {
                var t1072 string
                var inline2432 string = "invalid leading zero"
                var inline2433 string = "" + inline2432
                var inline2434 string = inline2433 + " at byte "
                var inline2435 *ref_int_x = value__28.index
                var inline2436 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2435)
                var inline2437 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2436)
                var inline2438 string = inline2434 + inline2437
                t1072 = inline2438
                var t1073 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1072,
                }
                return t1073
            } else {
                var t1042 *ref_int_x = value__28.index
                var t1043 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1042)
                var t1044 string = value__28.input
                var t1045 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1044)
                var t1046 bool = t1043 < t1045
                var jp1032 bool
                if t1046 {
                    var t1047 string = value__28.input
                    var t1048 *ref_int_x = value__28.index
                    var t1049 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1048)
                    var t1050 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1047, t1049)
                    var t1051 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1050, 46)
                    jp1032 = t1051
                } else {
                    jp1032 = false
                }
                if jp1032 {
                    var t1033 *ref_int_x = value__28.index
                    var t1034 *ref_int_x = value__28.index
                    var t1035 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1034)
                    var t1036 int = t1035 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1033, t1036)
                    var t1038 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t1039 bool = !t1038
                    if t1039 {
                        var t1040 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t1041 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1040,
                        }
                        return t1041
                    } else {
                        var t1014 *ref_int_x = value__28.index
                        var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                        var t1016 string = value__28.input
                        var t1017 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1016)
                        var t1018 bool = t1015 < t1017
                        var jp979 bool
                        if t1018 {
                            var t1021 string = value__28.input
                            var t1022 *ref_int_x = value__28.index
                            var t1023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1022)
                            var t1024 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1021, t1023)
                            var t1025 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1024, 101)
                            if t1025 {
                                jp979 = true
                            } else {
                                var t1026 string = value__28.input
                                var t1027 *ref_int_x = value__28.index
                                var t1028 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1027)
                                var t1029 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1026, t1028)
                                var t1030 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1029, 69)
                                jp979 = t1030
                            }
                        } else {
                            jp979 = false
                        }
                        if jp979 {
                            var t980 *ref_int_x = value__28.index
                            var t981 *ref_int_x = value__28.index
                            var t982 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t981)
                            var t983 int = t982 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t980, t983)
                            var t997 *ref_int_x = value__28.index
                            var t998 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t997)
                            var t999 string = value__28.input
                            var t1000 int = _goml_m_inherent_i_string_i_string_i_byte__len(t999)
                            var t1001 bool = t998 < t1000
                            var jp991 bool
                            if t1001 {
                                var t1004 string = value__28.input
                                var t1005 *ref_int_x = value__28.index
                                var t1006 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1005)
                                var t1007 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1004, t1006)
                                var t1008 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1007, 43)
                                if t1008 {
                                    jp991 = true
                                } else {
                                    var t1009 string = value__28.input
                                    var t1010 *ref_int_x = value__28.index
                                    var t1011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1010)
                                    var t1012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1009, t1011)
                                    var t1013 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1012, 45)
                                    jp991 = t1013
                                }
                            } else {
                                jp991 = false
                            }
                            if jp991 {
                                var t992 *ref_int_x = value__28.index
                                var t993 *ref_int_x = value__28.index
                                var t994 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t993)
                                var t995 int = t994 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t992, t995)
                            } else {}
                            var t986 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t987 bool = !t986
                            if t987 {
                                var t988 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t989 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t988,
                                }
                                return t989
                            } else {
                                var t972 string = value__28.input
                                var t973 *ref_int_x = value__28.index
                                var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t973)
                                var t975 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t972, start__29, t974)
                                var t976 _goml_m_std_p_json_p_Value = Number{
                                    _0: t975,
                                }
                                var t977 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t976,
                                }
                                return t977
                            }
                        } else {
                            var t972 string = value__28.input
                            var t973 *ref_int_x = value__28.index
                            var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t973)
                            var t975 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t972, start__29, t974)
                            var t976 _goml_m_std_p_json_p_Value = Number{
                                _0: t975,
                            }
                            var t977 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t976,
                            }
                            return t977
                        }
                    }
                } else {
                    var t1014 *ref_int_x = value__28.index
                    var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                    var t1016 string = value__28.input
                    var t1017 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1016)
                    var t1018 bool = t1015 < t1017
                    var jp979 bool
                    if t1018 {
                        var t1021 string = value__28.input
                        var t1022 *ref_int_x = value__28.index
                        var t1023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1022)
                        var t1024 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1021, t1023)
                        var t1025 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1024, 101)
                        if t1025 {
                            jp979 = true
                        } else {
                            var t1026 string = value__28.input
                            var t1027 *ref_int_x = value__28.index
                            var t1028 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1027)
                            var t1029 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1026, t1028)
                            var t1030 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1029, 69)
                            jp979 = t1030
                        }
                    } else {
                        jp979 = false
                    }
                    if jp979 {
                        var t980 *ref_int_x = value__28.index
                        var t981 *ref_int_x = value__28.index
                        var t982 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t981)
                        var t983 int = t982 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t980, t983)
                        var t997 *ref_int_x = value__28.index
                        var t998 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t997)
                        var t999 string = value__28.input
                        var t1000 int = _goml_m_inherent_i_string_i_string_i_byte__len(t999)
                        var t1001 bool = t998 < t1000
                        var jp991 bool
                        if t1001 {
                            var t1004 string = value__28.input
                            var t1005 *ref_int_x = value__28.index
                            var t1006 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1005)
                            var t1007 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1004, t1006)
                            var t1008 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1007, 43)
                            if t1008 {
                                jp991 = true
                            } else {
                                var t1009 string = value__28.input
                                var t1010 *ref_int_x = value__28.index
                                var t1011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1010)
                                var t1012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1009, t1011)
                                var t1013 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1012, 45)
                                jp991 = t1013
                            }
                        } else {
                            jp991 = false
                        }
                        if jp991 {
                            var t992 *ref_int_x = value__28.index
                            var t993 *ref_int_x = value__28.index
                            var t994 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t993)
                            var t995 int = t994 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t992, t995)
                        } else {}
                        var t986 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t987 bool = !t986
                        if t987 {
                            var t988 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t989 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t988,
                            }
                            return t989
                        } else {
                            var t972 string = value__28.input
                            var t973 *ref_int_x = value__28.index
                            var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t973)
                            var t975 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t972, start__29, t974)
                            var t976 _goml_m_std_p_json_p_Value = Number{
                                _0: t975,
                            }
                            var t977 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t976,
                            }
                            return t977
                        }
                    } else {
                        var t972 string = value__28.input
                        var t973 *ref_int_x = value__28.index
                        var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t973)
                        var t975 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t972, start__29, t974)
                        var t976 _goml_m_std_p_json_p_Value = Number{
                            _0: t975,
                        }
                        var t977 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t976,
                        }
                        return t977
                    }
                }
            }
        } else {
            var t1085 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t1086 bool = !t1085
            if t1086 {
                var t1087 string
                var inline2448 string = "expected number"
                var inline2449 string = "" + inline2448
                var inline2450 string = inline2449 + " at byte "
                var inline2451 *ref_int_x = value__28.index
                var inline2452 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2451)
                var inline2453 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2452)
                var inline2454 string = inline2450 + inline2453
                t1087 = inline2454
                var t1088 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1087,
                }
                return t1088
            } else {
                var t1042 *ref_int_x = value__28.index
                var t1043 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1042)
                var t1044 string = value__28.input
                var t1045 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1044)
                var t1046 bool = t1043 < t1045
                var jp1032 bool
                if t1046 {
                    var t1047 string = value__28.input
                    var t1048 *ref_int_x = value__28.index
                    var t1049 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1048)
                    var t1050 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1047, t1049)
                    var t1051 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1050, 46)
                    jp1032 = t1051
                } else {
                    jp1032 = false
                }
                if jp1032 {
                    var t1033 *ref_int_x = value__28.index
                    var t1034 *ref_int_x = value__28.index
                    var t1035 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1034)
                    var t1036 int = t1035 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1033, t1036)
                    var t1038 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t1039 bool = !t1038
                    if t1039 {
                        var t1040 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t1041 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1040,
                        }
                        return t1041
                    } else {
                        var t1014 *ref_int_x = value__28.index
                        var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                        var t1016 string = value__28.input
                        var t1017 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1016)
                        var t1018 bool = t1015 < t1017
                        var jp979 bool
                        if t1018 {
                            var t1021 string = value__28.input
                            var t1022 *ref_int_x = value__28.index
                            var t1023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1022)
                            var t1024 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1021, t1023)
                            var t1025 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1024, 101)
                            if t1025 {
                                jp979 = true
                            } else {
                                var t1026 string = value__28.input
                                var t1027 *ref_int_x = value__28.index
                                var t1028 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1027)
                                var t1029 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1026, t1028)
                                var t1030 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1029, 69)
                                jp979 = t1030
                            }
                        } else {
                            jp979 = false
                        }
                        if jp979 {
                            var t980 *ref_int_x = value__28.index
                            var t981 *ref_int_x = value__28.index
                            var t982 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t981)
                            var t983 int = t982 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t980, t983)
                            var t997 *ref_int_x = value__28.index
                            var t998 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t997)
                            var t999 string = value__28.input
                            var t1000 int = _goml_m_inherent_i_string_i_string_i_byte__len(t999)
                            var t1001 bool = t998 < t1000
                            var jp991 bool
                            if t1001 {
                                var t1004 string = value__28.input
                                var t1005 *ref_int_x = value__28.index
                                var t1006 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1005)
                                var t1007 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1004, t1006)
                                var t1008 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1007, 43)
                                if t1008 {
                                    jp991 = true
                                } else {
                                    var t1009 string = value__28.input
                                    var t1010 *ref_int_x = value__28.index
                                    var t1011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1010)
                                    var t1012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1009, t1011)
                                    var t1013 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1012, 45)
                                    jp991 = t1013
                                }
                            } else {
                                jp991 = false
                            }
                            if jp991 {
                                var t992 *ref_int_x = value__28.index
                                var t993 *ref_int_x = value__28.index
                                var t994 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t993)
                                var t995 int = t994 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t992, t995)
                            } else {}
                            var t986 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t987 bool = !t986
                            if t987 {
                                var t988 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t989 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t988,
                                }
                                return t989
                            } else {
                                var t972 string = value__28.input
                                var t973 *ref_int_x = value__28.index
                                var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t973)
                                var t975 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t972, start__29, t974)
                                var t976 _goml_m_std_p_json_p_Value = Number{
                                    _0: t975,
                                }
                                var t977 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t976,
                                }
                                return t977
                            }
                        } else {
                            var t972 string = value__28.input
                            var t973 *ref_int_x = value__28.index
                            var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t973)
                            var t975 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t972, start__29, t974)
                            var t976 _goml_m_std_p_json_p_Value = Number{
                                _0: t975,
                            }
                            var t977 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t976,
                            }
                            return t977
                        }
                    }
                } else {
                    var t1014 *ref_int_x = value__28.index
                    var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                    var t1016 string = value__28.input
                    var t1017 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1016)
                    var t1018 bool = t1015 < t1017
                    var jp979 bool
                    if t1018 {
                        var t1021 string = value__28.input
                        var t1022 *ref_int_x = value__28.index
                        var t1023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1022)
                        var t1024 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1021, t1023)
                        var t1025 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1024, 101)
                        if t1025 {
                            jp979 = true
                        } else {
                            var t1026 string = value__28.input
                            var t1027 *ref_int_x = value__28.index
                            var t1028 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1027)
                            var t1029 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1026, t1028)
                            var t1030 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1029, 69)
                            jp979 = t1030
                        }
                    } else {
                        jp979 = false
                    }
                    if jp979 {
                        var t980 *ref_int_x = value__28.index
                        var t981 *ref_int_x = value__28.index
                        var t982 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t981)
                        var t983 int = t982 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t980, t983)
                        var t997 *ref_int_x = value__28.index
                        var t998 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t997)
                        var t999 string = value__28.input
                        var t1000 int = _goml_m_inherent_i_string_i_string_i_byte__len(t999)
                        var t1001 bool = t998 < t1000
                        var jp991 bool
                        if t1001 {
                            var t1004 string = value__28.input
                            var t1005 *ref_int_x = value__28.index
                            var t1006 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1005)
                            var t1007 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1004, t1006)
                            var t1008 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1007, 43)
                            if t1008 {
                                jp991 = true
                            } else {
                                var t1009 string = value__28.input
                                var t1010 *ref_int_x = value__28.index
                                var t1011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1010)
                                var t1012 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1009, t1011)
                                var t1013 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1012, 45)
                                jp991 = t1013
                            }
                        } else {
                            jp991 = false
                        }
                        if jp991 {
                            var t992 *ref_int_x = value__28.index
                            var t993 *ref_int_x = value__28.index
                            var t994 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t993)
                            var t995 int = t994 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t992, t995)
                        } else {}
                        var t986 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t987 bool = !t986
                        if t987 {
                            var t988 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t989 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t988,
                            }
                            return t989
                        } else {
                            var t972 string = value__28.input
                            var t973 *ref_int_x = value__28.index
                            var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t973)
                            var t975 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t972, start__29, t974)
                            var t976 _goml_m_std_p_json_p_Value = Number{
                                _0: t975,
                            }
                            var t977 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t976,
                            }
                            return t977
                        }
                    } else {
                        var t972 string = value__28.input
                        var t973 *ref_int_x = value__28.index
                        var t974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t973)
                        var t975 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t972, start__29, t974)
                        var t976 _goml_m_std_p_json_p_Value = Number{
                            _0: t975,
                        }
                        var t977 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t976,
                        }
                        return t977
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t1113 *ref_int_x = value__30.index
    var t1114 int
    var inline2491 int = ref_get__Ref_3int(t1113)
    t1114 = inline2491
    var t1115 int
    var inline2489 int = _goml_runtime_core_string_len(expected__31)
    t1115 = inline2489
    var t1116 int = t1114 + t1115
    var t1117 string = value__30.input
    var t1118 int
    var inline2487 int = _goml_runtime_core_string_len(t1117)
    t1118 = inline2487
    var t1119 bool = t1116 <= t1118
    var jp1104 bool
    if t1119 {
        var t1120 string = value__30.input
        var t1121 *ref_int_x = value__30.index
        var t1122 int
        var inline2471 int = ref_get__Ref_3int(t1121)
        t1122 = inline2471
        var t1123 *ref_int_x = value__30.index
        var t1124 int
        var inline2469 int = ref_get__Ref_3int(t1123)
        t1124 = inline2469
        var t1125 int
        var inline2467 int = _goml_runtime_core_string_len(expected__31)
        t1125 = inline2467
        var t1126 int = t1124 + t1125
        var t1127 string
        var inline2465 string = string_byte_slice(t1120, t1122, t1126)
        t1127 = inline2465
        var inline2463 bool = t1127 == expected__31
        jp1104 = inline2463
    } else {
        jp1104 = false
    }
    if jp1104 {
        var t1105 *ref_int_x = value__30.index
        var t1106 *ref_int_x = value__30.index
        var t1107 int
        var inline2477 int = ref_get__Ref_3int(t1106)
        t1107 = inline2477
        var t1108 int
        var inline2475 int = _goml_runtime_core_string_len(expected__31)
        t1108 = inline2475
        var t1109 int = t1107 + t1108
        ref_set__Ref_3int(t1105, t1109)
        var t1110 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        return t1110
    } else {
        var t1111 string
        var inline2479 string = "invalid literal"
        var inline2480 string = "" + inline2479
        var inline2481 string = inline2480 + " at byte "
        var inline2482 *ref_int_x = value__30.index
        var inline2483 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2482)
        var inline2484 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2483)
        var inline2485 string = inline2481 + inline2484
        t1111 = inline2485
        var t1112 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1111,
        }
        return t1112
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1131 *ref_int_x = value__33.index
    var t1132 *ref_int_x = value__33.index
    var t1133 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1132)
    var t1134 int = t1133 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1131, t1134)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var vec_literal__8702 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1189 *ref_int_x = value__33.index
    var t1190 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1189)
    var t1191 string = value__33.input
    var t1192 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1191)
    var t1193 bool = t1190 < t1192
    var jp1182 bool
    if t1193 {
        var t1194 string = value__33.input
        var t1195 *ref_int_x = value__33.index
        var t1196 int
        var inline2498 int = ref_get__Ref_3int(t1195)
        t1196 = inline2498
        var t1197 uint8
        var inline2496 uint8 = _goml_runtime_core_string_byte_get(t1194, t1196)
        t1197 = inline2496
        var inline2493 uint8 = 93
        var inline2494 bool = t1197 == inline2493
        jp1182 = inline2494
    } else {
        jp1182 = false
    }
    if jp1182 {
        var t1183 *ref_int_x = value__33.index
        var t1184 *ref_int_x = value__33.index
        var t1185 int
        var inline2502 int = ref_get__Ref_3int(t1184)
        t1185 = inline2502
        var t1186 int = t1185 + 1
        ref_set__Ref_3int(t1183, t1186)
        var t1187 _goml_m_std_p_json_p_Value = Array{
            _0: vec_literal__8702,
        }
        var t1188 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1187,
        }
        return t1188
    } else {
        Loop_loop1139:
        for {
            var t1140 *ref_int_x = value__33.index
            var t1141 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1140)
            var t1142 string = value__33.input
            var t1143 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1142)
            var t1144 bool = t1141 < t1143
            if t1144 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1146 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp1146 = x51
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8702, jp1146)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1148 *ref_int_x = value__33.index
                    var t1149 int
                    var inline2544 int = ref_get__Ref_3int(t1148)
                    t1149 = inline2544
                    var t1150 string = value__33.input
                    var t1151 int
                    var inline2542 int = _goml_runtime_core_string_len(t1150)
                    t1151 = inline2542
                    var t1152 bool = t1149 >= t1151
                    if t1152 {
                        var t1153 string
                        var inline2504 string = "unterminated array"
                        var inline2505 string = "" + inline2504
                        var inline2506 string = inline2505 + " at byte "
                        var inline2507 *ref_int_x = value__33.index
                        var inline2508 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2507)
                        var inline2509 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2508)
                        var inline2510 string = inline2506 + inline2509
                        t1153 = inline2510
                        var t1154 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1153,
                        }
                        return t1154
                    } else {
                        var t1156 string = value__33.input
                        var t1157 *ref_int_x = value__33.index
                        var t1158 int
                        var inline2540 int = ref_get__Ref_3int(t1157)
                        t1158 = inline2540
                        var t1159 uint8
                        var inline2538 uint8 = _goml_runtime_core_string_byte_get(t1156, t1158)
                        t1159 = inline2538
                        var t1160 bool
                        var inline2535 uint8 = 93
                        var inline2536 bool = t1159 == inline2535
                        t1160 = inline2536
                        if t1160 {
                            var t1161 *ref_int_x = value__33.index
                            var t1162 *ref_int_x = value__33.index
                            var t1163 int
                            var inline2514 int = ref_get__Ref_3int(t1162)
                            t1163 = inline2514
                            var t1164 int = t1163 + 1
                            ref_set__Ref_3int(t1161, t1164)
                            var t1165 _goml_m_std_p_json_p_Value = Array{
                                _0: vec_literal__8702,
                            }
                            var t1166 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1165,
                            }
                            return t1166
                        } else {
                            var t1168 string = value__33.input
                            var t1169 *ref_int_x = value__33.index
                            var t1170 int
                            var inline2533 int = ref_get__Ref_3int(t1169)
                            t1170 = inline2533
                            var t1171 uint8
                            var inline2531 uint8 = _goml_runtime_core_string_byte_get(t1168, t1170)
                            t1171 = inline2531
                            var t1172 bool
                            var inline2528 uint8 = 44
                            var inline2529 bool = t1171 == inline2528
                            t1172 = inline2529
                            if t1172 {
                                var t1173 *ref_int_x = value__33.index
                                var t1174 *ref_int_x = value__33.index
                                var t1175 int
                                var inline2518 int = ref_get__Ref_3int(t1174)
                                t1175 = inline2518
                                var t1176 int = t1175 + 1
                                ref_set__Ref_3int(t1173, t1176)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1178 string
                                var inline2520 string = "expected array separator"
                                var inline2521 string = "" + inline2520
                                var inline2522 string = inline2521 + " at byte "
                                var inline2523 *ref_int_x = value__33.index
                                var inline2524 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2523)
                                var inline2525 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2524)
                                var inline2526 string = inline2522 + inline2525
                                t1178 = inline2526
                                var t1179 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1178,
                                }
                                return t1179
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t1180 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x52,
                    }
                    return t1180
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1139
            }
        }
        var t1137 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1138 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1137,
        }
        return t1138
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1201 *ref_int_x = value__36.index
    var t1202 *ref_int_x = value__36.index
    var t1203 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1202)
    var t1204 int = t1203 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1201, t1204)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var vec_literal__9904 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1284 *ref_int_x = value__36.index
    var t1285 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1284)
    var t1286 string = value__36.input
    var t1287 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1286)
    var t1288 bool = t1285 < t1287
    var jp1277 bool
    if t1288 {
        var t1289 string = value__36.input
        var t1290 *ref_int_x = value__36.index
        var t1291 int
        var inline2551 int = ref_get__Ref_3int(t1290)
        t1291 = inline2551
        var t1292 uint8
        var inline2549 uint8 = _goml_runtime_core_string_byte_get(t1289, t1291)
        t1292 = inline2549
        var inline2546 uint8 = 125
        var inline2547 bool = t1292 == inline2546
        jp1277 = inline2547
    } else {
        jp1277 = false
    }
    if jp1277 {
        var t1278 *ref_int_x = value__36.index
        var t1279 *ref_int_x = value__36.index
        var t1280 int
        var inline2555 int = ref_get__Ref_3int(t1279)
        t1280 = inline2555
        var t1281 int = t1280 + 1
        ref_set__Ref_3int(t1278, t1281)
        var t1282 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__9904,
        }
        var t1283 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1282,
        }
        return t1283
    } else {
        Loop_loop1209:
        for {
            var t1210 *ref_int_x = value__36.index
            var t1211 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1210)
            var t1212 string = value__36.input
            var t1213 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1212)
            var t1214 bool = t1211 < t1213
            if t1214 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1216 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    jp1216 = x63
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1264 *ref_int_x = value__36.index
                    var t1265 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1264)
                    var t1266 string = value__36.input
                    var t1267 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1266)
                    var t1268 bool = t1265 >= t1267
                    var jp1256 bool
                    if t1268 {
                        jp1256 = true
                    } else {
                        var t1269 string = value__36.input
                        var t1270 *ref_int_x = value__36.index
                        var t1271 int
                        var inline2562 int = ref_get__Ref_3int(t1270)
                        t1271 = inline2562
                        var t1272 uint8
                        var inline2560 uint8 = _goml_runtime_core_string_byte_get(t1269, t1271)
                        t1272 = inline2560
                        var t1273 bool
                        var inline2557 uint8 = 58
                        var inline2558 bool = t1272 == inline2557
                        t1273 = inline2558
                        var t1274 bool = !t1273
                        jp1256 = t1274
                    }
                    if jp1256 {
                        var t1257 string
                        var inline2564 string = "expected object colon"
                        var inline2565 string = "" + inline2564
                        var inline2566 string = inline2565 + " at byte "
                        var inline2567 *ref_int_x = value__36.index
                        var inline2568 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2567)
                        var inline2569 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2568)
                        var inline2570 string = inline2566 + inline2569
                        t1257 = inline2570
                        var t1258 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1257,
                        }
                        return t1258
                    } else {
                        var t1259 *ref_int_x = value__36.index
                        var t1260 *ref_int_x = value__36.index
                        var t1261 int
                        var inline2574 int = ref_get__Ref_3int(t1260)
                        t1261 = inline2574
                        var t1262 int = t1261 + 1
                        ref_set__Ref_3int(t1259, t1262)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1219 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp1219 = x69
                            var t1220 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp1216,
                                _1: jp1219,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__9904, t1220)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1222 *ref_int_x = value__36.index
                            var t1223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1222)
                            var t1224 string = value__36.input
                            var t1225 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1224)
                            var t1226 bool = t1223 >= t1225
                            if t1226 {
                                var t1227 string
                                var inline2576 string = "unterminated object"
                                var inline2577 string = "" + inline2576
                                var inline2578 string = inline2577 + " at byte "
                                var inline2579 *ref_int_x = value__36.index
                                var inline2580 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2579)
                                var inline2581 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2580)
                                var inline2582 string = inline2578 + inline2581
                                t1227 = inline2582
                                var t1228 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1227,
                                }
                                return t1228
                            } else {
                                var t1230 string = value__36.input
                                var t1231 *ref_int_x = value__36.index
                                var t1232 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1231)
                                var t1233 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1230, t1232)
                                var t1234 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1233, 125)
                                if t1234 {
                                    var t1235 *ref_int_x = value__36.index
                                    var t1236 *ref_int_x = value__36.index
                                    var t1237 int
                                    var inline2586 int = ref_get__Ref_3int(t1236)
                                    t1237 = inline2586
                                    var t1238 int = t1237 + 1
                                    ref_set__Ref_3int(t1235, t1238)
                                    var t1239 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__9904,
                                    }
                                    var t1240 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1239,
                                    }
                                    return t1240
                                } else {
                                    var t1242 string = value__36.input
                                    var t1243 *ref_int_x = value__36.index
                                    var t1244 int
                                    var inline2597 int = ref_get__Ref_3int(t1243)
                                    t1244 = inline2597
                                    var t1245 uint8
                                    var inline2595 uint8 = _goml_runtime_core_string_byte_get(t1242, t1244)
                                    t1245 = inline2595
                                    var t1246 bool
                                    var inline2592 uint8 = 44
                                    var inline2593 bool = t1245 == inline2592
                                    t1246 = inline2593
                                    if t1246 {
                                        var t1247 *ref_int_x = value__36.index
                                        var t1248 *ref_int_x = value__36.index
                                        var t1249 int
                                        var inline2590 int = ref_get__Ref_3int(t1248)
                                        t1249 = inline2590
                                        var t1250 int = t1249 + 1
                                        ref_set__Ref_3int(t1247, t1250)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1252 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1253 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1252,
                                        }
                                        return t1253
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t1254 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x70,
                            }
                            return t1254
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var t1275 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x64,
                    }
                    return t1275
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1209
            }
        }
        var t1207 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1208 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1207,
        }
        return t1208
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1298 *ref_int_x = value__40.index
    var t1299 int
    var inline2627 int = ref_get__Ref_3int(t1298)
    t1299 = inline2627
    var t1300 string = value__40.input
    var t1301 int
    var inline2625 int = _goml_runtime_core_string_len(t1300)
    t1301 = inline2625
    var t1302 bool = t1299 >= t1301
    if t1302 {
        var t1303 string
        var inline2599 string = "expected JSON value"
        var inline2600 string = "" + inline2599
        var inline2601 string = inline2600 + " at byte "
        var inline2602 *ref_int_x = value__40.index
        var inline2603 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2602)
        var inline2604 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2603)
        var inline2605 string = inline2601 + inline2604
        t1303 = inline2605
        var t1304 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1303,
        }
        return t1304
    } else {
        var t1305 string = value__40.input
        var t1306 *ref_int_x = value__40.index
        var t1307 int
        var inline2623 int = ref_get__Ref_3int(t1306)
        t1307 = inline2623
        var mtmp77 uint8
        var inline2621 uint8 = _goml_runtime_core_string_byte_get(t1305, t1307)
        mtmp77 = inline2621
        switch mtmp77 {
        case 123:
            var t1310 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            return t1310
        case 91:
            var t1311 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            return t1311
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var t1314 _goml_m_std_p_json_p_Value = String{
                    _0: x79,
                }
                var t1315 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1314,
                }
                return t1315
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var t1316 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x80,
                }
                return t1316
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t1317 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t1318 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1317)
            return t1318
        case 102:
            var t1319 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t1320 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1319)
            return t1320
        case 110:
            var t1321 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            return t1321
        default:
            var t1329 bool
            var inline2618 uint8 = 45
            var inline2619 bool = mtmp77 == inline2618
            t1329 = inline2619
            var jp1325 bool
            if t1329 {
                jp1325 = true
            } else {
                var inline2607 bool = mtmp77 >= 48
                if inline2607 {
                    var inline2608 bool = mtmp77 <= 57
                    jp1325 = inline2608
                } else {
                    jp1325 = false
                }
            }
            if jp1325 {
                var t1326 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                return t1326
            } else {
                var t1327 string
                var inline2610 string = "unexpected JSON token"
                var inline2611 string = "" + inline2610
                var inline2612 string = inline2611 + " at byte "
                var inline2613 *ref_int_x = value__40.index
                var inline2614 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2613)
                var inline2615 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2614)
                var inline2616 string = inline2612 + inline2615
                t1327 = inline2616
                var t1328 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1327,
                }
                return t1328
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__45 _goml_m_std_p_json_p_JsonParser
    var inline2643 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline2644 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__44,
        index: inline2643,
    }
    parser__45 = inline2644
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1334 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1334 = x82
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1337 *ref_int_x = parser__45.index
        var t1338 int
        var inline2641 int = ref_get__Ref_3int(t1337)
        t1338 = inline2641
        var t1339 int
        var inline2639 int = _goml_runtime_core_string_len(input__44)
        t1339 = inline2639
        var t1340 bool
        var inline2637 bool = t1338 == t1339
        t1340 = inline2637
        if t1340 {
            var t1341 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp1334,
            }
            return t1341
        } else {
            var t1342 string
            var inline2629 string = "trailing JSON data"
            var inline2630 string = "" + inline2629
            var inline2631 string = inline2630 + " at byte "
            var inline2632 *ref_int_x = parser__45.index
            var inline2633 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2632)
            var inline2634 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2633)
            var inline2635 string = inline2631 + inline2634
            t1342 = inline2635
            var t1343 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1342,
            }
            return t1343
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t1344 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x83,
        }
        return t1344
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1358:
    for {
        var t1359 bool = for_index86 < for_limit87
        if t1359 {
            var for_item88 int = for_index86
            var t1360 int = for_index86 + 1
            for_index86 = t1360
            var byte__52 uint8
            var inline2705 uint8 = _goml_runtime_core_string_byte_get(value__49, for_item88)
            byte__52 = inline2705
            var t1413 bool
            var inline2702 uint8 = 34
            var inline2703 bool = byte__52 == inline2702
            t1413 = inline2703
            var jp1411 bool
            if t1413 {
                jp1411 = true
            } else {
                var inline2649 uint8 = 92
                var inline2650 bool = byte__52 == inline2649
                jp1411 = inline2650
            }
            var jp1408 bool
            if jp1411 {
                jp1408 = true
            } else {
                var inline2652 uint8 = 8
                var inline2653 bool = byte__52 == inline2652
                jp1408 = inline2653
            }
            var jp1405 bool
            if jp1408 {
                jp1405 = true
            } else {
                var inline2655 uint8 = 9
                var inline2656 bool = byte__52 == inline2655
                jp1405 = inline2656
            }
            var jp1402 bool
            if jp1405 {
                jp1402 = true
            } else {
                var inline2658 uint8 = 10
                var inline2659 bool = byte__52 == inline2658
                jp1402 = inline2659
            }
            var jp1399 bool
            if jp1402 {
                jp1399 = true
            } else {
                var inline2661 uint8 = 12
                var inline2662 bool = byte__52 == inline2661
                jp1399 = inline2662
            }
            var jp1396 bool
            if jp1399 {
                jp1396 = true
            } else {
                var inline2664 uint8 = 13
                var inline2665 bool = byte__52 == inline2664
                jp1396 = inline2665
            }
            var jp1363 bool
            if jp1396 {
                jp1363 = true
            } else {
                var t1397 bool = byte__52 < 32
                jp1363 = t1397
            }
            if jp1363 {
                var t1392 bool = start__50 < for_item88
                if t1392 {
                    var t1393 string
                    var inline2667 string = string_byte_slice(value__49, start__50, for_item88)
                    t1393 = inline2667
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1393)
                } else {}
                var t1367 bool
                var inline2699 uint8 = 34
                var inline2700 bool = byte__52 == inline2699
                t1367 = inline2700
                if t1367 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1370 bool
                    var inline2696 uint8 = 92
                    var inline2697 bool = byte__52 == inline2696
                    t1370 = inline2697
                    if t1370 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1373 bool
                        var inline2693 uint8 = 8
                        var inline2694 bool = byte__52 == inline2693
                        t1373 = inline2694
                        if t1373 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1376 bool
                            var inline2690 uint8 = 9
                            var inline2691 bool = byte__52 == inline2690
                            t1376 = inline2691
                            if t1376 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1379 bool
                                var inline2687 uint8 = 10
                                var inline2688 bool = byte__52 == inline2687
                                t1379 = inline2688
                                if t1379 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1382 bool
                                    var inline2684 uint8 = 12
                                    var inline2685 bool = byte__52 == inline2684
                                    t1382 = inline2685
                                    if t1382 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1385 bool
                                        var inline2681 uint8 = 13
                                        var inline2682 bool = byte__52 == inline2681
                                        t1385 = inline2682
                                        if t1385 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1387 uint8 = byte__52 / 16
                                            var t1388 rune
                                            var inline2678 int = int(uint8(t1387))
                                            var inline2679 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline2678)
                                            t1388 = inline2679
                                            var inline2675 string = _goml_m_inherent_i_char_i_char_i_to__string(t1388)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline2675)
                                            var t1389_rhs uint8 = 16
                                            var t1389 uint8 = byte__52 % t1389_rhs
                                            var t1390 rune
                                            var inline2672 int = int(uint8(t1389))
                                            var inline2673 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline2672)
                                            t1390 = inline2673
                                            var inline2669 string = _goml_m_inherent_i_char_i_char_i_to__string(t1390)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline2669)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1366 int = for_item88 + 1
                start__50 = t1366
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop1358
        }
    }
    var t1353 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1354 bool = start__50 < t1353
    if t1354 {
        var t1355 int
        var inline2709 int = _goml_runtime_core_string_len(value__49)
        t1355 = inline2709
        var t1356 string
        var inline2707 string = string_byte_slice(value__49, start__50, t1355)
        t1356 = inline2707
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1356)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__53 _goml_m_std_p_text_p_StringBuilder, value__54 _goml_m_std_p_json_p_Value) struct{} {
    switch value__54.(type) {
    case Object:
        var x97 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__54.(Object)._0
        var inline2723 rune = 123
        var inline2724 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2723)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2724)
        var index__56 int = 0
        var for_limit104 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97)
        var for_index105 int = 0
        Loop_loop1419:
        for {
            var t1420 bool = for_index105 < for_limit104
            if t1420 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97, for_index105)
                var t1421 int = for_index105 + 1
                for_index105 = t1421
                var t1427 bool = index__56 > 0
                if t1427 {
                    var inline2711 rune = 44
                    var inline2712 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2711)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2712)
                } else {}
                var t1423 string = for_item106._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1423)
                var inline2715 rune = 58
                var inline2716 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2715)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2716)
                var t1424 _goml_m_std_p_json_p_Value = for_item106._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1424)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1425 int = compound_old112 + compound_value113
                index__56 = t1425
                continue
            } else {
                break Loop_loop1419
            }
        }
        var inline2719 rune = 125
        var inline2720 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2719)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2720)
        return struct{}{}
    case Array:
        var x98 *_goml_vec__goml_m_std_p_json_p_Value = value__54.(Array)._0
        var inline2735 rune = 91
        var inline2736 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2735)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2736)
        var index__59 int = 0
        var for_limit118 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x98)
        var for_index119 int = 0
        Loop_loop1431:
        for {
            var t1432 bool = for_index119 < for_limit118
            if t1432 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x98, for_index119)
                var t1433 int = for_index119 + 1
                for_index119 = t1433
                var t1437 bool = index__59 > 0
                if t1437 {
                    var inline2727 rune = 44
                    var inline2728 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2727)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2728)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, for_item120)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1435 int = compound_old124 + compound_value125
                index__59 = t1435
                continue
            } else {
                break Loop_loop1431
            }
        }
        var inline2731 rune = 93
        var inline2732 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2731)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2732)
        return struct{}{}
    case String:
        var x99 string = value__54.(String)._0
        _goml_m_std_p_json_p_write__json__string(builder__53, x99)
        return struct{}{}
    case Number:
        var x100 string = value__54.(Number)._0
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, x100)
        return struct{}{}
    case Bool:
        var x101 bool = value__54.(Bool)._0
        var jp1442 string
        if x101 {
            jp1442 = "true"
        } else {
            jp1442 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1442)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__64 _goml_m_std_p_json_p_Value) string {
    var builder__65 _goml_m_std_p_text_p_StringBuilder
    var inline2745 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline2746 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline2745,
    }
    builder__65 = inline2746
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var inline2739 *_goml_vec_uint8 = builder__65.values
    var inline2740 Tuple2_4bool_6string = string_from_utf8(inline2739)
    var inline2742 string = inline2740._1
    return inline2742
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129)
        var for_index136 int = 0
        Loop_loop1453:
        for {
            var t1454 bool = for_index136 < for_limit135
            if t1454 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129, for_index136)
                var t1455 int = for_index136 + 1
                for_index136 = t1455
                var t1457 string = for_item137._0
                var t1458 bool
                var inline2748 bool = t1457 == name__67
                t1458 = inline2748
                if t1458 {
                    var t1459 _goml_m_std_p_json_p_Value = for_item137._1
                    var t1460 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1459,
                    }
                    return t1460
                } else {
                    continue
                }
            } else {
                break Loop_loop1453
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var t1470 int
    var inline2767 int = _goml_runtime_core_string_len(value__72)
    t1470 = inline2767
    var t1471 bool
    var inline2764 int = 0
    var inline2765 bool = t1470 == inline2764
    t1471 = inline2765
    if t1471 {
        return Option__int_None{}
    } else {
        var t1472 uint8
        var inline2761 int = 0
        var inline2762 uint8 = _goml_runtime_core_string_byte_get(value__72, inline2761)
        t1472 = inline2762
        var negative__73 bool
        var inline2758 uint8 = 45
        var inline2759 bool = t1472 == inline2758
        negative__73 = inline2759
        var jp1474 int
        if negative__73 {
            jp1474 = 1
        } else {
            jp1474 = 0
        }
        var index__74 int = jp1474
        var result__75 int = 0
        var t1495 int
        var inline2756 int = _goml_runtime_core_string_len(value__72)
        t1495 = inline2756
        var t1496 bool
        var inline2754 bool = index__74 == t1495
        t1496 = inline2754
        if t1496 {
            return Option__int_None{}
        } else {
            Loop_loop1481:
            for {
                var t1482 int
                var inline2752 int = _goml_runtime_core_string_len(value__72)
                t1482 = inline2752
                var t1483 bool = index__74 < t1482
                if t1483 {
                    var byte__76 uint8
                    var inline2750 uint8 = _goml_runtime_core_string_byte_get(value__72, index__74)
                    byte__76 = inline2750
                    var t1493 bool = byte__76 < 48
                    var jp1488 bool
                    if t1493 {
                        jp1488 = true
                    } else {
                        var t1494 bool = byte__76 > 57
                        jp1488 = t1494
                    }
                    if jp1488 {
                        return Option__int_None{}
                    } else {
                        var t1489 int = result__75 * 10
                        var t1490 uint8 = byte__76 - 48
                        var t1491 int = int(uint8(t1490))
                        var t1492 int = t1489 + t1491
                        result__75 = t1492
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1485 int = compound_old148 + compound_value149
                        index__74 = t1485
                        continue
                    }
                } else {
                    break Loop_loop1481
                }
            }
            var jp1478 int
            if negative__73 {
                var t1480 int = 0 - result__75
                jp1478 = t1480
            } else {
                jp1478 = result__75
            }
            var t1479 Option__int = Option__int_Some{
                _0: jp1478,
            }
            return t1479
        }
    }
}

func main0() struct{} {
    var mtmp136 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1517 _goml_m_std_p_json_p_Value
    switch mtmp136.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x137 _goml_m_std_p_json_p_Value = mtmp136.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1517 = x137
        var mtmp140 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1517, "name")
        switch mtmp140.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline2772 string = "missing name"
            var inline2773 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2772)
            _goml_runtime_core_string_println(inline2773)
            var mtmp145 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1517, "version")
            switch mtmp145.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline2787 string = "missing version"
                var inline2788 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2787)
                _goml_runtime_core_string_println(inline2788)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x146 _goml_m_std_p_json_p_Value = mtmp145.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp147 Option__int
                switch x146.(type) {
                case Number:
                    var inline2798 string = x146.(Number)._0
                    var inline2800 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2798)
                    mtmp147 = inline2800
                default:
                    mtmp147 = Option__int_None{}
                }
                switch mtmp147.(type) {
                case Option__int_None:
                    var inline2791 string = "invalid version"
                    var inline2792 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2791)
                    _goml_runtime_core_string_println(inline2792)
                case Option__int_Some:
                    var x148 int = mtmp147.(Option__int_Some)._0
                    var inline2795 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x148)
                    _goml_runtime_core_string_println(inline2795)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp150 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1517, "stable")
            switch mtmp150.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline2802 string = "missing stable"
                var inline2803 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2802)
                _goml_runtime_core_string_println(inline2803)
                var t1521 string = _goml_m_std_p_json_p_encode(jp1517)
                println__T_string(t1521)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x151 _goml_m_std_p_json_p_Value = mtmp150.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field2962 bool
                switch x151.(type) {
                case Bool:
                    var inline2813 bool = x151.(Bool)._0
                    commute_field2962 = inline2813
                    var inline2810 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2962)
                    _goml_runtime_core_string_println(inline2810)
                    var t1521 string = _goml_m_std_p_json_p_encode(jp1517)
                    println__T_string(t1521)
                    return struct{}{}
                default:
                    var inline2806 string = "invalid stable"
                    var inline2807 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2806)
                    _goml_runtime_core_string_println(inline2807)
                    var t1521 string = _goml_m_std_p_json_p_encode(jp1517)
                    println__T_string(t1521)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x141 _goml_m_std_p_json_p_Value = mtmp140.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field2968 string
            switch x141.(type) {
            case String:
                var inline2783 string = x141.(String)._0
                commute_field2968 = inline2783
                var inline2780 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field2968)
                _goml_runtime_core_string_println(inline2780)
                var mtmp145 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1517, "version")
                switch mtmp145.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2787 string = "missing version"
                    var inline2788 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2787)
                    _goml_runtime_core_string_println(inline2788)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x146 _goml_m_std_p_json_p_Value = mtmp145.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp147 Option__int
                    switch x146.(type) {
                    case Number:
                        var inline2798 string = x146.(Number)._0
                        var inline2800 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2798)
                        mtmp147 = inline2800
                    default:
                        mtmp147 = Option__int_None{}
                    }
                    switch mtmp147.(type) {
                    case Option__int_None:
                        var inline2791 string = "invalid version"
                        var inline2792 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2791)
                        _goml_runtime_core_string_println(inline2792)
                    case Option__int_Some:
                        var x148 int = mtmp147.(Option__int_Some)._0
                        var inline2795 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x148)
                        _goml_runtime_core_string_println(inline2795)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp150 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1517, "stable")
                switch mtmp150.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2802 string = "missing stable"
                    var inline2803 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2802)
                    _goml_runtime_core_string_println(inline2803)
                    var t1521 string = _goml_m_std_p_json_p_encode(jp1517)
                    println__T_string(t1521)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x151 _goml_m_std_p_json_p_Value = mtmp150.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field2962 bool
                    switch x151.(type) {
                    case Bool:
                        var inline2813 bool = x151.(Bool)._0
                        commute_field2962 = inline2813
                        var inline2810 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2962)
                        _goml_runtime_core_string_println(inline2810)
                        var t1521 string = _goml_m_std_p_json_p_encode(jp1517)
                        println__T_string(t1521)
                        return struct{}{}
                    default:
                        var inline2806 string = "invalid stable"
                        var inline2807 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2806)
                        _goml_runtime_core_string_println(inline2807)
                        var t1521 string = _goml_m_std_p_json_p_encode(jp1517)
                        println__T_string(t1521)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline2776 string = "invalid name"
                var inline2777 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2776)
                _goml_runtime_core_string_println(inline2777)
                var mtmp145 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1517, "version")
                switch mtmp145.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2787 string = "missing version"
                    var inline2788 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2787)
                    _goml_runtime_core_string_println(inline2788)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x146 _goml_m_std_p_json_p_Value = mtmp145.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp147 Option__int
                    switch x146.(type) {
                    case Number:
                        var inline2798 string = x146.(Number)._0
                        var inline2800 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2798)
                        mtmp147 = inline2800
                    default:
                        mtmp147 = Option__int_None{}
                    }
                    switch mtmp147.(type) {
                    case Option__int_None:
                        var inline2791 string = "invalid version"
                        var inline2792 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2791)
                        _goml_runtime_core_string_println(inline2792)
                    case Option__int_Some:
                        var x148 int = mtmp147.(Option__int_Some)._0
                        var inline2795 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x148)
                        _goml_runtime_core_string_println(inline2795)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp150 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1517, "stable")
                switch mtmp150.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2802 string = "missing stable"
                    var inline2803 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2802)
                    _goml_runtime_core_string_println(inline2803)
                    var t1521 string = _goml_m_std_p_json_p_encode(jp1517)
                    println__T_string(t1521)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x151 _goml_m_std_p_json_p_Value = mtmp150.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field2962 bool
                    switch x151.(type) {
                    case Bool:
                        var inline2813 bool = x151.(Bool)._0
                        commute_field2962 = inline2813
                        var inline2810 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2962)
                        _goml_runtime_core_string_println(inline2810)
                        var t1521 string = _goml_m_std_p_json_p_encode(jp1517)
                        println__T_string(t1521)
                        return struct{}{}
                    default:
                        var inline2806 string = "invalid stable"
                        var inline2807 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2806)
                        _goml_runtime_core_string_println(inline2807)
                        var t1521 string = _goml_m_std_p_json_p_encode(jp1517)
                        println__T_string(t1521)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            panic("non-exhaustive match")
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x138 string = mtmp136.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var inline2769 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x138)
        _goml_runtime_core_string_println(inline2769)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__98 uint8, other__99 uint8) bool {
    var t1537 bool = self__98 == other__99
    return t1537
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline2817 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline2818 bool = inline2817._0
    var inline2819 rune = inline2817._1
    if inline2818 {
        return inline2819
    } else {
        var inline2823 rune = _goml_runtime_core_string_get("", -1)
        return inline2823
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t1543 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t1543
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1550:
    for {
        var t1551 int
        var inline2825 int = _goml_runtime_core_string_len(x12)
        t1551 = inline2825
        var t1552 bool = index__26 < t1551
        if t1552 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1554 int = compound_old17 + x16
                index__26 = t1554
                continue
            } else {
                var t1556 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1556
            }
        } else {
            break Loop_loop1550
        }
    }
    var t1549 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t1549
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline2827 uint32 = uint32(rune(self__36))
    var inline2828 bool = utf8_valid_scalar(inline2827)
    if inline2828 {
        var inline2829 string = _goml_runtime_core_char_to_string(self__36)
        return inline2829
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t1595 int = _goml_runtime_core_string_len(self__38)
    return t1595
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t1598 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t1598
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline2845 bool = string_is_char_boundary(self__43, start__44)
    var inline2847 bool
    if inline2845 {
        var inline2850 bool = string_is_char_boundary(self__43, end__45)
        inline2847 = inline2850
    } else {
        inline2847 = false
    }
    if inline2847 {
        var inline2848 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline2848
    } else {
        var inline2849 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline2849
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__215 int) *ref_int_x {
    var t1633 *ref_int_x = ref__Ref_3int(value__215)
    return t1633
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__216 *ref_int_x) int {
    var t1636 int = ref_get__Ref_3int(self__216)
    return t1636
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t1639 string = _goml_runtime_core_int_to_string(self__34)
    return t1639
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__217 *ref_int_x, value__218 int) struct{} {
    ref_set__Ref_3int(self__217, value__218)
    return struct{}{}
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1646 bool
    var inline2862 bool = value__32 <= 1114111
    if inline2862 {
        var inline2863 bool = value__32 >= 55296
        var inline2865 bool
        if inline2863 {
            var inline2867 bool = value__32 <= 57343
            inline2865 = inline2867
        } else {
            inline2865 = false
        }
        var inline2866 bool = !inline2865
        t1646 = inline2866
    } else {
        t1646 = false
    }
    if t1646 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1647 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1647
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t1650 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t1650
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__134 *_goml_vec__goml_m_std_p_json_p_Value, elem__135 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__134, elem__135)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t1655 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t1655
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__134 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__135 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__134, elem__135)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t1659 string
    t1659 = value__31
    _goml_runtime_core_string_println(t1659)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1790 bool = index__6 < 0
    var jp1788 bool
    if t1790 {
        jp1788 = true
    } else {
        var t1791 bool = index__6 >= length__7
        jp1788 = t1791
    }
    if jp1788 {
        var inline2874 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2874
    } else {
        var t1675 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1675))
        var t1678 bool = first__8 < 128
        if t1678 {
            var inline2876 int = 1
            var inline2877 Option__char = char_from_uint32(first__8)
            switch inline2877.(type) {
            case Option__char_None:
                var inline2878 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2878
            case Option__char_Some:
                var inline2879 rune = inline2877.(Option__char_Some)._0
                var inline2881 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2879,
                    _2: inline2876,
                }
                return inline2881
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1682 bool = first__8 < 194
            if t1682 {
                var inline2883 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2883
            } else {
                var t1686 bool = first__8 < 224
                if t1686 {
                    var t1699 int = length__7 - index__6
                    var t1700 bool = t1699 < 2
                    if t1700 {
                        var inline2885 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2885
                    } else {
                        var t1688 int = index__6 + 1
                        var t1689 uint8
                        var inline2899 uint8 = _goml_runtime_core_string_byte_get(value__5, t1688)
                        t1689 = inline2899
                        var second__9 uint32 = uint32(uint8(t1689))
                        var t1692 bool
                        var inline2896 bool = second__9 < 128
                        if inline2896 {
                            t1692 = true
                        } else {
                            var inline2897 bool = second__9 > 191
                            t1692 = inline2897
                        }
                        if t1692 {
                            var inline2887 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2887
                        } else {
                            var t1694_rhs uint32 = 31
                            var t1694 uint32 = first__8 & t1694_rhs
                            var t1695_rhs int = 6
                            var t1695 uint32 = t1694 << t1695_rhs
                            var t1696_rhs uint32 = 63
                            var t1696 uint32 = second__9 & t1696_rhs
                            var t1697 uint32 = t1695 | t1696
                            var inline2889 int = 2
                            var inline2890 Option__char = char_from_uint32(t1697)
                            switch inline2890.(type) {
                            case Option__char_None:
                                var inline2891 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2891
                            case Option__char_Some:
                                var inline2892 rune = inline2890.(Option__char_Some)._0
                                var inline2894 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2892,
                                    _2: inline2889,
                                }
                                return inline2894
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1704 bool = first__8 < 240
                    if t1704 {
                        var t1737 int = length__7 - index__6
                        var t1738 bool = t1737 < 3
                        if t1738 {
                            var inline2901 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2901
                        } else {
                            var t1706 int = index__6 + 1
                            var t1707 uint8
                            var inline2916 uint8 = _goml_runtime_core_string_byte_get(value__5, t1706)
                            t1707 = inline2916
                            var second__10 uint32 = uint32(uint8(t1707))
                            var t1708 int = index__6 + 2
                            var t1709 uint8
                            var inline2914 uint8 = _goml_runtime_core_string_byte_get(value__5, t1708)
                            t1709 = inline2914
                            var third__11 uint32 = uint32(uint8(t1709))
                            var t1735 bool = utf8_invalid_continuation(second__10)
                            var jp1730 bool
                            if t1735 {
                                jp1730 = true
                            } else {
                                var inline2903 bool = third__11 < 128
                                if inline2903 {
                                    jp1730 = true
                                } else {
                                    var inline2904 bool = third__11 > 191
                                    jp1730 = inline2904
                                }
                            }
                            var jp1724 bool
                            if jp1730 {
                                jp1724 = true
                            } else {
                                var t1733 bool
                                var inline2906 uint32 = 224
                                var inline2907 bool = first__8 == inline2906
                                t1733 = inline2907
                                if t1733 {
                                    var t1734 bool = second__10 < 160
                                    jp1724 = t1734
                                } else {
                                    jp1724 = false
                                }
                            }
                            var jp1713 bool
                            if jp1724 {
                                jp1713 = true
                            } else {
                                var t1727 bool
                                var inline2909 uint32 = 237
                                var inline2910 bool = first__8 == inline2909
                                t1727 = inline2910
                                if t1727 {
                                    var t1728 bool = second__10 >= 160
                                    jp1713 = t1728
                                } else {
                                    jp1713 = false
                                }
                            }
                            if jp1713 {
                                var inline2912 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2912
                            } else {
                                var t1715_rhs uint32 = 15
                                var t1715 uint32 = first__8 & t1715_rhs
                                var t1716_rhs int = 12
                                var t1716 uint32 = t1715 << t1716_rhs
                                var t1717_rhs uint32 = 63
                                var t1717 uint32 = second__10 & t1717_rhs
                                var t1718_rhs int = 6
                                var t1718 uint32 = t1717 << t1718_rhs
                                var t1719 uint32 = t1716 | t1718
                                var t1720_rhs uint32 = 63
                                var t1720 uint32 = third__11 & t1720_rhs
                                var t1721 uint32 = t1719 | t1720
                                var t1722 Tuple3_4bool_4char_3int = utf8_valid_decode(t1721, 3)
                                return t1722
                            }
                        }
                    } else {
                        var t1742 bool = first__8 < 245
                        if t1742 {
                            var t1783 int = length__7 - index__6
                            var t1784 bool = t1783 < 4
                            if t1784 {
                                var t1785 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1785
                            } else {
                                var t1744 int = index__6 + 1
                                var t1745 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1744)
                                var second__12 uint32 = uint32(uint8(t1745))
                                var t1746 int = index__6 + 2
                                var t1747 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1746)
                                var third__13 uint32 = uint32(uint8(t1747))
                                var t1748 int = index__6 + 3
                                var t1749 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1748)
                                var fourth__14 uint32 = uint32(uint8(t1749))
                                var t1781 bool = utf8_invalid_continuation(second__12)
                                var jp1779 bool
                                if t1781 {
                                    jp1779 = true
                                } else {
                                    var t1782 bool = utf8_invalid_continuation(third__13)
                                    jp1779 = t1782
                                }
                                var jp1773 bool
                                if jp1779 {
                                    jp1773 = true
                                } else {
                                    var t1780 bool = utf8_invalid_continuation(fourth__14)
                                    jp1773 = t1780
                                }
                                var jp1767 bool
                                if jp1773 {
                                    jp1767 = true
                                } else {
                                    var t1776 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t1776 {
                                        var t1777 bool = second__12 < 144
                                        jp1767 = t1777
                                    } else {
                                        jp1767 = false
                                    }
                                }
                                var jp1753 bool
                                if jp1767 {
                                    jp1753 = true
                                } else {
                                    var t1770 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t1770 {
                                        var t1771 bool = second__12 > 143
                                        jp1753 = t1771
                                    } else {
                                        jp1753 = false
                                    }
                                }
                                if jp1753 {
                                    var t1754 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1754
                                } else {
                                    var t1755_rhs uint32 = 7
                                    var t1755 uint32 = first__8 & t1755_rhs
                                    var t1756_rhs int = 18
                                    var t1756 uint32 = t1755 << t1756_rhs
                                    var t1757_rhs uint32 = 63
                                    var t1757 uint32 = second__12 & t1757_rhs
                                    var t1758_rhs int = 12
                                    var t1758 uint32 = t1757 << t1758_rhs
                                    var t1759 uint32 = t1756 | t1758
                                    var t1760_rhs uint32 = 63
                                    var t1760 uint32 = third__13 & t1760_rhs
                                    var t1761_rhs int = 6
                                    var t1761 uint32 = t1760 << t1761_rhs
                                    var t1762 uint32 = t1759 | t1761
                                    var t1763_rhs uint32 = 63
                                    var t1763 uint32 = fourth__14 & t1763_rhs
                                    var t1764 uint32 = t1762 | t1763
                                    var t1765 Tuple3_4bool_4char_3int = utf8_valid_decode(t1764, 4)
                                    return t1765
                                }
                            }
                        } else {
                            var t1786 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1786
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t1796 uint32 = uint32(rune(value__29))
    var t1797 bool
    var inline2918 bool = t1796 <= 1114111
    if inline2918 {
        var inline2919 bool = t1796 >= 55296
        var inline2921 bool
        if inline2919 {
            var inline2923 bool = t1796 <= 57343
            inline2921 = inline2923
        } else {
            inline2921 = false
        }
        var inline2922 bool = !inline2921
        t1797 = inline2922
    } else {
        t1797 = false
    }
    if t1797 {
        var t1798 string = _goml_runtime_core_char_to_string(value__29)
        return t1798
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t1813 bool = index__16 < 0
    var jp1804 bool
    if t1813 {
        jp1804 = true
    } else {
        var t1814 int
        var inline2925 int = _goml_runtime_core_string_len(value__15)
        t1814 = inline2925
        var t1815 bool = index__16 > t1814
        jp1804 = t1815
    }
    if jp1804 {
        return false
    } else {
        var t1807 int
        var inline2934 int = _goml_runtime_core_string_len(value__15)
        t1807 = inline2934
        var t1808 bool
        var inline2932 bool = index__16 == t1807
        t1808 = inline2932
        if t1808 {
            return true
        } else {
            var t1809 uint8
            var inline2930 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1809 = inline2930
            var t1810_rhs uint8 = 192
            var t1810 uint8 = t1809 & t1810_rhs
            var t1811 bool
            var inline2927 uint8 = 128
            var inline2928 bool = t1810 == inline2927
            t1811 = inline2928
            var t1812 bool = !t1811
            return t1812
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t1824 bool = string_is_char_boundary(value__21, start__22)
    var jp1821 bool
    if t1824 {
        var t1825 bool = string_is_char_boundary(value__21, end__23)
        jp1821 = t1825
    } else {
        jp1821 = false
    }
    if jp1821 {
        var t1822 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t1822
    } else {
        var t1823 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t1823
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1832 bool = value__4 <= 1114111
    if t1832 {
        var t1836 bool = value__4 >= 55296
        var jp1834 bool
        if t1836 {
            var t1837 bool = value__4 <= 57343
            jp1834 = t1837
        } else {
            jp1834 = false
        }
        var t1835 bool = !jp1834
        return t1835
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t1842 string = _goml_runtime_core_int_to_string(self__69)
    return t1842
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t1845 string = _goml_runtime_core_bool_to_string(self__66)
    return t1845
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1848 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1848
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field2971 rune
    var inline2938 bool = utf8_valid_scalar(value__0)
    if inline2938 {
        var inline2939 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline2941 rune = inline2939._1
        commute_field2971 = inline2941
        var t1854 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2971,
            _2: width__1,
        }
        return t1854
    } else {
        var inline2936 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2936
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1859 bool = value__3 < 128
    if t1859 {
        return true
    } else {
        var t1860 bool = value__3 > 191
        return t1860
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t1863 bool = self__102 == other__103
    return t1863
}

func main() {
    main0()
}
