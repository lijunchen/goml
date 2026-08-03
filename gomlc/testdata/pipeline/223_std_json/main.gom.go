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
    var inline2015 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline2015
    var t386 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t386
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline2030 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline2030
    var t400 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t400, length__5)
    var for_index1 int = 0
    Loop_loop402:
    for {
        var t403 bool = for_index1 < length__5
        if t403 {
            var for_item3 int = for_index1
            var t404 int = for_index1 + 1
            for_index1 = t404
            var t405 *_goml_vec_uint8 = self__3.values
            var t406 uint8
            var inline2026 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t406 = inline2026
            vec_push__Vec_5uint8(t405, t406)
            continue
        } else {
            break Loop_loop402
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t409 string
    var inline2032 string = char_to_string(value__8)
    t409 = inline2032
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t409)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var t674 string = "" + message__2
    var t675 string = t674 + " at byte "
    var t676 *ref_int_x = value__1.index
    var t677 int
    var inline2257 int = ref_get__Ref_3int(t676)
    t677 = inline2257
    var t678 string
    var inline2255 string = _goml_runtime_core_int_to_string(t677)
    t678 = inline2255
    var t679 string = t675 + t678
    return t679
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop694:
    for {
        var t702 *ref_int_x = value__4.index
        var t703 int
        var inline2290 int = ref_get__Ref_3int(t702)
        t703 = inline2290
        var t704 string = value__4.input
        var t705 int
        var inline2288 int = _goml_runtime_core_string_len(t704)
        t705 = inline2288
        var t706 bool = t703 < t705
        var jp696 bool
        if t706 {
            var t707 string = value__4.input
            var t708 *ref_int_x = value__4.index
            var t709 int
            var inline2282 int = ref_get__Ref_3int(t708)
            t709 = inline2282
            var t710 uint8
            var inline2280 uint8 = _goml_runtime_core_string_byte_get(t707, t709)
            t710 = inline2280
            var inline2271 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t710, 9)
            var inline2273 bool
            if inline2271 {
                inline2273 = true
            } else {
                var inline2278 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t710, 10)
                inline2273 = inline2278
            }
            var inline2275 bool
            if inline2273 {
                inline2275 = true
            } else {
                var inline2277 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t710, 13)
                inline2275 = inline2277
            }
            if inline2275 {
                jp696 = true
            } else {
                var inline2276 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t710, 32)
                jp696 = inline2276
            }
        } else {
            jp696 = false
        }
        if jp696 {
            var t697 *ref_int_x = value__4.index
            var t698 *ref_int_x = value__4.index
            var t699 int
            var inline2286 int = ref_get__Ref_3int(t698)
            t699 = inline2286
            var t700 int = t699 + 1
            ref_set__Ref_3int(t697, t700)
            continue
        } else {
            break Loop_loop694
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var t741 bool = value__5 >= 48
    var jp717 bool
    if t741 {
        var t742 bool = value__5 <= 57
        jp717 = t742
    } else {
        jp717 = false
    }
    if jp717 {
        var t718 uint8 = value__5 - 48
        var t719 uint32 = uint32(uint8(t718))
        var t720 Option__uint32 = Option__uint32_Some{
            _0: t719,
        }
        return t720
    } else {
        var t739 bool = value__5 >= 65
        var jp724 bool
        if t739 {
            var t740 bool = value__5 <= 70
            jp724 = t740
        } else {
            jp724 = false
        }
        if jp724 {
            var t725 uint8 = value__5 - 65
            var t726 uint8 = t725 + 10
            var t727 uint32 = uint32(uint8(t726))
            var t728 Option__uint32 = Option__uint32_Some{
                _0: t727,
            }
            return t728
        } else {
            var t737 bool = value__5 >= 97
            var jp732 bool
            if t737 {
                var t738 bool = value__5 <= 102
                jp732 = t738
            } else {
                jp732 = false
            }
            if jp732 {
                var t733 uint8 = value__5 - 97
                var t734 uint8 = t733 + 10
                var t735 uint32 = uint32(uint8(t734))
                var t736 Option__uint32 = Option__uint32_Some{
                    _0: t735,
                }
                return t736
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t747 *ref_int_x = value__6.index
    var t748 int
    var inline2318 int = ref_get__Ref_3int(t747)
    t748 = inline2318
    var t749 int = t748 + 4
    var t750 string = value__6.input
    var t751 int
    var inline2316 int = _goml_runtime_core_string_len(t750)
    t751 = inline2316
    var t752 bool = t749 > t751
    if t752 {
        var t753 string
        var inline2292 string = "incomplete unicode escape"
        var inline2293 string = "" + inline2292
        var inline2294 string = inline2293 + " at byte "
        var inline2295 *ref_int_x = value__6.index
        var inline2296 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2295)
        var inline2297 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2296)
        var inline2298 string = inline2294 + inline2297
        t753 = inline2298
        var t754 Result__uint32__string = Result__uint32__string_Err{
            _0: t753,
        }
        return t754
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop761:
        for {
            var t762 bool = for_index0 < for_limit1
            if t762 {
                var for_item2 int = for_index0
                var t763 int = for_index0 + 1
                for_index0 = t763
                var t764 string = value__6.input
                var t765 *ref_int_x = value__6.index
                var t766 int
                var inline2310 int = ref_get__Ref_3int(t765)
                t766 = inline2310
                var t767 int = t766 + for_item2
                var t768 uint8
                var inline2308 uint8 = _goml_runtime_core_string_byte_get(t764, t767)
                t768 = inline2308
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t768)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t770 string
                    var inline2300 string = "invalid unicode escape"
                    var inline2301 string = "" + inline2300
                    var inline2302 string = inline2301 + " at byte "
                    var inline2303 *ref_int_x = value__6.index
                    var inline2304 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2303)
                    var inline2305 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2304)
                    var inline2306 string = inline2302 + inline2305
                    t770 = inline2306
                    var t771 Result__uint32__string = Result__uint32__string_Err{
                        _0: t770,
                    }
                    return t771
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var t772 uint32 = result__7 * 16
                    var t773 uint32 = t772 + x5
                    result__7 = t773
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop761
            }
        }
        var t756 *ref_int_x = value__6.index
        var t757 *ref_int_x = value__6.index
        var t758 int
        var inline2314 int = ref_get__Ref_3int(t757)
        t758 = inline2314
        var t759 int = t758 + 4
        ref_set__Ref_3int(t756, t759)
        var t760 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        return t760
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var commute_field2993 rune
    var inline2331 bool = utf8_valid_scalar(codepoint__12)
    if inline2331 {
        var inline2332 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__12)
        var inline2334 rune = inline2332._1
        commute_field2993 = inline2334
        var inline2328 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field2993)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__11, inline2328)
        var t780 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t780
    } else {
        var t778 string
        var inline2320 string = "invalid unicode codepoint"
        var inline2321 string = "" + inline2320
        var inline2322 string = inline2321 + " at byte "
        var inline2323 *ref_int_x = value__10.index
        var inline2324 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2323)
        var inline2325 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2324)
        var inline2326 string = inline2322 + inline2325
        t778 = inline2326
        var t779 Result__unit__string = Result__unit__string_Err{
            _0: t778,
        }
        return t779
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp784 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        jp784 = x13
        var t846 bool = jp784 >= 55296
        var jp788 bool
        if t846 {
            var t847 bool = jp784 <= 56319
            jp788 = t847
        } else {
            jp788 = false
        }
        if jp788 {
            var t825 *ref_int_x = value__14.index
            var t826 int
            var inline2382 int = ref_get__Ref_3int(t825)
            t826 = inline2382
            var t827 int = t826 + 2
            var t828 string = value__14.input
            var t829 int
            var inline2380 int = _goml_runtime_core_string_len(t828)
            t829 = inline2380
            var t830 bool = t827 > t829
            var jp817 bool
            if t830 {
                jp817 = true
            } else {
                var t831 string = value__14.input
                var t832 *ref_int_x = value__14.index
                var t833 int
                var inline2343 int = ref_get__Ref_3int(t832)
                t833 = inline2343
                var t834 uint8
                var inline2341 uint8 = _goml_runtime_core_string_byte_get(t831, t833)
                t834 = inline2341
                var t835 bool
                var inline2338 uint8 = 92
                var inline2339 bool = t834 == inline2338
                t835 = inline2339
                var t836 bool = !t835
                jp817 = t836
            }
            var jp792 bool
            if jp817 {
                jp792 = true
            } else {
                var t818 string = value__14.input
                var t819 *ref_int_x = value__14.index
                var t820 int
                var inline2350 int = ref_get__Ref_3int(t819)
                t820 = inline2350
                var t821 int = t820 + 1
                var t822 uint8
                var inline2348 uint8 = _goml_runtime_core_string_byte_get(t818, t821)
                t822 = inline2348
                var t823 bool
                var inline2345 uint8 = 117
                var inline2346 bool = t822 == inline2345
                t823 = inline2346
                var t824 bool = !t823
                jp792 = t824
            }
            if jp792 {
                var t793 string
                var inline2352 string = "missing low surrogate"
                var inline2353 string = "" + inline2352
                var inline2354 string = inline2353 + " at byte "
                var inline2355 *ref_int_x = value__14.index
                var inline2356 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2355)
                var inline2357 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2356)
                var inline2358 string = inline2354 + inline2357
                t793 = inline2358
                var t794 Result__unit__string = Result__unit__string_Err{
                    _0: t793,
                }
                return t794
            } else {
                var t795 *ref_int_x = value__14.index
                var t796 *ref_int_x = value__14.index
                var t797 int
                var inline2378 int = ref_get__Ref_3int(t796)
                t797 = inline2378
                var t798 int = t797 + 2
                ref_set__Ref_3int(t795, t798)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp800 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    jp800 = x17
                    var t813 bool = jp800 < 56320
                    var jp804 bool
                    if t813 {
                        jp804 = true
                    } else {
                        var t814 bool = jp800 > 57343
                        jp804 = t814
                    }
                    if jp804 {
                        var t805 string
                        var inline2360 string = "invalid low surrogate"
                        var inline2361 string = "" + inline2360
                        var inline2362 string = inline2361 + " at byte "
                        var inline2363 *ref_int_x = value__14.index
                        var inline2364 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2363)
                        var inline2365 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2364)
                        var inline2366 string = inline2362 + inline2365
                        t805 = inline2366
                        var t806 Result__unit__string = Result__unit__string_Err{
                            _0: t805,
                        }
                        return t806
                    } else {
                        var t807 uint32 = jp784 - 55296
                        var t808 uint32 = t807 * 1024
                        var t809 uint32 = 65536 + t808
                        var t810 uint32 = t809 + jp800
                        var t811 uint32 = t810 - 56320
                        var inline2368 Option__char = char_from_uint32(t811)
                        switch inline2368.(type) {
                        case Option__char_None:
                            var inline2369 string = _goml_m_std_p_json_p_json__error(value__14, "invalid unicode codepoint")
                            var inline2370 Result__unit__string = Result__unit__string_Err{
                                _0: inline2369,
                            }
                            return inline2370
                        case Option__char_Some:
                            var inline2371 rune = inline2368.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__15, inline2371)
                            var inline2374 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline2374
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var t815 Result__unit__string = Result__unit__string_Err{
                        _0: x18,
                    }
                    return t815
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t844 bool = jp784 >= 56320
            var jp840 bool
            if t844 {
                var t845 bool = jp784 <= 57343
                jp840 = t845
            } else {
                jp840 = false
            }
            if jp840 {
                var t841 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t842 Result__unit__string = Result__unit__string_Err{
                    _0: t841,
                }
                return t842
            } else {
                var t843 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, jp784)
                return t843
            }
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var t848 Result__unit__string = Result__unit__string_Err{
            _0: x14,
        }
        return t848
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t964 *ref_int_x = value__18.index
    var t965 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t964)
    var t966 string = value__18.input
    var t967 int = _goml_m_inherent_i_string_i_string_i_byte__len(t966)
    var t968 bool = t965 >= t967
    var jp956 bool
    if t968 {
        jp956 = true
    } else {
        var t969 string = value__18.input
        var t970 *ref_int_x = value__18.index
        var t971 int
        var inline2389 int = ref_get__Ref_3int(t970)
        t971 = inline2389
        var t972 uint8
        var inline2387 uint8 = _goml_runtime_core_string_byte_get(t969, t971)
        t972 = inline2387
        var t973 bool
        var inline2384 uint8 = 34
        var inline2385 bool = t972 == inline2384
        t973 = inline2385
        var t974 bool = !t973
        jp956 = t974
    }
    if jp956 {
        var t957 string
        var inline2391 string = "expected string"
        var inline2392 string = "" + inline2391
        var inline2393 string = inline2392 + " at byte "
        var inline2394 *ref_int_x = value__18.index
        var inline2395 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2394)
        var inline2396 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2395)
        var inline2397 string = inline2393 + inline2396
        t957 = inline2397
        var t958 Result__string__string = Result__string__string_Err{
            _0: t957,
        }
        return t958
    } else {
        var t959 *ref_int_x = value__18.index
        var t960 *ref_int_x = value__18.index
        var t961 int
        var inline2401 int = ref_get__Ref_3int(t960)
        t961 = inline2401
        var t962 int = t961 + 1
        ref_set__Ref_3int(t959, t962)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t852 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t852)
        Loop_loop856:
        for {
            var t857 *ref_int_x = value__18.index
            var t858 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t857)
            var t859 string = value__18.input
            var t860 int = _goml_m_inherent_i_string_i_string_i_byte__len(t859)
            var t861 bool = t858 < t860
            if t861 {
                var t862 string = value__18.input
                var t863 *ref_int_x = value__18.index
                var t864 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t863)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t862, t864)
                var t866 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 34)
                if t866 {
                    var t874 *ref_int_x = value__18.index
                    var t875 int
                    var inline2417 int = ref_get__Ref_3int(t874)
                    t875 = inline2417
                    var t876 bool = segment__20 < t875
                    if t876 {
                        var t877 string = value__18.input
                        var t878 *ref_int_x = value__18.index
                        var t879 int
                        var inline2405 int = ref_get__Ref_3int(t878)
                        t879 = inline2405
                        var t880 string
                        var inline2403 string = string_byte_slice(t877, segment__20, t879)
                        t880 = inline2403
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t880)
                    } else {}
                    var t868 *ref_int_x = value__18.index
                    var t869 *ref_int_x = value__18.index
                    var t870 int
                    var inline2415 int = ref_get__Ref_3int(t869)
                    t870 = inline2415
                    var t871 int = t870 + 1
                    ref_set__Ref_3int(t868, t871)
                    var t872 string
                    var inline2407 *_goml_vec_uint8 = builder__19.values
                    var inline2408 Tuple2_4bool_6string = string_from_utf8(inline2407)
                    var inline2410 string = inline2408._1
                    t872 = inline2410
                    var t873 Result__string__string = Result__string__string_Ok{
                        _0: t872,
                    }
                    return t873
                } else {
                    var t883 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 92)
                    if t883 {
                        var t938 *ref_int_x = value__18.index
                        var t939 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t938)
                        var t940 bool = segment__20 < t939
                        if t940 {
                            var t941 string = value__18.input
                            var t942 *ref_int_x = value__18.index
                            var t943 int
                            var inline2421 int = ref_get__Ref_3int(t942)
                            t943 = inline2421
                            var t944 string
                            var inline2419 string = string_byte_slice(t941, segment__20, t943)
                            t944 = inline2419
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t944)
                        } else {}
                        var t885 *ref_int_x = value__18.index
                        var t886 *ref_int_x = value__18.index
                        var t887 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t886)
                        var t888 int = t887 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t885, t888)
                        var t931 *ref_int_x = value__18.index
                        var t932 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t931)
                        var t933 string = value__18.input
                        var t934 int = _goml_m_inherent_i_string_i_string_i_byte__len(t933)
                        var t935 bool = t932 >= t934
                        if t935 {
                            var t936 string
                            var inline2423 string = "incomplete escape"
                            var inline2424 string = "" + inline2423
                            var inline2425 string = inline2424 + " at byte "
                            var inline2426 *ref_int_x = value__18.index
                            var inline2427 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2426)
                            var inline2428 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2427)
                            var inline2429 string = inline2425 + inline2428
                            t936 = inline2429
                            var t937 Result__string__string = Result__string__string_Err{
                                _0: t936,
                            }
                            return t937
                        } else {
                            var t890 string = value__18.input
                            var t891 *ref_int_x = value__18.index
                            var t892 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t891)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t890, t892)
                            var t893 *ref_int_x = value__18.index
                            var t894 *ref_int_x = value__18.index
                            var t895 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t894)
                            var t896 int = t895 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t893, t896)
                            var t900 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 34)
                            if t900 {
                                var inline2431 rune = 34
                                var inline2432 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2431)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, inline2432)
                                var t898 *ref_int_x = value__18.index
                                var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                segment__20 = t899
                                continue
                            } else {
                                var t903 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 92)
                                if t903 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t898 *ref_int_x = value__18.index
                                    var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                    segment__20 = t899
                                    continue
                                } else {
                                    var t906 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 47)
                                    if t906 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t898 *ref_int_x = value__18.index
                                        var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                        segment__20 = t899
                                        continue
                                    } else {
                                        var t909 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 98)
                                        if t909 {
                                            var mtmp26 Option__char = char_from_uint32(8)
                                            switch mtmp26.(type) {
                                            case Option__char_None:
                                                var t898 *ref_int_x = value__18.index
                                                var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                                segment__20 = t899
                                                continue
                                            case Option__char_Some:
                                                var x27 rune = mtmp26.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x27)
                                                var t898 *ref_int_x = value__18.index
                                                var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                                segment__20 = t899
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t913 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 102)
                                            if t913 {
                                                var mtmp28 Option__char = char_from_uint32(12)
                                                switch mtmp28.(type) {
                                                case Option__char_None:
                                                    var t898 *ref_int_x = value__18.index
                                                    var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                                    segment__20 = t899
                                                    continue
                                                case Option__char_Some:
                                                    var x29 rune = mtmp28.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x29)
                                                    var t898 *ref_int_x = value__18.index
                                                    var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                                    segment__20 = t899
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t917 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 110)
                                                if t917 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t898 *ref_int_x = value__18.index
                                                    var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                                    segment__20 = t899
                                                    continue
                                                } else {
                                                    var t920 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 114)
                                                    if t920 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t898 *ref_int_x = value__18.index
                                                        var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                                        segment__20 = t899
                                                        continue
                                                    } else {
                                                        var t923 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 116)
                                                        if t923 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t898 *ref_int_x = value__18.index
                                                            var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                                            segment__20 = t899
                                                            continue
                                                        } else {
                                                            var t926 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 117)
                                                            if t926 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t898 *ref_int_x = value__18.index
                                                                    var t899 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t898)
                                                                    segment__20 = t899
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var t928 Result__string__string = Result__string__string_Err{
                                                                        _0: x32,
                                                                    }
                                                                    return t928
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t929 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t930 Result__string__string = Result__string__string_Err{
                                                                    _0: t929,
                                                                }
                                                                return t930
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
                        var t947 bool = byte__21 < 32
                        if t947 {
                            var t948 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t949 Result__string__string = Result__string__string_Err{
                                _0: t948,
                            }
                            return t949
                        } else {
                            var t950 *ref_int_x = value__18.index
                            var t951 *ref_int_x = value__18.index
                            var t952 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t951)
                            var t953 int = t952 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t950, t953)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop856
            }
        }
        var t854 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t855 Result__string__string = Result__string__string_Err{
            _0: t854,
        }
        return t855
    }
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var t983 *ref_int_x = value__26.index
    var start__27 int
    var inline2452 int = ref_get__Ref_3int(t983)
    start__27 = inline2452
    Loop_loop988:
    for {
        var t996 *ref_int_x = value__26.index
        var t997 int
        var inline2448 int = ref_get__Ref_3int(t996)
        t997 = inline2448
        var t998 string = value__26.input
        var t999 int
        var inline2446 int = _goml_runtime_core_string_len(t998)
        t999 = inline2446
        var t1000 bool = t997 < t999
        var jp990 bool
        if t1000 {
            var t1001 string = value__26.input
            var t1002 *ref_int_x = value__26.index
            var t1003 int
            var inline2440 int = ref_get__Ref_3int(t1002)
            t1003 = inline2440
            var t1004 uint8
            var inline2438 uint8 = _goml_runtime_core_string_byte_get(t1001, t1003)
            t1004 = inline2438
            var inline2435 bool = t1004 >= 48
            if inline2435 {
                var inline2436 bool = t1004 <= 57
                jp990 = inline2436
            } else {
                jp990 = false
            }
        } else {
            jp990 = false
        }
        if jp990 {
            var t991 *ref_int_x = value__26.index
            var t992 *ref_int_x = value__26.index
            var t993 int
            var inline2444 int = ref_get__Ref_3int(t992)
            t993 = inline2444
            var t994 int = t993 + 1
            ref_set__Ref_3int(t991, t994)
            continue
        } else {
            break Loop_loop988
        }
    }
    var t985 *ref_int_x = value__26.index
    var t986 int
    var inline2450 int = ref_get__Ref_3int(t985)
    t986 = inline2450
    var t987 bool = t986 > start__27
    return t987
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1008 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1008)
    var t1130 string = value__28.input
    var t1131 *ref_int_x = value__28.index
    var t1132 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1131)
    var t1133 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1130, t1132)
    var t1134 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1133, 45)
    if t1134 {
        var t1135 *ref_int_x = value__28.index
        var t1136 *ref_int_x = value__28.index
        var t1137 int
        var inline2456 int = ref_get__Ref_3int(t1136)
        t1137 = inline2456
        var t1138 int = t1137 + 1
        ref_set__Ref_3int(t1135, t1138)
    } else {}
    var t1093 *ref_int_x = value__28.index
    var t1094 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1093)
    var t1095 string = value__28.input
    var t1096 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1095)
    var t1097 bool = t1094 >= t1096
    if t1097 {
        var t1098 string
        var inline2458 string = "incomplete number"
        var inline2459 string = "" + inline2458
        var inline2460 string = inline2459 + " at byte "
        var inline2461 *ref_int_x = value__28.index
        var inline2462 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2461)
        var inline2463 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2462)
        var inline2464 string = inline2460 + inline2463
        t1098 = inline2464
        var t1099 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1098,
        }
        return t1099
    } else {
        var t1101 string = value__28.input
        var t1102 *ref_int_x = value__28.index
        var t1103 int
        var inline2502 int = ref_get__Ref_3int(t1102)
        t1103 = inline2502
        var t1104 uint8
        var inline2500 uint8 = _goml_runtime_core_string_byte_get(t1101, t1103)
        t1104 = inline2500
        var t1105 bool
        var inline2497 uint8 = 48
        var inline2498 bool = t1104 == inline2497
        t1105 = inline2498
        if t1105 {
            var t1106 *ref_int_x = value__28.index
            var t1107 *ref_int_x = value__28.index
            var t1108 int
            var inline2487 int = ref_get__Ref_3int(t1107)
            t1108 = inline2487
            var t1109 int = t1108 + 1
            ref_set__Ref_3int(t1106, t1109)
            var t1115 *ref_int_x = value__28.index
            var t1116 int
            var inline2483 int = ref_get__Ref_3int(t1115)
            t1116 = inline2483
            var t1117 string = value__28.input
            var t1118 int
            var inline2481 int = _goml_runtime_core_string_len(t1117)
            t1118 = inline2481
            var t1119 bool = t1116 < t1118
            var jp1112 bool
            if t1119 {
                var t1120 string = value__28.input
                var t1121 *ref_int_x = value__28.index
                var t1122 int
                var inline2471 int = ref_get__Ref_3int(t1121)
                t1122 = inline2471
                var t1123 uint8
                var inline2469 uint8 = _goml_runtime_core_string_byte_get(t1120, t1122)
                t1123 = inline2469
                var inline2466 bool = t1123 >= 48
                if inline2466 {
                    var inline2467 bool = t1123 <= 57
                    jp1112 = inline2467
                } else {
                    jp1112 = false
                }
            } else {
                jp1112 = false
            }
            if jp1112 {
                var t1113 string
                var inline2473 string = "invalid leading zero"
                var inline2474 string = "" + inline2473
                var inline2475 string = inline2474 + " at byte "
                var inline2476 *ref_int_x = value__28.index
                var inline2477 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2476)
                var inline2478 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2477)
                var inline2479 string = inline2475 + inline2478
                t1113 = inline2479
                var t1114 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1113,
                }
                return t1114
            } else {
                var t1083 *ref_int_x = value__28.index
                var t1084 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1083)
                var t1085 string = value__28.input
                var t1086 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1085)
                var t1087 bool = t1084 < t1086
                var jp1073 bool
                if t1087 {
                    var t1088 string = value__28.input
                    var t1089 *ref_int_x = value__28.index
                    var t1090 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1089)
                    var t1091 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1088, t1090)
                    var t1092 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1091, 46)
                    jp1073 = t1092
                } else {
                    jp1073 = false
                }
                if jp1073 {
                    var t1074 *ref_int_x = value__28.index
                    var t1075 *ref_int_x = value__28.index
                    var t1076 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1075)
                    var t1077 int = t1076 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1074, t1077)
                    var t1079 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t1080 bool = !t1079
                    if t1080 {
                        var t1081 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t1082 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1081,
                        }
                        return t1082
                    } else {
                        var t1055 *ref_int_x = value__28.index
                        var t1056 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1055)
                        var t1057 string = value__28.input
                        var t1058 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1057)
                        var t1059 bool = t1056 < t1058
                        var jp1020 bool
                        if t1059 {
                            var t1062 string = value__28.input
                            var t1063 *ref_int_x = value__28.index
                            var t1064 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1063)
                            var t1065 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1062, t1064)
                            var t1066 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1065, 101)
                            if t1066 {
                                jp1020 = true
                            } else {
                                var t1067 string = value__28.input
                                var t1068 *ref_int_x = value__28.index
                                var t1069 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1068)
                                var t1070 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1067, t1069)
                                var t1071 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1070, 69)
                                jp1020 = t1071
                            }
                        } else {
                            jp1020 = false
                        }
                        if jp1020 {
                            var t1021 *ref_int_x = value__28.index
                            var t1022 *ref_int_x = value__28.index
                            var t1023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1022)
                            var t1024 int = t1023 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1021, t1024)
                            var t1038 *ref_int_x = value__28.index
                            var t1039 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1038)
                            var t1040 string = value__28.input
                            var t1041 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1040)
                            var t1042 bool = t1039 < t1041
                            var jp1032 bool
                            if t1042 {
                                var t1045 string = value__28.input
                                var t1046 *ref_int_x = value__28.index
                                var t1047 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1046)
                                var t1048 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1045, t1047)
                                var t1049 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1048, 43)
                                if t1049 {
                                    jp1032 = true
                                } else {
                                    var t1050 string = value__28.input
                                    var t1051 *ref_int_x = value__28.index
                                    var t1052 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1051)
                                    var t1053 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1050, t1052)
                                    var t1054 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1053, 45)
                                    jp1032 = t1054
                                }
                            } else {
                                jp1032 = false
                            }
                            if jp1032 {
                                var t1033 *ref_int_x = value__28.index
                                var t1034 *ref_int_x = value__28.index
                                var t1035 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1034)
                                var t1036 int = t1035 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1033, t1036)
                            } else {}
                            var t1027 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t1028 bool = !t1027
                            if t1028 {
                                var t1029 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t1030 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1029,
                                }
                                return t1030
                            } else {
                                var t1013 string = value__28.input
                                var t1014 *ref_int_x = value__28.index
                                var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                                var t1016 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1013, start__29, t1015)
                                var t1017 _goml_m_std_p_json_p_Value = Number{
                                    _0: t1016,
                                }
                                var t1018 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t1017,
                                }
                                return t1018
                            }
                        } else {
                            var t1013 string = value__28.input
                            var t1014 *ref_int_x = value__28.index
                            var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                            var t1016 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1013, start__29, t1015)
                            var t1017 _goml_m_std_p_json_p_Value = Number{
                                _0: t1016,
                            }
                            var t1018 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1017,
                            }
                            return t1018
                        }
                    }
                } else {
                    var t1055 *ref_int_x = value__28.index
                    var t1056 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1055)
                    var t1057 string = value__28.input
                    var t1058 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1057)
                    var t1059 bool = t1056 < t1058
                    var jp1020 bool
                    if t1059 {
                        var t1062 string = value__28.input
                        var t1063 *ref_int_x = value__28.index
                        var t1064 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1063)
                        var t1065 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1062, t1064)
                        var t1066 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1065, 101)
                        if t1066 {
                            jp1020 = true
                        } else {
                            var t1067 string = value__28.input
                            var t1068 *ref_int_x = value__28.index
                            var t1069 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1068)
                            var t1070 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1067, t1069)
                            var t1071 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1070, 69)
                            jp1020 = t1071
                        }
                    } else {
                        jp1020 = false
                    }
                    if jp1020 {
                        var t1021 *ref_int_x = value__28.index
                        var t1022 *ref_int_x = value__28.index
                        var t1023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1022)
                        var t1024 int = t1023 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1021, t1024)
                        var t1038 *ref_int_x = value__28.index
                        var t1039 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1038)
                        var t1040 string = value__28.input
                        var t1041 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1040)
                        var t1042 bool = t1039 < t1041
                        var jp1032 bool
                        if t1042 {
                            var t1045 string = value__28.input
                            var t1046 *ref_int_x = value__28.index
                            var t1047 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1046)
                            var t1048 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1045, t1047)
                            var t1049 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1048, 43)
                            if t1049 {
                                jp1032 = true
                            } else {
                                var t1050 string = value__28.input
                                var t1051 *ref_int_x = value__28.index
                                var t1052 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1051)
                                var t1053 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1050, t1052)
                                var t1054 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1053, 45)
                                jp1032 = t1054
                            }
                        } else {
                            jp1032 = false
                        }
                        if jp1032 {
                            var t1033 *ref_int_x = value__28.index
                            var t1034 *ref_int_x = value__28.index
                            var t1035 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1034)
                            var t1036 int = t1035 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1033, t1036)
                        } else {}
                        var t1027 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t1028 bool = !t1027
                        if t1028 {
                            var t1029 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t1030 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t1029,
                            }
                            return t1030
                        } else {
                            var t1013 string = value__28.input
                            var t1014 *ref_int_x = value__28.index
                            var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                            var t1016 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1013, start__29, t1015)
                            var t1017 _goml_m_std_p_json_p_Value = Number{
                                _0: t1016,
                            }
                            var t1018 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1017,
                            }
                            return t1018
                        }
                    } else {
                        var t1013 string = value__28.input
                        var t1014 *ref_int_x = value__28.index
                        var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                        var t1016 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1013, start__29, t1015)
                        var t1017 _goml_m_std_p_json_p_Value = Number{
                            _0: t1016,
                        }
                        var t1018 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t1017,
                        }
                        return t1018
                    }
                }
            }
        } else {
            var t1126 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t1127 bool = !t1126
            if t1127 {
                var t1128 string
                var inline2489 string = "expected number"
                var inline2490 string = "" + inline2489
                var inline2491 string = inline2490 + " at byte "
                var inline2492 *ref_int_x = value__28.index
                var inline2493 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2492)
                var inline2494 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2493)
                var inline2495 string = inline2491 + inline2494
                t1128 = inline2495
                var t1129 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1128,
                }
                return t1129
            } else {
                var t1083 *ref_int_x = value__28.index
                var t1084 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1083)
                var t1085 string = value__28.input
                var t1086 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1085)
                var t1087 bool = t1084 < t1086
                var jp1073 bool
                if t1087 {
                    var t1088 string = value__28.input
                    var t1089 *ref_int_x = value__28.index
                    var t1090 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1089)
                    var t1091 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1088, t1090)
                    var t1092 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1091, 46)
                    jp1073 = t1092
                } else {
                    jp1073 = false
                }
                if jp1073 {
                    var t1074 *ref_int_x = value__28.index
                    var t1075 *ref_int_x = value__28.index
                    var t1076 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1075)
                    var t1077 int = t1076 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1074, t1077)
                    var t1079 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t1080 bool = !t1079
                    if t1080 {
                        var t1081 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t1082 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1081,
                        }
                        return t1082
                    } else {
                        var t1055 *ref_int_x = value__28.index
                        var t1056 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1055)
                        var t1057 string = value__28.input
                        var t1058 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1057)
                        var t1059 bool = t1056 < t1058
                        var jp1020 bool
                        if t1059 {
                            var t1062 string = value__28.input
                            var t1063 *ref_int_x = value__28.index
                            var t1064 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1063)
                            var t1065 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1062, t1064)
                            var t1066 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1065, 101)
                            if t1066 {
                                jp1020 = true
                            } else {
                                var t1067 string = value__28.input
                                var t1068 *ref_int_x = value__28.index
                                var t1069 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1068)
                                var t1070 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1067, t1069)
                                var t1071 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1070, 69)
                                jp1020 = t1071
                            }
                        } else {
                            jp1020 = false
                        }
                        if jp1020 {
                            var t1021 *ref_int_x = value__28.index
                            var t1022 *ref_int_x = value__28.index
                            var t1023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1022)
                            var t1024 int = t1023 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1021, t1024)
                            var t1038 *ref_int_x = value__28.index
                            var t1039 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1038)
                            var t1040 string = value__28.input
                            var t1041 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1040)
                            var t1042 bool = t1039 < t1041
                            var jp1032 bool
                            if t1042 {
                                var t1045 string = value__28.input
                                var t1046 *ref_int_x = value__28.index
                                var t1047 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1046)
                                var t1048 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1045, t1047)
                                var t1049 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1048, 43)
                                if t1049 {
                                    jp1032 = true
                                } else {
                                    var t1050 string = value__28.input
                                    var t1051 *ref_int_x = value__28.index
                                    var t1052 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1051)
                                    var t1053 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1050, t1052)
                                    var t1054 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1053, 45)
                                    jp1032 = t1054
                                }
                            } else {
                                jp1032 = false
                            }
                            if jp1032 {
                                var t1033 *ref_int_x = value__28.index
                                var t1034 *ref_int_x = value__28.index
                                var t1035 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1034)
                                var t1036 int = t1035 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1033, t1036)
                            } else {}
                            var t1027 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t1028 bool = !t1027
                            if t1028 {
                                var t1029 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t1030 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1029,
                                }
                                return t1030
                            } else {
                                var t1013 string = value__28.input
                                var t1014 *ref_int_x = value__28.index
                                var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                                var t1016 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1013, start__29, t1015)
                                var t1017 _goml_m_std_p_json_p_Value = Number{
                                    _0: t1016,
                                }
                                var t1018 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t1017,
                                }
                                return t1018
                            }
                        } else {
                            var t1013 string = value__28.input
                            var t1014 *ref_int_x = value__28.index
                            var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                            var t1016 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1013, start__29, t1015)
                            var t1017 _goml_m_std_p_json_p_Value = Number{
                                _0: t1016,
                            }
                            var t1018 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1017,
                            }
                            return t1018
                        }
                    }
                } else {
                    var t1055 *ref_int_x = value__28.index
                    var t1056 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1055)
                    var t1057 string = value__28.input
                    var t1058 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1057)
                    var t1059 bool = t1056 < t1058
                    var jp1020 bool
                    if t1059 {
                        var t1062 string = value__28.input
                        var t1063 *ref_int_x = value__28.index
                        var t1064 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1063)
                        var t1065 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1062, t1064)
                        var t1066 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1065, 101)
                        if t1066 {
                            jp1020 = true
                        } else {
                            var t1067 string = value__28.input
                            var t1068 *ref_int_x = value__28.index
                            var t1069 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1068)
                            var t1070 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1067, t1069)
                            var t1071 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1070, 69)
                            jp1020 = t1071
                        }
                    } else {
                        jp1020 = false
                    }
                    if jp1020 {
                        var t1021 *ref_int_x = value__28.index
                        var t1022 *ref_int_x = value__28.index
                        var t1023 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1022)
                        var t1024 int = t1023 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1021, t1024)
                        var t1038 *ref_int_x = value__28.index
                        var t1039 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1038)
                        var t1040 string = value__28.input
                        var t1041 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1040)
                        var t1042 bool = t1039 < t1041
                        var jp1032 bool
                        if t1042 {
                            var t1045 string = value__28.input
                            var t1046 *ref_int_x = value__28.index
                            var t1047 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1046)
                            var t1048 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1045, t1047)
                            var t1049 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1048, 43)
                            if t1049 {
                                jp1032 = true
                            } else {
                                var t1050 string = value__28.input
                                var t1051 *ref_int_x = value__28.index
                                var t1052 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1051)
                                var t1053 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1050, t1052)
                                var t1054 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1053, 45)
                                jp1032 = t1054
                            }
                        } else {
                            jp1032 = false
                        }
                        if jp1032 {
                            var t1033 *ref_int_x = value__28.index
                            var t1034 *ref_int_x = value__28.index
                            var t1035 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1034)
                            var t1036 int = t1035 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1033, t1036)
                        } else {}
                        var t1027 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t1028 bool = !t1027
                        if t1028 {
                            var t1029 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t1030 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t1029,
                            }
                            return t1030
                        } else {
                            var t1013 string = value__28.input
                            var t1014 *ref_int_x = value__28.index
                            var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                            var t1016 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1013, start__29, t1015)
                            var t1017 _goml_m_std_p_json_p_Value = Number{
                                _0: t1016,
                            }
                            var t1018 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1017,
                            }
                            return t1018
                        }
                    } else {
                        var t1013 string = value__28.input
                        var t1014 *ref_int_x = value__28.index
                        var t1015 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1014)
                        var t1016 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1013, start__29, t1015)
                        var t1017 _goml_m_std_p_json_p_Value = Number{
                            _0: t1016,
                        }
                        var t1018 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t1017,
                        }
                        return t1018
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t1154 *ref_int_x = value__30.index
    var t1155 int
    var inline2532 int = ref_get__Ref_3int(t1154)
    t1155 = inline2532
    var t1156 int
    var inline2530 int = _goml_runtime_core_string_len(expected__31)
    t1156 = inline2530
    var t1157 int = t1155 + t1156
    var t1158 string = value__30.input
    var t1159 int
    var inline2528 int = _goml_runtime_core_string_len(t1158)
    t1159 = inline2528
    var t1160 bool = t1157 <= t1159
    var jp1145 bool
    if t1160 {
        var t1161 string = value__30.input
        var t1162 *ref_int_x = value__30.index
        var t1163 int
        var inline2512 int = ref_get__Ref_3int(t1162)
        t1163 = inline2512
        var t1164 *ref_int_x = value__30.index
        var t1165 int
        var inline2510 int = ref_get__Ref_3int(t1164)
        t1165 = inline2510
        var t1166 int
        var inline2508 int = _goml_runtime_core_string_len(expected__31)
        t1166 = inline2508
        var t1167 int = t1165 + t1166
        var t1168 string
        var inline2506 string = string_byte_slice(t1161, t1163, t1167)
        t1168 = inline2506
        var inline2504 bool = t1168 == expected__31
        jp1145 = inline2504
    } else {
        jp1145 = false
    }
    if jp1145 {
        var t1146 *ref_int_x = value__30.index
        var t1147 *ref_int_x = value__30.index
        var t1148 int
        var inline2518 int = ref_get__Ref_3int(t1147)
        t1148 = inline2518
        var t1149 int
        var inline2516 int = _goml_runtime_core_string_len(expected__31)
        t1149 = inline2516
        var t1150 int = t1148 + t1149
        ref_set__Ref_3int(t1146, t1150)
        var t1151 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        return t1151
    } else {
        var t1152 string
        var inline2520 string = "invalid literal"
        var inline2521 string = "" + inline2520
        var inline2522 string = inline2521 + " at byte "
        var inline2523 *ref_int_x = value__30.index
        var inline2524 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2523)
        var inline2525 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2524)
        var inline2526 string = inline2522 + inline2525
        t1152 = inline2526
        var t1153 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1152,
        }
        return t1153
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1172 *ref_int_x = value__33.index
    var t1173 *ref_int_x = value__33.index
    var t1174 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1173)
    var t1175 int = t1174 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1172, t1175)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var vec_literal__8961 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1230 *ref_int_x = value__33.index
    var t1231 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1230)
    var t1232 string = value__33.input
    var t1233 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1232)
    var t1234 bool = t1231 < t1233
    var jp1223 bool
    if t1234 {
        var t1235 string = value__33.input
        var t1236 *ref_int_x = value__33.index
        var t1237 int
        var inline2539 int = ref_get__Ref_3int(t1236)
        t1237 = inline2539
        var t1238 uint8
        var inline2537 uint8 = _goml_runtime_core_string_byte_get(t1235, t1237)
        t1238 = inline2537
        var inline2534 uint8 = 93
        var inline2535 bool = t1238 == inline2534
        jp1223 = inline2535
    } else {
        jp1223 = false
    }
    if jp1223 {
        var t1224 *ref_int_x = value__33.index
        var t1225 *ref_int_x = value__33.index
        var t1226 int
        var inline2543 int = ref_get__Ref_3int(t1225)
        t1226 = inline2543
        var t1227 int = t1226 + 1
        ref_set__Ref_3int(t1224, t1227)
        var t1228 _goml_m_std_p_json_p_Value = Array{
            _0: vec_literal__8961,
        }
        var t1229 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1228,
        }
        return t1229
    } else {
        Loop_loop1180:
        for {
            var t1181 *ref_int_x = value__33.index
            var t1182 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1181)
            var t1183 string = value__33.input
            var t1184 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1183)
            var t1185 bool = t1182 < t1184
            if t1185 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1187 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp1187 = x51
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8961, jp1187)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1189 *ref_int_x = value__33.index
                    var t1190 int
                    var inline2585 int = ref_get__Ref_3int(t1189)
                    t1190 = inline2585
                    var t1191 string = value__33.input
                    var t1192 int
                    var inline2583 int = _goml_runtime_core_string_len(t1191)
                    t1192 = inline2583
                    var t1193 bool = t1190 >= t1192
                    if t1193 {
                        var t1194 string
                        var inline2545 string = "unterminated array"
                        var inline2546 string = "" + inline2545
                        var inline2547 string = inline2546 + " at byte "
                        var inline2548 *ref_int_x = value__33.index
                        var inline2549 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2548)
                        var inline2550 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2549)
                        var inline2551 string = inline2547 + inline2550
                        t1194 = inline2551
                        var t1195 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1194,
                        }
                        return t1195
                    } else {
                        var t1197 string = value__33.input
                        var t1198 *ref_int_x = value__33.index
                        var t1199 int
                        var inline2581 int = ref_get__Ref_3int(t1198)
                        t1199 = inline2581
                        var t1200 uint8
                        var inline2579 uint8 = _goml_runtime_core_string_byte_get(t1197, t1199)
                        t1200 = inline2579
                        var t1201 bool
                        var inline2576 uint8 = 93
                        var inline2577 bool = t1200 == inline2576
                        t1201 = inline2577
                        if t1201 {
                            var t1202 *ref_int_x = value__33.index
                            var t1203 *ref_int_x = value__33.index
                            var t1204 int
                            var inline2555 int = ref_get__Ref_3int(t1203)
                            t1204 = inline2555
                            var t1205 int = t1204 + 1
                            ref_set__Ref_3int(t1202, t1205)
                            var t1206 _goml_m_std_p_json_p_Value = Array{
                                _0: vec_literal__8961,
                            }
                            var t1207 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1206,
                            }
                            return t1207
                        } else {
                            var t1209 string = value__33.input
                            var t1210 *ref_int_x = value__33.index
                            var t1211 int
                            var inline2574 int = ref_get__Ref_3int(t1210)
                            t1211 = inline2574
                            var t1212 uint8
                            var inline2572 uint8 = _goml_runtime_core_string_byte_get(t1209, t1211)
                            t1212 = inline2572
                            var t1213 bool
                            var inline2569 uint8 = 44
                            var inline2570 bool = t1212 == inline2569
                            t1213 = inline2570
                            if t1213 {
                                var t1214 *ref_int_x = value__33.index
                                var t1215 *ref_int_x = value__33.index
                                var t1216 int
                                var inline2559 int = ref_get__Ref_3int(t1215)
                                t1216 = inline2559
                                var t1217 int = t1216 + 1
                                ref_set__Ref_3int(t1214, t1217)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1219 string
                                var inline2561 string = "expected array separator"
                                var inline2562 string = "" + inline2561
                                var inline2563 string = inline2562 + " at byte "
                                var inline2564 *ref_int_x = value__33.index
                                var inline2565 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2564)
                                var inline2566 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2565)
                                var inline2567 string = inline2563 + inline2566
                                t1219 = inline2567
                                var t1220 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1219,
                                }
                                return t1220
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t1221 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x52,
                    }
                    return t1221
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1180
            }
        }
        var t1178 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1179 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1178,
        }
        return t1179
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1242 *ref_int_x = value__36.index
    var t1243 *ref_int_x = value__36.index
    var t1244 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1243)
    var t1245 int = t1244 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1242, t1245)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var vec_literal__10180 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1325 *ref_int_x = value__36.index
    var t1326 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1325)
    var t1327 string = value__36.input
    var t1328 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1327)
    var t1329 bool = t1326 < t1328
    var jp1318 bool
    if t1329 {
        var t1330 string = value__36.input
        var t1331 *ref_int_x = value__36.index
        var t1332 int
        var inline2592 int = ref_get__Ref_3int(t1331)
        t1332 = inline2592
        var t1333 uint8
        var inline2590 uint8 = _goml_runtime_core_string_byte_get(t1330, t1332)
        t1333 = inline2590
        var inline2587 uint8 = 125
        var inline2588 bool = t1333 == inline2587
        jp1318 = inline2588
    } else {
        jp1318 = false
    }
    if jp1318 {
        var t1319 *ref_int_x = value__36.index
        var t1320 *ref_int_x = value__36.index
        var t1321 int
        var inline2596 int = ref_get__Ref_3int(t1320)
        t1321 = inline2596
        var t1322 int = t1321 + 1
        ref_set__Ref_3int(t1319, t1322)
        var t1323 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10180,
        }
        var t1324 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1323,
        }
        return t1324
    } else {
        Loop_loop1250:
        for {
            var t1251 *ref_int_x = value__36.index
            var t1252 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1251)
            var t1253 string = value__36.input
            var t1254 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1253)
            var t1255 bool = t1252 < t1254
            if t1255 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1257 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    jp1257 = x63
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1305 *ref_int_x = value__36.index
                    var t1306 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1305)
                    var t1307 string = value__36.input
                    var t1308 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1307)
                    var t1309 bool = t1306 >= t1308
                    var jp1297 bool
                    if t1309 {
                        jp1297 = true
                    } else {
                        var t1310 string = value__36.input
                        var t1311 *ref_int_x = value__36.index
                        var t1312 int
                        var inline2603 int = ref_get__Ref_3int(t1311)
                        t1312 = inline2603
                        var t1313 uint8
                        var inline2601 uint8 = _goml_runtime_core_string_byte_get(t1310, t1312)
                        t1313 = inline2601
                        var t1314 bool
                        var inline2598 uint8 = 58
                        var inline2599 bool = t1313 == inline2598
                        t1314 = inline2599
                        var t1315 bool = !t1314
                        jp1297 = t1315
                    }
                    if jp1297 {
                        var t1298 string
                        var inline2605 string = "expected object colon"
                        var inline2606 string = "" + inline2605
                        var inline2607 string = inline2606 + " at byte "
                        var inline2608 *ref_int_x = value__36.index
                        var inline2609 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2608)
                        var inline2610 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2609)
                        var inline2611 string = inline2607 + inline2610
                        t1298 = inline2611
                        var t1299 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1298,
                        }
                        return t1299
                    } else {
                        var t1300 *ref_int_x = value__36.index
                        var t1301 *ref_int_x = value__36.index
                        var t1302 int
                        var inline2615 int = ref_get__Ref_3int(t1301)
                        t1302 = inline2615
                        var t1303 int = t1302 + 1
                        ref_set__Ref_3int(t1300, t1303)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1260 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp1260 = x69
                            var t1261 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp1257,
                                _1: jp1260,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10180, t1261)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1263 *ref_int_x = value__36.index
                            var t1264 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1263)
                            var t1265 string = value__36.input
                            var t1266 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1265)
                            var t1267 bool = t1264 >= t1266
                            if t1267 {
                                var t1268 string
                                var inline2617 string = "unterminated object"
                                var inline2618 string = "" + inline2617
                                var inline2619 string = inline2618 + " at byte "
                                var inline2620 *ref_int_x = value__36.index
                                var inline2621 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2620)
                                var inline2622 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2621)
                                var inline2623 string = inline2619 + inline2622
                                t1268 = inline2623
                                var t1269 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1268,
                                }
                                return t1269
                            } else {
                                var t1271 string = value__36.input
                                var t1272 *ref_int_x = value__36.index
                                var t1273 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1272)
                                var t1274 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1271, t1273)
                                var t1275 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1274, 125)
                                if t1275 {
                                    var t1276 *ref_int_x = value__36.index
                                    var t1277 *ref_int_x = value__36.index
                                    var t1278 int
                                    var inline2627 int = ref_get__Ref_3int(t1277)
                                    t1278 = inline2627
                                    var t1279 int = t1278 + 1
                                    ref_set__Ref_3int(t1276, t1279)
                                    var t1280 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10180,
                                    }
                                    var t1281 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1280,
                                    }
                                    return t1281
                                } else {
                                    var t1283 string = value__36.input
                                    var t1284 *ref_int_x = value__36.index
                                    var t1285 int
                                    var inline2638 int = ref_get__Ref_3int(t1284)
                                    t1285 = inline2638
                                    var t1286 uint8
                                    var inline2636 uint8 = _goml_runtime_core_string_byte_get(t1283, t1285)
                                    t1286 = inline2636
                                    var t1287 bool
                                    var inline2633 uint8 = 44
                                    var inline2634 bool = t1286 == inline2633
                                    t1287 = inline2634
                                    if t1287 {
                                        var t1288 *ref_int_x = value__36.index
                                        var t1289 *ref_int_x = value__36.index
                                        var t1290 int
                                        var inline2631 int = ref_get__Ref_3int(t1289)
                                        t1290 = inline2631
                                        var t1291 int = t1290 + 1
                                        ref_set__Ref_3int(t1288, t1291)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1293 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1294 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1293,
                                        }
                                        return t1294
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t1295 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x70,
                            }
                            return t1295
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var t1316 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x64,
                    }
                    return t1316
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1250
            }
        }
        var t1248 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1249 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1248,
        }
        return t1249
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1339 *ref_int_x = value__40.index
    var t1340 int
    var inline2668 int = ref_get__Ref_3int(t1339)
    t1340 = inline2668
    var t1341 string = value__40.input
    var t1342 int
    var inline2666 int = _goml_runtime_core_string_len(t1341)
    t1342 = inline2666
    var t1343 bool = t1340 >= t1342
    if t1343 {
        var t1344 string
        var inline2640 string = "expected JSON value"
        var inline2641 string = "" + inline2640
        var inline2642 string = inline2641 + " at byte "
        var inline2643 *ref_int_x = value__40.index
        var inline2644 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2643)
        var inline2645 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2644)
        var inline2646 string = inline2642 + inline2645
        t1344 = inline2646
        var t1345 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1344,
        }
        return t1345
    } else {
        var t1346 string = value__40.input
        var t1347 *ref_int_x = value__40.index
        var t1348 int
        var inline2664 int = ref_get__Ref_3int(t1347)
        t1348 = inline2664
        var mtmp77 uint8
        var inline2662 uint8 = _goml_runtime_core_string_byte_get(t1346, t1348)
        mtmp77 = inline2662
        switch mtmp77 {
        case 123:
            var t1351 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            return t1351
        case 91:
            var t1352 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            return t1352
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var t1355 _goml_m_std_p_json_p_Value = String{
                    _0: x79,
                }
                var t1356 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1355,
                }
                return t1356
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var t1357 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x80,
                }
                return t1357
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t1358 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t1359 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1358)
            return t1359
        case 102:
            var t1360 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t1361 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1360)
            return t1361
        case 110:
            var t1362 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            return t1362
        default:
            var t1370 bool
            var inline2659 uint8 = 45
            var inline2660 bool = mtmp77 == inline2659
            t1370 = inline2660
            var jp1366 bool
            if t1370 {
                jp1366 = true
            } else {
                var inline2648 bool = mtmp77 >= 48
                if inline2648 {
                    var inline2649 bool = mtmp77 <= 57
                    jp1366 = inline2649
                } else {
                    jp1366 = false
                }
            }
            if jp1366 {
                var t1367 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                return t1367
            } else {
                var t1368 string
                var inline2651 string = "unexpected JSON token"
                var inline2652 string = "" + inline2651
                var inline2653 string = inline2652 + " at byte "
                var inline2654 *ref_int_x = value__40.index
                var inline2655 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2654)
                var inline2656 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2655)
                var inline2657 string = inline2653 + inline2656
                t1368 = inline2657
                var t1369 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1368,
                }
                return t1369
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__45 _goml_m_std_p_json_p_JsonParser
    var inline2684 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline2685 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__44,
        index: inline2684,
    }
    parser__45 = inline2685
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1375 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1375 = x82
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1378 *ref_int_x = parser__45.index
        var t1379 int
        var inline2682 int = ref_get__Ref_3int(t1378)
        t1379 = inline2682
        var t1380 int
        var inline2680 int = _goml_runtime_core_string_len(input__44)
        t1380 = inline2680
        var t1381 bool
        var inline2678 bool = t1379 == t1380
        t1381 = inline2678
        if t1381 {
            var t1382 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp1375,
            }
            return t1382
        } else {
            var t1383 string
            var inline2670 string = "trailing JSON data"
            var inline2671 string = "" + inline2670
            var inline2672 string = inline2671 + " at byte "
            var inline2673 *ref_int_x = parser__45.index
            var inline2674 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2673)
            var inline2675 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2674)
            var inline2676 string = inline2672 + inline2675
            t1383 = inline2676
            var t1384 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1383,
            }
            return t1384
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t1385 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x83,
        }
        return t1385
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1399:
    for {
        var t1400 bool = for_index86 < for_limit87
        if t1400 {
            var for_item88 int = for_index86
            var t1401 int = for_index86 + 1
            for_index86 = t1401
            var byte__52 uint8
            var inline2746 uint8 = _goml_runtime_core_string_byte_get(value__49, for_item88)
            byte__52 = inline2746
            var t1454 bool
            var inline2743 uint8 = 34
            var inline2744 bool = byte__52 == inline2743
            t1454 = inline2744
            var jp1452 bool
            if t1454 {
                jp1452 = true
            } else {
                var inline2690 uint8 = 92
                var inline2691 bool = byte__52 == inline2690
                jp1452 = inline2691
            }
            var jp1449 bool
            if jp1452 {
                jp1449 = true
            } else {
                var inline2693 uint8 = 8
                var inline2694 bool = byte__52 == inline2693
                jp1449 = inline2694
            }
            var jp1446 bool
            if jp1449 {
                jp1446 = true
            } else {
                var inline2696 uint8 = 9
                var inline2697 bool = byte__52 == inline2696
                jp1446 = inline2697
            }
            var jp1443 bool
            if jp1446 {
                jp1443 = true
            } else {
                var inline2699 uint8 = 10
                var inline2700 bool = byte__52 == inline2699
                jp1443 = inline2700
            }
            var jp1440 bool
            if jp1443 {
                jp1440 = true
            } else {
                var inline2702 uint8 = 12
                var inline2703 bool = byte__52 == inline2702
                jp1440 = inline2703
            }
            var jp1437 bool
            if jp1440 {
                jp1437 = true
            } else {
                var inline2705 uint8 = 13
                var inline2706 bool = byte__52 == inline2705
                jp1437 = inline2706
            }
            var jp1404 bool
            if jp1437 {
                jp1404 = true
            } else {
                var t1438 bool = byte__52 < 32
                jp1404 = t1438
            }
            if jp1404 {
                var t1433 bool = start__50 < for_item88
                if t1433 {
                    var t1434 string
                    var inline2708 string = string_byte_slice(value__49, start__50, for_item88)
                    t1434 = inline2708
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1434)
                } else {}
                var t1408 bool
                var inline2740 uint8 = 34
                var inline2741 bool = byte__52 == inline2740
                t1408 = inline2741
                if t1408 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1411 bool
                    var inline2737 uint8 = 92
                    var inline2738 bool = byte__52 == inline2737
                    t1411 = inline2738
                    if t1411 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1414 bool
                        var inline2734 uint8 = 8
                        var inline2735 bool = byte__52 == inline2734
                        t1414 = inline2735
                        if t1414 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1417 bool
                            var inline2731 uint8 = 9
                            var inline2732 bool = byte__52 == inline2731
                            t1417 = inline2732
                            if t1417 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1420 bool
                                var inline2728 uint8 = 10
                                var inline2729 bool = byte__52 == inline2728
                                t1420 = inline2729
                                if t1420 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1423 bool
                                    var inline2725 uint8 = 12
                                    var inline2726 bool = byte__52 == inline2725
                                    t1423 = inline2726
                                    if t1423 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1426 bool
                                        var inline2722 uint8 = 13
                                        var inline2723 bool = byte__52 == inline2722
                                        t1426 = inline2723
                                        if t1426 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1428 uint8 = byte__52 / 16
                                            var t1429 rune
                                            var inline2719 int = int(uint8(t1428))
                                            var inline2720 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline2719)
                                            t1429 = inline2720
                                            var inline2716 string = _goml_m_inherent_i_char_i_char_i_to__string(t1429)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline2716)
                                            var t1430_rhs uint8 = 16
                                            var t1430 uint8 = byte__52 % t1430_rhs
                                            var t1431 rune
                                            var inline2713 int = int(uint8(t1430))
                                            var inline2714 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline2713)
                                            t1431 = inline2714
                                            var inline2710 string = _goml_m_inherent_i_char_i_char_i_to__string(t1431)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline2710)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1407 int = for_item88 + 1
                start__50 = t1407
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop1399
        }
    }
    var t1394 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1395 bool = start__50 < t1394
    if t1395 {
        var t1396 int
        var inline2750 int = _goml_runtime_core_string_len(value__49)
        t1396 = inline2750
        var t1397 string
        var inline2748 string = string_byte_slice(value__49, start__50, t1396)
        t1397 = inline2748
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1397)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__53 _goml_m_std_p_text_p_StringBuilder, value__54 _goml_m_std_p_json_p_Value) struct{} {
    switch value__54.(type) {
    case Object:
        var x97 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__54.(Object)._0
        var inline2764 rune = 123
        var inline2765 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2764)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2765)
        var index__56 int = 0
        var for_limit104 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97)
        var for_index105 int = 0
        Loop_loop1460:
        for {
            var t1461 bool = for_index105 < for_limit104
            if t1461 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97, for_index105)
                var t1462 int = for_index105 + 1
                for_index105 = t1462
                var t1468 bool = index__56 > 0
                if t1468 {
                    var inline2752 rune = 44
                    var inline2753 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2752)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2753)
                } else {}
                var t1464 string = for_item106._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1464)
                var inline2756 rune = 58
                var inline2757 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2756)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2757)
                var t1465 _goml_m_std_p_json_p_Value = for_item106._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1465)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1466 int = compound_old112 + compound_value113
                index__56 = t1466
                continue
            } else {
                break Loop_loop1460
            }
        }
        var inline2760 rune = 125
        var inline2761 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2760)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2761)
        return struct{}{}
    case Array:
        var x98 *_goml_vec__goml_m_std_p_json_p_Value = value__54.(Array)._0
        var inline2776 rune = 91
        var inline2777 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2776)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2777)
        var index__59 int = 0
        var for_limit118 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x98)
        var for_index119 int = 0
        Loop_loop1472:
        for {
            var t1473 bool = for_index119 < for_limit118
            if t1473 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x98, for_index119)
                var t1474 int = for_index119 + 1
                for_index119 = t1474
                var t1478 bool = index__59 > 0
                if t1478 {
                    var inline2768 rune = 44
                    var inline2769 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2768)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2769)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, for_item120)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1476 int = compound_old124 + compound_value125
                index__59 = t1476
                continue
            } else {
                break Loop_loop1472
            }
        }
        var inline2772 rune = 93
        var inline2773 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2772)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2773)
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
        var jp1483 string
        if x101 {
            jp1483 = "true"
        } else {
            jp1483 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1483)
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
    var inline2786 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline2787 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline2786,
    }
    builder__65 = inline2787
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var inline2780 *_goml_vec_uint8 = builder__65.values
    var inline2781 Tuple2_4bool_6string = string_from_utf8(inline2780)
    var inline2783 string = inline2781._1
    return inline2783
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129)
        var for_index136 int = 0
        Loop_loop1494:
        for {
            var t1495 bool = for_index136 < for_limit135
            if t1495 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129, for_index136)
                var t1496 int = for_index136 + 1
                for_index136 = t1496
                var t1498 string = for_item137._0
                var t1499 bool
                var inline2789 bool = t1498 == name__67
                t1499 = inline2789
                if t1499 {
                    var t1500 _goml_m_std_p_json_p_Value = for_item137._1
                    var t1501 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1500,
                    }
                    return t1501
                } else {
                    continue
                }
            } else {
                break Loop_loop1494
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var t1511 int
    var inline2808 int = _goml_runtime_core_string_len(value__72)
    t1511 = inline2808
    var t1512 bool
    var inline2805 int = 0
    var inline2806 bool = t1511 == inline2805
    t1512 = inline2806
    if t1512 {
        return Option__int_None{}
    } else {
        var t1513 uint8
        var inline2802 int = 0
        var inline2803 uint8 = _goml_runtime_core_string_byte_get(value__72, inline2802)
        t1513 = inline2803
        var negative__73 bool
        var inline2799 uint8 = 45
        var inline2800 bool = t1513 == inline2799
        negative__73 = inline2800
        var jp1515 int
        if negative__73 {
            jp1515 = 1
        } else {
            jp1515 = 0
        }
        var index__74 int = jp1515
        var result__75 int = 0
        var t1536 int
        var inline2797 int = _goml_runtime_core_string_len(value__72)
        t1536 = inline2797
        var t1537 bool
        var inline2795 bool = index__74 == t1536
        t1537 = inline2795
        if t1537 {
            return Option__int_None{}
        } else {
            Loop_loop1522:
            for {
                var t1523 int
                var inline2793 int = _goml_runtime_core_string_len(value__72)
                t1523 = inline2793
                var t1524 bool = index__74 < t1523
                if t1524 {
                    var byte__76 uint8
                    var inline2791 uint8 = _goml_runtime_core_string_byte_get(value__72, index__74)
                    byte__76 = inline2791
                    var t1534 bool = byte__76 < 48
                    var jp1529 bool
                    if t1534 {
                        jp1529 = true
                    } else {
                        var t1535 bool = byte__76 > 57
                        jp1529 = t1535
                    }
                    if jp1529 {
                        return Option__int_None{}
                    } else {
                        var t1530 int = result__75 * 10
                        var t1531 uint8 = byte__76 - 48
                        var t1532 int = int(uint8(t1531))
                        var t1533 int = t1530 + t1532
                        result__75 = t1533
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1526 int = compound_old148 + compound_value149
                        index__74 = t1526
                        continue
                    }
                } else {
                    break Loop_loop1522
                }
            }
            var jp1519 int
            if negative__73 {
                var t1521 int = 0 - result__75
                jp1519 = t1521
            } else {
                jp1519 = result__75
            }
            var t1520 Option__int = Option__int_Some{
                _0: jp1519,
            }
            return t1520
        }
    }
}

func main0() struct{} {
    var mtmp177 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1558 _goml_m_std_p_json_p_Value
    switch mtmp177.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x178 _goml_m_std_p_json_p_Value = mtmp177.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1558 = x178
        var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1558, "name")
        switch mtmp181.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline2813 string = "missing name"
            var inline2814 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2813)
            _goml_runtime_core_string_println(inline2814)
            var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1558, "version")
            switch mtmp186.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline2828 string = "missing version"
                var inline2829 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2828)
                _goml_runtime_core_string_println(inline2829)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp188 Option__int
                switch x187.(type) {
                case Number:
                    var inline2839 string = x187.(Number)._0
                    var inline2841 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2839)
                    mtmp188 = inline2841
                default:
                    mtmp188 = Option__int_None{}
                }
                switch mtmp188.(type) {
                case Option__int_None:
                    var inline2832 string = "invalid version"
                    var inline2833 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2832)
                    _goml_runtime_core_string_println(inline2833)
                case Option__int_Some:
                    var x189 int = mtmp188.(Option__int_Some)._0
                    var inline2836 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x189)
                    _goml_runtime_core_string_println(inline2836)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1558, "stable")
            switch mtmp191.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline2843 string = "missing stable"
                var inline2844 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2843)
                _goml_runtime_core_string_println(inline2844)
                var t1562 string = _goml_m_std_p_json_p_encode(jp1558)
                println__T_string(t1562)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field3003 bool
                switch x192.(type) {
                case Bool:
                    var inline2854 bool = x192.(Bool)._0
                    commute_field3003 = inline2854
                    var inline2851 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3003)
                    _goml_runtime_core_string_println(inline2851)
                    var t1562 string = _goml_m_std_p_json_p_encode(jp1558)
                    println__T_string(t1562)
                    return struct{}{}
                default:
                    var inline2847 string = "invalid stable"
                    var inline2848 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2847)
                    _goml_runtime_core_string_println(inline2848)
                    var t1562 string = _goml_m_std_p_json_p_encode(jp1558)
                    println__T_string(t1562)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field3009 string
            switch x182.(type) {
            case String:
                var inline2824 string = x182.(String)._0
                commute_field3009 = inline2824
                var inline2821 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field3009)
                _goml_runtime_core_string_println(inline2821)
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1558, "version")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2828 string = "missing version"
                    var inline2829 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2828)
                    _goml_runtime_core_string_println(inline2829)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp188 Option__int
                    switch x187.(type) {
                    case Number:
                        var inline2839 string = x187.(Number)._0
                        var inline2841 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2839)
                        mtmp188 = inline2841
                    default:
                        mtmp188 = Option__int_None{}
                    }
                    switch mtmp188.(type) {
                    case Option__int_None:
                        var inline2832 string = "invalid version"
                        var inline2833 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2832)
                        _goml_runtime_core_string_println(inline2833)
                    case Option__int_Some:
                        var x189 int = mtmp188.(Option__int_Some)._0
                        var inline2836 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x189)
                        _goml_runtime_core_string_println(inline2836)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1558, "stable")
                switch mtmp191.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2843 string = "missing stable"
                    var inline2844 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2843)
                    _goml_runtime_core_string_println(inline2844)
                    var t1562 string = _goml_m_std_p_json_p_encode(jp1558)
                    println__T_string(t1562)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field3003 bool
                    switch x192.(type) {
                    case Bool:
                        var inline2854 bool = x192.(Bool)._0
                        commute_field3003 = inline2854
                        var inline2851 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3003)
                        _goml_runtime_core_string_println(inline2851)
                        var t1562 string = _goml_m_std_p_json_p_encode(jp1558)
                        println__T_string(t1562)
                        return struct{}{}
                    default:
                        var inline2847 string = "invalid stable"
                        var inline2848 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2847)
                        _goml_runtime_core_string_println(inline2848)
                        var t1562 string = _goml_m_std_p_json_p_encode(jp1558)
                        println__T_string(t1562)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline2817 string = "invalid name"
                var inline2818 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2817)
                _goml_runtime_core_string_println(inline2818)
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1558, "version")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2828 string = "missing version"
                    var inline2829 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2828)
                    _goml_runtime_core_string_println(inline2829)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp188 Option__int
                    switch x187.(type) {
                    case Number:
                        var inline2839 string = x187.(Number)._0
                        var inline2841 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2839)
                        mtmp188 = inline2841
                    default:
                        mtmp188 = Option__int_None{}
                    }
                    switch mtmp188.(type) {
                    case Option__int_None:
                        var inline2832 string = "invalid version"
                        var inline2833 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2832)
                        _goml_runtime_core_string_println(inline2833)
                    case Option__int_Some:
                        var x189 int = mtmp188.(Option__int_Some)._0
                        var inline2836 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x189)
                        _goml_runtime_core_string_println(inline2836)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp191 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1558, "stable")
                switch mtmp191.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2843 string = "missing stable"
                    var inline2844 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2843)
                    _goml_runtime_core_string_println(inline2844)
                    var t1562 string = _goml_m_std_p_json_p_encode(jp1558)
                    println__T_string(t1562)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x192 _goml_m_std_p_json_p_Value = mtmp191.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field3003 bool
                    switch x192.(type) {
                    case Bool:
                        var inline2854 bool = x192.(Bool)._0
                        commute_field3003 = inline2854
                        var inline2851 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3003)
                        _goml_runtime_core_string_println(inline2851)
                        var t1562 string = _goml_m_std_p_json_p_encode(jp1558)
                        println__T_string(t1562)
                        return struct{}{}
                    default:
                        var inline2847 string = "invalid stable"
                        var inline2848 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2847)
                        _goml_runtime_core_string_println(inline2848)
                        var t1562 string = _goml_m_std_p_json_p_encode(jp1558)
                        println__T_string(t1562)
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
        var x179 string = mtmp177.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var inline2810 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x179)
        _goml_runtime_core_string_println(inline2810)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__98 uint8, other__99 uint8) bool {
    var t1578 bool = self__98 == other__99
    return t1578
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline2858 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline2859 bool = inline2858._0
    var inline2860 rune = inline2858._1
    if inline2859 {
        return inline2860
    } else {
        var inline2864 rune = _goml_runtime_core_string_get("", -1)
        return inline2864
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t1584 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t1584
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1591:
    for {
        var t1592 int
        var inline2866 int = _goml_runtime_core_string_len(x12)
        t1592 = inline2866
        var t1593 bool = index__26 < t1592
        if t1593 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1595 int = compound_old17 + x16
                index__26 = t1595
                continue
            } else {
                var t1597 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1597
            }
        } else {
            break Loop_loop1591
        }
    }
    var t1590 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t1590
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline2868 uint32 = uint32(rune(self__36))
    var inline2869 bool = utf8_valid_scalar(inline2868)
    if inline2869 {
        var inline2870 string = _goml_runtime_core_char_to_string(self__36)
        return inline2870
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t1636 int = _goml_runtime_core_string_len(self__38)
    return t1636
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t1639 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t1639
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline2886 bool = string_is_char_boundary(self__43, start__44)
    var inline2888 bool
    if inline2886 {
        var inline2891 bool = string_is_char_boundary(self__43, end__45)
        inline2888 = inline2891
    } else {
        inline2888 = false
    }
    if inline2888 {
        var inline2889 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline2889
    } else {
        var inline2890 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline2890
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__236 int) *ref_int_x {
    var t1674 *ref_int_x = ref__Ref_3int(value__236)
    return t1674
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__237 *ref_int_x) int {
    var t1677 int = ref_get__Ref_3int(self__237)
    return t1677
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t1680 string = _goml_runtime_core_int_to_string(self__34)
    return t1680
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__238 *ref_int_x, value__239 int) struct{} {
    ref_set__Ref_3int(self__238, value__239)
    return struct{}{}
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1687 bool
    var inline2903 bool = value__32 <= 1114111
    if inline2903 {
        var inline2904 bool = value__32 >= 55296
        var inline2906 bool
        if inline2904 {
            var inline2908 bool = value__32 <= 57343
            inline2906 = inline2908
        } else {
            inline2906 = false
        }
        var inline2907 bool = !inline2906
        t1687 = inline2907
    } else {
        t1687 = false
    }
    if t1687 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1688 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1688
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t1691 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t1691
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__155 *_goml_vec__goml_m_std_p_json_p_Value, elem__156 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__155, elem__156)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t1696 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t1696
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__155 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__156 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__155, elem__156)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t1700 string
    t1700 = value__31
    _goml_runtime_core_string_println(t1700)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1831 bool = index__6 < 0
    var jp1829 bool
    if t1831 {
        jp1829 = true
    } else {
        var t1832 bool = index__6 >= length__7
        jp1829 = t1832
    }
    if jp1829 {
        var inline2915 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2915
    } else {
        var t1716 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1716))
        var t1719 bool = first__8 < 128
        if t1719 {
            var inline2917 int = 1
            var inline2918 Option__char = char_from_uint32(first__8)
            switch inline2918.(type) {
            case Option__char_None:
                var inline2919 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2919
            case Option__char_Some:
                var inline2920 rune = inline2918.(Option__char_Some)._0
                var inline2922 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2920,
                    _2: inline2917,
                }
                return inline2922
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1723 bool = first__8 < 194
            if t1723 {
                var inline2924 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2924
            } else {
                var t1727 bool = first__8 < 224
                if t1727 {
                    var t1740 int = length__7 - index__6
                    var t1741 bool = t1740 < 2
                    if t1741 {
                        var inline2926 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2926
                    } else {
                        var t1729 int = index__6 + 1
                        var t1730 uint8
                        var inline2940 uint8 = _goml_runtime_core_string_byte_get(value__5, t1729)
                        t1730 = inline2940
                        var second__9 uint32 = uint32(uint8(t1730))
                        var t1733 bool
                        var inline2937 bool = second__9 < 128
                        if inline2937 {
                            t1733 = true
                        } else {
                            var inline2938 bool = second__9 > 191
                            t1733 = inline2938
                        }
                        if t1733 {
                            var inline2928 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2928
                        } else {
                            var t1735_rhs uint32 = 31
                            var t1735 uint32 = first__8 & t1735_rhs
                            var t1736_rhs int = 6
                            var t1736 uint32 = t1735 << t1736_rhs
                            var t1737_rhs uint32 = 63
                            var t1737 uint32 = second__9 & t1737_rhs
                            var t1738 uint32 = t1736 | t1737
                            var inline2930 int = 2
                            var inline2931 Option__char = char_from_uint32(t1738)
                            switch inline2931.(type) {
                            case Option__char_None:
                                var inline2932 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2932
                            case Option__char_Some:
                                var inline2933 rune = inline2931.(Option__char_Some)._0
                                var inline2935 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2933,
                                    _2: inline2930,
                                }
                                return inline2935
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1745 bool = first__8 < 240
                    if t1745 {
                        var t1778 int = length__7 - index__6
                        var t1779 bool = t1778 < 3
                        if t1779 {
                            var inline2942 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2942
                        } else {
                            var t1747 int = index__6 + 1
                            var t1748 uint8
                            var inline2957 uint8 = _goml_runtime_core_string_byte_get(value__5, t1747)
                            t1748 = inline2957
                            var second__10 uint32 = uint32(uint8(t1748))
                            var t1749 int = index__6 + 2
                            var t1750 uint8
                            var inline2955 uint8 = _goml_runtime_core_string_byte_get(value__5, t1749)
                            t1750 = inline2955
                            var third__11 uint32 = uint32(uint8(t1750))
                            var t1776 bool = utf8_invalid_continuation(second__10)
                            var jp1771 bool
                            if t1776 {
                                jp1771 = true
                            } else {
                                var inline2944 bool = third__11 < 128
                                if inline2944 {
                                    jp1771 = true
                                } else {
                                    var inline2945 bool = third__11 > 191
                                    jp1771 = inline2945
                                }
                            }
                            var jp1765 bool
                            if jp1771 {
                                jp1765 = true
                            } else {
                                var t1774 bool
                                var inline2947 uint32 = 224
                                var inline2948 bool = first__8 == inline2947
                                t1774 = inline2948
                                if t1774 {
                                    var t1775 bool = second__10 < 160
                                    jp1765 = t1775
                                } else {
                                    jp1765 = false
                                }
                            }
                            var jp1754 bool
                            if jp1765 {
                                jp1754 = true
                            } else {
                                var t1768 bool
                                var inline2950 uint32 = 237
                                var inline2951 bool = first__8 == inline2950
                                t1768 = inline2951
                                if t1768 {
                                    var t1769 bool = second__10 >= 160
                                    jp1754 = t1769
                                } else {
                                    jp1754 = false
                                }
                            }
                            if jp1754 {
                                var inline2953 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2953
                            } else {
                                var t1756_rhs uint32 = 15
                                var t1756 uint32 = first__8 & t1756_rhs
                                var t1757_rhs int = 12
                                var t1757 uint32 = t1756 << t1757_rhs
                                var t1758_rhs uint32 = 63
                                var t1758 uint32 = second__10 & t1758_rhs
                                var t1759_rhs int = 6
                                var t1759 uint32 = t1758 << t1759_rhs
                                var t1760 uint32 = t1757 | t1759
                                var t1761_rhs uint32 = 63
                                var t1761 uint32 = third__11 & t1761_rhs
                                var t1762 uint32 = t1760 | t1761
                                var t1763 Tuple3_4bool_4char_3int = utf8_valid_decode(t1762, 3)
                                return t1763
                            }
                        }
                    } else {
                        var t1783 bool = first__8 < 245
                        if t1783 {
                            var t1824 int = length__7 - index__6
                            var t1825 bool = t1824 < 4
                            if t1825 {
                                var t1826 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1826
                            } else {
                                var t1785 int = index__6 + 1
                                var t1786 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1785)
                                var second__12 uint32 = uint32(uint8(t1786))
                                var t1787 int = index__6 + 2
                                var t1788 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1787)
                                var third__13 uint32 = uint32(uint8(t1788))
                                var t1789 int = index__6 + 3
                                var t1790 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1789)
                                var fourth__14 uint32 = uint32(uint8(t1790))
                                var t1822 bool = utf8_invalid_continuation(second__12)
                                var jp1820 bool
                                if t1822 {
                                    jp1820 = true
                                } else {
                                    var t1823 bool = utf8_invalid_continuation(third__13)
                                    jp1820 = t1823
                                }
                                var jp1814 bool
                                if jp1820 {
                                    jp1814 = true
                                } else {
                                    var t1821 bool = utf8_invalid_continuation(fourth__14)
                                    jp1814 = t1821
                                }
                                var jp1808 bool
                                if jp1814 {
                                    jp1808 = true
                                } else {
                                    var t1817 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t1817 {
                                        var t1818 bool = second__12 < 144
                                        jp1808 = t1818
                                    } else {
                                        jp1808 = false
                                    }
                                }
                                var jp1794 bool
                                if jp1808 {
                                    jp1794 = true
                                } else {
                                    var t1811 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t1811 {
                                        var t1812 bool = second__12 > 143
                                        jp1794 = t1812
                                    } else {
                                        jp1794 = false
                                    }
                                }
                                if jp1794 {
                                    var t1795 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1795
                                } else {
                                    var t1796_rhs uint32 = 7
                                    var t1796 uint32 = first__8 & t1796_rhs
                                    var t1797_rhs int = 18
                                    var t1797 uint32 = t1796 << t1797_rhs
                                    var t1798_rhs uint32 = 63
                                    var t1798 uint32 = second__12 & t1798_rhs
                                    var t1799_rhs int = 12
                                    var t1799 uint32 = t1798 << t1799_rhs
                                    var t1800 uint32 = t1797 | t1799
                                    var t1801_rhs uint32 = 63
                                    var t1801 uint32 = third__13 & t1801_rhs
                                    var t1802_rhs int = 6
                                    var t1802 uint32 = t1801 << t1802_rhs
                                    var t1803 uint32 = t1800 | t1802
                                    var t1804_rhs uint32 = 63
                                    var t1804 uint32 = fourth__14 & t1804_rhs
                                    var t1805 uint32 = t1803 | t1804
                                    var t1806 Tuple3_4bool_4char_3int = utf8_valid_decode(t1805, 4)
                                    return t1806
                                }
                            }
                        } else {
                            var t1827 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1827
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t1837 uint32 = uint32(rune(value__29))
    var t1838 bool
    var inline2959 bool = t1837 <= 1114111
    if inline2959 {
        var inline2960 bool = t1837 >= 55296
        var inline2962 bool
        if inline2960 {
            var inline2964 bool = t1837 <= 57343
            inline2962 = inline2964
        } else {
            inline2962 = false
        }
        var inline2963 bool = !inline2962
        t1838 = inline2963
    } else {
        t1838 = false
    }
    if t1838 {
        var t1839 string = _goml_runtime_core_char_to_string(value__29)
        return t1839
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t1854 bool = index__16 < 0
    var jp1845 bool
    if t1854 {
        jp1845 = true
    } else {
        var t1855 int
        var inline2966 int = _goml_runtime_core_string_len(value__15)
        t1855 = inline2966
        var t1856 bool = index__16 > t1855
        jp1845 = t1856
    }
    if jp1845 {
        return false
    } else {
        var t1848 int
        var inline2975 int = _goml_runtime_core_string_len(value__15)
        t1848 = inline2975
        var t1849 bool
        var inline2973 bool = index__16 == t1848
        t1849 = inline2973
        if t1849 {
            return true
        } else {
            var t1850 uint8
            var inline2971 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1850 = inline2971
            var t1851_rhs uint8 = 192
            var t1851 uint8 = t1850 & t1851_rhs
            var t1852 bool
            var inline2968 uint8 = 128
            var inline2969 bool = t1851 == inline2968
            t1852 = inline2969
            var t1853 bool = !t1852
            return t1853
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t1865 bool = string_is_char_boundary(value__21, start__22)
    var jp1862 bool
    if t1865 {
        var t1866 bool = string_is_char_boundary(value__21, end__23)
        jp1862 = t1866
    } else {
        jp1862 = false
    }
    if jp1862 {
        var t1863 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t1863
    } else {
        var t1864 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t1864
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1873 bool = value__4 <= 1114111
    if t1873 {
        var t1877 bool = value__4 >= 55296
        var jp1875 bool
        if t1877 {
            var t1878 bool = value__4 <= 57343
            jp1875 = t1878
        } else {
            jp1875 = false
        }
        var t1876 bool = !jp1875
        return t1876
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t1883 string = _goml_runtime_core_int_to_string(self__69)
    return t1883
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t1886 string = _goml_runtime_core_bool_to_string(self__66)
    return t1886
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1889 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1889
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field3012 rune
    var inline2979 bool = utf8_valid_scalar(value__0)
    if inline2979 {
        var inline2980 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline2982 rune = inline2980._1
        commute_field3012 = inline2982
        var t1895 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field3012,
            _2: width__1,
        }
        return t1895
    } else {
        var inline2977 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2977
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1900 bool = value__3 < 128
    if t1900 {
        return true
    } else {
        var t1901 bool = value__3 > 191
        return t1901
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t1904 bool = self__102 == other__103
    return t1904
}

func main() {
    main0()
}
