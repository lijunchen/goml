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
    var inline1578 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline1578
    var t238 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t238
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline1593 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline1593
    var t252 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t252, length__5)
    var for_index1 int = 0
    Loop_loop254:
    for {
        var t255 bool = for_index1 < length__5
        if t255 {
            var for_item3 int = for_index1
            var t256 int = for_index1 + 1
            for_index1 = t256
            var t257 *_goml_vec_uint8 = self__3.values
            var t258 uint8
            var inline1589 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t258 = inline1589
            vec_push__Vec_5uint8(t257, t258)
            continue
        } else {
            break Loop_loop254
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t261 string
    var inline1595 string = _goml_runtime_core_char_to_string(value__8)
    t261 = inline1595
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t261)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var t511 string = "" + message__2
    var t512 string = t511 + " at byte "
    var t513 *ref_int_x = value__1.index
    var t514 int
    var inline1806 int = ref_get__Ref_3int(t513)
    t514 = inline1806
    var t515 string
    var inline1804 string = _goml_runtime_core_int_to_string(t514)
    t515 = inline1804
    var t516 string = t512 + t515
    return t516
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop531:
    for {
        var t539 *ref_int_x = value__4.index
        var t540 int
        var inline1839 int = ref_get__Ref_3int(t539)
        t540 = inline1839
        var t541 string = value__4.input
        var t542 int
        var inline1837 int = _goml_runtime_core_string_len(t541)
        t542 = inline1837
        var t543 bool = t540 < t542
        var jp533 bool
        if t543 {
            var t544 string = value__4.input
            var t545 *ref_int_x = value__4.index
            var t546 int
            var inline1831 int = ref_get__Ref_3int(t545)
            t546 = inline1831
            var t547 uint8
            var inline1829 uint8 = _goml_runtime_core_string_byte_get(t544, t546)
            t547 = inline1829
            var inline1820 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t547, 9)
            var inline1822 bool
            if inline1820 {
                inline1822 = true
            } else {
                var inline1827 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t547, 10)
                inline1822 = inline1827
            }
            var inline1824 bool
            if inline1822 {
                inline1824 = true
            } else {
                var inline1826 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t547, 13)
                inline1824 = inline1826
            }
            if inline1824 {
                jp533 = true
            } else {
                var inline1825 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t547, 32)
                jp533 = inline1825
            }
        } else {
            jp533 = false
        }
        if jp533 {
            var t534 *ref_int_x = value__4.index
            var t535 *ref_int_x = value__4.index
            var t536 int
            var inline1835 int = ref_get__Ref_3int(t535)
            t536 = inline1835
            var t537 int = t536 + 1
            ref_set__Ref_3int(t534, t537)
            continue
        } else {
            break Loop_loop531
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var t578 bool = value__5 >= 48
    var jp554 bool
    if t578 {
        var t579 bool = value__5 <= 57
        jp554 = t579
    } else {
        jp554 = false
    }
    if jp554 {
        var t555 uint8 = value__5 - 48
        var t556 uint32 = uint32(uint8(t555))
        var t557 Option__uint32 = Option__uint32_Some{
            _0: t556,
        }
        return t557
    } else {
        var t576 bool = value__5 >= 65
        var jp561 bool
        if t576 {
            var t577 bool = value__5 <= 70
            jp561 = t577
        } else {
            jp561 = false
        }
        if jp561 {
            var t562 uint8 = value__5 - 65
            var t563 uint8 = t562 + 10
            var t564 uint32 = uint32(uint8(t563))
            var t565 Option__uint32 = Option__uint32_Some{
                _0: t564,
            }
            return t565
        } else {
            var t574 bool = value__5 >= 97
            var jp569 bool
            if t574 {
                var t575 bool = value__5 <= 102
                jp569 = t575
            } else {
                jp569 = false
            }
            if jp569 {
                var t570 uint8 = value__5 - 97
                var t571 uint8 = t570 + 10
                var t572 uint32 = uint32(uint8(t571))
                var t573 Option__uint32 = Option__uint32_Some{
                    _0: t572,
                }
                return t573
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t584 *ref_int_x = value__6.index
    var t585 int
    var inline1867 int = ref_get__Ref_3int(t584)
    t585 = inline1867
    var t586 int = t585 + 4
    var t587 string = value__6.input
    var t588 int
    var inline1865 int = _goml_runtime_core_string_len(t587)
    t588 = inline1865
    var t589 bool = t586 > t588
    if t589 {
        var t590 string
        var inline1841 string = "incomplete unicode escape"
        var inline1842 string = "" + inline1841
        var inline1843 string = inline1842 + " at byte "
        var inline1844 *ref_int_x = value__6.index
        var inline1845 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline1844)
        var inline1846 string = _goml_m_inherent_i_int_i_int_i_to__string(inline1845)
        var inline1847 string = inline1843 + inline1846
        t590 = inline1847
        var t591 Result__uint32__string = Result__uint32__string_Err{
            _0: t590,
        }
        return t591
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop598:
        for {
            var t599 bool = for_index0 < for_limit1
            if t599 {
                var for_item2 int = for_index0
                var t600 int = for_index0 + 1
                for_index0 = t600
                var t601 string = value__6.input
                var t602 *ref_int_x = value__6.index
                var t603 int
                var inline1859 int = ref_get__Ref_3int(t602)
                t603 = inline1859
                var t604 int = t603 + for_item2
                var t605 uint8
                var inline1857 uint8 = _goml_runtime_core_string_byte_get(t601, t604)
                t605 = inline1857
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t605)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t607 string
                    var inline1849 string = "invalid unicode escape"
                    var inline1850 string = "" + inline1849
                    var inline1851 string = inline1850 + " at byte "
                    var inline1852 *ref_int_x = value__6.index
                    var inline1853 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline1852)
                    var inline1854 string = _goml_m_inherent_i_int_i_int_i_to__string(inline1853)
                    var inline1855 string = inline1851 + inline1854
                    t607 = inline1855
                    var t608 Result__uint32__string = Result__uint32__string_Err{
                        _0: t607,
                    }
                    return t608
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var t609 uint32 = result__7 * 16
                    var t610 uint32 = t609 + x5
                    result__7 = t610
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop598
            }
        }
        var t593 *ref_int_x = value__6.index
        var t594 *ref_int_x = value__6.index
        var t595 int
        var inline1863 int = ref_get__Ref_3int(t594)
        t595 = inline1863
        var t596 int = t595 + 4
        ref_set__Ref_3int(t593, t596)
        var t597 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        return t597
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var commute_field2442 rune
    var inline1880 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__12)
    var inline1881 bool = inline1880._0
    var inline1882 rune = inline1880._1
    if inline1881 {
        commute_field2442 = inline1882
        var inline1877 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field2442)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__11, inline1877)
        var t617 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t617
    } else {
        var t615 string
        var inline1869 string = "invalid unicode codepoint"
        var inline1870 string = "" + inline1869
        var inline1871 string = inline1870 + " at byte "
        var inline1872 *ref_int_x = value__10.index
        var inline1873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline1872)
        var inline1874 string = _goml_m_inherent_i_int_i_int_i_to__string(inline1873)
        var inline1875 string = inline1871 + inline1874
        t615 = inline1875
        var t616 Result__unit__string = Result__unit__string_Err{
            _0: t615,
        }
        return t616
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp621 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        jp621 = x13
        var t683 bool = jp621 >= 55296
        var jp625 bool
        if t683 {
            var t684 bool = jp621 <= 56319
            jp625 = t684
        } else {
            jp625 = false
        }
        if jp625 {
            var t662 *ref_int_x = value__14.index
            var t663 int
            var inline1931 int = ref_get__Ref_3int(t662)
            t663 = inline1931
            var t664 int = t663 + 2
            var t665 string = value__14.input
            var t666 int
            var inline1929 int = _goml_runtime_core_string_len(t665)
            t666 = inline1929
            var t667 bool = t664 > t666
            var jp654 bool
            if t667 {
                jp654 = true
            } else {
                var t668 string = value__14.input
                var t669 *ref_int_x = value__14.index
                var t670 int
                var inline1892 int = ref_get__Ref_3int(t669)
                t670 = inline1892
                var t671 uint8
                var inline1890 uint8 = _goml_runtime_core_string_byte_get(t668, t670)
                t671 = inline1890
                var t672 bool
                var inline1887 uint8 = 92
                var inline1888 bool = t671 == inline1887
                t672 = inline1888
                var t673 bool = !t672
                jp654 = t673
            }
            var jp629 bool
            if jp654 {
                jp629 = true
            } else {
                var t655 string = value__14.input
                var t656 *ref_int_x = value__14.index
                var t657 int
                var inline1899 int = ref_get__Ref_3int(t656)
                t657 = inline1899
                var t658 int = t657 + 1
                var t659 uint8
                var inline1897 uint8 = _goml_runtime_core_string_byte_get(t655, t658)
                t659 = inline1897
                var t660 bool
                var inline1894 uint8 = 117
                var inline1895 bool = t659 == inline1894
                t660 = inline1895
                var t661 bool = !t660
                jp629 = t661
            }
            if jp629 {
                var t630 string
                var inline1901 string = "missing low surrogate"
                var inline1902 string = "" + inline1901
                var inline1903 string = inline1902 + " at byte "
                var inline1904 *ref_int_x = value__14.index
                var inline1905 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline1904)
                var inline1906 string = _goml_m_inherent_i_int_i_int_i_to__string(inline1905)
                var inline1907 string = inline1903 + inline1906
                t630 = inline1907
                var t631 Result__unit__string = Result__unit__string_Err{
                    _0: t630,
                }
                return t631
            } else {
                var t632 *ref_int_x = value__14.index
                var t633 *ref_int_x = value__14.index
                var t634 int
                var inline1927 int = ref_get__Ref_3int(t633)
                t634 = inline1927
                var t635 int = t634 + 2
                ref_set__Ref_3int(t632, t635)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp637 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    jp637 = x17
                    var t650 bool = jp637 < 56320
                    var jp641 bool
                    if t650 {
                        jp641 = true
                    } else {
                        var t651 bool = jp637 > 57343
                        jp641 = t651
                    }
                    if jp641 {
                        var t642 string
                        var inline1909 string = "invalid low surrogate"
                        var inline1910 string = "" + inline1909
                        var inline1911 string = inline1910 + " at byte "
                        var inline1912 *ref_int_x = value__14.index
                        var inline1913 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline1912)
                        var inline1914 string = _goml_m_inherent_i_int_i_int_i_to__string(inline1913)
                        var inline1915 string = inline1911 + inline1914
                        t642 = inline1915
                        var t643 Result__unit__string = Result__unit__string_Err{
                            _0: t642,
                        }
                        return t643
                    } else {
                        var t644 uint32 = jp621 - 55296
                        var t645 uint32 = t644 * 1024
                        var t646 uint32 = 65536 + t645
                        var t647 uint32 = t646 + jp637
                        var t648 uint32 = t647 - 56320
                        var inline1917 Option__char = char_from_uint32(t648)
                        switch inline1917.(type) {
                        case Option__char_None:
                            var inline1918 string = _goml_m_std_p_json_p_json__error(value__14, "invalid unicode codepoint")
                            var inline1919 Result__unit__string = Result__unit__string_Err{
                                _0: inline1918,
                            }
                            return inline1919
                        case Option__char_Some:
                            var inline1920 rune = inline1917.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__15, inline1920)
                            var inline1923 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline1923
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var t652 Result__unit__string = Result__unit__string_Err{
                        _0: x18,
                    }
                    return t652
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t681 bool = jp621 >= 56320
            var jp677 bool
            if t681 {
                var t682 bool = jp621 <= 57343
                jp677 = t682
            } else {
                jp677 = false
            }
            if jp677 {
                var t678 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t679 Result__unit__string = Result__unit__string_Err{
                    _0: t678,
                }
                return t679
            } else {
                var t680 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, jp621)
                return t680
            }
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var t685 Result__unit__string = Result__unit__string_Err{
            _0: x14,
        }
        return t685
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t801 *ref_int_x = value__18.index
    var t802 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t801)
    var t803 string = value__18.input
    var t804 int = _goml_m_inherent_i_string_i_string_i_byte__len(t803)
    var t805 bool = t802 >= t804
    var jp793 bool
    if t805 {
        jp793 = true
    } else {
        var t806 string = value__18.input
        var t807 *ref_int_x = value__18.index
        var t808 int
        var inline1938 int = ref_get__Ref_3int(t807)
        t808 = inline1938
        var t809 uint8
        var inline1936 uint8 = _goml_runtime_core_string_byte_get(t806, t808)
        t809 = inline1936
        var t810 bool
        var inline1933 uint8 = 34
        var inline1934 bool = t809 == inline1933
        t810 = inline1934
        var t811 bool = !t810
        jp793 = t811
    }
    if jp793 {
        var t794 string
        var inline1940 string = "expected string"
        var inline1941 string = "" + inline1940
        var inline1942 string = inline1941 + " at byte "
        var inline1943 *ref_int_x = value__18.index
        var inline1944 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline1943)
        var inline1945 string = _goml_m_inherent_i_int_i_int_i_to__string(inline1944)
        var inline1946 string = inline1942 + inline1945
        t794 = inline1946
        var t795 Result__string__string = Result__string__string_Err{
            _0: t794,
        }
        return t795
    } else {
        var t796 *ref_int_x = value__18.index
        var t797 *ref_int_x = value__18.index
        var t798 int
        var inline1950 int = ref_get__Ref_3int(t797)
        t798 = inline1950
        var t799 int = t798 + 1
        ref_set__Ref_3int(t796, t799)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t689 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t689)
        Loop_loop693:
        for {
            var t694 *ref_int_x = value__18.index
            var t695 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t694)
            var t696 string = value__18.input
            var t697 int = _goml_m_inherent_i_string_i_string_i_byte__len(t696)
            var t698 bool = t695 < t697
            if t698 {
                var t699 string = value__18.input
                var t700 *ref_int_x = value__18.index
                var t701 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t700)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t699, t701)
                var t703 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 34)
                if t703 {
                    var t711 *ref_int_x = value__18.index
                    var t712 int
                    var inline1966 int = ref_get__Ref_3int(t711)
                    t712 = inline1966
                    var t713 bool = segment__20 < t712
                    if t713 {
                        var t714 string = value__18.input
                        var t715 *ref_int_x = value__18.index
                        var t716 int
                        var inline1954 int = ref_get__Ref_3int(t715)
                        t716 = inline1954
                        var t717 string
                        var inline1952 string = _goml_runtime_core_string_byte_slice(t714, segment__20, t716)
                        t717 = inline1952
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t717)
                    } else {}
                    var t705 *ref_int_x = value__18.index
                    var t706 *ref_int_x = value__18.index
                    var t707 int
                    var inline1964 int = ref_get__Ref_3int(t706)
                    t707 = inline1964
                    var t708 int = t707 + 1
                    ref_set__Ref_3int(t705, t708)
                    var t709 string
                    var inline1956 *_goml_vec_uint8 = builder__19.values
                    var inline1957 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(inline1956)
                    var inline1959 string = inline1957._1
                    t709 = inline1959
                    var t710 Result__string__string = Result__string__string_Ok{
                        _0: t709,
                    }
                    return t710
                } else {
                    var t720 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 92)
                    if t720 {
                        var t775 *ref_int_x = value__18.index
                        var t776 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t775)
                        var t777 bool = segment__20 < t776
                        if t777 {
                            var t778 string = value__18.input
                            var t779 *ref_int_x = value__18.index
                            var t780 int
                            var inline1970 int = ref_get__Ref_3int(t779)
                            t780 = inline1970
                            var t781 string
                            var inline1968 string = _goml_runtime_core_string_byte_slice(t778, segment__20, t780)
                            t781 = inline1968
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t781)
                        } else {}
                        var t722 *ref_int_x = value__18.index
                        var t723 *ref_int_x = value__18.index
                        var t724 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t723)
                        var t725 int = t724 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t722, t725)
                        var t768 *ref_int_x = value__18.index
                        var t769 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t768)
                        var t770 string = value__18.input
                        var t771 int = _goml_m_inherent_i_string_i_string_i_byte__len(t770)
                        var t772 bool = t769 >= t771
                        if t772 {
                            var t773 string
                            var inline1972 string = "incomplete escape"
                            var inline1973 string = "" + inline1972
                            var inline1974 string = inline1973 + " at byte "
                            var inline1975 *ref_int_x = value__18.index
                            var inline1976 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline1975)
                            var inline1977 string = _goml_m_inherent_i_int_i_int_i_to__string(inline1976)
                            var inline1978 string = inline1974 + inline1977
                            t773 = inline1978
                            var t774 Result__string__string = Result__string__string_Err{
                                _0: t773,
                            }
                            return t774
                        } else {
                            var t727 string = value__18.input
                            var t728 *ref_int_x = value__18.index
                            var t729 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t728)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t727, t729)
                            var t730 *ref_int_x = value__18.index
                            var t731 *ref_int_x = value__18.index
                            var t732 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t731)
                            var t733 int = t732 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t730, t733)
                            var t737 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 34)
                            if t737 {
                                var inline1980 rune = 34
                                var inline1981 string = _goml_m_inherent_i_char_i_char_i_to__string(inline1980)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, inline1981)
                                var t735 *ref_int_x = value__18.index
                                var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                segment__20 = t736
                                continue
                            } else {
                                var t740 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 92)
                                if t740 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t735 *ref_int_x = value__18.index
                                    var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                    segment__20 = t736
                                    continue
                                } else {
                                    var t743 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 47)
                                    if t743 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t735 *ref_int_x = value__18.index
                                        var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                        segment__20 = t736
                                        continue
                                    } else {
                                        var t746 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 98)
                                        if t746 {
                                            var mtmp26 Option__char = char_from_uint32(8)
                                            switch mtmp26.(type) {
                                            case Option__char_None:
                                                var t735 *ref_int_x = value__18.index
                                                var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                segment__20 = t736
                                                continue
                                            case Option__char_Some:
                                                var x27 rune = mtmp26.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x27)
                                                var t735 *ref_int_x = value__18.index
                                                var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                segment__20 = t736
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t750 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 102)
                                            if t750 {
                                                var mtmp28 Option__char = char_from_uint32(12)
                                                switch mtmp28.(type) {
                                                case Option__char_None:
                                                    var t735 *ref_int_x = value__18.index
                                                    var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                    segment__20 = t736
                                                    continue
                                                case Option__char_Some:
                                                    var x29 rune = mtmp28.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x29)
                                                    var t735 *ref_int_x = value__18.index
                                                    var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                    segment__20 = t736
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t754 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 110)
                                                if t754 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t735 *ref_int_x = value__18.index
                                                    var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                    segment__20 = t736
                                                    continue
                                                } else {
                                                    var t757 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 114)
                                                    if t757 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t735 *ref_int_x = value__18.index
                                                        var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                        segment__20 = t736
                                                        continue
                                                    } else {
                                                        var t760 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 116)
                                                        if t760 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t735 *ref_int_x = value__18.index
                                                            var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                            segment__20 = t736
                                                            continue
                                                        } else {
                                                            var t763 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 117)
                                                            if t763 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t735 *ref_int_x = value__18.index
                                                                    var t736 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t735)
                                                                    segment__20 = t736
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var t765 Result__string__string = Result__string__string_Err{
                                                                        _0: x32,
                                                                    }
                                                                    return t765
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t766 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t767 Result__string__string = Result__string__string_Err{
                                                                    _0: t766,
                                                                }
                                                                return t767
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
                        var t784 bool = byte__21 < 32
                        if t784 {
                            var t785 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t786 Result__string__string = Result__string__string_Err{
                                _0: t785,
                            }
                            return t786
                        } else {
                            var t787 *ref_int_x = value__18.index
                            var t788 *ref_int_x = value__18.index
                            var t789 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t788)
                            var t790 int = t789 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t787, t790)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop693
            }
        }
        var t691 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t692 Result__string__string = Result__string__string_Err{
            _0: t691,
        }
        return t692
    }
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var t820 *ref_int_x = value__26.index
    var start__27 int
    var inline2001 int = ref_get__Ref_3int(t820)
    start__27 = inline2001
    Loop_loop825:
    for {
        var t833 *ref_int_x = value__26.index
        var t834 int
        var inline1997 int = ref_get__Ref_3int(t833)
        t834 = inline1997
        var t835 string = value__26.input
        var t836 int
        var inline1995 int = _goml_runtime_core_string_len(t835)
        t836 = inline1995
        var t837 bool = t834 < t836
        var jp827 bool
        if t837 {
            var t838 string = value__26.input
            var t839 *ref_int_x = value__26.index
            var t840 int
            var inline1989 int = ref_get__Ref_3int(t839)
            t840 = inline1989
            var t841 uint8
            var inline1987 uint8 = _goml_runtime_core_string_byte_get(t838, t840)
            t841 = inline1987
            var inline1984 bool = t841 >= 48
            if inline1984 {
                var inline1985 bool = t841 <= 57
                jp827 = inline1985
            } else {
                jp827 = false
            }
        } else {
            jp827 = false
        }
        if jp827 {
            var t828 *ref_int_x = value__26.index
            var t829 *ref_int_x = value__26.index
            var t830 int
            var inline1993 int = ref_get__Ref_3int(t829)
            t830 = inline1993
            var t831 int = t830 + 1
            ref_set__Ref_3int(t828, t831)
            continue
        } else {
            break Loop_loop825
        }
    }
    var t822 *ref_int_x = value__26.index
    var t823 int
    var inline1999 int = ref_get__Ref_3int(t822)
    t823 = inline1999
    var t824 bool = t823 > start__27
    return t824
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t845 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t845)
    var t967 string = value__28.input
    var t968 *ref_int_x = value__28.index
    var t969 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t968)
    var t970 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t967, t969)
    var t971 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t970, 45)
    if t971 {
        var t972 *ref_int_x = value__28.index
        var t973 *ref_int_x = value__28.index
        var t974 int
        var inline2005 int = ref_get__Ref_3int(t973)
        t974 = inline2005
        var t975 int = t974 + 1
        ref_set__Ref_3int(t972, t975)
    } else {}
    var t930 *ref_int_x = value__28.index
    var t931 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t930)
    var t932 string = value__28.input
    var t933 int = _goml_m_inherent_i_string_i_string_i_byte__len(t932)
    var t934 bool = t931 >= t933
    if t934 {
        var t935 string
        var inline2007 string = "incomplete number"
        var inline2008 string = "" + inline2007
        var inline2009 string = inline2008 + " at byte "
        var inline2010 *ref_int_x = value__28.index
        var inline2011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2010)
        var inline2012 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2011)
        var inline2013 string = inline2009 + inline2012
        t935 = inline2013
        var t936 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t935,
        }
        return t936
    } else {
        var t938 string = value__28.input
        var t939 *ref_int_x = value__28.index
        var t940 int
        var inline2051 int = ref_get__Ref_3int(t939)
        t940 = inline2051
        var t941 uint8
        var inline2049 uint8 = _goml_runtime_core_string_byte_get(t938, t940)
        t941 = inline2049
        var t942 bool
        var inline2046 uint8 = 48
        var inline2047 bool = t941 == inline2046
        t942 = inline2047
        if t942 {
            var t943 *ref_int_x = value__28.index
            var t944 *ref_int_x = value__28.index
            var t945 int
            var inline2036 int = ref_get__Ref_3int(t944)
            t945 = inline2036
            var t946 int = t945 + 1
            ref_set__Ref_3int(t943, t946)
            var t952 *ref_int_x = value__28.index
            var t953 int
            var inline2032 int = ref_get__Ref_3int(t952)
            t953 = inline2032
            var t954 string = value__28.input
            var t955 int
            var inline2030 int = _goml_runtime_core_string_len(t954)
            t955 = inline2030
            var t956 bool = t953 < t955
            var jp949 bool
            if t956 {
                var t957 string = value__28.input
                var t958 *ref_int_x = value__28.index
                var t959 int
                var inline2020 int = ref_get__Ref_3int(t958)
                t959 = inline2020
                var t960 uint8
                var inline2018 uint8 = _goml_runtime_core_string_byte_get(t957, t959)
                t960 = inline2018
                var inline2015 bool = t960 >= 48
                if inline2015 {
                    var inline2016 bool = t960 <= 57
                    jp949 = inline2016
                } else {
                    jp949 = false
                }
            } else {
                jp949 = false
            }
            if jp949 {
                var t950 string
                var inline2022 string = "invalid leading zero"
                var inline2023 string = "" + inline2022
                var inline2024 string = inline2023 + " at byte "
                var inline2025 *ref_int_x = value__28.index
                var inline2026 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2025)
                var inline2027 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2026)
                var inline2028 string = inline2024 + inline2027
                t950 = inline2028
                var t951 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t950,
                }
                return t951
            } else {
                var t920 *ref_int_x = value__28.index
                var t921 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t920)
                var t922 string = value__28.input
                var t923 int = _goml_m_inherent_i_string_i_string_i_byte__len(t922)
                var t924 bool = t921 < t923
                var jp910 bool
                if t924 {
                    var t925 string = value__28.input
                    var t926 *ref_int_x = value__28.index
                    var t927 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t926)
                    var t928 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t925, t927)
                    var t929 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t928, 46)
                    jp910 = t929
                } else {
                    jp910 = false
                }
                if jp910 {
                    var t911 *ref_int_x = value__28.index
                    var t912 *ref_int_x = value__28.index
                    var t913 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t912)
                    var t914 int = t913 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t911, t914)
                    var t916 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t917 bool = !t916
                    if t917 {
                        var t918 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t919 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t918,
                        }
                        return t919
                    } else {
                        var t892 *ref_int_x = value__28.index
                        var t893 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t892)
                        var t894 string = value__28.input
                        var t895 int = _goml_m_inherent_i_string_i_string_i_byte__len(t894)
                        var t896 bool = t893 < t895
                        var jp857 bool
                        if t896 {
                            var t899 string = value__28.input
                            var t900 *ref_int_x = value__28.index
                            var t901 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t900)
                            var t902 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t899, t901)
                            var t903 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t902, 101)
                            if t903 {
                                jp857 = true
                            } else {
                                var t904 string = value__28.input
                                var t905 *ref_int_x = value__28.index
                                var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                                var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                                var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 69)
                                jp857 = t908
                            }
                        } else {
                            jp857 = false
                        }
                        if jp857 {
                            var t858 *ref_int_x = value__28.index
                            var t859 *ref_int_x = value__28.index
                            var t860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t859)
                            var t861 int = t860 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t858, t861)
                            var t875 *ref_int_x = value__28.index
                            var t876 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t875)
                            var t877 string = value__28.input
                            var t878 int = _goml_m_inherent_i_string_i_string_i_byte__len(t877)
                            var t879 bool = t876 < t878
                            var jp869 bool
                            if t879 {
                                var t882 string = value__28.input
                                var t883 *ref_int_x = value__28.index
                                var t884 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t883)
                                var t885 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t882, t884)
                                var t886 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t885, 43)
                                if t886 {
                                    jp869 = true
                                } else {
                                    var t887 string = value__28.input
                                    var t888 *ref_int_x = value__28.index
                                    var t889 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t888)
                                    var t890 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t887, t889)
                                    var t891 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t890, 45)
                                    jp869 = t891
                                }
                            } else {
                                jp869 = false
                            }
                            if jp869 {
                                var t870 *ref_int_x = value__28.index
                                var t871 *ref_int_x = value__28.index
                                var t872 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t871)
                                var t873 int = t872 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t870, t873)
                            } else {}
                            var t864 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t865 bool = !t864
                            if t865 {
                                var t866 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t867 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t866,
                                }
                                return t867
                            } else {
                                var t850 string = value__28.input
                                var t851 *ref_int_x = value__28.index
                                var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                                var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                                var t854 _goml_m_std_p_json_p_Value = Number{
                                    _0: t853,
                                }
                                var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t854,
                                }
                                return t855
                            }
                        } else {
                            var t850 string = value__28.input
                            var t851 *ref_int_x = value__28.index
                            var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                            var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                            var t854 _goml_m_std_p_json_p_Value = Number{
                                _0: t853,
                            }
                            var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t854,
                            }
                            return t855
                        }
                    }
                } else {
                    var t892 *ref_int_x = value__28.index
                    var t893 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t892)
                    var t894 string = value__28.input
                    var t895 int = _goml_m_inherent_i_string_i_string_i_byte__len(t894)
                    var t896 bool = t893 < t895
                    var jp857 bool
                    if t896 {
                        var t899 string = value__28.input
                        var t900 *ref_int_x = value__28.index
                        var t901 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t900)
                        var t902 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t899, t901)
                        var t903 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t902, 101)
                        if t903 {
                            jp857 = true
                        } else {
                            var t904 string = value__28.input
                            var t905 *ref_int_x = value__28.index
                            var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                            var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                            var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 69)
                            jp857 = t908
                        }
                    } else {
                        jp857 = false
                    }
                    if jp857 {
                        var t858 *ref_int_x = value__28.index
                        var t859 *ref_int_x = value__28.index
                        var t860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t859)
                        var t861 int = t860 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t858, t861)
                        var t875 *ref_int_x = value__28.index
                        var t876 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t875)
                        var t877 string = value__28.input
                        var t878 int = _goml_m_inherent_i_string_i_string_i_byte__len(t877)
                        var t879 bool = t876 < t878
                        var jp869 bool
                        if t879 {
                            var t882 string = value__28.input
                            var t883 *ref_int_x = value__28.index
                            var t884 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t883)
                            var t885 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t882, t884)
                            var t886 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t885, 43)
                            if t886 {
                                jp869 = true
                            } else {
                                var t887 string = value__28.input
                                var t888 *ref_int_x = value__28.index
                                var t889 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t888)
                                var t890 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t887, t889)
                                var t891 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t890, 45)
                                jp869 = t891
                            }
                        } else {
                            jp869 = false
                        }
                        if jp869 {
                            var t870 *ref_int_x = value__28.index
                            var t871 *ref_int_x = value__28.index
                            var t872 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t871)
                            var t873 int = t872 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t870, t873)
                        } else {}
                        var t864 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t865 bool = !t864
                        if t865 {
                            var t866 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t867 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t866,
                            }
                            return t867
                        } else {
                            var t850 string = value__28.input
                            var t851 *ref_int_x = value__28.index
                            var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                            var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                            var t854 _goml_m_std_p_json_p_Value = Number{
                                _0: t853,
                            }
                            var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t854,
                            }
                            return t855
                        }
                    } else {
                        var t850 string = value__28.input
                        var t851 *ref_int_x = value__28.index
                        var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                        var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                        var t854 _goml_m_std_p_json_p_Value = Number{
                            _0: t853,
                        }
                        var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t854,
                        }
                        return t855
                    }
                }
            }
        } else {
            var t963 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t964 bool = !t963
            if t964 {
                var t965 string
                var inline2038 string = "expected number"
                var inline2039 string = "" + inline2038
                var inline2040 string = inline2039 + " at byte "
                var inline2041 *ref_int_x = value__28.index
                var inline2042 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2041)
                var inline2043 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2042)
                var inline2044 string = inline2040 + inline2043
                t965 = inline2044
                var t966 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t965,
                }
                return t966
            } else {
                var t920 *ref_int_x = value__28.index
                var t921 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t920)
                var t922 string = value__28.input
                var t923 int = _goml_m_inherent_i_string_i_string_i_byte__len(t922)
                var t924 bool = t921 < t923
                var jp910 bool
                if t924 {
                    var t925 string = value__28.input
                    var t926 *ref_int_x = value__28.index
                    var t927 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t926)
                    var t928 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t925, t927)
                    var t929 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t928, 46)
                    jp910 = t929
                } else {
                    jp910 = false
                }
                if jp910 {
                    var t911 *ref_int_x = value__28.index
                    var t912 *ref_int_x = value__28.index
                    var t913 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t912)
                    var t914 int = t913 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t911, t914)
                    var t916 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t917 bool = !t916
                    if t917 {
                        var t918 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t919 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t918,
                        }
                        return t919
                    } else {
                        var t892 *ref_int_x = value__28.index
                        var t893 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t892)
                        var t894 string = value__28.input
                        var t895 int = _goml_m_inherent_i_string_i_string_i_byte__len(t894)
                        var t896 bool = t893 < t895
                        var jp857 bool
                        if t896 {
                            var t899 string = value__28.input
                            var t900 *ref_int_x = value__28.index
                            var t901 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t900)
                            var t902 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t899, t901)
                            var t903 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t902, 101)
                            if t903 {
                                jp857 = true
                            } else {
                                var t904 string = value__28.input
                                var t905 *ref_int_x = value__28.index
                                var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                                var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                                var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 69)
                                jp857 = t908
                            }
                        } else {
                            jp857 = false
                        }
                        if jp857 {
                            var t858 *ref_int_x = value__28.index
                            var t859 *ref_int_x = value__28.index
                            var t860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t859)
                            var t861 int = t860 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t858, t861)
                            var t875 *ref_int_x = value__28.index
                            var t876 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t875)
                            var t877 string = value__28.input
                            var t878 int = _goml_m_inherent_i_string_i_string_i_byte__len(t877)
                            var t879 bool = t876 < t878
                            var jp869 bool
                            if t879 {
                                var t882 string = value__28.input
                                var t883 *ref_int_x = value__28.index
                                var t884 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t883)
                                var t885 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t882, t884)
                                var t886 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t885, 43)
                                if t886 {
                                    jp869 = true
                                } else {
                                    var t887 string = value__28.input
                                    var t888 *ref_int_x = value__28.index
                                    var t889 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t888)
                                    var t890 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t887, t889)
                                    var t891 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t890, 45)
                                    jp869 = t891
                                }
                            } else {
                                jp869 = false
                            }
                            if jp869 {
                                var t870 *ref_int_x = value__28.index
                                var t871 *ref_int_x = value__28.index
                                var t872 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t871)
                                var t873 int = t872 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t870, t873)
                            } else {}
                            var t864 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t865 bool = !t864
                            if t865 {
                                var t866 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t867 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t866,
                                }
                                return t867
                            } else {
                                var t850 string = value__28.input
                                var t851 *ref_int_x = value__28.index
                                var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                                var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                                var t854 _goml_m_std_p_json_p_Value = Number{
                                    _0: t853,
                                }
                                var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t854,
                                }
                                return t855
                            }
                        } else {
                            var t850 string = value__28.input
                            var t851 *ref_int_x = value__28.index
                            var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                            var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                            var t854 _goml_m_std_p_json_p_Value = Number{
                                _0: t853,
                            }
                            var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t854,
                            }
                            return t855
                        }
                    }
                } else {
                    var t892 *ref_int_x = value__28.index
                    var t893 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t892)
                    var t894 string = value__28.input
                    var t895 int = _goml_m_inherent_i_string_i_string_i_byte__len(t894)
                    var t896 bool = t893 < t895
                    var jp857 bool
                    if t896 {
                        var t899 string = value__28.input
                        var t900 *ref_int_x = value__28.index
                        var t901 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t900)
                        var t902 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t899, t901)
                        var t903 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t902, 101)
                        if t903 {
                            jp857 = true
                        } else {
                            var t904 string = value__28.input
                            var t905 *ref_int_x = value__28.index
                            var t906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t905)
                            var t907 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t904, t906)
                            var t908 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t907, 69)
                            jp857 = t908
                        }
                    } else {
                        jp857 = false
                    }
                    if jp857 {
                        var t858 *ref_int_x = value__28.index
                        var t859 *ref_int_x = value__28.index
                        var t860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t859)
                        var t861 int = t860 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t858, t861)
                        var t875 *ref_int_x = value__28.index
                        var t876 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t875)
                        var t877 string = value__28.input
                        var t878 int = _goml_m_inherent_i_string_i_string_i_byte__len(t877)
                        var t879 bool = t876 < t878
                        var jp869 bool
                        if t879 {
                            var t882 string = value__28.input
                            var t883 *ref_int_x = value__28.index
                            var t884 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t883)
                            var t885 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t882, t884)
                            var t886 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t885, 43)
                            if t886 {
                                jp869 = true
                            } else {
                                var t887 string = value__28.input
                                var t888 *ref_int_x = value__28.index
                                var t889 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t888)
                                var t890 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t887, t889)
                                var t891 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t890, 45)
                                jp869 = t891
                            }
                        } else {
                            jp869 = false
                        }
                        if jp869 {
                            var t870 *ref_int_x = value__28.index
                            var t871 *ref_int_x = value__28.index
                            var t872 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t871)
                            var t873 int = t872 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t870, t873)
                        } else {}
                        var t864 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t865 bool = !t864
                        if t865 {
                            var t866 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t867 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t866,
                            }
                            return t867
                        } else {
                            var t850 string = value__28.input
                            var t851 *ref_int_x = value__28.index
                            var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                            var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                            var t854 _goml_m_std_p_json_p_Value = Number{
                                _0: t853,
                            }
                            var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t854,
                            }
                            return t855
                        }
                    } else {
                        var t850 string = value__28.input
                        var t851 *ref_int_x = value__28.index
                        var t852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t851)
                        var t853 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t850, start__29, t852)
                        var t854 _goml_m_std_p_json_p_Value = Number{
                            _0: t853,
                        }
                        var t855 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t854,
                        }
                        return t855
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t991 *ref_int_x = value__30.index
    var t992 int
    var inline2081 int = ref_get__Ref_3int(t991)
    t992 = inline2081
    var t993 int
    var inline2079 int = _goml_runtime_core_string_len(expected__31)
    t993 = inline2079
    var t994 int = t992 + t993
    var t995 string = value__30.input
    var t996 int
    var inline2077 int = _goml_runtime_core_string_len(t995)
    t996 = inline2077
    var t997 bool = t994 <= t996
    var jp982 bool
    if t997 {
        var t998 string = value__30.input
        var t999 *ref_int_x = value__30.index
        var t1000 int
        var inline2061 int = ref_get__Ref_3int(t999)
        t1000 = inline2061
        var t1001 *ref_int_x = value__30.index
        var t1002 int
        var inline2059 int = ref_get__Ref_3int(t1001)
        t1002 = inline2059
        var t1003 int
        var inline2057 int = _goml_runtime_core_string_len(expected__31)
        t1003 = inline2057
        var t1004 int = t1002 + t1003
        var t1005 string
        var inline2055 string = _goml_runtime_core_string_byte_slice(t998, t1000, t1004)
        t1005 = inline2055
        var inline2053 bool = t1005 == expected__31
        jp982 = inline2053
    } else {
        jp982 = false
    }
    if jp982 {
        var t983 *ref_int_x = value__30.index
        var t984 *ref_int_x = value__30.index
        var t985 int
        var inline2067 int = ref_get__Ref_3int(t984)
        t985 = inline2067
        var t986 int
        var inline2065 int = _goml_runtime_core_string_len(expected__31)
        t986 = inline2065
        var t987 int = t985 + t986
        ref_set__Ref_3int(t983, t987)
        var t988 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        return t988
    } else {
        var t989 string
        var inline2069 string = "invalid literal"
        var inline2070 string = "" + inline2069
        var inline2071 string = inline2070 + " at byte "
        var inline2072 *ref_int_x = value__30.index
        var inline2073 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2072)
        var inline2074 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2073)
        var inline2075 string = inline2071 + inline2074
        t989 = inline2075
        var t990 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t989,
        }
        return t990
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1009 *ref_int_x = value__33.index
    var t1010 *ref_int_x = value__33.index
    var t1011 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1010)
    var t1012 int = t1011 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1009, t1012)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var vec_literal__8961 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1067 *ref_int_x = value__33.index
    var t1068 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1067)
    var t1069 string = value__33.input
    var t1070 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1069)
    var t1071 bool = t1068 < t1070
    var jp1060 bool
    if t1071 {
        var t1072 string = value__33.input
        var t1073 *ref_int_x = value__33.index
        var t1074 int
        var inline2088 int = ref_get__Ref_3int(t1073)
        t1074 = inline2088
        var t1075 uint8
        var inline2086 uint8 = _goml_runtime_core_string_byte_get(t1072, t1074)
        t1075 = inline2086
        var inline2083 uint8 = 93
        var inline2084 bool = t1075 == inline2083
        jp1060 = inline2084
    } else {
        jp1060 = false
    }
    if jp1060 {
        var t1061 *ref_int_x = value__33.index
        var t1062 *ref_int_x = value__33.index
        var t1063 int
        var inline2092 int = ref_get__Ref_3int(t1062)
        t1063 = inline2092
        var t1064 int = t1063 + 1
        ref_set__Ref_3int(t1061, t1064)
        var t1065 _goml_m_std_p_json_p_Value = Array{
            _0: vec_literal__8961,
        }
        var t1066 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1065,
        }
        return t1066
    } else {
        Loop_loop1017:
        for {
            var t1018 *ref_int_x = value__33.index
            var t1019 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1018)
            var t1020 string = value__33.input
            var t1021 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1020)
            var t1022 bool = t1019 < t1021
            if t1022 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1024 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp1024 = x51
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8961, jp1024)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1026 *ref_int_x = value__33.index
                    var t1027 int
                    var inline2134 int = ref_get__Ref_3int(t1026)
                    t1027 = inline2134
                    var t1028 string = value__33.input
                    var t1029 int
                    var inline2132 int = _goml_runtime_core_string_len(t1028)
                    t1029 = inline2132
                    var t1030 bool = t1027 >= t1029
                    if t1030 {
                        var t1031 string
                        var inline2094 string = "unterminated array"
                        var inline2095 string = "" + inline2094
                        var inline2096 string = inline2095 + " at byte "
                        var inline2097 *ref_int_x = value__33.index
                        var inline2098 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2097)
                        var inline2099 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2098)
                        var inline2100 string = inline2096 + inline2099
                        t1031 = inline2100
                        var t1032 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1031,
                        }
                        return t1032
                    } else {
                        var t1034 string = value__33.input
                        var t1035 *ref_int_x = value__33.index
                        var t1036 int
                        var inline2130 int = ref_get__Ref_3int(t1035)
                        t1036 = inline2130
                        var t1037 uint8
                        var inline2128 uint8 = _goml_runtime_core_string_byte_get(t1034, t1036)
                        t1037 = inline2128
                        var t1038 bool
                        var inline2125 uint8 = 93
                        var inline2126 bool = t1037 == inline2125
                        t1038 = inline2126
                        if t1038 {
                            var t1039 *ref_int_x = value__33.index
                            var t1040 *ref_int_x = value__33.index
                            var t1041 int
                            var inline2104 int = ref_get__Ref_3int(t1040)
                            t1041 = inline2104
                            var t1042 int = t1041 + 1
                            ref_set__Ref_3int(t1039, t1042)
                            var t1043 _goml_m_std_p_json_p_Value = Array{
                                _0: vec_literal__8961,
                            }
                            var t1044 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1043,
                            }
                            return t1044
                        } else {
                            var t1046 string = value__33.input
                            var t1047 *ref_int_x = value__33.index
                            var t1048 int
                            var inline2123 int = ref_get__Ref_3int(t1047)
                            t1048 = inline2123
                            var t1049 uint8
                            var inline2121 uint8 = _goml_runtime_core_string_byte_get(t1046, t1048)
                            t1049 = inline2121
                            var t1050 bool
                            var inline2118 uint8 = 44
                            var inline2119 bool = t1049 == inline2118
                            t1050 = inline2119
                            if t1050 {
                                var t1051 *ref_int_x = value__33.index
                                var t1052 *ref_int_x = value__33.index
                                var t1053 int
                                var inline2108 int = ref_get__Ref_3int(t1052)
                                t1053 = inline2108
                                var t1054 int = t1053 + 1
                                ref_set__Ref_3int(t1051, t1054)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1056 string
                                var inline2110 string = "expected array separator"
                                var inline2111 string = "" + inline2110
                                var inline2112 string = inline2111 + " at byte "
                                var inline2113 *ref_int_x = value__33.index
                                var inline2114 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2113)
                                var inline2115 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2114)
                                var inline2116 string = inline2112 + inline2115
                                t1056 = inline2116
                                var t1057 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1056,
                                }
                                return t1057
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t1058 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x52,
                    }
                    return t1058
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1017
            }
        }
        var t1015 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1016 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1015,
        }
        return t1016
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1079 *ref_int_x = value__36.index
    var t1080 *ref_int_x = value__36.index
    var t1081 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1080)
    var t1082 int = t1081 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1079, t1082)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var vec_literal__10180 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1162 *ref_int_x = value__36.index
    var t1163 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1162)
    var t1164 string = value__36.input
    var t1165 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1164)
    var t1166 bool = t1163 < t1165
    var jp1155 bool
    if t1166 {
        var t1167 string = value__36.input
        var t1168 *ref_int_x = value__36.index
        var t1169 int
        var inline2141 int = ref_get__Ref_3int(t1168)
        t1169 = inline2141
        var t1170 uint8
        var inline2139 uint8 = _goml_runtime_core_string_byte_get(t1167, t1169)
        t1170 = inline2139
        var inline2136 uint8 = 125
        var inline2137 bool = t1170 == inline2136
        jp1155 = inline2137
    } else {
        jp1155 = false
    }
    if jp1155 {
        var t1156 *ref_int_x = value__36.index
        var t1157 *ref_int_x = value__36.index
        var t1158 int
        var inline2145 int = ref_get__Ref_3int(t1157)
        t1158 = inline2145
        var t1159 int = t1158 + 1
        ref_set__Ref_3int(t1156, t1159)
        var t1160 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10180,
        }
        var t1161 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1160,
        }
        return t1161
    } else {
        Loop_loop1087:
        for {
            var t1088 *ref_int_x = value__36.index
            var t1089 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1088)
            var t1090 string = value__36.input
            var t1091 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1090)
            var t1092 bool = t1089 < t1091
            if t1092 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1094 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    jp1094 = x63
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1142 *ref_int_x = value__36.index
                    var t1143 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1142)
                    var t1144 string = value__36.input
                    var t1145 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1144)
                    var t1146 bool = t1143 >= t1145
                    var jp1134 bool
                    if t1146 {
                        jp1134 = true
                    } else {
                        var t1147 string = value__36.input
                        var t1148 *ref_int_x = value__36.index
                        var t1149 int
                        var inline2152 int = ref_get__Ref_3int(t1148)
                        t1149 = inline2152
                        var t1150 uint8
                        var inline2150 uint8 = _goml_runtime_core_string_byte_get(t1147, t1149)
                        t1150 = inline2150
                        var t1151 bool
                        var inline2147 uint8 = 58
                        var inline2148 bool = t1150 == inline2147
                        t1151 = inline2148
                        var t1152 bool = !t1151
                        jp1134 = t1152
                    }
                    if jp1134 {
                        var t1135 string
                        var inline2154 string = "expected object colon"
                        var inline2155 string = "" + inline2154
                        var inline2156 string = inline2155 + " at byte "
                        var inline2157 *ref_int_x = value__36.index
                        var inline2158 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2157)
                        var inline2159 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2158)
                        var inline2160 string = inline2156 + inline2159
                        t1135 = inline2160
                        var t1136 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1135,
                        }
                        return t1136
                    } else {
                        var t1137 *ref_int_x = value__36.index
                        var t1138 *ref_int_x = value__36.index
                        var t1139 int
                        var inline2164 int = ref_get__Ref_3int(t1138)
                        t1139 = inline2164
                        var t1140 int = t1139 + 1
                        ref_set__Ref_3int(t1137, t1140)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1097 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp1097 = x69
                            var t1098 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp1094,
                                _1: jp1097,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10180, t1098)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1100 *ref_int_x = value__36.index
                            var t1101 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1100)
                            var t1102 string = value__36.input
                            var t1103 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1102)
                            var t1104 bool = t1101 >= t1103
                            if t1104 {
                                var t1105 string
                                var inline2166 string = "unterminated object"
                                var inline2167 string = "" + inline2166
                                var inline2168 string = inline2167 + " at byte "
                                var inline2169 *ref_int_x = value__36.index
                                var inline2170 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2169)
                                var inline2171 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2170)
                                var inline2172 string = inline2168 + inline2171
                                t1105 = inline2172
                                var t1106 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1105,
                                }
                                return t1106
                            } else {
                                var t1108 string = value__36.input
                                var t1109 *ref_int_x = value__36.index
                                var t1110 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1109)
                                var t1111 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1108, t1110)
                                var t1112 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1111, 125)
                                if t1112 {
                                    var t1113 *ref_int_x = value__36.index
                                    var t1114 *ref_int_x = value__36.index
                                    var t1115 int
                                    var inline2176 int = ref_get__Ref_3int(t1114)
                                    t1115 = inline2176
                                    var t1116 int = t1115 + 1
                                    ref_set__Ref_3int(t1113, t1116)
                                    var t1117 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10180,
                                    }
                                    var t1118 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1117,
                                    }
                                    return t1118
                                } else {
                                    var t1120 string = value__36.input
                                    var t1121 *ref_int_x = value__36.index
                                    var t1122 int
                                    var inline2187 int = ref_get__Ref_3int(t1121)
                                    t1122 = inline2187
                                    var t1123 uint8
                                    var inline2185 uint8 = _goml_runtime_core_string_byte_get(t1120, t1122)
                                    t1123 = inline2185
                                    var t1124 bool
                                    var inline2182 uint8 = 44
                                    var inline2183 bool = t1123 == inline2182
                                    t1124 = inline2183
                                    if t1124 {
                                        var t1125 *ref_int_x = value__36.index
                                        var t1126 *ref_int_x = value__36.index
                                        var t1127 int
                                        var inline2180 int = ref_get__Ref_3int(t1126)
                                        t1127 = inline2180
                                        var t1128 int = t1127 + 1
                                        ref_set__Ref_3int(t1125, t1128)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1130 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1131 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1130,
                                        }
                                        return t1131
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t1132 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x70,
                            }
                            return t1132
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var t1153 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x64,
                    }
                    return t1153
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1087
            }
        }
        var t1085 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1086 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1085,
        }
        return t1086
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1176 *ref_int_x = value__40.index
    var t1177 int
    var inline2217 int = ref_get__Ref_3int(t1176)
    t1177 = inline2217
    var t1178 string = value__40.input
    var t1179 int
    var inline2215 int = _goml_runtime_core_string_len(t1178)
    t1179 = inline2215
    var t1180 bool = t1177 >= t1179
    if t1180 {
        var t1181 string
        var inline2189 string = "expected JSON value"
        var inline2190 string = "" + inline2189
        var inline2191 string = inline2190 + " at byte "
        var inline2192 *ref_int_x = value__40.index
        var inline2193 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2192)
        var inline2194 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2193)
        var inline2195 string = inline2191 + inline2194
        t1181 = inline2195
        var t1182 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1181,
        }
        return t1182
    } else {
        var t1183 string = value__40.input
        var t1184 *ref_int_x = value__40.index
        var t1185 int
        var inline2213 int = ref_get__Ref_3int(t1184)
        t1185 = inline2213
        var mtmp77 uint8
        var inline2211 uint8 = _goml_runtime_core_string_byte_get(t1183, t1185)
        mtmp77 = inline2211
        switch mtmp77 {
        case 123:
            var t1188 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            return t1188
        case 91:
            var t1189 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            return t1189
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var t1192 _goml_m_std_p_json_p_Value = String{
                    _0: x79,
                }
                var t1193 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1192,
                }
                return t1193
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var t1194 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x80,
                }
                return t1194
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t1195 _goml_m_std_p_json_p_Value = Bool{
                _0: true,
            }
            var t1196 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1195)
            return t1196
        case 102:
            var t1197 _goml_m_std_p_json_p_Value = Bool{
                _0: false,
            }
            var t1198 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1197)
            return t1198
        case 110:
            var t1199 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            return t1199
        default:
            var t1207 bool
            var inline2208 uint8 = 45
            var inline2209 bool = mtmp77 == inline2208
            t1207 = inline2209
            var jp1203 bool
            if t1207 {
                jp1203 = true
            } else {
                var inline2197 bool = mtmp77 >= 48
                if inline2197 {
                    var inline2198 bool = mtmp77 <= 57
                    jp1203 = inline2198
                } else {
                    jp1203 = false
                }
            }
            if jp1203 {
                var t1204 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                return t1204
            } else {
                var t1205 string
                var inline2200 string = "unexpected JSON token"
                var inline2201 string = "" + inline2200
                var inline2202 string = inline2201 + " at byte "
                var inline2203 *ref_int_x = value__40.index
                var inline2204 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2203)
                var inline2205 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2204)
                var inline2206 string = inline2202 + inline2205
                t1205 = inline2206
                var t1206 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1205,
                }
                return t1206
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__45 _goml_m_std_p_json_p_JsonParser
    var inline2233 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline2234 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__44,
        index: inline2233,
    }
    parser__45 = inline2234
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1212 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1212 = x82
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1215 *ref_int_x = parser__45.index
        var t1216 int
        var inline2231 int = ref_get__Ref_3int(t1215)
        t1216 = inline2231
        var t1217 int
        var inline2229 int = _goml_runtime_core_string_len(input__44)
        t1217 = inline2229
        var t1218 bool
        var inline2227 bool = t1216 == t1217
        t1218 = inline2227
        if t1218 {
            var t1219 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp1212,
            }
            return t1219
        } else {
            var t1220 string
            var inline2219 string = "trailing JSON data"
            var inline2220 string = "" + inline2219
            var inline2221 string = inline2220 + " at byte "
            var inline2222 *ref_int_x = parser__45.index
            var inline2223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2222)
            var inline2224 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2223)
            var inline2225 string = inline2221 + inline2224
            t1220 = inline2225
            var t1221 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1220,
            }
            return t1221
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t1222 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x83,
        }
        return t1222
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1236:
    for {
        var t1237 bool = for_index86 < for_limit87
        if t1237 {
            var for_item88 int = for_index86
            var t1238 int = for_index86 + 1
            for_index86 = t1238
            var byte__52 uint8
            var inline2295 uint8 = _goml_runtime_core_string_byte_get(value__49, for_item88)
            byte__52 = inline2295
            var t1291 bool
            var inline2292 uint8 = 34
            var inline2293 bool = byte__52 == inline2292
            t1291 = inline2293
            var jp1289 bool
            if t1291 {
                jp1289 = true
            } else {
                var inline2239 uint8 = 92
                var inline2240 bool = byte__52 == inline2239
                jp1289 = inline2240
            }
            var jp1286 bool
            if jp1289 {
                jp1286 = true
            } else {
                var inline2242 uint8 = 8
                var inline2243 bool = byte__52 == inline2242
                jp1286 = inline2243
            }
            var jp1283 bool
            if jp1286 {
                jp1283 = true
            } else {
                var inline2245 uint8 = 9
                var inline2246 bool = byte__52 == inline2245
                jp1283 = inline2246
            }
            var jp1280 bool
            if jp1283 {
                jp1280 = true
            } else {
                var inline2248 uint8 = 10
                var inline2249 bool = byte__52 == inline2248
                jp1280 = inline2249
            }
            var jp1277 bool
            if jp1280 {
                jp1277 = true
            } else {
                var inline2251 uint8 = 12
                var inline2252 bool = byte__52 == inline2251
                jp1277 = inline2252
            }
            var jp1274 bool
            if jp1277 {
                jp1274 = true
            } else {
                var inline2254 uint8 = 13
                var inline2255 bool = byte__52 == inline2254
                jp1274 = inline2255
            }
            var jp1241 bool
            if jp1274 {
                jp1241 = true
            } else {
                var t1275 bool = byte__52 < 32
                jp1241 = t1275
            }
            if jp1241 {
                var t1270 bool = start__50 < for_item88
                if t1270 {
                    var t1271 string
                    var inline2257 string = _goml_runtime_core_string_byte_slice(value__49, start__50, for_item88)
                    t1271 = inline2257
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1271)
                } else {}
                var t1245 bool
                var inline2289 uint8 = 34
                var inline2290 bool = byte__52 == inline2289
                t1245 = inline2290
                if t1245 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1248 bool
                    var inline2286 uint8 = 92
                    var inline2287 bool = byte__52 == inline2286
                    t1248 = inline2287
                    if t1248 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1251 bool
                        var inline2283 uint8 = 8
                        var inline2284 bool = byte__52 == inline2283
                        t1251 = inline2284
                        if t1251 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1254 bool
                            var inline2280 uint8 = 9
                            var inline2281 bool = byte__52 == inline2280
                            t1254 = inline2281
                            if t1254 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1257 bool
                                var inline2277 uint8 = 10
                                var inline2278 bool = byte__52 == inline2277
                                t1257 = inline2278
                                if t1257 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1260 bool
                                    var inline2274 uint8 = 12
                                    var inline2275 bool = byte__52 == inline2274
                                    t1260 = inline2275
                                    if t1260 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1263 bool
                                        var inline2271 uint8 = 13
                                        var inline2272 bool = byte__52 == inline2271
                                        t1263 = inline2272
                                        if t1263 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1265 uint8 = byte__52 / 16
                                            var t1266 rune
                                            var inline2268 int = int(uint8(t1265))
                                            var inline2269 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline2268)
                                            t1266 = inline2269
                                            var inline2265 string = _goml_m_inherent_i_char_i_char_i_to__string(t1266)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline2265)
                                            var t1267_rhs uint8 = 16
                                            var t1267 uint8 = byte__52 % t1267_rhs
                                            var t1268 rune
                                            var inline2262 int = int(uint8(t1267))
                                            var inline2263 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline2262)
                                            t1268 = inline2263
                                            var inline2259 string = _goml_m_inherent_i_char_i_char_i_to__string(t1268)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline2259)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1244 int = for_item88 + 1
                start__50 = t1244
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop1236
        }
    }
    var t1231 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1232 bool = start__50 < t1231
    if t1232 {
        var t1233 int
        var inline2299 int = _goml_runtime_core_string_len(value__49)
        t1233 = inline2299
        var t1234 string
        var inline2297 string = _goml_runtime_core_string_byte_slice(value__49, start__50, t1233)
        t1234 = inline2297
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1234)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__53 _goml_m_std_p_text_p_StringBuilder, value__54 _goml_m_std_p_json_p_Value) struct{} {
    switch value__54.(type) {
    case Object:
        var x97 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__54.(Object)._0
        var inline2313 rune = 123
        var inline2314 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2313)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2314)
        var index__56 int = 0
        var for_limit104 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97)
        var for_index105 int = 0
        Loop_loop1297:
        for {
            var t1298 bool = for_index105 < for_limit104
            if t1298 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97, for_index105)
                var t1299 int = for_index105 + 1
                for_index105 = t1299
                var t1305 bool = index__56 > 0
                if t1305 {
                    var inline2301 rune = 44
                    var inline2302 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2301)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2302)
                } else {}
                var t1301 string = for_item106._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1301)
                var inline2305 rune = 58
                var inline2306 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2305)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2306)
                var t1302 _goml_m_std_p_json_p_Value = for_item106._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1302)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1303 int = compound_old112 + compound_value113
                index__56 = t1303
                continue
            } else {
                break Loop_loop1297
            }
        }
        var inline2309 rune = 125
        var inline2310 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2309)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2310)
        return struct{}{}
    case Array:
        var x98 *_goml_vec__goml_m_std_p_json_p_Value = value__54.(Array)._0
        var inline2325 rune = 91
        var inline2326 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2325)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2326)
        var index__59 int = 0
        var for_limit118 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x98)
        var for_index119 int = 0
        Loop_loop1309:
        for {
            var t1310 bool = for_index119 < for_limit118
            if t1310 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x98, for_index119)
                var t1311 int = for_index119 + 1
                for_index119 = t1311
                var t1315 bool = index__59 > 0
                if t1315 {
                    var inline2317 rune = 44
                    var inline2318 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2317)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2318)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, for_item120)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1313 int = compound_old124 + compound_value125
                index__59 = t1313
                continue
            } else {
                break Loop_loop1309
            }
        }
        var inline2321 rune = 93
        var inline2322 string = _goml_m_inherent_i_char_i_char_i_to__string(inline2321)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline2322)
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
        var jp1320 string
        if x101 {
            jp1320 = "true"
        } else {
            jp1320 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1320)
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
    var inline2335 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline2336 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline2335,
    }
    builder__65 = inline2336
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var inline2329 *_goml_vec_uint8 = builder__65.values
    var inline2330 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(inline2329)
    var inline2332 string = inline2330._1
    return inline2332
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129)
        var for_index136 int = 0
        Loop_loop1331:
        for {
            var t1332 bool = for_index136 < for_limit135
            if t1332 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129, for_index136)
                var t1333 int = for_index136 + 1
                for_index136 = t1333
                var t1335 string = for_item137._0
                var t1336 bool
                var inline2338 bool = t1335 == name__67
                t1336 = inline2338
                if t1336 {
                    var t1337 _goml_m_std_p_json_p_Value = for_item137._1
                    var t1338 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1337,
                    }
                    return t1338
                } else {
                    continue
                }
            } else {
                break Loop_loop1331
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var t1348 int
    var inline2357 int = _goml_runtime_core_string_len(value__72)
    t1348 = inline2357
    var t1349 bool
    var inline2354 int = 0
    var inline2355 bool = t1348 == inline2354
    t1349 = inline2355
    if t1349 {
        return Option__int_None{}
    } else {
        var t1350 uint8
        var inline2351 int = 0
        var inline2352 uint8 = _goml_runtime_core_string_byte_get(value__72, inline2351)
        t1350 = inline2352
        var negative__73 bool
        var inline2348 uint8 = 45
        var inline2349 bool = t1350 == inline2348
        negative__73 = inline2349
        var jp1352 int
        if negative__73 {
            jp1352 = 1
        } else {
            jp1352 = 0
        }
        var index__74 int = jp1352
        var result__75 int = 0
        var t1373 int
        var inline2346 int = _goml_runtime_core_string_len(value__72)
        t1373 = inline2346
        var t1374 bool
        var inline2344 bool = index__74 == t1373
        t1374 = inline2344
        if t1374 {
            return Option__int_None{}
        } else {
            Loop_loop1359:
            for {
                var t1360 int
                var inline2342 int = _goml_runtime_core_string_len(value__72)
                t1360 = inline2342
                var t1361 bool = index__74 < t1360
                if t1361 {
                    var byte__76 uint8
                    var inline2340 uint8 = _goml_runtime_core_string_byte_get(value__72, index__74)
                    byte__76 = inline2340
                    var t1371 bool = byte__76 < 48
                    var jp1366 bool
                    if t1371 {
                        jp1366 = true
                    } else {
                        var t1372 bool = byte__76 > 57
                        jp1366 = t1372
                    }
                    if jp1366 {
                        return Option__int_None{}
                    } else {
                        var t1367 int = result__75 * 10
                        var t1368 uint8 = byte__76 - 48
                        var t1369 int = int(uint8(t1368))
                        var t1370 int = t1367 + t1369
                        result__75 = t1370
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1363 int = compound_old148 + compound_value149
                        index__74 = t1363
                        continue
                    }
                } else {
                    break Loop_loop1359
                }
            }
            var jp1356 int
            if negative__73 {
                var t1358 int = 0 - result__75
                jp1356 = t1358
            } else {
                jp1356 = result__75
            }
            var t1357 Option__int = Option__int_Some{
                _0: jp1356,
            }
            return t1357
        }
    }
}

func main0() struct{} {
    var mtmp155 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1395 _goml_m_std_p_json_p_Value
    switch mtmp155.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x156 _goml_m_std_p_json_p_Value = mtmp155.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1395 = x156
        var mtmp159 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1395, "name")
        switch mtmp159.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline2362 string = "missing name"
            var inline2363 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2362)
            _goml_runtime_core_string_println(inline2363)
            var mtmp164 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1395, "version")
            switch mtmp164.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline2377 string = "missing version"
                var inline2378 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2377)
                _goml_runtime_core_string_println(inline2378)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x165 _goml_m_std_p_json_p_Value = mtmp164.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp166 Option__int
                switch x165.(type) {
                case Number:
                    var inline2388 string = x165.(Number)._0
                    var inline2390 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2388)
                    mtmp166 = inline2390
                default:
                    mtmp166 = Option__int_None{}
                }
                switch mtmp166.(type) {
                case Option__int_None:
                    var inline2381 string = "invalid version"
                    var inline2382 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2381)
                    _goml_runtime_core_string_println(inline2382)
                case Option__int_Some:
                    var x167 int = mtmp166.(Option__int_Some)._0
                    var inline2385 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x167)
                    _goml_runtime_core_string_println(inline2385)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp169 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1395, "stable")
            switch mtmp169.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline2392 string = "missing stable"
                var inline2393 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2392)
                _goml_runtime_core_string_println(inline2393)
                var t1399 string = _goml_m_std_p_json_p_encode(jp1395)
                println__T_string(t1399)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x170 _goml_m_std_p_json_p_Value = mtmp169.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field2452 bool
                switch x170.(type) {
                case Bool:
                    var inline2403 bool = x170.(Bool)._0
                    commute_field2452 = inline2403
                    var inline2400 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2452)
                    _goml_runtime_core_string_println(inline2400)
                    var t1399 string = _goml_m_std_p_json_p_encode(jp1395)
                    println__T_string(t1399)
                    return struct{}{}
                default:
                    var inline2396 string = "invalid stable"
                    var inline2397 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2396)
                    _goml_runtime_core_string_println(inline2397)
                    var t1399 string = _goml_m_std_p_json_p_encode(jp1395)
                    println__T_string(t1399)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x160 _goml_m_std_p_json_p_Value = mtmp159.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field2458 string
            switch x160.(type) {
            case String:
                var inline2373 string = x160.(String)._0
                commute_field2458 = inline2373
                var inline2370 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field2458)
                _goml_runtime_core_string_println(inline2370)
                var mtmp164 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1395, "version")
                switch mtmp164.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2377 string = "missing version"
                    var inline2378 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2377)
                    _goml_runtime_core_string_println(inline2378)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x165 _goml_m_std_p_json_p_Value = mtmp164.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp166 Option__int
                    switch x165.(type) {
                    case Number:
                        var inline2388 string = x165.(Number)._0
                        var inline2390 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2388)
                        mtmp166 = inline2390
                    default:
                        mtmp166 = Option__int_None{}
                    }
                    switch mtmp166.(type) {
                    case Option__int_None:
                        var inline2381 string = "invalid version"
                        var inline2382 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2381)
                        _goml_runtime_core_string_println(inline2382)
                    case Option__int_Some:
                        var x167 int = mtmp166.(Option__int_Some)._0
                        var inline2385 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x167)
                        _goml_runtime_core_string_println(inline2385)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp169 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1395, "stable")
                switch mtmp169.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2392 string = "missing stable"
                    var inline2393 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2392)
                    _goml_runtime_core_string_println(inline2393)
                    var t1399 string = _goml_m_std_p_json_p_encode(jp1395)
                    println__T_string(t1399)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x170 _goml_m_std_p_json_p_Value = mtmp169.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field2452 bool
                    switch x170.(type) {
                    case Bool:
                        var inline2403 bool = x170.(Bool)._0
                        commute_field2452 = inline2403
                        var inline2400 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2452)
                        _goml_runtime_core_string_println(inline2400)
                        var t1399 string = _goml_m_std_p_json_p_encode(jp1395)
                        println__T_string(t1399)
                        return struct{}{}
                    default:
                        var inline2396 string = "invalid stable"
                        var inline2397 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2396)
                        _goml_runtime_core_string_println(inline2397)
                        var t1399 string = _goml_m_std_p_json_p_encode(jp1395)
                        println__T_string(t1399)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline2366 string = "invalid name"
                var inline2367 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2366)
                _goml_runtime_core_string_println(inline2367)
                var mtmp164 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1395, "version")
                switch mtmp164.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2377 string = "missing version"
                    var inline2378 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2377)
                    _goml_runtime_core_string_println(inline2378)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x165 _goml_m_std_p_json_p_Value = mtmp164.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp166 Option__int
                    switch x165.(type) {
                    case Number:
                        var inline2388 string = x165.(Number)._0
                        var inline2390 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline2388)
                        mtmp166 = inline2390
                    default:
                        mtmp166 = Option__int_None{}
                    }
                    switch mtmp166.(type) {
                    case Option__int_None:
                        var inline2381 string = "invalid version"
                        var inline2382 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2381)
                        _goml_runtime_core_string_println(inline2382)
                    case Option__int_Some:
                        var x167 int = mtmp166.(Option__int_Some)._0
                        var inline2385 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x167)
                        _goml_runtime_core_string_println(inline2385)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp169 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1395, "stable")
                switch mtmp169.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline2392 string = "missing stable"
                    var inline2393 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2392)
                    _goml_runtime_core_string_println(inline2393)
                    var t1399 string = _goml_m_std_p_json_p_encode(jp1395)
                    println__T_string(t1399)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x170 _goml_m_std_p_json_p_Value = mtmp169.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field2452 bool
                    switch x170.(type) {
                    case Bool:
                        var inline2403 bool = x170.(Bool)._0
                        commute_field2452 = inline2403
                        var inline2400 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field2452)
                        _goml_runtime_core_string_println(inline2400)
                        var t1399 string = _goml_m_std_p_json_p_encode(jp1395)
                        println__T_string(t1399)
                        return struct{}{}
                    default:
                        var inline2396 string = "invalid stable"
                        var inline2397 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2396)
                        _goml_runtime_core_string_println(inline2397)
                        var t1399 string = _goml_m_std_p_json_p_encode(jp1395)
                        println__T_string(t1399)
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
        var x157 string = mtmp155.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var inline2359 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x157)
        _goml_runtime_core_string_println(inline2359)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t1415 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t1415
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__9 string) int {
    var t1453 int = _goml_runtime_core_string_len(self__9)
    return t1453
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__12 string, index__13 int) uint8 {
    var t1456 uint8 = _goml_runtime_core_string_byte_get(self__12, index__13)
    return t1456
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__7 rune) string {
    var t1459 string = _goml_runtime_core_char_to_string(self__7)
    return t1459
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__69 uint8, other__70 uint8) bool {
    var t1462 bool = self__69 == other__70
    return t1462
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__14 string, start__15 int, end__16 int) string {
    var t1471 string = _goml_runtime_core_string_byte_slice(self__14, start__15, end__16)
    return t1471
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var t1497 *ref_int_x = ref__Ref_3int(value__207)
    return t1497
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var t1500 int = ref_get__Ref_3int(self__208)
    return t1500
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t1503 string = _goml_runtime_core_int_to_string(self__5)
    return t1503
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func char_from_uint32(value__2 uint32) Option__char {
    var mtmp0 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
    var x1 bool = mtmp0._0
    var x2 rune = mtmp0._1
    if x1 {
        var t1510 Option__char = Option__char_Some{
            _0: x2,
        }
        return t1510
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t1513 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t1513
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__126 *_goml_vec__goml_m_std_p_json_p_Value, elem__127 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t1518 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t1518
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__126 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__127 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__126, elem__127)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_get(self__10 string, index__11 int) rune {
    var t1523 rune = _goml_runtime_core_string_get(self__10, index__11)
    return t1523
}

func println__T_string(value__1 string) struct{} {
    var t1525 string
    t1525 = value__1
    _goml_runtime_core_string_println(t1525)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t1539 string = _goml_runtime_core_int_to_string(self__40)
    return t1539
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t1542 string = _goml_runtime_core_bool_to_string(self__37)
    return t1542
}

func main() {
    main0()
}
