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

type _goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value struct {
    items []Tuple2_6string_27_goml_m_std_p_serde_p_Value
}

type _goml_vec__goml_m_std_p_serde_p_Value struct {
    items []_goml_m_std_p_serde_p_Value
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

type Tuple3_4bool_3int_6string struct {
    _0 bool
    _1 int
    _2 string
}

type Tuple3_4bool_4uint_6string struct {
    _0 bool
    _1 uint
    _2 string
}

type Tuple3_4bool_7float32_6string struct {
    _0 bool
    _1 float32
    _2 string
}

type Tuple3_4bool_7float64_6string struct {
    _0 bool
    _1 float64
    _2 string
}

type Tuple2_6string_27_goml_m_std_p_serde_p_Value struct {
    _0 string
    _1 _goml_m_std_p_serde_p_Value
}

type Tuple3_6string_3int_50Vec_44Tuple2_6string_27_goml_m_std_p_serde_p_Value struct {
    _0 string
    _1 int
    _2 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
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

type _goml_m_std_p_serde_p_Value interface {
    is_goml_m_std_p_serde_p_Value()
}

type Unit struct {}

func (_ Unit) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Bool struct {
    _0 bool
}

func (_ _goml_m_std_p_serde_p_Value_Bool) is_goml_m_std_p_serde_p_Value() {}

type Int struct {
    _0 int
}

func (_ Int) is_goml_m_std_p_serde_p_Value() {}

type Int8 struct {
    _0 int8
}

func (_ Int8) is_goml_m_std_p_serde_p_Value() {}

type Int16 struct {
    _0 int16
}

func (_ Int16) is_goml_m_std_p_serde_p_Value() {}

type Int32 struct {
    _0 int32
}

func (_ Int32) is_goml_m_std_p_serde_p_Value() {}

type Int64 struct {
    _0 int64
}

func (_ Int64) is_goml_m_std_p_serde_p_Value() {}

type Uint struct {
    _0 uint
}

func (_ Uint) is_goml_m_std_p_serde_p_Value() {}

type Uint8 struct {
    _0 uint8
}

func (_ Uint8) is_goml_m_std_p_serde_p_Value() {}

type Uint16 struct {
    _0 uint16
}

func (_ Uint16) is_goml_m_std_p_serde_p_Value() {}

type Uint32 struct {
    _0 uint32
}

func (_ Uint32) is_goml_m_std_p_serde_p_Value() {}

type Uint64 struct {
    _0 uint64
}

func (_ Uint64) is_goml_m_std_p_serde_p_Value() {}

type Float32 struct {
    _0 float32
}

func (_ Float32) is_goml_m_std_p_serde_p_Value() {}

type Float64 struct {
    _0 float64
}

func (_ Float64) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Number struct {
    _0 string
}

func (_ _goml_m_std_p_serde_p_Value_Number) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_String struct {
    _0 string
}

func (_ _goml_m_std_p_serde_p_Value_String) is_goml_m_std_p_serde_p_Value() {}

type Char struct {
    _0 rune
}

func (_ Char) is_goml_m_std_p_serde_p_Value() {}

type Sequence struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
}

func (_ Sequence) is_goml_m_std_p_serde_p_Value() {}

type Struct struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
}

func (_ Struct) is_goml_m_std_p_serde_p_Value() {}

type Variant struct {
    _0 int
    _1 string
    _2 int
    _3 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
}

func (_ Variant) is_goml_m_std_p_serde_p_Value() {}

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

type _goml_m_std_p_json_p_Value_String struct {
    _0 string
}

func (_ _goml_m_std_p_json_p_Value_String) is_goml_m_std_p_json_p_Value() {}

type _goml_m_std_p_json_p_Value_Number struct {
    _0 string
}

func (_ _goml_m_std_p_json_p_Value_Number) is_goml_m_std_p_json_p_Value() {}

type _goml_m_std_p_json_p_Value_Bool struct {
    _0 bool
}

func (_ _goml_m_std_p_json_p_Value_Bool) is_goml_m_std_p_json_p_Value() {}

type Null struct {}

func (_ Null) is_goml_m_std_p_json_p_Value() {}

type Result__int__string interface {
    isResult__int__string()
}

type Result__int__string_Ok struct {
    _0 int
}

func (_ Result__int__string_Ok) isResult__int__string() {}

type Result__int__string_Err struct {
    _0 string
}

func (_ Result__int__string_Err) isResult__int__string() {}

type Result__uint__string interface {
    isResult__uint__string()
}

type Result__uint__string_Ok struct {
    _0 uint
}

func (_ Result__uint__string_Ok) isResult__uint__string() {}

type Result__uint__string_Err struct {
    _0 string
}

func (_ Result__uint__string_Err) isResult__uint__string() {}

type Result__float32__string interface {
    isResult__float32__string()
}

type Result__float32__string_Ok struct {
    _0 float32
}

func (_ Result__float32__string_Ok) isResult__float32__string() {}

type Result__float32__string_Err struct {
    _0 string
}

func (_ Result__float32__string_Err) isResult__float32__string() {}

type Result__float64__string interface {
    isResult__float64__string()
}

type Result__float64__string_Ok struct {
    _0 float64
}

func (_ Result__float64__string_Ok) isResult__float64__string() {}

type Result__float64__string_Err struct {
    _0 string
}

func (_ Result__float64__string_Err) isResult__float64__string() {}

type _goml_m_Result____std_p_serde_p_Value____string interface {
    is_goml_m_Result____std_p_serde_p_Value____string()
}

type _goml_m_Result____std_p_serde_p_Value____string_Ok struct {
    _0 _goml_m_std_p_serde_p_Value
}

func (_ _goml_m_Result____std_p_serde_p_Value____string_Ok) is_goml_m_Result____std_p_serde_p_Value____string() {}

type _goml_m_Result____std_p_serde_p_Value____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_serde_p_Value____string_Err) is_goml_m_Result____std_p_serde_p_Value____string() {}

type _goml_m_Result_____o_string_c__h4bbf09ce886e073b1c182b1babf9f12e_r__q_____string interface {
    is_goml_m_Result_____o_string__h05dd8cd8e038ae5a53bbc5c6c05aa6f2_r__q_____string()
}

type _goml_m_Result_____o_string_c__hecaffbd6b72fdeaeefff7411bf378c56_q_____string_Ok struct {
    _0 Tuple3_6string_3int_50Vec_44Tuple2_6string_27_goml_m_std_p_serde_p_Value
}

func (_ _goml_m_Result_____o_string_c__hecaffbd6b72fdeaeefff7411bf378c56_q_____string_Ok) is_goml_m_Result_____o_string__h05dd8cd8e038ae5a53bbc5c6c05aa6f2_r__q_____string() {}

type _goml_m_Result_____o_string_c__h2bc7fa489d9ad0d4700220a31aaeb507______string_Err struct {
    _0 string
}

func (_ _goml_m_Result_____o_string_c__h2bc7fa489d9ad0d4700220a31aaeb507______string_Err) is_goml_m_Result_____o_string__h05dd8cd8e038ae5a53bbc5c6c05aa6f2_r__q_____string() {}

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

type Result__bool__string interface {
    isResult__bool__string()
}

type Result__bool__string_Ok struct {
    _0 bool
}

func (_ Result__bool__string_Ok) isResult__bool__string() {}

type Result__bool__string_Err struct {
    _0 string
}

func (_ Result__bool__string_Err) isResult__bool__string() {}

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

type Result__char__string interface {
    isResult__char__string()
}

type Result__char__string_Ok struct {
    _0 rune
}

func (_ Result__char__string_Ok) isResult__char__string() {}

type Result__char__string_Err struct {
    _0 string
}

func (_ Result__char__string_Err) isResult__char__string() {}

type Result__int8__string interface {
    isResult__int8__string()
}

type Result__int8__string_Ok struct {
    _0 int8
}

func (_ Result__int8__string_Ok) isResult__int8__string() {}

type Result__int8__string_Err struct {
    _0 string
}

func (_ Result__int8__string_Err) isResult__int8__string() {}

type Result__int16__string interface {
    isResult__int16__string()
}

type Result__int16__string_Ok struct {
    _0 int16
}

func (_ Result__int16__string_Ok) isResult__int16__string() {}

type Result__int16__string_Err struct {
    _0 string
}

func (_ Result__int16__string_Err) isResult__int16__string() {}

type Result__int32__string interface {
    isResult__int32__string()
}

type Result__int32__string_Ok struct {
    _0 int32
}

func (_ Result__int32__string_Ok) isResult__int32__string() {}

type Result__int32__string_Err struct {
    _0 string
}

func (_ Result__int32__string_Err) isResult__int32__string() {}

type Result__int64__string interface {
    isResult__int64__string()
}

type Result__int64__string_Ok struct {
    _0 int64
}

func (_ Result__int64__string_Ok) isResult__int64__string() {}

type Result__int64__string_Err struct {
    _0 string
}

func (_ Result__int64__string_Err) isResult__int64__string() {}

type Result__uint8__string interface {
    isResult__uint8__string()
}

type Result__uint8__string_Ok struct {
    _0 uint8
}

func (_ Result__uint8__string_Ok) isResult__uint8__string() {}

type Result__uint8__string_Err struct {
    _0 string
}

func (_ Result__uint8__string_Err) isResult__uint8__string() {}

type Result__uint16__string interface {
    isResult__uint16__string()
}

type Result__uint16__string_Ok struct {
    _0 uint16
}

func (_ Result__uint16__string_Ok) isResult__uint16__string() {}

type Result__uint16__string_Err struct {
    _0 string
}

func (_ Result__uint16__string_Err) isResult__uint16__string() {}

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

type Result__uint64__string interface {
    isResult__uint64__string()
}

type Result__uint64__string_Ok struct {
    _0 uint64
}

func (_ Result__uint64__string_Ok) isResult__uint64__string() {}

type Result__uint64__string_Err struct {
    _0 string
}

func (_ Result__uint64__string_Err) isResult__uint64__string() {}

type Option__uint8 interface {
    isOption__uint8()
}

type Option__uint8_None struct {}

func (_ Option__uint8_None) isOption__uint8() {}

type Option__uint8_Some struct {
    _0 uint8
}

func (_ Option__uint8_Some) isOption__uint8() {}

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
    var inline2625 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline2625
    var t702 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t702
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline2640 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline2640
    var t716 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t716, length__5)
    var for_index1 int = 0
    Loop_loop718:
    for {
        var t719 bool = for_index1 < length__5
        if t719 {
            var for_item3 int = for_index1
            var t720 int = for_index1 + 1
            for_index1 = t720
            var t721 *_goml_vec_uint8 = self__3.values
            var t722 uint8
            var inline2636 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t722 = inline2636
            vec_push__Vec_5uint8(t721, t722)
            continue
        } else {
            break Loop_loop718
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t725 string
    var inline2642 string = char_to_string(value__8)
    t725 = inline2642
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t725)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var t990 string = "" + message__2
    var t991 string = t990 + " at byte "
    var t992 *ref_int_x = value__1.index
    var t993 int
    var inline2867 int = ref_get__Ref_3int(t992)
    t993 = inline2867
    var t994 string
    var inline2865 string = _goml_runtime_core_int_to_string(t993)
    t994 = inline2865
    var t995 string = t991 + t994
    return t995
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop1010:
    for {
        var t1018 *ref_int_x = value__4.index
        var t1019 int
        var inline2900 int = ref_get__Ref_3int(t1018)
        t1019 = inline2900
        var t1020 string = value__4.input
        var t1021 int
        var inline2898 int = _goml_runtime_core_string_len(t1020)
        t1021 = inline2898
        var t1022 bool = t1019 < t1021
        var jp1012 bool
        if t1022 {
            var t1023 string = value__4.input
            var t1024 *ref_int_x = value__4.index
            var t1025 int
            var inline2892 int = ref_get__Ref_3int(t1024)
            t1025 = inline2892
            var t1026 uint8
            var inline2890 uint8 = _goml_runtime_core_string_byte_get(t1023, t1025)
            t1026 = inline2890
            var inline2881 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1026, 9)
            var inline2883 bool
            if inline2881 {
                inline2883 = true
            } else {
                var inline2888 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1026, 10)
                inline2883 = inline2888
            }
            var inline2885 bool
            if inline2883 {
                inline2885 = true
            } else {
                var inline2887 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1026, 13)
                inline2885 = inline2887
            }
            if inline2885 {
                jp1012 = true
            } else {
                var inline2886 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1026, 32)
                jp1012 = inline2886
            }
        } else {
            jp1012 = false
        }
        if jp1012 {
            var t1013 *ref_int_x = value__4.index
            var t1014 *ref_int_x = value__4.index
            var t1015 int
            var inline2896 int = ref_get__Ref_3int(t1014)
            t1015 = inline2896
            var t1016 int = t1015 + 1
            ref_set__Ref_3int(t1013, t1016)
            continue
        } else {
            break Loop_loop1010
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var t1057 bool = value__5 >= 48
    var jp1033 bool
    if t1057 {
        var t1058 bool = value__5 <= 57
        jp1033 = t1058
    } else {
        jp1033 = false
    }
    if jp1033 {
        var t1034 uint8 = value__5 - 48
        var t1035 uint32 = uint32(uint8(t1034))
        var t1036 Option__uint32 = Option__uint32_Some{
            _0: t1035,
        }
        return t1036
    } else {
        var t1055 bool = value__5 >= 65
        var jp1040 bool
        if t1055 {
            var t1056 bool = value__5 <= 70
            jp1040 = t1056
        } else {
            jp1040 = false
        }
        if jp1040 {
            var t1041 uint8 = value__5 - 65
            var t1042 uint8 = t1041 + 10
            var t1043 uint32 = uint32(uint8(t1042))
            var t1044 Option__uint32 = Option__uint32_Some{
                _0: t1043,
            }
            return t1044
        } else {
            var t1053 bool = value__5 >= 97
            var jp1048 bool
            if t1053 {
                var t1054 bool = value__5 <= 102
                jp1048 = t1054
            } else {
                jp1048 = false
            }
            if jp1048 {
                var t1049 uint8 = value__5 - 97
                var t1050 uint8 = t1049 + 10
                var t1051 uint32 = uint32(uint8(t1050))
                var t1052 Option__uint32 = Option__uint32_Some{
                    _0: t1051,
                }
                return t1052
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t1063 *ref_int_x = value__6.index
    var t1064 int
    var inline2928 int = ref_get__Ref_3int(t1063)
    t1064 = inline2928
    var t1065 int = t1064 + 4
    var t1066 string = value__6.input
    var t1067 int
    var inline2926 int = _goml_runtime_core_string_len(t1066)
    t1067 = inline2926
    var t1068 bool = t1065 > t1067
    if t1068 {
        var t1069 string
        var inline2902 string = "incomplete unicode escape"
        var inline2903 string = "" + inline2902
        var inline2904 string = inline2903 + " at byte "
        var inline2905 *ref_int_x = value__6.index
        var inline2906 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2905)
        var inline2907 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2906)
        var inline2908 string = inline2904 + inline2907
        t1069 = inline2908
        var t1070 Result__uint32__string = Result__uint32__string_Err{
            _0: t1069,
        }
        return t1070
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop1077:
        for {
            var t1078 bool = for_index0 < for_limit1
            if t1078 {
                var for_item2 int = for_index0
                var t1079 int = for_index0 + 1
                for_index0 = t1079
                var t1080 string = value__6.input
                var t1081 *ref_int_x = value__6.index
                var t1082 int
                var inline2920 int = ref_get__Ref_3int(t1081)
                t1082 = inline2920
                var t1083 int = t1082 + for_item2
                var t1084 uint8
                var inline2918 uint8 = _goml_runtime_core_string_byte_get(t1080, t1083)
                t1084 = inline2918
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t1084)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t1086 string
                    var inline2910 string = "invalid unicode escape"
                    var inline2911 string = "" + inline2910
                    var inline2912 string = inline2911 + " at byte "
                    var inline2913 *ref_int_x = value__6.index
                    var inline2914 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2913)
                    var inline2915 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2914)
                    var inline2916 string = inline2912 + inline2915
                    t1086 = inline2916
                    var t1087 Result__uint32__string = Result__uint32__string_Err{
                        _0: t1086,
                    }
                    return t1087
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var t1088 uint32 = result__7 * 16
                    var t1089 uint32 = t1088 + x5
                    result__7 = t1089
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1077
            }
        }
        var t1072 *ref_int_x = value__6.index
        var t1073 *ref_int_x = value__6.index
        var t1074 int
        var inline2924 int = ref_get__Ref_3int(t1073)
        t1074 = inline2924
        var t1075 int = t1074 + 4
        ref_set__Ref_3int(t1072, t1075)
        var t1076 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        return t1076
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var commute_field3681 rune
    var inline2941 bool = utf8_valid_scalar(codepoint__12)
    if inline2941 {
        var inline2942 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__12)
        var inline2944 rune = inline2942._1
        commute_field3681 = inline2944
        var inline2938 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field3681)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__11, inline2938)
        var t1096 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t1096
    } else {
        var t1094 string
        var inline2930 string = "invalid unicode codepoint"
        var inline2931 string = "" + inline2930
        var inline2932 string = inline2931 + " at byte "
        var inline2933 *ref_int_x = value__10.index
        var inline2934 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2933)
        var inline2935 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2934)
        var inline2936 string = inline2932 + inline2935
        t1094 = inline2936
        var t1095 Result__unit__string = Result__unit__string_Err{
            _0: t1094,
        }
        return t1095
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp1100 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        jp1100 = x13
        var t1162 bool = jp1100 >= 55296
        var jp1104 bool
        if t1162 {
            var t1163 bool = jp1100 <= 56319
            jp1104 = t1163
        } else {
            jp1104 = false
        }
        if jp1104 {
            var t1141 *ref_int_x = value__14.index
            var t1142 int
            var inline2992 int = ref_get__Ref_3int(t1141)
            t1142 = inline2992
            var t1143 int = t1142 + 2
            var t1144 string = value__14.input
            var t1145 int
            var inline2990 int = _goml_runtime_core_string_len(t1144)
            t1145 = inline2990
            var t1146 bool = t1143 > t1145
            var jp1133 bool
            if t1146 {
                jp1133 = true
            } else {
                var t1147 string = value__14.input
                var t1148 *ref_int_x = value__14.index
                var t1149 int
                var inline2953 int = ref_get__Ref_3int(t1148)
                t1149 = inline2953
                var t1150 uint8
                var inline2951 uint8 = _goml_runtime_core_string_byte_get(t1147, t1149)
                t1150 = inline2951
                var t1151 bool
                var inline2948 uint8 = 92
                var inline2949 bool = t1150 == inline2948
                t1151 = inline2949
                var t1152 bool = !t1151
                jp1133 = t1152
            }
            var jp1108 bool
            if jp1133 {
                jp1108 = true
            } else {
                var t1134 string = value__14.input
                var t1135 *ref_int_x = value__14.index
                var t1136 int
                var inline2960 int = ref_get__Ref_3int(t1135)
                t1136 = inline2960
                var t1137 int = t1136 + 1
                var t1138 uint8
                var inline2958 uint8 = _goml_runtime_core_string_byte_get(t1134, t1137)
                t1138 = inline2958
                var t1139 bool
                var inline2955 uint8 = 117
                var inline2956 bool = t1138 == inline2955
                t1139 = inline2956
                var t1140 bool = !t1139
                jp1108 = t1140
            }
            if jp1108 {
                var t1109 string
                var inline2962 string = "missing low surrogate"
                var inline2963 string = "" + inline2962
                var inline2964 string = inline2963 + " at byte "
                var inline2965 *ref_int_x = value__14.index
                var inline2966 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2965)
                var inline2967 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2966)
                var inline2968 string = inline2964 + inline2967
                t1109 = inline2968
                var t1110 Result__unit__string = Result__unit__string_Err{
                    _0: t1109,
                }
                return t1110
            } else {
                var t1111 *ref_int_x = value__14.index
                var t1112 *ref_int_x = value__14.index
                var t1113 int
                var inline2988 int = ref_get__Ref_3int(t1112)
                t1113 = inline2988
                var t1114 int = t1113 + 2
                ref_set__Ref_3int(t1111, t1114)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp1116 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    jp1116 = x17
                    var t1129 bool = jp1116 < 56320
                    var jp1120 bool
                    if t1129 {
                        jp1120 = true
                    } else {
                        var t1130 bool = jp1116 > 57343
                        jp1120 = t1130
                    }
                    if jp1120 {
                        var t1121 string
                        var inline2970 string = "invalid low surrogate"
                        var inline2971 string = "" + inline2970
                        var inline2972 string = inline2971 + " at byte "
                        var inline2973 *ref_int_x = value__14.index
                        var inline2974 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline2973)
                        var inline2975 string = _goml_m_inherent_i_int_i_int_i_to__string(inline2974)
                        var inline2976 string = inline2972 + inline2975
                        t1121 = inline2976
                        var t1122 Result__unit__string = Result__unit__string_Err{
                            _0: t1121,
                        }
                        return t1122
                    } else {
                        var t1123 uint32 = jp1100 - 55296
                        var t1124 uint32 = t1123 * 1024
                        var t1125 uint32 = 65536 + t1124
                        var t1126 uint32 = t1125 + jp1116
                        var t1127 uint32 = t1126 - 56320
                        var inline2978 Option__char = char_from_uint32(t1127)
                        switch inline2978.(type) {
                        case Option__char_None:
                            var inline2979 string = _goml_m_std_p_json_p_json__error(value__14, "invalid unicode codepoint")
                            var inline2980 Result__unit__string = Result__unit__string_Err{
                                _0: inline2979,
                            }
                            return inline2980
                        case Option__char_Some:
                            var inline2981 rune = inline2978.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__15, inline2981)
                            var inline2984 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline2984
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var t1131 Result__unit__string = Result__unit__string_Err{
                        _0: x18,
                    }
                    return t1131
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t1160 bool = jp1100 >= 56320
            var jp1156 bool
            if t1160 {
                var t1161 bool = jp1100 <= 57343
                jp1156 = t1161
            } else {
                jp1156 = false
            }
            if jp1156 {
                var t1157 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t1158 Result__unit__string = Result__unit__string_Err{
                    _0: t1157,
                }
                return t1158
            } else {
                var t1159 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, jp1100)
                return t1159
            }
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var t1164 Result__unit__string = Result__unit__string_Err{
            _0: x14,
        }
        return t1164
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t1280 *ref_int_x = value__18.index
    var t1281 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1280)
    var t1282 string = value__18.input
    var t1283 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1282)
    var t1284 bool = t1281 >= t1283
    var jp1272 bool
    if t1284 {
        jp1272 = true
    } else {
        var t1285 string = value__18.input
        var t1286 *ref_int_x = value__18.index
        var t1287 int
        var inline2999 int = ref_get__Ref_3int(t1286)
        t1287 = inline2999
        var t1288 uint8
        var inline2997 uint8 = _goml_runtime_core_string_byte_get(t1285, t1287)
        t1288 = inline2997
        var t1289 bool
        var inline2994 uint8 = 34
        var inline2995 bool = t1288 == inline2994
        t1289 = inline2995
        var t1290 bool = !t1289
        jp1272 = t1290
    }
    if jp1272 {
        var t1273 string
        var inline3001 string = "expected string"
        var inline3002 string = "" + inline3001
        var inline3003 string = inline3002 + " at byte "
        var inline3004 *ref_int_x = value__18.index
        var inline3005 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3004)
        var inline3006 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3005)
        var inline3007 string = inline3003 + inline3006
        t1273 = inline3007
        var t1274 Result__string__string = Result__string__string_Err{
            _0: t1273,
        }
        return t1274
    } else {
        var t1275 *ref_int_x = value__18.index
        var t1276 *ref_int_x = value__18.index
        var t1277 int
        var inline3011 int = ref_get__Ref_3int(t1276)
        t1277 = inline3011
        var t1278 int = t1277 + 1
        ref_set__Ref_3int(t1275, t1278)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t1168 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1168)
        Loop_loop1172:
        for {
            var t1173 *ref_int_x = value__18.index
            var t1174 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1173)
            var t1175 string = value__18.input
            var t1176 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1175)
            var t1177 bool = t1174 < t1176
            if t1177 {
                var t1178 string = value__18.input
                var t1179 *ref_int_x = value__18.index
                var t1180 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1179)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1178, t1180)
                var t1182 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 34)
                if t1182 {
                    var t1190 *ref_int_x = value__18.index
                    var t1191 int
                    var inline3027 int = ref_get__Ref_3int(t1190)
                    t1191 = inline3027
                    var t1192 bool = segment__20 < t1191
                    if t1192 {
                        var t1193 string = value__18.input
                        var t1194 *ref_int_x = value__18.index
                        var t1195 int
                        var inline3015 int = ref_get__Ref_3int(t1194)
                        t1195 = inline3015
                        var t1196 string
                        var inline3013 string = string_byte_slice(t1193, segment__20, t1195)
                        t1196 = inline3013
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t1196)
                    } else {}
                    var t1184 *ref_int_x = value__18.index
                    var t1185 *ref_int_x = value__18.index
                    var t1186 int
                    var inline3025 int = ref_get__Ref_3int(t1185)
                    t1186 = inline3025
                    var t1187 int = t1186 + 1
                    ref_set__Ref_3int(t1184, t1187)
                    var t1188 string
                    var inline3017 *_goml_vec_uint8 = builder__19.values
                    var inline3018 Tuple2_4bool_6string = string_from_utf8(inline3017)
                    var inline3020 string = inline3018._1
                    t1188 = inline3020
                    var t1189 Result__string__string = Result__string__string_Ok{
                        _0: t1188,
                    }
                    return t1189
                } else {
                    var t1199 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 92)
                    if t1199 {
                        var t1254 *ref_int_x = value__18.index
                        var t1255 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1254)
                        var t1256 bool = segment__20 < t1255
                        if t1256 {
                            var t1257 string = value__18.input
                            var t1258 *ref_int_x = value__18.index
                            var t1259 int
                            var inline3031 int = ref_get__Ref_3int(t1258)
                            t1259 = inline3031
                            var t1260 string
                            var inline3029 string = string_byte_slice(t1257, segment__20, t1259)
                            t1260 = inline3029
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t1260)
                        } else {}
                        var t1201 *ref_int_x = value__18.index
                        var t1202 *ref_int_x = value__18.index
                        var t1203 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1202)
                        var t1204 int = t1203 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1201, t1204)
                        var t1247 *ref_int_x = value__18.index
                        var t1248 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1247)
                        var t1249 string = value__18.input
                        var t1250 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1249)
                        var t1251 bool = t1248 >= t1250
                        if t1251 {
                            var t1252 string
                            var inline3033 string = "incomplete escape"
                            var inline3034 string = "" + inline3033
                            var inline3035 string = inline3034 + " at byte "
                            var inline3036 *ref_int_x = value__18.index
                            var inline3037 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3036)
                            var inline3038 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3037)
                            var inline3039 string = inline3035 + inline3038
                            t1252 = inline3039
                            var t1253 Result__string__string = Result__string__string_Err{
                                _0: t1252,
                            }
                            return t1253
                        } else {
                            var t1206 string = value__18.input
                            var t1207 *ref_int_x = value__18.index
                            var t1208 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1207)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1206, t1208)
                            var t1209 *ref_int_x = value__18.index
                            var t1210 *ref_int_x = value__18.index
                            var t1211 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1210)
                            var t1212 int = t1211 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1209, t1212)
                            var t1216 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 34)
                            if t1216 {
                                var inline3041 rune = 34
                                var inline3042 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3041)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, inline3042)
                                var t1214 *ref_int_x = value__18.index
                                var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                segment__20 = t1215
                                continue
                            } else {
                                var t1219 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 92)
                                if t1219 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t1214 *ref_int_x = value__18.index
                                    var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                    segment__20 = t1215
                                    continue
                                } else {
                                    var t1222 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 47)
                                    if t1222 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t1214 *ref_int_x = value__18.index
                                        var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                        segment__20 = t1215
                                        continue
                                    } else {
                                        var t1225 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 98)
                                        if t1225 {
                                            var mtmp26 Option__char = char_from_uint32(8)
                                            switch mtmp26.(type) {
                                            case Option__char_None:
                                                var t1214 *ref_int_x = value__18.index
                                                var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                                segment__20 = t1215
                                                continue
                                            case Option__char_Some:
                                                var x27 rune = mtmp26.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x27)
                                                var t1214 *ref_int_x = value__18.index
                                                var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                                segment__20 = t1215
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t1229 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 102)
                                            if t1229 {
                                                var mtmp28 Option__char = char_from_uint32(12)
                                                switch mtmp28.(type) {
                                                case Option__char_None:
                                                    var t1214 *ref_int_x = value__18.index
                                                    var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                                    segment__20 = t1215
                                                    continue
                                                case Option__char_Some:
                                                    var x29 rune = mtmp28.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x29)
                                                    var t1214 *ref_int_x = value__18.index
                                                    var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                                    segment__20 = t1215
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t1233 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 110)
                                                if t1233 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t1214 *ref_int_x = value__18.index
                                                    var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                                    segment__20 = t1215
                                                    continue
                                                } else {
                                                    var t1236 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 114)
                                                    if t1236 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t1214 *ref_int_x = value__18.index
                                                        var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                                        segment__20 = t1215
                                                        continue
                                                    } else {
                                                        var t1239 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 116)
                                                        if t1239 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t1214 *ref_int_x = value__18.index
                                                            var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                                            segment__20 = t1215
                                                            continue
                                                        } else {
                                                            var t1242 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 117)
                                                            if t1242 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t1214 *ref_int_x = value__18.index
                                                                    var t1215 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1214)
                                                                    segment__20 = t1215
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var t1244 Result__string__string = Result__string__string_Err{
                                                                        _0: x32,
                                                                    }
                                                                    return t1244
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t1245 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t1246 Result__string__string = Result__string__string_Err{
                                                                    _0: t1245,
                                                                }
                                                                return t1246
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
                        var t1263 bool = byte__21 < 32
                        if t1263 {
                            var t1264 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t1265 Result__string__string = Result__string__string_Err{
                                _0: t1264,
                            }
                            return t1265
                        } else {
                            var t1266 *ref_int_x = value__18.index
                            var t1267 *ref_int_x = value__18.index
                            var t1268 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1267)
                            var t1269 int = t1268 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1266, t1269)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop1172
            }
        }
        var t1170 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t1171 Result__string__string = Result__string__string_Err{
            _0: t1170,
        }
        return t1171
    }
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var t1299 *ref_int_x = value__26.index
    var start__27 int
    var inline3062 int = ref_get__Ref_3int(t1299)
    start__27 = inline3062
    Loop_loop1304:
    for {
        var t1312 *ref_int_x = value__26.index
        var t1313 int
        var inline3058 int = ref_get__Ref_3int(t1312)
        t1313 = inline3058
        var t1314 string = value__26.input
        var t1315 int
        var inline3056 int = _goml_runtime_core_string_len(t1314)
        t1315 = inline3056
        var t1316 bool = t1313 < t1315
        var jp1306 bool
        if t1316 {
            var t1317 string = value__26.input
            var t1318 *ref_int_x = value__26.index
            var t1319 int
            var inline3050 int = ref_get__Ref_3int(t1318)
            t1319 = inline3050
            var t1320 uint8
            var inline3048 uint8 = _goml_runtime_core_string_byte_get(t1317, t1319)
            t1320 = inline3048
            var inline3045 bool = t1320 >= 48
            if inline3045 {
                var inline3046 bool = t1320 <= 57
                jp1306 = inline3046
            } else {
                jp1306 = false
            }
        } else {
            jp1306 = false
        }
        if jp1306 {
            var t1307 *ref_int_x = value__26.index
            var t1308 *ref_int_x = value__26.index
            var t1309 int
            var inline3054 int = ref_get__Ref_3int(t1308)
            t1309 = inline3054
            var t1310 int = t1309 + 1
            ref_set__Ref_3int(t1307, t1310)
            continue
        } else {
            break Loop_loop1304
        }
    }
    var t1301 *ref_int_x = value__26.index
    var t1302 int
    var inline3060 int = ref_get__Ref_3int(t1301)
    t1302 = inline3060
    var t1303 bool = t1302 > start__27
    return t1303
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1324 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1324)
    var t1446 string = value__28.input
    var t1447 *ref_int_x = value__28.index
    var t1448 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1447)
    var t1449 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1446, t1448)
    var t1450 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1449, 45)
    if t1450 {
        var t1451 *ref_int_x = value__28.index
        var t1452 *ref_int_x = value__28.index
        var t1453 int
        var inline3066 int = ref_get__Ref_3int(t1452)
        t1453 = inline3066
        var t1454 int = t1453 + 1
        ref_set__Ref_3int(t1451, t1454)
    } else {}
    var t1409 *ref_int_x = value__28.index
    var t1410 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1409)
    var t1411 string = value__28.input
    var t1412 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1411)
    var t1413 bool = t1410 >= t1412
    if t1413 {
        var t1414 string
        var inline3068 string = "incomplete number"
        var inline3069 string = "" + inline3068
        var inline3070 string = inline3069 + " at byte "
        var inline3071 *ref_int_x = value__28.index
        var inline3072 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3071)
        var inline3073 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3072)
        var inline3074 string = inline3070 + inline3073
        t1414 = inline3074
        var t1415 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1414,
        }
        return t1415
    } else {
        var t1417 string = value__28.input
        var t1418 *ref_int_x = value__28.index
        var t1419 int
        var inline3112 int = ref_get__Ref_3int(t1418)
        t1419 = inline3112
        var t1420 uint8
        var inline3110 uint8 = _goml_runtime_core_string_byte_get(t1417, t1419)
        t1420 = inline3110
        var t1421 bool
        var inline3107 uint8 = 48
        var inline3108 bool = t1420 == inline3107
        t1421 = inline3108
        if t1421 {
            var t1422 *ref_int_x = value__28.index
            var t1423 *ref_int_x = value__28.index
            var t1424 int
            var inline3097 int = ref_get__Ref_3int(t1423)
            t1424 = inline3097
            var t1425 int = t1424 + 1
            ref_set__Ref_3int(t1422, t1425)
            var t1431 *ref_int_x = value__28.index
            var t1432 int
            var inline3093 int = ref_get__Ref_3int(t1431)
            t1432 = inline3093
            var t1433 string = value__28.input
            var t1434 int
            var inline3091 int = _goml_runtime_core_string_len(t1433)
            t1434 = inline3091
            var t1435 bool = t1432 < t1434
            var jp1428 bool
            if t1435 {
                var t1436 string = value__28.input
                var t1437 *ref_int_x = value__28.index
                var t1438 int
                var inline3081 int = ref_get__Ref_3int(t1437)
                t1438 = inline3081
                var t1439 uint8
                var inline3079 uint8 = _goml_runtime_core_string_byte_get(t1436, t1438)
                t1439 = inline3079
                var inline3076 bool = t1439 >= 48
                if inline3076 {
                    var inline3077 bool = t1439 <= 57
                    jp1428 = inline3077
                } else {
                    jp1428 = false
                }
            } else {
                jp1428 = false
            }
            if jp1428 {
                var t1429 string
                var inline3083 string = "invalid leading zero"
                var inline3084 string = "" + inline3083
                var inline3085 string = inline3084 + " at byte "
                var inline3086 *ref_int_x = value__28.index
                var inline3087 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3086)
                var inline3088 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3087)
                var inline3089 string = inline3085 + inline3088
                t1429 = inline3089
                var t1430 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1429,
                }
                return t1430
            } else {
                var t1399 *ref_int_x = value__28.index
                var t1400 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1399)
                var t1401 string = value__28.input
                var t1402 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1401)
                var t1403 bool = t1400 < t1402
                var jp1389 bool
                if t1403 {
                    var t1404 string = value__28.input
                    var t1405 *ref_int_x = value__28.index
                    var t1406 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1405)
                    var t1407 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1404, t1406)
                    var t1408 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1407, 46)
                    jp1389 = t1408
                } else {
                    jp1389 = false
                }
                if jp1389 {
                    var t1390 *ref_int_x = value__28.index
                    var t1391 *ref_int_x = value__28.index
                    var t1392 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1391)
                    var t1393 int = t1392 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1390, t1393)
                    var t1395 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t1396 bool = !t1395
                    if t1396 {
                        var t1397 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t1398 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1397,
                        }
                        return t1398
                    } else {
                        var t1371 *ref_int_x = value__28.index
                        var t1372 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1371)
                        var t1373 string = value__28.input
                        var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1373)
                        var t1375 bool = t1372 < t1374
                        var jp1336 bool
                        if t1375 {
                            var t1378 string = value__28.input
                            var t1379 *ref_int_x = value__28.index
                            var t1380 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1379)
                            var t1381 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1378, t1380)
                            var t1382 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1381, 101)
                            if t1382 {
                                jp1336 = true
                            } else {
                                var t1383 string = value__28.input
                                var t1384 *ref_int_x = value__28.index
                                var t1385 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1384)
                                var t1386 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1383, t1385)
                                var t1387 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1386, 69)
                                jp1336 = t1387
                            }
                        } else {
                            jp1336 = false
                        }
                        if jp1336 {
                            var t1337 *ref_int_x = value__28.index
                            var t1338 *ref_int_x = value__28.index
                            var t1339 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1338)
                            var t1340 int = t1339 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1337, t1340)
                            var t1354 *ref_int_x = value__28.index
                            var t1355 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1354)
                            var t1356 string = value__28.input
                            var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1356)
                            var t1358 bool = t1355 < t1357
                            var jp1348 bool
                            if t1358 {
                                var t1361 string = value__28.input
                                var t1362 *ref_int_x = value__28.index
                                var t1363 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1362)
                                var t1364 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1361, t1363)
                                var t1365 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1364, 43)
                                if t1365 {
                                    jp1348 = true
                                } else {
                                    var t1366 string = value__28.input
                                    var t1367 *ref_int_x = value__28.index
                                    var t1368 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1367)
                                    var t1369 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1366, t1368)
                                    var t1370 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1369, 45)
                                    jp1348 = t1370
                                }
                            } else {
                                jp1348 = false
                            }
                            if jp1348 {
                                var t1349 *ref_int_x = value__28.index
                                var t1350 *ref_int_x = value__28.index
                                var t1351 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1350)
                                var t1352 int = t1351 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1349, t1352)
                            } else {}
                            var t1343 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t1344 bool = !t1343
                            if t1344 {
                                var t1345 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t1346 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1345,
                                }
                                return t1346
                            } else {
                                var t1329 string = value__28.input
                                var t1330 *ref_int_x = value__28.index
                                var t1331 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1330)
                                var t1332 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1329, start__29, t1331)
                                var t1333 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                    _0: t1332,
                                }
                                var t1334 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t1333,
                                }
                                return t1334
                            }
                        } else {
                            var t1329 string = value__28.input
                            var t1330 *ref_int_x = value__28.index
                            var t1331 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1330)
                            var t1332 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1329, start__29, t1331)
                            var t1333 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1332,
                            }
                            var t1334 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1333,
                            }
                            return t1334
                        }
                    }
                } else {
                    var t1371 *ref_int_x = value__28.index
                    var t1372 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1371)
                    var t1373 string = value__28.input
                    var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1373)
                    var t1375 bool = t1372 < t1374
                    var jp1336 bool
                    if t1375 {
                        var t1378 string = value__28.input
                        var t1379 *ref_int_x = value__28.index
                        var t1380 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1379)
                        var t1381 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1378, t1380)
                        var t1382 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1381, 101)
                        if t1382 {
                            jp1336 = true
                        } else {
                            var t1383 string = value__28.input
                            var t1384 *ref_int_x = value__28.index
                            var t1385 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1384)
                            var t1386 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1383, t1385)
                            var t1387 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1386, 69)
                            jp1336 = t1387
                        }
                    } else {
                        jp1336 = false
                    }
                    if jp1336 {
                        var t1337 *ref_int_x = value__28.index
                        var t1338 *ref_int_x = value__28.index
                        var t1339 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1338)
                        var t1340 int = t1339 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1337, t1340)
                        var t1354 *ref_int_x = value__28.index
                        var t1355 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1354)
                        var t1356 string = value__28.input
                        var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1356)
                        var t1358 bool = t1355 < t1357
                        var jp1348 bool
                        if t1358 {
                            var t1361 string = value__28.input
                            var t1362 *ref_int_x = value__28.index
                            var t1363 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1362)
                            var t1364 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1361, t1363)
                            var t1365 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1364, 43)
                            if t1365 {
                                jp1348 = true
                            } else {
                                var t1366 string = value__28.input
                                var t1367 *ref_int_x = value__28.index
                                var t1368 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1367)
                                var t1369 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1366, t1368)
                                var t1370 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1369, 45)
                                jp1348 = t1370
                            }
                        } else {
                            jp1348 = false
                        }
                        if jp1348 {
                            var t1349 *ref_int_x = value__28.index
                            var t1350 *ref_int_x = value__28.index
                            var t1351 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1350)
                            var t1352 int = t1351 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1349, t1352)
                        } else {}
                        var t1343 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t1344 bool = !t1343
                        if t1344 {
                            var t1345 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t1346 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t1345,
                            }
                            return t1346
                        } else {
                            var t1329 string = value__28.input
                            var t1330 *ref_int_x = value__28.index
                            var t1331 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1330)
                            var t1332 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1329, start__29, t1331)
                            var t1333 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1332,
                            }
                            var t1334 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1333,
                            }
                            return t1334
                        }
                    } else {
                        var t1329 string = value__28.input
                        var t1330 *ref_int_x = value__28.index
                        var t1331 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1330)
                        var t1332 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1329, start__29, t1331)
                        var t1333 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                            _0: t1332,
                        }
                        var t1334 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t1333,
                        }
                        return t1334
                    }
                }
            }
        } else {
            var t1442 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t1443 bool = !t1442
            if t1443 {
                var t1444 string
                var inline3099 string = "expected number"
                var inline3100 string = "" + inline3099
                var inline3101 string = inline3100 + " at byte "
                var inline3102 *ref_int_x = value__28.index
                var inline3103 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3102)
                var inline3104 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3103)
                var inline3105 string = inline3101 + inline3104
                t1444 = inline3105
                var t1445 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1444,
                }
                return t1445
            } else {
                var t1399 *ref_int_x = value__28.index
                var t1400 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1399)
                var t1401 string = value__28.input
                var t1402 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1401)
                var t1403 bool = t1400 < t1402
                var jp1389 bool
                if t1403 {
                    var t1404 string = value__28.input
                    var t1405 *ref_int_x = value__28.index
                    var t1406 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1405)
                    var t1407 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1404, t1406)
                    var t1408 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1407, 46)
                    jp1389 = t1408
                } else {
                    jp1389 = false
                }
                if jp1389 {
                    var t1390 *ref_int_x = value__28.index
                    var t1391 *ref_int_x = value__28.index
                    var t1392 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1391)
                    var t1393 int = t1392 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1390, t1393)
                    var t1395 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t1396 bool = !t1395
                    if t1396 {
                        var t1397 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t1398 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1397,
                        }
                        return t1398
                    } else {
                        var t1371 *ref_int_x = value__28.index
                        var t1372 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1371)
                        var t1373 string = value__28.input
                        var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1373)
                        var t1375 bool = t1372 < t1374
                        var jp1336 bool
                        if t1375 {
                            var t1378 string = value__28.input
                            var t1379 *ref_int_x = value__28.index
                            var t1380 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1379)
                            var t1381 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1378, t1380)
                            var t1382 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1381, 101)
                            if t1382 {
                                jp1336 = true
                            } else {
                                var t1383 string = value__28.input
                                var t1384 *ref_int_x = value__28.index
                                var t1385 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1384)
                                var t1386 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1383, t1385)
                                var t1387 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1386, 69)
                                jp1336 = t1387
                            }
                        } else {
                            jp1336 = false
                        }
                        if jp1336 {
                            var t1337 *ref_int_x = value__28.index
                            var t1338 *ref_int_x = value__28.index
                            var t1339 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1338)
                            var t1340 int = t1339 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1337, t1340)
                            var t1354 *ref_int_x = value__28.index
                            var t1355 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1354)
                            var t1356 string = value__28.input
                            var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1356)
                            var t1358 bool = t1355 < t1357
                            var jp1348 bool
                            if t1358 {
                                var t1361 string = value__28.input
                                var t1362 *ref_int_x = value__28.index
                                var t1363 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1362)
                                var t1364 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1361, t1363)
                                var t1365 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1364, 43)
                                if t1365 {
                                    jp1348 = true
                                } else {
                                    var t1366 string = value__28.input
                                    var t1367 *ref_int_x = value__28.index
                                    var t1368 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1367)
                                    var t1369 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1366, t1368)
                                    var t1370 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1369, 45)
                                    jp1348 = t1370
                                }
                            } else {
                                jp1348 = false
                            }
                            if jp1348 {
                                var t1349 *ref_int_x = value__28.index
                                var t1350 *ref_int_x = value__28.index
                                var t1351 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1350)
                                var t1352 int = t1351 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1349, t1352)
                            } else {}
                            var t1343 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t1344 bool = !t1343
                            if t1344 {
                                var t1345 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t1346 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1345,
                                }
                                return t1346
                            } else {
                                var t1329 string = value__28.input
                                var t1330 *ref_int_x = value__28.index
                                var t1331 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1330)
                                var t1332 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1329, start__29, t1331)
                                var t1333 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                    _0: t1332,
                                }
                                var t1334 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t1333,
                                }
                                return t1334
                            }
                        } else {
                            var t1329 string = value__28.input
                            var t1330 *ref_int_x = value__28.index
                            var t1331 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1330)
                            var t1332 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1329, start__29, t1331)
                            var t1333 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1332,
                            }
                            var t1334 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1333,
                            }
                            return t1334
                        }
                    }
                } else {
                    var t1371 *ref_int_x = value__28.index
                    var t1372 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1371)
                    var t1373 string = value__28.input
                    var t1374 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1373)
                    var t1375 bool = t1372 < t1374
                    var jp1336 bool
                    if t1375 {
                        var t1378 string = value__28.input
                        var t1379 *ref_int_x = value__28.index
                        var t1380 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1379)
                        var t1381 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1378, t1380)
                        var t1382 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1381, 101)
                        if t1382 {
                            jp1336 = true
                        } else {
                            var t1383 string = value__28.input
                            var t1384 *ref_int_x = value__28.index
                            var t1385 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1384)
                            var t1386 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1383, t1385)
                            var t1387 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1386, 69)
                            jp1336 = t1387
                        }
                    } else {
                        jp1336 = false
                    }
                    if jp1336 {
                        var t1337 *ref_int_x = value__28.index
                        var t1338 *ref_int_x = value__28.index
                        var t1339 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1338)
                        var t1340 int = t1339 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1337, t1340)
                        var t1354 *ref_int_x = value__28.index
                        var t1355 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1354)
                        var t1356 string = value__28.input
                        var t1357 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1356)
                        var t1358 bool = t1355 < t1357
                        var jp1348 bool
                        if t1358 {
                            var t1361 string = value__28.input
                            var t1362 *ref_int_x = value__28.index
                            var t1363 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1362)
                            var t1364 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1361, t1363)
                            var t1365 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1364, 43)
                            if t1365 {
                                jp1348 = true
                            } else {
                                var t1366 string = value__28.input
                                var t1367 *ref_int_x = value__28.index
                                var t1368 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1367)
                                var t1369 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1366, t1368)
                                var t1370 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1369, 45)
                                jp1348 = t1370
                            }
                        } else {
                            jp1348 = false
                        }
                        if jp1348 {
                            var t1349 *ref_int_x = value__28.index
                            var t1350 *ref_int_x = value__28.index
                            var t1351 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1350)
                            var t1352 int = t1351 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1349, t1352)
                        } else {}
                        var t1343 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t1344 bool = !t1343
                        if t1344 {
                            var t1345 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t1346 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t1345,
                            }
                            return t1346
                        } else {
                            var t1329 string = value__28.input
                            var t1330 *ref_int_x = value__28.index
                            var t1331 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1330)
                            var t1332 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1329, start__29, t1331)
                            var t1333 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1332,
                            }
                            var t1334 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1333,
                            }
                            return t1334
                        }
                    } else {
                        var t1329 string = value__28.input
                        var t1330 *ref_int_x = value__28.index
                        var t1331 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1330)
                        var t1332 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1329, start__29, t1331)
                        var t1333 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                            _0: t1332,
                        }
                        var t1334 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t1333,
                        }
                        return t1334
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t1470 *ref_int_x = value__30.index
    var t1471 int
    var inline3142 int = ref_get__Ref_3int(t1470)
    t1471 = inline3142
    var t1472 int
    var inline3140 int = _goml_runtime_core_string_len(expected__31)
    t1472 = inline3140
    var t1473 int = t1471 + t1472
    var t1474 string = value__30.input
    var t1475 int
    var inline3138 int = _goml_runtime_core_string_len(t1474)
    t1475 = inline3138
    var t1476 bool = t1473 <= t1475
    var jp1461 bool
    if t1476 {
        var t1477 string = value__30.input
        var t1478 *ref_int_x = value__30.index
        var t1479 int
        var inline3122 int = ref_get__Ref_3int(t1478)
        t1479 = inline3122
        var t1480 *ref_int_x = value__30.index
        var t1481 int
        var inline3120 int = ref_get__Ref_3int(t1480)
        t1481 = inline3120
        var t1482 int
        var inline3118 int = _goml_runtime_core_string_len(expected__31)
        t1482 = inline3118
        var t1483 int = t1481 + t1482
        var t1484 string
        var inline3116 string = string_byte_slice(t1477, t1479, t1483)
        t1484 = inline3116
        var inline3114 bool = t1484 == expected__31
        jp1461 = inline3114
    } else {
        jp1461 = false
    }
    if jp1461 {
        var t1462 *ref_int_x = value__30.index
        var t1463 *ref_int_x = value__30.index
        var t1464 int
        var inline3128 int = ref_get__Ref_3int(t1463)
        t1464 = inline3128
        var t1465 int
        var inline3126 int = _goml_runtime_core_string_len(expected__31)
        t1465 = inline3126
        var t1466 int = t1464 + t1465
        ref_set__Ref_3int(t1462, t1466)
        var t1467 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        return t1467
    } else {
        var t1468 string
        var inline3130 string = "invalid literal"
        var inline3131 string = "" + inline3130
        var inline3132 string = inline3131 + " at byte "
        var inline3133 *ref_int_x = value__30.index
        var inline3134 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3133)
        var inline3135 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3134)
        var inline3136 string = inline3132 + inline3135
        t1468 = inline3136
        var t1469 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1468,
        }
        return t1469
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1488 *ref_int_x = value__33.index
    var t1489 *ref_int_x = value__33.index
    var t1490 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1489)
    var t1491 int = t1490 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1488, t1491)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var vec_literal__8702 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1546 *ref_int_x = value__33.index
    var t1547 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1546)
    var t1548 string = value__33.input
    var t1549 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1548)
    var t1550 bool = t1547 < t1549
    var jp1539 bool
    if t1550 {
        var t1551 string = value__33.input
        var t1552 *ref_int_x = value__33.index
        var t1553 int
        var inline3149 int = ref_get__Ref_3int(t1552)
        t1553 = inline3149
        var t1554 uint8
        var inline3147 uint8 = _goml_runtime_core_string_byte_get(t1551, t1553)
        t1554 = inline3147
        var inline3144 uint8 = 93
        var inline3145 bool = t1554 == inline3144
        jp1539 = inline3145
    } else {
        jp1539 = false
    }
    if jp1539 {
        var t1540 *ref_int_x = value__33.index
        var t1541 *ref_int_x = value__33.index
        var t1542 int
        var inline3153 int = ref_get__Ref_3int(t1541)
        t1542 = inline3153
        var t1543 int = t1542 + 1
        ref_set__Ref_3int(t1540, t1543)
        var t1544 _goml_m_std_p_json_p_Value = Array{
            _0: vec_literal__8702,
        }
        var t1545 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1544,
        }
        return t1545
    } else {
        Loop_loop1496:
        for {
            var t1497 *ref_int_x = value__33.index
            var t1498 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1497)
            var t1499 string = value__33.input
            var t1500 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1499)
            var t1501 bool = t1498 < t1500
            if t1501 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1503 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp1503 = x51
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8702, jp1503)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1505 *ref_int_x = value__33.index
                    var t1506 int
                    var inline3195 int = ref_get__Ref_3int(t1505)
                    t1506 = inline3195
                    var t1507 string = value__33.input
                    var t1508 int
                    var inline3193 int = _goml_runtime_core_string_len(t1507)
                    t1508 = inline3193
                    var t1509 bool = t1506 >= t1508
                    if t1509 {
                        var t1510 string
                        var inline3155 string = "unterminated array"
                        var inline3156 string = "" + inline3155
                        var inline3157 string = inline3156 + " at byte "
                        var inline3158 *ref_int_x = value__33.index
                        var inline3159 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3158)
                        var inline3160 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3159)
                        var inline3161 string = inline3157 + inline3160
                        t1510 = inline3161
                        var t1511 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1510,
                        }
                        return t1511
                    } else {
                        var t1513 string = value__33.input
                        var t1514 *ref_int_x = value__33.index
                        var t1515 int
                        var inline3191 int = ref_get__Ref_3int(t1514)
                        t1515 = inline3191
                        var t1516 uint8
                        var inline3189 uint8 = _goml_runtime_core_string_byte_get(t1513, t1515)
                        t1516 = inline3189
                        var t1517 bool
                        var inline3186 uint8 = 93
                        var inline3187 bool = t1516 == inline3186
                        t1517 = inline3187
                        if t1517 {
                            var t1518 *ref_int_x = value__33.index
                            var t1519 *ref_int_x = value__33.index
                            var t1520 int
                            var inline3165 int = ref_get__Ref_3int(t1519)
                            t1520 = inline3165
                            var t1521 int = t1520 + 1
                            ref_set__Ref_3int(t1518, t1521)
                            var t1522 _goml_m_std_p_json_p_Value = Array{
                                _0: vec_literal__8702,
                            }
                            var t1523 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1522,
                            }
                            return t1523
                        } else {
                            var t1525 string = value__33.input
                            var t1526 *ref_int_x = value__33.index
                            var t1527 int
                            var inline3184 int = ref_get__Ref_3int(t1526)
                            t1527 = inline3184
                            var t1528 uint8
                            var inline3182 uint8 = _goml_runtime_core_string_byte_get(t1525, t1527)
                            t1528 = inline3182
                            var t1529 bool
                            var inline3179 uint8 = 44
                            var inline3180 bool = t1528 == inline3179
                            t1529 = inline3180
                            if t1529 {
                                var t1530 *ref_int_x = value__33.index
                                var t1531 *ref_int_x = value__33.index
                                var t1532 int
                                var inline3169 int = ref_get__Ref_3int(t1531)
                                t1532 = inline3169
                                var t1533 int = t1532 + 1
                                ref_set__Ref_3int(t1530, t1533)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1535 string
                                var inline3171 string = "expected array separator"
                                var inline3172 string = "" + inline3171
                                var inline3173 string = inline3172 + " at byte "
                                var inline3174 *ref_int_x = value__33.index
                                var inline3175 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3174)
                                var inline3176 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3175)
                                var inline3177 string = inline3173 + inline3176
                                t1535 = inline3177
                                var t1536 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1535,
                                }
                                return t1536
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t1537 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x52,
                    }
                    return t1537
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1496
            }
        }
        var t1494 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1495 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1494,
        }
        return t1495
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1558 *ref_int_x = value__36.index
    var t1559 *ref_int_x = value__36.index
    var t1560 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1559)
    var t1561 int = t1560 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1558, t1561)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var vec_literal__9904 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1641 *ref_int_x = value__36.index
    var t1642 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1641)
    var t1643 string = value__36.input
    var t1644 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1643)
    var t1645 bool = t1642 < t1644
    var jp1634 bool
    if t1645 {
        var t1646 string = value__36.input
        var t1647 *ref_int_x = value__36.index
        var t1648 int
        var inline3202 int = ref_get__Ref_3int(t1647)
        t1648 = inline3202
        var t1649 uint8
        var inline3200 uint8 = _goml_runtime_core_string_byte_get(t1646, t1648)
        t1649 = inline3200
        var inline3197 uint8 = 125
        var inline3198 bool = t1649 == inline3197
        jp1634 = inline3198
    } else {
        jp1634 = false
    }
    if jp1634 {
        var t1635 *ref_int_x = value__36.index
        var t1636 *ref_int_x = value__36.index
        var t1637 int
        var inline3206 int = ref_get__Ref_3int(t1636)
        t1637 = inline3206
        var t1638 int = t1637 + 1
        ref_set__Ref_3int(t1635, t1638)
        var t1639 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__9904,
        }
        var t1640 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1639,
        }
        return t1640
    } else {
        Loop_loop1566:
        for {
            var t1567 *ref_int_x = value__36.index
            var t1568 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1567)
            var t1569 string = value__36.input
            var t1570 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1569)
            var t1571 bool = t1568 < t1570
            if t1571 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1573 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    jp1573 = x63
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1621 *ref_int_x = value__36.index
                    var t1622 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1621)
                    var t1623 string = value__36.input
                    var t1624 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1623)
                    var t1625 bool = t1622 >= t1624
                    var jp1613 bool
                    if t1625 {
                        jp1613 = true
                    } else {
                        var t1626 string = value__36.input
                        var t1627 *ref_int_x = value__36.index
                        var t1628 int
                        var inline3213 int = ref_get__Ref_3int(t1627)
                        t1628 = inline3213
                        var t1629 uint8
                        var inline3211 uint8 = _goml_runtime_core_string_byte_get(t1626, t1628)
                        t1629 = inline3211
                        var t1630 bool
                        var inline3208 uint8 = 58
                        var inline3209 bool = t1629 == inline3208
                        t1630 = inline3209
                        var t1631 bool = !t1630
                        jp1613 = t1631
                    }
                    if jp1613 {
                        var t1614 string
                        var inline3215 string = "expected object colon"
                        var inline3216 string = "" + inline3215
                        var inline3217 string = inline3216 + " at byte "
                        var inline3218 *ref_int_x = value__36.index
                        var inline3219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3218)
                        var inline3220 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3219)
                        var inline3221 string = inline3217 + inline3220
                        t1614 = inline3221
                        var t1615 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1614,
                        }
                        return t1615
                    } else {
                        var t1616 *ref_int_x = value__36.index
                        var t1617 *ref_int_x = value__36.index
                        var t1618 int
                        var inline3225 int = ref_get__Ref_3int(t1617)
                        t1618 = inline3225
                        var t1619 int = t1618 + 1
                        ref_set__Ref_3int(t1616, t1619)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1576 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp1576 = x69
                            var t1577 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp1573,
                                _1: jp1576,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__9904, t1577)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1579 *ref_int_x = value__36.index
                            var t1580 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1579)
                            var t1581 string = value__36.input
                            var t1582 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1581)
                            var t1583 bool = t1580 >= t1582
                            if t1583 {
                                var t1584 string
                                var inline3227 string = "unterminated object"
                                var inline3228 string = "" + inline3227
                                var inline3229 string = inline3228 + " at byte "
                                var inline3230 *ref_int_x = value__36.index
                                var inline3231 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3230)
                                var inline3232 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3231)
                                var inline3233 string = inline3229 + inline3232
                                t1584 = inline3233
                                var t1585 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1584,
                                }
                                return t1585
                            } else {
                                var t1587 string = value__36.input
                                var t1588 *ref_int_x = value__36.index
                                var t1589 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1588)
                                var t1590 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1587, t1589)
                                var t1591 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1590, 125)
                                if t1591 {
                                    var t1592 *ref_int_x = value__36.index
                                    var t1593 *ref_int_x = value__36.index
                                    var t1594 int
                                    var inline3237 int = ref_get__Ref_3int(t1593)
                                    t1594 = inline3237
                                    var t1595 int = t1594 + 1
                                    ref_set__Ref_3int(t1592, t1595)
                                    var t1596 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__9904,
                                    }
                                    var t1597 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1596,
                                    }
                                    return t1597
                                } else {
                                    var t1599 string = value__36.input
                                    var t1600 *ref_int_x = value__36.index
                                    var t1601 int
                                    var inline3248 int = ref_get__Ref_3int(t1600)
                                    t1601 = inline3248
                                    var t1602 uint8
                                    var inline3246 uint8 = _goml_runtime_core_string_byte_get(t1599, t1601)
                                    t1602 = inline3246
                                    var t1603 bool
                                    var inline3243 uint8 = 44
                                    var inline3244 bool = t1602 == inline3243
                                    t1603 = inline3244
                                    if t1603 {
                                        var t1604 *ref_int_x = value__36.index
                                        var t1605 *ref_int_x = value__36.index
                                        var t1606 int
                                        var inline3241 int = ref_get__Ref_3int(t1605)
                                        t1606 = inline3241
                                        var t1607 int = t1606 + 1
                                        ref_set__Ref_3int(t1604, t1607)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1609 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1610 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1609,
                                        }
                                        return t1610
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t1611 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x70,
                            }
                            return t1611
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var t1632 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x64,
                    }
                    return t1632
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1566
            }
        }
        var t1564 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1565 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1564,
        }
        return t1565
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1655 *ref_int_x = value__40.index
    var t1656 int
    var inline3278 int = ref_get__Ref_3int(t1655)
    t1656 = inline3278
    var t1657 string = value__40.input
    var t1658 int
    var inline3276 int = _goml_runtime_core_string_len(t1657)
    t1658 = inline3276
    var t1659 bool = t1656 >= t1658
    if t1659 {
        var t1660 string
        var inline3250 string = "expected JSON value"
        var inline3251 string = "" + inline3250
        var inline3252 string = inline3251 + " at byte "
        var inline3253 *ref_int_x = value__40.index
        var inline3254 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3253)
        var inline3255 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3254)
        var inline3256 string = inline3252 + inline3255
        t1660 = inline3256
        var t1661 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1660,
        }
        return t1661
    } else {
        var t1662 string = value__40.input
        var t1663 *ref_int_x = value__40.index
        var t1664 int
        var inline3274 int = ref_get__Ref_3int(t1663)
        t1664 = inline3274
        var mtmp77 uint8
        var inline3272 uint8 = _goml_runtime_core_string_byte_get(t1662, t1664)
        mtmp77 = inline3272
        switch mtmp77 {
        case 123:
            var t1667 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            return t1667
        case 91:
            var t1668 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            return t1668
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var t1671 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x79,
                }
                var t1672 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1671,
                }
                return t1672
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var t1673 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x80,
                }
                return t1673
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t1674 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t1675 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1674)
            return t1675
        case 102:
            var t1676 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t1677 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1676)
            return t1677
        case 110:
            var t1678 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            return t1678
        default:
            var t1686 bool
            var inline3269 uint8 = 45
            var inline3270 bool = mtmp77 == inline3269
            t1686 = inline3270
            var jp1682 bool
            if t1686 {
                jp1682 = true
            } else {
                var inline3258 bool = mtmp77 >= 48
                if inline3258 {
                    var inline3259 bool = mtmp77 <= 57
                    jp1682 = inline3259
                } else {
                    jp1682 = false
                }
            }
            if jp1682 {
                var t1683 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                return t1683
            } else {
                var t1684 string
                var inline3261 string = "unexpected JSON token"
                var inline3262 string = "" + inline3261
                var inline3263 string = inline3262 + " at byte "
                var inline3264 *ref_int_x = value__40.index
                var inline3265 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3264)
                var inline3266 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3265)
                var inline3267 string = inline3263 + inline3266
                t1684 = inline3267
                var t1685 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1684,
                }
                return t1685
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__45 _goml_m_std_p_json_p_JsonParser
    var inline3294 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline3295 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__44,
        index: inline3294,
    }
    parser__45 = inline3295
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1691 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1691 = x82
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1694 *ref_int_x = parser__45.index
        var t1695 int
        var inline3292 int = ref_get__Ref_3int(t1694)
        t1695 = inline3292
        var t1696 int
        var inline3290 int = _goml_runtime_core_string_len(input__44)
        t1696 = inline3290
        var t1697 bool
        var inline3288 bool = t1695 == t1696
        t1697 = inline3288
        if t1697 {
            var t1698 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp1691,
            }
            return t1698
        } else {
            var t1699 string
            var inline3280 string = "trailing JSON data"
            var inline3281 string = "" + inline3280
            var inline3282 string = inline3281 + " at byte "
            var inline3283 *ref_int_x = parser__45.index
            var inline3284 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3283)
            var inline3285 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3284)
            var inline3286 string = inline3282 + inline3285
            t1699 = inline3286
            var t1700 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1699,
            }
            return t1700
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t1701 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x83,
        }
        return t1701
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1715:
    for {
        var t1716 bool = for_index86 < for_limit87
        if t1716 {
            var for_item88 int = for_index86
            var t1717 int = for_index86 + 1
            for_index86 = t1717
            var byte__52 uint8
            var inline3356 uint8 = _goml_runtime_core_string_byte_get(value__49, for_item88)
            byte__52 = inline3356
            var t1770 bool
            var inline3353 uint8 = 34
            var inline3354 bool = byte__52 == inline3353
            t1770 = inline3354
            var jp1768 bool
            if t1770 {
                jp1768 = true
            } else {
                var inline3300 uint8 = 92
                var inline3301 bool = byte__52 == inline3300
                jp1768 = inline3301
            }
            var jp1765 bool
            if jp1768 {
                jp1765 = true
            } else {
                var inline3303 uint8 = 8
                var inline3304 bool = byte__52 == inline3303
                jp1765 = inline3304
            }
            var jp1762 bool
            if jp1765 {
                jp1762 = true
            } else {
                var inline3306 uint8 = 9
                var inline3307 bool = byte__52 == inline3306
                jp1762 = inline3307
            }
            var jp1759 bool
            if jp1762 {
                jp1759 = true
            } else {
                var inline3309 uint8 = 10
                var inline3310 bool = byte__52 == inline3309
                jp1759 = inline3310
            }
            var jp1756 bool
            if jp1759 {
                jp1756 = true
            } else {
                var inline3312 uint8 = 12
                var inline3313 bool = byte__52 == inline3312
                jp1756 = inline3313
            }
            var jp1753 bool
            if jp1756 {
                jp1753 = true
            } else {
                var inline3315 uint8 = 13
                var inline3316 bool = byte__52 == inline3315
                jp1753 = inline3316
            }
            var jp1720 bool
            if jp1753 {
                jp1720 = true
            } else {
                var t1754 bool = byte__52 < 32
                jp1720 = t1754
            }
            if jp1720 {
                var t1749 bool = start__50 < for_item88
                if t1749 {
                    var t1750 string
                    var inline3318 string = string_byte_slice(value__49, start__50, for_item88)
                    t1750 = inline3318
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1750)
                } else {}
                var t1724 bool
                var inline3350 uint8 = 34
                var inline3351 bool = byte__52 == inline3350
                t1724 = inline3351
                if t1724 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1727 bool
                    var inline3347 uint8 = 92
                    var inline3348 bool = byte__52 == inline3347
                    t1727 = inline3348
                    if t1727 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1730 bool
                        var inline3344 uint8 = 8
                        var inline3345 bool = byte__52 == inline3344
                        t1730 = inline3345
                        if t1730 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1733 bool
                            var inline3341 uint8 = 9
                            var inline3342 bool = byte__52 == inline3341
                            t1733 = inline3342
                            if t1733 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1736 bool
                                var inline3338 uint8 = 10
                                var inline3339 bool = byte__52 == inline3338
                                t1736 = inline3339
                                if t1736 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1739 bool
                                    var inline3335 uint8 = 12
                                    var inline3336 bool = byte__52 == inline3335
                                    t1739 = inline3336
                                    if t1739 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1742 bool
                                        var inline3332 uint8 = 13
                                        var inline3333 bool = byte__52 == inline3332
                                        t1742 = inline3333
                                        if t1742 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1744 uint8 = byte__52 / 16
                                            var t1745 rune
                                            var inline3329 int = int(uint8(t1744))
                                            var inline3330 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline3329)
                                            t1745 = inline3330
                                            var inline3326 string = _goml_m_inherent_i_char_i_char_i_to__string(t1745)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline3326)
                                            var t1746_rhs uint8 = 16
                                            var t1746 uint8 = byte__52 % t1746_rhs
                                            var t1747 rune
                                            var inline3323 int = int(uint8(t1746))
                                            var inline3324 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline3323)
                                            t1747 = inline3324
                                            var inline3320 string = _goml_m_inherent_i_char_i_char_i_to__string(t1747)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline3320)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1723 int = for_item88 + 1
                start__50 = t1723
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop1715
        }
    }
    var t1710 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1711 bool = start__50 < t1710
    if t1711 {
        var t1712 int
        var inline3360 int = _goml_runtime_core_string_len(value__49)
        t1712 = inline3360
        var t1713 string
        var inline3358 string = string_byte_slice(value__49, start__50, t1712)
        t1713 = inline3358
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1713)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__53 _goml_m_std_p_text_p_StringBuilder, value__54 _goml_m_std_p_json_p_Value) struct{} {
    switch value__54.(type) {
    case Object:
        var x97 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__54.(Object)._0
        var inline3374 rune = 123
        var inline3375 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3374)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3375)
        var index__56 int = 0
        var for_limit104 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97)
        var for_index105 int = 0
        Loop_loop1776:
        for {
            var t1777 bool = for_index105 < for_limit104
            if t1777 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97, for_index105)
                var t1778 int = for_index105 + 1
                for_index105 = t1778
                var t1784 bool = index__56 > 0
                if t1784 {
                    var inline3362 rune = 44
                    var inline3363 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3362)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3363)
                } else {}
                var t1780 string = for_item106._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1780)
                var inline3366 rune = 58
                var inline3367 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3366)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3367)
                var t1781 _goml_m_std_p_json_p_Value = for_item106._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1781)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1782 int = compound_old112 + compound_value113
                index__56 = t1782
                continue
            } else {
                break Loop_loop1776
            }
        }
        var inline3370 rune = 125
        var inline3371 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3370)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3371)
        return struct{}{}
    case Array:
        var x98 *_goml_vec__goml_m_std_p_json_p_Value = value__54.(Array)._0
        var inline3386 rune = 91
        var inline3387 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3386)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3387)
        var index__59 int = 0
        var for_limit118 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x98)
        var for_index119 int = 0
        Loop_loop1788:
        for {
            var t1789 bool = for_index119 < for_limit118
            if t1789 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x98, for_index119)
                var t1790 int = for_index119 + 1
                for_index119 = t1790
                var t1794 bool = index__59 > 0
                if t1794 {
                    var inline3378 rune = 44
                    var inline3379 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3378)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3379)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, for_item120)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1792 int = compound_old124 + compound_value125
                index__59 = t1792
                continue
            } else {
                break Loop_loop1788
            }
        }
        var inline3382 rune = 93
        var inline3383 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3382)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3383)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_String:
        var x99 string = value__54.(_goml_m_std_p_json_p_Value_String)._0
        _goml_m_std_p_json_p_write__json__string(builder__53, x99)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Number:
        var x100 string = value__54.(_goml_m_std_p_json_p_Value_Number)._0
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, x100)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Bool:
        var x101 bool = value__54.(_goml_m_std_p_json_p_Value_Bool)._0
        var jp1799 string
        if x101 {
            jp1799 = "true"
        } else {
            jp1799 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1799)
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
    var inline3396 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline3397 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline3396,
    }
    builder__65 = inline3397
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var inline3390 *_goml_vec_uint8 = builder__65.values
    var inline3391 Tuple2_4bool_6string = string_from_utf8(inline3390)
    var inline3393 string = inline3391._1
    return inline3393
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129)
        var for_index136 int = 0
        Loop_loop1810:
        for {
            var t1811 bool = for_index136 < for_limit135
            if t1811 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129, for_index136)
                var t1812 int = for_index136 + 1
                for_index136 = t1812
                var t1814 string = for_item137._0
                var t1815 bool
                var inline3399 bool = t1814 == name__67
                t1815 = inline3399
                if t1815 {
                    var t1816 _goml_m_std_p_json_p_Value = for_item137._1
                    var t1817 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1816,
                    }
                    return t1817
                } else {
                    continue
                }
            } else {
                break Loop_loop1810
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var t1827 int
    var inline3418 int = _goml_runtime_core_string_len(value__72)
    t1827 = inline3418
    var t1828 bool
    var inline3415 int = 0
    var inline3416 bool = t1827 == inline3415
    t1828 = inline3416
    if t1828 {
        return Option__int_None{}
    } else {
        var t1829 uint8
        var inline3412 int = 0
        var inline3413 uint8 = _goml_runtime_core_string_byte_get(value__72, inline3412)
        t1829 = inline3413
        var negative__73 bool
        var inline3409 uint8 = 45
        var inline3410 bool = t1829 == inline3409
        negative__73 = inline3410
        var jp1831 int
        if negative__73 {
            jp1831 = 1
        } else {
            jp1831 = 0
        }
        var index__74 int = jp1831
        var result__75 int = 0
        var t1852 int
        var inline3407 int = _goml_runtime_core_string_len(value__72)
        t1852 = inline3407
        var t1853 bool
        var inline3405 bool = index__74 == t1852
        t1853 = inline3405
        if t1853 {
            return Option__int_None{}
        } else {
            Loop_loop1838:
            for {
                var t1839 int
                var inline3403 int = _goml_runtime_core_string_len(value__72)
                t1839 = inline3403
                var t1840 bool = index__74 < t1839
                if t1840 {
                    var byte__76 uint8
                    var inline3401 uint8 = _goml_runtime_core_string_byte_get(value__72, index__74)
                    byte__76 = inline3401
                    var t1850 bool = byte__76 < 48
                    var jp1845 bool
                    if t1850 {
                        jp1845 = true
                    } else {
                        var t1851 bool = byte__76 > 57
                        jp1845 = t1851
                    }
                    if jp1845 {
                        return Option__int_None{}
                    } else {
                        var t1846 int = result__75 * 10
                        var t1847 uint8 = byte__76 - 48
                        var t1848 int = int(uint8(t1847))
                        var t1849 int = t1846 + t1848
                        result__75 = t1849
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1842 int = compound_old148 + compound_value149
                        index__74 = t1842
                        continue
                    }
                } else {
                    break Loop_loop1838
                }
            }
            var jp1835 int
            if negative__73 {
                var t1837 int = 0 - result__75
                jp1835 = t1837
            } else {
                jp1835 = result__75
            }
            var t1836 Option__int = Option__int_Some{
                _0: jp1835,
            }
            return t1836
        }
    }
}

func main0() struct{} {
    var mtmp136 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp1996 _goml_m_std_p_json_p_Value
    switch mtmp136.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x137 _goml_m_std_p_json_p_Value = mtmp136.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1996 = x137
        var mtmp140 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1996, "name")
        switch mtmp140.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline3493 string = "missing name"
            var inline3494 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3493)
            _goml_runtime_core_string_println(inline3494)
            var mtmp145 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1996, "version")
            switch mtmp145.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline3508 string = "missing version"
                var inline3509 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3508)
                _goml_runtime_core_string_println(inline3509)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x146 _goml_m_std_p_json_p_Value = mtmp145.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp147 Option__int
                switch x146.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline3519 string = x146.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline3521 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline3519)
                    mtmp147 = inline3521
                default:
                    mtmp147 = Option__int_None{}
                }
                switch mtmp147.(type) {
                case Option__int_None:
                    var inline3512 string = "invalid version"
                    var inline3513 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3512)
                    _goml_runtime_core_string_println(inline3513)
                case Option__int_Some:
                    var x148 int = mtmp147.(Option__int_Some)._0
                    var inline3516 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x148)
                    _goml_runtime_core_string_println(inline3516)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp150 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1996, "stable")
            switch mtmp150.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline3523 string = "missing stable"
                var inline3524 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3523)
                _goml_runtime_core_string_println(inline3524)
                var t2000 string = _goml_m_std_p_json_p_encode(jp1996)
                println__T_string(t2000)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x151 _goml_m_std_p_json_p_Value = mtmp150.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field3691 bool
                switch x151.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline3534 bool = x151.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field3691 = inline3534
                    var inline3531 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3691)
                    _goml_runtime_core_string_println(inline3531)
                    var t2000 string = _goml_m_std_p_json_p_encode(jp1996)
                    println__T_string(t2000)
                    return struct{}{}
                default:
                    var inline3527 string = "invalid stable"
                    var inline3528 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3527)
                    _goml_runtime_core_string_println(inline3528)
                    var t2000 string = _goml_m_std_p_json_p_encode(jp1996)
                    println__T_string(t2000)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x141 _goml_m_std_p_json_p_Value = mtmp140.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field3697 string
            switch x141.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline3504 string = x141.(_goml_m_std_p_json_p_Value_String)._0
                commute_field3697 = inline3504
                var inline3501 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field3697)
                _goml_runtime_core_string_println(inline3501)
                var mtmp145 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1996, "version")
                switch mtmp145.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3508 string = "missing version"
                    var inline3509 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3508)
                    _goml_runtime_core_string_println(inline3509)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x146 _goml_m_std_p_json_p_Value = mtmp145.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp147 Option__int
                    switch x146.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline3519 string = x146.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline3521 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline3519)
                        mtmp147 = inline3521
                    default:
                        mtmp147 = Option__int_None{}
                    }
                    switch mtmp147.(type) {
                    case Option__int_None:
                        var inline3512 string = "invalid version"
                        var inline3513 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3512)
                        _goml_runtime_core_string_println(inline3513)
                    case Option__int_Some:
                        var x148 int = mtmp147.(Option__int_Some)._0
                        var inline3516 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x148)
                        _goml_runtime_core_string_println(inline3516)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp150 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1996, "stable")
                switch mtmp150.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3523 string = "missing stable"
                    var inline3524 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3523)
                    _goml_runtime_core_string_println(inline3524)
                    var t2000 string = _goml_m_std_p_json_p_encode(jp1996)
                    println__T_string(t2000)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x151 _goml_m_std_p_json_p_Value = mtmp150.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field3691 bool
                    switch x151.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline3534 bool = x151.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field3691 = inline3534
                        var inline3531 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3691)
                        _goml_runtime_core_string_println(inline3531)
                        var t2000 string = _goml_m_std_p_json_p_encode(jp1996)
                        println__T_string(t2000)
                        return struct{}{}
                    default:
                        var inline3527 string = "invalid stable"
                        var inline3528 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3527)
                        _goml_runtime_core_string_println(inline3528)
                        var t2000 string = _goml_m_std_p_json_p_encode(jp1996)
                        println__T_string(t2000)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline3497 string = "invalid name"
                var inline3498 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3497)
                _goml_runtime_core_string_println(inline3498)
                var mtmp145 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1996, "version")
                switch mtmp145.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3508 string = "missing version"
                    var inline3509 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3508)
                    _goml_runtime_core_string_println(inline3509)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x146 _goml_m_std_p_json_p_Value = mtmp145.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp147 Option__int
                    switch x146.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline3519 string = x146.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline3521 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline3519)
                        mtmp147 = inline3521
                    default:
                        mtmp147 = Option__int_None{}
                    }
                    switch mtmp147.(type) {
                    case Option__int_None:
                        var inline3512 string = "invalid version"
                        var inline3513 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3512)
                        _goml_runtime_core_string_println(inline3513)
                    case Option__int_Some:
                        var x148 int = mtmp147.(Option__int_Some)._0
                        var inline3516 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x148)
                        _goml_runtime_core_string_println(inline3516)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp150 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp1996, "stable")
                switch mtmp150.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3523 string = "missing stable"
                    var inline3524 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3523)
                    _goml_runtime_core_string_println(inline3524)
                    var t2000 string = _goml_m_std_p_json_p_encode(jp1996)
                    println__T_string(t2000)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x151 _goml_m_std_p_json_p_Value = mtmp150.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field3691 bool
                    switch x151.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline3534 bool = x151.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field3691 = inline3534
                        var inline3531 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3691)
                        _goml_runtime_core_string_println(inline3531)
                        var t2000 string = _goml_m_std_p_json_p_encode(jp1996)
                        println__T_string(t2000)
                        return struct{}{}
                    default:
                        var inline3527 string = "invalid stable"
                        var inline3528 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3527)
                        _goml_runtime_core_string_println(inline3528)
                        var t2000 string = _goml_m_std_p_json_p_encode(jp1996)
                        println__T_string(t2000)
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
        var inline3490 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x138)
        _goml_runtime_core_string_println(inline3490)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t2022 string = _goml_runtime_core_int_to_string(self__34)
    return t2022
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline3538 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline3539 bool = inline3538._0
    var inline3540 rune = inline3538._1
    if inline3539 {
        return inline3540
    } else {
        var inline3544 rune = _goml_runtime_core_string_get("", -1)
        return inline3544
    }
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__98 uint8, other__99 uint8) bool {
    var t2045 bool = self__98 == other__99
    return t2045
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t2048 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t2048
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop2055:
    for {
        var t2056 int
        var inline3546 int = _goml_runtime_core_string_len(x12)
        t2056 = inline3546
        var t2057 bool = index__26 < t2056
        if t2057 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t2059 int = compound_old17 + x16
                index__26 = t2059
                continue
            } else {
                var t2061 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t2061
            }
        } else {
            break Loop_loop2055
        }
    }
    var t2054 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t2054
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline3548 uint32 = uint32(rune(self__36))
    var inline3549 bool = utf8_valid_scalar(inline3548)
    if inline3549 {
        var inline3550 string = _goml_runtime_core_char_to_string(self__36)
        return inline3550
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t2100 int = _goml_runtime_core_string_len(self__38)
    return t2100
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t2103 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t2103
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline3566 bool = string_is_char_boundary(self__43, start__44)
    var inline3568 bool
    if inline3566 {
        var inline3571 bool = string_is_char_boundary(self__43, end__45)
        inline3568 = inline3571
    } else {
        inline3568 = false
    }
    if inline3568 {
        var inline3569 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline3569
    } else {
        var inline3570 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline3570
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__215 int) *ref_int_x {
    var t2132 *ref_int_x = ref__Ref_3int(value__215)
    return t2132
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__216 *ref_int_x) int {
    var t2135 int = ref_get__Ref_3int(self__216)
    return t2135
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__217 *ref_int_x, value__218 int) struct{} {
    ref_set__Ref_3int(self__217, value__218)
    return struct{}{}
}

func char_from_uint32(value__32 uint32) Option__char {
    var t2142 bool
    var inline3583 bool = value__32 <= 1114111
    if inline3583 {
        var inline3584 bool = value__32 >= 55296
        var inline3586 bool
        if inline3584 {
            var inline3588 bool = value__32 <= 57343
            inline3586 = inline3588
        } else {
            inline3586 = false
        }
        var inline3587 bool = !inline3586
        t2142 = inline3587
    } else {
        t2142 = false
    }
    if t2142 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t2143 Option__char = Option__char_Some{
            _0: x24,
        }
        return t2143
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t2146 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t2146
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__134 *_goml_vec__goml_m_std_p_json_p_Value, elem__135 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__134, elem__135)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t2151 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t2151
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__134 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__135 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__134, elem__135)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t2205 string
    t2205 = value__31
    _goml_runtime_core_string_println(t2205)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t2336 bool = index__6 < 0
    var jp2334 bool
    if t2336 {
        jp2334 = true
    } else {
        var t2337 bool = index__6 >= length__7
        jp2334 = t2337
    }
    if jp2334 {
        var inline3595 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline3595
    } else {
        var t2221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t2221))
        var t2224 bool = first__8 < 128
        if t2224 {
            var inline3597 int = 1
            var inline3598 Option__char = char_from_uint32(first__8)
            switch inline3598.(type) {
            case Option__char_None:
                var inline3599 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline3599
            case Option__char_Some:
                var inline3600 rune = inline3598.(Option__char_Some)._0
                var inline3602 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline3600,
                    _2: inline3597,
                }
                return inline3602
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t2228 bool = first__8 < 194
            if t2228 {
                var inline3604 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline3604
            } else {
                var t2232 bool = first__8 < 224
                if t2232 {
                    var t2245 int = length__7 - index__6
                    var t2246 bool = t2245 < 2
                    if t2246 {
                        var inline3606 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline3606
                    } else {
                        var t2234 int = index__6 + 1
                        var t2235 uint8
                        var inline3620 uint8 = _goml_runtime_core_string_byte_get(value__5, t2234)
                        t2235 = inline3620
                        var second__9 uint32 = uint32(uint8(t2235))
                        var t2238 bool
                        var inline3617 bool = second__9 < 128
                        if inline3617 {
                            t2238 = true
                        } else {
                            var inline3618 bool = second__9 > 191
                            t2238 = inline3618
                        }
                        if t2238 {
                            var inline3608 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline3608
                        } else {
                            var t2240_rhs uint32 = 31
                            var t2240 uint32 = first__8 & t2240_rhs
                            var t2241_rhs int = 6
                            var t2241 uint32 = t2240 << t2241_rhs
                            var t2242_rhs uint32 = 63
                            var t2242 uint32 = second__9 & t2242_rhs
                            var t2243 uint32 = t2241 | t2242
                            var inline3610 int = 2
                            var inline3611 Option__char = char_from_uint32(t2243)
                            switch inline3611.(type) {
                            case Option__char_None:
                                var inline3612 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline3612
                            case Option__char_Some:
                                var inline3613 rune = inline3611.(Option__char_Some)._0
                                var inline3615 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline3613,
                                    _2: inline3610,
                                }
                                return inline3615
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t2250 bool = first__8 < 240
                    if t2250 {
                        var t2283 int = length__7 - index__6
                        var t2284 bool = t2283 < 3
                        if t2284 {
                            var inline3622 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline3622
                        } else {
                            var t2252 int = index__6 + 1
                            var t2253 uint8
                            var inline3637 uint8 = _goml_runtime_core_string_byte_get(value__5, t2252)
                            t2253 = inline3637
                            var second__10 uint32 = uint32(uint8(t2253))
                            var t2254 int = index__6 + 2
                            var t2255 uint8
                            var inline3635 uint8 = _goml_runtime_core_string_byte_get(value__5, t2254)
                            t2255 = inline3635
                            var third__11 uint32 = uint32(uint8(t2255))
                            var t2281 bool = utf8_invalid_continuation(second__10)
                            var jp2276 bool
                            if t2281 {
                                jp2276 = true
                            } else {
                                var inline3624 bool = third__11 < 128
                                if inline3624 {
                                    jp2276 = true
                                } else {
                                    var inline3625 bool = third__11 > 191
                                    jp2276 = inline3625
                                }
                            }
                            var jp2270 bool
                            if jp2276 {
                                jp2270 = true
                            } else {
                                var t2279 bool
                                var inline3627 uint32 = 224
                                var inline3628 bool = first__8 == inline3627
                                t2279 = inline3628
                                if t2279 {
                                    var t2280 bool = second__10 < 160
                                    jp2270 = t2280
                                } else {
                                    jp2270 = false
                                }
                            }
                            var jp2259 bool
                            if jp2270 {
                                jp2259 = true
                            } else {
                                var t2273 bool
                                var inline3630 uint32 = 237
                                var inline3631 bool = first__8 == inline3630
                                t2273 = inline3631
                                if t2273 {
                                    var t2274 bool = second__10 >= 160
                                    jp2259 = t2274
                                } else {
                                    jp2259 = false
                                }
                            }
                            if jp2259 {
                                var inline3633 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline3633
                            } else {
                                var t2261_rhs uint32 = 15
                                var t2261 uint32 = first__8 & t2261_rhs
                                var t2262_rhs int = 12
                                var t2262 uint32 = t2261 << t2262_rhs
                                var t2263_rhs uint32 = 63
                                var t2263 uint32 = second__10 & t2263_rhs
                                var t2264_rhs int = 6
                                var t2264 uint32 = t2263 << t2264_rhs
                                var t2265 uint32 = t2262 | t2264
                                var t2266_rhs uint32 = 63
                                var t2266 uint32 = third__11 & t2266_rhs
                                var t2267 uint32 = t2265 | t2266
                                var t2268 Tuple3_4bool_4char_3int = utf8_valid_decode(t2267, 3)
                                return t2268
                            }
                        }
                    } else {
                        var t2288 bool = first__8 < 245
                        if t2288 {
                            var t2329 int = length__7 - index__6
                            var t2330 bool = t2329 < 4
                            if t2330 {
                                var t2331 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t2331
                            } else {
                                var t2290 int = index__6 + 1
                                var t2291 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t2290)
                                var second__12 uint32 = uint32(uint8(t2291))
                                var t2292 int = index__6 + 2
                                var t2293 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t2292)
                                var third__13 uint32 = uint32(uint8(t2293))
                                var t2294 int = index__6 + 3
                                var t2295 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t2294)
                                var fourth__14 uint32 = uint32(uint8(t2295))
                                var t2327 bool = utf8_invalid_continuation(second__12)
                                var jp2325 bool
                                if t2327 {
                                    jp2325 = true
                                } else {
                                    var t2328 bool = utf8_invalid_continuation(third__13)
                                    jp2325 = t2328
                                }
                                var jp2319 bool
                                if jp2325 {
                                    jp2319 = true
                                } else {
                                    var t2326 bool = utf8_invalid_continuation(fourth__14)
                                    jp2319 = t2326
                                }
                                var jp2313 bool
                                if jp2319 {
                                    jp2313 = true
                                } else {
                                    var t2322 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t2322 {
                                        var t2323 bool = second__12 < 144
                                        jp2313 = t2323
                                    } else {
                                        jp2313 = false
                                    }
                                }
                                var jp2299 bool
                                if jp2313 {
                                    jp2299 = true
                                } else {
                                    var t2316 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t2316 {
                                        var t2317 bool = second__12 > 143
                                        jp2299 = t2317
                                    } else {
                                        jp2299 = false
                                    }
                                }
                                if jp2299 {
                                    var t2300 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t2300
                                } else {
                                    var t2301_rhs uint32 = 7
                                    var t2301 uint32 = first__8 & t2301_rhs
                                    var t2302_rhs int = 18
                                    var t2302 uint32 = t2301 << t2302_rhs
                                    var t2303_rhs uint32 = 63
                                    var t2303 uint32 = second__12 & t2303_rhs
                                    var t2304_rhs int = 12
                                    var t2304 uint32 = t2303 << t2304_rhs
                                    var t2305 uint32 = t2302 | t2304
                                    var t2306_rhs uint32 = 63
                                    var t2306 uint32 = third__13 & t2306_rhs
                                    var t2307_rhs int = 6
                                    var t2307 uint32 = t2306 << t2307_rhs
                                    var t2308 uint32 = t2305 | t2307
                                    var t2309_rhs uint32 = 63
                                    var t2309 uint32 = fourth__14 & t2309_rhs
                                    var t2310 uint32 = t2308 | t2309
                                    var t2311 Tuple3_4bool_4char_3int = utf8_valid_decode(t2310, 4)
                                    return t2311
                                }
                            }
                        } else {
                            var t2332 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t2332
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t2342 uint32 = uint32(rune(value__29))
    var t2343 bool
    var inline3639 bool = t2342 <= 1114111
    if inline3639 {
        var inline3640 bool = t2342 >= 55296
        var inline3642 bool
        if inline3640 {
            var inline3644 bool = t2342 <= 57343
            inline3642 = inline3644
        } else {
            inline3642 = false
        }
        var inline3643 bool = !inline3642
        t2343 = inline3643
    } else {
        t2343 = false
    }
    if t2343 {
        var t2344 string = _goml_runtime_core_char_to_string(value__29)
        return t2344
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t2359 bool = index__16 < 0
    var jp2350 bool
    if t2359 {
        jp2350 = true
    } else {
        var t2360 int
        var inline3646 int = _goml_runtime_core_string_len(value__15)
        t2360 = inline3646
        var t2361 bool = index__16 > t2360
        jp2350 = t2361
    }
    if jp2350 {
        return false
    } else {
        var t2353 int
        var inline3655 int = _goml_runtime_core_string_len(value__15)
        t2353 = inline3655
        var t2354 bool
        var inline3653 bool = index__16 == t2353
        t2354 = inline3653
        if t2354 {
            return true
        } else {
            var t2355 uint8
            var inline3651 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t2355 = inline3651
            var t2356_rhs uint8 = 192
            var t2356 uint8 = t2355 & t2356_rhs
            var t2357 bool
            var inline3648 uint8 = 128
            var inline3649 bool = t2356 == inline3648
            t2357 = inline3649
            var t2358 bool = !t2357
            return t2358
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t2370 bool = string_is_char_boundary(value__21, start__22)
    var jp2367 bool
    if t2370 {
        var t2371 bool = string_is_char_boundary(value__21, end__23)
        jp2367 = t2371
    } else {
        jp2367 = false
    }
    if jp2367 {
        var t2368 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t2368
    } else {
        var t2369 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t2369
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t2378 bool = value__4 <= 1114111
    if t2378 {
        var t2382 bool = value__4 >= 55296
        var jp2380 bool
        if t2382 {
            var t2383 bool = value__4 <= 57343
            jp2380 = t2383
        } else {
            jp2380 = false
        }
        var t2381 bool = !jp2380
        return t2381
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t2388 string = _goml_runtime_core_int_to_string(self__69)
    return t2388
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t2391 string = _goml_runtime_core_bool_to_string(self__66)
    return t2391
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t2394 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t2394
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field3700 rune
    var inline3659 bool = utf8_valid_scalar(value__0)
    if inline3659 {
        var inline3660 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline3662 rune = inline3660._1
        commute_field3700 = inline3662
        var t2400 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field3700,
            _2: width__1,
        }
        return t2400
    } else {
        var inline3657 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline3657
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t2405 bool = value__3 < 128
    if t2405 {
        return true
    } else {
        var t2406 bool = value__3 > 191
        return t2406
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t2409 bool = self__102 == other__103
    return t2409
}

func main() {
    main0()
}
