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

type _goml_vec__goml_m_Tuple2__6string__18std_p_serde_p_Schema struct {
    items []Tuple2_6string_28_goml_m_std_p_serde_p_Schema
}

type _goml_vec__goml_m_std_p_serde_p_VariantSchema struct {
    items []_goml_m_std_p_serde_p_VariantSchema
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

type _goml_vec__goml_m_std_p_serde_p_Schema struct {
    items []_goml_m_std_p_serde_p_Schema
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

type Tuple2_6string_28_goml_m_std_p_serde_p_Schema struct {
    _0 string
    _1 _goml_m_std_p_serde_p_Schema
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

type _goml_m_std_p_serde_p_VariantSchema struct {
    index int
    name string
    kind int
    fields *_goml_vec__goml_m_Tuple2__6string__18std_p_serde_p_Schema
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

type _goml_m_std_p_serde_p_TypedSchema____std_p_serde_p_Value struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____unit struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____bool struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____string struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____char struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____int struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____int8 struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____int16 struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____int32 struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____int64 struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____uint struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____uint8 struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____uint16 struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____uint32 struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____uint64 struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____float32 struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____float64 struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_TypedSchema____std_p_json_p_Value struct {
    value _goml_m_std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_Value interface {
    is_goml_m_std_p_serde_p_Value()
}

type _goml_m_std_p_serde_p_Value_Unit struct {}

func (_ _goml_m_std_p_serde_p_Value_Unit) is_goml_m_std_p_serde_p_Value() {}

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

type _goml_m_std_p_serde_p_Value_Char struct {
    _0 rune
}

func (_ _goml_m_std_p_serde_p_Value_Char) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Sequence struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
}

func (_ _goml_m_std_p_serde_p_Value_Sequence) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Tuple struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
}

func (_ _goml_m_std_p_serde_p_Value_Tuple) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Optional struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
}

func (_ _goml_m_std_p_serde_p_Value_Optional) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Value_Struct struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
}

func (_ _goml_m_std_p_serde_p_Value_Struct) is_goml_m_std_p_serde_p_Value() {}

type Variant struct {
    _0 int
    _1 string
    _2 int
    _3 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
}

func (_ Variant) is_goml_m_std_p_serde_p_Value() {}

type _goml_m_std_p_serde_p_Schema interface {
    is_goml_m_std_p_serde_p_Schema()
}

type Any struct {}

func (_ Any) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Unit struct {}

func (_ _goml_m_std_p_serde_p_Schema_Unit) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Bool struct {}

func (_ _goml_m_std_p_serde_p_Schema_Bool) is_goml_m_std_p_serde_p_Schema() {}

type Integer struct {
    _0 bool
    _1 int
}

func (_ Integer) is_goml_m_std_p_serde_p_Schema() {}

type Float struct {
    _0 int
}

func (_ Float) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_String struct {}

func (_ _goml_m_std_p_serde_p_Schema_String) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Char struct {}

func (_ _goml_m_std_p_serde_p_Schema_Char) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Sequence struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Schema
}

func (_ _goml_m_std_p_serde_p_Schema_Sequence) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Tuple struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Schema
}

func (_ _goml_m_std_p_serde_p_Schema_Tuple) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Optional struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Schema
}

func (_ _goml_m_std_p_serde_p_Schema_Optional) is_goml_m_std_p_serde_p_Schema() {}

type _goml_m_std_p_serde_p_Schema_Struct struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__18std_p_serde_p_Schema
}

func (_ _goml_m_std_p_serde_p_Schema_Struct) is_goml_m_std_p_serde_p_Schema() {}

type Enum struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_VariantSchema
}

func (_ Enum) is_goml_m_std_p_serde_p_Schema() {}

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

type _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string interface {
    is_goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string()
}

type _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string_Ok struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
}

func (_ _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string_Ok) is_goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string() {}

type _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string_Err) is_goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string() {}

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
    var inline2855 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline2855
    var t824 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t824
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline2870 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline2870
    var t838 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t838, length__5)
    var for_index1 int = 0
    Loop_loop840:
    for {
        var t841 bool = for_index1 < length__5
        if t841 {
            var for_item3 int = for_index1
            var t842 int = for_index1 + 1
            for_index1 = t842
            var t843 *_goml_vec_uint8 = self__3.values
            var t844 uint8
            var inline2866 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t844 = inline2866
            vec_push__Vec_5uint8(t843, t844)
            continue
        } else {
            break Loop_loop840
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t847 string
    var inline2872 string = char_to_string(value__8)
    t847 = inline2872
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t847)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var t1112 string = "" + message__2
    var t1113 string = t1112 + " at byte "
    var t1114 *ref_int_x = value__1.index
    var t1115 int
    var inline3097 int = ref_get__Ref_3int(t1114)
    t1115 = inline3097
    var t1116 string
    var inline3095 string = _goml_runtime_core_int_to_string(t1115)
    t1116 = inline3095
    var t1117 string = t1113 + t1116
    return t1117
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop1132:
    for {
        var t1140 *ref_int_x = value__4.index
        var t1141 int
        var inline3130 int = ref_get__Ref_3int(t1140)
        t1141 = inline3130
        var t1142 string = value__4.input
        var t1143 int
        var inline3128 int = _goml_runtime_core_string_len(t1142)
        t1143 = inline3128
        var t1144 bool = t1141 < t1143
        var jp1134 bool
        if t1144 {
            var t1145 string = value__4.input
            var t1146 *ref_int_x = value__4.index
            var t1147 int
            var inline3122 int = ref_get__Ref_3int(t1146)
            t1147 = inline3122
            var t1148 uint8
            var inline3120 uint8 = _goml_runtime_core_string_byte_get(t1145, t1147)
            t1148 = inline3120
            var inline3111 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1148, 9)
            var inline3113 bool
            if inline3111 {
                inline3113 = true
            } else {
                var inline3118 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1148, 10)
                inline3113 = inline3118
            }
            var inline3115 bool
            if inline3113 {
                inline3115 = true
            } else {
                var inline3117 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1148, 13)
                inline3115 = inline3117
            }
            if inline3115 {
                jp1134 = true
            } else {
                var inline3116 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1148, 32)
                jp1134 = inline3116
            }
        } else {
            jp1134 = false
        }
        if jp1134 {
            var t1135 *ref_int_x = value__4.index
            var t1136 *ref_int_x = value__4.index
            var t1137 int
            var inline3126 int = ref_get__Ref_3int(t1136)
            t1137 = inline3126
            var t1138 int = t1137 + 1
            ref_set__Ref_3int(t1135, t1138)
            continue
        } else {
            break Loop_loop1132
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var t1179 bool = value__5 >= 48
    var jp1155 bool
    if t1179 {
        var t1180 bool = value__5 <= 57
        jp1155 = t1180
    } else {
        jp1155 = false
    }
    if jp1155 {
        var t1156 uint8 = value__5 - 48
        var t1157 uint32 = uint32(uint8(t1156))
        var t1158 Option__uint32 = Option__uint32_Some{
            _0: t1157,
        }
        return t1158
    } else {
        var t1177 bool = value__5 >= 65
        var jp1162 bool
        if t1177 {
            var t1178 bool = value__5 <= 70
            jp1162 = t1178
        } else {
            jp1162 = false
        }
        if jp1162 {
            var t1163 uint8 = value__5 - 65
            var t1164 uint8 = t1163 + 10
            var t1165 uint32 = uint32(uint8(t1164))
            var t1166 Option__uint32 = Option__uint32_Some{
                _0: t1165,
            }
            return t1166
        } else {
            var t1175 bool = value__5 >= 97
            var jp1170 bool
            if t1175 {
                var t1176 bool = value__5 <= 102
                jp1170 = t1176
            } else {
                jp1170 = false
            }
            if jp1170 {
                var t1171 uint8 = value__5 - 97
                var t1172 uint8 = t1171 + 10
                var t1173 uint32 = uint32(uint8(t1172))
                var t1174 Option__uint32 = Option__uint32_Some{
                    _0: t1173,
                }
                return t1174
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t1185 *ref_int_x = value__6.index
    var t1186 int
    var inline3158 int = ref_get__Ref_3int(t1185)
    t1186 = inline3158
    var t1187 int = t1186 + 4
    var t1188 string = value__6.input
    var t1189 int
    var inline3156 int = _goml_runtime_core_string_len(t1188)
    t1189 = inline3156
    var t1190 bool = t1187 > t1189
    if t1190 {
        var t1191 string
        var inline3132 string = "incomplete unicode escape"
        var inline3133 string = "" + inline3132
        var inline3134 string = inline3133 + " at byte "
        var inline3135 *ref_int_x = value__6.index
        var inline3136 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3135)
        var inline3137 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3136)
        var inline3138 string = inline3134 + inline3137
        t1191 = inline3138
        var t1192 Result__uint32__string = Result__uint32__string_Err{
            _0: t1191,
        }
        return t1192
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop1199:
        for {
            var t1200 bool = for_index0 < for_limit1
            if t1200 {
                var for_item2 int = for_index0
                var t1201 int = for_index0 + 1
                for_index0 = t1201
                var t1202 string = value__6.input
                var t1203 *ref_int_x = value__6.index
                var t1204 int
                var inline3150 int = ref_get__Ref_3int(t1203)
                t1204 = inline3150
                var t1205 int = t1204 + for_item2
                var t1206 uint8
                var inline3148 uint8 = _goml_runtime_core_string_byte_get(t1202, t1205)
                t1206 = inline3148
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t1206)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t1208 string
                    var inline3140 string = "invalid unicode escape"
                    var inline3141 string = "" + inline3140
                    var inline3142 string = inline3141 + " at byte "
                    var inline3143 *ref_int_x = value__6.index
                    var inline3144 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3143)
                    var inline3145 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3144)
                    var inline3146 string = inline3142 + inline3145
                    t1208 = inline3146
                    var t1209 Result__uint32__string = Result__uint32__string_Err{
                        _0: t1208,
                    }
                    return t1209
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var t1210 uint32 = result__7 * 16
                    var t1211 uint32 = t1210 + x5
                    result__7 = t1211
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1199
            }
        }
        var t1194 *ref_int_x = value__6.index
        var t1195 *ref_int_x = value__6.index
        var t1196 int
        var inline3154 int = ref_get__Ref_3int(t1195)
        t1196 = inline3154
        var t1197 int = t1196 + 4
        ref_set__Ref_3int(t1194, t1197)
        var t1198 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        return t1198
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var commute_field3923 rune
    var inline3171 bool = utf8_valid_scalar(codepoint__12)
    if inline3171 {
        var inline3172 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__12)
        var inline3174 rune = inline3172._1
        commute_field3923 = inline3174
        var inline3168 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field3923)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__11, inline3168)
        var t1218 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t1218
    } else {
        var t1216 string
        var inline3160 string = "invalid unicode codepoint"
        var inline3161 string = "" + inline3160
        var inline3162 string = inline3161 + " at byte "
        var inline3163 *ref_int_x = value__10.index
        var inline3164 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3163)
        var inline3165 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3164)
        var inline3166 string = inline3162 + inline3165
        t1216 = inline3166
        var t1217 Result__unit__string = Result__unit__string_Err{
            _0: t1216,
        }
        return t1217
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp1222 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        jp1222 = x13
        var t1284 bool = jp1222 >= 55296
        var jp1226 bool
        if t1284 {
            var t1285 bool = jp1222 <= 56319
            jp1226 = t1285
        } else {
            jp1226 = false
        }
        if jp1226 {
            var t1263 *ref_int_x = value__14.index
            var t1264 int
            var inline3222 int = ref_get__Ref_3int(t1263)
            t1264 = inline3222
            var t1265 int = t1264 + 2
            var t1266 string = value__14.input
            var t1267 int
            var inline3220 int = _goml_runtime_core_string_len(t1266)
            t1267 = inline3220
            var t1268 bool = t1265 > t1267
            var jp1255 bool
            if t1268 {
                jp1255 = true
            } else {
                var t1269 string = value__14.input
                var t1270 *ref_int_x = value__14.index
                var t1271 int
                var inline3183 int = ref_get__Ref_3int(t1270)
                t1271 = inline3183
                var t1272 uint8
                var inline3181 uint8 = _goml_runtime_core_string_byte_get(t1269, t1271)
                t1272 = inline3181
                var t1273 bool
                var inline3178 uint8 = 92
                var inline3179 bool = t1272 == inline3178
                t1273 = inline3179
                var t1274 bool = !t1273
                jp1255 = t1274
            }
            var jp1230 bool
            if jp1255 {
                jp1230 = true
            } else {
                var t1256 string = value__14.input
                var t1257 *ref_int_x = value__14.index
                var t1258 int
                var inline3190 int = ref_get__Ref_3int(t1257)
                t1258 = inline3190
                var t1259 int = t1258 + 1
                var t1260 uint8
                var inline3188 uint8 = _goml_runtime_core_string_byte_get(t1256, t1259)
                t1260 = inline3188
                var t1261 bool
                var inline3185 uint8 = 117
                var inline3186 bool = t1260 == inline3185
                t1261 = inline3186
                var t1262 bool = !t1261
                jp1230 = t1262
            }
            if jp1230 {
                var t1231 string
                var inline3192 string = "missing low surrogate"
                var inline3193 string = "" + inline3192
                var inline3194 string = inline3193 + " at byte "
                var inline3195 *ref_int_x = value__14.index
                var inline3196 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3195)
                var inline3197 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3196)
                var inline3198 string = inline3194 + inline3197
                t1231 = inline3198
                var t1232 Result__unit__string = Result__unit__string_Err{
                    _0: t1231,
                }
                return t1232
            } else {
                var t1233 *ref_int_x = value__14.index
                var t1234 *ref_int_x = value__14.index
                var t1235 int
                var inline3218 int = ref_get__Ref_3int(t1234)
                t1235 = inline3218
                var t1236 int = t1235 + 2
                ref_set__Ref_3int(t1233, t1236)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp1238 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    jp1238 = x17
                    var t1251 bool = jp1238 < 56320
                    var jp1242 bool
                    if t1251 {
                        jp1242 = true
                    } else {
                        var t1252 bool = jp1238 > 57343
                        jp1242 = t1252
                    }
                    if jp1242 {
                        var t1243 string
                        var inline3200 string = "invalid low surrogate"
                        var inline3201 string = "" + inline3200
                        var inline3202 string = inline3201 + " at byte "
                        var inline3203 *ref_int_x = value__14.index
                        var inline3204 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3203)
                        var inline3205 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3204)
                        var inline3206 string = inline3202 + inline3205
                        t1243 = inline3206
                        var t1244 Result__unit__string = Result__unit__string_Err{
                            _0: t1243,
                        }
                        return t1244
                    } else {
                        var t1245 uint32 = jp1222 - 55296
                        var t1246 uint32 = t1245 * 1024
                        var t1247 uint32 = 65536 + t1246
                        var t1248 uint32 = t1247 + jp1238
                        var t1249 uint32 = t1248 - 56320
                        var inline3208 Option__char = char_from_uint32(t1249)
                        switch inline3208.(type) {
                        case Option__char_None:
                            var inline3209 string = _goml_m_std_p_json_p_json__error(value__14, "invalid unicode codepoint")
                            var inline3210 Result__unit__string = Result__unit__string_Err{
                                _0: inline3209,
                            }
                            return inline3210
                        case Option__char_Some:
                            var inline3211 rune = inline3208.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__15, inline3211)
                            var inline3214 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline3214
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var t1253 Result__unit__string = Result__unit__string_Err{
                        _0: x18,
                    }
                    return t1253
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t1282 bool = jp1222 >= 56320
            var jp1278 bool
            if t1282 {
                var t1283 bool = jp1222 <= 57343
                jp1278 = t1283
            } else {
                jp1278 = false
            }
            if jp1278 {
                var t1279 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t1280 Result__unit__string = Result__unit__string_Err{
                    _0: t1279,
                }
                return t1280
            } else {
                var t1281 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, jp1222)
                return t1281
            }
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var t1286 Result__unit__string = Result__unit__string_Err{
            _0: x14,
        }
        return t1286
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t1402 *ref_int_x = value__18.index
    var t1403 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1402)
    var t1404 string = value__18.input
    var t1405 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1404)
    var t1406 bool = t1403 >= t1405
    var jp1394 bool
    if t1406 {
        jp1394 = true
    } else {
        var t1407 string = value__18.input
        var t1408 *ref_int_x = value__18.index
        var t1409 int
        var inline3229 int = ref_get__Ref_3int(t1408)
        t1409 = inline3229
        var t1410 uint8
        var inline3227 uint8 = _goml_runtime_core_string_byte_get(t1407, t1409)
        t1410 = inline3227
        var t1411 bool
        var inline3224 uint8 = 34
        var inline3225 bool = t1410 == inline3224
        t1411 = inline3225
        var t1412 bool = !t1411
        jp1394 = t1412
    }
    if jp1394 {
        var t1395 string
        var inline3231 string = "expected string"
        var inline3232 string = "" + inline3231
        var inline3233 string = inline3232 + " at byte "
        var inline3234 *ref_int_x = value__18.index
        var inline3235 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3234)
        var inline3236 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3235)
        var inline3237 string = inline3233 + inline3236
        t1395 = inline3237
        var t1396 Result__string__string = Result__string__string_Err{
            _0: t1395,
        }
        return t1396
    } else {
        var t1397 *ref_int_x = value__18.index
        var t1398 *ref_int_x = value__18.index
        var t1399 int
        var inline3241 int = ref_get__Ref_3int(t1398)
        t1399 = inline3241
        var t1400 int = t1399 + 1
        ref_set__Ref_3int(t1397, t1400)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t1290 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1290)
        Loop_loop1294:
        for {
            var t1295 *ref_int_x = value__18.index
            var t1296 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1295)
            var t1297 string = value__18.input
            var t1298 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1297)
            var t1299 bool = t1296 < t1298
            if t1299 {
                var t1300 string = value__18.input
                var t1301 *ref_int_x = value__18.index
                var t1302 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1301)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1300, t1302)
                var t1304 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(byte__21, 34)
                if t1304 {
                    var t1312 *ref_int_x = value__18.index
                    var t1313 int
                    var inline3257 int = ref_get__Ref_3int(t1312)
                    t1313 = inline3257
                    var t1314 bool = segment__20 < t1313
                    if t1314 {
                        var t1315 string = value__18.input
                        var t1316 *ref_int_x = value__18.index
                        var t1317 int
                        var inline3245 int = ref_get__Ref_3int(t1316)
                        t1317 = inline3245
                        var t1318 string
                        var inline3243 string = string_byte_slice(t1315, segment__20, t1317)
                        t1318 = inline3243
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t1318)
                    } else {}
                    var t1306 *ref_int_x = value__18.index
                    var t1307 *ref_int_x = value__18.index
                    var t1308 int
                    var inline3255 int = ref_get__Ref_3int(t1307)
                    t1308 = inline3255
                    var t1309 int = t1308 + 1
                    ref_set__Ref_3int(t1306, t1309)
                    var t1310 string
                    var inline3247 *_goml_vec_uint8 = builder__19.values
                    var inline3248 Tuple2_4bool_6string = string_from_utf8(inline3247)
                    var inline3250 string = inline3248._1
                    t1310 = inline3250
                    var t1311 Result__string__string = Result__string__string_Ok{
                        _0: t1310,
                    }
                    return t1311
                } else {
                    var t1321 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(byte__21, 92)
                    if t1321 {
                        var t1376 *ref_int_x = value__18.index
                        var t1377 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1376)
                        var t1378 bool = segment__20 < t1377
                        if t1378 {
                            var t1379 string = value__18.input
                            var t1380 *ref_int_x = value__18.index
                            var t1381 int
                            var inline3261 int = ref_get__Ref_3int(t1380)
                            t1381 = inline3261
                            var t1382 string
                            var inline3259 string = string_byte_slice(t1379, segment__20, t1381)
                            t1382 = inline3259
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t1382)
                        } else {}
                        var t1323 *ref_int_x = value__18.index
                        var t1324 *ref_int_x = value__18.index
                        var t1325 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1324)
                        var t1326 int = t1325 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1323, t1326)
                        var t1369 *ref_int_x = value__18.index
                        var t1370 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1369)
                        var t1371 string = value__18.input
                        var t1372 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1371)
                        var t1373 bool = t1370 >= t1372
                        if t1373 {
                            var t1374 string
                            var inline3263 string = "incomplete escape"
                            var inline3264 string = "" + inline3263
                            var inline3265 string = inline3264 + " at byte "
                            var inline3266 *ref_int_x = value__18.index
                            var inline3267 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3266)
                            var inline3268 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3267)
                            var inline3269 string = inline3265 + inline3268
                            t1374 = inline3269
                            var t1375 Result__string__string = Result__string__string_Err{
                                _0: t1374,
                            }
                            return t1375
                        } else {
                            var t1328 string = value__18.input
                            var t1329 *ref_int_x = value__18.index
                            var t1330 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1329)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1328, t1330)
                            var t1331 *ref_int_x = value__18.index
                            var t1332 *ref_int_x = value__18.index
                            var t1333 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1332)
                            var t1334 int = t1333 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1331, t1334)
                            var t1338 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__22, 34)
                            if t1338 {
                                var inline3271 rune = 34
                                var inline3272 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3271)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, inline3272)
                                var t1336 *ref_int_x = value__18.index
                                var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                segment__20 = t1337
                                continue
                            } else {
                                var t1341 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__22, 92)
                                if t1341 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t1336 *ref_int_x = value__18.index
                                    var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                    segment__20 = t1337
                                    continue
                                } else {
                                    var t1344 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__22, 47)
                                    if t1344 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t1336 *ref_int_x = value__18.index
                                        var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                        segment__20 = t1337
                                        continue
                                    } else {
                                        var t1347 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__22, 98)
                                        if t1347 {
                                            var mtmp26 Option__char = char_from_uint32(8)
                                            switch mtmp26.(type) {
                                            case Option__char_None:
                                                var t1336 *ref_int_x = value__18.index
                                                var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                                segment__20 = t1337
                                                continue
                                            case Option__char_Some:
                                                var x27 rune = mtmp26.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x27)
                                                var t1336 *ref_int_x = value__18.index
                                                var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                                segment__20 = t1337
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t1351 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__22, 102)
                                            if t1351 {
                                                var mtmp28 Option__char = char_from_uint32(12)
                                                switch mtmp28.(type) {
                                                case Option__char_None:
                                                    var t1336 *ref_int_x = value__18.index
                                                    var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                                    segment__20 = t1337
                                                    continue
                                                case Option__char_Some:
                                                    var x29 rune = mtmp28.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x29)
                                                    var t1336 *ref_int_x = value__18.index
                                                    var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                                    segment__20 = t1337
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t1355 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__22, 110)
                                                if t1355 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t1336 *ref_int_x = value__18.index
                                                    var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                                    segment__20 = t1337
                                                    continue
                                                } else {
                                                    var t1358 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__22, 114)
                                                    if t1358 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t1336 *ref_int_x = value__18.index
                                                        var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                                        segment__20 = t1337
                                                        continue
                                                    } else {
                                                        var t1361 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__22, 116)
                                                        if t1361 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t1336 *ref_int_x = value__18.index
                                                            var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                                            segment__20 = t1337
                                                            continue
                                                        } else {
                                                            var t1364 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__22, 117)
                                                            if t1364 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t1336 *ref_int_x = value__18.index
                                                                    var t1337 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1336)
                                                                    segment__20 = t1337
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var t1366 Result__string__string = Result__string__string_Err{
                                                                        _0: x32,
                                                                    }
                                                                    return t1366
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t1367 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t1368 Result__string__string = Result__string__string_Err{
                                                                    _0: t1367,
                                                                }
                                                                return t1368
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
                        var t1385 bool = byte__21 < 32
                        if t1385 {
                            var t1386 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t1387 Result__string__string = Result__string__string_Err{
                                _0: t1386,
                            }
                            return t1387
                        } else {
                            var t1388 *ref_int_x = value__18.index
                            var t1389 *ref_int_x = value__18.index
                            var t1390 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1389)
                            var t1391 int = t1390 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1388, t1391)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop1294
            }
        }
        var t1292 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t1293 Result__string__string = Result__string__string_Err{
            _0: t1292,
        }
        return t1293
    }
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var t1421 *ref_int_x = value__26.index
    var start__27 int
    var inline3292 int = ref_get__Ref_3int(t1421)
    start__27 = inline3292
    Loop_loop1426:
    for {
        var t1434 *ref_int_x = value__26.index
        var t1435 int
        var inline3288 int = ref_get__Ref_3int(t1434)
        t1435 = inline3288
        var t1436 string = value__26.input
        var t1437 int
        var inline3286 int = _goml_runtime_core_string_len(t1436)
        t1437 = inline3286
        var t1438 bool = t1435 < t1437
        var jp1428 bool
        if t1438 {
            var t1439 string = value__26.input
            var t1440 *ref_int_x = value__26.index
            var t1441 int
            var inline3280 int = ref_get__Ref_3int(t1440)
            t1441 = inline3280
            var t1442 uint8
            var inline3278 uint8 = _goml_runtime_core_string_byte_get(t1439, t1441)
            t1442 = inline3278
            var inline3275 bool = t1442 >= 48
            if inline3275 {
                var inline3276 bool = t1442 <= 57
                jp1428 = inline3276
            } else {
                jp1428 = false
            }
        } else {
            jp1428 = false
        }
        if jp1428 {
            var t1429 *ref_int_x = value__26.index
            var t1430 *ref_int_x = value__26.index
            var t1431 int
            var inline3284 int = ref_get__Ref_3int(t1430)
            t1431 = inline3284
            var t1432 int = t1431 + 1
            ref_set__Ref_3int(t1429, t1432)
            continue
        } else {
            break Loop_loop1426
        }
    }
    var t1423 *ref_int_x = value__26.index
    var t1424 int
    var inline3290 int = ref_get__Ref_3int(t1423)
    t1424 = inline3290
    var t1425 bool = t1424 > start__27
    return t1425
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1446 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1446)
    var t1568 string = value__28.input
    var t1569 *ref_int_x = value__28.index
    var t1570 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1569)
    var t1571 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1568, t1570)
    var t1572 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1571, 45)
    if t1572 {
        var t1573 *ref_int_x = value__28.index
        var t1574 *ref_int_x = value__28.index
        var t1575 int
        var inline3296 int = ref_get__Ref_3int(t1574)
        t1575 = inline3296
        var t1576 int = t1575 + 1
        ref_set__Ref_3int(t1573, t1576)
    } else {}
    var t1531 *ref_int_x = value__28.index
    var t1532 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1531)
    var t1533 string = value__28.input
    var t1534 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1533)
    var t1535 bool = t1532 >= t1534
    if t1535 {
        var t1536 string
        var inline3298 string = "incomplete number"
        var inline3299 string = "" + inline3298
        var inline3300 string = inline3299 + " at byte "
        var inline3301 *ref_int_x = value__28.index
        var inline3302 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3301)
        var inline3303 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3302)
        var inline3304 string = inline3300 + inline3303
        t1536 = inline3304
        var t1537 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1536,
        }
        return t1537
    } else {
        var t1539 string = value__28.input
        var t1540 *ref_int_x = value__28.index
        var t1541 int
        var inline3342 int = ref_get__Ref_3int(t1540)
        t1541 = inline3342
        var t1542 uint8
        var inline3340 uint8 = _goml_runtime_core_string_byte_get(t1539, t1541)
        t1542 = inline3340
        var t1543 bool
        var inline3337 uint8 = 48
        var inline3338 bool = t1542 == inline3337
        t1543 = inline3338
        if t1543 {
            var t1544 *ref_int_x = value__28.index
            var t1545 *ref_int_x = value__28.index
            var t1546 int
            var inline3327 int = ref_get__Ref_3int(t1545)
            t1546 = inline3327
            var t1547 int = t1546 + 1
            ref_set__Ref_3int(t1544, t1547)
            var t1553 *ref_int_x = value__28.index
            var t1554 int
            var inline3323 int = ref_get__Ref_3int(t1553)
            t1554 = inline3323
            var t1555 string = value__28.input
            var t1556 int
            var inline3321 int = _goml_runtime_core_string_len(t1555)
            t1556 = inline3321
            var t1557 bool = t1554 < t1556
            var jp1550 bool
            if t1557 {
                var t1558 string = value__28.input
                var t1559 *ref_int_x = value__28.index
                var t1560 int
                var inline3311 int = ref_get__Ref_3int(t1559)
                t1560 = inline3311
                var t1561 uint8
                var inline3309 uint8 = _goml_runtime_core_string_byte_get(t1558, t1560)
                t1561 = inline3309
                var inline3306 bool = t1561 >= 48
                if inline3306 {
                    var inline3307 bool = t1561 <= 57
                    jp1550 = inline3307
                } else {
                    jp1550 = false
                }
            } else {
                jp1550 = false
            }
            if jp1550 {
                var t1551 string
                var inline3313 string = "invalid leading zero"
                var inline3314 string = "" + inline3313
                var inline3315 string = inline3314 + " at byte "
                var inline3316 *ref_int_x = value__28.index
                var inline3317 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3316)
                var inline3318 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3317)
                var inline3319 string = inline3315 + inline3318
                t1551 = inline3319
                var t1552 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1551,
                }
                return t1552
            } else {
                var t1521 *ref_int_x = value__28.index
                var t1522 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1521)
                var t1523 string = value__28.input
                var t1524 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1523)
                var t1525 bool = t1522 < t1524
                var jp1511 bool
                if t1525 {
                    var t1526 string = value__28.input
                    var t1527 *ref_int_x = value__28.index
                    var t1528 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1527)
                    var t1529 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1526, t1528)
                    var t1530 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1529, 46)
                    jp1511 = t1530
                } else {
                    jp1511 = false
                }
                if jp1511 {
                    var t1512 *ref_int_x = value__28.index
                    var t1513 *ref_int_x = value__28.index
                    var t1514 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1513)
                    var t1515 int = t1514 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1512, t1515)
                    var t1517 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t1518 bool = !t1517
                    if t1518 {
                        var t1519 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t1520 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1519,
                        }
                        return t1520
                    } else {
                        var t1493 *ref_int_x = value__28.index
                        var t1494 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1493)
                        var t1495 string = value__28.input
                        var t1496 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1495)
                        var t1497 bool = t1494 < t1496
                        var jp1458 bool
                        if t1497 {
                            var t1500 string = value__28.input
                            var t1501 *ref_int_x = value__28.index
                            var t1502 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1501)
                            var t1503 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1500, t1502)
                            var t1504 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1503, 101)
                            if t1504 {
                                jp1458 = true
                            } else {
                                var t1505 string = value__28.input
                                var t1506 *ref_int_x = value__28.index
                                var t1507 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1506)
                                var t1508 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1505, t1507)
                                var t1509 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1508, 69)
                                jp1458 = t1509
                            }
                        } else {
                            jp1458 = false
                        }
                        if jp1458 {
                            var t1459 *ref_int_x = value__28.index
                            var t1460 *ref_int_x = value__28.index
                            var t1461 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1460)
                            var t1462 int = t1461 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1459, t1462)
                            var t1476 *ref_int_x = value__28.index
                            var t1477 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1476)
                            var t1478 string = value__28.input
                            var t1479 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1478)
                            var t1480 bool = t1477 < t1479
                            var jp1470 bool
                            if t1480 {
                                var t1483 string = value__28.input
                                var t1484 *ref_int_x = value__28.index
                                var t1485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1484)
                                var t1486 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1483, t1485)
                                var t1487 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1486, 43)
                                if t1487 {
                                    jp1470 = true
                                } else {
                                    var t1488 string = value__28.input
                                    var t1489 *ref_int_x = value__28.index
                                    var t1490 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1489)
                                    var t1491 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1488, t1490)
                                    var t1492 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1491, 45)
                                    jp1470 = t1492
                                }
                            } else {
                                jp1470 = false
                            }
                            if jp1470 {
                                var t1471 *ref_int_x = value__28.index
                                var t1472 *ref_int_x = value__28.index
                                var t1473 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1472)
                                var t1474 int = t1473 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1471, t1474)
                            } else {}
                            var t1465 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t1466 bool = !t1465
                            if t1466 {
                                var t1467 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t1468 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1467,
                                }
                                return t1468
                            } else {
                                var t1451 string = value__28.input
                                var t1452 *ref_int_x = value__28.index
                                var t1453 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1452)
                                var t1454 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1451, start__29, t1453)
                                var t1455 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                    _0: t1454,
                                }
                                var t1456 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t1455,
                                }
                                return t1456
                            }
                        } else {
                            var t1451 string = value__28.input
                            var t1452 *ref_int_x = value__28.index
                            var t1453 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1452)
                            var t1454 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1451, start__29, t1453)
                            var t1455 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1454,
                            }
                            var t1456 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1455,
                            }
                            return t1456
                        }
                    }
                } else {
                    var t1493 *ref_int_x = value__28.index
                    var t1494 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1493)
                    var t1495 string = value__28.input
                    var t1496 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1495)
                    var t1497 bool = t1494 < t1496
                    var jp1458 bool
                    if t1497 {
                        var t1500 string = value__28.input
                        var t1501 *ref_int_x = value__28.index
                        var t1502 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1501)
                        var t1503 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1500, t1502)
                        var t1504 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1503, 101)
                        if t1504 {
                            jp1458 = true
                        } else {
                            var t1505 string = value__28.input
                            var t1506 *ref_int_x = value__28.index
                            var t1507 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1506)
                            var t1508 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1505, t1507)
                            var t1509 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1508, 69)
                            jp1458 = t1509
                        }
                    } else {
                        jp1458 = false
                    }
                    if jp1458 {
                        var t1459 *ref_int_x = value__28.index
                        var t1460 *ref_int_x = value__28.index
                        var t1461 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1460)
                        var t1462 int = t1461 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1459, t1462)
                        var t1476 *ref_int_x = value__28.index
                        var t1477 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1476)
                        var t1478 string = value__28.input
                        var t1479 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1478)
                        var t1480 bool = t1477 < t1479
                        var jp1470 bool
                        if t1480 {
                            var t1483 string = value__28.input
                            var t1484 *ref_int_x = value__28.index
                            var t1485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1484)
                            var t1486 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1483, t1485)
                            var t1487 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1486, 43)
                            if t1487 {
                                jp1470 = true
                            } else {
                                var t1488 string = value__28.input
                                var t1489 *ref_int_x = value__28.index
                                var t1490 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1489)
                                var t1491 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1488, t1490)
                                var t1492 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1491, 45)
                                jp1470 = t1492
                            }
                        } else {
                            jp1470 = false
                        }
                        if jp1470 {
                            var t1471 *ref_int_x = value__28.index
                            var t1472 *ref_int_x = value__28.index
                            var t1473 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1472)
                            var t1474 int = t1473 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1471, t1474)
                        } else {}
                        var t1465 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t1466 bool = !t1465
                        if t1466 {
                            var t1467 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t1468 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t1467,
                            }
                            return t1468
                        } else {
                            var t1451 string = value__28.input
                            var t1452 *ref_int_x = value__28.index
                            var t1453 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1452)
                            var t1454 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1451, start__29, t1453)
                            var t1455 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1454,
                            }
                            var t1456 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1455,
                            }
                            return t1456
                        }
                    } else {
                        var t1451 string = value__28.input
                        var t1452 *ref_int_x = value__28.index
                        var t1453 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1452)
                        var t1454 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1451, start__29, t1453)
                        var t1455 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                            _0: t1454,
                        }
                        var t1456 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t1455,
                        }
                        return t1456
                    }
                }
            }
        } else {
            var t1564 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t1565 bool = !t1564
            if t1565 {
                var t1566 string
                var inline3329 string = "expected number"
                var inline3330 string = "" + inline3329
                var inline3331 string = inline3330 + " at byte "
                var inline3332 *ref_int_x = value__28.index
                var inline3333 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3332)
                var inline3334 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3333)
                var inline3335 string = inline3331 + inline3334
                t1566 = inline3335
                var t1567 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1566,
                }
                return t1567
            } else {
                var t1521 *ref_int_x = value__28.index
                var t1522 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1521)
                var t1523 string = value__28.input
                var t1524 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1523)
                var t1525 bool = t1522 < t1524
                var jp1511 bool
                if t1525 {
                    var t1526 string = value__28.input
                    var t1527 *ref_int_x = value__28.index
                    var t1528 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1527)
                    var t1529 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1526, t1528)
                    var t1530 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1529, 46)
                    jp1511 = t1530
                } else {
                    jp1511 = false
                }
                if jp1511 {
                    var t1512 *ref_int_x = value__28.index
                    var t1513 *ref_int_x = value__28.index
                    var t1514 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1513)
                    var t1515 int = t1514 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1512, t1515)
                    var t1517 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t1518 bool = !t1517
                    if t1518 {
                        var t1519 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t1520 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1519,
                        }
                        return t1520
                    } else {
                        var t1493 *ref_int_x = value__28.index
                        var t1494 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1493)
                        var t1495 string = value__28.input
                        var t1496 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1495)
                        var t1497 bool = t1494 < t1496
                        var jp1458 bool
                        if t1497 {
                            var t1500 string = value__28.input
                            var t1501 *ref_int_x = value__28.index
                            var t1502 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1501)
                            var t1503 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1500, t1502)
                            var t1504 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1503, 101)
                            if t1504 {
                                jp1458 = true
                            } else {
                                var t1505 string = value__28.input
                                var t1506 *ref_int_x = value__28.index
                                var t1507 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1506)
                                var t1508 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1505, t1507)
                                var t1509 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1508, 69)
                                jp1458 = t1509
                            }
                        } else {
                            jp1458 = false
                        }
                        if jp1458 {
                            var t1459 *ref_int_x = value__28.index
                            var t1460 *ref_int_x = value__28.index
                            var t1461 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1460)
                            var t1462 int = t1461 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1459, t1462)
                            var t1476 *ref_int_x = value__28.index
                            var t1477 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1476)
                            var t1478 string = value__28.input
                            var t1479 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1478)
                            var t1480 bool = t1477 < t1479
                            var jp1470 bool
                            if t1480 {
                                var t1483 string = value__28.input
                                var t1484 *ref_int_x = value__28.index
                                var t1485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1484)
                                var t1486 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1483, t1485)
                                var t1487 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1486, 43)
                                if t1487 {
                                    jp1470 = true
                                } else {
                                    var t1488 string = value__28.input
                                    var t1489 *ref_int_x = value__28.index
                                    var t1490 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1489)
                                    var t1491 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1488, t1490)
                                    var t1492 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1491, 45)
                                    jp1470 = t1492
                                }
                            } else {
                                jp1470 = false
                            }
                            if jp1470 {
                                var t1471 *ref_int_x = value__28.index
                                var t1472 *ref_int_x = value__28.index
                                var t1473 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1472)
                                var t1474 int = t1473 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1471, t1474)
                            } else {}
                            var t1465 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t1466 bool = !t1465
                            if t1466 {
                                var t1467 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t1468 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1467,
                                }
                                return t1468
                            } else {
                                var t1451 string = value__28.input
                                var t1452 *ref_int_x = value__28.index
                                var t1453 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1452)
                                var t1454 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1451, start__29, t1453)
                                var t1455 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                    _0: t1454,
                                }
                                var t1456 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t1455,
                                }
                                return t1456
                            }
                        } else {
                            var t1451 string = value__28.input
                            var t1452 *ref_int_x = value__28.index
                            var t1453 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1452)
                            var t1454 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1451, start__29, t1453)
                            var t1455 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1454,
                            }
                            var t1456 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1455,
                            }
                            return t1456
                        }
                    }
                } else {
                    var t1493 *ref_int_x = value__28.index
                    var t1494 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1493)
                    var t1495 string = value__28.input
                    var t1496 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1495)
                    var t1497 bool = t1494 < t1496
                    var jp1458 bool
                    if t1497 {
                        var t1500 string = value__28.input
                        var t1501 *ref_int_x = value__28.index
                        var t1502 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1501)
                        var t1503 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1500, t1502)
                        var t1504 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1503, 101)
                        if t1504 {
                            jp1458 = true
                        } else {
                            var t1505 string = value__28.input
                            var t1506 *ref_int_x = value__28.index
                            var t1507 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1506)
                            var t1508 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1505, t1507)
                            var t1509 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1508, 69)
                            jp1458 = t1509
                        }
                    } else {
                        jp1458 = false
                    }
                    if jp1458 {
                        var t1459 *ref_int_x = value__28.index
                        var t1460 *ref_int_x = value__28.index
                        var t1461 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1460)
                        var t1462 int = t1461 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1459, t1462)
                        var t1476 *ref_int_x = value__28.index
                        var t1477 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1476)
                        var t1478 string = value__28.input
                        var t1479 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1478)
                        var t1480 bool = t1477 < t1479
                        var jp1470 bool
                        if t1480 {
                            var t1483 string = value__28.input
                            var t1484 *ref_int_x = value__28.index
                            var t1485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1484)
                            var t1486 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1483, t1485)
                            var t1487 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1486, 43)
                            if t1487 {
                                jp1470 = true
                            } else {
                                var t1488 string = value__28.input
                                var t1489 *ref_int_x = value__28.index
                                var t1490 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1489)
                                var t1491 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1488, t1490)
                                var t1492 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1491, 45)
                                jp1470 = t1492
                            }
                        } else {
                            jp1470 = false
                        }
                        if jp1470 {
                            var t1471 *ref_int_x = value__28.index
                            var t1472 *ref_int_x = value__28.index
                            var t1473 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1472)
                            var t1474 int = t1473 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1471, t1474)
                        } else {}
                        var t1465 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t1466 bool = !t1465
                        if t1466 {
                            var t1467 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t1468 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t1467,
                            }
                            return t1468
                        } else {
                            var t1451 string = value__28.input
                            var t1452 *ref_int_x = value__28.index
                            var t1453 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1452)
                            var t1454 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1451, start__29, t1453)
                            var t1455 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1454,
                            }
                            var t1456 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1455,
                            }
                            return t1456
                        }
                    } else {
                        var t1451 string = value__28.input
                        var t1452 *ref_int_x = value__28.index
                        var t1453 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1452)
                        var t1454 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1451, start__29, t1453)
                        var t1455 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                            _0: t1454,
                        }
                        var t1456 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t1455,
                        }
                        return t1456
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t1592 *ref_int_x = value__30.index
    var t1593 int
    var inline3372 int = ref_get__Ref_3int(t1592)
    t1593 = inline3372
    var t1594 int
    var inline3370 int = _goml_runtime_core_string_len(expected__31)
    t1594 = inline3370
    var t1595 int = t1593 + t1594
    var t1596 string = value__30.input
    var t1597 int
    var inline3368 int = _goml_runtime_core_string_len(t1596)
    t1597 = inline3368
    var t1598 bool = t1595 <= t1597
    var jp1583 bool
    if t1598 {
        var t1599 string = value__30.input
        var t1600 *ref_int_x = value__30.index
        var t1601 int
        var inline3352 int = ref_get__Ref_3int(t1600)
        t1601 = inline3352
        var t1602 *ref_int_x = value__30.index
        var t1603 int
        var inline3350 int = ref_get__Ref_3int(t1602)
        t1603 = inline3350
        var t1604 int
        var inline3348 int = _goml_runtime_core_string_len(expected__31)
        t1604 = inline3348
        var t1605 int = t1603 + t1604
        var t1606 string
        var inline3346 string = string_byte_slice(t1599, t1601, t1605)
        t1606 = inline3346
        var inline3344 bool = t1606 == expected__31
        jp1583 = inline3344
    } else {
        jp1583 = false
    }
    if jp1583 {
        var t1584 *ref_int_x = value__30.index
        var t1585 *ref_int_x = value__30.index
        var t1586 int
        var inline3358 int = ref_get__Ref_3int(t1585)
        t1586 = inline3358
        var t1587 int
        var inline3356 int = _goml_runtime_core_string_len(expected__31)
        t1587 = inline3356
        var t1588 int = t1586 + t1587
        ref_set__Ref_3int(t1584, t1588)
        var t1589 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        return t1589
    } else {
        var t1590 string
        var inline3360 string = "invalid literal"
        var inline3361 string = "" + inline3360
        var inline3362 string = inline3361 + " at byte "
        var inline3363 *ref_int_x = value__30.index
        var inline3364 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3363)
        var inline3365 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3364)
        var inline3366 string = inline3362 + inline3365
        t1590 = inline3366
        var t1591 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1590,
        }
        return t1591
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1610 *ref_int_x = value__33.index
    var t1611 *ref_int_x = value__33.index
    var t1612 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1611)
    var t1613 int = t1612 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1610, t1613)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var vec_literal__8702 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1668 *ref_int_x = value__33.index
    var t1669 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1668)
    var t1670 string = value__33.input
    var t1671 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1670)
    var t1672 bool = t1669 < t1671
    var jp1661 bool
    if t1672 {
        var t1673 string = value__33.input
        var t1674 *ref_int_x = value__33.index
        var t1675 int
        var inline3379 int = ref_get__Ref_3int(t1674)
        t1675 = inline3379
        var t1676 uint8
        var inline3377 uint8 = _goml_runtime_core_string_byte_get(t1673, t1675)
        t1676 = inline3377
        var inline3374 uint8 = 93
        var inline3375 bool = t1676 == inline3374
        jp1661 = inline3375
    } else {
        jp1661 = false
    }
    if jp1661 {
        var t1662 *ref_int_x = value__33.index
        var t1663 *ref_int_x = value__33.index
        var t1664 int
        var inline3383 int = ref_get__Ref_3int(t1663)
        t1664 = inline3383
        var t1665 int = t1664 + 1
        ref_set__Ref_3int(t1662, t1665)
        var t1666 _goml_m_std_p_json_p_Value = Array{
            _0: vec_literal__8702,
        }
        var t1667 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1666,
        }
        return t1667
    } else {
        Loop_loop1618:
        for {
            var t1619 *ref_int_x = value__33.index
            var t1620 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1619)
            var t1621 string = value__33.input
            var t1622 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1621)
            var t1623 bool = t1620 < t1622
            if t1623 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1625 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp1625 = x51
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8702, jp1625)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1627 *ref_int_x = value__33.index
                    var t1628 int
                    var inline3425 int = ref_get__Ref_3int(t1627)
                    t1628 = inline3425
                    var t1629 string = value__33.input
                    var t1630 int
                    var inline3423 int = _goml_runtime_core_string_len(t1629)
                    t1630 = inline3423
                    var t1631 bool = t1628 >= t1630
                    if t1631 {
                        var t1632 string
                        var inline3385 string = "unterminated array"
                        var inline3386 string = "" + inline3385
                        var inline3387 string = inline3386 + " at byte "
                        var inline3388 *ref_int_x = value__33.index
                        var inline3389 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3388)
                        var inline3390 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3389)
                        var inline3391 string = inline3387 + inline3390
                        t1632 = inline3391
                        var t1633 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1632,
                        }
                        return t1633
                    } else {
                        var t1635 string = value__33.input
                        var t1636 *ref_int_x = value__33.index
                        var t1637 int
                        var inline3421 int = ref_get__Ref_3int(t1636)
                        t1637 = inline3421
                        var t1638 uint8
                        var inline3419 uint8 = _goml_runtime_core_string_byte_get(t1635, t1637)
                        t1638 = inline3419
                        var t1639 bool
                        var inline3416 uint8 = 93
                        var inline3417 bool = t1638 == inline3416
                        t1639 = inline3417
                        if t1639 {
                            var t1640 *ref_int_x = value__33.index
                            var t1641 *ref_int_x = value__33.index
                            var t1642 int
                            var inline3395 int = ref_get__Ref_3int(t1641)
                            t1642 = inline3395
                            var t1643 int = t1642 + 1
                            ref_set__Ref_3int(t1640, t1643)
                            var t1644 _goml_m_std_p_json_p_Value = Array{
                                _0: vec_literal__8702,
                            }
                            var t1645 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1644,
                            }
                            return t1645
                        } else {
                            var t1647 string = value__33.input
                            var t1648 *ref_int_x = value__33.index
                            var t1649 int
                            var inline3414 int = ref_get__Ref_3int(t1648)
                            t1649 = inline3414
                            var t1650 uint8
                            var inline3412 uint8 = _goml_runtime_core_string_byte_get(t1647, t1649)
                            t1650 = inline3412
                            var t1651 bool
                            var inline3409 uint8 = 44
                            var inline3410 bool = t1650 == inline3409
                            t1651 = inline3410
                            if t1651 {
                                var t1652 *ref_int_x = value__33.index
                                var t1653 *ref_int_x = value__33.index
                                var t1654 int
                                var inline3399 int = ref_get__Ref_3int(t1653)
                                t1654 = inline3399
                                var t1655 int = t1654 + 1
                                ref_set__Ref_3int(t1652, t1655)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1657 string
                                var inline3401 string = "expected array separator"
                                var inline3402 string = "" + inline3401
                                var inline3403 string = inline3402 + " at byte "
                                var inline3404 *ref_int_x = value__33.index
                                var inline3405 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3404)
                                var inline3406 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3405)
                                var inline3407 string = inline3403 + inline3406
                                t1657 = inline3407
                                var t1658 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1657,
                                }
                                return t1658
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t1659 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x52,
                    }
                    return t1659
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1618
            }
        }
        var t1616 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1617 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1616,
        }
        return t1617
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1680 *ref_int_x = value__36.index
    var t1681 *ref_int_x = value__36.index
    var t1682 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1681)
    var t1683 int = t1682 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1680, t1683)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var vec_literal__9904 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1763 *ref_int_x = value__36.index
    var t1764 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1763)
    var t1765 string = value__36.input
    var t1766 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1765)
    var t1767 bool = t1764 < t1766
    var jp1756 bool
    if t1767 {
        var t1768 string = value__36.input
        var t1769 *ref_int_x = value__36.index
        var t1770 int
        var inline3432 int = ref_get__Ref_3int(t1769)
        t1770 = inline3432
        var t1771 uint8
        var inline3430 uint8 = _goml_runtime_core_string_byte_get(t1768, t1770)
        t1771 = inline3430
        var inline3427 uint8 = 125
        var inline3428 bool = t1771 == inline3427
        jp1756 = inline3428
    } else {
        jp1756 = false
    }
    if jp1756 {
        var t1757 *ref_int_x = value__36.index
        var t1758 *ref_int_x = value__36.index
        var t1759 int
        var inline3436 int = ref_get__Ref_3int(t1758)
        t1759 = inline3436
        var t1760 int = t1759 + 1
        ref_set__Ref_3int(t1757, t1760)
        var t1761 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__9904,
        }
        var t1762 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1761,
        }
        return t1762
    } else {
        Loop_loop1688:
        for {
            var t1689 *ref_int_x = value__36.index
            var t1690 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1689)
            var t1691 string = value__36.input
            var t1692 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1691)
            var t1693 bool = t1690 < t1692
            if t1693 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1695 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    jp1695 = x63
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1743 *ref_int_x = value__36.index
                    var t1744 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1743)
                    var t1745 string = value__36.input
                    var t1746 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1745)
                    var t1747 bool = t1744 >= t1746
                    var jp1735 bool
                    if t1747 {
                        jp1735 = true
                    } else {
                        var t1748 string = value__36.input
                        var t1749 *ref_int_x = value__36.index
                        var t1750 int
                        var inline3443 int = ref_get__Ref_3int(t1749)
                        t1750 = inline3443
                        var t1751 uint8
                        var inline3441 uint8 = _goml_runtime_core_string_byte_get(t1748, t1750)
                        t1751 = inline3441
                        var t1752 bool
                        var inline3438 uint8 = 58
                        var inline3439 bool = t1751 == inline3438
                        t1752 = inline3439
                        var t1753 bool = !t1752
                        jp1735 = t1753
                    }
                    if jp1735 {
                        var t1736 string
                        var inline3445 string = "expected object colon"
                        var inline3446 string = "" + inline3445
                        var inline3447 string = inline3446 + " at byte "
                        var inline3448 *ref_int_x = value__36.index
                        var inline3449 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3448)
                        var inline3450 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3449)
                        var inline3451 string = inline3447 + inline3450
                        t1736 = inline3451
                        var t1737 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1736,
                        }
                        return t1737
                    } else {
                        var t1738 *ref_int_x = value__36.index
                        var t1739 *ref_int_x = value__36.index
                        var t1740 int
                        var inline3455 int = ref_get__Ref_3int(t1739)
                        t1740 = inline3455
                        var t1741 int = t1740 + 1
                        ref_set__Ref_3int(t1738, t1741)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1698 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp1698 = x69
                            var t1699 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp1695,
                                _1: jp1698,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__9904, t1699)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1701 *ref_int_x = value__36.index
                            var t1702 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1701)
                            var t1703 string = value__36.input
                            var t1704 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1703)
                            var t1705 bool = t1702 >= t1704
                            if t1705 {
                                var t1706 string
                                var inline3457 string = "unterminated object"
                                var inline3458 string = "" + inline3457
                                var inline3459 string = inline3458 + " at byte "
                                var inline3460 *ref_int_x = value__36.index
                                var inline3461 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3460)
                                var inline3462 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3461)
                                var inline3463 string = inline3459 + inline3462
                                t1706 = inline3463
                                var t1707 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1706,
                                }
                                return t1707
                            } else {
                                var t1709 string = value__36.input
                                var t1710 *ref_int_x = value__36.index
                                var t1711 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1710)
                                var t1712 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1709, t1711)
                                var t1713 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t1712, 125)
                                if t1713 {
                                    var t1714 *ref_int_x = value__36.index
                                    var t1715 *ref_int_x = value__36.index
                                    var t1716 int
                                    var inline3467 int = ref_get__Ref_3int(t1715)
                                    t1716 = inline3467
                                    var t1717 int = t1716 + 1
                                    ref_set__Ref_3int(t1714, t1717)
                                    var t1718 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__9904,
                                    }
                                    var t1719 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1718,
                                    }
                                    return t1719
                                } else {
                                    var t1721 string = value__36.input
                                    var t1722 *ref_int_x = value__36.index
                                    var t1723 int
                                    var inline3478 int = ref_get__Ref_3int(t1722)
                                    t1723 = inline3478
                                    var t1724 uint8
                                    var inline3476 uint8 = _goml_runtime_core_string_byte_get(t1721, t1723)
                                    t1724 = inline3476
                                    var t1725 bool
                                    var inline3473 uint8 = 44
                                    var inline3474 bool = t1724 == inline3473
                                    t1725 = inline3474
                                    if t1725 {
                                        var t1726 *ref_int_x = value__36.index
                                        var t1727 *ref_int_x = value__36.index
                                        var t1728 int
                                        var inline3471 int = ref_get__Ref_3int(t1727)
                                        t1728 = inline3471
                                        var t1729 int = t1728 + 1
                                        ref_set__Ref_3int(t1726, t1729)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1731 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1732 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1731,
                                        }
                                        return t1732
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t1733 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x70,
                            }
                            return t1733
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var t1754 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x64,
                    }
                    return t1754
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1688
            }
        }
        var t1686 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1687 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1686,
        }
        return t1687
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1777 *ref_int_x = value__40.index
    var t1778 int
    var inline3508 int = ref_get__Ref_3int(t1777)
    t1778 = inline3508
    var t1779 string = value__40.input
    var t1780 int
    var inline3506 int = _goml_runtime_core_string_len(t1779)
    t1780 = inline3506
    var t1781 bool = t1778 >= t1780
    if t1781 {
        var t1782 string
        var inline3480 string = "expected JSON value"
        var inline3481 string = "" + inline3480
        var inline3482 string = inline3481 + " at byte "
        var inline3483 *ref_int_x = value__40.index
        var inline3484 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3483)
        var inline3485 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3484)
        var inline3486 string = inline3482 + inline3485
        t1782 = inline3486
        var t1783 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1782,
        }
        return t1783
    } else {
        var t1784 string = value__40.input
        var t1785 *ref_int_x = value__40.index
        var t1786 int
        var inline3504 int = ref_get__Ref_3int(t1785)
        t1786 = inline3504
        var mtmp77 uint8
        var inline3502 uint8 = _goml_runtime_core_string_byte_get(t1784, t1786)
        mtmp77 = inline3502
        switch mtmp77 {
        case 123:
            var t1789 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            return t1789
        case 91:
            var t1790 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            return t1790
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var t1793 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x79,
                }
                var t1794 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1793,
                }
                return t1794
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var t1795 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x80,
                }
                return t1795
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t1796 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t1797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1796)
            return t1797
        case 102:
            var t1798 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t1799 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1798)
            return t1799
        case 110:
            var t1800 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            return t1800
        default:
            var t1808 bool
            var inline3499 uint8 = 45
            var inline3500 bool = mtmp77 == inline3499
            t1808 = inline3500
            var jp1804 bool
            if t1808 {
                jp1804 = true
            } else {
                var inline3488 bool = mtmp77 >= 48
                if inline3488 {
                    var inline3489 bool = mtmp77 <= 57
                    jp1804 = inline3489
                } else {
                    jp1804 = false
                }
            }
            if jp1804 {
                var t1805 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                return t1805
            } else {
                var t1806 string
                var inline3491 string = "unexpected JSON token"
                var inline3492 string = "" + inline3491
                var inline3493 string = inline3492 + " at byte "
                var inline3494 *ref_int_x = value__40.index
                var inline3495 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3494)
                var inline3496 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3495)
                var inline3497 string = inline3493 + inline3496
                t1806 = inline3497
                var t1807 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1806,
                }
                return t1807
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__45 _goml_m_std_p_json_p_JsonParser
    var inline3524 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline3525 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__44,
        index: inline3524,
    }
    parser__45 = inline3525
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1813 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1813 = x82
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1816 *ref_int_x = parser__45.index
        var t1817 int
        var inline3522 int = ref_get__Ref_3int(t1816)
        t1817 = inline3522
        var t1818 int
        var inline3520 int = _goml_runtime_core_string_len(input__44)
        t1818 = inline3520
        var t1819 bool
        var inline3518 bool = t1817 == t1818
        t1819 = inline3518
        if t1819 {
            var t1820 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp1813,
            }
            return t1820
        } else {
            var t1821 string
            var inline3510 string = "trailing JSON data"
            var inline3511 string = "" + inline3510
            var inline3512 string = inline3511 + " at byte "
            var inline3513 *ref_int_x = parser__45.index
            var inline3514 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3513)
            var inline3515 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3514)
            var inline3516 string = inline3512 + inline3515
            t1821 = inline3516
            var t1822 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1821,
            }
            return t1822
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t1823 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x83,
        }
        return t1823
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1837:
    for {
        var t1838 bool = for_index86 < for_limit87
        if t1838 {
            var for_item88 int = for_index86
            var t1839 int = for_index86 + 1
            for_index86 = t1839
            var byte__52 uint8
            var inline3586 uint8 = _goml_runtime_core_string_byte_get(value__49, for_item88)
            byte__52 = inline3586
            var t1892 bool
            var inline3583 uint8 = 34
            var inline3584 bool = byte__52 == inline3583
            t1892 = inline3584
            var jp1890 bool
            if t1892 {
                jp1890 = true
            } else {
                var inline3530 uint8 = 92
                var inline3531 bool = byte__52 == inline3530
                jp1890 = inline3531
            }
            var jp1887 bool
            if jp1890 {
                jp1887 = true
            } else {
                var inline3533 uint8 = 8
                var inline3534 bool = byte__52 == inline3533
                jp1887 = inline3534
            }
            var jp1884 bool
            if jp1887 {
                jp1884 = true
            } else {
                var inline3536 uint8 = 9
                var inline3537 bool = byte__52 == inline3536
                jp1884 = inline3537
            }
            var jp1881 bool
            if jp1884 {
                jp1881 = true
            } else {
                var inline3539 uint8 = 10
                var inline3540 bool = byte__52 == inline3539
                jp1881 = inline3540
            }
            var jp1878 bool
            if jp1881 {
                jp1878 = true
            } else {
                var inline3542 uint8 = 12
                var inline3543 bool = byte__52 == inline3542
                jp1878 = inline3543
            }
            var jp1875 bool
            if jp1878 {
                jp1875 = true
            } else {
                var inline3545 uint8 = 13
                var inline3546 bool = byte__52 == inline3545
                jp1875 = inline3546
            }
            var jp1842 bool
            if jp1875 {
                jp1842 = true
            } else {
                var t1876 bool = byte__52 < 32
                jp1842 = t1876
            }
            if jp1842 {
                var t1871 bool = start__50 < for_item88
                if t1871 {
                    var t1872 string
                    var inline3548 string = string_byte_slice(value__49, start__50, for_item88)
                    t1872 = inline3548
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1872)
                } else {}
                var t1846 bool
                var inline3580 uint8 = 34
                var inline3581 bool = byte__52 == inline3580
                t1846 = inline3581
                if t1846 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1849 bool
                    var inline3577 uint8 = 92
                    var inline3578 bool = byte__52 == inline3577
                    t1849 = inline3578
                    if t1849 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1852 bool
                        var inline3574 uint8 = 8
                        var inline3575 bool = byte__52 == inline3574
                        t1852 = inline3575
                        if t1852 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1855 bool
                            var inline3571 uint8 = 9
                            var inline3572 bool = byte__52 == inline3571
                            t1855 = inline3572
                            if t1855 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1858 bool
                                var inline3568 uint8 = 10
                                var inline3569 bool = byte__52 == inline3568
                                t1858 = inline3569
                                if t1858 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1861 bool
                                    var inline3565 uint8 = 12
                                    var inline3566 bool = byte__52 == inline3565
                                    t1861 = inline3566
                                    if t1861 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1864 bool
                                        var inline3562 uint8 = 13
                                        var inline3563 bool = byte__52 == inline3562
                                        t1864 = inline3563
                                        if t1864 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1866 uint8 = byte__52 / 16
                                            var t1867 rune
                                            var inline3559 int = int(uint8(t1866))
                                            var inline3560 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline3559)
                                            t1867 = inline3560
                                            var inline3556 string = _goml_m_inherent_i_char_i_char_i_to__string(t1867)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline3556)
                                            var t1868_rhs uint8 = 16
                                            var t1868 uint8 = byte__52 % t1868_rhs
                                            var t1869 rune
                                            var inline3553 int = int(uint8(t1868))
                                            var inline3554 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline3553)
                                            t1869 = inline3554
                                            var inline3550 string = _goml_m_inherent_i_char_i_char_i_to__string(t1869)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline3550)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1845 int = for_item88 + 1
                start__50 = t1845
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop1837
        }
    }
    var t1832 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1833 bool = start__50 < t1832
    if t1833 {
        var t1834 int
        var inline3590 int = _goml_runtime_core_string_len(value__49)
        t1834 = inline3590
        var t1835 string
        var inline3588 string = string_byte_slice(value__49, start__50, t1834)
        t1835 = inline3588
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1835)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__53 _goml_m_std_p_text_p_StringBuilder, value__54 _goml_m_std_p_json_p_Value) struct{} {
    switch value__54.(type) {
    case Object:
        var x97 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__54.(Object)._0
        var inline3604 rune = 123
        var inline3605 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3604)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3605)
        var index__56 int = 0
        var for_limit104 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97)
        var for_index105 int = 0
        Loop_loop1898:
        for {
            var t1899 bool = for_index105 < for_limit104
            if t1899 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97, for_index105)
                var t1900 int = for_index105 + 1
                for_index105 = t1900
                var t1906 bool = index__56 > 0
                if t1906 {
                    var inline3592 rune = 44
                    var inline3593 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3592)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3593)
                } else {}
                var t1902 string = for_item106._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1902)
                var inline3596 rune = 58
                var inline3597 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3596)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3597)
                var t1903 _goml_m_std_p_json_p_Value = for_item106._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1903)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1904 int = compound_old112 + compound_value113
                index__56 = t1904
                continue
            } else {
                break Loop_loop1898
            }
        }
        var inline3600 rune = 125
        var inline3601 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3600)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3601)
        return struct{}{}
    case Array:
        var x98 *_goml_vec__goml_m_std_p_json_p_Value = value__54.(Array)._0
        var inline3616 rune = 91
        var inline3617 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3616)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3617)
        var index__59 int = 0
        var for_limit118 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x98)
        var for_index119 int = 0
        Loop_loop1910:
        for {
            var t1911 bool = for_index119 < for_limit118
            if t1911 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x98, for_index119)
                var t1912 int = for_index119 + 1
                for_index119 = t1912
                var t1916 bool = index__59 > 0
                if t1916 {
                    var inline3608 rune = 44
                    var inline3609 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3608)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3609)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, for_item120)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1914 int = compound_old124 + compound_value125
                index__59 = t1914
                continue
            } else {
                break Loop_loop1910
            }
        }
        var inline3612 rune = 93
        var inline3613 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3612)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3613)
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
        var jp1921 string
        if x101 {
            jp1921 = "true"
        } else {
            jp1921 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1921)
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
    var inline3626 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline3627 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline3626,
    }
    builder__65 = inline3627
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var inline3620 *_goml_vec_uint8 = builder__65.values
    var inline3621 Tuple2_4bool_6string = string_from_utf8(inline3620)
    var inline3623 string = inline3621._1
    return inline3623
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129)
        var for_index136 int = 0
        Loop_loop1932:
        for {
            var t1933 bool = for_index136 < for_limit135
            if t1933 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129, for_index136)
                var t1934 int = for_index136 + 1
                for_index136 = t1934
                var t1936 string = for_item137._0
                var t1937 bool
                var inline3629 bool = t1936 == name__67
                t1937 = inline3629
                if t1937 {
                    var t1938 _goml_m_std_p_json_p_Value = for_item137._1
                    var t1939 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1938,
                    }
                    return t1939
                } else {
                    continue
                }
            } else {
                break Loop_loop1932
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var t1949 int
    var inline3648 int = _goml_runtime_core_string_len(value__72)
    t1949 = inline3648
    var t1950 bool
    var inline3645 int = 0
    var inline3646 bool = t1949 == inline3645
    t1950 = inline3646
    if t1950 {
        return Option__int_None{}
    } else {
        var t1951 uint8
        var inline3642 int = 0
        var inline3643 uint8 = _goml_runtime_core_string_byte_get(value__72, inline3642)
        t1951 = inline3643
        var negative__73 bool
        var inline3639 uint8 = 45
        var inline3640 bool = t1951 == inline3639
        negative__73 = inline3640
        var jp1953 int
        if negative__73 {
            jp1953 = 1
        } else {
            jp1953 = 0
        }
        var index__74 int = jp1953
        var result__75 int = 0
        var t1974 int
        var inline3637 int = _goml_runtime_core_string_len(value__72)
        t1974 = inline3637
        var t1975 bool
        var inline3635 bool = index__74 == t1974
        t1975 = inline3635
        if t1975 {
            return Option__int_None{}
        } else {
            Loop_loop1960:
            for {
                var t1961 int
                var inline3633 int = _goml_runtime_core_string_len(value__72)
                t1961 = inline3633
                var t1962 bool = index__74 < t1961
                if t1962 {
                    var byte__76 uint8
                    var inline3631 uint8 = _goml_runtime_core_string_byte_get(value__72, index__74)
                    byte__76 = inline3631
                    var t1972 bool = byte__76 < 48
                    var jp1967 bool
                    if t1972 {
                        jp1967 = true
                    } else {
                        var t1973 bool = byte__76 > 57
                        jp1967 = t1973
                    }
                    if jp1967 {
                        return Option__int_None{}
                    } else {
                        var t1968 int = result__75 * 10
                        var t1969 uint8 = byte__76 - 48
                        var t1970 int = int(uint8(t1969))
                        var t1971 int = t1968 + t1970
                        result__75 = t1971
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1964 int = compound_old148 + compound_value149
                        index__74 = t1964
                        continue
                    }
                } else {
                    break Loop_loop1960
                }
            }
            var jp1957 int
            if negative__73 {
                var t1959 int = 0 - result__75
                jp1957 = t1959
            } else {
                jp1957 = result__75
            }
            var t1958 Option__int = Option__int_Some{
                _0: jp1957,
            }
            return t1958
        }
    }
}

func main0() struct{} {
    var mtmp172 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp2132 _goml_m_std_p_json_p_Value
    switch mtmp172.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x173 _goml_m_std_p_json_p_Value = mtmp172.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp2132 = x173
        var mtmp176 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2132, "name")
        switch mtmp176.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline3735 string = "missing name"
            var inline3736 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3735)
            _goml_runtime_core_string_println(inline3736)
            var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2132, "version")
            switch mtmp181.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline3750 string = "missing version"
                var inline3751 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3750)
                _goml_runtime_core_string_println(inline3751)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp183 Option__int
                switch x182.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline3761 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline3763 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline3761)
                    mtmp183 = inline3763
                default:
                    mtmp183 = Option__int_None{}
                }
                switch mtmp183.(type) {
                case Option__int_None:
                    var inline3754 string = "invalid version"
                    var inline3755 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3754)
                    _goml_runtime_core_string_println(inline3755)
                case Option__int_Some:
                    var x184 int = mtmp183.(Option__int_Some)._0
                    var inline3758 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                    _goml_runtime_core_string_println(inline3758)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2132, "stable")
            switch mtmp186.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline3765 string = "missing stable"
                var inline3766 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3765)
                _goml_runtime_core_string_println(inline3766)
                var t2136 string = _goml_m_std_p_json_p_encode(jp2132)
                println__T_string(t2136)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field3933 bool
                switch x187.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline3776 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field3933 = inline3776
                    var inline3773 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3933)
                    _goml_runtime_core_string_println(inline3773)
                    var t2136 string = _goml_m_std_p_json_p_encode(jp2132)
                    println__T_string(t2136)
                    return struct{}{}
                default:
                    var inline3769 string = "invalid stable"
                    var inline3770 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3769)
                    _goml_runtime_core_string_println(inline3770)
                    var t2136 string = _goml_m_std_p_json_p_encode(jp2132)
                    println__T_string(t2136)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x177 _goml_m_std_p_json_p_Value = mtmp176.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field3939 string
            switch x177.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline3746 string = x177.(_goml_m_std_p_json_p_Value_String)._0
                commute_field3939 = inline3746
                var inline3743 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field3939)
                _goml_runtime_core_string_println(inline3743)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2132, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3750 string = "missing version"
                    var inline3751 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3750)
                    _goml_runtime_core_string_println(inline3751)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline3761 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline3763 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline3761)
                        mtmp183 = inline3763
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline3754 string = "invalid version"
                        var inline3755 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3754)
                        _goml_runtime_core_string_println(inline3755)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline3758 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline3758)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2132, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3765 string = "missing stable"
                    var inline3766 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3765)
                    _goml_runtime_core_string_println(inline3766)
                    var t2136 string = _goml_m_std_p_json_p_encode(jp2132)
                    println__T_string(t2136)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field3933 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline3776 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field3933 = inline3776
                        var inline3773 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3933)
                        _goml_runtime_core_string_println(inline3773)
                        var t2136 string = _goml_m_std_p_json_p_encode(jp2132)
                        println__T_string(t2136)
                        return struct{}{}
                    default:
                        var inline3769 string = "invalid stable"
                        var inline3770 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3769)
                        _goml_runtime_core_string_println(inline3770)
                        var t2136 string = _goml_m_std_p_json_p_encode(jp2132)
                        println__T_string(t2136)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline3739 string = "invalid name"
                var inline3740 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3739)
                _goml_runtime_core_string_println(inline3740)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2132, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3750 string = "missing version"
                    var inline3751 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3750)
                    _goml_runtime_core_string_println(inline3751)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline3761 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline3763 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline3761)
                        mtmp183 = inline3763
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline3754 string = "invalid version"
                        var inline3755 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3754)
                        _goml_runtime_core_string_println(inline3755)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline3758 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline3758)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2132, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3765 string = "missing stable"
                    var inline3766 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3765)
                    _goml_runtime_core_string_println(inline3766)
                    var t2136 string = _goml_m_std_p_json_p_encode(jp2132)
                    println__T_string(t2136)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field3933 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline3776 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field3933 = inline3776
                        var inline3773 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3933)
                        _goml_runtime_core_string_println(inline3773)
                        var t2136 string = _goml_m_std_p_json_p_encode(jp2132)
                        println__T_string(t2136)
                        return struct{}{}
                    default:
                        var inline3769 string = "invalid stable"
                        var inline3770 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3769)
                        _goml_runtime_core_string_println(inline3770)
                        var t2136 string = _goml_m_std_p_json_p_encode(jp2132)
                        println__T_string(t2136)
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
        var x174 string = mtmp172.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var inline3732 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x174)
        _goml_runtime_core_string_println(inline3732)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t2158 string = _goml_runtime_core_int_to_string(self__34)
    return t2158
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline3780 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline3781 bool = inline3780._0
    var inline3782 rune = inline3780._1
    if inline3781 {
        return inline3782
    } else {
        var inline3786 rune = _goml_runtime_core_string_get("", -1)
        return inline3786
    }
}

func _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(self__113 uint8, other__114 uint8) bool {
    var t2232 bool = self__113 == other__114
    return t2232
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t2235 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t2235
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop2242:
    for {
        var t2243 int
        var inline3788 int = _goml_runtime_core_string_len(x12)
        t2243 = inline3788
        var t2244 bool = index__26 < t2243
        if t2244 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t2246 int = compound_old17 + x16
                index__26 = t2246
                continue
            } else {
                var t2248 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t2248
            }
        } else {
            break Loop_loop2242
        }
    }
    var t2241 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t2241
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline3790 uint32 = uint32(rune(self__36))
    var inline3791 bool = utf8_valid_scalar(inline3790)
    if inline3791 {
        var inline3792 string = _goml_runtime_core_char_to_string(self__36)
        return inline3792
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t2287 int = _goml_runtime_core_string_len(self__38)
    return t2287
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t2290 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t2290
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline3808 bool = string_is_char_boundary(self__43, start__44)
    var inline3810 bool
    if inline3808 {
        var inline3813 bool = string_is_char_boundary(self__43, end__45)
        inline3810 = inline3813
    } else {
        inline3810 = false
    }
    if inline3810 {
        var inline3811 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline3811
    } else {
        var inline3812 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline3812
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t2319 *ref_int_x = ref__Ref_3int(value__257)
    return t2319
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t2322 int = ref_get__Ref_3int(self__258)
    return t2322
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__259 *ref_int_x, value__260 int) struct{} {
    ref_set__Ref_3int(self__259, value__260)
    return struct{}{}
}

func char_from_uint32(value__32 uint32) Option__char {
    var t2329 bool
    var inline3825 bool = value__32 <= 1114111
    if inline3825 {
        var inline3826 bool = value__32 >= 55296
        var inline3828 bool
        if inline3826 {
            var inline3830 bool = value__32 <= 57343
            inline3828 = inline3830
        } else {
            inline3828 = false
        }
        var inline3829 bool = !inline3828
        t2329 = inline3829
    } else {
        t2329 = false
    }
    if t2329 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t2330 Option__char = Option__char_Some{
            _0: x24,
        }
        return t2330
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t2333 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t2333
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__176 *_goml_vec__goml_m_std_p_json_p_Value, elem__177 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__176, elem__177)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t2338 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t2338
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__176 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__177 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__176, elem__177)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t2395 string
    t2395 = value__31
    _goml_runtime_core_string_println(t2395)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t2526 bool = index__6 < 0
    var jp2524 bool
    if t2526 {
        jp2524 = true
    } else {
        var t2527 bool = index__6 >= length__7
        jp2524 = t2527
    }
    if jp2524 {
        var inline3837 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline3837
    } else {
        var t2411 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t2411))
        var t2414 bool = first__8 < 128
        if t2414 {
            var inline3839 int = 1
            var inline3840 Option__char = char_from_uint32(first__8)
            switch inline3840.(type) {
            case Option__char_None:
                var inline3841 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline3841
            case Option__char_Some:
                var inline3842 rune = inline3840.(Option__char_Some)._0
                var inline3844 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline3842,
                    _2: inline3839,
                }
                return inline3844
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t2418 bool = first__8 < 194
            if t2418 {
                var inline3846 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline3846
            } else {
                var t2422 bool = first__8 < 224
                if t2422 {
                    var t2435 int = length__7 - index__6
                    var t2436 bool = t2435 < 2
                    if t2436 {
                        var inline3848 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline3848
                    } else {
                        var t2424 int = index__6 + 1
                        var t2425 uint8
                        var inline3862 uint8 = _goml_runtime_core_string_byte_get(value__5, t2424)
                        t2425 = inline3862
                        var second__9 uint32 = uint32(uint8(t2425))
                        var t2428 bool
                        var inline3859 bool = second__9 < 128
                        if inline3859 {
                            t2428 = true
                        } else {
                            var inline3860 bool = second__9 > 191
                            t2428 = inline3860
                        }
                        if t2428 {
                            var inline3850 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline3850
                        } else {
                            var t2430_rhs uint32 = 31
                            var t2430 uint32 = first__8 & t2430_rhs
                            var t2431_rhs int = 6
                            var t2431 uint32 = t2430 << t2431_rhs
                            var t2432_rhs uint32 = 63
                            var t2432 uint32 = second__9 & t2432_rhs
                            var t2433 uint32 = t2431 | t2432
                            var inline3852 int = 2
                            var inline3853 Option__char = char_from_uint32(t2433)
                            switch inline3853.(type) {
                            case Option__char_None:
                                var inline3854 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline3854
                            case Option__char_Some:
                                var inline3855 rune = inline3853.(Option__char_Some)._0
                                var inline3857 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline3855,
                                    _2: inline3852,
                                }
                                return inline3857
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t2440 bool = first__8 < 240
                    if t2440 {
                        var t2473 int = length__7 - index__6
                        var t2474 bool = t2473 < 3
                        if t2474 {
                            var inline3864 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline3864
                        } else {
                            var t2442 int = index__6 + 1
                            var t2443 uint8
                            var inline3879 uint8 = _goml_runtime_core_string_byte_get(value__5, t2442)
                            t2443 = inline3879
                            var second__10 uint32 = uint32(uint8(t2443))
                            var t2444 int = index__6 + 2
                            var t2445 uint8
                            var inline3877 uint8 = _goml_runtime_core_string_byte_get(value__5, t2444)
                            t2445 = inline3877
                            var third__11 uint32 = uint32(uint8(t2445))
                            var t2471 bool = utf8_invalid_continuation(second__10)
                            var jp2466 bool
                            if t2471 {
                                jp2466 = true
                            } else {
                                var inline3866 bool = third__11 < 128
                                if inline3866 {
                                    jp2466 = true
                                } else {
                                    var inline3867 bool = third__11 > 191
                                    jp2466 = inline3867
                                }
                            }
                            var jp2460 bool
                            if jp2466 {
                                jp2460 = true
                            } else {
                                var t2469 bool
                                var inline3869 uint32 = 224
                                var inline3870 bool = first__8 == inline3869
                                t2469 = inline3870
                                if t2469 {
                                    var t2470 bool = second__10 < 160
                                    jp2460 = t2470
                                } else {
                                    jp2460 = false
                                }
                            }
                            var jp2449 bool
                            if jp2460 {
                                jp2449 = true
                            } else {
                                var t2463 bool
                                var inline3872 uint32 = 237
                                var inline3873 bool = first__8 == inline3872
                                t2463 = inline3873
                                if t2463 {
                                    var t2464 bool = second__10 >= 160
                                    jp2449 = t2464
                                } else {
                                    jp2449 = false
                                }
                            }
                            if jp2449 {
                                var inline3875 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline3875
                            } else {
                                var t2451_rhs uint32 = 15
                                var t2451 uint32 = first__8 & t2451_rhs
                                var t2452_rhs int = 12
                                var t2452 uint32 = t2451 << t2452_rhs
                                var t2453_rhs uint32 = 63
                                var t2453 uint32 = second__10 & t2453_rhs
                                var t2454_rhs int = 6
                                var t2454 uint32 = t2453 << t2454_rhs
                                var t2455 uint32 = t2452 | t2454
                                var t2456_rhs uint32 = 63
                                var t2456 uint32 = third__11 & t2456_rhs
                                var t2457 uint32 = t2455 | t2456
                                var t2458 Tuple3_4bool_4char_3int = utf8_valid_decode(t2457, 3)
                                return t2458
                            }
                        }
                    } else {
                        var t2478 bool = first__8 < 245
                        if t2478 {
                            var t2519 int = length__7 - index__6
                            var t2520 bool = t2519 < 4
                            if t2520 {
                                var t2521 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t2521
                            } else {
                                var t2480 int = index__6 + 1
                                var t2481 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t2480)
                                var second__12 uint32 = uint32(uint8(t2481))
                                var t2482 int = index__6 + 2
                                var t2483 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t2482)
                                var third__13 uint32 = uint32(uint8(t2483))
                                var t2484 int = index__6 + 3
                                var t2485 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t2484)
                                var fourth__14 uint32 = uint32(uint8(t2485))
                                var t2517 bool = utf8_invalid_continuation(second__12)
                                var jp2515 bool
                                if t2517 {
                                    jp2515 = true
                                } else {
                                    var t2518 bool = utf8_invalid_continuation(third__13)
                                    jp2515 = t2518
                                }
                                var jp2509 bool
                                if jp2515 {
                                    jp2509 = true
                                } else {
                                    var t2516 bool = utf8_invalid_continuation(fourth__14)
                                    jp2509 = t2516
                                }
                                var jp2503 bool
                                if jp2509 {
                                    jp2503 = true
                                } else {
                                    var t2512 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t2512 {
                                        var t2513 bool = second__12 < 144
                                        jp2503 = t2513
                                    } else {
                                        jp2503 = false
                                    }
                                }
                                var jp2489 bool
                                if jp2503 {
                                    jp2489 = true
                                } else {
                                    var t2506 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t2506 {
                                        var t2507 bool = second__12 > 143
                                        jp2489 = t2507
                                    } else {
                                        jp2489 = false
                                    }
                                }
                                if jp2489 {
                                    var t2490 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t2490
                                } else {
                                    var t2491_rhs uint32 = 7
                                    var t2491 uint32 = first__8 & t2491_rhs
                                    var t2492_rhs int = 18
                                    var t2492 uint32 = t2491 << t2492_rhs
                                    var t2493_rhs uint32 = 63
                                    var t2493 uint32 = second__12 & t2493_rhs
                                    var t2494_rhs int = 12
                                    var t2494 uint32 = t2493 << t2494_rhs
                                    var t2495 uint32 = t2492 | t2494
                                    var t2496_rhs uint32 = 63
                                    var t2496 uint32 = third__13 & t2496_rhs
                                    var t2497_rhs int = 6
                                    var t2497 uint32 = t2496 << t2497_rhs
                                    var t2498 uint32 = t2495 | t2497
                                    var t2499_rhs uint32 = 63
                                    var t2499 uint32 = fourth__14 & t2499_rhs
                                    var t2500 uint32 = t2498 | t2499
                                    var t2501 Tuple3_4bool_4char_3int = utf8_valid_decode(t2500, 4)
                                    return t2501
                                }
                            }
                        } else {
                            var t2522 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t2522
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t2532 uint32 = uint32(rune(value__29))
    var t2533 bool
    var inline3881 bool = t2532 <= 1114111
    if inline3881 {
        var inline3882 bool = t2532 >= 55296
        var inline3884 bool
        if inline3882 {
            var inline3886 bool = t2532 <= 57343
            inline3884 = inline3886
        } else {
            inline3884 = false
        }
        var inline3885 bool = !inline3884
        t2533 = inline3885
    } else {
        t2533 = false
    }
    if t2533 {
        var t2534 string = _goml_runtime_core_char_to_string(value__29)
        return t2534
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t2549 bool = index__16 < 0
    var jp2540 bool
    if t2549 {
        jp2540 = true
    } else {
        var t2550 int
        var inline3888 int = _goml_runtime_core_string_len(value__15)
        t2550 = inline3888
        var t2551 bool = index__16 > t2550
        jp2540 = t2551
    }
    if jp2540 {
        return false
    } else {
        var t2543 int
        var inline3897 int = _goml_runtime_core_string_len(value__15)
        t2543 = inline3897
        var t2544 bool
        var inline3895 bool = index__16 == t2543
        t2544 = inline3895
        if t2544 {
            return true
        } else {
            var t2545 uint8
            var inline3893 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t2545 = inline3893
            var t2546_rhs uint8 = 192
            var t2546 uint8 = t2545 & t2546_rhs
            var t2547 bool
            var inline3890 uint8 = 128
            var inline3891 bool = t2546 == inline3890
            t2547 = inline3891
            var t2548 bool = !t2547
            return t2548
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t2560 bool = string_is_char_boundary(value__21, start__22)
    var jp2557 bool
    if t2560 {
        var t2561 bool = string_is_char_boundary(value__21, end__23)
        jp2557 = t2561
    } else {
        jp2557 = false
    }
    if jp2557 {
        var t2558 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t2558
    } else {
        var t2559 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t2559
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t2568 bool = value__4 <= 1114111
    if t2568 {
        var t2572 bool = value__4 >= 55296
        var jp2570 bool
        if t2572 {
            var t2573 bool = value__4 <= 57343
            jp2570 = t2573
        } else {
            jp2570 = false
        }
        var t2571 bool = !jp2570
        return t2571
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t2578 string = _goml_runtime_core_int_to_string(self__69)
    return t2578
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t2581 string = _goml_runtime_core_bool_to_string(self__66)
    return t2581
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t2584 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t2584
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field3942 rune
    var inline3901 bool = utf8_valid_scalar(value__0)
    if inline3901 {
        var inline3902 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline3904 rune = inline3902._1
        commute_field3942 = inline3904
        var t2590 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field3942,
            _2: width__1,
        }
        return t2590
    } else {
        var inline3899 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline3899
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t2595 bool = value__3 < 128
    if t2595 {
        return true
    } else {
        var t2596 bool = value__3 > 191
        return t2596
    }
}

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t2599 bool = self__117 == other__118
    return t2599
}

func main() {
    main0()
}
