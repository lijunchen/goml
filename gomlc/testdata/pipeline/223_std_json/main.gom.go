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
    var inline2819 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline2819
    var t788 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t788
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline2834 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline2834
    var t802 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t802, length__5)
    var for_index1 int = 0
    Loop_loop804:
    for {
        var t805 bool = for_index1 < length__5
        if t805 {
            var for_item3 int = for_index1
            var t806 int = for_index1 + 1
            for_index1 = t806
            var t807 *_goml_vec_uint8 = self__3.values
            var t808 uint8
            var inline2830 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t808 = inline2830
            vec_push__Vec_5uint8(t807, t808)
            continue
        } else {
            break Loop_loop804
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t811 string
    var inline2836 string = char_to_string(value__8)
    t811 = inline2836
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t811)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__1 _goml_m_std_p_json_p_JsonParser, message__2 string) string {
    var t1076 string = "" + message__2
    var t1077 string = t1076 + " at byte "
    var t1078 *ref_int_x = value__1.index
    var t1079 int
    var inline3061 int = ref_get__Ref_3int(t1078)
    t1079 = inline3061
    var t1080 string
    var inline3059 string = _goml_runtime_core_int_to_string(t1079)
    t1080 = inline3059
    var t1081 string = t1077 + t1080
    return t1081
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__4 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop1096:
    for {
        var t1104 *ref_int_x = value__4.index
        var t1105 int
        var inline3094 int = ref_get__Ref_3int(t1104)
        t1105 = inline3094
        var t1106 string = value__4.input
        var t1107 int
        var inline3092 int = _goml_runtime_core_string_len(t1106)
        t1107 = inline3092
        var t1108 bool = t1105 < t1107
        var jp1098 bool
        if t1108 {
            var t1109 string = value__4.input
            var t1110 *ref_int_x = value__4.index
            var t1111 int
            var inline3086 int = ref_get__Ref_3int(t1110)
            t1111 = inline3086
            var t1112 uint8
            var inline3084 uint8 = _goml_runtime_core_string_byte_get(t1109, t1111)
            t1112 = inline3084
            var inline3075 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1112, 9)
            var inline3077 bool
            if inline3075 {
                inline3077 = true
            } else {
                var inline3082 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1112, 10)
                inline3077 = inline3082
            }
            var inline3079 bool
            if inline3077 {
                inline3079 = true
            } else {
                var inline3081 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1112, 13)
                inline3079 = inline3081
            }
            if inline3079 {
                jp1098 = true
            } else {
                var inline3080 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1112, 32)
                jp1098 = inline3080
            }
        } else {
            jp1098 = false
        }
        if jp1098 {
            var t1099 *ref_int_x = value__4.index
            var t1100 *ref_int_x = value__4.index
            var t1101 int
            var inline3090 int = ref_get__Ref_3int(t1100)
            t1101 = inline3090
            var t1102 int = t1101 + 1
            ref_set__Ref_3int(t1099, t1102)
            continue
        } else {
            break Loop_loop1096
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__5 uint8) Option__uint32 {
    var t1143 bool = value__5 >= 48
    var jp1119 bool
    if t1143 {
        var t1144 bool = value__5 <= 57
        jp1119 = t1144
    } else {
        jp1119 = false
    }
    if jp1119 {
        var t1120 uint8 = value__5 - 48
        var t1121 uint32 = uint32(uint8(t1120))
        var t1122 Option__uint32 = Option__uint32_Some{
            _0: t1121,
        }
        return t1122
    } else {
        var t1141 bool = value__5 >= 65
        var jp1126 bool
        if t1141 {
            var t1142 bool = value__5 <= 70
            jp1126 = t1142
        } else {
            jp1126 = false
        }
        if jp1126 {
            var t1127 uint8 = value__5 - 65
            var t1128 uint8 = t1127 + 10
            var t1129 uint32 = uint32(uint8(t1128))
            var t1130 Option__uint32 = Option__uint32_Some{
                _0: t1129,
            }
            return t1130
        } else {
            var t1139 bool = value__5 >= 97
            var jp1134 bool
            if t1139 {
                var t1140 bool = value__5 <= 102
                jp1134 = t1140
            } else {
                jp1134 = false
            }
            if jp1134 {
                var t1135 uint8 = value__5 - 97
                var t1136 uint8 = t1135 + 10
                var t1137 uint32 = uint32(uint8(t1136))
                var t1138 Option__uint32 = Option__uint32_Some{
                    _0: t1137,
                }
                return t1138
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__6 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t1149 *ref_int_x = value__6.index
    var t1150 int
    var inline3122 int = ref_get__Ref_3int(t1149)
    t1150 = inline3122
    var t1151 int = t1150 + 4
    var t1152 string = value__6.input
    var t1153 int
    var inline3120 int = _goml_runtime_core_string_len(t1152)
    t1153 = inline3120
    var t1154 bool = t1151 > t1153
    if t1154 {
        var t1155 string
        var inline3096 string = "incomplete unicode escape"
        var inline3097 string = "" + inline3096
        var inline3098 string = inline3097 + " at byte "
        var inline3099 *ref_int_x = value__6.index
        var inline3100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3099)
        var inline3101 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3100)
        var inline3102 string = inline3098 + inline3101
        t1155 = inline3102
        var t1156 Result__uint32__string = Result__uint32__string_Err{
            _0: t1155,
        }
        return t1156
    } else {
        var result__7_source int = 0
        var result__7 uint32 = uint32(int(result__7_source))
        var for_index0 int = 0
        var for_limit1 int = 4
        Loop_loop1163:
        for {
            var t1164 bool = for_index0 < for_limit1
            if t1164 {
                var for_item2 int = for_index0
                var t1165 int = for_index0 + 1
                for_index0 = t1165
                var t1166 string = value__6.input
                var t1167 *ref_int_x = value__6.index
                var t1168 int
                var inline3114 int = ref_get__Ref_3int(t1167)
                t1168 = inline3114
                var t1169 int = t1168 + for_item2
                var t1170 uint8
                var inline3112 uint8 = _goml_runtime_core_string_byte_get(t1166, t1169)
                t1170 = inline3112
                var mtmp4 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t1170)
                switch mtmp4.(type) {
                case Option__uint32_None:
                    var t1172 string
                    var inline3104 string = "invalid unicode escape"
                    var inline3105 string = "" + inline3104
                    var inline3106 string = inline3105 + " at byte "
                    var inline3107 *ref_int_x = value__6.index
                    var inline3108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3107)
                    var inline3109 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3108)
                    var inline3110 string = inline3106 + inline3109
                    t1172 = inline3110
                    var t1173 Result__uint32__string = Result__uint32__string_Err{
                        _0: t1172,
                    }
                    return t1173
                case Option__uint32_Some:
                    var x5 uint32 = mtmp4.(Option__uint32_Some)._0
                    var t1174 uint32 = result__7 * 16
                    var t1175 uint32 = t1174 + x5
                    result__7 = t1175
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1163
            }
        }
        var t1158 *ref_int_x = value__6.index
        var t1159 *ref_int_x = value__6.index
        var t1160 int
        var inline3118 int = ref_get__Ref_3int(t1159)
        t1160 = inline3118
        var t1161 int = t1160 + 4
        ref_set__Ref_3int(t1158, t1161)
        var t1162 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__7,
        }
        return t1162
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__10 _goml_m_std_p_json_p_JsonParser, builder__11 _goml_m_std_p_text_p_StringBuilder, codepoint__12 uint32) Result__unit__string {
    var commute_field3887 rune
    var inline3135 bool = utf8_valid_scalar(codepoint__12)
    if inline3135 {
        var inline3136 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__12)
        var inline3138 rune = inline3136._1
        commute_field3887 = inline3138
        var inline3132 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field3887)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__11, inline3132)
        var t1182 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t1182
    } else {
        var t1180 string
        var inline3124 string = "invalid unicode codepoint"
        var inline3125 string = "" + inline3124
        var inline3126 string = inline3125 + " at byte "
        var inline3127 *ref_int_x = value__10.index
        var inline3128 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3127)
        var inline3129 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3128)
        var inline3130 string = inline3126 + inline3129
        t1180 = inline3130
        var t1181 Result__unit__string = Result__unit__string_Err{
            _0: t1180,
        }
        return t1181
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__14 _goml_m_std_p_json_p_JsonParser, builder__15 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp12 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
    var jp1186 uint32
    switch mtmp12.(type) {
    case Result__uint32__string_Ok:
        var x13 uint32 = mtmp12.(Result__uint32__string_Ok)._0
        jp1186 = x13
        var t1248 bool = jp1186 >= 55296
        var jp1190 bool
        if t1248 {
            var t1249 bool = jp1186 <= 56319
            jp1190 = t1249
        } else {
            jp1190 = false
        }
        if jp1190 {
            var t1227 *ref_int_x = value__14.index
            var t1228 int
            var inline3186 int = ref_get__Ref_3int(t1227)
            t1228 = inline3186
            var t1229 int = t1228 + 2
            var t1230 string = value__14.input
            var t1231 int
            var inline3184 int = _goml_runtime_core_string_len(t1230)
            t1231 = inline3184
            var t1232 bool = t1229 > t1231
            var jp1219 bool
            if t1232 {
                jp1219 = true
            } else {
                var t1233 string = value__14.input
                var t1234 *ref_int_x = value__14.index
                var t1235 int
                var inline3147 int = ref_get__Ref_3int(t1234)
                t1235 = inline3147
                var t1236 uint8
                var inline3145 uint8 = _goml_runtime_core_string_byte_get(t1233, t1235)
                t1236 = inline3145
                var t1237 bool
                var inline3142 uint8 = 92
                var inline3143 bool = t1236 == inline3142
                t1237 = inline3143
                var t1238 bool = !t1237
                jp1219 = t1238
            }
            var jp1194 bool
            if jp1219 {
                jp1194 = true
            } else {
                var t1220 string = value__14.input
                var t1221 *ref_int_x = value__14.index
                var t1222 int
                var inline3154 int = ref_get__Ref_3int(t1221)
                t1222 = inline3154
                var t1223 int = t1222 + 1
                var t1224 uint8
                var inline3152 uint8 = _goml_runtime_core_string_byte_get(t1220, t1223)
                t1224 = inline3152
                var t1225 bool
                var inline3149 uint8 = 117
                var inline3150 bool = t1224 == inline3149
                t1225 = inline3150
                var t1226 bool = !t1225
                jp1194 = t1226
            }
            if jp1194 {
                var t1195 string
                var inline3156 string = "missing low surrogate"
                var inline3157 string = "" + inline3156
                var inline3158 string = inline3157 + " at byte "
                var inline3159 *ref_int_x = value__14.index
                var inline3160 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3159)
                var inline3161 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3160)
                var inline3162 string = inline3158 + inline3161
                t1195 = inline3162
                var t1196 Result__unit__string = Result__unit__string_Err{
                    _0: t1195,
                }
                return t1196
            } else {
                var t1197 *ref_int_x = value__14.index
                var t1198 *ref_int_x = value__14.index
                var t1199 int
                var inline3182 int = ref_get__Ref_3int(t1198)
                t1199 = inline3182
                var t1200 int = t1199 + 2
                ref_set__Ref_3int(t1197, t1200)
                var mtmp16 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__14)
                var jp1202 uint32
                switch mtmp16.(type) {
                case Result__uint32__string_Ok:
                    var x17 uint32 = mtmp16.(Result__uint32__string_Ok)._0
                    jp1202 = x17
                    var t1215 bool = jp1202 < 56320
                    var jp1206 bool
                    if t1215 {
                        jp1206 = true
                    } else {
                        var t1216 bool = jp1202 > 57343
                        jp1206 = t1216
                    }
                    if jp1206 {
                        var t1207 string
                        var inline3164 string = "invalid low surrogate"
                        var inline3165 string = "" + inline3164
                        var inline3166 string = inline3165 + " at byte "
                        var inline3167 *ref_int_x = value__14.index
                        var inline3168 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3167)
                        var inline3169 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3168)
                        var inline3170 string = inline3166 + inline3169
                        t1207 = inline3170
                        var t1208 Result__unit__string = Result__unit__string_Err{
                            _0: t1207,
                        }
                        return t1208
                    } else {
                        var t1209 uint32 = jp1186 - 55296
                        var t1210 uint32 = t1209 * 1024
                        var t1211 uint32 = 65536 + t1210
                        var t1212 uint32 = t1211 + jp1202
                        var t1213 uint32 = t1212 - 56320
                        var inline3172 Option__char = char_from_uint32(t1213)
                        switch inline3172.(type) {
                        case Option__char_None:
                            var inline3173 string = _goml_m_std_p_json_p_json__error(value__14, "invalid unicode codepoint")
                            var inline3174 Result__unit__string = Result__unit__string_Err{
                                _0: inline3173,
                            }
                            return inline3174
                        case Option__char_Some:
                            var inline3175 rune = inline3172.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__15, inline3175)
                            var inline3178 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline3178
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x18 string = mtmp16.(Result__uint32__string_Err)._0
                    var t1217 Result__unit__string = Result__unit__string_Err{
                        _0: x18,
                    }
                    return t1217
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t1246 bool = jp1186 >= 56320
            var jp1242 bool
            if t1246 {
                var t1247 bool = jp1186 <= 57343
                jp1242 = t1247
            } else {
                jp1242 = false
            }
            if jp1242 {
                var t1243 string = _goml_m_std_p_json_p_json__error(value__14, "unexpected low surrogate")
                var t1244 Result__unit__string = Result__unit__string_Err{
                    _0: t1243,
                }
                return t1244
            } else {
                var t1245 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__14, builder__15, jp1186)
                return t1245
            }
        }
    case Result__uint32__string_Err:
        var x14 string = mtmp12.(Result__uint32__string_Err)._0
        var t1250 Result__unit__string = Result__unit__string_Err{
            _0: x14,
        }
        return t1250
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__18 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t1366 *ref_int_x = value__18.index
    var t1367 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1366)
    var t1368 string = value__18.input
    var t1369 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1368)
    var t1370 bool = t1367 >= t1369
    var jp1358 bool
    if t1370 {
        jp1358 = true
    } else {
        var t1371 string = value__18.input
        var t1372 *ref_int_x = value__18.index
        var t1373 int
        var inline3193 int = ref_get__Ref_3int(t1372)
        t1373 = inline3193
        var t1374 uint8
        var inline3191 uint8 = _goml_runtime_core_string_byte_get(t1371, t1373)
        t1374 = inline3191
        var t1375 bool
        var inline3188 uint8 = 34
        var inline3189 bool = t1374 == inline3188
        t1375 = inline3189
        var t1376 bool = !t1375
        jp1358 = t1376
    }
    if jp1358 {
        var t1359 string
        var inline3195 string = "expected string"
        var inline3196 string = "" + inline3195
        var inline3197 string = inline3196 + " at byte "
        var inline3198 *ref_int_x = value__18.index
        var inline3199 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3198)
        var inline3200 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3199)
        var inline3201 string = inline3197 + inline3200
        t1359 = inline3201
        var t1360 Result__string__string = Result__string__string_Err{
            _0: t1359,
        }
        return t1360
    } else {
        var t1361 *ref_int_x = value__18.index
        var t1362 *ref_int_x = value__18.index
        var t1363 int
        var inline3205 int = ref_get__Ref_3int(t1362)
        t1363 = inline3205
        var t1364 int = t1363 + 1
        ref_set__Ref_3int(t1361, t1364)
        var builder__19 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t1254 *ref_int_x = value__18.index
        var segment__20 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1254)
        Loop_loop1258:
        for {
            var t1259 *ref_int_x = value__18.index
            var t1260 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1259)
            var t1261 string = value__18.input
            var t1262 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1261)
            var t1263 bool = t1260 < t1262
            if t1263 {
                var t1264 string = value__18.input
                var t1265 *ref_int_x = value__18.index
                var t1266 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1265)
                var byte__21 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1264, t1266)
                var t1268 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 34)
                if t1268 {
                    var t1276 *ref_int_x = value__18.index
                    var t1277 int
                    var inline3221 int = ref_get__Ref_3int(t1276)
                    t1277 = inline3221
                    var t1278 bool = segment__20 < t1277
                    if t1278 {
                        var t1279 string = value__18.input
                        var t1280 *ref_int_x = value__18.index
                        var t1281 int
                        var inline3209 int = ref_get__Ref_3int(t1280)
                        t1281 = inline3209
                        var t1282 string
                        var inline3207 string = string_byte_slice(t1279, segment__20, t1281)
                        t1282 = inline3207
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t1282)
                    } else {}
                    var t1270 *ref_int_x = value__18.index
                    var t1271 *ref_int_x = value__18.index
                    var t1272 int
                    var inline3219 int = ref_get__Ref_3int(t1271)
                    t1272 = inline3219
                    var t1273 int = t1272 + 1
                    ref_set__Ref_3int(t1270, t1273)
                    var t1274 string
                    var inline3211 *_goml_vec_uint8 = builder__19.values
                    var inline3212 Tuple2_4bool_6string = string_from_utf8(inline3211)
                    var inline3214 string = inline3212._1
                    t1274 = inline3214
                    var t1275 Result__string__string = Result__string__string_Ok{
                        _0: t1274,
                    }
                    return t1275
                } else {
                    var t1285 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(byte__21, 92)
                    if t1285 {
                        var t1340 *ref_int_x = value__18.index
                        var t1341 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1340)
                        var t1342 bool = segment__20 < t1341
                        if t1342 {
                            var t1343 string = value__18.input
                            var t1344 *ref_int_x = value__18.index
                            var t1345 int
                            var inline3225 int = ref_get__Ref_3int(t1344)
                            t1345 = inline3225
                            var t1346 string
                            var inline3223 string = string_byte_slice(t1343, segment__20, t1345)
                            t1346 = inline3223
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, t1346)
                        } else {}
                        var t1287 *ref_int_x = value__18.index
                        var t1288 *ref_int_x = value__18.index
                        var t1289 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1288)
                        var t1290 int = t1289 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1287, t1290)
                        var t1333 *ref_int_x = value__18.index
                        var t1334 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1333)
                        var t1335 string = value__18.input
                        var t1336 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1335)
                        var t1337 bool = t1334 >= t1336
                        if t1337 {
                            var t1338 string
                            var inline3227 string = "incomplete escape"
                            var inline3228 string = "" + inline3227
                            var inline3229 string = inline3228 + " at byte "
                            var inline3230 *ref_int_x = value__18.index
                            var inline3231 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3230)
                            var inline3232 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3231)
                            var inline3233 string = inline3229 + inline3232
                            t1338 = inline3233
                            var t1339 Result__string__string = Result__string__string_Err{
                                _0: t1338,
                            }
                            return t1339
                        } else {
                            var t1292 string = value__18.input
                            var t1293 *ref_int_x = value__18.index
                            var t1294 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1293)
                            var escape__22 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1292, t1294)
                            var t1295 *ref_int_x = value__18.index
                            var t1296 *ref_int_x = value__18.index
                            var t1297 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1296)
                            var t1298 int = t1297 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1295, t1298)
                            var t1302 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 34)
                            if t1302 {
                                var inline3235 rune = 34
                                var inline3236 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3235)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__19, inline3236)
                                var t1300 *ref_int_x = value__18.index
                                var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                segment__20 = t1301
                                continue
                            } else {
                                var t1305 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 92)
                                if t1305 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 92)
                                    var t1300 *ref_int_x = value__18.index
                                    var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                    segment__20 = t1301
                                    continue
                                } else {
                                    var t1308 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 47)
                                    if t1308 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 47)
                                        var t1300 *ref_int_x = value__18.index
                                        var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                        segment__20 = t1301
                                        continue
                                    } else {
                                        var t1311 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 98)
                                        if t1311 {
                                            var mtmp26 Option__char = char_from_uint32(8)
                                            switch mtmp26.(type) {
                                            case Option__char_None:
                                                var t1300 *ref_int_x = value__18.index
                                                var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                                segment__20 = t1301
                                                continue
                                            case Option__char_Some:
                                                var x27 rune = mtmp26.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x27)
                                                var t1300 *ref_int_x = value__18.index
                                                var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                                segment__20 = t1301
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t1315 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 102)
                                            if t1315 {
                                                var mtmp28 Option__char = char_from_uint32(12)
                                                switch mtmp28.(type) {
                                                case Option__char_None:
                                                    var t1300 *ref_int_x = value__18.index
                                                    var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                                    segment__20 = t1301
                                                    continue
                                                case Option__char_Some:
                                                    var x29 rune = mtmp28.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, x29)
                                                    var t1300 *ref_int_x = value__18.index
                                                    var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                                    segment__20 = t1301
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t1319 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 110)
                                                if t1319 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 10)
                                                    var t1300 *ref_int_x = value__18.index
                                                    var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                                    segment__20 = t1301
                                                    continue
                                                } else {
                                                    var t1322 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 114)
                                                    if t1322 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 13)
                                                        var t1300 *ref_int_x = value__18.index
                                                        var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                                        segment__20 = t1301
                                                        continue
                                                    } else {
                                                        var t1325 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 116)
                                                        if t1325 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__19, 9)
                                                            var t1300 *ref_int_x = value__18.index
                                                            var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                                            segment__20 = t1301
                                                            continue
                                                        } else {
                                                            var t1328 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(escape__22, 117)
                                                            if t1328 {
                                                                var mtmp30 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__18, builder__19)
                                                                switch mtmp30.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t1300 *ref_int_x = value__18.index
                                                                    var t1301 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1300)
                                                                    segment__20 = t1301
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x32 string = mtmp30.(Result__unit__string_Err)._0
                                                                    var t1330 Result__string__string = Result__string__string_Err{
                                                                        _0: x32,
                                                                    }
                                                                    return t1330
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t1331 string = _goml_m_std_p_json_p_json__error(value__18, "invalid escape")
                                                                var t1332 Result__string__string = Result__string__string_Err{
                                                                    _0: t1331,
                                                                }
                                                                return t1332
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
                        var t1349 bool = byte__21 < 32
                        if t1349 {
                            var t1350 string = _goml_m_std_p_json_p_json__error(value__18, "unescaped control character")
                            var t1351 Result__string__string = Result__string__string_Err{
                                _0: t1350,
                            }
                            return t1351
                        } else {
                            var t1352 *ref_int_x = value__18.index
                            var t1353 *ref_int_x = value__18.index
                            var t1354 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1353)
                            var t1355 int = t1354 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1352, t1355)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop1258
            }
        }
        var t1256 string = _goml_m_std_p_json_p_json__error(value__18, "unterminated string")
        var t1257 Result__string__string = Result__string__string_Err{
            _0: t1256,
        }
        return t1257
    }
}

func _goml_m_std_p_json_p_parse__digits(value__26 _goml_m_std_p_json_p_JsonParser) bool {
    var t1385 *ref_int_x = value__26.index
    var start__27 int
    var inline3256 int = ref_get__Ref_3int(t1385)
    start__27 = inline3256
    Loop_loop1390:
    for {
        var t1398 *ref_int_x = value__26.index
        var t1399 int
        var inline3252 int = ref_get__Ref_3int(t1398)
        t1399 = inline3252
        var t1400 string = value__26.input
        var t1401 int
        var inline3250 int = _goml_runtime_core_string_len(t1400)
        t1401 = inline3250
        var t1402 bool = t1399 < t1401
        var jp1392 bool
        if t1402 {
            var t1403 string = value__26.input
            var t1404 *ref_int_x = value__26.index
            var t1405 int
            var inline3244 int = ref_get__Ref_3int(t1404)
            t1405 = inline3244
            var t1406 uint8
            var inline3242 uint8 = _goml_runtime_core_string_byte_get(t1403, t1405)
            t1406 = inline3242
            var inline3239 bool = t1406 >= 48
            if inline3239 {
                var inline3240 bool = t1406 <= 57
                jp1392 = inline3240
            } else {
                jp1392 = false
            }
        } else {
            jp1392 = false
        }
        if jp1392 {
            var t1393 *ref_int_x = value__26.index
            var t1394 *ref_int_x = value__26.index
            var t1395 int
            var inline3248 int = ref_get__Ref_3int(t1394)
            t1395 = inline3248
            var t1396 int = t1395 + 1
            ref_set__Ref_3int(t1393, t1396)
            continue
        } else {
            break Loop_loop1390
        }
    }
    var t1387 *ref_int_x = value__26.index
    var t1388 int
    var inline3254 int = ref_get__Ref_3int(t1387)
    t1388 = inline3254
    var t1389 bool = t1388 > start__27
    return t1389
}

func _goml_m_std_p_json_p_parse__json__number(value__28 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1410 *ref_int_x = value__28.index
    var start__29 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1410)
    var t1532 string = value__28.input
    var t1533 *ref_int_x = value__28.index
    var t1534 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1533)
    var t1535 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1532, t1534)
    var t1536 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1535, 45)
    if t1536 {
        var t1537 *ref_int_x = value__28.index
        var t1538 *ref_int_x = value__28.index
        var t1539 int
        var inline3260 int = ref_get__Ref_3int(t1538)
        t1539 = inline3260
        var t1540 int = t1539 + 1
        ref_set__Ref_3int(t1537, t1540)
    } else {}
    var t1495 *ref_int_x = value__28.index
    var t1496 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1495)
    var t1497 string = value__28.input
    var t1498 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1497)
    var t1499 bool = t1496 >= t1498
    if t1499 {
        var t1500 string
        var inline3262 string = "incomplete number"
        var inline3263 string = "" + inline3262
        var inline3264 string = inline3263 + " at byte "
        var inline3265 *ref_int_x = value__28.index
        var inline3266 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3265)
        var inline3267 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3266)
        var inline3268 string = inline3264 + inline3267
        t1500 = inline3268
        var t1501 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1500,
        }
        return t1501
    } else {
        var t1503 string = value__28.input
        var t1504 *ref_int_x = value__28.index
        var t1505 int
        var inline3306 int = ref_get__Ref_3int(t1504)
        t1505 = inline3306
        var t1506 uint8
        var inline3304 uint8 = _goml_runtime_core_string_byte_get(t1503, t1505)
        t1506 = inline3304
        var t1507 bool
        var inline3301 uint8 = 48
        var inline3302 bool = t1506 == inline3301
        t1507 = inline3302
        if t1507 {
            var t1508 *ref_int_x = value__28.index
            var t1509 *ref_int_x = value__28.index
            var t1510 int
            var inline3291 int = ref_get__Ref_3int(t1509)
            t1510 = inline3291
            var t1511 int = t1510 + 1
            ref_set__Ref_3int(t1508, t1511)
            var t1517 *ref_int_x = value__28.index
            var t1518 int
            var inline3287 int = ref_get__Ref_3int(t1517)
            t1518 = inline3287
            var t1519 string = value__28.input
            var t1520 int
            var inline3285 int = _goml_runtime_core_string_len(t1519)
            t1520 = inline3285
            var t1521 bool = t1518 < t1520
            var jp1514 bool
            if t1521 {
                var t1522 string = value__28.input
                var t1523 *ref_int_x = value__28.index
                var t1524 int
                var inline3275 int = ref_get__Ref_3int(t1523)
                t1524 = inline3275
                var t1525 uint8
                var inline3273 uint8 = _goml_runtime_core_string_byte_get(t1522, t1524)
                t1525 = inline3273
                var inline3270 bool = t1525 >= 48
                if inline3270 {
                    var inline3271 bool = t1525 <= 57
                    jp1514 = inline3271
                } else {
                    jp1514 = false
                }
            } else {
                jp1514 = false
            }
            if jp1514 {
                var t1515 string
                var inline3277 string = "invalid leading zero"
                var inline3278 string = "" + inline3277
                var inline3279 string = inline3278 + " at byte "
                var inline3280 *ref_int_x = value__28.index
                var inline3281 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3280)
                var inline3282 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3281)
                var inline3283 string = inline3279 + inline3282
                t1515 = inline3283
                var t1516 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1515,
                }
                return t1516
            } else {
                var t1485 *ref_int_x = value__28.index
                var t1486 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1485)
                var t1487 string = value__28.input
                var t1488 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1487)
                var t1489 bool = t1486 < t1488
                var jp1475 bool
                if t1489 {
                    var t1490 string = value__28.input
                    var t1491 *ref_int_x = value__28.index
                    var t1492 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1491)
                    var t1493 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1490, t1492)
                    var t1494 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1493, 46)
                    jp1475 = t1494
                } else {
                    jp1475 = false
                }
                if jp1475 {
                    var t1476 *ref_int_x = value__28.index
                    var t1477 *ref_int_x = value__28.index
                    var t1478 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1477)
                    var t1479 int = t1478 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1476, t1479)
                    var t1481 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t1482 bool = !t1481
                    if t1482 {
                        var t1483 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t1484 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1483,
                        }
                        return t1484
                    } else {
                        var t1457 *ref_int_x = value__28.index
                        var t1458 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1457)
                        var t1459 string = value__28.input
                        var t1460 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1459)
                        var t1461 bool = t1458 < t1460
                        var jp1422 bool
                        if t1461 {
                            var t1464 string = value__28.input
                            var t1465 *ref_int_x = value__28.index
                            var t1466 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1465)
                            var t1467 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1464, t1466)
                            var t1468 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1467, 101)
                            if t1468 {
                                jp1422 = true
                            } else {
                                var t1469 string = value__28.input
                                var t1470 *ref_int_x = value__28.index
                                var t1471 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1470)
                                var t1472 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1469, t1471)
                                var t1473 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1472, 69)
                                jp1422 = t1473
                            }
                        } else {
                            jp1422 = false
                        }
                        if jp1422 {
                            var t1423 *ref_int_x = value__28.index
                            var t1424 *ref_int_x = value__28.index
                            var t1425 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1424)
                            var t1426 int = t1425 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1423, t1426)
                            var t1440 *ref_int_x = value__28.index
                            var t1441 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1440)
                            var t1442 string = value__28.input
                            var t1443 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1442)
                            var t1444 bool = t1441 < t1443
                            var jp1434 bool
                            if t1444 {
                                var t1447 string = value__28.input
                                var t1448 *ref_int_x = value__28.index
                                var t1449 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1448)
                                var t1450 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1447, t1449)
                                var t1451 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1450, 43)
                                if t1451 {
                                    jp1434 = true
                                } else {
                                    var t1452 string = value__28.input
                                    var t1453 *ref_int_x = value__28.index
                                    var t1454 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1453)
                                    var t1455 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1452, t1454)
                                    var t1456 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1455, 45)
                                    jp1434 = t1456
                                }
                            } else {
                                jp1434 = false
                            }
                            if jp1434 {
                                var t1435 *ref_int_x = value__28.index
                                var t1436 *ref_int_x = value__28.index
                                var t1437 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1436)
                                var t1438 int = t1437 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1435, t1438)
                            } else {}
                            var t1429 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t1430 bool = !t1429
                            if t1430 {
                                var t1431 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t1432 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1431,
                                }
                                return t1432
                            } else {
                                var t1415 string = value__28.input
                                var t1416 *ref_int_x = value__28.index
                                var t1417 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1416)
                                var t1418 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1415, start__29, t1417)
                                var t1419 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                    _0: t1418,
                                }
                                var t1420 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t1419,
                                }
                                return t1420
                            }
                        } else {
                            var t1415 string = value__28.input
                            var t1416 *ref_int_x = value__28.index
                            var t1417 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1416)
                            var t1418 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1415, start__29, t1417)
                            var t1419 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1418,
                            }
                            var t1420 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1419,
                            }
                            return t1420
                        }
                    }
                } else {
                    var t1457 *ref_int_x = value__28.index
                    var t1458 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1457)
                    var t1459 string = value__28.input
                    var t1460 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1459)
                    var t1461 bool = t1458 < t1460
                    var jp1422 bool
                    if t1461 {
                        var t1464 string = value__28.input
                        var t1465 *ref_int_x = value__28.index
                        var t1466 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1465)
                        var t1467 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1464, t1466)
                        var t1468 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1467, 101)
                        if t1468 {
                            jp1422 = true
                        } else {
                            var t1469 string = value__28.input
                            var t1470 *ref_int_x = value__28.index
                            var t1471 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1470)
                            var t1472 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1469, t1471)
                            var t1473 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1472, 69)
                            jp1422 = t1473
                        }
                    } else {
                        jp1422 = false
                    }
                    if jp1422 {
                        var t1423 *ref_int_x = value__28.index
                        var t1424 *ref_int_x = value__28.index
                        var t1425 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1424)
                        var t1426 int = t1425 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1423, t1426)
                        var t1440 *ref_int_x = value__28.index
                        var t1441 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1440)
                        var t1442 string = value__28.input
                        var t1443 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1442)
                        var t1444 bool = t1441 < t1443
                        var jp1434 bool
                        if t1444 {
                            var t1447 string = value__28.input
                            var t1448 *ref_int_x = value__28.index
                            var t1449 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1448)
                            var t1450 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1447, t1449)
                            var t1451 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1450, 43)
                            if t1451 {
                                jp1434 = true
                            } else {
                                var t1452 string = value__28.input
                                var t1453 *ref_int_x = value__28.index
                                var t1454 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1453)
                                var t1455 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1452, t1454)
                                var t1456 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1455, 45)
                                jp1434 = t1456
                            }
                        } else {
                            jp1434 = false
                        }
                        if jp1434 {
                            var t1435 *ref_int_x = value__28.index
                            var t1436 *ref_int_x = value__28.index
                            var t1437 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1436)
                            var t1438 int = t1437 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1435, t1438)
                        } else {}
                        var t1429 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t1430 bool = !t1429
                        if t1430 {
                            var t1431 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t1432 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t1431,
                            }
                            return t1432
                        } else {
                            var t1415 string = value__28.input
                            var t1416 *ref_int_x = value__28.index
                            var t1417 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1416)
                            var t1418 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1415, start__29, t1417)
                            var t1419 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1418,
                            }
                            var t1420 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1419,
                            }
                            return t1420
                        }
                    } else {
                        var t1415 string = value__28.input
                        var t1416 *ref_int_x = value__28.index
                        var t1417 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1416)
                        var t1418 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1415, start__29, t1417)
                        var t1419 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                            _0: t1418,
                        }
                        var t1420 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t1419,
                        }
                        return t1420
                    }
                }
            }
        } else {
            var t1528 bool = _goml_m_std_p_json_p_parse__digits(value__28)
            var t1529 bool = !t1528
            if t1529 {
                var t1530 string
                var inline3293 string = "expected number"
                var inline3294 string = "" + inline3293
                var inline3295 string = inline3294 + " at byte "
                var inline3296 *ref_int_x = value__28.index
                var inline3297 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3296)
                var inline3298 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3297)
                var inline3299 string = inline3295 + inline3298
                t1530 = inline3299
                var t1531 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1530,
                }
                return t1531
            } else {
                var t1485 *ref_int_x = value__28.index
                var t1486 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1485)
                var t1487 string = value__28.input
                var t1488 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1487)
                var t1489 bool = t1486 < t1488
                var jp1475 bool
                if t1489 {
                    var t1490 string = value__28.input
                    var t1491 *ref_int_x = value__28.index
                    var t1492 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1491)
                    var t1493 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1490, t1492)
                    var t1494 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1493, 46)
                    jp1475 = t1494
                } else {
                    jp1475 = false
                }
                if jp1475 {
                    var t1476 *ref_int_x = value__28.index
                    var t1477 *ref_int_x = value__28.index
                    var t1478 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1477)
                    var t1479 int = t1478 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1476, t1479)
                    var t1481 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                    var t1482 bool = !t1481
                    if t1482 {
                        var t1483 string = _goml_m_std_p_json_p_json__error(value__28, "missing fraction digits")
                        var t1484 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1483,
                        }
                        return t1484
                    } else {
                        var t1457 *ref_int_x = value__28.index
                        var t1458 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1457)
                        var t1459 string = value__28.input
                        var t1460 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1459)
                        var t1461 bool = t1458 < t1460
                        var jp1422 bool
                        if t1461 {
                            var t1464 string = value__28.input
                            var t1465 *ref_int_x = value__28.index
                            var t1466 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1465)
                            var t1467 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1464, t1466)
                            var t1468 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1467, 101)
                            if t1468 {
                                jp1422 = true
                            } else {
                                var t1469 string = value__28.input
                                var t1470 *ref_int_x = value__28.index
                                var t1471 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1470)
                                var t1472 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1469, t1471)
                                var t1473 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1472, 69)
                                jp1422 = t1473
                            }
                        } else {
                            jp1422 = false
                        }
                        if jp1422 {
                            var t1423 *ref_int_x = value__28.index
                            var t1424 *ref_int_x = value__28.index
                            var t1425 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1424)
                            var t1426 int = t1425 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1423, t1426)
                            var t1440 *ref_int_x = value__28.index
                            var t1441 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1440)
                            var t1442 string = value__28.input
                            var t1443 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1442)
                            var t1444 bool = t1441 < t1443
                            var jp1434 bool
                            if t1444 {
                                var t1447 string = value__28.input
                                var t1448 *ref_int_x = value__28.index
                                var t1449 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1448)
                                var t1450 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1447, t1449)
                                var t1451 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1450, 43)
                                if t1451 {
                                    jp1434 = true
                                } else {
                                    var t1452 string = value__28.input
                                    var t1453 *ref_int_x = value__28.index
                                    var t1454 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1453)
                                    var t1455 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1452, t1454)
                                    var t1456 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1455, 45)
                                    jp1434 = t1456
                                }
                            } else {
                                jp1434 = false
                            }
                            if jp1434 {
                                var t1435 *ref_int_x = value__28.index
                                var t1436 *ref_int_x = value__28.index
                                var t1437 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1436)
                                var t1438 int = t1437 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1435, t1438)
                            } else {}
                            var t1429 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                            var t1430 bool = !t1429
                            if t1430 {
                                var t1431 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                                var t1432 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1431,
                                }
                                return t1432
                            } else {
                                var t1415 string = value__28.input
                                var t1416 *ref_int_x = value__28.index
                                var t1417 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1416)
                                var t1418 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1415, start__29, t1417)
                                var t1419 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                    _0: t1418,
                                }
                                var t1420 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                    _0: t1419,
                                }
                                return t1420
                            }
                        } else {
                            var t1415 string = value__28.input
                            var t1416 *ref_int_x = value__28.index
                            var t1417 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1416)
                            var t1418 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1415, start__29, t1417)
                            var t1419 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1418,
                            }
                            var t1420 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1419,
                            }
                            return t1420
                        }
                    }
                } else {
                    var t1457 *ref_int_x = value__28.index
                    var t1458 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1457)
                    var t1459 string = value__28.input
                    var t1460 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1459)
                    var t1461 bool = t1458 < t1460
                    var jp1422 bool
                    if t1461 {
                        var t1464 string = value__28.input
                        var t1465 *ref_int_x = value__28.index
                        var t1466 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1465)
                        var t1467 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1464, t1466)
                        var t1468 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1467, 101)
                        if t1468 {
                            jp1422 = true
                        } else {
                            var t1469 string = value__28.input
                            var t1470 *ref_int_x = value__28.index
                            var t1471 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1470)
                            var t1472 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1469, t1471)
                            var t1473 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1472, 69)
                            jp1422 = t1473
                        }
                    } else {
                        jp1422 = false
                    }
                    if jp1422 {
                        var t1423 *ref_int_x = value__28.index
                        var t1424 *ref_int_x = value__28.index
                        var t1425 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1424)
                        var t1426 int = t1425 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1423, t1426)
                        var t1440 *ref_int_x = value__28.index
                        var t1441 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1440)
                        var t1442 string = value__28.input
                        var t1443 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1442)
                        var t1444 bool = t1441 < t1443
                        var jp1434 bool
                        if t1444 {
                            var t1447 string = value__28.input
                            var t1448 *ref_int_x = value__28.index
                            var t1449 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1448)
                            var t1450 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1447, t1449)
                            var t1451 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1450, 43)
                            if t1451 {
                                jp1434 = true
                            } else {
                                var t1452 string = value__28.input
                                var t1453 *ref_int_x = value__28.index
                                var t1454 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1453)
                                var t1455 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1452, t1454)
                                var t1456 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1455, 45)
                                jp1434 = t1456
                            }
                        } else {
                            jp1434 = false
                        }
                        if jp1434 {
                            var t1435 *ref_int_x = value__28.index
                            var t1436 *ref_int_x = value__28.index
                            var t1437 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1436)
                            var t1438 int = t1437 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1435, t1438)
                        } else {}
                        var t1429 bool = _goml_m_std_p_json_p_parse__digits(value__28)
                        var t1430 bool = !t1429
                        if t1430 {
                            var t1431 string = _goml_m_std_p_json_p_json__error(value__28, "missing exponent digits")
                            var t1432 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: t1431,
                            }
                            return t1432
                        } else {
                            var t1415 string = value__28.input
                            var t1416 *ref_int_x = value__28.index
                            var t1417 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1416)
                            var t1418 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1415, start__29, t1417)
                            var t1419 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                                _0: t1418,
                            }
                            var t1420 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1419,
                            }
                            return t1420
                        }
                    } else {
                        var t1415 string = value__28.input
                        var t1416 *ref_int_x = value__28.index
                        var t1417 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1416)
                        var t1418 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t1415, start__29, t1417)
                        var t1419 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                            _0: t1418,
                        }
                        var t1420 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                            _0: t1419,
                        }
                        return t1420
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__30 _goml_m_std_p_json_p_JsonParser, expected__31 string, result__32 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t1556 *ref_int_x = value__30.index
    var t1557 int
    var inline3336 int = ref_get__Ref_3int(t1556)
    t1557 = inline3336
    var t1558 int
    var inline3334 int = _goml_runtime_core_string_len(expected__31)
    t1558 = inline3334
    var t1559 int = t1557 + t1558
    var t1560 string = value__30.input
    var t1561 int
    var inline3332 int = _goml_runtime_core_string_len(t1560)
    t1561 = inline3332
    var t1562 bool = t1559 <= t1561
    var jp1547 bool
    if t1562 {
        var t1563 string = value__30.input
        var t1564 *ref_int_x = value__30.index
        var t1565 int
        var inline3316 int = ref_get__Ref_3int(t1564)
        t1565 = inline3316
        var t1566 *ref_int_x = value__30.index
        var t1567 int
        var inline3314 int = ref_get__Ref_3int(t1566)
        t1567 = inline3314
        var t1568 int
        var inline3312 int = _goml_runtime_core_string_len(expected__31)
        t1568 = inline3312
        var t1569 int = t1567 + t1568
        var t1570 string
        var inline3310 string = string_byte_slice(t1563, t1565, t1569)
        t1570 = inline3310
        var inline3308 bool = t1570 == expected__31
        jp1547 = inline3308
    } else {
        jp1547 = false
    }
    if jp1547 {
        var t1548 *ref_int_x = value__30.index
        var t1549 *ref_int_x = value__30.index
        var t1550 int
        var inline3322 int = ref_get__Ref_3int(t1549)
        t1550 = inline3322
        var t1551 int
        var inline3320 int = _goml_runtime_core_string_len(expected__31)
        t1551 = inline3320
        var t1552 int = t1550 + t1551
        ref_set__Ref_3int(t1548, t1552)
        var t1553 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__32,
        }
        return t1553
    } else {
        var t1554 string
        var inline3324 string = "invalid literal"
        var inline3325 string = "" + inline3324
        var inline3326 string = inline3325 + " at byte "
        var inline3327 *ref_int_x = value__30.index
        var inline3328 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3327)
        var inline3329 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3328)
        var inline3330 string = inline3326 + inline3329
        t1554 = inline3330
        var t1555 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1554,
        }
        return t1555
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__33 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1574 *ref_int_x = value__33.index
    var t1575 *ref_int_x = value__33.index
    var t1576 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1575)
    var t1577 int = t1576 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1574, t1577)
    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
    var vec_literal__8702 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t1632 *ref_int_x = value__33.index
    var t1633 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1632)
    var t1634 string = value__33.input
    var t1635 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1634)
    var t1636 bool = t1633 < t1635
    var jp1625 bool
    if t1636 {
        var t1637 string = value__33.input
        var t1638 *ref_int_x = value__33.index
        var t1639 int
        var inline3343 int = ref_get__Ref_3int(t1638)
        t1639 = inline3343
        var t1640 uint8
        var inline3341 uint8 = _goml_runtime_core_string_byte_get(t1637, t1639)
        t1640 = inline3341
        var inline3338 uint8 = 93
        var inline3339 bool = t1640 == inline3338
        jp1625 = inline3339
    } else {
        jp1625 = false
    }
    if jp1625 {
        var t1626 *ref_int_x = value__33.index
        var t1627 *ref_int_x = value__33.index
        var t1628 int
        var inline3347 int = ref_get__Ref_3int(t1627)
        t1628 = inline3347
        var t1629 int = t1628 + 1
        ref_set__Ref_3int(t1626, t1629)
        var t1630 _goml_m_std_p_json_p_Value = Array{
            _0: vec_literal__8702,
        }
        var t1631 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1630,
        }
        return t1631
    } else {
        Loop_loop1582:
        for {
            var t1583 *ref_int_x = value__33.index
            var t1584 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1583)
            var t1585 string = value__33.input
            var t1586 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1585)
            var t1587 bool = t1584 < t1586
            if t1587 {
                var mtmp50 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__33)
                var jp1589 _goml_m_std_p_json_p_Value
                switch mtmp50.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x51 _goml_m_std_p_json_p_Value = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp1589 = x51
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8702, jp1589)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                    var t1591 *ref_int_x = value__33.index
                    var t1592 int
                    var inline3389 int = ref_get__Ref_3int(t1591)
                    t1592 = inline3389
                    var t1593 string = value__33.input
                    var t1594 int
                    var inline3387 int = _goml_runtime_core_string_len(t1593)
                    t1594 = inline3387
                    var t1595 bool = t1592 >= t1594
                    if t1595 {
                        var t1596 string
                        var inline3349 string = "unterminated array"
                        var inline3350 string = "" + inline3349
                        var inline3351 string = inline3350 + " at byte "
                        var inline3352 *ref_int_x = value__33.index
                        var inline3353 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3352)
                        var inline3354 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3353)
                        var inline3355 string = inline3351 + inline3354
                        t1596 = inline3355
                        var t1597 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1596,
                        }
                        return t1597
                    } else {
                        var t1599 string = value__33.input
                        var t1600 *ref_int_x = value__33.index
                        var t1601 int
                        var inline3385 int = ref_get__Ref_3int(t1600)
                        t1601 = inline3385
                        var t1602 uint8
                        var inline3383 uint8 = _goml_runtime_core_string_byte_get(t1599, t1601)
                        t1602 = inline3383
                        var t1603 bool
                        var inline3380 uint8 = 93
                        var inline3381 bool = t1602 == inline3380
                        t1603 = inline3381
                        if t1603 {
                            var t1604 *ref_int_x = value__33.index
                            var t1605 *ref_int_x = value__33.index
                            var t1606 int
                            var inline3359 int = ref_get__Ref_3int(t1605)
                            t1606 = inline3359
                            var t1607 int = t1606 + 1
                            ref_set__Ref_3int(t1604, t1607)
                            var t1608 _goml_m_std_p_json_p_Value = Array{
                                _0: vec_literal__8702,
                            }
                            var t1609 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t1608,
                            }
                            return t1609
                        } else {
                            var t1611 string = value__33.input
                            var t1612 *ref_int_x = value__33.index
                            var t1613 int
                            var inline3378 int = ref_get__Ref_3int(t1612)
                            t1613 = inline3378
                            var t1614 uint8
                            var inline3376 uint8 = _goml_runtime_core_string_byte_get(t1611, t1613)
                            t1614 = inline3376
                            var t1615 bool
                            var inline3373 uint8 = 44
                            var inline3374 bool = t1614 == inline3373
                            t1615 = inline3374
                            if t1615 {
                                var t1616 *ref_int_x = value__33.index
                                var t1617 *ref_int_x = value__33.index
                                var t1618 int
                                var inline3363 int = ref_get__Ref_3int(t1617)
                                t1618 = inline3363
                                var t1619 int = t1618 + 1
                                ref_set__Ref_3int(t1616, t1619)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__33)
                                continue
                            } else {
                                var t1621 string
                                var inline3365 string = "expected array separator"
                                var inline3366 string = "" + inline3365
                                var inline3367 string = inline3366 + " at byte "
                                var inline3368 *ref_int_x = value__33.index
                                var inline3369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3368)
                                var inline3370 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3369)
                                var inline3371 string = inline3367 + inline3370
                                t1621 = inline3371
                                var t1622 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1621,
                                }
                                return t1622
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x52 string = mtmp50.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t1623 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x52,
                    }
                    return t1623
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1582
            }
        }
        var t1580 string = _goml_m_std_p_json_p_json__error(value__33, "unterminated array")
        var t1581 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1580,
        }
        return t1581
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__36 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t1644 *ref_int_x = value__36.index
    var t1645 *ref_int_x = value__36.index
    var t1646 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1645)
    var t1647 int = t1646 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t1644, t1647)
    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
    var vec_literal__9904 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t1727 *ref_int_x = value__36.index
    var t1728 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1727)
    var t1729 string = value__36.input
    var t1730 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1729)
    var t1731 bool = t1728 < t1730
    var jp1720 bool
    if t1731 {
        var t1732 string = value__36.input
        var t1733 *ref_int_x = value__36.index
        var t1734 int
        var inline3396 int = ref_get__Ref_3int(t1733)
        t1734 = inline3396
        var t1735 uint8
        var inline3394 uint8 = _goml_runtime_core_string_byte_get(t1732, t1734)
        t1735 = inline3394
        var inline3391 uint8 = 125
        var inline3392 bool = t1735 == inline3391
        jp1720 = inline3392
    } else {
        jp1720 = false
    }
    if jp1720 {
        var t1721 *ref_int_x = value__36.index
        var t1722 *ref_int_x = value__36.index
        var t1723 int
        var inline3400 int = ref_get__Ref_3int(t1722)
        t1723 = inline3400
        var t1724 int = t1723 + 1
        ref_set__Ref_3int(t1721, t1724)
        var t1725 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__9904,
        }
        var t1726 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t1725,
        }
        return t1726
    } else {
        Loop_loop1652:
        for {
            var t1653 *ref_int_x = value__36.index
            var t1654 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1653)
            var t1655 string = value__36.input
            var t1656 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1655)
            var t1657 bool = t1654 < t1656
            if t1657 {
                var mtmp62 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__36)
                var jp1659 string
                switch mtmp62.(type) {
                case Result__string__string_Ok:
                    var x63 string = mtmp62.(Result__string__string_Ok)._0
                    jp1659 = x63
                    _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                    var t1707 *ref_int_x = value__36.index
                    var t1708 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1707)
                    var t1709 string = value__36.input
                    var t1710 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1709)
                    var t1711 bool = t1708 >= t1710
                    var jp1699 bool
                    if t1711 {
                        jp1699 = true
                    } else {
                        var t1712 string = value__36.input
                        var t1713 *ref_int_x = value__36.index
                        var t1714 int
                        var inline3407 int = ref_get__Ref_3int(t1713)
                        t1714 = inline3407
                        var t1715 uint8
                        var inline3405 uint8 = _goml_runtime_core_string_byte_get(t1712, t1714)
                        t1715 = inline3405
                        var t1716 bool
                        var inline3402 uint8 = 58
                        var inline3403 bool = t1715 == inline3402
                        t1716 = inline3403
                        var t1717 bool = !t1716
                        jp1699 = t1717
                    }
                    if jp1699 {
                        var t1700 string
                        var inline3409 string = "expected object colon"
                        var inline3410 string = "" + inline3409
                        var inline3411 string = inline3410 + " at byte "
                        var inline3412 *ref_int_x = value__36.index
                        var inline3413 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3412)
                        var inline3414 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3413)
                        var inline3415 string = inline3411 + inline3414
                        t1700 = inline3415
                        var t1701 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t1700,
                        }
                        return t1701
                    } else {
                        var t1702 *ref_int_x = value__36.index
                        var t1703 *ref_int_x = value__36.index
                        var t1704 int
                        var inline3419 int = ref_get__Ref_3int(t1703)
                        t1704 = inline3419
                        var t1705 int = t1704 + 1
                        ref_set__Ref_3int(t1702, t1705)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                        var mtmp68 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__36)
                        var jp1662 _goml_m_std_p_json_p_Value
                        switch mtmp68.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x69 _goml_m_std_p_json_p_Value = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp1662 = x69
                            var t1663 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp1659,
                                _1: jp1662,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__9904, t1663)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                            var t1665 *ref_int_x = value__36.index
                            var t1666 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1665)
                            var t1667 string = value__36.input
                            var t1668 int = _goml_m_inherent_i_string_i_string_i_byte__len(t1667)
                            var t1669 bool = t1666 >= t1668
                            if t1669 {
                                var t1670 string
                                var inline3421 string = "unterminated object"
                                var inline3422 string = "" + inline3421
                                var inline3423 string = inline3422 + " at byte "
                                var inline3424 *ref_int_x = value__36.index
                                var inline3425 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3424)
                                var inline3426 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3425)
                                var inline3427 string = inline3423 + inline3426
                                t1670 = inline3427
                                var t1671 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t1670,
                                }
                                return t1671
                            } else {
                                var t1673 string = value__36.input
                                var t1674 *ref_int_x = value__36.index
                                var t1675 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t1674)
                                var t1676 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t1673, t1675)
                                var t1677 bool = _goml_m_trait__impl_i_Eq_i_uint8_i_eq(t1676, 125)
                                if t1677 {
                                    var t1678 *ref_int_x = value__36.index
                                    var t1679 *ref_int_x = value__36.index
                                    var t1680 int
                                    var inline3431 int = ref_get__Ref_3int(t1679)
                                    t1680 = inline3431
                                    var t1681 int = t1680 + 1
                                    ref_set__Ref_3int(t1678, t1681)
                                    var t1682 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__9904,
                                    }
                                    var t1683 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t1682,
                                    }
                                    return t1683
                                } else {
                                    var t1685 string = value__36.input
                                    var t1686 *ref_int_x = value__36.index
                                    var t1687 int
                                    var inline3442 int = ref_get__Ref_3int(t1686)
                                    t1687 = inline3442
                                    var t1688 uint8
                                    var inline3440 uint8 = _goml_runtime_core_string_byte_get(t1685, t1687)
                                    t1688 = inline3440
                                    var t1689 bool
                                    var inline3437 uint8 = 44
                                    var inline3438 bool = t1688 == inline3437
                                    t1689 = inline3438
                                    if t1689 {
                                        var t1690 *ref_int_x = value__36.index
                                        var t1691 *ref_int_x = value__36.index
                                        var t1692 int
                                        var inline3435 int = ref_get__Ref_3int(t1691)
                                        t1692 = inline3435
                                        var t1693 int = t1692 + 1
                                        ref_set__Ref_3int(t1690, t1693)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__36)
                                        continue
                                    } else {
                                        var t1695 string = _goml_m_std_p_json_p_json__error(value__36, "expected object separator")
                                        var t1696 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t1695,
                                        }
                                        return t1696
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x70 string = mtmp68.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t1697 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x70,
                            }
                            return t1697
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x64 string = mtmp62.(Result__string__string_Err)._0
                    var t1718 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x64,
                    }
                    return t1718
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop1652
            }
        }
        var t1650 string = _goml_m_std_p_json_p_json__error(value__36, "unterminated object")
        var t1651 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1650,
        }
        return t1651
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__40 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__40)
    var t1741 *ref_int_x = value__40.index
    var t1742 int
    var inline3472 int = ref_get__Ref_3int(t1741)
    t1742 = inline3472
    var t1743 string = value__40.input
    var t1744 int
    var inline3470 int = _goml_runtime_core_string_len(t1743)
    t1744 = inline3470
    var t1745 bool = t1742 >= t1744
    if t1745 {
        var t1746 string
        var inline3444 string = "expected JSON value"
        var inline3445 string = "" + inline3444
        var inline3446 string = inline3445 + " at byte "
        var inline3447 *ref_int_x = value__40.index
        var inline3448 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3447)
        var inline3449 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3448)
        var inline3450 string = inline3446 + inline3449
        t1746 = inline3450
        var t1747 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t1746,
        }
        return t1747
    } else {
        var t1748 string = value__40.input
        var t1749 *ref_int_x = value__40.index
        var t1750 int
        var inline3468 int = ref_get__Ref_3int(t1749)
        t1750 = inline3468
        var mtmp77 uint8
        var inline3466 uint8 = _goml_runtime_core_string_byte_get(t1748, t1750)
        mtmp77 = inline3466
        switch mtmp77 {
        case 123:
            var t1753 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__40)
            return t1753
        case 91:
            var t1754 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__40)
            return t1754
        case 34:
            var mtmp78 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__40)
            switch mtmp78.(type) {
            case Result__string__string_Ok:
                var x79 string = mtmp78.(Result__string__string_Ok)._0
                var t1757 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x79,
                }
                var t1758 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t1757,
                }
                return t1758
            case Result__string__string_Err:
                var x80 string = mtmp78.(Result__string__string_Err)._0
                var t1759 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x80,
                }
                return t1759
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t1760 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t1761 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "true", t1760)
            return t1761
        case 102:
            var t1762 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t1763 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "false", t1762)
            return t1763
        case 110:
            var t1764 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__40, "null", Null{})
            return t1764
        default:
            var t1772 bool
            var inline3463 uint8 = 45
            var inline3464 bool = mtmp77 == inline3463
            t1772 = inline3464
            var jp1768 bool
            if t1772 {
                jp1768 = true
            } else {
                var inline3452 bool = mtmp77 >= 48
                if inline3452 {
                    var inline3453 bool = mtmp77 <= 57
                    jp1768 = inline3453
                } else {
                    jp1768 = false
                }
            }
            if jp1768 {
                var t1769 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__number(value__40)
                return t1769
            } else {
                var t1770 string
                var inline3455 string = "unexpected JSON token"
                var inline3456 string = "" + inline3455
                var inline3457 string = inline3456 + " at byte "
                var inline3458 *ref_int_x = value__40.index
                var inline3459 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3458)
                var inline3460 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3459)
                var inline3461 string = inline3457 + inline3460
                t1770 = inline3461
                var t1771 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t1770,
                }
                return t1771
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__44 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__45 _goml_m_std_p_json_p_JsonParser
    var inline3488 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline3489 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__44,
        index: inline3488,
    }
    parser__45 = inline3489
    var mtmp81 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__45)
    var jp1777 _goml_m_std_p_json_p_Value
    switch mtmp81.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x82 _goml_m_std_p_json_p_Value = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp1777 = x82
        _goml_m_std_p_json_p_skip__json__whitespace(parser__45)
        var t1780 *ref_int_x = parser__45.index
        var t1781 int
        var inline3486 int = ref_get__Ref_3int(t1780)
        t1781 = inline3486
        var t1782 int
        var inline3484 int = _goml_runtime_core_string_len(input__44)
        t1782 = inline3484
        var t1783 bool
        var inline3482 bool = t1781 == t1782
        t1783 = inline3482
        if t1783 {
            var t1784 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp1777,
            }
            return t1784
        } else {
            var t1785 string
            var inline3474 string = "trailing JSON data"
            var inline3475 string = "" + inline3474
            var inline3476 string = inline3475 + " at byte "
            var inline3477 *ref_int_x = parser__45.index
            var inline3478 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline3477)
            var inline3479 string = _goml_m_inherent_i_int_i_int_i_to__string(inline3478)
            var inline3480 string = inline3476 + inline3479
            t1785 = inline3480
            var t1786 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t1785,
            }
            return t1786
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x83 string = mtmp81.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t1787 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x83,
        }
        return t1787
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__48 _goml_m_std_p_text_p_StringBuilder, value__49 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    var start__50 int = 0
    var for_index86 int = 0
    var for_limit87 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    Loop_loop1801:
    for {
        var t1802 bool = for_index86 < for_limit87
        if t1802 {
            var for_item88 int = for_index86
            var t1803 int = for_index86 + 1
            for_index86 = t1803
            var byte__52 uint8
            var inline3550 uint8 = _goml_runtime_core_string_byte_get(value__49, for_item88)
            byte__52 = inline3550
            var t1856 bool
            var inline3547 uint8 = 34
            var inline3548 bool = byte__52 == inline3547
            t1856 = inline3548
            var jp1854 bool
            if t1856 {
                jp1854 = true
            } else {
                var inline3494 uint8 = 92
                var inline3495 bool = byte__52 == inline3494
                jp1854 = inline3495
            }
            var jp1851 bool
            if jp1854 {
                jp1851 = true
            } else {
                var inline3497 uint8 = 8
                var inline3498 bool = byte__52 == inline3497
                jp1851 = inline3498
            }
            var jp1848 bool
            if jp1851 {
                jp1848 = true
            } else {
                var inline3500 uint8 = 9
                var inline3501 bool = byte__52 == inline3500
                jp1848 = inline3501
            }
            var jp1845 bool
            if jp1848 {
                jp1845 = true
            } else {
                var inline3503 uint8 = 10
                var inline3504 bool = byte__52 == inline3503
                jp1845 = inline3504
            }
            var jp1842 bool
            if jp1845 {
                jp1842 = true
            } else {
                var inline3506 uint8 = 12
                var inline3507 bool = byte__52 == inline3506
                jp1842 = inline3507
            }
            var jp1839 bool
            if jp1842 {
                jp1839 = true
            } else {
                var inline3509 uint8 = 13
                var inline3510 bool = byte__52 == inline3509
                jp1839 = inline3510
            }
            var jp1806 bool
            if jp1839 {
                jp1806 = true
            } else {
                var t1840 bool = byte__52 < 32
                jp1806 = t1840
            }
            if jp1806 {
                var t1835 bool = start__50 < for_item88
                if t1835 {
                    var t1836 string
                    var inline3512 string = string_byte_slice(value__49, start__50, for_item88)
                    t1836 = inline3512
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1836)
                } else {}
                var t1810 bool
                var inline3544 uint8 = 34
                var inline3545 bool = byte__52 == inline3544
                t1810 = inline3545
                if t1810 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\"")
                } else {
                    var t1813 bool
                    var inline3541 uint8 = 92
                    var inline3542 bool = byte__52 == inline3541
                    t1813 = inline3542
                    if t1813 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\\\")
                    } else {
                        var t1816 bool
                        var inline3538 uint8 = 8
                        var inline3539 bool = byte__52 == inline3538
                        t1816 = inline3539
                        if t1816 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\b")
                        } else {
                            var t1819 bool
                            var inline3535 uint8 = 9
                            var inline3536 bool = byte__52 == inline3535
                            t1819 = inline3536
                            if t1819 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\t")
                            } else {
                                var t1822 bool
                                var inline3532 uint8 = 10
                                var inline3533 bool = byte__52 == inline3532
                                t1822 = inline3533
                                if t1822 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\n")
                                } else {
                                    var t1825 bool
                                    var inline3529 uint8 = 12
                                    var inline3530 bool = byte__52 == inline3529
                                    t1825 = inline3530
                                    if t1825 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\f")
                                    } else {
                                        var t1828 bool
                                        var inline3526 uint8 = 13
                                        var inline3527 bool = byte__52 == inline3526
                                        t1828 = inline3527
                                        if t1828 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, "\\u00")
                                            var t1830 uint8 = byte__52 / 16
                                            var t1831 rune
                                            var inline3523 int = int(uint8(t1830))
                                            var inline3524 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline3523)
                                            t1831 = inline3524
                                            var inline3520 string = _goml_m_inherent_i_char_i_char_i_to__string(t1831)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline3520)
                                            var t1832_rhs uint8 = 16
                                            var t1832 uint8 = byte__52 % t1832_rhs
                                            var t1833 rune
                                            var inline3517 int = int(uint8(t1832))
                                            var inline3518 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline3517)
                                            t1833 = inline3518
                                            var inline3514 string = _goml_m_inherent_i_char_i_char_i_to__string(t1833)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, inline3514)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t1809 int = for_item88 + 1
                start__50 = t1809
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop1801
        }
    }
    var t1796 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__49)
    var t1797 bool = start__50 < t1796
    if t1797 {
        var t1798 int
        var inline3554 int = _goml_runtime_core_string_len(value__49)
        t1798 = inline3554
        var t1799 string
        var inline3552 string = string_byte_slice(value__49, start__50, t1798)
        t1799 = inline3552
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__48, t1799)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__48, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__53 _goml_m_std_p_text_p_StringBuilder, value__54 _goml_m_std_p_json_p_Value) struct{} {
    switch value__54.(type) {
    case Object:
        var x97 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__54.(Object)._0
        var inline3568 rune = 123
        var inline3569 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3568)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3569)
        var index__56 int = 0
        var for_limit104 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97)
        var for_index105 int = 0
        Loop_loop1862:
        for {
            var t1863 bool = for_index105 < for_limit104
            if t1863 {
                var for_item106 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x97, for_index105)
                var t1864 int = for_index105 + 1
                for_index105 = t1864
                var t1870 bool = index__56 > 0
                if t1870 {
                    var inline3556 rune = 44
                    var inline3557 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3556)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3557)
                } else {}
                var t1866 string = for_item106._0
                _goml_m_std_p_json_p_write__json__string(builder__53, t1866)
                var inline3560 rune = 58
                var inline3561 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3560)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3561)
                var t1867 _goml_m_std_p_json_p_Value = for_item106._1
                _goml_m_std_p_json_p_write__json__value(builder__53, t1867)
                var compound_old112 int = index__56
                var compound_value113 int = 1
                var t1868 int = compound_old112 + compound_value113
                index__56 = t1868
                continue
            } else {
                break Loop_loop1862
            }
        }
        var inline3564 rune = 125
        var inline3565 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3564)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3565)
        return struct{}{}
    case Array:
        var x98 *_goml_vec__goml_m_std_p_json_p_Value = value__54.(Array)._0
        var inline3580 rune = 91
        var inline3581 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3580)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3581)
        var index__59 int = 0
        var for_limit118 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x98)
        var for_index119 int = 0
        Loop_loop1874:
        for {
            var t1875 bool = for_index119 < for_limit118
            if t1875 {
                var for_item120 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x98, for_index119)
                var t1876 int = for_index119 + 1
                for_index119 = t1876
                var t1880 bool = index__59 > 0
                if t1880 {
                    var inline3572 rune = 44
                    var inline3573 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3572)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3573)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__53, for_item120)
                var compound_old124 int = index__59
                var compound_value125 int = 1
                var t1878 int = compound_old124 + compound_value125
                index__59 = t1878
                continue
            } else {
                break Loop_loop1874
            }
        }
        var inline3576 rune = 93
        var inline3577 string = _goml_m_inherent_i_char_i_char_i_to__string(inline3576)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, inline3577)
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
        var jp1885 string
        if x101 {
            jp1885 = "true"
        } else {
            jp1885 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__53, jp1885)
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
    var inline3590 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline3591 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline3590,
    }
    builder__65 = inline3591
    _goml_m_std_p_json_p_write__json__value(builder__65, value__64)
    var inline3584 *_goml_vec_uint8 = builder__65.values
    var inline3585 Tuple2_4bool_6string = string_from_utf8(inline3584)
    var inline3587 string = inline3585._1
    return inline3587
}

func _goml_m_std_p_json_p_field(value__66 _goml_m_std_p_json_p_Value, name__67 string) _goml_m_Option____std_p_json_p_Value {
    switch value__66.(type) {
    case Object:
        var x129 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__66.(Object)._0
        var for_limit135 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129)
        var for_index136 int = 0
        Loop_loop1896:
        for {
            var t1897 bool = for_index136 < for_limit135
            if t1897 {
                var for_item137 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x129, for_index136)
                var t1898 int = for_index136 + 1
                for_index136 = t1898
                var t1900 string = for_item137._0
                var t1901 bool
                var inline3593 bool = t1900 == name__67
                t1901 = inline3593
                if t1901 {
                    var t1902 _goml_m_std_p_json_p_Value = for_item137._1
                    var t1903 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t1902,
                    }
                    return t1903
                } else {
                    continue
                }
            } else {
                break Loop_loop1896
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__72 string) Option__int {
    var t1913 int
    var inline3612 int = _goml_runtime_core_string_len(value__72)
    t1913 = inline3612
    var t1914 bool
    var inline3609 int = 0
    var inline3610 bool = t1913 == inline3609
    t1914 = inline3610
    if t1914 {
        return Option__int_None{}
    } else {
        var t1915 uint8
        var inline3606 int = 0
        var inline3607 uint8 = _goml_runtime_core_string_byte_get(value__72, inline3606)
        t1915 = inline3607
        var negative__73 bool
        var inline3603 uint8 = 45
        var inline3604 bool = t1915 == inline3603
        negative__73 = inline3604
        var jp1917 int
        if negative__73 {
            jp1917 = 1
        } else {
            jp1917 = 0
        }
        var index__74 int = jp1917
        var result__75 int = 0
        var t1938 int
        var inline3601 int = _goml_runtime_core_string_len(value__72)
        t1938 = inline3601
        var t1939 bool
        var inline3599 bool = index__74 == t1938
        t1939 = inline3599
        if t1939 {
            return Option__int_None{}
        } else {
            Loop_loop1924:
            for {
                var t1925 int
                var inline3597 int = _goml_runtime_core_string_len(value__72)
                t1925 = inline3597
                var t1926 bool = index__74 < t1925
                if t1926 {
                    var byte__76 uint8
                    var inline3595 uint8 = _goml_runtime_core_string_byte_get(value__72, index__74)
                    byte__76 = inline3595
                    var t1936 bool = byte__76 < 48
                    var jp1931 bool
                    if t1936 {
                        jp1931 = true
                    } else {
                        var t1937 bool = byte__76 > 57
                        jp1931 = t1937
                    }
                    if jp1931 {
                        return Option__int_None{}
                    } else {
                        var t1932 int = result__75 * 10
                        var t1933 uint8 = byte__76 - 48
                        var t1934 int = int(uint8(t1933))
                        var t1935 int = t1932 + t1934
                        result__75 = t1935
                        var compound_old148 int = index__74
                        var compound_value149 int = 1
                        var t1928 int = compound_old148 + compound_value149
                        index__74 = t1928
                        continue
                    }
                } else {
                    break Loop_loop1924
                }
            }
            var jp1921 int
            if negative__73 {
                var t1923 int = 0 - result__75
                jp1921 = t1923
            } else {
                jp1921 = result__75
            }
            var t1922 Option__int = Option__int_Some{
                _0: jp1921,
            }
            return t1922
        }
    }
}

func main0() struct{} {
    var mtmp136 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp2096 _goml_m_std_p_json_p_Value
    switch mtmp136.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x137 _goml_m_std_p_json_p_Value = mtmp136.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp2096 = x137
        var mtmp140 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2096, "name")
        switch mtmp140.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline3699 string = "missing name"
            var inline3700 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3699)
            _goml_runtime_core_string_println(inline3700)
            var mtmp145 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2096, "version")
            switch mtmp145.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline3714 string = "missing version"
                var inline3715 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3714)
                _goml_runtime_core_string_println(inline3715)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x146 _goml_m_std_p_json_p_Value = mtmp145.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp147 Option__int
                switch x146.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline3725 string = x146.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline3727 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline3725)
                    mtmp147 = inline3727
                default:
                    mtmp147 = Option__int_None{}
                }
                switch mtmp147.(type) {
                case Option__int_None:
                    var inline3718 string = "invalid version"
                    var inline3719 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3718)
                    _goml_runtime_core_string_println(inline3719)
                case Option__int_Some:
                    var x148 int = mtmp147.(Option__int_Some)._0
                    var inline3722 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x148)
                    _goml_runtime_core_string_println(inline3722)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp150 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2096, "stable")
            switch mtmp150.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline3729 string = "missing stable"
                var inline3730 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3729)
                _goml_runtime_core_string_println(inline3730)
                var t2100 string = _goml_m_std_p_json_p_encode(jp2096)
                println__T_string(t2100)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x151 _goml_m_std_p_json_p_Value = mtmp150.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field3897 bool
                switch x151.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline3740 bool = x151.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field3897 = inline3740
                    var inline3737 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3897)
                    _goml_runtime_core_string_println(inline3737)
                    var t2100 string = _goml_m_std_p_json_p_encode(jp2096)
                    println__T_string(t2100)
                    return struct{}{}
                default:
                    var inline3733 string = "invalid stable"
                    var inline3734 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3733)
                    _goml_runtime_core_string_println(inline3734)
                    var t2100 string = _goml_m_std_p_json_p_encode(jp2096)
                    println__T_string(t2100)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x141 _goml_m_std_p_json_p_Value = mtmp140.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field3903 string
            switch x141.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline3710 string = x141.(_goml_m_std_p_json_p_Value_String)._0
                commute_field3903 = inline3710
                var inline3707 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field3903)
                _goml_runtime_core_string_println(inline3707)
                var mtmp145 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2096, "version")
                switch mtmp145.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3714 string = "missing version"
                    var inline3715 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3714)
                    _goml_runtime_core_string_println(inline3715)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x146 _goml_m_std_p_json_p_Value = mtmp145.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp147 Option__int
                    switch x146.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline3725 string = x146.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline3727 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline3725)
                        mtmp147 = inline3727
                    default:
                        mtmp147 = Option__int_None{}
                    }
                    switch mtmp147.(type) {
                    case Option__int_None:
                        var inline3718 string = "invalid version"
                        var inline3719 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3718)
                        _goml_runtime_core_string_println(inline3719)
                    case Option__int_Some:
                        var x148 int = mtmp147.(Option__int_Some)._0
                        var inline3722 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x148)
                        _goml_runtime_core_string_println(inline3722)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp150 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2096, "stable")
                switch mtmp150.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3729 string = "missing stable"
                    var inline3730 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3729)
                    _goml_runtime_core_string_println(inline3730)
                    var t2100 string = _goml_m_std_p_json_p_encode(jp2096)
                    println__T_string(t2100)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x151 _goml_m_std_p_json_p_Value = mtmp150.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field3897 bool
                    switch x151.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline3740 bool = x151.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field3897 = inline3740
                        var inline3737 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3897)
                        _goml_runtime_core_string_println(inline3737)
                        var t2100 string = _goml_m_std_p_json_p_encode(jp2096)
                        println__T_string(t2100)
                        return struct{}{}
                    default:
                        var inline3733 string = "invalid stable"
                        var inline3734 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3733)
                        _goml_runtime_core_string_println(inline3734)
                        var t2100 string = _goml_m_std_p_json_p_encode(jp2096)
                        println__T_string(t2100)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline3703 string = "invalid name"
                var inline3704 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3703)
                _goml_runtime_core_string_println(inline3704)
                var mtmp145 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2096, "version")
                switch mtmp145.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3714 string = "missing version"
                    var inline3715 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3714)
                    _goml_runtime_core_string_println(inline3715)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x146 _goml_m_std_p_json_p_Value = mtmp145.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp147 Option__int
                    switch x146.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline3725 string = x146.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline3727 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline3725)
                        mtmp147 = inline3727
                    default:
                        mtmp147 = Option__int_None{}
                    }
                    switch mtmp147.(type) {
                    case Option__int_None:
                        var inline3718 string = "invalid version"
                        var inline3719 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3718)
                        _goml_runtime_core_string_println(inline3719)
                    case Option__int_Some:
                        var x148 int = mtmp147.(Option__int_Some)._0
                        var inline3722 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x148)
                        _goml_runtime_core_string_println(inline3722)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp150 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp2096, "stable")
                switch mtmp150.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline3729 string = "missing stable"
                    var inline3730 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3729)
                    _goml_runtime_core_string_println(inline3730)
                    var t2100 string = _goml_m_std_p_json_p_encode(jp2096)
                    println__T_string(t2100)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x151 _goml_m_std_p_json_p_Value = mtmp150.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field3897 bool
                    switch x151.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline3740 bool = x151.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field3897 = inline3740
                        var inline3737 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field3897)
                        _goml_runtime_core_string_println(inline3737)
                        var t2100 string = _goml_m_std_p_json_p_encode(jp2096)
                        println__T_string(t2100)
                        return struct{}{}
                    default:
                        var inline3733 string = "invalid stable"
                        var inline3734 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline3733)
                        _goml_runtime_core_string_println(inline3734)
                        var t2100 string = _goml_m_std_p_json_p_encode(jp2096)
                        println__T_string(t2100)
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
        var inline3696 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x138)
        _goml_runtime_core_string_println(inline3696)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t2122 string = _goml_runtime_core_int_to_string(self__34)
    return t2122
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline3744 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline3745 bool = inline3744._0
    var inline3746 rune = inline3744._1
    if inline3745 {
        return inline3746
    } else {
        var inline3750 rune = _goml_runtime_core_string_get("", -1)
        return inline3750
    }
}

func _goml_m_trait__impl_i_Eq_i_uint8_i_eq(self__98 uint8, other__99 uint8) bool {
    var t2196 bool = self__98 == other__99
    return t2196
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t2199 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t2199
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop2206:
    for {
        var t2207 int
        var inline3752 int = _goml_runtime_core_string_len(x12)
        t2207 = inline3752
        var t2208 bool = index__26 < t2207
        if t2208 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t2210 int = compound_old17 + x16
                index__26 = t2210
                continue
            } else {
                var t2212 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t2212
            }
        } else {
            break Loop_loop2206
        }
    }
    var t2205 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t2205
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline3754 uint32 = uint32(rune(self__36))
    var inline3755 bool = utf8_valid_scalar(inline3754)
    if inline3755 {
        var inline3756 string = _goml_runtime_core_char_to_string(self__36)
        return inline3756
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t2251 int = _goml_runtime_core_string_len(self__38)
    return t2251
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t2254 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t2254
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline3772 bool = string_is_char_boundary(self__43, start__44)
    var inline3774 bool
    if inline3772 {
        var inline3777 bool = string_is_char_boundary(self__43, end__45)
        inline3774 = inline3777
    } else {
        inline3774 = false
    }
    if inline3774 {
        var inline3775 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline3775
    } else {
        var inline3776 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline3776
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__215 int) *ref_int_x {
    var t2283 *ref_int_x = ref__Ref_3int(value__215)
    return t2283
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__216 *ref_int_x) int {
    var t2286 int = ref_get__Ref_3int(self__216)
    return t2286
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__217 *ref_int_x, value__218 int) struct{} {
    ref_set__Ref_3int(self__217, value__218)
    return struct{}{}
}

func char_from_uint32(value__32 uint32) Option__char {
    var t2293 bool
    var inline3789 bool = value__32 <= 1114111
    if inline3789 {
        var inline3790 bool = value__32 >= 55296
        var inline3792 bool
        if inline3790 {
            var inline3794 bool = value__32 <= 57343
            inline3792 = inline3794
        } else {
            inline3792 = false
        }
        var inline3793 bool = !inline3792
        t2293 = inline3793
    } else {
        t2293 = false
    }
    if t2293 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t2294 Option__char = Option__char_Some{
            _0: x24,
        }
        return t2294
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t2297 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t2297
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__134 *_goml_vec__goml_m_std_p_json_p_Value, elem__135 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__134, elem__135)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t2302 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t2302
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__134 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__135 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__134, elem__135)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t2359 string
    t2359 = value__31
    _goml_runtime_core_string_println(t2359)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t2490 bool = index__6 < 0
    var jp2488 bool
    if t2490 {
        jp2488 = true
    } else {
        var t2491 bool = index__6 >= length__7
        jp2488 = t2491
    }
    if jp2488 {
        var inline3801 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline3801
    } else {
        var t2375 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t2375))
        var t2378 bool = first__8 < 128
        if t2378 {
            var inline3803 int = 1
            var inline3804 Option__char = char_from_uint32(first__8)
            switch inline3804.(type) {
            case Option__char_None:
                var inline3805 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline3805
            case Option__char_Some:
                var inline3806 rune = inline3804.(Option__char_Some)._0
                var inline3808 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline3806,
                    _2: inline3803,
                }
                return inline3808
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t2382 bool = first__8 < 194
            if t2382 {
                var inline3810 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline3810
            } else {
                var t2386 bool = first__8 < 224
                if t2386 {
                    var t2399 int = length__7 - index__6
                    var t2400 bool = t2399 < 2
                    if t2400 {
                        var inline3812 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline3812
                    } else {
                        var t2388 int = index__6 + 1
                        var t2389 uint8
                        var inline3826 uint8 = _goml_runtime_core_string_byte_get(value__5, t2388)
                        t2389 = inline3826
                        var second__9 uint32 = uint32(uint8(t2389))
                        var t2392 bool
                        var inline3823 bool = second__9 < 128
                        if inline3823 {
                            t2392 = true
                        } else {
                            var inline3824 bool = second__9 > 191
                            t2392 = inline3824
                        }
                        if t2392 {
                            var inline3814 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline3814
                        } else {
                            var t2394_rhs uint32 = 31
                            var t2394 uint32 = first__8 & t2394_rhs
                            var t2395_rhs int = 6
                            var t2395 uint32 = t2394 << t2395_rhs
                            var t2396_rhs uint32 = 63
                            var t2396 uint32 = second__9 & t2396_rhs
                            var t2397 uint32 = t2395 | t2396
                            var inline3816 int = 2
                            var inline3817 Option__char = char_from_uint32(t2397)
                            switch inline3817.(type) {
                            case Option__char_None:
                                var inline3818 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline3818
                            case Option__char_Some:
                                var inline3819 rune = inline3817.(Option__char_Some)._0
                                var inline3821 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline3819,
                                    _2: inline3816,
                                }
                                return inline3821
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t2404 bool = first__8 < 240
                    if t2404 {
                        var t2437 int = length__7 - index__6
                        var t2438 bool = t2437 < 3
                        if t2438 {
                            var inline3828 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline3828
                        } else {
                            var t2406 int = index__6 + 1
                            var t2407 uint8
                            var inline3843 uint8 = _goml_runtime_core_string_byte_get(value__5, t2406)
                            t2407 = inline3843
                            var second__10 uint32 = uint32(uint8(t2407))
                            var t2408 int = index__6 + 2
                            var t2409 uint8
                            var inline3841 uint8 = _goml_runtime_core_string_byte_get(value__5, t2408)
                            t2409 = inline3841
                            var third__11 uint32 = uint32(uint8(t2409))
                            var t2435 bool = utf8_invalid_continuation(second__10)
                            var jp2430 bool
                            if t2435 {
                                jp2430 = true
                            } else {
                                var inline3830 bool = third__11 < 128
                                if inline3830 {
                                    jp2430 = true
                                } else {
                                    var inline3831 bool = third__11 > 191
                                    jp2430 = inline3831
                                }
                            }
                            var jp2424 bool
                            if jp2430 {
                                jp2424 = true
                            } else {
                                var t2433 bool
                                var inline3833 uint32 = 224
                                var inline3834 bool = first__8 == inline3833
                                t2433 = inline3834
                                if t2433 {
                                    var t2434 bool = second__10 < 160
                                    jp2424 = t2434
                                } else {
                                    jp2424 = false
                                }
                            }
                            var jp2413 bool
                            if jp2424 {
                                jp2413 = true
                            } else {
                                var t2427 bool
                                var inline3836 uint32 = 237
                                var inline3837 bool = first__8 == inline3836
                                t2427 = inline3837
                                if t2427 {
                                    var t2428 bool = second__10 >= 160
                                    jp2413 = t2428
                                } else {
                                    jp2413 = false
                                }
                            }
                            if jp2413 {
                                var inline3839 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline3839
                            } else {
                                var t2415_rhs uint32 = 15
                                var t2415 uint32 = first__8 & t2415_rhs
                                var t2416_rhs int = 12
                                var t2416 uint32 = t2415 << t2416_rhs
                                var t2417_rhs uint32 = 63
                                var t2417 uint32 = second__10 & t2417_rhs
                                var t2418_rhs int = 6
                                var t2418 uint32 = t2417 << t2418_rhs
                                var t2419 uint32 = t2416 | t2418
                                var t2420_rhs uint32 = 63
                                var t2420 uint32 = third__11 & t2420_rhs
                                var t2421 uint32 = t2419 | t2420
                                var t2422 Tuple3_4bool_4char_3int = utf8_valid_decode(t2421, 3)
                                return t2422
                            }
                        }
                    } else {
                        var t2442 bool = first__8 < 245
                        if t2442 {
                            var t2483 int = length__7 - index__6
                            var t2484 bool = t2483 < 4
                            if t2484 {
                                var t2485 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t2485
                            } else {
                                var t2444 int = index__6 + 1
                                var t2445 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t2444)
                                var second__12 uint32 = uint32(uint8(t2445))
                                var t2446 int = index__6 + 2
                                var t2447 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t2446)
                                var third__13 uint32 = uint32(uint8(t2447))
                                var t2448 int = index__6 + 3
                                var t2449 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t2448)
                                var fourth__14 uint32 = uint32(uint8(t2449))
                                var t2481 bool = utf8_invalid_continuation(second__12)
                                var jp2479 bool
                                if t2481 {
                                    jp2479 = true
                                } else {
                                    var t2482 bool = utf8_invalid_continuation(third__13)
                                    jp2479 = t2482
                                }
                                var jp2473 bool
                                if jp2479 {
                                    jp2473 = true
                                } else {
                                    var t2480 bool = utf8_invalid_continuation(fourth__14)
                                    jp2473 = t2480
                                }
                                var jp2467 bool
                                if jp2473 {
                                    jp2467 = true
                                } else {
                                    var t2476 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 240)
                                    if t2476 {
                                        var t2477 bool = second__12 < 144
                                        jp2467 = t2477
                                    } else {
                                        jp2467 = false
                                    }
                                }
                                var jp2453 bool
                                if jp2467 {
                                    jp2453 = true
                                } else {
                                    var t2470 bool = _goml_m_trait__impl_i_Eq_i_uint32_i_eq(first__8, 244)
                                    if t2470 {
                                        var t2471 bool = second__12 > 143
                                        jp2453 = t2471
                                    } else {
                                        jp2453 = false
                                    }
                                }
                                if jp2453 {
                                    var t2454 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t2454
                                } else {
                                    var t2455_rhs uint32 = 7
                                    var t2455 uint32 = first__8 & t2455_rhs
                                    var t2456_rhs int = 18
                                    var t2456 uint32 = t2455 << t2456_rhs
                                    var t2457_rhs uint32 = 63
                                    var t2457 uint32 = second__12 & t2457_rhs
                                    var t2458_rhs int = 12
                                    var t2458 uint32 = t2457 << t2458_rhs
                                    var t2459 uint32 = t2456 | t2458
                                    var t2460_rhs uint32 = 63
                                    var t2460 uint32 = third__13 & t2460_rhs
                                    var t2461_rhs int = 6
                                    var t2461 uint32 = t2460 << t2461_rhs
                                    var t2462 uint32 = t2459 | t2461
                                    var t2463_rhs uint32 = 63
                                    var t2463 uint32 = fourth__14 & t2463_rhs
                                    var t2464 uint32 = t2462 | t2463
                                    var t2465 Tuple3_4bool_4char_3int = utf8_valid_decode(t2464, 4)
                                    return t2465
                                }
                            }
                        } else {
                            var t2486 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t2486
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t2496 uint32 = uint32(rune(value__29))
    var t2497 bool
    var inline3845 bool = t2496 <= 1114111
    if inline3845 {
        var inline3846 bool = t2496 >= 55296
        var inline3848 bool
        if inline3846 {
            var inline3850 bool = t2496 <= 57343
            inline3848 = inline3850
        } else {
            inline3848 = false
        }
        var inline3849 bool = !inline3848
        t2497 = inline3849
    } else {
        t2497 = false
    }
    if t2497 {
        var t2498 string = _goml_runtime_core_char_to_string(value__29)
        return t2498
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t2513 bool = index__16 < 0
    var jp2504 bool
    if t2513 {
        jp2504 = true
    } else {
        var t2514 int
        var inline3852 int = _goml_runtime_core_string_len(value__15)
        t2514 = inline3852
        var t2515 bool = index__16 > t2514
        jp2504 = t2515
    }
    if jp2504 {
        return false
    } else {
        var t2507 int
        var inline3861 int = _goml_runtime_core_string_len(value__15)
        t2507 = inline3861
        var t2508 bool
        var inline3859 bool = index__16 == t2507
        t2508 = inline3859
        if t2508 {
            return true
        } else {
            var t2509 uint8
            var inline3857 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t2509 = inline3857
            var t2510_rhs uint8 = 192
            var t2510 uint8 = t2509 & t2510_rhs
            var t2511 bool
            var inline3854 uint8 = 128
            var inline3855 bool = t2510 == inline3854
            t2511 = inline3855
            var t2512 bool = !t2511
            return t2512
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t2524 bool = string_is_char_boundary(value__21, start__22)
    var jp2521 bool
    if t2524 {
        var t2525 bool = string_is_char_boundary(value__21, end__23)
        jp2521 = t2525
    } else {
        jp2521 = false
    }
    if jp2521 {
        var t2522 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t2522
    } else {
        var t2523 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t2523
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t2532 bool = value__4 <= 1114111
    if t2532 {
        var t2536 bool = value__4 >= 55296
        var jp2534 bool
        if t2536 {
            var t2537 bool = value__4 <= 57343
            jp2534 = t2537
        } else {
            jp2534 = false
        }
        var t2535 bool = !jp2534
        return t2535
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t2542 string = _goml_runtime_core_int_to_string(self__69)
    return t2542
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t2545 string = _goml_runtime_core_bool_to_string(self__66)
    return t2545
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t2548 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t2548
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field3906 rune
    var inline3865 bool = utf8_valid_scalar(value__0)
    if inline3865 {
        var inline3866 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline3868 rune = inline3866._1
        commute_field3906 = inline3868
        var t2554 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field3906,
            _2: width__1,
        }
        return t2554
    } else {
        var inline3863 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline3863
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t2559 bool = value__3 < 128
    if t2559 {
        return true
    } else {
        var t2560 bool = value__3 > 191
        return t2560
    }
}

func _goml_m_trait__impl_i_Eq_i_uint32_i_eq(self__102 uint32, other__103 uint32) bool {
    var t2563 bool = self__102 == other__103
    return t2563
}

func main() {
    main0()
}
