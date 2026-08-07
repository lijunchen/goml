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

type _goml_vec__goml_m_Tuple2__6string__18std_p_serde_p_Schema struct {
    items []Tuple2_6string_28_goml_m_std_p_serde_p_Schema
}

type _goml_vec__goml_m_std_p_serde_p_VariantSchema struct {
    items []_goml_m_std_p_serde_p_VariantSchema
}

type _goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value struct {
    items []Tuple2_6string_27_goml_m_std_p_serde_p_Value
}

type _goml_vec__goml_m_std_p_serde_p_Value struct {
    items []_goml_m_std_p_serde_p_Value
}

type _goml_vec__goml_m_std_p_serde_p_ValueDeserializeFrame struct {
    items []_goml_m_std_p_serde_p_ValueDeserializeFrame
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

type _goml_vec__goml_m_std_p_serde_p_ValueSerializeFrame struct {
    items []_goml_m_std_p_serde_p_ValueSerializeFrame
}

type _goml_vec_string struct {
    items []string
}

type _goml_vec__goml_m_std_p_json_p_JsonDeserializeFrame struct {
    items []_goml_m_std_p_json_p_JsonDeserializeFrame
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

type _goml_vec__goml_m_std_p_json_p_JsonSerializeFrame struct {
    items []_goml_m_std_p_json_p_JsonSerializeFrame
}

type _goml_vec__goml_m_std_p_serde_p_Schema struct {
    items []_goml_m_std_p_serde_p_Schema
}

type ref__goml_m_Option____std_p_serde_p_Value_x struct {
    value _goml_m_Option____std_p_serde_p_Value
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

type ref_bool_x struct {
    value bool
}

type ref_Option__string_x struct {
    value Option__string
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

type Tuple2_6string_28_goml_m_std_p_serde_p_Schema struct {
    _0 string
    _1 _goml_m_std_p_serde_p_Schema
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

type Tuple3_4bool_4bool_4bool struct {
    _0 bool
    _1 bool
    _2 bool
}

type Tuple3_4bool_4bool_14Option__string struct {
    _0 bool
    _1 bool
    _2 Option__string
}

type Tuple2_4char_3int struct {
    _0 rune
    _1 int
}

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Tuple2_6string_26_goml_m_std_p_json_p_Value struct {
    _0 string
    _1 _goml_m_std_p_json_p_Value
}

type Tuple2_14Option__string_14Option__string struct {
    _0 Option__string
    _1 Option__string
}

type _goml_m_std_p_serde_p_VariantSchema struct {
    index int
    name string
    kind int
    fields *_goml_vec__goml_m_Tuple2__6string__18std_p_serde_p_Schema
}

type _goml_m_std_p_serde_p_ValueDeserializer struct {
    current *ref__goml_m_Option____std_p_serde_p_Value_x
    frames *_goml_vec__goml_m_std_p_serde_p_ValueDeserializeFrame
}

type _goml_m_std_p_serde_p_ValueSerializer struct {
    frames *_goml_vec__goml_m_std_p_serde_p_ValueSerializeFrame
    result *ref__goml_m_Option____std_p_serde_p_Value_x
}

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_text_p_StringBuilder struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_json_p_JsonDeserializer struct {
    parser _goml_m_std_p_json_p_JsonParser
    frames *_goml_vec__goml_m_std_p_json_p_JsonDeserializeFrame
}

type _goml_m_std_p_json_p_JsonParser struct {
    input string
    index *ref_int_x
}

type _goml_m_std_p_json_p_JsonSerializer struct {
    builder _goml_m_std_p_text_p_StringBuilder
    frames *_goml_vec__goml_m_std_p_json_p_JsonSerializeFrame
    root_written *ref_bool_x
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

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_goml_builtin_range_0 struct {
    current_0 *ref_int_x
    end_1 int
}

type _goml_m_std_p_serde_p_FieldKey interface {
    is_goml_m_std_p_serde_p_FieldKey()
}

type Position struct {
    _0 int
}

func (_ Position) is_goml_m_std_p_serde_p_FieldKey() {}

type _goml_m_std_p_serde_p_FieldKey_Name struct {
    _0 string
}

func (_ _goml_m_std_p_serde_p_FieldKey_Name) is_goml_m_std_p_serde_p_FieldKey() {}

type _goml_m_std_p_serde_p_VariantKey interface {
    is_goml_m_std_p_serde_p_VariantKey()
}

type Index struct {
    _0 int
}

func (_ Index) is_goml_m_std_p_serde_p_VariantKey() {}

type _goml_m_std_p_serde_p_VariantKey_Name struct {
    _0 string
}

func (_ _goml_m_std_p_serde_p_VariantKey_Name) is_goml_m_std_p_serde_p_VariantKey() {}

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

type _goml_m_std_p_serde_p_Value_Variant struct {
    _0 int
    _1 string
    _2 int
    _3 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
}

func (_ _goml_m_std_p_serde_p_Value_Variant) is_goml_m_std_p_serde_p_Value() {}

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

type _goml_m_std_p_serde_p_ValueDeserializeFrame interface {
    is_goml_m_std_p_serde_p_ValueDeserializeFrame()
}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Optional struct {}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Optional) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Sequence struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 *ref_int_x
}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Sequence) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Tuple struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 *ref_int_x
}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Tuple) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Map struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 *ref_int_x
    _2 *ref_int_x
}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Map) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Struct struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
    _1 *ref_int_x
}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Struct) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueDeserializeFrame_Variant struct {
    _0 int
    _1 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
    _2 *ref_int_x
    _3 *ref_bool_x
}

func (_ _goml_m_std_p_serde_p_ValueDeserializeFrame_Variant) is_goml_m_std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame interface {
    is_goml_m_std_p_serde_p_ValueSerializeFrame()
}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Optional struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 bool
    _2 *ref_bool_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Optional) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Sequence struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 int
    _2 *ref_bool_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Sequence) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Tuple struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 int
    _2 *ref_bool_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Tuple) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Map struct {
    _0 *_goml_vec__goml_m_std_p_serde_p_Value
    _1 int
    _2 *ref_int_x
    _3 *ref__goml_m_Option____std_p_serde_p_Value_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Map) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Struct struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
    _1 int
    _2 *ref_Option__string_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Struct) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_serde_p_ValueSerializeFrame_Variant struct {
    _0 int
    _1 string
    _2 int
    _3 *_goml_vec__goml_m_Tuple2__6string__17std_p_serde_p_Value
    _4 int
    _5 *ref_Option__string_x
}

func (_ _goml_m_std_p_serde_p_ValueSerializeFrame_Variant) is_goml_m_std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame interface {
    is_goml_m_std_p_json_p_JsonDeserializeFrame()
}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Optional struct {}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Optional) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Sequence struct {
    _0 *ref_bool_x
    _1 *ref_bool_x
    _2 *ref_int_x
}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Sequence) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Tuple struct {
    _0 int
    _1 *ref_int_x
}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Tuple) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Map struct {
    _0 *ref_bool_x
    _1 *ref_bool_x
    _2 *ref_int_x
    _3 *ref_Option__string_x
}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Map) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Struct struct {
    _0 string
    _1 *ref_bool_x
    _2 *ref_bool_x
    _3 *ref_Option__string_x
}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Struct) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_JsonDeserializeFrame_Variant struct {
    _0 string
    _1 string
    _2 int
    _3 *ref_int_x
    _4 *ref_int_x
    _5 *ref_int_x
    _6 *ref_bool_x
    _7 *ref_bool_x
    _8 *ref_bool_x
}

func (_ _goml_m_std_p_json_p_JsonDeserializeFrame_Variant) is_goml_m_std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_std_p_json_p_Value interface {
    is_goml_m_std_p_json_p_Value()
}

type Object struct {
    _0 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value
}

func (_ Object) is_goml_m_std_p_json_p_Value() {}

type _goml_m_std_p_json_p_Value_Array struct {
    _0 *_goml_vec__goml_m_std_p_json_p_Value
}

func (_ _goml_m_std_p_json_p_Value_Array) is_goml_m_std_p_json_p_Value() {}

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

type _goml_m_std_p_json_p_JsonSerializeFrame interface {
    is_goml_m_std_p_json_p_JsonSerializeFrame()
}

type _goml_m_std_p_json_p_JsonSerializeFrame_Optional struct {
    _0 *ref_bool_x
}

func (_ _goml_m_std_p_json_p_JsonSerializeFrame_Optional) is_goml_m_std_p_json_p_JsonSerializeFrame() {}

type _goml_m_std_p_json_p_JsonSerializeFrame_Array struct {
    _0 int
    _1 *ref_int_x
    _2 *ref_bool_x
}

func (_ _goml_m_std_p_json_p_JsonSerializeFrame_Array) is_goml_m_std_p_json_p_JsonSerializeFrame() {}

type _goml_m_std_p_json_p_JsonSerializeFrame_Map struct {
    _0 int
    _1 *ref_int_x
    _2 *ref_int_x
}

func (_ _goml_m_std_p_json_p_JsonSerializeFrame_Map) is_goml_m_std_p_json_p_JsonSerializeFrame() {}

type _goml_m_std_p_json_p_JsonSerializeFrame_Struct struct {
    _0 int
    _1 *ref_int_x
    _2 *ref_bool_x
}

func (_ _goml_m_std_p_json_p_JsonSerializeFrame_Struct) is_goml_m_std_p_json_p_JsonSerializeFrame() {}

type _goml_m_std_p_json_p_JsonSerializeFrame_Variant struct {
    _0 int
    _1 int
    _2 *ref_int_x
    _3 *ref_bool_x
}

func (_ _goml_m_std_p_json_p_JsonSerializeFrame_Variant) is_goml_m_std_p_json_p_JsonSerializeFrame() {}

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

type _goml_m_Option____std_p_serde_p_FieldKey interface {
    is_goml_m_Option____std_p_serde_p_FieldKey()
}

type _goml_m_Option____std_p_serde_p_FieldKey_None struct {}

func (_ _goml_m_Option____std_p_serde_p_FieldKey_None) is_goml_m_Option____std_p_serde_p_FieldKey() {}

type _goml_m_Option____std_p_serde_p_FieldKey_Some struct {
    _0 _goml_m_std_p_serde_p_FieldKey
}

func (_ _goml_m_Option____std_p_serde_p_FieldKey_Some) is_goml_m_Option____std_p_serde_p_FieldKey() {}

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

type _goml_m_Option____std_p_serde_p_Value interface {
    is_goml_m_Option____std_p_serde_p_Value()
}

type _goml_m_Option____std_p_serde_p_Value_None struct {}

func (_ _goml_m_Option____std_p_serde_p_Value_None) is_goml_m_Option____std_p_serde_p_Value() {}

type _goml_m_Option____std_p_serde_p_Value_Some struct {
    _0 _goml_m_std_p_serde_p_Value
}

func (_ _goml_m_Option____std_p_serde_p_Value_Some) is_goml_m_Option____std_p_serde_p_Value() {}

type _goml_m_Option____std_p_serde_p_ValueDeserializeFrame interface {
    is_goml_m_Option____std_p_serde_p_ValueDeserializeFrame()
}

type _goml_m_Option____std_p_serde_p_ValueDeserializeFrame_None struct {}

func (_ _goml_m_Option____std_p_serde_p_ValueDeserializeFrame_None) is_goml_m_Option____std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_Option____std_p_serde_p_ValueDeserializeFrame_Some struct {
    _0 _goml_m_std_p_serde_p_ValueDeserializeFrame
}

func (_ _goml_m_Option____std_p_serde_p_ValueDeserializeFrame_Some) is_goml_m_Option____std_p_serde_p_ValueDeserializeFrame() {}

type _goml_m_Result____Vec_l_uint8_r_____string interface {
    is_goml_m_Result____Vec_l_uint8_r_____string()
}

type _goml_m_Result____Vec_l_uint8_r_____string_Ok struct {
    _0 *_goml_vec_uint8
}

func (_ _goml_m_Result____Vec_l_uint8_r_____string_Ok) is_goml_m_Result____Vec_l_uint8_r_____string() {}

type _goml_m_Result____Vec_l_uint8_r_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Vec_l_uint8_r_____string_Err) is_goml_m_Result____Vec_l_uint8_r_____string() {}

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

type Result__Option__int__string interface {
    isResult__Option__int__string()
}

type Result__Option__int__string_Ok struct {
    _0 Option__int
}

func (_ Result__Option__int__string_Ok) isResult__Option__int__string() {}

type Result__Option__int__string_Err struct {
    _0 string
}

func (_ Result__Option__int__string_Err) isResult__Option__int__string() {}

type _goml_m_Result____Option____std_p_serde_p_FieldKey____string interface {
    is_goml_m_Result____Option____std_p_serde_p_FieldKey____string()
}

type _goml_m_Result____Option____std_p_serde_p_FieldKey____string_Ok struct {
    _0 _goml_m_Option____std_p_serde_p_FieldKey
}

func (_ _goml_m_Result____Option____std_p_serde_p_FieldKey____string_Ok) is_goml_m_Result____Option____std_p_serde_p_FieldKey____string() {}

type _goml_m_Result____Option____std_p_serde_p_FieldKey____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Option____std_p_serde_p_FieldKey____string_Err) is_goml_m_Result____Option____std_p_serde_p_FieldKey____string() {}

type _goml_m_Result____std_p_serde_p_VariantKey____string interface {
    is_goml_m_Result____std_p_serde_p_VariantKey____string()
}

type _goml_m_Result____std_p_serde_p_VariantKey____string_Ok struct {
    _0 _goml_m_std_p_serde_p_VariantKey
}

func (_ _goml_m_Result____std_p_serde_p_VariantKey____string_Ok) is_goml_m_Result____std_p_serde_p_VariantKey____string() {}

type _goml_m_Result____std_p_serde_p_VariantKey____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_serde_p_VariantKey____string_Err) is_goml_m_Result____std_p_serde_p_VariantKey____string() {}

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame interface {
    is_goml_m_Option____std_p_serde_p_ValueSerializeFrame()
}

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame_None struct {}

func (_ _goml_m_Option____std_p_serde_p_ValueSerializeFrame_None) is_goml_m_Option____std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame_Some struct {
    _0 _goml_m_std_p_serde_p_ValueSerializeFrame
}

func (_ _goml_m_Option____std_p_serde_p_ValueSerializeFrame_Some) is_goml_m_Option____std_p_serde_p_ValueSerializeFrame() {}

type Option__uint8 interface {
    isOption__uint8()
}

type Option__uint8_None struct {}

func (_ Option__uint8_None) isOption__uint8() {}

type Option__uint8_Some struct {
    _0 uint8
}

func (_ Option__uint8_Some) isOption__uint8() {}

type _goml_m_Option_____o_string_c_string_q_ interface {
    is_goml_m_Option_____o_string_c_string_q_()
}

type _goml_m_Option_____o_string_c_string_q__None struct {}

func (_ _goml_m_Option_____o_string_c_string_q__None) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option_____o_string_c_string_q__Some struct {
    _0 Tuple2_6string_6string
}

func (_ _goml_m_Option_____o_string_c_string_q__Some) is_goml_m_Option_____o_string_c_string_q_() {}

type _goml_m_Option____std_p_json_p_JsonDeserializeFrame interface {
    is_goml_m_Option____std_p_json_p_JsonDeserializeFrame()
}

type _goml_m_Option____std_p_json_p_JsonDeserializeFrame_None struct {}

func (_ _goml_m_Option____std_p_json_p_JsonDeserializeFrame_None) is_goml_m_Option____std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_Option____std_p_json_p_JsonDeserializeFrame_Some struct {
    _0 _goml_m_std_p_json_p_JsonDeserializeFrame
}

func (_ _goml_m_Option____std_p_json_p_JsonDeserializeFrame_Some) is_goml_m_Option____std_p_json_p_JsonDeserializeFrame() {}

type _goml_m_Result_____o_bool_c_bool_c_bool_q_____string interface {
    is_goml_m_Result_____o_bool_c_bool_c_bool_q_____string()
}

type _goml_m_Result_____o_bool_c_bool_c_bool_q_____string_Ok struct {
    _0 Tuple3_4bool_4bool_4bool
}

func (_ _goml_m_Result_____o_bool_c_bool_c_bool_q_____string_Ok) is_goml_m_Result_____o_bool_c_bool_c_bool_q_____string() {}

type _goml_m_Result_____o_bool_c_bool_c_bool_q_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result_____o_bool_c_bool_c_bool_q_____string_Err) is_goml_m_Result_____o_bool_c_bool_c_bool_q_____string() {}

type _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string interface {
    is_goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string()
}

type _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string_Ok struct {
    _0 Tuple3_4bool_4bool_14Option__string
}

func (_ _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string_Ok) is_goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string() {}

type _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string_Err) is_goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string() {}

type Result__Option__string__string interface {
    isResult__Option__string__string()
}

type Result__Option__string__string_Ok struct {
    _0 Option__string
}

func (_ Result__Option__string__string_Ok) isResult__Option__string__string() {}

type Result__Option__string__string_Err struct {
    _0 string
}

func (_ Result__Option__string__string_Err) isResult__Option__string__string() {}

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

type _goml_m_Option_____o_char_c_int_q_ interface {
    is_goml_m_Option_____o_char_c_int_q_()
}

type _goml_m_Option_____o_char_c_int_q__None struct {}

func (_ _goml_m_Option_____o_char_c_int_q__None) is_goml_m_Option_____o_char_c_int_q_() {}

type _goml_m_Option_____o_char_c_int_q__Some struct {
    _0 Tuple2_4char_3int
}

func (_ _goml_m_Option_____o_char_c_int_q__Some) is_goml_m_Option_____o_char_c_int_q_() {}

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

type _goml_m_Option____std_p_json_p_JsonSerializeFrame interface {
    is_goml_m_Option____std_p_json_p_JsonSerializeFrame()
}

type _goml_m_Option____std_p_json_p_JsonSerializeFrame_None struct {}

func (_ _goml_m_Option____std_p_json_p_JsonSerializeFrame_None) is_goml_m_Option____std_p_json_p_JsonSerializeFrame() {}

type _goml_m_Option____std_p_json_p_JsonSerializeFrame_Some struct {
    _0 _goml_m_std_p_json_p_JsonSerializeFrame
}

func (_ _goml_m_Option____std_p_json_p_JsonSerializeFrame_Some) is_goml_m_Option____std_p_json_p_JsonSerializeFrame() {}

func _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new() _goml_m_std_p_text_p_StringBuilder {
    var vec_literal__178 *_goml_vec_uint8
    var inline7924 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline7924
    var t2224 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t2224
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline7939 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline7939
    var t2238 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t2238, length__5)
    var for_index1 int = 0
    Loop_loop2240:
    for {
        var t2241 bool = for_index1 < length__5
        if t2241 {
            var for_item3 int = for_index1
            var t2242 int = for_index1 + 1
            for_index1 = t2242
            var t2243 *_goml_vec_uint8 = self__3.values
            var t2244 uint8
            var inline7935 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2244 = inline7935
            vec_push__Vec_5uint8(t2243, t2244)
            continue
        } else {
            break Loop_loop2240
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t2247 string
    var inline7941 string = char_to_string(value__8)
    t2247 = inline7941
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2247)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t4144 string = "" + message__201
    var t4145 string = t4144 + " at byte "
    var t4146 *ref_int_x = value__200.index
    var t4147 int
    var inline9616 int = ref_get__Ref_3int(t4146)
    t4147 = inline9616
    var t4148 string
    var inline9614 string = _goml_runtime_core_int_to_string(t4147)
    t4148 = inline9614
    var t4149 string = t4145 + t4148
    return t4149
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4164:
    for {
        var t4172 *ref_int_x = value__203.index
        var t4173 int
        var inline9649 int = ref_get__Ref_3int(t4172)
        t4173 = inline9649
        var t4174 string = value__203.input
        var t4175 int
        var inline9647 int = _goml_runtime_core_string_len(t4174)
        t4175 = inline9647
        var t4176 bool = t4173 < t4175
        var jp4166 bool
        if t4176 {
            var t4177 string = value__203.input
            var t4178 *ref_int_x = value__203.index
            var t4179 int
            var inline9641 int = ref_get__Ref_3int(t4178)
            t4179 = inline9641
            var t4180 uint8
            var inline9639 uint8 = _goml_runtime_core_string_byte_get(t4177, t4179)
            t4180 = inline9639
            var inline9630 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4180, 9)
            var inline9632 bool
            if inline9630 {
                inline9632 = true
            } else {
                var inline9637 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4180, 10)
                inline9632 = inline9637
            }
            var inline9634 bool
            if inline9632 {
                inline9634 = true
            } else {
                var inline9636 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4180, 13)
                inline9634 = inline9636
            }
            if inline9634 {
                jp4166 = true
            } else {
                var inline9635 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4180, 32)
                jp4166 = inline9635
            }
        } else {
            jp4166 = false
        }
        if jp4166 {
            var t4167 *ref_int_x = value__203.index
            var t4168 *ref_int_x = value__203.index
            var t4169 int
            var inline9645 int = ref_get__Ref_3int(t4168)
            t4169 = inline9645
            var t4170 int = t4169 + 1
            ref_set__Ref_3int(t4167, t4170)
            continue
        } else {
            break Loop_loop4164
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__uint32 {
    var t4211 bool = value__204 >= 48
    var jp4187 bool
    if t4211 {
        var t4212 bool = value__204 <= 57
        jp4187 = t4212
    } else {
        jp4187 = false
    }
    if jp4187 {
        var t4188 uint8 = value__204 - 48
        var t4189 uint32 = uint32(uint8(t4188))
        var t4190 Option__uint32 = Option__uint32_Some{
            _0: t4189,
        }
        return t4190
    } else {
        var t4209 bool = value__204 >= 65
        var jp4194 bool
        if t4209 {
            var t4210 bool = value__204 <= 70
            jp4194 = t4210
        } else {
            jp4194 = false
        }
        if jp4194 {
            var t4195 uint8 = value__204 - 65
            var t4196 uint8 = t4195 + 10
            var t4197 uint32 = uint32(uint8(t4196))
            var t4198 Option__uint32 = Option__uint32_Some{
                _0: t4197,
            }
            return t4198
        } else {
            var t4207 bool = value__204 >= 97
            var jp4202 bool
            if t4207 {
                var t4208 bool = value__204 <= 102
                jp4202 = t4208
            } else {
                jp4202 = false
            }
            if jp4202 {
                var t4203 uint8 = value__204 - 97
                var t4204 uint8 = t4203 + 10
                var t4205 uint32 = uint32(uint8(t4204))
                var t4206 Option__uint32 = Option__uint32_Some{
                    _0: t4205,
                }
                return t4206
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t4217 *ref_int_x = value__205.index
    var t4218 int
    var inline9677 int = ref_get__Ref_3int(t4217)
    t4218 = inline9677
    var t4219 int = t4218 + 4
    var t4220 string = value__205.input
    var t4221 int
    var inline9675 int = _goml_runtime_core_string_len(t4220)
    t4221 = inline9675
    var t4222 bool = t4219 > t4221
    if t4222 {
        var t4223 string
        var inline9651 string = "incomplete unicode escape"
        var inline9652 string = "" + inline9651
        var inline9653 string = inline9652 + " at byte "
        var inline9654 *ref_int_x = value__205.index
        var inline9655 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9654)
        var inline9656 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9655)
        var inline9657 string = inline9653 + inline9656
        t4223 = inline9657
        var t4224 Result__uint32__string = Result__uint32__string_Err{
            _0: t4223,
        }
        return t4224
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop4231:
        for {
            var t4232 bool = for_index744 < for_limit745
            if t4232 {
                var for_item746 int = for_index744
                var t4233 int = for_index744 + 1
                for_index744 = t4233
                var t4234 string = value__205.input
                var t4235 *ref_int_x = value__205.index
                var t4236 int
                var inline9669 int = ref_get__Ref_3int(t4235)
                t4236 = inline9669
                var t4237 int = t4236 + for_item746
                var t4238 uint8
                var inline9667 uint8 = _goml_runtime_core_string_byte_get(t4234, t4237)
                t4238 = inline9667
                var mtmp748 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t4238)
                switch mtmp748.(type) {
                case Option__uint32_None:
                    var t4240 string
                    var inline9659 string = "invalid unicode escape"
                    var inline9660 string = "" + inline9659
                    var inline9661 string = inline9660 + " at byte "
                    var inline9662 *ref_int_x = value__205.index
                    var inline9663 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9662)
                    var inline9664 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9663)
                    var inline9665 string = inline9661 + inline9664
                    t4240 = inline9665
                    var t4241 Result__uint32__string = Result__uint32__string_Err{
                        _0: t4240,
                    }
                    return t4241
                case Option__uint32_Some:
                    var x749 uint32 = mtmp748.(Option__uint32_Some)._0
                    var t4242 uint32 = result__206 * 16
                    var t4243 uint32 = t4242 + x749
                    result__206 = t4243
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4231
            }
        }
        var t4226 *ref_int_x = value__205.index
        var t4227 *ref_int_x = value__205.index
        var t4228 int
        var inline9673 int = ref_get__Ref_3int(t4227)
        t4228 = inline9673
        var t4229 int = t4228 + 4
        ref_set__Ref_3int(t4226, t4229)
        var t4230 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__206,
        }
        return t4230
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var commute_field11029 rune
    var inline9690 bool = utf8_valid_scalar(codepoint__211)
    if inline9690 {
        var inline9691 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__211)
        var inline9693 rune = inline9691._1
        commute_field11029 = inline9693
        var inline9687 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field11029)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline9687)
        var t4250 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t4250
    } else {
        var t4248 string
        var inline9679 string = "invalid unicode codepoint"
        var inline9680 string = "" + inline9679
        var inline9681 string = inline9680 + " at byte "
        var inline9682 *ref_int_x = value__209.index
        var inline9683 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9682)
        var inline9684 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9683)
        var inline9685 string = inline9681 + inline9684
        t4248 = inline9685
        var t4249 Result__unit__string = Result__unit__string_Err{
            _0: t4248,
        }
        return t4249
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4254 uint32
    switch mtmp756.(type) {
    case Result__uint32__string_Ok:
        var x757 uint32 = mtmp756.(Result__uint32__string_Ok)._0
        jp4254 = x757
        var t4316 bool = jp4254 >= 55296
        var jp4258 bool
        if t4316 {
            var t4317 bool = jp4254 <= 56319
            jp4258 = t4317
        } else {
            jp4258 = false
        }
        if jp4258 {
            var t4295 *ref_int_x = value__213.index
            var t4296 int
            var inline9741 int = ref_get__Ref_3int(t4295)
            t4296 = inline9741
            var t4297 int = t4296 + 2
            var t4298 string = value__213.input
            var t4299 int
            var inline9739 int = _goml_runtime_core_string_len(t4298)
            t4299 = inline9739
            var t4300 bool = t4297 > t4299
            var jp4287 bool
            if t4300 {
                jp4287 = true
            } else {
                var t4301 string = value__213.input
                var t4302 *ref_int_x = value__213.index
                var t4303 int
                var inline9702 int = ref_get__Ref_3int(t4302)
                t4303 = inline9702
                var t4304 uint8
                var inline9700 uint8 = _goml_runtime_core_string_byte_get(t4301, t4303)
                t4304 = inline9700
                var t4305 bool
                var inline9697 uint8 = 92
                var inline9698 bool = t4304 == inline9697
                t4305 = inline9698
                var t4306 bool = !t4305
                jp4287 = t4306
            }
            var jp4262 bool
            if jp4287 {
                jp4262 = true
            } else {
                var t4288 string = value__213.input
                var t4289 *ref_int_x = value__213.index
                var t4290 int
                var inline9709 int = ref_get__Ref_3int(t4289)
                t4290 = inline9709
                var t4291 int = t4290 + 1
                var t4292 uint8
                var inline9707 uint8 = _goml_runtime_core_string_byte_get(t4288, t4291)
                t4292 = inline9707
                var t4293 bool
                var inline9704 uint8 = 117
                var inline9705 bool = t4292 == inline9704
                t4293 = inline9705
                var t4294 bool = !t4293
                jp4262 = t4294
            }
            if jp4262 {
                var t4263 string
                var inline9711 string = "missing low surrogate"
                var inline9712 string = "" + inline9711
                var inline9713 string = inline9712 + " at byte "
                var inline9714 *ref_int_x = value__213.index
                var inline9715 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9714)
                var inline9716 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9715)
                var inline9717 string = inline9713 + inline9716
                t4263 = inline9717
                var t4264 Result__unit__string = Result__unit__string_Err{
                    _0: t4263,
                }
                return t4264
            } else {
                var t4265 *ref_int_x = value__213.index
                var t4266 *ref_int_x = value__213.index
                var t4267 int
                var inline9737 int = ref_get__Ref_3int(t4266)
                t4267 = inline9737
                var t4268 int = t4267 + 2
                ref_set__Ref_3int(t4265, t4268)
                var mtmp760 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4270 uint32
                switch mtmp760.(type) {
                case Result__uint32__string_Ok:
                    var x761 uint32 = mtmp760.(Result__uint32__string_Ok)._0
                    jp4270 = x761
                    var t4283 bool = jp4270 < 56320
                    var jp4274 bool
                    if t4283 {
                        jp4274 = true
                    } else {
                        var t4284 bool = jp4270 > 57343
                        jp4274 = t4284
                    }
                    if jp4274 {
                        var t4275 string
                        var inline9719 string = "invalid low surrogate"
                        var inline9720 string = "" + inline9719
                        var inline9721 string = inline9720 + " at byte "
                        var inline9722 *ref_int_x = value__213.index
                        var inline9723 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9722)
                        var inline9724 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9723)
                        var inline9725 string = inline9721 + inline9724
                        t4275 = inline9725
                        var t4276 Result__unit__string = Result__unit__string_Err{
                            _0: t4275,
                        }
                        return t4276
                    } else {
                        var t4277 uint32 = jp4254 - 55296
                        var t4278 uint32 = t4277 * 1024
                        var t4279 uint32 = 65536 + t4278
                        var t4280 uint32 = t4279 + jp4270
                        var t4281 uint32 = t4280 - 56320
                        var inline9727 Option__char = char_from_uint32(t4281)
                        switch inline9727.(type) {
                        case Option__char_None:
                            var inline9728 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline9729 Result__unit__string = Result__unit__string_Err{
                                _0: inline9728,
                            }
                            return inline9729
                        case Option__char_Some:
                            var inline9730 rune = inline9727.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline9730)
                            var inline9733 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline9733
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x762 string = mtmp760.(Result__uint32__string_Err)._0
                    var t4285 Result__unit__string = Result__unit__string_Err{
                        _0: x762,
                    }
                    return t4285
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t4314 bool = jp4254 >= 56320
            var jp4310 bool
            if t4314 {
                var t4315 bool = jp4254 <= 57343
                jp4310 = t4315
            } else {
                jp4310 = false
            }
            if jp4310 {
                var t4311 string = _goml_m_std_p_json_p_json__error(value__213, "unexpected low surrogate")
                var t4312 Result__unit__string = Result__unit__string_Err{
                    _0: t4311,
                }
                return t4312
            } else {
                var t4313 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4254)
                return t4313
            }
        }
    case Result__uint32__string_Err:
        var x758 string = mtmp756.(Result__uint32__string_Err)._0
        var t4318 Result__unit__string = Result__unit__string_Err{
            _0: x758,
        }
        return t4318
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4434 *ref_int_x = value__217.index
    var t4435 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4434)
    var t4436 string = value__217.input
    var t4437 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4436)
    var t4438 bool = t4435 >= t4437
    var jp4426 bool
    if t4438 {
        jp4426 = true
    } else {
        var t4439 string = value__217.input
        var t4440 *ref_int_x = value__217.index
        var t4441 int
        var inline9748 int = ref_get__Ref_3int(t4440)
        t4441 = inline9748
        var t4442 uint8
        var inline9746 uint8 = _goml_runtime_core_string_byte_get(t4439, t4441)
        t4442 = inline9746
        var t4443 bool
        var inline9743 uint8 = 34
        var inline9744 bool = t4442 == inline9743
        t4443 = inline9744
        var t4444 bool = !t4443
        jp4426 = t4444
    }
    if jp4426 {
        var t4427 string
        var inline9750 string = "expected string"
        var inline9751 string = "" + inline9750
        var inline9752 string = inline9751 + " at byte "
        var inline9753 *ref_int_x = value__217.index
        var inline9754 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9753)
        var inline9755 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9754)
        var inline9756 string = inline9752 + inline9755
        t4427 = inline9756
        var t4428 Result__string__string = Result__string__string_Err{
            _0: t4427,
        }
        return t4428
    } else {
        var t4429 *ref_int_x = value__217.index
        var t4430 *ref_int_x = value__217.index
        var t4431 int
        var inline9760 int = ref_get__Ref_3int(t4430)
        t4431 = inline9760
        var t4432 int = t4431 + 1
        ref_set__Ref_3int(t4429, t4432)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t4322 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4322)
        Loop_loop4326:
        for {
            var t4327 *ref_int_x = value__217.index
            var t4328 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4327)
            var t4329 string = value__217.input
            var t4330 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4329)
            var t4331 bool = t4328 < t4330
            if t4331 {
                var t4332 string = value__217.input
                var t4333 *ref_int_x = value__217.index
                var t4334 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4333)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4332, t4334)
                var t4336 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(byte__220, 34)
                if t4336 {
                    var t4344 *ref_int_x = value__217.index
                    var t4345 int
                    var inline9776 int = ref_get__Ref_3int(t4344)
                    t4345 = inline9776
                    var t4346 bool = segment__219 < t4345
                    if t4346 {
                        var t4347 string = value__217.input
                        var t4348 *ref_int_x = value__217.index
                        var t4349 int
                        var inline9764 int = ref_get__Ref_3int(t4348)
                        t4349 = inline9764
                        var t4350 string
                        var inline9762 string = string_byte_slice(t4347, segment__219, t4349)
                        t4350 = inline9762
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4350)
                    } else {}
                    var t4338 *ref_int_x = value__217.index
                    var t4339 *ref_int_x = value__217.index
                    var t4340 int
                    var inline9774 int = ref_get__Ref_3int(t4339)
                    t4340 = inline9774
                    var t4341 int = t4340 + 1
                    ref_set__Ref_3int(t4338, t4341)
                    var t4342 string
                    var inline9766 *_goml_vec_uint8 = builder__218.values
                    var inline9767 Tuple2_4bool_6string = string_from_utf8(inline9766)
                    var inline9769 string = inline9767._1
                    t4342 = inline9769
                    var t4343 Result__string__string = Result__string__string_Ok{
                        _0: t4342,
                    }
                    return t4343
                } else {
                    var t4353 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(byte__220, 92)
                    if t4353 {
                        var t4408 *ref_int_x = value__217.index
                        var t4409 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4408)
                        var t4410 bool = segment__219 < t4409
                        if t4410 {
                            var t4411 string = value__217.input
                            var t4412 *ref_int_x = value__217.index
                            var t4413 int
                            var inline9780 int = ref_get__Ref_3int(t4412)
                            t4413 = inline9780
                            var t4414 string
                            var inline9778 string = string_byte_slice(t4411, segment__219, t4413)
                            t4414 = inline9778
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4414)
                        } else {}
                        var t4355 *ref_int_x = value__217.index
                        var t4356 *ref_int_x = value__217.index
                        var t4357 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4356)
                        var t4358 int = t4357 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4355, t4358)
                        var t4401 *ref_int_x = value__217.index
                        var t4402 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4401)
                        var t4403 string = value__217.input
                        var t4404 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4403)
                        var t4405 bool = t4402 >= t4404
                        if t4405 {
                            var t4406 string
                            var inline9782 string = "incomplete escape"
                            var inline9783 string = "" + inline9782
                            var inline9784 string = inline9783 + " at byte "
                            var inline9785 *ref_int_x = value__217.index
                            var inline9786 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9785)
                            var inline9787 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9786)
                            var inline9788 string = inline9784 + inline9787
                            t4406 = inline9788
                            var t4407 Result__string__string = Result__string__string_Err{
                                _0: t4406,
                            }
                            return t4407
                        } else {
                            var t4360 string = value__217.input
                            var t4361 *ref_int_x = value__217.index
                            var t4362 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4361)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4360, t4362)
                            var t4363 *ref_int_x = value__217.index
                            var t4364 *ref_int_x = value__217.index
                            var t4365 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4364)
                            var t4366 int = t4365 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4363, t4366)
                            var t4370 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 34)
                            if t4370 {
                                var inline9790 rune = 34
                                var inline9791 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9790)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline9791)
                                var t4368 *ref_int_x = value__217.index
                                var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                segment__219 = t4369
                                continue
                            } else {
                                var t4373 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 92)
                                if t4373 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 92)
                                    var t4368 *ref_int_x = value__217.index
                                    var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                    segment__219 = t4369
                                    continue
                                } else {
                                    var t4376 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 47)
                                    if t4376 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t4368 *ref_int_x = value__217.index
                                        var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                        segment__219 = t4369
                                        continue
                                    } else {
                                        var t4379 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 98)
                                        if t4379 {
                                            var mtmp770 Option__char = char_from_uint32(8)
                                            switch mtmp770.(type) {
                                            case Option__char_None:
                                                var t4368 *ref_int_x = value__217.index
                                                var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                                segment__219 = t4369
                                                continue
                                            case Option__char_Some:
                                                var x771 rune = mtmp770.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t4368 *ref_int_x = value__217.index
                                                var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                                segment__219 = t4369
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t4383 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 102)
                                            if t4383 {
                                                var mtmp772 Option__char = char_from_uint32(12)
                                                switch mtmp772.(type) {
                                                case Option__char_None:
                                                    var t4368 *ref_int_x = value__217.index
                                                    var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                                    segment__219 = t4369
                                                    continue
                                                case Option__char_Some:
                                                    var x773 rune = mtmp772.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t4368 *ref_int_x = value__217.index
                                                    var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                                    segment__219 = t4369
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t4387 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 110)
                                                if t4387 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t4368 *ref_int_x = value__217.index
                                                    var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                                    segment__219 = t4369
                                                    continue
                                                } else {
                                                    var t4390 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 114)
                                                    if t4390 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t4368 *ref_int_x = value__217.index
                                                        var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                                        segment__219 = t4369
                                                        continue
                                                    } else {
                                                        var t4393 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 116)
                                                        if t4393 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t4368 *ref_int_x = value__217.index
                                                            var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                                            segment__219 = t4369
                                                            continue
                                                        } else {
                                                            var t4396 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 117)
                                                            if t4396 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t4368 *ref_int_x = value__217.index
                                                                    var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                                                                    segment__219 = t4369
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x776 string = mtmp774.(Result__unit__string_Err)._0
                                                                    var t4398 Result__string__string = Result__string__string_Err{
                                                                        _0: x776,
                                                                    }
                                                                    return t4398
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t4399 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t4400 Result__string__string = Result__string__string_Err{
                                                                    _0: t4399,
                                                                }
                                                                return t4400
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
                        var t4417 bool = byte__220 < 32
                        if t4417 {
                            var t4418 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t4419 Result__string__string = Result__string__string_Err{
                                _0: t4418,
                            }
                            return t4419
                        } else {
                            var t4420 *ref_int_x = value__217.index
                            var t4421 *ref_int_x = value__217.index
                            var t4422 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4421)
                            var t4423 int = t4422 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4420, t4423)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop4326
            }
        }
        var t4324 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t4325 Result__string__string = Result__string__string_Err{
            _0: t4324,
        }
        return t4325
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t4453 *ref_int_x = value__225.index
    var start__226 int
    var inline9811 int = ref_get__Ref_3int(t4453)
    start__226 = inline9811
    Loop_loop4458:
    for {
        var t4466 *ref_int_x = value__225.index
        var t4467 int
        var inline9807 int = ref_get__Ref_3int(t4466)
        t4467 = inline9807
        var t4468 string = value__225.input
        var t4469 int
        var inline9805 int = _goml_runtime_core_string_len(t4468)
        t4469 = inline9805
        var t4470 bool = t4467 < t4469
        var jp4460 bool
        if t4470 {
            var t4471 string = value__225.input
            var t4472 *ref_int_x = value__225.index
            var t4473 int
            var inline9799 int = ref_get__Ref_3int(t4472)
            t4473 = inline9799
            var t4474 uint8
            var inline9797 uint8 = _goml_runtime_core_string_byte_get(t4471, t4473)
            t4474 = inline9797
            var inline9794 bool = t4474 >= 48
            if inline9794 {
                var inline9795 bool = t4474 <= 57
                jp4460 = inline9795
            } else {
                jp4460 = false
            }
        } else {
            jp4460 = false
        }
        if jp4460 {
            var t4461 *ref_int_x = value__225.index
            var t4462 *ref_int_x = value__225.index
            var t4463 int
            var inline9803 int = ref_get__Ref_3int(t4462)
            t4463 = inline9803
            var t4464 int = t4463 + 1
            ref_set__Ref_3int(t4461, t4464)
            continue
        } else {
            break Loop_loop4458
        }
    }
    var t4455 *ref_int_x = value__225.index
    var t4456 int
    var inline9809 int = ref_get__Ref_3int(t4455)
    t4456 = inline9809
    var t4457 bool = t4456 > start__226
    return t4457
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4478 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4478)
    var t4599 string = value__227.input
    var t4600 *ref_int_x = value__227.index
    var t4601 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4600)
    var t4602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4599, t4601)
    var t4603 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4602, 45)
    if t4603 {
        var t4604 *ref_int_x = value__227.index
        var t4605 *ref_int_x = value__227.index
        var t4606 int
        var inline9815 int = ref_get__Ref_3int(t4605)
        t4606 = inline9815
        var t4607 int = t4606 + 1
        ref_set__Ref_3int(t4604, t4607)
    } else {}
    var t4562 *ref_int_x = value__227.index
    var t4563 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4562)
    var t4564 string = value__227.input
    var t4565 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4564)
    var t4566 bool = t4563 >= t4565
    if t4566 {
        var t4567 string
        var inline9817 string = "incomplete number"
        var inline9818 string = "" + inline9817
        var inline9819 string = inline9818 + " at byte "
        var inline9820 *ref_int_x = value__227.index
        var inline9821 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9820)
        var inline9822 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9821)
        var inline9823 string = inline9819 + inline9822
        t4567 = inline9823
        var t4568 Result__string__string = Result__string__string_Err{
            _0: t4567,
        }
        return t4568
    } else {
        var t4570 string = value__227.input
        var t4571 *ref_int_x = value__227.index
        var t4572 int
        var inline9861 int = ref_get__Ref_3int(t4571)
        t4572 = inline9861
        var t4573 uint8
        var inline9859 uint8 = _goml_runtime_core_string_byte_get(t4570, t4572)
        t4573 = inline9859
        var t4574 bool
        var inline9856 uint8 = 48
        var inline9857 bool = t4573 == inline9856
        t4574 = inline9857
        if t4574 {
            var t4575 *ref_int_x = value__227.index
            var t4576 *ref_int_x = value__227.index
            var t4577 int
            var inline9846 int = ref_get__Ref_3int(t4576)
            t4577 = inline9846
            var t4578 int = t4577 + 1
            ref_set__Ref_3int(t4575, t4578)
            var t4584 *ref_int_x = value__227.index
            var t4585 int
            var inline9842 int = ref_get__Ref_3int(t4584)
            t4585 = inline9842
            var t4586 string = value__227.input
            var t4587 int
            var inline9840 int = _goml_runtime_core_string_len(t4586)
            t4587 = inline9840
            var t4588 bool = t4585 < t4587
            var jp4581 bool
            if t4588 {
                var t4589 string = value__227.input
                var t4590 *ref_int_x = value__227.index
                var t4591 int
                var inline9830 int = ref_get__Ref_3int(t4590)
                t4591 = inline9830
                var t4592 uint8
                var inline9828 uint8 = _goml_runtime_core_string_byte_get(t4589, t4591)
                t4592 = inline9828
                var inline9825 bool = t4592 >= 48
                if inline9825 {
                    var inline9826 bool = t4592 <= 57
                    jp4581 = inline9826
                } else {
                    jp4581 = false
                }
            } else {
                jp4581 = false
            }
            if jp4581 {
                var t4582 string
                var inline9832 string = "invalid leading zero"
                var inline9833 string = "" + inline9832
                var inline9834 string = inline9833 + " at byte "
                var inline9835 *ref_int_x = value__227.index
                var inline9836 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9835)
                var inline9837 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9836)
                var inline9838 string = inline9834 + inline9837
                t4582 = inline9838
                var t4583 Result__string__string = Result__string__string_Err{
                    _0: t4582,
                }
                return t4583
            } else {
                var t4552 *ref_int_x = value__227.index
                var t4553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4552)
                var t4554 string = value__227.input
                var t4555 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4554)
                var t4556 bool = t4553 < t4555
                var jp4542 bool
                if t4556 {
                    var t4557 string = value__227.input
                    var t4558 *ref_int_x = value__227.index
                    var t4559 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4558)
                    var t4560 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4557, t4559)
                    var t4561 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4560, 46)
                    jp4542 = t4561
                } else {
                    jp4542 = false
                }
                if jp4542 {
                    var t4543 *ref_int_x = value__227.index
                    var t4544 *ref_int_x = value__227.index
                    var t4545 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4544)
                    var t4546 int = t4545 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4543, t4546)
                    var t4548 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t4549 bool = !t4548
                    if t4549 {
                        var t4550 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t4551 Result__string__string = Result__string__string_Err{
                            _0: t4550,
                        }
                        return t4551
                    } else {
                        var t4524 *ref_int_x = value__227.index
                        var t4525 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4524)
                        var t4526 string = value__227.input
                        var t4527 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4526)
                        var t4528 bool = t4525 < t4527
                        var jp4489 bool
                        if t4528 {
                            var t4531 string = value__227.input
                            var t4532 *ref_int_x = value__227.index
                            var t4533 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4532)
                            var t4534 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4531, t4533)
                            var t4535 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4534, 101)
                            if t4535 {
                                jp4489 = true
                            } else {
                                var t4536 string = value__227.input
                                var t4537 *ref_int_x = value__227.index
                                var t4538 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4537)
                                var t4539 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4536, t4538)
                                var t4540 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4539, 69)
                                jp4489 = t4540
                            }
                        } else {
                            jp4489 = false
                        }
                        if jp4489 {
                            var t4490 *ref_int_x = value__227.index
                            var t4491 *ref_int_x = value__227.index
                            var t4492 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4491)
                            var t4493 int = t4492 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4490, t4493)
                            var t4507 *ref_int_x = value__227.index
                            var t4508 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4507)
                            var t4509 string = value__227.input
                            var t4510 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4509)
                            var t4511 bool = t4508 < t4510
                            var jp4501 bool
                            if t4511 {
                                var t4514 string = value__227.input
                                var t4515 *ref_int_x = value__227.index
                                var t4516 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4515)
                                var t4517 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4514, t4516)
                                var t4518 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4517, 43)
                                if t4518 {
                                    jp4501 = true
                                } else {
                                    var t4519 string = value__227.input
                                    var t4520 *ref_int_x = value__227.index
                                    var t4521 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4520)
                                    var t4522 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4519, t4521)
                                    var t4523 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4522, 45)
                                    jp4501 = t4523
                                }
                            } else {
                                jp4501 = false
                            }
                            if jp4501 {
                                var t4502 *ref_int_x = value__227.index
                                var t4503 *ref_int_x = value__227.index
                                var t4504 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4503)
                                var t4505 int = t4504 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4502, t4505)
                            } else {}
                            var t4496 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4497 bool = !t4496
                            if t4497 {
                                var t4498 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4499 Result__string__string = Result__string__string_Err{
                                    _0: t4498,
                                }
                                return t4499
                            } else {
                                var t4483 string = value__227.input
                                var t4484 *ref_int_x = value__227.index
                                var t4485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4484)
                                var t4486 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4483, start__228, t4485)
                                var t4487 Result__string__string = Result__string__string_Ok{
                                    _0: t4486,
                                }
                                return t4487
                            }
                        } else {
                            var t4483 string = value__227.input
                            var t4484 *ref_int_x = value__227.index
                            var t4485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4484)
                            var t4486 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4483, start__228, t4485)
                            var t4487 Result__string__string = Result__string__string_Ok{
                                _0: t4486,
                            }
                            return t4487
                        }
                    }
                } else {
                    var t4524 *ref_int_x = value__227.index
                    var t4525 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4524)
                    var t4526 string = value__227.input
                    var t4527 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4526)
                    var t4528 bool = t4525 < t4527
                    var jp4489 bool
                    if t4528 {
                        var t4531 string = value__227.input
                        var t4532 *ref_int_x = value__227.index
                        var t4533 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4532)
                        var t4534 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4531, t4533)
                        var t4535 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4534, 101)
                        if t4535 {
                            jp4489 = true
                        } else {
                            var t4536 string = value__227.input
                            var t4537 *ref_int_x = value__227.index
                            var t4538 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4537)
                            var t4539 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4536, t4538)
                            var t4540 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4539, 69)
                            jp4489 = t4540
                        }
                    } else {
                        jp4489 = false
                    }
                    if jp4489 {
                        var t4490 *ref_int_x = value__227.index
                        var t4491 *ref_int_x = value__227.index
                        var t4492 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4491)
                        var t4493 int = t4492 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4490, t4493)
                        var t4507 *ref_int_x = value__227.index
                        var t4508 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4507)
                        var t4509 string = value__227.input
                        var t4510 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4509)
                        var t4511 bool = t4508 < t4510
                        var jp4501 bool
                        if t4511 {
                            var t4514 string = value__227.input
                            var t4515 *ref_int_x = value__227.index
                            var t4516 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4515)
                            var t4517 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4514, t4516)
                            var t4518 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4517, 43)
                            if t4518 {
                                jp4501 = true
                            } else {
                                var t4519 string = value__227.input
                                var t4520 *ref_int_x = value__227.index
                                var t4521 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4520)
                                var t4522 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4519, t4521)
                                var t4523 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4522, 45)
                                jp4501 = t4523
                            }
                        } else {
                            jp4501 = false
                        }
                        if jp4501 {
                            var t4502 *ref_int_x = value__227.index
                            var t4503 *ref_int_x = value__227.index
                            var t4504 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4503)
                            var t4505 int = t4504 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4502, t4505)
                        } else {}
                        var t4496 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4497 bool = !t4496
                        if t4497 {
                            var t4498 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4499 Result__string__string = Result__string__string_Err{
                                _0: t4498,
                            }
                            return t4499
                        } else {
                            var t4483 string = value__227.input
                            var t4484 *ref_int_x = value__227.index
                            var t4485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4484)
                            var t4486 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4483, start__228, t4485)
                            var t4487 Result__string__string = Result__string__string_Ok{
                                _0: t4486,
                            }
                            return t4487
                        }
                    } else {
                        var t4483 string = value__227.input
                        var t4484 *ref_int_x = value__227.index
                        var t4485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4484)
                        var t4486 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4483, start__228, t4485)
                        var t4487 Result__string__string = Result__string__string_Ok{
                            _0: t4486,
                        }
                        return t4487
                    }
                }
            }
        } else {
            var t4595 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t4596 bool = !t4595
            if t4596 {
                var t4597 string
                var inline9848 string = "expected number"
                var inline9849 string = "" + inline9848
                var inline9850 string = inline9849 + " at byte "
                var inline9851 *ref_int_x = value__227.index
                var inline9852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9851)
                var inline9853 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9852)
                var inline9854 string = inline9850 + inline9853
                t4597 = inline9854
                var t4598 Result__string__string = Result__string__string_Err{
                    _0: t4597,
                }
                return t4598
            } else {
                var t4552 *ref_int_x = value__227.index
                var t4553 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4552)
                var t4554 string = value__227.input
                var t4555 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4554)
                var t4556 bool = t4553 < t4555
                var jp4542 bool
                if t4556 {
                    var t4557 string = value__227.input
                    var t4558 *ref_int_x = value__227.index
                    var t4559 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4558)
                    var t4560 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4557, t4559)
                    var t4561 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4560, 46)
                    jp4542 = t4561
                } else {
                    jp4542 = false
                }
                if jp4542 {
                    var t4543 *ref_int_x = value__227.index
                    var t4544 *ref_int_x = value__227.index
                    var t4545 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4544)
                    var t4546 int = t4545 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4543, t4546)
                    var t4548 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t4549 bool = !t4548
                    if t4549 {
                        var t4550 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t4551 Result__string__string = Result__string__string_Err{
                            _0: t4550,
                        }
                        return t4551
                    } else {
                        var t4524 *ref_int_x = value__227.index
                        var t4525 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4524)
                        var t4526 string = value__227.input
                        var t4527 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4526)
                        var t4528 bool = t4525 < t4527
                        var jp4489 bool
                        if t4528 {
                            var t4531 string = value__227.input
                            var t4532 *ref_int_x = value__227.index
                            var t4533 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4532)
                            var t4534 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4531, t4533)
                            var t4535 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4534, 101)
                            if t4535 {
                                jp4489 = true
                            } else {
                                var t4536 string = value__227.input
                                var t4537 *ref_int_x = value__227.index
                                var t4538 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4537)
                                var t4539 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4536, t4538)
                                var t4540 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4539, 69)
                                jp4489 = t4540
                            }
                        } else {
                            jp4489 = false
                        }
                        if jp4489 {
                            var t4490 *ref_int_x = value__227.index
                            var t4491 *ref_int_x = value__227.index
                            var t4492 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4491)
                            var t4493 int = t4492 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4490, t4493)
                            var t4507 *ref_int_x = value__227.index
                            var t4508 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4507)
                            var t4509 string = value__227.input
                            var t4510 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4509)
                            var t4511 bool = t4508 < t4510
                            var jp4501 bool
                            if t4511 {
                                var t4514 string = value__227.input
                                var t4515 *ref_int_x = value__227.index
                                var t4516 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4515)
                                var t4517 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4514, t4516)
                                var t4518 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4517, 43)
                                if t4518 {
                                    jp4501 = true
                                } else {
                                    var t4519 string = value__227.input
                                    var t4520 *ref_int_x = value__227.index
                                    var t4521 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4520)
                                    var t4522 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4519, t4521)
                                    var t4523 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4522, 45)
                                    jp4501 = t4523
                                }
                            } else {
                                jp4501 = false
                            }
                            if jp4501 {
                                var t4502 *ref_int_x = value__227.index
                                var t4503 *ref_int_x = value__227.index
                                var t4504 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4503)
                                var t4505 int = t4504 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4502, t4505)
                            } else {}
                            var t4496 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4497 bool = !t4496
                            if t4497 {
                                var t4498 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4499 Result__string__string = Result__string__string_Err{
                                    _0: t4498,
                                }
                                return t4499
                            } else {
                                var t4483 string = value__227.input
                                var t4484 *ref_int_x = value__227.index
                                var t4485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4484)
                                var t4486 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4483, start__228, t4485)
                                var t4487 Result__string__string = Result__string__string_Ok{
                                    _0: t4486,
                                }
                                return t4487
                            }
                        } else {
                            var t4483 string = value__227.input
                            var t4484 *ref_int_x = value__227.index
                            var t4485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4484)
                            var t4486 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4483, start__228, t4485)
                            var t4487 Result__string__string = Result__string__string_Ok{
                                _0: t4486,
                            }
                            return t4487
                        }
                    }
                } else {
                    var t4524 *ref_int_x = value__227.index
                    var t4525 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4524)
                    var t4526 string = value__227.input
                    var t4527 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4526)
                    var t4528 bool = t4525 < t4527
                    var jp4489 bool
                    if t4528 {
                        var t4531 string = value__227.input
                        var t4532 *ref_int_x = value__227.index
                        var t4533 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4532)
                        var t4534 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4531, t4533)
                        var t4535 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4534, 101)
                        if t4535 {
                            jp4489 = true
                        } else {
                            var t4536 string = value__227.input
                            var t4537 *ref_int_x = value__227.index
                            var t4538 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4537)
                            var t4539 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4536, t4538)
                            var t4540 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4539, 69)
                            jp4489 = t4540
                        }
                    } else {
                        jp4489 = false
                    }
                    if jp4489 {
                        var t4490 *ref_int_x = value__227.index
                        var t4491 *ref_int_x = value__227.index
                        var t4492 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4491)
                        var t4493 int = t4492 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4490, t4493)
                        var t4507 *ref_int_x = value__227.index
                        var t4508 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4507)
                        var t4509 string = value__227.input
                        var t4510 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4509)
                        var t4511 bool = t4508 < t4510
                        var jp4501 bool
                        if t4511 {
                            var t4514 string = value__227.input
                            var t4515 *ref_int_x = value__227.index
                            var t4516 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4515)
                            var t4517 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4514, t4516)
                            var t4518 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4517, 43)
                            if t4518 {
                                jp4501 = true
                            } else {
                                var t4519 string = value__227.input
                                var t4520 *ref_int_x = value__227.index
                                var t4521 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4520)
                                var t4522 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4519, t4521)
                                var t4523 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4522, 45)
                                jp4501 = t4523
                            }
                        } else {
                            jp4501 = false
                        }
                        if jp4501 {
                            var t4502 *ref_int_x = value__227.index
                            var t4503 *ref_int_x = value__227.index
                            var t4504 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4503)
                            var t4505 int = t4504 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4502, t4505)
                        } else {}
                        var t4496 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4497 bool = !t4496
                        if t4497 {
                            var t4498 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4499 Result__string__string = Result__string__string_Err{
                                _0: t4498,
                            }
                            return t4499
                        } else {
                            var t4483 string = value__227.input
                            var t4484 *ref_int_x = value__227.index
                            var t4485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4484)
                            var t4486 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4483, start__228, t4485)
                            var t4487 Result__string__string = Result__string__string_Ok{
                                _0: t4486,
                            }
                            return t4487
                        }
                    } else {
                        var t4483 string = value__227.input
                        var t4484 *ref_int_x = value__227.index
                        var t4485 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4484)
                        var t4486 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4483, start__228, t4485)
                        var t4487 Result__string__string = Result__string__string_Ok{
                            _0: t4486,
                        }
                        return t4487
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t4630 *ref_int_x = value__230.index
    var t4631 int
    var inline9891 int = ref_get__Ref_3int(t4630)
    t4631 = inline9891
    var t4632 int
    var inline9889 int = _goml_runtime_core_string_len(expected__231)
    t4632 = inline9889
    var t4633 int = t4631 + t4632
    var t4634 string = value__230.input
    var t4635 int
    var inline9887 int = _goml_runtime_core_string_len(t4634)
    t4635 = inline9887
    var t4636 bool = t4633 <= t4635
    var jp4621 bool
    if t4636 {
        var t4637 string = value__230.input
        var t4638 *ref_int_x = value__230.index
        var t4639 int
        var inline9871 int = ref_get__Ref_3int(t4638)
        t4639 = inline9871
        var t4640 *ref_int_x = value__230.index
        var t4641 int
        var inline9869 int = ref_get__Ref_3int(t4640)
        t4641 = inline9869
        var t4642 int
        var inline9867 int = _goml_runtime_core_string_len(expected__231)
        t4642 = inline9867
        var t4643 int = t4641 + t4642
        var t4644 string
        var inline9865 string = string_byte_slice(t4637, t4639, t4643)
        t4644 = inline9865
        var inline9863 bool = t4644 == expected__231
        jp4621 = inline9863
    } else {
        jp4621 = false
    }
    if jp4621 {
        var t4622 *ref_int_x = value__230.index
        var t4623 *ref_int_x = value__230.index
        var t4624 int
        var inline9877 int = ref_get__Ref_3int(t4623)
        t4624 = inline9877
        var t4625 int
        var inline9875 int = _goml_runtime_core_string_len(expected__231)
        t4625 = inline9875
        var t4626 int = t4624 + t4625
        ref_set__Ref_3int(t4622, t4626)
        var t4627 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__232,
        }
        return t4627
    } else {
        var t4628 string
        var inline9879 string = "invalid literal"
        var inline9880 string = "" + inline9879
        var inline9881 string = inline9880 + " at byte "
        var inline9882 *ref_int_x = value__230.index
        var inline9883 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9882)
        var inline9884 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9883)
        var inline9885 string = inline9881 + inline9884
        t4628 = inline9885
        var t4629 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4628,
        }
        return t4629
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t4648 *ref_int_x = value__233.index
    var t4649 *ref_int_x = value__233.index
    var t4650 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4649)
    var t4651 int = t4650 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4648, t4651)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var vec_literal__8825 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t4706 *ref_int_x = value__233.index
    var t4707 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4706)
    var t4708 string = value__233.input
    var t4709 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4708)
    var t4710 bool = t4707 < t4709
    var jp4699 bool
    if t4710 {
        var t4711 string = value__233.input
        var t4712 *ref_int_x = value__233.index
        var t4713 int
        var inline9898 int = ref_get__Ref_3int(t4712)
        t4713 = inline9898
        var t4714 uint8
        var inline9896 uint8 = _goml_runtime_core_string_byte_get(t4711, t4713)
        t4714 = inline9896
        var inline9893 uint8 = 93
        var inline9894 bool = t4714 == inline9893
        jp4699 = inline9894
    } else {
        jp4699 = false
    }
    if jp4699 {
        var t4700 *ref_int_x = value__233.index
        var t4701 *ref_int_x = value__233.index
        var t4702 int
        var inline9902 int = ref_get__Ref_3int(t4701)
        t4702 = inline9902
        var t4703 int = t4702 + 1
        ref_set__Ref_3int(t4700, t4703)
        var t4704 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: vec_literal__8825,
        }
        var t4705 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t4704,
        }
        return t4705
    } else {
        Loop_loop4656:
        for {
            var t4657 *ref_int_x = value__233.index
            var t4658 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4657)
            var t4659 string = value__233.input
            var t4660 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4659)
            var t4661 bool = t4658 < t4660
            if t4661 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp4663 _goml_m_std_p_json_p_Value
                switch mtmp797.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp4663 = x798
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8825, jp4663)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t4665 *ref_int_x = value__233.index
                    var t4666 int
                    var inline9944 int = ref_get__Ref_3int(t4665)
                    t4666 = inline9944
                    var t4667 string = value__233.input
                    var t4668 int
                    var inline9942 int = _goml_runtime_core_string_len(t4667)
                    t4668 = inline9942
                    var t4669 bool = t4666 >= t4668
                    if t4669 {
                        var t4670 string
                        var inline9904 string = "unterminated array"
                        var inline9905 string = "" + inline9904
                        var inline9906 string = inline9905 + " at byte "
                        var inline9907 *ref_int_x = value__233.index
                        var inline9908 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9907)
                        var inline9909 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9908)
                        var inline9910 string = inline9906 + inline9909
                        t4670 = inline9910
                        var t4671 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t4670,
                        }
                        return t4671
                    } else {
                        var t4673 string = value__233.input
                        var t4674 *ref_int_x = value__233.index
                        var t4675 int
                        var inline9940 int = ref_get__Ref_3int(t4674)
                        t4675 = inline9940
                        var t4676 uint8
                        var inline9938 uint8 = _goml_runtime_core_string_byte_get(t4673, t4675)
                        t4676 = inline9938
                        var t4677 bool
                        var inline9935 uint8 = 93
                        var inline9936 bool = t4676 == inline9935
                        t4677 = inline9936
                        if t4677 {
                            var t4678 *ref_int_x = value__233.index
                            var t4679 *ref_int_x = value__233.index
                            var t4680 int
                            var inline9914 int = ref_get__Ref_3int(t4679)
                            t4680 = inline9914
                            var t4681 int = t4680 + 1
                            ref_set__Ref_3int(t4678, t4681)
                            var t4682 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: vec_literal__8825,
                            }
                            var t4683 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t4682,
                            }
                            return t4683
                        } else {
                            var t4685 string = value__233.input
                            var t4686 *ref_int_x = value__233.index
                            var t4687 int
                            var inline9933 int = ref_get__Ref_3int(t4686)
                            t4687 = inline9933
                            var t4688 uint8
                            var inline9931 uint8 = _goml_runtime_core_string_byte_get(t4685, t4687)
                            t4688 = inline9931
                            var t4689 bool
                            var inline9928 uint8 = 44
                            var inline9929 bool = t4688 == inline9928
                            t4689 = inline9929
                            if t4689 {
                                var t4690 *ref_int_x = value__233.index
                                var t4691 *ref_int_x = value__233.index
                                var t4692 int
                                var inline9918 int = ref_get__Ref_3int(t4691)
                                t4692 = inline9918
                                var t4693 int = t4692 + 1
                                ref_set__Ref_3int(t4690, t4693)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t4695 string
                                var inline9920 string = "expected array separator"
                                var inline9921 string = "" + inline9920
                                var inline9922 string = inline9921 + " at byte "
                                var inline9923 *ref_int_x = value__233.index
                                var inline9924 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9923)
                                var inline9925 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9924)
                                var inline9926 string = inline9922 + inline9925
                                t4695 = inline9926
                                var t4696 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t4695,
                                }
                                return t4696
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x799 string = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t4697 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x799,
                    }
                    return t4697
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4656
            }
        }
        var t4654 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t4655 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4654,
        }
        return t4655
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t4718 *ref_int_x = value__236.index
    var t4719 *ref_int_x = value__236.index
    var t4720 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4719)
    var t4721 int = t4720 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4718, t4721)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var vec_literal__10027 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t4801 *ref_int_x = value__236.index
    var t4802 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4801)
    var t4803 string = value__236.input
    var t4804 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4803)
    var t4805 bool = t4802 < t4804
    var jp4794 bool
    if t4805 {
        var t4806 string = value__236.input
        var t4807 *ref_int_x = value__236.index
        var t4808 int
        var inline9951 int = ref_get__Ref_3int(t4807)
        t4808 = inline9951
        var t4809 uint8
        var inline9949 uint8 = _goml_runtime_core_string_byte_get(t4806, t4808)
        t4809 = inline9949
        var inline9946 uint8 = 125
        var inline9947 bool = t4809 == inline9946
        jp4794 = inline9947
    } else {
        jp4794 = false
    }
    if jp4794 {
        var t4795 *ref_int_x = value__236.index
        var t4796 *ref_int_x = value__236.index
        var t4797 int
        var inline9955 int = ref_get__Ref_3int(t4796)
        t4797 = inline9955
        var t4798 int = t4797 + 1
        ref_set__Ref_3int(t4795, t4798)
        var t4799 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10027,
        }
        var t4800 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t4799,
        }
        return t4800
    } else {
        Loop_loop4726:
        for {
            var t4727 *ref_int_x = value__236.index
            var t4728 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4727)
            var t4729 string = value__236.input
            var t4730 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4729)
            var t4731 bool = t4728 < t4730
            if t4731 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp4733 string
                switch mtmp809.(type) {
                case Result__string__string_Ok:
                    var x810 string = mtmp809.(Result__string__string_Ok)._0
                    jp4733 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t4781 *ref_int_x = value__236.index
                    var t4782 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4781)
                    var t4783 string = value__236.input
                    var t4784 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4783)
                    var t4785 bool = t4782 >= t4784
                    var jp4773 bool
                    if t4785 {
                        jp4773 = true
                    } else {
                        var t4786 string = value__236.input
                        var t4787 *ref_int_x = value__236.index
                        var t4788 int
                        var inline9962 int = ref_get__Ref_3int(t4787)
                        t4788 = inline9962
                        var t4789 uint8
                        var inline9960 uint8 = _goml_runtime_core_string_byte_get(t4786, t4788)
                        t4789 = inline9960
                        var t4790 bool
                        var inline9957 uint8 = 58
                        var inline9958 bool = t4789 == inline9957
                        t4790 = inline9958
                        var t4791 bool = !t4790
                        jp4773 = t4791
                    }
                    if jp4773 {
                        var t4774 string
                        var inline9964 string = "expected object colon"
                        var inline9965 string = "" + inline9964
                        var inline9966 string = inline9965 + " at byte "
                        var inline9967 *ref_int_x = value__236.index
                        var inline9968 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9967)
                        var inline9969 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9968)
                        var inline9970 string = inline9966 + inline9969
                        t4774 = inline9970
                        var t4775 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t4774,
                        }
                        return t4775
                    } else {
                        var t4776 *ref_int_x = value__236.index
                        var t4777 *ref_int_x = value__236.index
                        var t4778 int
                        var inline9974 int = ref_get__Ref_3int(t4777)
                        t4778 = inline9974
                        var t4779 int = t4778 + 1
                        ref_set__Ref_3int(t4776, t4779)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp4736 _goml_m_std_p_json_p_Value
                        switch mtmp815.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp4736 = x816
                            var t4737 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp4733,
                                _1: jp4736,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10027, t4737)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t4739 *ref_int_x = value__236.index
                            var t4740 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4739)
                            var t4741 string = value__236.input
                            var t4742 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4741)
                            var t4743 bool = t4740 >= t4742
                            if t4743 {
                                var t4744 string
                                var inline9976 string = "unterminated object"
                                var inline9977 string = "" + inline9976
                                var inline9978 string = inline9977 + " at byte "
                                var inline9979 *ref_int_x = value__236.index
                                var inline9980 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9979)
                                var inline9981 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9980)
                                var inline9982 string = inline9978 + inline9981
                                t4744 = inline9982
                                var t4745 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t4744,
                                }
                                return t4745
                            } else {
                                var t4747 string = value__236.input
                                var t4748 *ref_int_x = value__236.index
                                var t4749 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4748)
                                var t4750 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4747, t4749)
                                var t4751 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4750, 125)
                                if t4751 {
                                    var t4752 *ref_int_x = value__236.index
                                    var t4753 *ref_int_x = value__236.index
                                    var t4754 int
                                    var inline9986 int = ref_get__Ref_3int(t4753)
                                    t4754 = inline9986
                                    var t4755 int = t4754 + 1
                                    ref_set__Ref_3int(t4752, t4755)
                                    var t4756 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10027,
                                    }
                                    var t4757 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t4756,
                                    }
                                    return t4757
                                } else {
                                    var t4759 string = value__236.input
                                    var t4760 *ref_int_x = value__236.index
                                    var t4761 int
                                    var inline9997 int = ref_get__Ref_3int(t4760)
                                    t4761 = inline9997
                                    var t4762 uint8
                                    var inline9995 uint8 = _goml_runtime_core_string_byte_get(t4759, t4761)
                                    t4762 = inline9995
                                    var t4763 bool
                                    var inline9992 uint8 = 44
                                    var inline9993 bool = t4762 == inline9992
                                    t4763 = inline9993
                                    if t4763 {
                                        var t4764 *ref_int_x = value__236.index
                                        var t4765 *ref_int_x = value__236.index
                                        var t4766 int
                                        var inline9990 int = ref_get__Ref_3int(t4765)
                                        t4766 = inline9990
                                        var t4767 int = t4766 + 1
                                        ref_set__Ref_3int(t4764, t4767)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t4769 string = _goml_m_std_p_json_p_json__error(value__236, "expected object separator")
                                        var t4770 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t4769,
                                        }
                                        return t4770
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x817 string = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t4771 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x817,
                            }
                            return t4771
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x811 string = mtmp809.(Result__string__string_Err)._0
                    var t4792 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x811,
                    }
                    return t4792
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4726
            }
        }
        var t4724 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t4725 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4724,
        }
        return t4725
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t4815 *ref_int_x = value__240.index
    var t4816 int
    var inline10038 int = ref_get__Ref_3int(t4815)
    t4816 = inline10038
    var t4817 string = value__240.input
    var t4818 int
    var inline10036 int = _goml_runtime_core_string_len(t4817)
    t4818 = inline10036
    var t4819 bool = t4816 >= t4818
    if t4819 {
        var t4820 string
        var inline9999 string = "expected JSON value"
        var inline10000 string = "" + inline9999
        var inline10001 string = inline10000 + " at byte "
        var inline10002 *ref_int_x = value__240.index
        var inline10003 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10002)
        var inline10004 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10003)
        var inline10005 string = inline10001 + inline10004
        t4820 = inline10005
        var t4821 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4820,
        }
        return t4821
    } else {
        var t4822 string = value__240.input
        var t4823 *ref_int_x = value__240.index
        var t4824 int
        var inline10034 int = ref_get__Ref_3int(t4823)
        t4824 = inline10034
        var mtmp824 uint8
        var inline10032 uint8 = _goml_runtime_core_string_byte_get(t4822, t4824)
        mtmp824 = inline10032
        switch mtmp824 {
        case 123:
            var t4827 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t4827
        case 91:
            var t4828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t4828
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825.(type) {
            case Result__string__string_Ok:
                var x826 string = mtmp825.(Result__string__string_Ok)._0
                var t4831 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t4832 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t4831,
                }
                return t4832
            case Result__string__string_Err:
                var x827 string = mtmp825.(Result__string__string_Err)._0
                var t4833 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x827,
                }
                return t4833
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t4834 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t4835 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t4834)
            return t4835
        case 102:
            var t4836 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t4837 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t4836)
            return t4837
        case 110:
            var t4838 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t4838
        default:
            var t4846 bool
            var inline10029 uint8 = 45
            var inline10030 bool = mtmp824 == inline10029
            t4846 = inline10030
            var jp4842 bool
            if t4846 {
                jp4842 = true
            } else {
                var inline10007 bool = mtmp824 >= 48
                if inline10007 {
                    var inline10008 bool = mtmp824 <= 57
                    jp4842 = inline10008
                } else {
                    jp4842 = false
                }
            }
            if jp4842 {
                var inline10010 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline10012 string
                switch inline10010.(type) {
                case Result__string__string_Ok:
                    var inline10015 string = inline10010.(Result__string__string_Ok)._0
                    inline10012 = inline10015
                    var inline10013 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline10012,
                    }
                    var inline10014 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                        _0: inline10013,
                    }
                    return inline10014
                case Result__string__string_Err:
                    var inline10017 string = inline10010.(Result__string__string_Err)._0
                    var inline10019 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: inline10017,
                    }
                    return inline10019
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t4844 string
                var inline10021 string = "unexpected JSON token"
                var inline10022 string = "" + inline10021
                var inline10023 string = inline10022 + " at byte "
                var inline10024 *ref_int_x = value__240.index
                var inline10025 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10024)
                var inline10026 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10025)
                var inline10027 string = inline10023 + inline10026
                t4844 = inline10027
                var t4845 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t4844,
                }
                return t4845
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline10054 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline10055 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline10054,
    }
    parser__245 = inline10055
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp4851 _goml_m_std_p_json_p_Value
    switch mtmp828.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x829 _goml_m_std_p_json_p_Value = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp4851 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t4854 *ref_int_x = parser__245.index
        var t4855 int
        var inline10052 int = ref_get__Ref_3int(t4854)
        t4855 = inline10052
        var t4856 int
        var inline10050 int = _goml_runtime_core_string_len(input__244)
        t4856 = inline10050
        var t4857 bool
        var inline10048 bool = t4855 == t4856
        t4857 = inline10048
        if t4857 {
            var t4858 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp4851,
            }
            return t4858
        } else {
            var t4859 string
            var inline10040 string = "trailing JSON data"
            var inline10041 string = "" + inline10040
            var inline10042 string = inline10041 + " at byte "
            var inline10043 *ref_int_x = parser__245.index
            var inline10044 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10043)
            var inline10045 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10044)
            var inline10046 string = inline10042 + inline10045
            t4859 = inline10046
            var t4860 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t4859,
            }
            return t4860
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x830 string = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t4861 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x830,
        }
        return t4861
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__248, 34)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__249)
    Loop_loop4875:
    for {
        var t4876 bool = for_index833 < for_limit834
        if t4876 {
            var for_item835 int = for_index833
            var t4877 int = for_index833 + 1
            for_index833 = t4877
            var byte__252 uint8
            var inline10116 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline10116
            var t4930 bool
            var inline10113 uint8 = 34
            var inline10114 bool = byte__252 == inline10113
            t4930 = inline10114
            var jp4928 bool
            if t4930 {
                jp4928 = true
            } else {
                var inline10060 uint8 = 92
                var inline10061 bool = byte__252 == inline10060
                jp4928 = inline10061
            }
            var jp4925 bool
            if jp4928 {
                jp4925 = true
            } else {
                var inline10063 uint8 = 8
                var inline10064 bool = byte__252 == inline10063
                jp4925 = inline10064
            }
            var jp4922 bool
            if jp4925 {
                jp4922 = true
            } else {
                var inline10066 uint8 = 9
                var inline10067 bool = byte__252 == inline10066
                jp4922 = inline10067
            }
            var jp4919 bool
            if jp4922 {
                jp4919 = true
            } else {
                var inline10069 uint8 = 10
                var inline10070 bool = byte__252 == inline10069
                jp4919 = inline10070
            }
            var jp4916 bool
            if jp4919 {
                jp4916 = true
            } else {
                var inline10072 uint8 = 12
                var inline10073 bool = byte__252 == inline10072
                jp4916 = inline10073
            }
            var jp4913 bool
            if jp4916 {
                jp4913 = true
            } else {
                var inline10075 uint8 = 13
                var inline10076 bool = byte__252 == inline10075
                jp4913 = inline10076
            }
            var jp4880 bool
            if jp4913 {
                jp4880 = true
            } else {
                var t4914 bool = byte__252 < 32
                jp4880 = t4914
            }
            if jp4880 {
                var t4909 bool = start__250 < for_item835
                if t4909 {
                    var t4910 string
                    var inline10078 string = string_byte_slice(value__249, start__250, for_item835)
                    t4910 = inline10078
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t4910)
                } else {}
                var t4884 bool
                var inline10110 uint8 = 34
                var inline10111 bool = byte__252 == inline10110
                t4884 = inline10111
                if t4884 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t4887 bool
                    var inline10107 uint8 = 92
                    var inline10108 bool = byte__252 == inline10107
                    t4887 = inline10108
                    if t4887 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t4890 bool
                        var inline10104 uint8 = 8
                        var inline10105 bool = byte__252 == inline10104
                        t4890 = inline10105
                        if t4890 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t4893 bool
                            var inline10101 uint8 = 9
                            var inline10102 bool = byte__252 == inline10101
                            t4893 = inline10102
                            if t4893 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t4896 bool
                                var inline10098 uint8 = 10
                                var inline10099 bool = byte__252 == inline10098
                                t4896 = inline10099
                                if t4896 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t4899 bool
                                    var inline10095 uint8 = 12
                                    var inline10096 bool = byte__252 == inline10095
                                    t4899 = inline10096
                                    if t4899 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t4902 bool
                                        var inline10092 uint8 = 13
                                        var inline10093 bool = byte__252 == inline10092
                                        t4902 = inline10093
                                        if t4902 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t4904 uint8 = byte__252 / 16
                                            var t4905 rune
                                            var inline10089 int = int(uint8(t4904))
                                            var inline10090 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10089)
                                            t4905 = inline10090
                                            var inline10086 string = _goml_m_inherent_i_char_i_char_i_to__string(t4905)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10086)
                                            var t4906_rhs uint8 = 16
                                            var t4906 uint8 = byte__252 % t4906_rhs
                                            var t4907 rune
                                            var inline10083 int = int(uint8(t4906))
                                            var inline10084 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10083)
                                            t4907 = inline10084
                                            var inline10080 string = _goml_m_inherent_i_char_i_char_i_to__string(t4907)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10080)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t4883 int = for_item835 + 1
                start__250 = t4883
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop4875
        }
    }
    var t4870 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__249)
    var t4871 bool = start__250 < t4870
    if t4871 {
        var t4872 int
        var inline10120 int = _goml_runtime_core_string_len(value__249)
        t4872 = inline10120
        var t4873 string
        var inline10118 string = string_byte_slice(value__249, start__250, t4872)
        t4873 = inline10118
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t4873)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__248, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline10134 rune = 123
        var inline10135 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10134)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10135)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop4936:
        for {
            var t4937 bool = for_index852 < for_limit851
            if t4937 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t4938 int = for_index852 + 1
                for_index852 = t4938
                var t4944 bool = index__256 > 0
                if t4944 {
                    var inline10122 rune = 44
                    var inline10123 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10122)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10123)
                } else {}
                var t4940 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t4940)
                var inline10126 rune = 58
                var inline10127 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10126)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10127)
                var t4941 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t4941)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t4942 int = compound_old859 + compound_value860
                index__256 = t4942
                continue
            } else {
                break Loop_loop4936
            }
        }
        var inline10130 rune = 125
        var inline10131 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10130)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10131)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline10146 rune = 91
        var inline10147 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10146)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10147)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop4948:
        for {
            var t4949 bool = for_index866 < for_limit865
            if t4949 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t4950 int = for_index866 + 1
                for_index866 = t4950
                var t4954 bool = index__259 > 0
                if t4954 {
                    var inline10138 rune = 44
                    var inline10139 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10138)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10139)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t4952 int = compound_old871 + compound_value872
                index__259 = t4952
                continue
            } else {
                break Loop_loop4948
            }
        }
        var inline10142 rune = 93
        var inline10143 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10142)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10143)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_String:
        var x846 string = value__254.(_goml_m_std_p_json_p_Value_String)._0
        _goml_m_std_p_json_p_write__json__string(builder__253, x846)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Number:
        var x847 string = value__254.(_goml_m_std_p_json_p_Value_Number)._0
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, x847)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Bool:
        var x848 bool = value__254.(_goml_m_std_p_json_p_Value_Bool)._0
        var jp4959 string
        if x848 {
            jp4959 = "true"
        } else {
            jp4959 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp4959)
        return struct{}{}
    case Null:
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, "null")
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_encode(value__264 _goml_m_std_p_json_p_Value) string {
    var builder__265 _goml_m_std_p_text_p_StringBuilder
    var inline10156 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline10157 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline10156,
    }
    builder__265 = inline10157
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline10150 *_goml_vec_uint8 = builder__265.values
    var inline10151 Tuple2_4bool_6string = string_from_utf8(inline10150)
    var inline10153 string = inline10151._1
    return inline10153
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop4970:
        for {
            var t4971 bool = for_index883 < for_limit882
            if t4971 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t4972 int = for_index883 + 1
                for_index883 = t4972
                var t4974 string = for_item884._0
                var t4975 bool
                var inline10159 bool = t4974 == name__267
                t4975 = inline10159
                if t4975 {
                    var t4976 _goml_m_std_p_json_p_Value = for_item884._1
                    var t4977 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t4976,
                    }
                    return t4977
                } else {
                    continue
                }
            } else {
                break Loop_loop4970
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__int {
    var t4987 int
    var inline10178 int = _goml_runtime_core_string_len(value__272)
    t4987 = inline10178
    var t4988 bool
    var inline10175 int = 0
    var inline10176 bool = t4987 == inline10175
    t4988 = inline10176
    if t4988 {
        return Option__int_None{}
    } else {
        var t4989 uint8
        var inline10172 int = 0
        var inline10173 uint8 = _goml_runtime_core_string_byte_get(value__272, inline10172)
        t4989 = inline10173
        var negative__273 bool
        var inline10169 uint8 = 45
        var inline10170 bool = t4989 == inline10169
        negative__273 = inline10170
        var jp4991 int
        if negative__273 {
            jp4991 = 1
        } else {
            jp4991 = 0
        }
        var index__274 int = jp4991
        var result__275 int = 0
        var t5012 int
        var inline10167 int = _goml_runtime_core_string_len(value__272)
        t5012 = inline10167
        var t5013 bool
        var inline10165 bool = index__274 == t5012
        t5013 = inline10165
        if t5013 {
            return Option__int_None{}
        } else {
            Loop_loop4998:
            for {
                var t4999 int
                var inline10163 int = _goml_runtime_core_string_len(value__272)
                t4999 = inline10163
                var t5000 bool = index__274 < t4999
                if t5000 {
                    var byte__276 uint8
                    var inline10161 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline10161
                    var t5010 bool = byte__276 < 48
                    var jp5005 bool
                    if t5010 {
                        jp5005 = true
                    } else {
                        var t5011 bool = byte__276 > 57
                        jp5005 = t5011
                    }
                    if jp5005 {
                        return Option__int_None{}
                    } else {
                        var t5006 int = result__275 * 10
                        var t5007 uint8 = byte__276 - 48
                        var t5008 int = int(uint8(t5007))
                        var t5009 int = t5006 + t5008
                        result__275 = t5009
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t5002 int = compound_old895 + compound_value896
                        index__274 = t5002
                        continue
                    }
                } else {
                    break Loop_loop4998
                }
            }
            var jp4995 int
            if negative__273 {
                var t4997 int = 0 - result__275
                jp4995 = t4997
            } else {
                jp4995 = result__275
            }
            var t4996 Option__int = Option__int_Some{
                _0: jp4995,
            }
            return t4996
        }
    }
}

func main0() struct{} {
    var mtmp172 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp5808 _goml_m_std_p_json_p_Value
    switch mtmp172.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x173 _goml_m_std_p_json_p_Value = mtmp172.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp5808 = x173
        var mtmp176 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5808, "name")
        switch mtmp176.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline10697 string = "missing name"
            var inline10698 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10697)
            _goml_runtime_core_string_println(inline10698)
            var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5808, "version")
            switch mtmp181.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10712 string = "missing version"
                var inline10713 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10712)
                _goml_runtime_core_string_println(inline10713)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp183 Option__int
                switch x182.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline10723 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline10725 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10723)
                    mtmp183 = inline10725
                default:
                    mtmp183 = Option__int_None{}
                }
                switch mtmp183.(type) {
                case Option__int_None:
                    var inline10716 string = "invalid version"
                    var inline10717 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10716)
                    _goml_runtime_core_string_println(inline10717)
                case Option__int_Some:
                    var x184 int = mtmp183.(Option__int_Some)._0
                    var inline10720 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                    _goml_runtime_core_string_println(inline10720)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5808, "stable")
            switch mtmp186.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10727 string = "missing stable"
                var inline10728 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10727)
                _goml_runtime_core_string_println(inline10728)
                var t5812 string = _goml_m_std_p_json_p_encode(jp5808)
                println__T_string(t5812)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field11039 bool
                switch x187.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline10738 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field11039 = inline10738
                    var inline10735 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11039)
                    _goml_runtime_core_string_println(inline10735)
                    var t5812 string = _goml_m_std_p_json_p_encode(jp5808)
                    println__T_string(t5812)
                    return struct{}{}
                default:
                    var inline10731 string = "invalid stable"
                    var inline10732 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10731)
                    _goml_runtime_core_string_println(inline10732)
                    var t5812 string = _goml_m_std_p_json_p_encode(jp5808)
                    println__T_string(t5812)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x177 _goml_m_std_p_json_p_Value = mtmp176.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field11045 string
            switch x177.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline10708 string = x177.(_goml_m_std_p_json_p_Value_String)._0
                commute_field11045 = inline10708
                var inline10705 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field11045)
                _goml_runtime_core_string_println(inline10705)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5808, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10712 string = "missing version"
                    var inline10713 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10712)
                    _goml_runtime_core_string_println(inline10713)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10723 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10725 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10723)
                        mtmp183 = inline10725
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline10716 string = "invalid version"
                        var inline10717 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10716)
                        _goml_runtime_core_string_println(inline10717)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline10720 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline10720)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5808, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10727 string = "missing stable"
                    var inline10728 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10727)
                    _goml_runtime_core_string_println(inline10728)
                    var t5812 string = _goml_m_std_p_json_p_encode(jp5808)
                    println__T_string(t5812)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11039 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10738 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11039 = inline10738
                        var inline10735 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11039)
                        _goml_runtime_core_string_println(inline10735)
                        var t5812 string = _goml_m_std_p_json_p_encode(jp5808)
                        println__T_string(t5812)
                        return struct{}{}
                    default:
                        var inline10731 string = "invalid stable"
                        var inline10732 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10731)
                        _goml_runtime_core_string_println(inline10732)
                        var t5812 string = _goml_m_std_p_json_p_encode(jp5808)
                        println__T_string(t5812)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline10701 string = "invalid name"
                var inline10702 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10701)
                _goml_runtime_core_string_println(inline10702)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5808, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10712 string = "missing version"
                    var inline10713 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10712)
                    _goml_runtime_core_string_println(inline10713)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10723 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10725 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10723)
                        mtmp183 = inline10725
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline10716 string = "invalid version"
                        var inline10717 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10716)
                        _goml_runtime_core_string_println(inline10717)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline10720 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline10720)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5808, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10727 string = "missing stable"
                    var inline10728 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10727)
                    _goml_runtime_core_string_println(inline10728)
                    var t5812 string = _goml_m_std_p_json_p_encode(jp5808)
                    println__T_string(t5812)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11039 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10738 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11039 = inline10738
                        var inline10735 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11039)
                        _goml_runtime_core_string_println(inline10735)
                        var t5812 string = _goml_m_std_p_json_p_encode(jp5808)
                        println__T_string(t5812)
                        return struct{}{}
                    default:
                        var inline10731 string = "invalid stable"
                        var inline10732 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10731)
                        _goml_runtime_core_string_println(inline10732)
                        var t5812 string = _goml_m_std_p_json_p_encode(jp5808)
                        println__T_string(t5812)
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
        var inline10694 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x174)
        _goml_runtime_core_string_println(inline10694)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t5843 string = _goml_runtime_core_int_to_string(self__34)
    return t5843
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline10742 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline10743 bool = inline10742._0
    var inline10744 rune = inline10742._1
    if inline10743 {
        return inline10744
    } else {
        var inline10748 rune = _goml_runtime_core_string_get("", -1)
        return inline10748
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t5984 *ref_int_x = ref__Ref_3int(value__257)
    return t5984
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t5987 int = ref_get__Ref_3int(self__258)
    return t5987
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__259 *ref_int_x, value__260 int) struct{} {
    ref_set__Ref_3int(self__259, value__260)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(self__113 uint8, other__114 uint8) bool {
    var t6049 bool = self__113 == other__114
    return t6049
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t6052 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t6052
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop6057:
    for {
        var t6058 int
        var inline10772 int = _goml_runtime_core_string_len(x12)
        t6058 = inline10772
        var t6059 bool = index__26 < t6058
        if t6059 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t6061 int = compound_old17 + x16
                index__26 = t6061
                continue
            } else {
                var t6063 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6063
            }
        } else {
            break Loop_loop6057
        }
    }
    var t6056 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t6056
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline10774 uint32 = uint32(rune(self__36))
    var inline10775 bool = utf8_valid_scalar(inline10774)
    if inline10775 {
        var inline10776 string = _goml_runtime_core_char_to_string(self__36)
        return inline10776
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t6099 int = _goml_runtime_core_string_len(self__38)
    return t6099
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t6102 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t6102
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline10792 bool = string_is_char_boundary(self__43, start__44)
    var inline10794 bool
    if inline10792 {
        var inline10797 bool = string_is_char_boundary(self__43, end__45)
        inline10794 = inline10797
    } else {
        inline10794 = false
    }
    if inline10794 {
        var inline10795 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline10795
    } else {
        var inline10796 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline10796
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t6188 bool
    var inline10851 bool = value__32 <= 1114111
    if inline10851 {
        var inline10852 bool = value__32 >= 55296
        var inline10854 bool
        if inline10852 {
            var inline10856 bool = value__32 <= 57343
            inline10854 = inline10856
        } else {
            inline10854 = false
        }
        var inline10855 bool = !inline10854
        t6188 = inline10855
    } else {
        t6188 = false
    }
    if t6188 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t6189 Option__char = Option__char_Some{
            _0: x24,
        }
        return t6189
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t6192 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t6192
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__176 *_goml_vec__goml_m_std_p_json_p_Value, elem__177 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__176, elem__177)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t6197 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t6197
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__176 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__177 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__176, elem__177)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t6236 string
    t6236 = value__31
    _goml_runtime_core_string_println(t6236)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t6371 bool = index__6 < 0
    var jp6369 bool
    if t6371 {
        jp6369 = true
    } else {
        var t6372 bool = index__6 >= length__7
        jp6369 = t6372
    }
    if jp6369 {
        var inline10873 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline10873
    } else {
        var t6256 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6256))
        var t6259 bool = first__8 < 128
        if t6259 {
            var inline10875 int = 1
            var inline10876 Option__char = char_from_uint32(first__8)
            switch inline10876.(type) {
            case Option__char_None:
                var inline10877 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline10877
            case Option__char_Some:
                var inline10878 rune = inline10876.(Option__char_Some)._0
                var inline10880 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline10878,
                    _2: inline10875,
                }
                return inline10880
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6263 bool = first__8 < 194
            if t6263 {
                var inline10882 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline10882
            } else {
                var t6267 bool = first__8 < 224
                if t6267 {
                    var t6280 int = length__7 - index__6
                    var t6281 bool = t6280 < 2
                    if t6281 {
                        var inline10884 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline10884
                    } else {
                        var t6269 int = index__6 + 1
                        var t6270 uint8
                        var inline10898 uint8 = _goml_runtime_core_string_byte_get(value__5, t6269)
                        t6270 = inline10898
                        var second__9 uint32 = uint32(uint8(t6270))
                        var t6273 bool
                        var inline10895 bool = second__9 < 128
                        if inline10895 {
                            t6273 = true
                        } else {
                            var inline10896 bool = second__9 > 191
                            t6273 = inline10896
                        }
                        if t6273 {
                            var inline10886 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline10886
                        } else {
                            var t6275_rhs uint32 = 31
                            var t6275 uint32 = first__8 & t6275_rhs
                            var t6276_rhs int = 6
                            var t6276 uint32 = t6275 << t6276_rhs
                            var t6277_rhs uint32 = 63
                            var t6277 uint32 = second__9 & t6277_rhs
                            var t6278 uint32 = t6276 | t6277
                            var inline10888 int = 2
                            var inline10889 Option__char = char_from_uint32(t6278)
                            switch inline10889.(type) {
                            case Option__char_None:
                                var inline10890 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline10890
                            case Option__char_Some:
                                var inline10891 rune = inline10889.(Option__char_Some)._0
                                var inline10893 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline10891,
                                    _2: inline10888,
                                }
                                return inline10893
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6285 bool = first__8 < 240
                    if t6285 {
                        var t6318 int = length__7 - index__6
                        var t6319 bool = t6318 < 3
                        if t6319 {
                            var inline10900 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline10900
                        } else {
                            var t6287 int = index__6 + 1
                            var t6288 uint8
                            var inline10915 uint8 = _goml_runtime_core_string_byte_get(value__5, t6287)
                            t6288 = inline10915
                            var second__10 uint32 = uint32(uint8(t6288))
                            var t6289 int = index__6 + 2
                            var t6290 uint8
                            var inline10913 uint8 = _goml_runtime_core_string_byte_get(value__5, t6289)
                            t6290 = inline10913
                            var third__11 uint32 = uint32(uint8(t6290))
                            var t6316 bool = utf8_invalid_continuation(second__10)
                            var jp6311 bool
                            if t6316 {
                                jp6311 = true
                            } else {
                                var inline10902 bool = third__11 < 128
                                if inline10902 {
                                    jp6311 = true
                                } else {
                                    var inline10903 bool = third__11 > 191
                                    jp6311 = inline10903
                                }
                            }
                            var jp6305 bool
                            if jp6311 {
                                jp6305 = true
                            } else {
                                var t6314 bool
                                var inline10905 uint32 = 224
                                var inline10906 bool = first__8 == inline10905
                                t6314 = inline10906
                                if t6314 {
                                    var t6315 bool = second__10 < 160
                                    jp6305 = t6315
                                } else {
                                    jp6305 = false
                                }
                            }
                            var jp6294 bool
                            if jp6305 {
                                jp6294 = true
                            } else {
                                var t6308 bool
                                var inline10908 uint32 = 237
                                var inline10909 bool = first__8 == inline10908
                                t6308 = inline10909
                                if t6308 {
                                    var t6309 bool = second__10 >= 160
                                    jp6294 = t6309
                                } else {
                                    jp6294 = false
                                }
                            }
                            if jp6294 {
                                var inline10911 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline10911
                            } else {
                                var t6296_rhs uint32 = 15
                                var t6296 uint32 = first__8 & t6296_rhs
                                var t6297_rhs int = 12
                                var t6297 uint32 = t6296 << t6297_rhs
                                var t6298_rhs uint32 = 63
                                var t6298 uint32 = second__10 & t6298_rhs
                                var t6299_rhs int = 6
                                var t6299 uint32 = t6298 << t6299_rhs
                                var t6300 uint32 = t6297 | t6299
                                var t6301_rhs uint32 = 63
                                var t6301 uint32 = third__11 & t6301_rhs
                                var t6302 uint32 = t6300 | t6301
                                var t6303 Tuple3_4bool_4char_3int = utf8_valid_decode(t6302, 3)
                                return t6303
                            }
                        }
                    } else {
                        var t6323 bool = first__8 < 245
                        if t6323 {
                            var t6364 int = length__7 - index__6
                            var t6365 bool = t6364 < 4
                            if t6365 {
                                var t6366 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t6366
                            } else {
                                var t6325 int = index__6 + 1
                                var t6326 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6325)
                                var second__12 uint32 = uint32(uint8(t6326))
                                var t6327 int = index__6 + 2
                                var t6328 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6327)
                                var third__13 uint32 = uint32(uint8(t6328))
                                var t6329 int = index__6 + 3
                                var t6330 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6329)
                                var fourth__14 uint32 = uint32(uint8(t6330))
                                var t6362 bool = utf8_invalid_continuation(second__12)
                                var jp6360 bool
                                if t6362 {
                                    jp6360 = true
                                } else {
                                    var t6363 bool = utf8_invalid_continuation(third__13)
                                    jp6360 = t6363
                                }
                                var jp6354 bool
                                if jp6360 {
                                    jp6354 = true
                                } else {
                                    var t6361 bool = utf8_invalid_continuation(fourth__14)
                                    jp6354 = t6361
                                }
                                var jp6348 bool
                                if jp6354 {
                                    jp6348 = true
                                } else {
                                    var t6357 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t6357 {
                                        var t6358 bool = second__12 < 144
                                        jp6348 = t6358
                                    } else {
                                        jp6348 = false
                                    }
                                }
                                var jp6334 bool
                                if jp6348 {
                                    jp6334 = true
                                } else {
                                    var t6351 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t6351 {
                                        var t6352 bool = second__12 > 143
                                        jp6334 = t6352
                                    } else {
                                        jp6334 = false
                                    }
                                }
                                if jp6334 {
                                    var t6335 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6335
                                } else {
                                    var t6336_rhs uint32 = 7
                                    var t6336 uint32 = first__8 & t6336_rhs
                                    var t6337_rhs int = 18
                                    var t6337 uint32 = t6336 << t6337_rhs
                                    var t6338_rhs uint32 = 63
                                    var t6338 uint32 = second__12 & t6338_rhs
                                    var t6339_rhs int = 12
                                    var t6339 uint32 = t6338 << t6339_rhs
                                    var t6340 uint32 = t6337 | t6339
                                    var t6341_rhs uint32 = 63
                                    var t6341 uint32 = third__13 & t6341_rhs
                                    var t6342_rhs int = 6
                                    var t6342 uint32 = t6341 << t6342_rhs
                                    var t6343 uint32 = t6340 | t6342
                                    var t6344_rhs uint32 = 63
                                    var t6344 uint32 = fourth__14 & t6344_rhs
                                    var t6345 uint32 = t6343 | t6344
                                    var t6346 Tuple3_4bool_4char_3int = utf8_valid_decode(t6345, 4)
                                    return t6346
                                }
                            }
                        } else {
                            var t6367 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t6367
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t6377 uint32 = uint32(rune(value__29))
    var t6378 bool
    var inline10917 bool = t6377 <= 1114111
    if inline10917 {
        var inline10918 bool = t6377 >= 55296
        var inline10920 bool
        if inline10918 {
            var inline10922 bool = t6377 <= 57343
            inline10920 = inline10922
        } else {
            inline10920 = false
        }
        var inline10921 bool = !inline10920
        t6378 = inline10921
    } else {
        t6378 = false
    }
    if t6378 {
        var t6379 string = _goml_runtime_core_char_to_string(value__29)
        return t6379
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t6394 bool = index__16 < 0
    var jp6385 bool
    if t6394 {
        jp6385 = true
    } else {
        var t6395 int
        var inline10924 int = _goml_runtime_core_string_len(value__15)
        t6395 = inline10924
        var t6396 bool = index__16 > t6395
        jp6385 = t6396
    }
    if jp6385 {
        return false
    } else {
        var t6388 int
        var inline10933 int = _goml_runtime_core_string_len(value__15)
        t6388 = inline10933
        var t6389 bool
        var inline10931 bool = index__16 == t6388
        t6389 = inline10931
        if t6389 {
            return true
        } else {
            var t6390 uint8
            var inline10929 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t6390 = inline10929
            var t6391_rhs uint8 = 192
            var t6391 uint8 = t6390 & t6391_rhs
            var t6392 bool
            var inline10926 uint8 = 128
            var inline10927 bool = t6391 == inline10926
            t6392 = inline10927
            var t6393 bool = !t6392
            return t6393
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t6405 bool = string_is_char_boundary(value__21, start__22)
    var jp6402 bool
    if t6405 {
        var t6406 bool = string_is_char_boundary(value__21, end__23)
        jp6402 = t6406
    } else {
        jp6402 = false
    }
    if jp6402 {
        var t6403 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t6403
    } else {
        var t6404 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t6404
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t6425 bool = value__4 <= 1114111
    if t6425 {
        var t6429 bool = value__4 >= 55296
        var jp6427 bool
        if t6429 {
            var t6430 bool = value__4 <= 57343
            jp6427 = t6430
        } else {
            jp6427 = false
        }
        var t6428 bool = !jp6427
        return t6428
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t6437 string = _goml_runtime_core_int_to_string(self__69)
    return t6437
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t6440 string = _goml_runtime_core_bool_to_string(self__66)
    return t6440
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t6443 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t6443
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field11051 rune
    var inline10937 bool = utf8_valid_scalar(value__0)
    if inline10937 {
        var inline10938 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline10940 rune = inline10938._1
        commute_field11051 = inline10940
        var t6449 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field11051,
            _2: width__1,
        }
        return t6449
    } else {
        var inline10935 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline10935
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t6454 bool = value__3 < 128
    if t6454 {
        return true
    } else {
        var t6455 bool = value__3 > 191
        return t6455
    }
}

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t6458 bool = self__117 == other__118
    return t6458
}

func main() {
    main0()
}
