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
    var inline7629 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline7629
    var t2068 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t2068
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline7644 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline7644
    var t2082 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t2082, length__5)
    var for_index1 int = 0
    Loop_loop2084:
    for {
        var t2085 bool = for_index1 < length__5
        if t2085 {
            var for_item3 int = for_index1
            var t2086 int = for_index1 + 1
            for_index1 = t2086
            var t2087 *_goml_vec_uint8 = self__3.values
            var t2088 uint8
            var inline7640 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2088 = inline7640
            vec_push__Vec_5uint8(t2087, t2088)
            continue
        } else {
            break Loop_loop2084
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t2091 string
    var inline7646 string = char_to_string(value__8)
    t2091 = inline7646
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2091)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t3988 string = "" + message__201
    var t3989 string = t3988 + " at byte "
    var t3990 *ref_int_x = value__200.index
    var t3991 int
    var inline9321 int = ref_get__Ref_3int(t3990)
    t3991 = inline9321
    var t3992 string
    var inline9319 string = _goml_runtime_core_int_to_string(t3991)
    t3992 = inline9319
    var t3993 string = t3989 + t3992
    return t3993
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4008:
    for {
        var t4016 *ref_int_x = value__203.index
        var t4017 int
        var inline9354 int = ref_get__Ref_3int(t4016)
        t4017 = inline9354
        var t4018 string = value__203.input
        var t4019 int
        var inline9352 int = _goml_runtime_core_string_len(t4018)
        t4019 = inline9352
        var t4020 bool = t4017 < t4019
        var jp4010 bool
        if t4020 {
            var t4021 string = value__203.input
            var t4022 *ref_int_x = value__203.index
            var t4023 int
            var inline9346 int = ref_get__Ref_3int(t4022)
            t4023 = inline9346
            var t4024 uint8
            var inline9344 uint8 = _goml_runtime_core_string_byte_get(t4021, t4023)
            t4024 = inline9344
            var inline9335 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4024, 9)
            var inline9337 bool
            if inline9335 {
                inline9337 = true
            } else {
                var inline9342 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4024, 10)
                inline9337 = inline9342
            }
            var inline9339 bool
            if inline9337 {
                inline9339 = true
            } else {
                var inline9341 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4024, 13)
                inline9339 = inline9341
            }
            if inline9339 {
                jp4010 = true
            } else {
                var inline9340 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4024, 32)
                jp4010 = inline9340
            }
        } else {
            jp4010 = false
        }
        if jp4010 {
            var t4011 *ref_int_x = value__203.index
            var t4012 *ref_int_x = value__203.index
            var t4013 int
            var inline9350 int = ref_get__Ref_3int(t4012)
            t4013 = inline9350
            var t4014 int = t4013 + 1
            ref_set__Ref_3int(t4011, t4014)
            continue
        } else {
            break Loop_loop4008
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__uint32 {
    var t4055 bool = value__204 >= 48
    var jp4031 bool
    if t4055 {
        var t4056 bool = value__204 <= 57
        jp4031 = t4056
    } else {
        jp4031 = false
    }
    if jp4031 {
        var t4032 uint8 = value__204 - 48
        var t4033 uint32 = uint32(uint8(t4032))
        var t4034 Option__uint32 = Option__uint32_Some{
            _0: t4033,
        }
        return t4034
    } else {
        var t4053 bool = value__204 >= 65
        var jp4038 bool
        if t4053 {
            var t4054 bool = value__204 <= 70
            jp4038 = t4054
        } else {
            jp4038 = false
        }
        if jp4038 {
            var t4039 uint8 = value__204 - 65
            var t4040 uint8 = t4039 + 10
            var t4041 uint32 = uint32(uint8(t4040))
            var t4042 Option__uint32 = Option__uint32_Some{
                _0: t4041,
            }
            return t4042
        } else {
            var t4051 bool = value__204 >= 97
            var jp4046 bool
            if t4051 {
                var t4052 bool = value__204 <= 102
                jp4046 = t4052
            } else {
                jp4046 = false
            }
            if jp4046 {
                var t4047 uint8 = value__204 - 97
                var t4048 uint8 = t4047 + 10
                var t4049 uint32 = uint32(uint8(t4048))
                var t4050 Option__uint32 = Option__uint32_Some{
                    _0: t4049,
                }
                return t4050
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t4061 *ref_int_x = value__205.index
    var t4062 int
    var inline9382 int = ref_get__Ref_3int(t4061)
    t4062 = inline9382
    var t4063 int = t4062 + 4
    var t4064 string = value__205.input
    var t4065 int
    var inline9380 int = _goml_runtime_core_string_len(t4064)
    t4065 = inline9380
    var t4066 bool = t4063 > t4065
    if t4066 {
        var t4067 string
        var inline9356 string = "incomplete unicode escape"
        var inline9357 string = "" + inline9356
        var inline9358 string = inline9357 + " at byte "
        var inline9359 *ref_int_x = value__205.index
        var inline9360 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9359)
        var inline9361 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9360)
        var inline9362 string = inline9358 + inline9361
        t4067 = inline9362
        var t4068 Result__uint32__string = Result__uint32__string_Err{
            _0: t4067,
        }
        return t4068
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop4075:
        for {
            var t4076 bool = for_index744 < for_limit745
            if t4076 {
                var for_item746 int = for_index744
                var t4077 int = for_index744 + 1
                for_index744 = t4077
                var t4078 string = value__205.input
                var t4079 *ref_int_x = value__205.index
                var t4080 int
                var inline9374 int = ref_get__Ref_3int(t4079)
                t4080 = inline9374
                var t4081 int = t4080 + for_item746
                var t4082 uint8
                var inline9372 uint8 = _goml_runtime_core_string_byte_get(t4078, t4081)
                t4082 = inline9372
                var mtmp748 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t4082)
                switch mtmp748.(type) {
                case Option__uint32_None:
                    var t4084 string
                    var inline9364 string = "invalid unicode escape"
                    var inline9365 string = "" + inline9364
                    var inline9366 string = inline9365 + " at byte "
                    var inline9367 *ref_int_x = value__205.index
                    var inline9368 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9367)
                    var inline9369 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9368)
                    var inline9370 string = inline9366 + inline9369
                    t4084 = inline9370
                    var t4085 Result__uint32__string = Result__uint32__string_Err{
                        _0: t4084,
                    }
                    return t4085
                case Option__uint32_Some:
                    var x749 uint32 = mtmp748.(Option__uint32_Some)._0
                    var t4086 uint32 = result__206 * 16
                    var t4087 uint32 = t4086 + x749
                    result__206 = t4087
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4075
            }
        }
        var t4070 *ref_int_x = value__205.index
        var t4071 *ref_int_x = value__205.index
        var t4072 int
        var inline9378 int = ref_get__Ref_3int(t4071)
        t4072 = inline9378
        var t4073 int = t4072 + 4
        ref_set__Ref_3int(t4070, t4073)
        var t4074 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__206,
        }
        return t4074
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var commute_field10704 rune
    var inline9395 bool = utf8_valid_scalar(codepoint__211)
    if inline9395 {
        var inline9396 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__211)
        var inline9398 rune = inline9396._1
        commute_field10704 = inline9398
        var inline9392 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field10704)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline9392)
        var t4094 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t4094
    } else {
        var t4092 string
        var inline9384 string = "invalid unicode codepoint"
        var inline9385 string = "" + inline9384
        var inline9386 string = inline9385 + " at byte "
        var inline9387 *ref_int_x = value__209.index
        var inline9388 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9387)
        var inline9389 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9388)
        var inline9390 string = inline9386 + inline9389
        t4092 = inline9390
        var t4093 Result__unit__string = Result__unit__string_Err{
            _0: t4092,
        }
        return t4093
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4098 uint32
    switch mtmp756.(type) {
    case Result__uint32__string_Ok:
        var x757 uint32 = mtmp756.(Result__uint32__string_Ok)._0
        jp4098 = x757
        var t4160 bool = jp4098 >= 55296
        var jp4102 bool
        if t4160 {
            var t4161 bool = jp4098 <= 56319
            jp4102 = t4161
        } else {
            jp4102 = false
        }
        if jp4102 {
            var t4139 *ref_int_x = value__213.index
            var t4140 int
            var inline9446 int = ref_get__Ref_3int(t4139)
            t4140 = inline9446
            var t4141 int = t4140 + 2
            var t4142 string = value__213.input
            var t4143 int
            var inline9444 int = _goml_runtime_core_string_len(t4142)
            t4143 = inline9444
            var t4144 bool = t4141 > t4143
            var jp4131 bool
            if t4144 {
                jp4131 = true
            } else {
                var t4145 string = value__213.input
                var t4146 *ref_int_x = value__213.index
                var t4147 int
                var inline9407 int = ref_get__Ref_3int(t4146)
                t4147 = inline9407
                var t4148 uint8
                var inline9405 uint8 = _goml_runtime_core_string_byte_get(t4145, t4147)
                t4148 = inline9405
                var t4149 bool
                var inline9402 uint8 = 92
                var inline9403 bool = t4148 == inline9402
                t4149 = inline9403
                var t4150 bool = !t4149
                jp4131 = t4150
            }
            var jp4106 bool
            if jp4131 {
                jp4106 = true
            } else {
                var t4132 string = value__213.input
                var t4133 *ref_int_x = value__213.index
                var t4134 int
                var inline9414 int = ref_get__Ref_3int(t4133)
                t4134 = inline9414
                var t4135 int = t4134 + 1
                var t4136 uint8
                var inline9412 uint8 = _goml_runtime_core_string_byte_get(t4132, t4135)
                t4136 = inline9412
                var t4137 bool
                var inline9409 uint8 = 117
                var inline9410 bool = t4136 == inline9409
                t4137 = inline9410
                var t4138 bool = !t4137
                jp4106 = t4138
            }
            if jp4106 {
                var t4107 string
                var inline9416 string = "missing low surrogate"
                var inline9417 string = "" + inline9416
                var inline9418 string = inline9417 + " at byte "
                var inline9419 *ref_int_x = value__213.index
                var inline9420 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9419)
                var inline9421 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9420)
                var inline9422 string = inline9418 + inline9421
                t4107 = inline9422
                var t4108 Result__unit__string = Result__unit__string_Err{
                    _0: t4107,
                }
                return t4108
            } else {
                var t4109 *ref_int_x = value__213.index
                var t4110 *ref_int_x = value__213.index
                var t4111 int
                var inline9442 int = ref_get__Ref_3int(t4110)
                t4111 = inline9442
                var t4112 int = t4111 + 2
                ref_set__Ref_3int(t4109, t4112)
                var mtmp760 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4114 uint32
                switch mtmp760.(type) {
                case Result__uint32__string_Ok:
                    var x761 uint32 = mtmp760.(Result__uint32__string_Ok)._0
                    jp4114 = x761
                    var t4127 bool = jp4114 < 56320
                    var jp4118 bool
                    if t4127 {
                        jp4118 = true
                    } else {
                        var t4128 bool = jp4114 > 57343
                        jp4118 = t4128
                    }
                    if jp4118 {
                        var t4119 string
                        var inline9424 string = "invalid low surrogate"
                        var inline9425 string = "" + inline9424
                        var inline9426 string = inline9425 + " at byte "
                        var inline9427 *ref_int_x = value__213.index
                        var inline9428 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9427)
                        var inline9429 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9428)
                        var inline9430 string = inline9426 + inline9429
                        t4119 = inline9430
                        var t4120 Result__unit__string = Result__unit__string_Err{
                            _0: t4119,
                        }
                        return t4120
                    } else {
                        var t4121 uint32 = jp4098 - 55296
                        var t4122 uint32 = t4121 * 1024
                        var t4123 uint32 = 65536 + t4122
                        var t4124 uint32 = t4123 + jp4114
                        var t4125 uint32 = t4124 - 56320
                        var inline9432 Option__char = char_from_uint32(t4125)
                        switch inline9432.(type) {
                        case Option__char_None:
                            var inline9433 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline9434 Result__unit__string = Result__unit__string_Err{
                                _0: inline9433,
                            }
                            return inline9434
                        case Option__char_Some:
                            var inline9435 rune = inline9432.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline9435)
                            var inline9438 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline9438
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x762 string = mtmp760.(Result__uint32__string_Err)._0
                    var t4129 Result__unit__string = Result__unit__string_Err{
                        _0: x762,
                    }
                    return t4129
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t4158 bool = jp4098 >= 56320
            var jp4154 bool
            if t4158 {
                var t4159 bool = jp4098 <= 57343
                jp4154 = t4159
            } else {
                jp4154 = false
            }
            if jp4154 {
                var t4155 string = _goml_m_std_p_json_p_json__error(value__213, "unexpected low surrogate")
                var t4156 Result__unit__string = Result__unit__string_Err{
                    _0: t4155,
                }
                return t4156
            } else {
                var t4157 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4098)
                return t4157
            }
        }
    case Result__uint32__string_Err:
        var x758 string = mtmp756.(Result__uint32__string_Err)._0
        var t4162 Result__unit__string = Result__unit__string_Err{
            _0: x758,
        }
        return t4162
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4278 *ref_int_x = value__217.index
    var t4279 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4278)
    var t4280 string = value__217.input
    var t4281 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4280)
    var t4282 bool = t4279 >= t4281
    var jp4270 bool
    if t4282 {
        jp4270 = true
    } else {
        var t4283 string = value__217.input
        var t4284 *ref_int_x = value__217.index
        var t4285 int
        var inline9453 int = ref_get__Ref_3int(t4284)
        t4285 = inline9453
        var t4286 uint8
        var inline9451 uint8 = _goml_runtime_core_string_byte_get(t4283, t4285)
        t4286 = inline9451
        var t4287 bool
        var inline9448 uint8 = 34
        var inline9449 bool = t4286 == inline9448
        t4287 = inline9449
        var t4288 bool = !t4287
        jp4270 = t4288
    }
    if jp4270 {
        var t4271 string
        var inline9455 string = "expected string"
        var inline9456 string = "" + inline9455
        var inline9457 string = inline9456 + " at byte "
        var inline9458 *ref_int_x = value__217.index
        var inline9459 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9458)
        var inline9460 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9459)
        var inline9461 string = inline9457 + inline9460
        t4271 = inline9461
        var t4272 Result__string__string = Result__string__string_Err{
            _0: t4271,
        }
        return t4272
    } else {
        var t4273 *ref_int_x = value__217.index
        var t4274 *ref_int_x = value__217.index
        var t4275 int
        var inline9465 int = ref_get__Ref_3int(t4274)
        t4275 = inline9465
        var t4276 int = t4275 + 1
        ref_set__Ref_3int(t4273, t4276)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t4166 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4166)
        Loop_loop4170:
        for {
            var t4171 *ref_int_x = value__217.index
            var t4172 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4171)
            var t4173 string = value__217.input
            var t4174 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4173)
            var t4175 bool = t4172 < t4174
            if t4175 {
                var t4176 string = value__217.input
                var t4177 *ref_int_x = value__217.index
                var t4178 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4177)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4176, t4178)
                var t4180 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(byte__220, 34)
                if t4180 {
                    var t4188 *ref_int_x = value__217.index
                    var t4189 int
                    var inline9481 int = ref_get__Ref_3int(t4188)
                    t4189 = inline9481
                    var t4190 bool = segment__219 < t4189
                    if t4190 {
                        var t4191 string = value__217.input
                        var t4192 *ref_int_x = value__217.index
                        var t4193 int
                        var inline9469 int = ref_get__Ref_3int(t4192)
                        t4193 = inline9469
                        var t4194 string
                        var inline9467 string = string_byte_slice(t4191, segment__219, t4193)
                        t4194 = inline9467
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4194)
                    } else {}
                    var t4182 *ref_int_x = value__217.index
                    var t4183 *ref_int_x = value__217.index
                    var t4184 int
                    var inline9479 int = ref_get__Ref_3int(t4183)
                    t4184 = inline9479
                    var t4185 int = t4184 + 1
                    ref_set__Ref_3int(t4182, t4185)
                    var t4186 string
                    var inline9471 *_goml_vec_uint8 = builder__218.values
                    var inline9472 Tuple2_4bool_6string = string_from_utf8(inline9471)
                    var inline9474 string = inline9472._1
                    t4186 = inline9474
                    var t4187 Result__string__string = Result__string__string_Ok{
                        _0: t4186,
                    }
                    return t4187
                } else {
                    var t4197 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(byte__220, 92)
                    if t4197 {
                        var t4252 *ref_int_x = value__217.index
                        var t4253 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4252)
                        var t4254 bool = segment__219 < t4253
                        if t4254 {
                            var t4255 string = value__217.input
                            var t4256 *ref_int_x = value__217.index
                            var t4257 int
                            var inline9485 int = ref_get__Ref_3int(t4256)
                            t4257 = inline9485
                            var t4258 string
                            var inline9483 string = string_byte_slice(t4255, segment__219, t4257)
                            t4258 = inline9483
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4258)
                        } else {}
                        var t4199 *ref_int_x = value__217.index
                        var t4200 *ref_int_x = value__217.index
                        var t4201 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4200)
                        var t4202 int = t4201 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4199, t4202)
                        var t4245 *ref_int_x = value__217.index
                        var t4246 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4245)
                        var t4247 string = value__217.input
                        var t4248 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4247)
                        var t4249 bool = t4246 >= t4248
                        if t4249 {
                            var t4250 string
                            var inline9487 string = "incomplete escape"
                            var inline9488 string = "" + inline9487
                            var inline9489 string = inline9488 + " at byte "
                            var inline9490 *ref_int_x = value__217.index
                            var inline9491 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9490)
                            var inline9492 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9491)
                            var inline9493 string = inline9489 + inline9492
                            t4250 = inline9493
                            var t4251 Result__string__string = Result__string__string_Err{
                                _0: t4250,
                            }
                            return t4251
                        } else {
                            var t4204 string = value__217.input
                            var t4205 *ref_int_x = value__217.index
                            var t4206 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4205)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4204, t4206)
                            var t4207 *ref_int_x = value__217.index
                            var t4208 *ref_int_x = value__217.index
                            var t4209 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4208)
                            var t4210 int = t4209 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4207, t4210)
                            var t4214 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 34)
                            if t4214 {
                                var inline9495 rune = 34
                                var inline9496 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9495)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline9496)
                                var t4212 *ref_int_x = value__217.index
                                var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                segment__219 = t4213
                                continue
                            } else {
                                var t4217 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 92)
                                if t4217 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 92)
                                    var t4212 *ref_int_x = value__217.index
                                    var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                    segment__219 = t4213
                                    continue
                                } else {
                                    var t4220 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 47)
                                    if t4220 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t4212 *ref_int_x = value__217.index
                                        var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                        segment__219 = t4213
                                        continue
                                    } else {
                                        var t4223 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 98)
                                        if t4223 {
                                            var mtmp770 Option__char = char_from_uint32(8)
                                            switch mtmp770.(type) {
                                            case Option__char_None:
                                                var t4212 *ref_int_x = value__217.index
                                                var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                                segment__219 = t4213
                                                continue
                                            case Option__char_Some:
                                                var x771 rune = mtmp770.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t4212 *ref_int_x = value__217.index
                                                var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                                segment__219 = t4213
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t4227 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 102)
                                            if t4227 {
                                                var mtmp772 Option__char = char_from_uint32(12)
                                                switch mtmp772.(type) {
                                                case Option__char_None:
                                                    var t4212 *ref_int_x = value__217.index
                                                    var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                                    segment__219 = t4213
                                                    continue
                                                case Option__char_Some:
                                                    var x773 rune = mtmp772.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t4212 *ref_int_x = value__217.index
                                                    var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                                    segment__219 = t4213
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t4231 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 110)
                                                if t4231 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t4212 *ref_int_x = value__217.index
                                                    var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                                    segment__219 = t4213
                                                    continue
                                                } else {
                                                    var t4234 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 114)
                                                    if t4234 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t4212 *ref_int_x = value__217.index
                                                        var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                                        segment__219 = t4213
                                                        continue
                                                    } else {
                                                        var t4237 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 116)
                                                        if t4237 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t4212 *ref_int_x = value__217.index
                                                            var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                                            segment__219 = t4213
                                                            continue
                                                        } else {
                                                            var t4240 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 117)
                                                            if t4240 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t4212 *ref_int_x = value__217.index
                                                                    var t4213 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4212)
                                                                    segment__219 = t4213
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x776 string = mtmp774.(Result__unit__string_Err)._0
                                                                    var t4242 Result__string__string = Result__string__string_Err{
                                                                        _0: x776,
                                                                    }
                                                                    return t4242
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t4243 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t4244 Result__string__string = Result__string__string_Err{
                                                                    _0: t4243,
                                                                }
                                                                return t4244
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
                        var t4261 bool = byte__220 < 32
                        if t4261 {
                            var t4262 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t4263 Result__string__string = Result__string__string_Err{
                                _0: t4262,
                            }
                            return t4263
                        } else {
                            var t4264 *ref_int_x = value__217.index
                            var t4265 *ref_int_x = value__217.index
                            var t4266 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4265)
                            var t4267 int = t4266 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4264, t4267)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop4170
            }
        }
        var t4168 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t4169 Result__string__string = Result__string__string_Err{
            _0: t4168,
        }
        return t4169
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t4297 *ref_int_x = value__225.index
    var start__226 int
    var inline9516 int = ref_get__Ref_3int(t4297)
    start__226 = inline9516
    Loop_loop4302:
    for {
        var t4310 *ref_int_x = value__225.index
        var t4311 int
        var inline9512 int = ref_get__Ref_3int(t4310)
        t4311 = inline9512
        var t4312 string = value__225.input
        var t4313 int
        var inline9510 int = _goml_runtime_core_string_len(t4312)
        t4313 = inline9510
        var t4314 bool = t4311 < t4313
        var jp4304 bool
        if t4314 {
            var t4315 string = value__225.input
            var t4316 *ref_int_x = value__225.index
            var t4317 int
            var inline9504 int = ref_get__Ref_3int(t4316)
            t4317 = inline9504
            var t4318 uint8
            var inline9502 uint8 = _goml_runtime_core_string_byte_get(t4315, t4317)
            t4318 = inline9502
            var inline9499 bool = t4318 >= 48
            if inline9499 {
                var inline9500 bool = t4318 <= 57
                jp4304 = inline9500
            } else {
                jp4304 = false
            }
        } else {
            jp4304 = false
        }
        if jp4304 {
            var t4305 *ref_int_x = value__225.index
            var t4306 *ref_int_x = value__225.index
            var t4307 int
            var inline9508 int = ref_get__Ref_3int(t4306)
            t4307 = inline9508
            var t4308 int = t4307 + 1
            ref_set__Ref_3int(t4305, t4308)
            continue
        } else {
            break Loop_loop4302
        }
    }
    var t4299 *ref_int_x = value__225.index
    var t4300 int
    var inline9514 int = ref_get__Ref_3int(t4299)
    t4300 = inline9514
    var t4301 bool = t4300 > start__226
    return t4301
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4322 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4322)
    var t4443 string = value__227.input
    var t4444 *ref_int_x = value__227.index
    var t4445 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4444)
    var t4446 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4443, t4445)
    var t4447 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4446, 45)
    if t4447 {
        var t4448 *ref_int_x = value__227.index
        var t4449 *ref_int_x = value__227.index
        var t4450 int
        var inline9520 int = ref_get__Ref_3int(t4449)
        t4450 = inline9520
        var t4451 int = t4450 + 1
        ref_set__Ref_3int(t4448, t4451)
    } else {}
    var t4406 *ref_int_x = value__227.index
    var t4407 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4406)
    var t4408 string = value__227.input
    var t4409 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4408)
    var t4410 bool = t4407 >= t4409
    if t4410 {
        var t4411 string
        var inline9522 string = "incomplete number"
        var inline9523 string = "" + inline9522
        var inline9524 string = inline9523 + " at byte "
        var inline9525 *ref_int_x = value__227.index
        var inline9526 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9525)
        var inline9527 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9526)
        var inline9528 string = inline9524 + inline9527
        t4411 = inline9528
        var t4412 Result__string__string = Result__string__string_Err{
            _0: t4411,
        }
        return t4412
    } else {
        var t4414 string = value__227.input
        var t4415 *ref_int_x = value__227.index
        var t4416 int
        var inline9566 int = ref_get__Ref_3int(t4415)
        t4416 = inline9566
        var t4417 uint8
        var inline9564 uint8 = _goml_runtime_core_string_byte_get(t4414, t4416)
        t4417 = inline9564
        var t4418 bool
        var inline9561 uint8 = 48
        var inline9562 bool = t4417 == inline9561
        t4418 = inline9562
        if t4418 {
            var t4419 *ref_int_x = value__227.index
            var t4420 *ref_int_x = value__227.index
            var t4421 int
            var inline9551 int = ref_get__Ref_3int(t4420)
            t4421 = inline9551
            var t4422 int = t4421 + 1
            ref_set__Ref_3int(t4419, t4422)
            var t4428 *ref_int_x = value__227.index
            var t4429 int
            var inline9547 int = ref_get__Ref_3int(t4428)
            t4429 = inline9547
            var t4430 string = value__227.input
            var t4431 int
            var inline9545 int = _goml_runtime_core_string_len(t4430)
            t4431 = inline9545
            var t4432 bool = t4429 < t4431
            var jp4425 bool
            if t4432 {
                var t4433 string = value__227.input
                var t4434 *ref_int_x = value__227.index
                var t4435 int
                var inline9535 int = ref_get__Ref_3int(t4434)
                t4435 = inline9535
                var t4436 uint8
                var inline9533 uint8 = _goml_runtime_core_string_byte_get(t4433, t4435)
                t4436 = inline9533
                var inline9530 bool = t4436 >= 48
                if inline9530 {
                    var inline9531 bool = t4436 <= 57
                    jp4425 = inline9531
                } else {
                    jp4425 = false
                }
            } else {
                jp4425 = false
            }
            if jp4425 {
                var t4426 string
                var inline9537 string = "invalid leading zero"
                var inline9538 string = "" + inline9537
                var inline9539 string = inline9538 + " at byte "
                var inline9540 *ref_int_x = value__227.index
                var inline9541 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9540)
                var inline9542 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9541)
                var inline9543 string = inline9539 + inline9542
                t4426 = inline9543
                var t4427 Result__string__string = Result__string__string_Err{
                    _0: t4426,
                }
                return t4427
            } else {
                var t4396 *ref_int_x = value__227.index
                var t4397 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4396)
                var t4398 string = value__227.input
                var t4399 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4398)
                var t4400 bool = t4397 < t4399
                var jp4386 bool
                if t4400 {
                    var t4401 string = value__227.input
                    var t4402 *ref_int_x = value__227.index
                    var t4403 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4402)
                    var t4404 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4401, t4403)
                    var t4405 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4404, 46)
                    jp4386 = t4405
                } else {
                    jp4386 = false
                }
                if jp4386 {
                    var t4387 *ref_int_x = value__227.index
                    var t4388 *ref_int_x = value__227.index
                    var t4389 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4388)
                    var t4390 int = t4389 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4387, t4390)
                    var t4392 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t4393 bool = !t4392
                    if t4393 {
                        var t4394 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t4395 Result__string__string = Result__string__string_Err{
                            _0: t4394,
                        }
                        return t4395
                    } else {
                        var t4368 *ref_int_x = value__227.index
                        var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                        var t4370 string = value__227.input
                        var t4371 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4370)
                        var t4372 bool = t4369 < t4371
                        var jp4333 bool
                        if t4372 {
                            var t4375 string = value__227.input
                            var t4376 *ref_int_x = value__227.index
                            var t4377 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4376)
                            var t4378 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4375, t4377)
                            var t4379 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4378, 101)
                            if t4379 {
                                jp4333 = true
                            } else {
                                var t4380 string = value__227.input
                                var t4381 *ref_int_x = value__227.index
                                var t4382 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4381)
                                var t4383 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4380, t4382)
                                var t4384 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4383, 69)
                                jp4333 = t4384
                            }
                        } else {
                            jp4333 = false
                        }
                        if jp4333 {
                            var t4334 *ref_int_x = value__227.index
                            var t4335 *ref_int_x = value__227.index
                            var t4336 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4335)
                            var t4337 int = t4336 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4334, t4337)
                            var t4351 *ref_int_x = value__227.index
                            var t4352 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4351)
                            var t4353 string = value__227.input
                            var t4354 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4353)
                            var t4355 bool = t4352 < t4354
                            var jp4345 bool
                            if t4355 {
                                var t4358 string = value__227.input
                                var t4359 *ref_int_x = value__227.index
                                var t4360 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4359)
                                var t4361 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4358, t4360)
                                var t4362 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4361, 43)
                                if t4362 {
                                    jp4345 = true
                                } else {
                                    var t4363 string = value__227.input
                                    var t4364 *ref_int_x = value__227.index
                                    var t4365 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4364)
                                    var t4366 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4363, t4365)
                                    var t4367 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4366, 45)
                                    jp4345 = t4367
                                }
                            } else {
                                jp4345 = false
                            }
                            if jp4345 {
                                var t4346 *ref_int_x = value__227.index
                                var t4347 *ref_int_x = value__227.index
                                var t4348 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4347)
                                var t4349 int = t4348 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4346, t4349)
                            } else {}
                            var t4340 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4341 bool = !t4340
                            if t4341 {
                                var t4342 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4343 Result__string__string = Result__string__string_Err{
                                    _0: t4342,
                                }
                                return t4343
                            } else {
                                var t4327 string = value__227.input
                                var t4328 *ref_int_x = value__227.index
                                var t4329 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4328)
                                var t4330 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4327, start__228, t4329)
                                var t4331 Result__string__string = Result__string__string_Ok{
                                    _0: t4330,
                                }
                                return t4331
                            }
                        } else {
                            var t4327 string = value__227.input
                            var t4328 *ref_int_x = value__227.index
                            var t4329 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4328)
                            var t4330 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4327, start__228, t4329)
                            var t4331 Result__string__string = Result__string__string_Ok{
                                _0: t4330,
                            }
                            return t4331
                        }
                    }
                } else {
                    var t4368 *ref_int_x = value__227.index
                    var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                    var t4370 string = value__227.input
                    var t4371 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4370)
                    var t4372 bool = t4369 < t4371
                    var jp4333 bool
                    if t4372 {
                        var t4375 string = value__227.input
                        var t4376 *ref_int_x = value__227.index
                        var t4377 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4376)
                        var t4378 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4375, t4377)
                        var t4379 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4378, 101)
                        if t4379 {
                            jp4333 = true
                        } else {
                            var t4380 string = value__227.input
                            var t4381 *ref_int_x = value__227.index
                            var t4382 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4381)
                            var t4383 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4380, t4382)
                            var t4384 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4383, 69)
                            jp4333 = t4384
                        }
                    } else {
                        jp4333 = false
                    }
                    if jp4333 {
                        var t4334 *ref_int_x = value__227.index
                        var t4335 *ref_int_x = value__227.index
                        var t4336 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4335)
                        var t4337 int = t4336 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4334, t4337)
                        var t4351 *ref_int_x = value__227.index
                        var t4352 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4351)
                        var t4353 string = value__227.input
                        var t4354 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4353)
                        var t4355 bool = t4352 < t4354
                        var jp4345 bool
                        if t4355 {
                            var t4358 string = value__227.input
                            var t4359 *ref_int_x = value__227.index
                            var t4360 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4359)
                            var t4361 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4358, t4360)
                            var t4362 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4361, 43)
                            if t4362 {
                                jp4345 = true
                            } else {
                                var t4363 string = value__227.input
                                var t4364 *ref_int_x = value__227.index
                                var t4365 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4364)
                                var t4366 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4363, t4365)
                                var t4367 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4366, 45)
                                jp4345 = t4367
                            }
                        } else {
                            jp4345 = false
                        }
                        if jp4345 {
                            var t4346 *ref_int_x = value__227.index
                            var t4347 *ref_int_x = value__227.index
                            var t4348 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4347)
                            var t4349 int = t4348 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4346, t4349)
                        } else {}
                        var t4340 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4341 bool = !t4340
                        if t4341 {
                            var t4342 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4343 Result__string__string = Result__string__string_Err{
                                _0: t4342,
                            }
                            return t4343
                        } else {
                            var t4327 string = value__227.input
                            var t4328 *ref_int_x = value__227.index
                            var t4329 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4328)
                            var t4330 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4327, start__228, t4329)
                            var t4331 Result__string__string = Result__string__string_Ok{
                                _0: t4330,
                            }
                            return t4331
                        }
                    } else {
                        var t4327 string = value__227.input
                        var t4328 *ref_int_x = value__227.index
                        var t4329 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4328)
                        var t4330 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4327, start__228, t4329)
                        var t4331 Result__string__string = Result__string__string_Ok{
                            _0: t4330,
                        }
                        return t4331
                    }
                }
            }
        } else {
            var t4439 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t4440 bool = !t4439
            if t4440 {
                var t4441 string
                var inline9553 string = "expected number"
                var inline9554 string = "" + inline9553
                var inline9555 string = inline9554 + " at byte "
                var inline9556 *ref_int_x = value__227.index
                var inline9557 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9556)
                var inline9558 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9557)
                var inline9559 string = inline9555 + inline9558
                t4441 = inline9559
                var t4442 Result__string__string = Result__string__string_Err{
                    _0: t4441,
                }
                return t4442
            } else {
                var t4396 *ref_int_x = value__227.index
                var t4397 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4396)
                var t4398 string = value__227.input
                var t4399 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4398)
                var t4400 bool = t4397 < t4399
                var jp4386 bool
                if t4400 {
                    var t4401 string = value__227.input
                    var t4402 *ref_int_x = value__227.index
                    var t4403 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4402)
                    var t4404 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4401, t4403)
                    var t4405 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4404, 46)
                    jp4386 = t4405
                } else {
                    jp4386 = false
                }
                if jp4386 {
                    var t4387 *ref_int_x = value__227.index
                    var t4388 *ref_int_x = value__227.index
                    var t4389 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4388)
                    var t4390 int = t4389 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4387, t4390)
                    var t4392 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t4393 bool = !t4392
                    if t4393 {
                        var t4394 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t4395 Result__string__string = Result__string__string_Err{
                            _0: t4394,
                        }
                        return t4395
                    } else {
                        var t4368 *ref_int_x = value__227.index
                        var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                        var t4370 string = value__227.input
                        var t4371 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4370)
                        var t4372 bool = t4369 < t4371
                        var jp4333 bool
                        if t4372 {
                            var t4375 string = value__227.input
                            var t4376 *ref_int_x = value__227.index
                            var t4377 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4376)
                            var t4378 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4375, t4377)
                            var t4379 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4378, 101)
                            if t4379 {
                                jp4333 = true
                            } else {
                                var t4380 string = value__227.input
                                var t4381 *ref_int_x = value__227.index
                                var t4382 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4381)
                                var t4383 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4380, t4382)
                                var t4384 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4383, 69)
                                jp4333 = t4384
                            }
                        } else {
                            jp4333 = false
                        }
                        if jp4333 {
                            var t4334 *ref_int_x = value__227.index
                            var t4335 *ref_int_x = value__227.index
                            var t4336 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4335)
                            var t4337 int = t4336 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4334, t4337)
                            var t4351 *ref_int_x = value__227.index
                            var t4352 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4351)
                            var t4353 string = value__227.input
                            var t4354 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4353)
                            var t4355 bool = t4352 < t4354
                            var jp4345 bool
                            if t4355 {
                                var t4358 string = value__227.input
                                var t4359 *ref_int_x = value__227.index
                                var t4360 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4359)
                                var t4361 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4358, t4360)
                                var t4362 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4361, 43)
                                if t4362 {
                                    jp4345 = true
                                } else {
                                    var t4363 string = value__227.input
                                    var t4364 *ref_int_x = value__227.index
                                    var t4365 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4364)
                                    var t4366 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4363, t4365)
                                    var t4367 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4366, 45)
                                    jp4345 = t4367
                                }
                            } else {
                                jp4345 = false
                            }
                            if jp4345 {
                                var t4346 *ref_int_x = value__227.index
                                var t4347 *ref_int_x = value__227.index
                                var t4348 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4347)
                                var t4349 int = t4348 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4346, t4349)
                            } else {}
                            var t4340 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4341 bool = !t4340
                            if t4341 {
                                var t4342 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4343 Result__string__string = Result__string__string_Err{
                                    _0: t4342,
                                }
                                return t4343
                            } else {
                                var t4327 string = value__227.input
                                var t4328 *ref_int_x = value__227.index
                                var t4329 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4328)
                                var t4330 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4327, start__228, t4329)
                                var t4331 Result__string__string = Result__string__string_Ok{
                                    _0: t4330,
                                }
                                return t4331
                            }
                        } else {
                            var t4327 string = value__227.input
                            var t4328 *ref_int_x = value__227.index
                            var t4329 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4328)
                            var t4330 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4327, start__228, t4329)
                            var t4331 Result__string__string = Result__string__string_Ok{
                                _0: t4330,
                            }
                            return t4331
                        }
                    }
                } else {
                    var t4368 *ref_int_x = value__227.index
                    var t4369 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4368)
                    var t4370 string = value__227.input
                    var t4371 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4370)
                    var t4372 bool = t4369 < t4371
                    var jp4333 bool
                    if t4372 {
                        var t4375 string = value__227.input
                        var t4376 *ref_int_x = value__227.index
                        var t4377 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4376)
                        var t4378 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4375, t4377)
                        var t4379 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4378, 101)
                        if t4379 {
                            jp4333 = true
                        } else {
                            var t4380 string = value__227.input
                            var t4381 *ref_int_x = value__227.index
                            var t4382 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4381)
                            var t4383 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4380, t4382)
                            var t4384 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4383, 69)
                            jp4333 = t4384
                        }
                    } else {
                        jp4333 = false
                    }
                    if jp4333 {
                        var t4334 *ref_int_x = value__227.index
                        var t4335 *ref_int_x = value__227.index
                        var t4336 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4335)
                        var t4337 int = t4336 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4334, t4337)
                        var t4351 *ref_int_x = value__227.index
                        var t4352 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4351)
                        var t4353 string = value__227.input
                        var t4354 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4353)
                        var t4355 bool = t4352 < t4354
                        var jp4345 bool
                        if t4355 {
                            var t4358 string = value__227.input
                            var t4359 *ref_int_x = value__227.index
                            var t4360 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4359)
                            var t4361 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4358, t4360)
                            var t4362 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4361, 43)
                            if t4362 {
                                jp4345 = true
                            } else {
                                var t4363 string = value__227.input
                                var t4364 *ref_int_x = value__227.index
                                var t4365 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4364)
                                var t4366 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4363, t4365)
                                var t4367 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4366, 45)
                                jp4345 = t4367
                            }
                        } else {
                            jp4345 = false
                        }
                        if jp4345 {
                            var t4346 *ref_int_x = value__227.index
                            var t4347 *ref_int_x = value__227.index
                            var t4348 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4347)
                            var t4349 int = t4348 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4346, t4349)
                        } else {}
                        var t4340 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4341 bool = !t4340
                        if t4341 {
                            var t4342 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4343 Result__string__string = Result__string__string_Err{
                                _0: t4342,
                            }
                            return t4343
                        } else {
                            var t4327 string = value__227.input
                            var t4328 *ref_int_x = value__227.index
                            var t4329 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4328)
                            var t4330 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4327, start__228, t4329)
                            var t4331 Result__string__string = Result__string__string_Ok{
                                _0: t4330,
                            }
                            return t4331
                        }
                    } else {
                        var t4327 string = value__227.input
                        var t4328 *ref_int_x = value__227.index
                        var t4329 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4328)
                        var t4330 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4327, start__228, t4329)
                        var t4331 Result__string__string = Result__string__string_Ok{
                            _0: t4330,
                        }
                        return t4331
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t4474 *ref_int_x = value__230.index
    var t4475 int
    var inline9596 int = ref_get__Ref_3int(t4474)
    t4475 = inline9596
    var t4476 int
    var inline9594 int = _goml_runtime_core_string_len(expected__231)
    t4476 = inline9594
    var t4477 int = t4475 + t4476
    var t4478 string = value__230.input
    var t4479 int
    var inline9592 int = _goml_runtime_core_string_len(t4478)
    t4479 = inline9592
    var t4480 bool = t4477 <= t4479
    var jp4465 bool
    if t4480 {
        var t4481 string = value__230.input
        var t4482 *ref_int_x = value__230.index
        var t4483 int
        var inline9576 int = ref_get__Ref_3int(t4482)
        t4483 = inline9576
        var t4484 *ref_int_x = value__230.index
        var t4485 int
        var inline9574 int = ref_get__Ref_3int(t4484)
        t4485 = inline9574
        var t4486 int
        var inline9572 int = _goml_runtime_core_string_len(expected__231)
        t4486 = inline9572
        var t4487 int = t4485 + t4486
        var t4488 string
        var inline9570 string = string_byte_slice(t4481, t4483, t4487)
        t4488 = inline9570
        var inline9568 bool = t4488 == expected__231
        jp4465 = inline9568
    } else {
        jp4465 = false
    }
    if jp4465 {
        var t4466 *ref_int_x = value__230.index
        var t4467 *ref_int_x = value__230.index
        var t4468 int
        var inline9582 int = ref_get__Ref_3int(t4467)
        t4468 = inline9582
        var t4469 int
        var inline9580 int = _goml_runtime_core_string_len(expected__231)
        t4469 = inline9580
        var t4470 int = t4468 + t4469
        ref_set__Ref_3int(t4466, t4470)
        var t4471 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__232,
        }
        return t4471
    } else {
        var t4472 string
        var inline9584 string = "invalid literal"
        var inline9585 string = "" + inline9584
        var inline9586 string = inline9585 + " at byte "
        var inline9587 *ref_int_x = value__230.index
        var inline9588 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9587)
        var inline9589 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9588)
        var inline9590 string = inline9586 + inline9589
        t4472 = inline9590
        var t4473 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4472,
        }
        return t4473
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t4492 *ref_int_x = value__233.index
    var t4493 *ref_int_x = value__233.index
    var t4494 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4493)
    var t4495 int = t4494 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4492, t4495)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var vec_literal__8825 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t4550 *ref_int_x = value__233.index
    var t4551 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4550)
    var t4552 string = value__233.input
    var t4553 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4552)
    var t4554 bool = t4551 < t4553
    var jp4543 bool
    if t4554 {
        var t4555 string = value__233.input
        var t4556 *ref_int_x = value__233.index
        var t4557 int
        var inline9603 int = ref_get__Ref_3int(t4556)
        t4557 = inline9603
        var t4558 uint8
        var inline9601 uint8 = _goml_runtime_core_string_byte_get(t4555, t4557)
        t4558 = inline9601
        var inline9598 uint8 = 93
        var inline9599 bool = t4558 == inline9598
        jp4543 = inline9599
    } else {
        jp4543 = false
    }
    if jp4543 {
        var t4544 *ref_int_x = value__233.index
        var t4545 *ref_int_x = value__233.index
        var t4546 int
        var inline9607 int = ref_get__Ref_3int(t4545)
        t4546 = inline9607
        var t4547 int = t4546 + 1
        ref_set__Ref_3int(t4544, t4547)
        var t4548 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: vec_literal__8825,
        }
        var t4549 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t4548,
        }
        return t4549
    } else {
        Loop_loop4500:
        for {
            var t4501 *ref_int_x = value__233.index
            var t4502 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4501)
            var t4503 string = value__233.input
            var t4504 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4503)
            var t4505 bool = t4502 < t4504
            if t4505 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp4507 _goml_m_std_p_json_p_Value
                switch mtmp797.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp4507 = x798
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8825, jp4507)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t4509 *ref_int_x = value__233.index
                    var t4510 int
                    var inline9649 int = ref_get__Ref_3int(t4509)
                    t4510 = inline9649
                    var t4511 string = value__233.input
                    var t4512 int
                    var inline9647 int = _goml_runtime_core_string_len(t4511)
                    t4512 = inline9647
                    var t4513 bool = t4510 >= t4512
                    if t4513 {
                        var t4514 string
                        var inline9609 string = "unterminated array"
                        var inline9610 string = "" + inline9609
                        var inline9611 string = inline9610 + " at byte "
                        var inline9612 *ref_int_x = value__233.index
                        var inline9613 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9612)
                        var inline9614 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9613)
                        var inline9615 string = inline9611 + inline9614
                        t4514 = inline9615
                        var t4515 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t4514,
                        }
                        return t4515
                    } else {
                        var t4517 string = value__233.input
                        var t4518 *ref_int_x = value__233.index
                        var t4519 int
                        var inline9645 int = ref_get__Ref_3int(t4518)
                        t4519 = inline9645
                        var t4520 uint8
                        var inline9643 uint8 = _goml_runtime_core_string_byte_get(t4517, t4519)
                        t4520 = inline9643
                        var t4521 bool
                        var inline9640 uint8 = 93
                        var inline9641 bool = t4520 == inline9640
                        t4521 = inline9641
                        if t4521 {
                            var t4522 *ref_int_x = value__233.index
                            var t4523 *ref_int_x = value__233.index
                            var t4524 int
                            var inline9619 int = ref_get__Ref_3int(t4523)
                            t4524 = inline9619
                            var t4525 int = t4524 + 1
                            ref_set__Ref_3int(t4522, t4525)
                            var t4526 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: vec_literal__8825,
                            }
                            var t4527 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t4526,
                            }
                            return t4527
                        } else {
                            var t4529 string = value__233.input
                            var t4530 *ref_int_x = value__233.index
                            var t4531 int
                            var inline9638 int = ref_get__Ref_3int(t4530)
                            t4531 = inline9638
                            var t4532 uint8
                            var inline9636 uint8 = _goml_runtime_core_string_byte_get(t4529, t4531)
                            t4532 = inline9636
                            var t4533 bool
                            var inline9633 uint8 = 44
                            var inline9634 bool = t4532 == inline9633
                            t4533 = inline9634
                            if t4533 {
                                var t4534 *ref_int_x = value__233.index
                                var t4535 *ref_int_x = value__233.index
                                var t4536 int
                                var inline9623 int = ref_get__Ref_3int(t4535)
                                t4536 = inline9623
                                var t4537 int = t4536 + 1
                                ref_set__Ref_3int(t4534, t4537)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t4539 string
                                var inline9625 string = "expected array separator"
                                var inline9626 string = "" + inline9625
                                var inline9627 string = inline9626 + " at byte "
                                var inline9628 *ref_int_x = value__233.index
                                var inline9629 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9628)
                                var inline9630 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9629)
                                var inline9631 string = inline9627 + inline9630
                                t4539 = inline9631
                                var t4540 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t4539,
                                }
                                return t4540
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x799 string = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t4541 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x799,
                    }
                    return t4541
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4500
            }
        }
        var t4498 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t4499 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4498,
        }
        return t4499
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t4562 *ref_int_x = value__236.index
    var t4563 *ref_int_x = value__236.index
    var t4564 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4563)
    var t4565 int = t4564 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4562, t4565)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var vec_literal__10027 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t4645 *ref_int_x = value__236.index
    var t4646 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4645)
    var t4647 string = value__236.input
    var t4648 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4647)
    var t4649 bool = t4646 < t4648
    var jp4638 bool
    if t4649 {
        var t4650 string = value__236.input
        var t4651 *ref_int_x = value__236.index
        var t4652 int
        var inline9656 int = ref_get__Ref_3int(t4651)
        t4652 = inline9656
        var t4653 uint8
        var inline9654 uint8 = _goml_runtime_core_string_byte_get(t4650, t4652)
        t4653 = inline9654
        var inline9651 uint8 = 125
        var inline9652 bool = t4653 == inline9651
        jp4638 = inline9652
    } else {
        jp4638 = false
    }
    if jp4638 {
        var t4639 *ref_int_x = value__236.index
        var t4640 *ref_int_x = value__236.index
        var t4641 int
        var inline9660 int = ref_get__Ref_3int(t4640)
        t4641 = inline9660
        var t4642 int = t4641 + 1
        ref_set__Ref_3int(t4639, t4642)
        var t4643 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10027,
        }
        var t4644 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t4643,
        }
        return t4644
    } else {
        Loop_loop4570:
        for {
            var t4571 *ref_int_x = value__236.index
            var t4572 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4571)
            var t4573 string = value__236.input
            var t4574 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4573)
            var t4575 bool = t4572 < t4574
            if t4575 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp4577 string
                switch mtmp809.(type) {
                case Result__string__string_Ok:
                    var x810 string = mtmp809.(Result__string__string_Ok)._0
                    jp4577 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t4625 *ref_int_x = value__236.index
                    var t4626 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4625)
                    var t4627 string = value__236.input
                    var t4628 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4627)
                    var t4629 bool = t4626 >= t4628
                    var jp4617 bool
                    if t4629 {
                        jp4617 = true
                    } else {
                        var t4630 string = value__236.input
                        var t4631 *ref_int_x = value__236.index
                        var t4632 int
                        var inline9667 int = ref_get__Ref_3int(t4631)
                        t4632 = inline9667
                        var t4633 uint8
                        var inline9665 uint8 = _goml_runtime_core_string_byte_get(t4630, t4632)
                        t4633 = inline9665
                        var t4634 bool
                        var inline9662 uint8 = 58
                        var inline9663 bool = t4633 == inline9662
                        t4634 = inline9663
                        var t4635 bool = !t4634
                        jp4617 = t4635
                    }
                    if jp4617 {
                        var t4618 string
                        var inline9669 string = "expected object colon"
                        var inline9670 string = "" + inline9669
                        var inline9671 string = inline9670 + " at byte "
                        var inline9672 *ref_int_x = value__236.index
                        var inline9673 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9672)
                        var inline9674 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9673)
                        var inline9675 string = inline9671 + inline9674
                        t4618 = inline9675
                        var t4619 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t4618,
                        }
                        return t4619
                    } else {
                        var t4620 *ref_int_x = value__236.index
                        var t4621 *ref_int_x = value__236.index
                        var t4622 int
                        var inline9679 int = ref_get__Ref_3int(t4621)
                        t4622 = inline9679
                        var t4623 int = t4622 + 1
                        ref_set__Ref_3int(t4620, t4623)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp4580 _goml_m_std_p_json_p_Value
                        switch mtmp815.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp4580 = x816
                            var t4581 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp4577,
                                _1: jp4580,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10027, t4581)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t4583 *ref_int_x = value__236.index
                            var t4584 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4583)
                            var t4585 string = value__236.input
                            var t4586 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4585)
                            var t4587 bool = t4584 >= t4586
                            if t4587 {
                                var t4588 string
                                var inline9681 string = "unterminated object"
                                var inline9682 string = "" + inline9681
                                var inline9683 string = inline9682 + " at byte "
                                var inline9684 *ref_int_x = value__236.index
                                var inline9685 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9684)
                                var inline9686 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9685)
                                var inline9687 string = inline9683 + inline9686
                                t4588 = inline9687
                                var t4589 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t4588,
                                }
                                return t4589
                            } else {
                                var t4591 string = value__236.input
                                var t4592 *ref_int_x = value__236.index
                                var t4593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4592)
                                var t4594 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4591, t4593)
                                var t4595 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4594, 125)
                                if t4595 {
                                    var t4596 *ref_int_x = value__236.index
                                    var t4597 *ref_int_x = value__236.index
                                    var t4598 int
                                    var inline9691 int = ref_get__Ref_3int(t4597)
                                    t4598 = inline9691
                                    var t4599 int = t4598 + 1
                                    ref_set__Ref_3int(t4596, t4599)
                                    var t4600 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10027,
                                    }
                                    var t4601 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t4600,
                                    }
                                    return t4601
                                } else {
                                    var t4603 string = value__236.input
                                    var t4604 *ref_int_x = value__236.index
                                    var t4605 int
                                    var inline9702 int = ref_get__Ref_3int(t4604)
                                    t4605 = inline9702
                                    var t4606 uint8
                                    var inline9700 uint8 = _goml_runtime_core_string_byte_get(t4603, t4605)
                                    t4606 = inline9700
                                    var t4607 bool
                                    var inline9697 uint8 = 44
                                    var inline9698 bool = t4606 == inline9697
                                    t4607 = inline9698
                                    if t4607 {
                                        var t4608 *ref_int_x = value__236.index
                                        var t4609 *ref_int_x = value__236.index
                                        var t4610 int
                                        var inline9695 int = ref_get__Ref_3int(t4609)
                                        t4610 = inline9695
                                        var t4611 int = t4610 + 1
                                        ref_set__Ref_3int(t4608, t4611)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t4613 string = _goml_m_std_p_json_p_json__error(value__236, "expected object separator")
                                        var t4614 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t4613,
                                        }
                                        return t4614
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x817 string = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t4615 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x817,
                            }
                            return t4615
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x811 string = mtmp809.(Result__string__string_Err)._0
                    var t4636 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x811,
                    }
                    return t4636
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4570
            }
        }
        var t4568 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t4569 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4568,
        }
        return t4569
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t4659 *ref_int_x = value__240.index
    var t4660 int
    var inline9743 int = ref_get__Ref_3int(t4659)
    t4660 = inline9743
    var t4661 string = value__240.input
    var t4662 int
    var inline9741 int = _goml_runtime_core_string_len(t4661)
    t4662 = inline9741
    var t4663 bool = t4660 >= t4662
    if t4663 {
        var t4664 string
        var inline9704 string = "expected JSON value"
        var inline9705 string = "" + inline9704
        var inline9706 string = inline9705 + " at byte "
        var inline9707 *ref_int_x = value__240.index
        var inline9708 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9707)
        var inline9709 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9708)
        var inline9710 string = inline9706 + inline9709
        t4664 = inline9710
        var t4665 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4664,
        }
        return t4665
    } else {
        var t4666 string = value__240.input
        var t4667 *ref_int_x = value__240.index
        var t4668 int
        var inline9739 int = ref_get__Ref_3int(t4667)
        t4668 = inline9739
        var mtmp824 uint8
        var inline9737 uint8 = _goml_runtime_core_string_byte_get(t4666, t4668)
        mtmp824 = inline9737
        switch mtmp824 {
        case 123:
            var t4671 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t4671
        case 91:
            var t4672 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t4672
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825.(type) {
            case Result__string__string_Ok:
                var x826 string = mtmp825.(Result__string__string_Ok)._0
                var t4675 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t4676 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t4675,
                }
                return t4676
            case Result__string__string_Err:
                var x827 string = mtmp825.(Result__string__string_Err)._0
                var t4677 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x827,
                }
                return t4677
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t4678 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t4679 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t4678)
            return t4679
        case 102:
            var t4680 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t4681 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t4680)
            return t4681
        case 110:
            var t4682 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t4682
        default:
            var t4690 bool
            var inline9734 uint8 = 45
            var inline9735 bool = mtmp824 == inline9734
            t4690 = inline9735
            var jp4686 bool
            if t4690 {
                jp4686 = true
            } else {
                var inline9712 bool = mtmp824 >= 48
                if inline9712 {
                    var inline9713 bool = mtmp824 <= 57
                    jp4686 = inline9713
                } else {
                    jp4686 = false
                }
            }
            if jp4686 {
                var inline9715 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline9717 string
                switch inline9715.(type) {
                case Result__string__string_Ok:
                    var inline9720 string = inline9715.(Result__string__string_Ok)._0
                    inline9717 = inline9720
                    var inline9718 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline9717,
                    }
                    var inline9719 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                        _0: inline9718,
                    }
                    return inline9719
                case Result__string__string_Err:
                    var inline9722 string = inline9715.(Result__string__string_Err)._0
                    var inline9724 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: inline9722,
                    }
                    return inline9724
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t4688 string
                var inline9726 string = "unexpected JSON token"
                var inline9727 string = "" + inline9726
                var inline9728 string = inline9727 + " at byte "
                var inline9729 *ref_int_x = value__240.index
                var inline9730 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9729)
                var inline9731 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9730)
                var inline9732 string = inline9728 + inline9731
                t4688 = inline9732
                var t4689 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t4688,
                }
                return t4689
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline9759 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline9760 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline9759,
    }
    parser__245 = inline9760
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp4695 _goml_m_std_p_json_p_Value
    switch mtmp828.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x829 _goml_m_std_p_json_p_Value = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp4695 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t4698 *ref_int_x = parser__245.index
        var t4699 int
        var inline9757 int = ref_get__Ref_3int(t4698)
        t4699 = inline9757
        var t4700 int
        var inline9755 int = _goml_runtime_core_string_len(input__244)
        t4700 = inline9755
        var t4701 bool
        var inline9753 bool = t4699 == t4700
        t4701 = inline9753
        if t4701 {
            var t4702 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp4695,
            }
            return t4702
        } else {
            var t4703 string
            var inline9745 string = "trailing JSON data"
            var inline9746 string = "" + inline9745
            var inline9747 string = inline9746 + " at byte "
            var inline9748 *ref_int_x = parser__245.index
            var inline9749 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9748)
            var inline9750 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9749)
            var inline9751 string = inline9747 + inline9750
            t4703 = inline9751
            var t4704 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t4703,
            }
            return t4704
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x830 string = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t4705 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x830,
        }
        return t4705
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__248, 34)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__249)
    Loop_loop4719:
    for {
        var t4720 bool = for_index833 < for_limit834
        if t4720 {
            var for_item835 int = for_index833
            var t4721 int = for_index833 + 1
            for_index833 = t4721
            var byte__252 uint8
            var inline9821 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline9821
            var t4774 bool
            var inline9818 uint8 = 34
            var inline9819 bool = byte__252 == inline9818
            t4774 = inline9819
            var jp4772 bool
            if t4774 {
                jp4772 = true
            } else {
                var inline9765 uint8 = 92
                var inline9766 bool = byte__252 == inline9765
                jp4772 = inline9766
            }
            var jp4769 bool
            if jp4772 {
                jp4769 = true
            } else {
                var inline9768 uint8 = 8
                var inline9769 bool = byte__252 == inline9768
                jp4769 = inline9769
            }
            var jp4766 bool
            if jp4769 {
                jp4766 = true
            } else {
                var inline9771 uint8 = 9
                var inline9772 bool = byte__252 == inline9771
                jp4766 = inline9772
            }
            var jp4763 bool
            if jp4766 {
                jp4763 = true
            } else {
                var inline9774 uint8 = 10
                var inline9775 bool = byte__252 == inline9774
                jp4763 = inline9775
            }
            var jp4760 bool
            if jp4763 {
                jp4760 = true
            } else {
                var inline9777 uint8 = 12
                var inline9778 bool = byte__252 == inline9777
                jp4760 = inline9778
            }
            var jp4757 bool
            if jp4760 {
                jp4757 = true
            } else {
                var inline9780 uint8 = 13
                var inline9781 bool = byte__252 == inline9780
                jp4757 = inline9781
            }
            var jp4724 bool
            if jp4757 {
                jp4724 = true
            } else {
                var t4758 bool = byte__252 < 32
                jp4724 = t4758
            }
            if jp4724 {
                var t4753 bool = start__250 < for_item835
                if t4753 {
                    var t4754 string
                    var inline9783 string = string_byte_slice(value__249, start__250, for_item835)
                    t4754 = inline9783
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t4754)
                } else {}
                var t4728 bool
                var inline9815 uint8 = 34
                var inline9816 bool = byte__252 == inline9815
                t4728 = inline9816
                if t4728 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t4731 bool
                    var inline9812 uint8 = 92
                    var inline9813 bool = byte__252 == inline9812
                    t4731 = inline9813
                    if t4731 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t4734 bool
                        var inline9809 uint8 = 8
                        var inline9810 bool = byte__252 == inline9809
                        t4734 = inline9810
                        if t4734 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t4737 bool
                            var inline9806 uint8 = 9
                            var inline9807 bool = byte__252 == inline9806
                            t4737 = inline9807
                            if t4737 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t4740 bool
                                var inline9803 uint8 = 10
                                var inline9804 bool = byte__252 == inline9803
                                t4740 = inline9804
                                if t4740 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t4743 bool
                                    var inline9800 uint8 = 12
                                    var inline9801 bool = byte__252 == inline9800
                                    t4743 = inline9801
                                    if t4743 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t4746 bool
                                        var inline9797 uint8 = 13
                                        var inline9798 bool = byte__252 == inline9797
                                        t4746 = inline9798
                                        if t4746 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t4748 uint8 = byte__252 / 16
                                            var t4749 rune
                                            var inline9794 int = int(uint8(t4748))
                                            var inline9795 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline9794)
                                            t4749 = inline9795
                                            var inline9791 string = _goml_m_inherent_i_char_i_char_i_to__string(t4749)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline9791)
                                            var t4750_rhs uint8 = 16
                                            var t4750 uint8 = byte__252 % t4750_rhs
                                            var t4751 rune
                                            var inline9788 int = int(uint8(t4750))
                                            var inline9789 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline9788)
                                            t4751 = inline9789
                                            var inline9785 string = _goml_m_inherent_i_char_i_char_i_to__string(t4751)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline9785)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t4727 int = for_item835 + 1
                start__250 = t4727
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop4719
        }
    }
    var t4714 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__249)
    var t4715 bool = start__250 < t4714
    if t4715 {
        var t4716 int
        var inline9825 int = _goml_runtime_core_string_len(value__249)
        t4716 = inline9825
        var t4717 string
        var inline9823 string = string_byte_slice(value__249, start__250, t4716)
        t4717 = inline9823
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t4717)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__248, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline9839 rune = 123
        var inline9840 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9839)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline9840)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop4780:
        for {
            var t4781 bool = for_index852 < for_limit851
            if t4781 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t4782 int = for_index852 + 1
                for_index852 = t4782
                var t4788 bool = index__256 > 0
                if t4788 {
                    var inline9827 rune = 44
                    var inline9828 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9827)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline9828)
                } else {}
                var t4784 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t4784)
                var inline9831 rune = 58
                var inline9832 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9831)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline9832)
                var t4785 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t4785)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t4786 int = compound_old859 + compound_value860
                index__256 = t4786
                continue
            } else {
                break Loop_loop4780
            }
        }
        var inline9835 rune = 125
        var inline9836 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9835)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline9836)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline9851 rune = 91
        var inline9852 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9851)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline9852)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop4792:
        for {
            var t4793 bool = for_index866 < for_limit865
            if t4793 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t4794 int = for_index866 + 1
                for_index866 = t4794
                var t4798 bool = index__259 > 0
                if t4798 {
                    var inline9843 rune = 44
                    var inline9844 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9843)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline9844)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t4796 int = compound_old871 + compound_value872
                index__259 = t4796
                continue
            } else {
                break Loop_loop4792
            }
        }
        var inline9847 rune = 93
        var inline9848 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9847)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline9848)
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
        var jp4803 string
        if x848 {
            jp4803 = "true"
        } else {
            jp4803 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp4803)
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
    var inline9861 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline9862 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline9861,
    }
    builder__265 = inline9862
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline9855 *_goml_vec_uint8 = builder__265.values
    var inline9856 Tuple2_4bool_6string = string_from_utf8(inline9855)
    var inline9858 string = inline9856._1
    return inline9858
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop4814:
        for {
            var t4815 bool = for_index883 < for_limit882
            if t4815 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t4816 int = for_index883 + 1
                for_index883 = t4816
                var t4818 string = for_item884._0
                var t4819 bool
                var inline9864 bool = t4818 == name__267
                t4819 = inline9864
                if t4819 {
                    var t4820 _goml_m_std_p_json_p_Value = for_item884._1
                    var t4821 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t4820,
                    }
                    return t4821
                } else {
                    continue
                }
            } else {
                break Loop_loop4814
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__int {
    var t4831 int
    var inline9883 int = _goml_runtime_core_string_len(value__272)
    t4831 = inline9883
    var t4832 bool
    var inline9880 int = 0
    var inline9881 bool = t4831 == inline9880
    t4832 = inline9881
    if t4832 {
        return Option__int_None{}
    } else {
        var t4833 uint8
        var inline9877 int = 0
        var inline9878 uint8 = _goml_runtime_core_string_byte_get(value__272, inline9877)
        t4833 = inline9878
        var negative__273 bool
        var inline9874 uint8 = 45
        var inline9875 bool = t4833 == inline9874
        negative__273 = inline9875
        var jp4835 int
        if negative__273 {
            jp4835 = 1
        } else {
            jp4835 = 0
        }
        var index__274 int = jp4835
        var result__275 int = 0
        var t4856 int
        var inline9872 int = _goml_runtime_core_string_len(value__272)
        t4856 = inline9872
        var t4857 bool
        var inline9870 bool = index__274 == t4856
        t4857 = inline9870
        if t4857 {
            return Option__int_None{}
        } else {
            Loop_loop4842:
            for {
                var t4843 int
                var inline9868 int = _goml_runtime_core_string_len(value__272)
                t4843 = inline9868
                var t4844 bool = index__274 < t4843
                if t4844 {
                    var byte__276 uint8
                    var inline9866 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline9866
                    var t4854 bool = byte__276 < 48
                    var jp4849 bool
                    if t4854 {
                        jp4849 = true
                    } else {
                        var t4855 bool = byte__276 > 57
                        jp4849 = t4855
                    }
                    if jp4849 {
                        return Option__int_None{}
                    } else {
                        var t4850 int = result__275 * 10
                        var t4851 uint8 = byte__276 - 48
                        var t4852 int = int(uint8(t4851))
                        var t4853 int = t4850 + t4852
                        result__275 = t4853
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t4846 int = compound_old895 + compound_value896
                        index__274 = t4846
                        continue
                    }
                } else {
                    break Loop_loop4842
                }
            }
            var jp4839 int
            if negative__273 {
                var t4841 int = 0 - result__275
                jp4839 = t4841
            } else {
                jp4839 = result__275
            }
            var t4840 Option__int = Option__int_Some{
                _0: jp4839,
            }
            return t4840
        }
    }
}

func main0() struct{} {
    var mtmp172 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp5642 _goml_m_std_p_json_p_Value
    switch mtmp172.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x173 _goml_m_std_p_json_p_Value = mtmp172.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp5642 = x173
        var mtmp176 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5642, "name")
        switch mtmp176.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline10400 string = "missing name"
            var inline10401 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10400)
            _goml_runtime_core_string_println(inline10401)
            var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5642, "version")
            switch mtmp181.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10415 string = "missing version"
                var inline10416 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10415)
                _goml_runtime_core_string_println(inline10416)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp183 Option__int
                switch x182.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline10426 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline10428 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10426)
                    mtmp183 = inline10428
                default:
                    mtmp183 = Option__int_None{}
                }
                switch mtmp183.(type) {
                case Option__int_None:
                    var inline10419 string = "invalid version"
                    var inline10420 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10419)
                    _goml_runtime_core_string_println(inline10420)
                case Option__int_Some:
                    var x184 int = mtmp183.(Option__int_Some)._0
                    var inline10423 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                    _goml_runtime_core_string_println(inline10423)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5642, "stable")
            switch mtmp186.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10430 string = "missing stable"
                var inline10431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10430)
                _goml_runtime_core_string_println(inline10431)
                var t5646 string = _goml_m_std_p_json_p_encode(jp5642)
                println__T_string(t5646)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field10714 bool
                switch x187.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline10441 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field10714 = inline10441
                    var inline10438 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field10714)
                    _goml_runtime_core_string_println(inline10438)
                    var t5646 string = _goml_m_std_p_json_p_encode(jp5642)
                    println__T_string(t5646)
                    return struct{}{}
                default:
                    var inline10434 string = "invalid stable"
                    var inline10435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10434)
                    _goml_runtime_core_string_println(inline10435)
                    var t5646 string = _goml_m_std_p_json_p_encode(jp5642)
                    println__T_string(t5646)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x177 _goml_m_std_p_json_p_Value = mtmp176.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field10720 string
            switch x177.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline10411 string = x177.(_goml_m_std_p_json_p_Value_String)._0
                commute_field10720 = inline10411
                var inline10408 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field10720)
                _goml_runtime_core_string_println(inline10408)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5642, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10415 string = "missing version"
                    var inline10416 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10415)
                    _goml_runtime_core_string_println(inline10416)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10426 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10428 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10426)
                        mtmp183 = inline10428
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline10419 string = "invalid version"
                        var inline10420 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10419)
                        _goml_runtime_core_string_println(inline10420)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline10423 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline10423)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5642, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10430 string = "missing stable"
                    var inline10431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10430)
                    _goml_runtime_core_string_println(inline10431)
                    var t5646 string = _goml_m_std_p_json_p_encode(jp5642)
                    println__T_string(t5646)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field10714 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10441 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field10714 = inline10441
                        var inline10438 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field10714)
                        _goml_runtime_core_string_println(inline10438)
                        var t5646 string = _goml_m_std_p_json_p_encode(jp5642)
                        println__T_string(t5646)
                        return struct{}{}
                    default:
                        var inline10434 string = "invalid stable"
                        var inline10435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10434)
                        _goml_runtime_core_string_println(inline10435)
                        var t5646 string = _goml_m_std_p_json_p_encode(jp5642)
                        println__T_string(t5646)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline10404 string = "invalid name"
                var inline10405 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10404)
                _goml_runtime_core_string_println(inline10405)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5642, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10415 string = "missing version"
                    var inline10416 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10415)
                    _goml_runtime_core_string_println(inline10416)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10426 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10428 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10426)
                        mtmp183 = inline10428
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline10419 string = "invalid version"
                        var inline10420 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10419)
                        _goml_runtime_core_string_println(inline10420)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline10423 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline10423)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp5642, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10430 string = "missing stable"
                    var inline10431 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10430)
                    _goml_runtime_core_string_println(inline10431)
                    var t5646 string = _goml_m_std_p_json_p_encode(jp5642)
                    println__T_string(t5646)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field10714 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10441 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field10714 = inline10441
                        var inline10438 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field10714)
                        _goml_runtime_core_string_println(inline10438)
                        var t5646 string = _goml_m_std_p_json_p_encode(jp5642)
                        println__T_string(t5646)
                        return struct{}{}
                    default:
                        var inline10434 string = "invalid stable"
                        var inline10435 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10434)
                        _goml_runtime_core_string_println(inline10435)
                        var t5646 string = _goml_m_std_p_json_p_encode(jp5642)
                        println__T_string(t5646)
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
        var inline10397 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x174)
        _goml_runtime_core_string_println(inline10397)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t5677 string = _goml_runtime_core_int_to_string(self__34)
    return t5677
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline10455 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline10456 bool = inline10455._0
    var inline10457 rune = inline10455._1
    if inline10456 {
        return inline10457
    } else {
        var inline10461 rune = _goml_runtime_core_string_get("", -1)
        return inline10461
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t5767 *ref_int_x = ref__Ref_3int(value__257)
    return t5767
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t5770 int = ref_get__Ref_3int(self__258)
    return t5770
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__259 *ref_int_x, value__260 int) struct{} {
    ref_set__Ref_3int(self__259, value__260)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(self__113 uint8, other__114 uint8) bool {
    var t5832 bool = self__113 == other__114
    return t5832
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t5835 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t5835
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop5840:
    for {
        var t5841 int
        var inline10475 int = _goml_runtime_core_string_len(x12)
        t5841 = inline10475
        var t5842 bool = index__26 < t5841
        if t5842 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t5844 int = compound_old17 + x16
                index__26 = t5844
                continue
            } else {
                var t5846 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t5846
            }
        } else {
            break Loop_loop5840
        }
    }
    var t5839 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t5839
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline10477 uint32 = uint32(rune(self__36))
    var inline10478 bool = utf8_valid_scalar(inline10477)
    if inline10478 {
        var inline10479 string = _goml_runtime_core_char_to_string(self__36)
        return inline10479
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t5882 int = _goml_runtime_core_string_len(self__38)
    return t5882
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t5885 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t5885
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline10495 bool = string_is_char_boundary(self__43, start__44)
    var inline10497 bool
    if inline10495 {
        var inline10500 bool = string_is_char_boundary(self__43, end__45)
        inline10497 = inline10500
    } else {
        inline10497 = false
    }
    if inline10497 {
        var inline10498 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline10498
    } else {
        var inline10499 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline10499
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t5971 bool
    var inline10554 bool = value__32 <= 1114111
    if inline10554 {
        var inline10555 bool = value__32 >= 55296
        var inline10557 bool
        if inline10555 {
            var inline10559 bool = value__32 <= 57343
            inline10557 = inline10559
        } else {
            inline10557 = false
        }
        var inline10558 bool = !inline10557
        t5971 = inline10558
    } else {
        t5971 = false
    }
    if t5971 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t5972 Option__char = Option__char_Some{
            _0: x24,
        }
        return t5972
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t5975 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t5975
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__176 *_goml_vec__goml_m_std_p_json_p_Value, elem__177 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__176, elem__177)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t5980 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t5980
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__176 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__177 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__176, elem__177)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t6016 string
    t6016 = value__31
    _goml_runtime_core_string_println(t6016)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t6151 bool = index__6 < 0
    var jp6149 bool
    if t6151 {
        jp6149 = true
    } else {
        var t6152 bool = index__6 >= length__7
        jp6149 = t6152
    }
    if jp6149 {
        var inline10576 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline10576
    } else {
        var t6036 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6036))
        var t6039 bool = first__8 < 128
        if t6039 {
            var inline10578 int = 1
            var inline10579 Option__char = char_from_uint32(first__8)
            switch inline10579.(type) {
            case Option__char_None:
                var inline10580 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline10580
            case Option__char_Some:
                var inline10581 rune = inline10579.(Option__char_Some)._0
                var inline10583 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline10581,
                    _2: inline10578,
                }
                return inline10583
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6043 bool = first__8 < 194
            if t6043 {
                var inline10585 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline10585
            } else {
                var t6047 bool = first__8 < 224
                if t6047 {
                    var t6060 int = length__7 - index__6
                    var t6061 bool = t6060 < 2
                    if t6061 {
                        var inline10587 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline10587
                    } else {
                        var t6049 int = index__6 + 1
                        var t6050 uint8
                        var inline10601 uint8 = _goml_runtime_core_string_byte_get(value__5, t6049)
                        t6050 = inline10601
                        var second__9 uint32 = uint32(uint8(t6050))
                        var t6053 bool
                        var inline10598 bool = second__9 < 128
                        if inline10598 {
                            t6053 = true
                        } else {
                            var inline10599 bool = second__9 > 191
                            t6053 = inline10599
                        }
                        if t6053 {
                            var inline10589 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline10589
                        } else {
                            var t6055_rhs uint32 = 31
                            var t6055 uint32 = first__8 & t6055_rhs
                            var t6056_rhs int = 6
                            var t6056 uint32 = t6055 << t6056_rhs
                            var t6057_rhs uint32 = 63
                            var t6057 uint32 = second__9 & t6057_rhs
                            var t6058 uint32 = t6056 | t6057
                            var inline10591 int = 2
                            var inline10592 Option__char = char_from_uint32(t6058)
                            switch inline10592.(type) {
                            case Option__char_None:
                                var inline10593 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline10593
                            case Option__char_Some:
                                var inline10594 rune = inline10592.(Option__char_Some)._0
                                var inline10596 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline10594,
                                    _2: inline10591,
                                }
                                return inline10596
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6065 bool = first__8 < 240
                    if t6065 {
                        var t6098 int = length__7 - index__6
                        var t6099 bool = t6098 < 3
                        if t6099 {
                            var inline10603 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline10603
                        } else {
                            var t6067 int = index__6 + 1
                            var t6068 uint8
                            var inline10618 uint8 = _goml_runtime_core_string_byte_get(value__5, t6067)
                            t6068 = inline10618
                            var second__10 uint32 = uint32(uint8(t6068))
                            var t6069 int = index__6 + 2
                            var t6070 uint8
                            var inline10616 uint8 = _goml_runtime_core_string_byte_get(value__5, t6069)
                            t6070 = inline10616
                            var third__11 uint32 = uint32(uint8(t6070))
                            var t6096 bool = utf8_invalid_continuation(second__10)
                            var jp6091 bool
                            if t6096 {
                                jp6091 = true
                            } else {
                                var inline10605 bool = third__11 < 128
                                if inline10605 {
                                    jp6091 = true
                                } else {
                                    var inline10606 bool = third__11 > 191
                                    jp6091 = inline10606
                                }
                            }
                            var jp6085 bool
                            if jp6091 {
                                jp6085 = true
                            } else {
                                var t6094 bool
                                var inline10608 uint32 = 224
                                var inline10609 bool = first__8 == inline10608
                                t6094 = inline10609
                                if t6094 {
                                    var t6095 bool = second__10 < 160
                                    jp6085 = t6095
                                } else {
                                    jp6085 = false
                                }
                            }
                            var jp6074 bool
                            if jp6085 {
                                jp6074 = true
                            } else {
                                var t6088 bool
                                var inline10611 uint32 = 237
                                var inline10612 bool = first__8 == inline10611
                                t6088 = inline10612
                                if t6088 {
                                    var t6089 bool = second__10 >= 160
                                    jp6074 = t6089
                                } else {
                                    jp6074 = false
                                }
                            }
                            if jp6074 {
                                var inline10614 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline10614
                            } else {
                                var t6076_rhs uint32 = 15
                                var t6076 uint32 = first__8 & t6076_rhs
                                var t6077_rhs int = 12
                                var t6077 uint32 = t6076 << t6077_rhs
                                var t6078_rhs uint32 = 63
                                var t6078 uint32 = second__10 & t6078_rhs
                                var t6079_rhs int = 6
                                var t6079 uint32 = t6078 << t6079_rhs
                                var t6080 uint32 = t6077 | t6079
                                var t6081_rhs uint32 = 63
                                var t6081 uint32 = third__11 & t6081_rhs
                                var t6082 uint32 = t6080 | t6081
                                var t6083 Tuple3_4bool_4char_3int = utf8_valid_decode(t6082, 3)
                                return t6083
                            }
                        }
                    } else {
                        var t6103 bool = first__8 < 245
                        if t6103 {
                            var t6144 int = length__7 - index__6
                            var t6145 bool = t6144 < 4
                            if t6145 {
                                var t6146 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t6146
                            } else {
                                var t6105 int = index__6 + 1
                                var t6106 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6105)
                                var second__12 uint32 = uint32(uint8(t6106))
                                var t6107 int = index__6 + 2
                                var t6108 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6107)
                                var third__13 uint32 = uint32(uint8(t6108))
                                var t6109 int = index__6 + 3
                                var t6110 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6109)
                                var fourth__14 uint32 = uint32(uint8(t6110))
                                var t6142 bool = utf8_invalid_continuation(second__12)
                                var jp6140 bool
                                if t6142 {
                                    jp6140 = true
                                } else {
                                    var t6143 bool = utf8_invalid_continuation(third__13)
                                    jp6140 = t6143
                                }
                                var jp6134 bool
                                if jp6140 {
                                    jp6134 = true
                                } else {
                                    var t6141 bool = utf8_invalid_continuation(fourth__14)
                                    jp6134 = t6141
                                }
                                var jp6128 bool
                                if jp6134 {
                                    jp6128 = true
                                } else {
                                    var t6137 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t6137 {
                                        var t6138 bool = second__12 < 144
                                        jp6128 = t6138
                                    } else {
                                        jp6128 = false
                                    }
                                }
                                var jp6114 bool
                                if jp6128 {
                                    jp6114 = true
                                } else {
                                    var t6131 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t6131 {
                                        var t6132 bool = second__12 > 143
                                        jp6114 = t6132
                                    } else {
                                        jp6114 = false
                                    }
                                }
                                if jp6114 {
                                    var t6115 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6115
                                } else {
                                    var t6116_rhs uint32 = 7
                                    var t6116 uint32 = first__8 & t6116_rhs
                                    var t6117_rhs int = 18
                                    var t6117 uint32 = t6116 << t6117_rhs
                                    var t6118_rhs uint32 = 63
                                    var t6118 uint32 = second__12 & t6118_rhs
                                    var t6119_rhs int = 12
                                    var t6119 uint32 = t6118 << t6119_rhs
                                    var t6120 uint32 = t6117 | t6119
                                    var t6121_rhs uint32 = 63
                                    var t6121 uint32 = third__13 & t6121_rhs
                                    var t6122_rhs int = 6
                                    var t6122 uint32 = t6121 << t6122_rhs
                                    var t6123 uint32 = t6120 | t6122
                                    var t6124_rhs uint32 = 63
                                    var t6124 uint32 = fourth__14 & t6124_rhs
                                    var t6125 uint32 = t6123 | t6124
                                    var t6126 Tuple3_4bool_4char_3int = utf8_valid_decode(t6125, 4)
                                    return t6126
                                }
                            }
                        } else {
                            var t6147 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t6147
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t6157 uint32 = uint32(rune(value__29))
    var t6158 bool
    var inline10620 bool = t6157 <= 1114111
    if inline10620 {
        var inline10621 bool = t6157 >= 55296
        var inline10623 bool
        if inline10621 {
            var inline10625 bool = t6157 <= 57343
            inline10623 = inline10625
        } else {
            inline10623 = false
        }
        var inline10624 bool = !inline10623
        t6158 = inline10624
    } else {
        t6158 = false
    }
    if t6158 {
        var t6159 string = _goml_runtime_core_char_to_string(value__29)
        return t6159
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t6174 bool = index__16 < 0
    var jp6165 bool
    if t6174 {
        jp6165 = true
    } else {
        var t6175 int
        var inline10627 int = _goml_runtime_core_string_len(value__15)
        t6175 = inline10627
        var t6176 bool = index__16 > t6175
        jp6165 = t6176
    }
    if jp6165 {
        return false
    } else {
        var t6168 int
        var inline10636 int = _goml_runtime_core_string_len(value__15)
        t6168 = inline10636
        var t6169 bool
        var inline10634 bool = index__16 == t6168
        t6169 = inline10634
        if t6169 {
            return true
        } else {
            var t6170 uint8
            var inline10632 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t6170 = inline10632
            var t6171_rhs uint8 = 192
            var t6171 uint8 = t6170 & t6171_rhs
            var t6172 bool
            var inline10629 uint8 = 128
            var inline10630 bool = t6171 == inline10629
            t6172 = inline10630
            var t6173 bool = !t6172
            return t6173
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t6185 bool = string_is_char_boundary(value__21, start__22)
    var jp6182 bool
    if t6185 {
        var t6186 bool = string_is_char_boundary(value__21, end__23)
        jp6182 = t6186
    } else {
        jp6182 = false
    }
    if jp6182 {
        var t6183 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t6183
    } else {
        var t6184 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t6184
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t6205 bool = value__4 <= 1114111
    if t6205 {
        var t6209 bool = value__4 >= 55296
        var jp6207 bool
        if t6209 {
            var t6210 bool = value__4 <= 57343
            jp6207 = t6210
        } else {
            jp6207 = false
        }
        var t6208 bool = !jp6207
        return t6208
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t6217 string = _goml_runtime_core_int_to_string(self__69)
    return t6217
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t6220 string = _goml_runtime_core_bool_to_string(self__66)
    return t6220
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t6223 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t6223
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field10726 rune
    var inline10640 bool = utf8_valid_scalar(value__0)
    if inline10640 {
        var inline10641 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline10643 rune = inline10641._1
        commute_field10726 = inline10643
        var t6229 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field10726,
            _2: width__1,
        }
        return t6229
    } else {
        var inline10638 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline10638
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t6234 bool = value__3 < 128
    if t6234 {
        return true
    } else {
        var t6235 bool = value__3 > 191
        return t6235
    }
}

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t6238 bool = self__117 == other__118
    return t6238
}

func main() {
    main0()
}
