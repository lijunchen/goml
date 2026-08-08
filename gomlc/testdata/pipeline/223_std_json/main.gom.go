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

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple2_11Option__int_11Option__int struct {
    _0 Option__int
    _1 Option__int
}

type Tuple2_14Option__string_14Option__string struct {
    _0 Option__string
    _1 Option__string
}

type Tuple6_4bool_10Vec_5uint8_3int_4bool_3int_6string struct {
    _0 bool
    _1 *_goml_vec_uint8
    _2 int
    _3 bool
    _4 int
    _5 string
}

type Tuple5_4bool_3int_4bool_3int_6string struct {
    _0 bool
    _1 int
    _2 bool
    _3 int
    _4 string
}

type Tuple3_4bool_10Vec_5uint8_6string struct {
    _0 bool
    _1 *_goml_vec_uint8
    _2 string
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

type Tuple2_3int_4char struct {
    _0 int
    _1 rune
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

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_io_p_ErrorDetails struct {
    kind_value _goml_m_std_p_io_p_ErrorKind
    operation_value string
    context_value Option__string
    raw_os_code_value Option__int
    message_value string
}

type _goml_m_std_p_io_p_Error struct {
    details _goml_m_std_p_io_p_ErrorDetails
}

type _goml_m_std_p_num_p_ParseIntError struct {
    details _goml_m_std_p_io_p_ErrorDetails
    input_value string
    radix_value int
}

type _goml_m_std_p_num_p_ParseFloatError struct {
    details _goml_m_std_p_io_p_ErrorDetails
    input_value string
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

type _goml_m_FnIterator_____o_int_c_char_q_ struct {
    next_fn func() _goml_m_Option_____o_int_c_char_q_
}

type FnIterator__char struct {
    next_fn func() Option__char
}

type FnIterator__int struct {
    next_fn func() Option__int
}

type closure_env_inherent_string_string_char_indices_0 struct {
    index_0 *ref_int_x
    self_1 string
}

type closure_env_inherent_string_string_chars_1 struct {
    self_0 string
    index_1 *ref_int_x
}

type closure_env_goml_builtin_range_2 struct {
    current_0 *ref_int_x
    end_1 int
}

type _goml_m_std_p_io_p_ErrorKind int32

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

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error interface {
    is_goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error()
}

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error_Ok struct {
    _0 _goml_m_std_p_bytes_p_Bytes
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error_Ok) is_goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error() {}

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error_Err struct {
    _0 _goml_m_std_p_io_p_Error
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error_Err) is_goml_m_Result____std_p_bytes_p_Bytes____std_p_io_p_Error() {}

type _goml_m_Result____string____std_p_io_p_Error interface {
    is_goml_m_Result____string____std_p_io_p_Error()
}

type _goml_m_Result____string____std_p_io_p_Error_Ok struct {
    _0 string
}

func (_ _goml_m_Result____string____std_p_io_p_Error_Ok) is_goml_m_Result____string____std_p_io_p_Error() {}

type _goml_m_Result____string____std_p_io_p_Error_Err struct {
    _0 _goml_m_std_p_io_p_Error
}

func (_ _goml_m_Result____string____std_p_io_p_Error_Err) is_goml_m_Result____string____std_p_io_p_Error() {}

type _goml_m_Result____unit____std_p_io_p_Error interface {
    is_goml_m_Result____unit____std_p_io_p_Error()
}

type _goml_m_Result____unit____std_p_io_p_Error_Ok struct {
    _0 struct{}
}

func (_ _goml_m_Result____unit____std_p_io_p_Error_Ok) is_goml_m_Result____unit____std_p_io_p_Error() {}

type _goml_m_Result____unit____std_p_io_p_Error_Err struct {
    _0 _goml_m_std_p_io_p_Error
}

func (_ _goml_m_Result____unit____std_p_io_p_Error_Err) is_goml_m_Result____unit____std_p_io_p_Error() {}

type _goml_m_Result____std_p_bytes_p_Bytes____string interface {
    is_goml_m_Result____std_p_bytes_p_Bytes____string()
}

type _goml_m_Result____std_p_bytes_p_Bytes____string_Ok struct {
    _0 _goml_m_std_p_bytes_p_Bytes
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____string_Ok) is_goml_m_Result____std_p_bytes_p_Bytes____string() {}

type _goml_m_Result____std_p_bytes_p_Bytes____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____string_Err) is_goml_m_Result____std_p_bytes_p_Bytes____string() {}

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

type _goml_m_Result____int____std_p_num_p_ParseIntError interface {
    is_goml_m_Result____int____std_p_num_p_ParseIntError()
}

type _goml_m_Result____int____std_p_num_p_ParseIntError_Ok struct {
    _0 int
}

func (_ _goml_m_Result____int____std_p_num_p_ParseIntError_Ok) is_goml_m_Result____int____std_p_num_p_ParseIntError() {}

type _goml_m_Result____int____std_p_num_p_ParseIntError_Err struct {
    _0 _goml_m_std_p_num_p_ParseIntError
}

func (_ _goml_m_Result____int____std_p_num_p_ParseIntError_Err) is_goml_m_Result____int____std_p_num_p_ParseIntError() {}

type _goml_m_Result____uint____std_p_num_p_ParseIntError interface {
    is_goml_m_Result____uint____std_p_num_p_ParseIntError()
}

type _goml_m_Result____uint____std_p_num_p_ParseIntError_Ok struct {
    _0 uint
}

func (_ _goml_m_Result____uint____std_p_num_p_ParseIntError_Ok) is_goml_m_Result____uint____std_p_num_p_ParseIntError() {}

type _goml_m_Result____uint____std_p_num_p_ParseIntError_Err struct {
    _0 _goml_m_std_p_num_p_ParseIntError
}

func (_ _goml_m_Result____uint____std_p_num_p_ParseIntError_Err) is_goml_m_Result____uint____std_p_num_p_ParseIntError() {}

type _goml_m_Result____float32____std_p_num_p_ParseFloatError interface {
    is_goml_m_Result____float32____std_p_num_p_ParseFloatError()
}

type _goml_m_Result____float32____std_p_num_p_ParseFloatError_Ok struct {
    _0 float32
}

func (_ _goml_m_Result____float32____std_p_num_p_ParseFloatError_Ok) is_goml_m_Result____float32____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____float32____std_p_num_p_ParseFloatError_Err struct {
    _0 _goml_m_std_p_num_p_ParseFloatError
}

func (_ _goml_m_Result____float32____std_p_num_p_ParseFloatError_Err) is_goml_m_Result____float32____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____float64____std_p_num_p_ParseFloatError interface {
    is_goml_m_Result____float64____std_p_num_p_ParseFloatError()
}

type _goml_m_Result____float64____std_p_num_p_ParseFloatError_Ok struct {
    _0 float64
}

func (_ _goml_m_Result____float64____std_p_num_p_ParseFloatError_Ok) is_goml_m_Result____float64____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____float64____std_p_num_p_ParseFloatError_Err struct {
    _0 _goml_m_std_p_num_p_ParseFloatError
}

func (_ _goml_m_Result____float64____std_p_num_p_ParseFloatError_Err) is_goml_m_Result____float64____std_p_num_p_ParseFloatError() {}

type Option__int64 interface {
    isOption__int64()
}

type Option__int64_None struct {}

func (_ Option__int64_None) isOption__int64() {}

type Option__int64_Some struct {
    _0 int64
}

func (_ Option__int64_Some) isOption__int64() {}

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

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame interface {
    is_goml_m_Option____std_p_serde_p_ValueSerializeFrame()
}

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame_None struct {}

func (_ _goml_m_Option____std_p_serde_p_ValueSerializeFrame_None) is_goml_m_Option____std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame_Some struct {
    _0 _goml_m_std_p_serde_p_ValueSerializeFrame
}

func (_ _goml_m_Option____std_p_serde_p_ValueSerializeFrame_Some) is_goml_m_Option____std_p_serde_p_ValueSerializeFrame() {}

type _goml_m_Option_____o_int_c_char_q_ interface {
    is_goml_m_Option_____o_int_c_char_q_()
}

type _goml_m_Option_____o_int_c_char_q__None struct {}

func (_ _goml_m_Option_____o_int_c_char_q__None) is_goml_m_Option_____o_int_c_char_q_() {}

type _goml_m_Option_____o_int_c_char_q__Some struct {
    _0 Tuple2_3int_4char
}

func (_ _goml_m_Option_____o_int_c_char_q__Some) is_goml_m_Option_____o_int_c_char_q_() {}

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

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
    var inline8431 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline8431
    var t2503 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t2503
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline8446 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline8446
    var t2517 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t2517, length__5)
    var for_index1 int = 0
    Loop_loop2519:
    for {
        var t2520 bool = for_index1 < length__5
        if t2520 {
            var for_item3 int = for_index1
            var t2521 int = for_index1 + 1
            for_index1 = t2521
            var t2522 *_goml_vec_uint8 = self__3.values
            var t2523 uint8
            var inline8442 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2523 = inline8442
            vec_push__Vec_5uint8(t2522, t2523)
            continue
        } else {
            break Loop_loop2519
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t2526 string
    var inline8448 string = char_to_string(value__8)
    t2526 = inline8448
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2526)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t4479 string = "" + message__201
    var t4480 string = t4479 + " at byte "
    var t4481 *ref_int_x = value__200.index
    var t4482 int
    var inline10165 int = ref_get__Ref_3int(t4481)
    t4482 = inline10165
    var t4483 string
    var inline10163 string = _goml_runtime_core_int_to_string(t4482)
    t4483 = inline10163
    var t4484 string = t4480 + t4483
    return t4484
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4499:
    for {
        var t4507 *ref_int_x = value__203.index
        var t4508 int
        var inline10198 int = ref_get__Ref_3int(t4507)
        t4508 = inline10198
        var t4509 string = value__203.input
        var t4510 int
        var inline10196 int = _goml_runtime_core_string_len(t4509)
        t4510 = inline10196
        var t4511 bool = t4508 < t4510
        var jp4501 bool
        if t4511 {
            var t4512 string = value__203.input
            var t4513 *ref_int_x = value__203.index
            var t4514 int
            var inline10190 int = ref_get__Ref_3int(t4513)
            t4514 = inline10190
            var t4515 uint8
            var inline10188 uint8 = _goml_runtime_core_string_byte_get(t4512, t4514)
            t4515 = inline10188
            var inline10179 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4515, 9)
            var inline10181 bool
            if inline10179 {
                inline10181 = true
            } else {
                var inline10186 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4515, 10)
                inline10181 = inline10186
            }
            var inline10183 bool
            if inline10181 {
                inline10183 = true
            } else {
                var inline10185 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4515, 13)
                inline10183 = inline10185
            }
            if inline10183 {
                jp4501 = true
            } else {
                var inline10184 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4515, 32)
                jp4501 = inline10184
            }
        } else {
            jp4501 = false
        }
        if jp4501 {
            var t4502 *ref_int_x = value__203.index
            var t4503 *ref_int_x = value__203.index
            var t4504 int
            var inline10194 int = ref_get__Ref_3int(t4503)
            t4504 = inline10194
            var t4505 int = t4504 + 1
            ref_set__Ref_3int(t4502, t4505)
            continue
        } else {
            break Loop_loop4499
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__uint32 {
    var t4546 bool = value__204 >= 48
    var jp4522 bool
    if t4546 {
        var t4547 bool = value__204 <= 57
        jp4522 = t4547
    } else {
        jp4522 = false
    }
    if jp4522 {
        var t4523 uint8 = value__204 - 48
        var t4524 uint32 = uint32(uint8(t4523))
        var t4525 Option__uint32 = Option__uint32_Some{
            _0: t4524,
        }
        return t4525
    } else {
        var t4544 bool = value__204 >= 65
        var jp4529 bool
        if t4544 {
            var t4545 bool = value__204 <= 70
            jp4529 = t4545
        } else {
            jp4529 = false
        }
        if jp4529 {
            var t4530 uint8 = value__204 - 65
            var t4531 uint8 = t4530 + 10
            var t4532 uint32 = uint32(uint8(t4531))
            var t4533 Option__uint32 = Option__uint32_Some{
                _0: t4532,
            }
            return t4533
        } else {
            var t4542 bool = value__204 >= 97
            var jp4537 bool
            if t4542 {
                var t4543 bool = value__204 <= 102
                jp4537 = t4543
            } else {
                jp4537 = false
            }
            if jp4537 {
                var t4538 uint8 = value__204 - 97
                var t4539 uint8 = t4538 + 10
                var t4540 uint32 = uint32(uint8(t4539))
                var t4541 Option__uint32 = Option__uint32_Some{
                    _0: t4540,
                }
                return t4541
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t4552 *ref_int_x = value__205.index
    var t4553 int
    var inline10226 int = ref_get__Ref_3int(t4552)
    t4553 = inline10226
    var t4554 int = t4553 + 4
    var t4555 string = value__205.input
    var t4556 int
    var inline10224 int = _goml_runtime_core_string_len(t4555)
    t4556 = inline10224
    var t4557 bool = t4554 > t4556
    if t4557 {
        var t4558 string
        var inline10200 string = "incomplete unicode escape"
        var inline10201 string = "" + inline10200
        var inline10202 string = inline10201 + " at byte "
        var inline10203 *ref_int_x = value__205.index
        var inline10204 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10203)
        var inline10205 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10204)
        var inline10206 string = inline10202 + inline10205
        t4558 = inline10206
        var t4559 Result__uint32__string = Result__uint32__string_Err{
            _0: t4558,
        }
        return t4559
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop4566:
        for {
            var t4567 bool = for_index744 < for_limit745
            if t4567 {
                var for_item746 int = for_index744
                var t4568 int = for_index744 + 1
                for_index744 = t4568
                var t4569 string = value__205.input
                var t4570 *ref_int_x = value__205.index
                var t4571 int
                var inline10218 int = ref_get__Ref_3int(t4570)
                t4571 = inline10218
                var t4572 int = t4571 + for_item746
                var t4573 uint8
                var inline10216 uint8 = _goml_runtime_core_string_byte_get(t4569, t4572)
                t4573 = inline10216
                var mtmp748 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t4573)
                switch mtmp748.(type) {
                case Option__uint32_None:
                    var t4575 string
                    var inline10208 string = "invalid unicode escape"
                    var inline10209 string = "" + inline10208
                    var inline10210 string = inline10209 + " at byte "
                    var inline10211 *ref_int_x = value__205.index
                    var inline10212 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10211)
                    var inline10213 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10212)
                    var inline10214 string = inline10210 + inline10213
                    t4575 = inline10214
                    var t4576 Result__uint32__string = Result__uint32__string_Err{
                        _0: t4575,
                    }
                    return t4576
                case Option__uint32_Some:
                    var x749 uint32 = mtmp748.(Option__uint32_Some)._0
                    var t4577 uint32 = result__206 * 16
                    var t4578 uint32 = t4577 + x749
                    result__206 = t4578
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4566
            }
        }
        var t4561 *ref_int_x = value__205.index
        var t4562 *ref_int_x = value__205.index
        var t4563 int
        var inline10222 int = ref_get__Ref_3int(t4562)
        t4563 = inline10222
        var t4564 int = t4563 + 4
        ref_set__Ref_3int(t4561, t4564)
        var t4565 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__206,
        }
        return t4565
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var commute_field11580 rune
    var inline10239 bool = utf8_valid_scalar(codepoint__211)
    if inline10239 {
        var inline10240 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__211)
        var inline10242 rune = inline10240._1
        commute_field11580 = inline10242
        var inline10236 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field11580)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline10236)
        var t4585 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t4585
    } else {
        var t4583 string
        var inline10228 string = "invalid unicode codepoint"
        var inline10229 string = "" + inline10228
        var inline10230 string = inline10229 + " at byte "
        var inline10231 *ref_int_x = value__209.index
        var inline10232 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10231)
        var inline10233 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10232)
        var inline10234 string = inline10230 + inline10233
        t4583 = inline10234
        var t4584 Result__unit__string = Result__unit__string_Err{
            _0: t4583,
        }
        return t4584
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4589 uint32
    switch mtmp756.(type) {
    case Result__uint32__string_Ok:
        var x757 uint32 = mtmp756.(Result__uint32__string_Ok)._0
        jp4589 = x757
        var t4651 bool = jp4589 >= 55296
        var jp4593 bool
        if t4651 {
            var t4652 bool = jp4589 <= 56319
            jp4593 = t4652
        } else {
            jp4593 = false
        }
        if jp4593 {
            var t4630 *ref_int_x = value__213.index
            var t4631 int
            var inline10290 int = ref_get__Ref_3int(t4630)
            t4631 = inline10290
            var t4632 int = t4631 + 2
            var t4633 string = value__213.input
            var t4634 int
            var inline10288 int = _goml_runtime_core_string_len(t4633)
            t4634 = inline10288
            var t4635 bool = t4632 > t4634
            var jp4622 bool
            if t4635 {
                jp4622 = true
            } else {
                var t4636 string = value__213.input
                var t4637 *ref_int_x = value__213.index
                var t4638 int
                var inline10251 int = ref_get__Ref_3int(t4637)
                t4638 = inline10251
                var t4639 uint8
                var inline10249 uint8 = _goml_runtime_core_string_byte_get(t4636, t4638)
                t4639 = inline10249
                var t4640 bool
                var inline10246 uint8 = 92
                var inline10247 bool = t4639 == inline10246
                t4640 = inline10247
                var t4641 bool = !t4640
                jp4622 = t4641
            }
            var jp4597 bool
            if jp4622 {
                jp4597 = true
            } else {
                var t4623 string = value__213.input
                var t4624 *ref_int_x = value__213.index
                var t4625 int
                var inline10258 int = ref_get__Ref_3int(t4624)
                t4625 = inline10258
                var t4626 int = t4625 + 1
                var t4627 uint8
                var inline10256 uint8 = _goml_runtime_core_string_byte_get(t4623, t4626)
                t4627 = inline10256
                var t4628 bool
                var inline10253 uint8 = 117
                var inline10254 bool = t4627 == inline10253
                t4628 = inline10254
                var t4629 bool = !t4628
                jp4597 = t4629
            }
            if jp4597 {
                var t4598 string
                var inline10260 string = "missing low surrogate"
                var inline10261 string = "" + inline10260
                var inline10262 string = inline10261 + " at byte "
                var inline10263 *ref_int_x = value__213.index
                var inline10264 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10263)
                var inline10265 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10264)
                var inline10266 string = inline10262 + inline10265
                t4598 = inline10266
                var t4599 Result__unit__string = Result__unit__string_Err{
                    _0: t4598,
                }
                return t4599
            } else {
                var t4600 *ref_int_x = value__213.index
                var t4601 *ref_int_x = value__213.index
                var t4602 int
                var inline10286 int = ref_get__Ref_3int(t4601)
                t4602 = inline10286
                var t4603 int = t4602 + 2
                ref_set__Ref_3int(t4600, t4603)
                var mtmp760 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4605 uint32
                switch mtmp760.(type) {
                case Result__uint32__string_Ok:
                    var x761 uint32 = mtmp760.(Result__uint32__string_Ok)._0
                    jp4605 = x761
                    var t4618 bool = jp4605 < 56320
                    var jp4609 bool
                    if t4618 {
                        jp4609 = true
                    } else {
                        var t4619 bool = jp4605 > 57343
                        jp4609 = t4619
                    }
                    if jp4609 {
                        var t4610 string
                        var inline10268 string = "invalid low surrogate"
                        var inline10269 string = "" + inline10268
                        var inline10270 string = inline10269 + " at byte "
                        var inline10271 *ref_int_x = value__213.index
                        var inline10272 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10271)
                        var inline10273 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10272)
                        var inline10274 string = inline10270 + inline10273
                        t4610 = inline10274
                        var t4611 Result__unit__string = Result__unit__string_Err{
                            _0: t4610,
                        }
                        return t4611
                    } else {
                        var t4612 uint32 = jp4589 - 55296
                        var t4613 uint32 = t4612 * 1024
                        var t4614 uint32 = 65536 + t4613
                        var t4615 uint32 = t4614 + jp4605
                        var t4616 uint32 = t4615 - 56320
                        var inline10276 Option__char = char_from_uint32(t4616)
                        switch inline10276.(type) {
                        case Option__char_None:
                            var inline10277 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline10278 Result__unit__string = Result__unit__string_Err{
                                _0: inline10277,
                            }
                            return inline10278
                        case Option__char_Some:
                            var inline10279 rune = inline10276.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline10279)
                            var inline10282 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline10282
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x762 string = mtmp760.(Result__uint32__string_Err)._0
                    var t4620 Result__unit__string = Result__unit__string_Err{
                        _0: x762,
                    }
                    return t4620
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t4649 bool = jp4589 >= 56320
            var jp4645 bool
            if t4649 {
                var t4650 bool = jp4589 <= 57343
                jp4645 = t4650
            } else {
                jp4645 = false
            }
            if jp4645 {
                var t4646 string = _goml_m_std_p_json_p_json__error(value__213, "unexpected low surrogate")
                var t4647 Result__unit__string = Result__unit__string_Err{
                    _0: t4646,
                }
                return t4647
            } else {
                var t4648 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4589)
                return t4648
            }
        }
    case Result__uint32__string_Err:
        var x758 string = mtmp756.(Result__uint32__string_Err)._0
        var t4653 Result__unit__string = Result__unit__string_Err{
            _0: x758,
        }
        return t4653
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4769 *ref_int_x = value__217.index
    var t4770 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4769)
    var t4771 string = value__217.input
    var t4772 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4771)
    var t4773 bool = t4770 >= t4772
    var jp4761 bool
    if t4773 {
        jp4761 = true
    } else {
        var t4774 string = value__217.input
        var t4775 *ref_int_x = value__217.index
        var t4776 int
        var inline10297 int = ref_get__Ref_3int(t4775)
        t4776 = inline10297
        var t4777 uint8
        var inline10295 uint8 = _goml_runtime_core_string_byte_get(t4774, t4776)
        t4777 = inline10295
        var t4778 bool
        var inline10292 uint8 = 34
        var inline10293 bool = t4777 == inline10292
        t4778 = inline10293
        var t4779 bool = !t4778
        jp4761 = t4779
    }
    if jp4761 {
        var t4762 string
        var inline10299 string = "expected string"
        var inline10300 string = "" + inline10299
        var inline10301 string = inline10300 + " at byte "
        var inline10302 *ref_int_x = value__217.index
        var inline10303 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10302)
        var inline10304 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10303)
        var inline10305 string = inline10301 + inline10304
        t4762 = inline10305
        var t4763 Result__string__string = Result__string__string_Err{
            _0: t4762,
        }
        return t4763
    } else {
        var t4764 *ref_int_x = value__217.index
        var t4765 *ref_int_x = value__217.index
        var t4766 int
        var inline10309 int = ref_get__Ref_3int(t4765)
        t4766 = inline10309
        var t4767 int = t4766 + 1
        ref_set__Ref_3int(t4764, t4767)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t4657 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4657)
        Loop_loop4661:
        for {
            var t4662 *ref_int_x = value__217.index
            var t4663 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4662)
            var t4664 string = value__217.input
            var t4665 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4664)
            var t4666 bool = t4663 < t4665
            if t4666 {
                var t4667 string = value__217.input
                var t4668 *ref_int_x = value__217.index
                var t4669 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4668)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4667, t4669)
                var t4671 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(byte__220, 34)
                if t4671 {
                    var t4679 *ref_int_x = value__217.index
                    var t4680 int
                    var inline10325 int = ref_get__Ref_3int(t4679)
                    t4680 = inline10325
                    var t4681 bool = segment__219 < t4680
                    if t4681 {
                        var t4682 string = value__217.input
                        var t4683 *ref_int_x = value__217.index
                        var t4684 int
                        var inline10313 int = ref_get__Ref_3int(t4683)
                        t4684 = inline10313
                        var t4685 string
                        var inline10311 string = string_byte_slice(t4682, segment__219, t4684)
                        t4685 = inline10311
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4685)
                    } else {}
                    var t4673 *ref_int_x = value__217.index
                    var t4674 *ref_int_x = value__217.index
                    var t4675 int
                    var inline10323 int = ref_get__Ref_3int(t4674)
                    t4675 = inline10323
                    var t4676 int = t4675 + 1
                    ref_set__Ref_3int(t4673, t4676)
                    var t4677 string
                    var inline10315 *_goml_vec_uint8 = builder__218.values
                    var inline10316 Tuple2_4bool_6string = string_from_utf8(inline10315)
                    var inline10318 string = inline10316._1
                    t4677 = inline10318
                    var t4678 Result__string__string = Result__string__string_Ok{
                        _0: t4677,
                    }
                    return t4678
                } else {
                    var t4688 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(byte__220, 92)
                    if t4688 {
                        var t4743 *ref_int_x = value__217.index
                        var t4744 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4743)
                        var t4745 bool = segment__219 < t4744
                        if t4745 {
                            var t4746 string = value__217.input
                            var t4747 *ref_int_x = value__217.index
                            var t4748 int
                            var inline10329 int = ref_get__Ref_3int(t4747)
                            t4748 = inline10329
                            var t4749 string
                            var inline10327 string = string_byte_slice(t4746, segment__219, t4748)
                            t4749 = inline10327
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4749)
                        } else {}
                        var t4690 *ref_int_x = value__217.index
                        var t4691 *ref_int_x = value__217.index
                        var t4692 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4691)
                        var t4693 int = t4692 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4690, t4693)
                        var t4736 *ref_int_x = value__217.index
                        var t4737 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4736)
                        var t4738 string = value__217.input
                        var t4739 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4738)
                        var t4740 bool = t4737 >= t4739
                        if t4740 {
                            var t4741 string
                            var inline10331 string = "incomplete escape"
                            var inline10332 string = "" + inline10331
                            var inline10333 string = inline10332 + " at byte "
                            var inline10334 *ref_int_x = value__217.index
                            var inline10335 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10334)
                            var inline10336 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10335)
                            var inline10337 string = inline10333 + inline10336
                            t4741 = inline10337
                            var t4742 Result__string__string = Result__string__string_Err{
                                _0: t4741,
                            }
                            return t4742
                        } else {
                            var t4695 string = value__217.input
                            var t4696 *ref_int_x = value__217.index
                            var t4697 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4696)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4695, t4697)
                            var t4698 *ref_int_x = value__217.index
                            var t4699 *ref_int_x = value__217.index
                            var t4700 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4699)
                            var t4701 int = t4700 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4698, t4701)
                            var t4705 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 34)
                            if t4705 {
                                var inline10339 rune = 34
                                var inline10340 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10339)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline10340)
                                var t4703 *ref_int_x = value__217.index
                                var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                segment__219 = t4704
                                continue
                            } else {
                                var t4708 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 92)
                                if t4708 {
                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 92)
                                    var t4703 *ref_int_x = value__217.index
                                    var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                    segment__219 = t4704
                                    continue
                                } else {
                                    var t4711 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 47)
                                    if t4711 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t4703 *ref_int_x = value__217.index
                                        var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                        segment__219 = t4704
                                        continue
                                    } else {
                                        var t4714 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 98)
                                        if t4714 {
                                            var mtmp770 Option__char = char_from_uint32(8)
                                            switch mtmp770.(type) {
                                            case Option__char_None:
                                                var t4703 *ref_int_x = value__217.index
                                                var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                                segment__219 = t4704
                                                continue
                                            case Option__char_Some:
                                                var x771 rune = mtmp770.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t4703 *ref_int_x = value__217.index
                                                var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                                segment__219 = t4704
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t4718 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 102)
                                            if t4718 {
                                                var mtmp772 Option__char = char_from_uint32(12)
                                                switch mtmp772.(type) {
                                                case Option__char_None:
                                                    var t4703 *ref_int_x = value__217.index
                                                    var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                                    segment__219 = t4704
                                                    continue
                                                case Option__char_Some:
                                                    var x773 rune = mtmp772.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t4703 *ref_int_x = value__217.index
                                                    var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                                    segment__219 = t4704
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t4722 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 110)
                                                if t4722 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t4703 *ref_int_x = value__217.index
                                                    var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                                    segment__219 = t4704
                                                    continue
                                                } else {
                                                    var t4725 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 114)
                                                    if t4725 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t4703 *ref_int_x = value__217.index
                                                        var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                                        segment__219 = t4704
                                                        continue
                                                    } else {
                                                        var t4728 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 116)
                                                        if t4728 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t4703 *ref_int_x = value__217.index
                                                            var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                                            segment__219 = t4704
                                                            continue
                                                        } else {
                                                            var t4731 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(escape__221, 117)
                                                            if t4731 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t4703 *ref_int_x = value__217.index
                                                                    var t4704 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4703)
                                                                    segment__219 = t4704
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x776 string = mtmp774.(Result__unit__string_Err)._0
                                                                    var t4733 Result__string__string = Result__string__string_Err{
                                                                        _0: x776,
                                                                    }
                                                                    return t4733
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t4734 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t4735 Result__string__string = Result__string__string_Err{
                                                                    _0: t4734,
                                                                }
                                                                return t4735
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
                        var t4752 bool = byte__220 < 32
                        if t4752 {
                            var t4753 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t4754 Result__string__string = Result__string__string_Err{
                                _0: t4753,
                            }
                            return t4754
                        } else {
                            var t4755 *ref_int_x = value__217.index
                            var t4756 *ref_int_x = value__217.index
                            var t4757 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4756)
                            var t4758 int = t4757 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4755, t4758)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop4661
            }
        }
        var t4659 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t4660 Result__string__string = Result__string__string_Err{
            _0: t4659,
        }
        return t4660
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t4788 *ref_int_x = value__225.index
    var start__226 int
    var inline10360 int = ref_get__Ref_3int(t4788)
    start__226 = inline10360
    Loop_loop4793:
    for {
        var t4801 *ref_int_x = value__225.index
        var t4802 int
        var inline10356 int = ref_get__Ref_3int(t4801)
        t4802 = inline10356
        var t4803 string = value__225.input
        var t4804 int
        var inline10354 int = _goml_runtime_core_string_len(t4803)
        t4804 = inline10354
        var t4805 bool = t4802 < t4804
        var jp4795 bool
        if t4805 {
            var t4806 string = value__225.input
            var t4807 *ref_int_x = value__225.index
            var t4808 int
            var inline10348 int = ref_get__Ref_3int(t4807)
            t4808 = inline10348
            var t4809 uint8
            var inline10346 uint8 = _goml_runtime_core_string_byte_get(t4806, t4808)
            t4809 = inline10346
            var inline10343 bool = t4809 >= 48
            if inline10343 {
                var inline10344 bool = t4809 <= 57
                jp4795 = inline10344
            } else {
                jp4795 = false
            }
        } else {
            jp4795 = false
        }
        if jp4795 {
            var t4796 *ref_int_x = value__225.index
            var t4797 *ref_int_x = value__225.index
            var t4798 int
            var inline10352 int = ref_get__Ref_3int(t4797)
            t4798 = inline10352
            var t4799 int = t4798 + 1
            ref_set__Ref_3int(t4796, t4799)
            continue
        } else {
            break Loop_loop4793
        }
    }
    var t4790 *ref_int_x = value__225.index
    var t4791 int
    var inline10358 int = ref_get__Ref_3int(t4790)
    t4791 = inline10358
    var t4792 bool = t4791 > start__226
    return t4792
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4813 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4813)
    var t4934 string = value__227.input
    var t4935 *ref_int_x = value__227.index
    var t4936 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4935)
    var t4937 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4934, t4936)
    var t4938 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4937, 45)
    if t4938 {
        var t4939 *ref_int_x = value__227.index
        var t4940 *ref_int_x = value__227.index
        var t4941 int
        var inline10364 int = ref_get__Ref_3int(t4940)
        t4941 = inline10364
        var t4942 int = t4941 + 1
        ref_set__Ref_3int(t4939, t4942)
    } else {}
    var t4897 *ref_int_x = value__227.index
    var t4898 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4897)
    var t4899 string = value__227.input
    var t4900 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4899)
    var t4901 bool = t4898 >= t4900
    if t4901 {
        var t4902 string
        var inline10366 string = "incomplete number"
        var inline10367 string = "" + inline10366
        var inline10368 string = inline10367 + " at byte "
        var inline10369 *ref_int_x = value__227.index
        var inline10370 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10369)
        var inline10371 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10370)
        var inline10372 string = inline10368 + inline10371
        t4902 = inline10372
        var t4903 Result__string__string = Result__string__string_Err{
            _0: t4902,
        }
        return t4903
    } else {
        var t4905 string = value__227.input
        var t4906 *ref_int_x = value__227.index
        var t4907 int
        var inline10410 int = ref_get__Ref_3int(t4906)
        t4907 = inline10410
        var t4908 uint8
        var inline10408 uint8 = _goml_runtime_core_string_byte_get(t4905, t4907)
        t4908 = inline10408
        var t4909 bool
        var inline10405 uint8 = 48
        var inline10406 bool = t4908 == inline10405
        t4909 = inline10406
        if t4909 {
            var t4910 *ref_int_x = value__227.index
            var t4911 *ref_int_x = value__227.index
            var t4912 int
            var inline10395 int = ref_get__Ref_3int(t4911)
            t4912 = inline10395
            var t4913 int = t4912 + 1
            ref_set__Ref_3int(t4910, t4913)
            var t4919 *ref_int_x = value__227.index
            var t4920 int
            var inline10391 int = ref_get__Ref_3int(t4919)
            t4920 = inline10391
            var t4921 string = value__227.input
            var t4922 int
            var inline10389 int = _goml_runtime_core_string_len(t4921)
            t4922 = inline10389
            var t4923 bool = t4920 < t4922
            var jp4916 bool
            if t4923 {
                var t4924 string = value__227.input
                var t4925 *ref_int_x = value__227.index
                var t4926 int
                var inline10379 int = ref_get__Ref_3int(t4925)
                t4926 = inline10379
                var t4927 uint8
                var inline10377 uint8 = _goml_runtime_core_string_byte_get(t4924, t4926)
                t4927 = inline10377
                var inline10374 bool = t4927 >= 48
                if inline10374 {
                    var inline10375 bool = t4927 <= 57
                    jp4916 = inline10375
                } else {
                    jp4916 = false
                }
            } else {
                jp4916 = false
            }
            if jp4916 {
                var t4917 string
                var inline10381 string = "invalid leading zero"
                var inline10382 string = "" + inline10381
                var inline10383 string = inline10382 + " at byte "
                var inline10384 *ref_int_x = value__227.index
                var inline10385 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10384)
                var inline10386 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10385)
                var inline10387 string = inline10383 + inline10386
                t4917 = inline10387
                var t4918 Result__string__string = Result__string__string_Err{
                    _0: t4917,
                }
                return t4918
            } else {
                var t4887 *ref_int_x = value__227.index
                var t4888 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4887)
                var t4889 string = value__227.input
                var t4890 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4889)
                var t4891 bool = t4888 < t4890
                var jp4877 bool
                if t4891 {
                    var t4892 string = value__227.input
                    var t4893 *ref_int_x = value__227.index
                    var t4894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4893)
                    var t4895 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4892, t4894)
                    var t4896 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4895, 46)
                    jp4877 = t4896
                } else {
                    jp4877 = false
                }
                if jp4877 {
                    var t4878 *ref_int_x = value__227.index
                    var t4879 *ref_int_x = value__227.index
                    var t4880 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4879)
                    var t4881 int = t4880 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4878, t4881)
                    var t4883 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t4884 bool = !t4883
                    if t4884 {
                        var t4885 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t4886 Result__string__string = Result__string__string_Err{
                            _0: t4885,
                        }
                        return t4886
                    } else {
                        var t4859 *ref_int_x = value__227.index
                        var t4860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4859)
                        var t4861 string = value__227.input
                        var t4862 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4861)
                        var t4863 bool = t4860 < t4862
                        var jp4824 bool
                        if t4863 {
                            var t4866 string = value__227.input
                            var t4867 *ref_int_x = value__227.index
                            var t4868 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4867)
                            var t4869 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4866, t4868)
                            var t4870 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4869, 101)
                            if t4870 {
                                jp4824 = true
                            } else {
                                var t4871 string = value__227.input
                                var t4872 *ref_int_x = value__227.index
                                var t4873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4872)
                                var t4874 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4871, t4873)
                                var t4875 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4874, 69)
                                jp4824 = t4875
                            }
                        } else {
                            jp4824 = false
                        }
                        if jp4824 {
                            var t4825 *ref_int_x = value__227.index
                            var t4826 *ref_int_x = value__227.index
                            var t4827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4826)
                            var t4828 int = t4827 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4825, t4828)
                            var t4842 *ref_int_x = value__227.index
                            var t4843 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4842)
                            var t4844 string = value__227.input
                            var t4845 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4844)
                            var t4846 bool = t4843 < t4845
                            var jp4836 bool
                            if t4846 {
                                var t4849 string = value__227.input
                                var t4850 *ref_int_x = value__227.index
                                var t4851 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4850)
                                var t4852 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4849, t4851)
                                var t4853 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4852, 43)
                                if t4853 {
                                    jp4836 = true
                                } else {
                                    var t4854 string = value__227.input
                                    var t4855 *ref_int_x = value__227.index
                                    var t4856 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4855)
                                    var t4857 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4854, t4856)
                                    var t4858 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4857, 45)
                                    jp4836 = t4858
                                }
                            } else {
                                jp4836 = false
                            }
                            if jp4836 {
                                var t4837 *ref_int_x = value__227.index
                                var t4838 *ref_int_x = value__227.index
                                var t4839 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4838)
                                var t4840 int = t4839 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4837, t4840)
                            } else {}
                            var t4831 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4832 bool = !t4831
                            if t4832 {
                                var t4833 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4834 Result__string__string = Result__string__string_Err{
                                    _0: t4833,
                                }
                                return t4834
                            } else {
                                var t4818 string = value__227.input
                                var t4819 *ref_int_x = value__227.index
                                var t4820 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4819)
                                var t4821 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4818, start__228, t4820)
                                var t4822 Result__string__string = Result__string__string_Ok{
                                    _0: t4821,
                                }
                                return t4822
                            }
                        } else {
                            var t4818 string = value__227.input
                            var t4819 *ref_int_x = value__227.index
                            var t4820 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4819)
                            var t4821 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4818, start__228, t4820)
                            var t4822 Result__string__string = Result__string__string_Ok{
                                _0: t4821,
                            }
                            return t4822
                        }
                    }
                } else {
                    var t4859 *ref_int_x = value__227.index
                    var t4860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4859)
                    var t4861 string = value__227.input
                    var t4862 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4861)
                    var t4863 bool = t4860 < t4862
                    var jp4824 bool
                    if t4863 {
                        var t4866 string = value__227.input
                        var t4867 *ref_int_x = value__227.index
                        var t4868 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4867)
                        var t4869 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4866, t4868)
                        var t4870 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4869, 101)
                        if t4870 {
                            jp4824 = true
                        } else {
                            var t4871 string = value__227.input
                            var t4872 *ref_int_x = value__227.index
                            var t4873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4872)
                            var t4874 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4871, t4873)
                            var t4875 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4874, 69)
                            jp4824 = t4875
                        }
                    } else {
                        jp4824 = false
                    }
                    if jp4824 {
                        var t4825 *ref_int_x = value__227.index
                        var t4826 *ref_int_x = value__227.index
                        var t4827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4826)
                        var t4828 int = t4827 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4825, t4828)
                        var t4842 *ref_int_x = value__227.index
                        var t4843 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4842)
                        var t4844 string = value__227.input
                        var t4845 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4844)
                        var t4846 bool = t4843 < t4845
                        var jp4836 bool
                        if t4846 {
                            var t4849 string = value__227.input
                            var t4850 *ref_int_x = value__227.index
                            var t4851 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4850)
                            var t4852 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4849, t4851)
                            var t4853 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4852, 43)
                            if t4853 {
                                jp4836 = true
                            } else {
                                var t4854 string = value__227.input
                                var t4855 *ref_int_x = value__227.index
                                var t4856 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4855)
                                var t4857 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4854, t4856)
                                var t4858 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4857, 45)
                                jp4836 = t4858
                            }
                        } else {
                            jp4836 = false
                        }
                        if jp4836 {
                            var t4837 *ref_int_x = value__227.index
                            var t4838 *ref_int_x = value__227.index
                            var t4839 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4838)
                            var t4840 int = t4839 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4837, t4840)
                        } else {}
                        var t4831 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4832 bool = !t4831
                        if t4832 {
                            var t4833 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4834 Result__string__string = Result__string__string_Err{
                                _0: t4833,
                            }
                            return t4834
                        } else {
                            var t4818 string = value__227.input
                            var t4819 *ref_int_x = value__227.index
                            var t4820 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4819)
                            var t4821 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4818, start__228, t4820)
                            var t4822 Result__string__string = Result__string__string_Ok{
                                _0: t4821,
                            }
                            return t4822
                        }
                    } else {
                        var t4818 string = value__227.input
                        var t4819 *ref_int_x = value__227.index
                        var t4820 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4819)
                        var t4821 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4818, start__228, t4820)
                        var t4822 Result__string__string = Result__string__string_Ok{
                            _0: t4821,
                        }
                        return t4822
                    }
                }
            }
        } else {
            var t4930 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t4931 bool = !t4930
            if t4931 {
                var t4932 string
                var inline10397 string = "expected number"
                var inline10398 string = "" + inline10397
                var inline10399 string = inline10398 + " at byte "
                var inline10400 *ref_int_x = value__227.index
                var inline10401 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10400)
                var inline10402 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10401)
                var inline10403 string = inline10399 + inline10402
                t4932 = inline10403
                var t4933 Result__string__string = Result__string__string_Err{
                    _0: t4932,
                }
                return t4933
            } else {
                var t4887 *ref_int_x = value__227.index
                var t4888 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4887)
                var t4889 string = value__227.input
                var t4890 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4889)
                var t4891 bool = t4888 < t4890
                var jp4877 bool
                if t4891 {
                    var t4892 string = value__227.input
                    var t4893 *ref_int_x = value__227.index
                    var t4894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4893)
                    var t4895 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4892, t4894)
                    var t4896 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4895, 46)
                    jp4877 = t4896
                } else {
                    jp4877 = false
                }
                if jp4877 {
                    var t4878 *ref_int_x = value__227.index
                    var t4879 *ref_int_x = value__227.index
                    var t4880 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4879)
                    var t4881 int = t4880 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4878, t4881)
                    var t4883 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t4884 bool = !t4883
                    if t4884 {
                        var t4885 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t4886 Result__string__string = Result__string__string_Err{
                            _0: t4885,
                        }
                        return t4886
                    } else {
                        var t4859 *ref_int_x = value__227.index
                        var t4860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4859)
                        var t4861 string = value__227.input
                        var t4862 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4861)
                        var t4863 bool = t4860 < t4862
                        var jp4824 bool
                        if t4863 {
                            var t4866 string = value__227.input
                            var t4867 *ref_int_x = value__227.index
                            var t4868 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4867)
                            var t4869 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4866, t4868)
                            var t4870 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4869, 101)
                            if t4870 {
                                jp4824 = true
                            } else {
                                var t4871 string = value__227.input
                                var t4872 *ref_int_x = value__227.index
                                var t4873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4872)
                                var t4874 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4871, t4873)
                                var t4875 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4874, 69)
                                jp4824 = t4875
                            }
                        } else {
                            jp4824 = false
                        }
                        if jp4824 {
                            var t4825 *ref_int_x = value__227.index
                            var t4826 *ref_int_x = value__227.index
                            var t4827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4826)
                            var t4828 int = t4827 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4825, t4828)
                            var t4842 *ref_int_x = value__227.index
                            var t4843 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4842)
                            var t4844 string = value__227.input
                            var t4845 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4844)
                            var t4846 bool = t4843 < t4845
                            var jp4836 bool
                            if t4846 {
                                var t4849 string = value__227.input
                                var t4850 *ref_int_x = value__227.index
                                var t4851 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4850)
                                var t4852 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4849, t4851)
                                var t4853 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4852, 43)
                                if t4853 {
                                    jp4836 = true
                                } else {
                                    var t4854 string = value__227.input
                                    var t4855 *ref_int_x = value__227.index
                                    var t4856 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4855)
                                    var t4857 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4854, t4856)
                                    var t4858 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4857, 45)
                                    jp4836 = t4858
                                }
                            } else {
                                jp4836 = false
                            }
                            if jp4836 {
                                var t4837 *ref_int_x = value__227.index
                                var t4838 *ref_int_x = value__227.index
                                var t4839 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4838)
                                var t4840 int = t4839 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4837, t4840)
                            } else {}
                            var t4831 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4832 bool = !t4831
                            if t4832 {
                                var t4833 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4834 Result__string__string = Result__string__string_Err{
                                    _0: t4833,
                                }
                                return t4834
                            } else {
                                var t4818 string = value__227.input
                                var t4819 *ref_int_x = value__227.index
                                var t4820 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4819)
                                var t4821 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4818, start__228, t4820)
                                var t4822 Result__string__string = Result__string__string_Ok{
                                    _0: t4821,
                                }
                                return t4822
                            }
                        } else {
                            var t4818 string = value__227.input
                            var t4819 *ref_int_x = value__227.index
                            var t4820 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4819)
                            var t4821 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4818, start__228, t4820)
                            var t4822 Result__string__string = Result__string__string_Ok{
                                _0: t4821,
                            }
                            return t4822
                        }
                    }
                } else {
                    var t4859 *ref_int_x = value__227.index
                    var t4860 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4859)
                    var t4861 string = value__227.input
                    var t4862 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4861)
                    var t4863 bool = t4860 < t4862
                    var jp4824 bool
                    if t4863 {
                        var t4866 string = value__227.input
                        var t4867 *ref_int_x = value__227.index
                        var t4868 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4867)
                        var t4869 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4866, t4868)
                        var t4870 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4869, 101)
                        if t4870 {
                            jp4824 = true
                        } else {
                            var t4871 string = value__227.input
                            var t4872 *ref_int_x = value__227.index
                            var t4873 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4872)
                            var t4874 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4871, t4873)
                            var t4875 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4874, 69)
                            jp4824 = t4875
                        }
                    } else {
                        jp4824 = false
                    }
                    if jp4824 {
                        var t4825 *ref_int_x = value__227.index
                        var t4826 *ref_int_x = value__227.index
                        var t4827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4826)
                        var t4828 int = t4827 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4825, t4828)
                        var t4842 *ref_int_x = value__227.index
                        var t4843 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4842)
                        var t4844 string = value__227.input
                        var t4845 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4844)
                        var t4846 bool = t4843 < t4845
                        var jp4836 bool
                        if t4846 {
                            var t4849 string = value__227.input
                            var t4850 *ref_int_x = value__227.index
                            var t4851 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4850)
                            var t4852 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4849, t4851)
                            var t4853 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4852, 43)
                            if t4853 {
                                jp4836 = true
                            } else {
                                var t4854 string = value__227.input
                                var t4855 *ref_int_x = value__227.index
                                var t4856 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4855)
                                var t4857 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4854, t4856)
                                var t4858 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t4857, 45)
                                jp4836 = t4858
                            }
                        } else {
                            jp4836 = false
                        }
                        if jp4836 {
                            var t4837 *ref_int_x = value__227.index
                            var t4838 *ref_int_x = value__227.index
                            var t4839 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4838)
                            var t4840 int = t4839 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4837, t4840)
                        } else {}
                        var t4831 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4832 bool = !t4831
                        if t4832 {
                            var t4833 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4834 Result__string__string = Result__string__string_Err{
                                _0: t4833,
                            }
                            return t4834
                        } else {
                            var t4818 string = value__227.input
                            var t4819 *ref_int_x = value__227.index
                            var t4820 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4819)
                            var t4821 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4818, start__228, t4820)
                            var t4822 Result__string__string = Result__string__string_Ok{
                                _0: t4821,
                            }
                            return t4822
                        }
                    } else {
                        var t4818 string = value__227.input
                        var t4819 *ref_int_x = value__227.index
                        var t4820 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4819)
                        var t4821 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4818, start__228, t4820)
                        var t4822 Result__string__string = Result__string__string_Ok{
                            _0: t4821,
                        }
                        return t4822
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t4965 *ref_int_x = value__230.index
    var t4966 int
    var inline10440 int = ref_get__Ref_3int(t4965)
    t4966 = inline10440
    var t4967 int
    var inline10438 int = _goml_runtime_core_string_len(expected__231)
    t4967 = inline10438
    var t4968 int = t4966 + t4967
    var t4969 string = value__230.input
    var t4970 int
    var inline10436 int = _goml_runtime_core_string_len(t4969)
    t4970 = inline10436
    var t4971 bool = t4968 <= t4970
    var jp4956 bool
    if t4971 {
        var t4972 string = value__230.input
        var t4973 *ref_int_x = value__230.index
        var t4974 int
        var inline10420 int = ref_get__Ref_3int(t4973)
        t4974 = inline10420
        var t4975 *ref_int_x = value__230.index
        var t4976 int
        var inline10418 int = ref_get__Ref_3int(t4975)
        t4976 = inline10418
        var t4977 int
        var inline10416 int = _goml_runtime_core_string_len(expected__231)
        t4977 = inline10416
        var t4978 int = t4976 + t4977
        var t4979 string
        var inline10414 string = string_byte_slice(t4972, t4974, t4978)
        t4979 = inline10414
        var inline10412 bool = t4979 == expected__231
        jp4956 = inline10412
    } else {
        jp4956 = false
    }
    if jp4956 {
        var t4957 *ref_int_x = value__230.index
        var t4958 *ref_int_x = value__230.index
        var t4959 int
        var inline10426 int = ref_get__Ref_3int(t4958)
        t4959 = inline10426
        var t4960 int
        var inline10424 int = _goml_runtime_core_string_len(expected__231)
        t4960 = inline10424
        var t4961 int = t4959 + t4960
        ref_set__Ref_3int(t4957, t4961)
        var t4962 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__232,
        }
        return t4962
    } else {
        var t4963 string
        var inline10428 string = "invalid literal"
        var inline10429 string = "" + inline10428
        var inline10430 string = inline10429 + " at byte "
        var inline10431 *ref_int_x = value__230.index
        var inline10432 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10431)
        var inline10433 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10432)
        var inline10434 string = inline10430 + inline10433
        t4963 = inline10434
        var t4964 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4963,
        }
        return t4964
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t4983 *ref_int_x = value__233.index
    var t4984 *ref_int_x = value__233.index
    var t4985 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4984)
    var t4986 int = t4985 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4983, t4986)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var vec_literal__8825 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t5041 *ref_int_x = value__233.index
    var t5042 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5041)
    var t5043 string = value__233.input
    var t5044 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5043)
    var t5045 bool = t5042 < t5044
    var jp5034 bool
    if t5045 {
        var t5046 string = value__233.input
        var t5047 *ref_int_x = value__233.index
        var t5048 int
        var inline10447 int = ref_get__Ref_3int(t5047)
        t5048 = inline10447
        var t5049 uint8
        var inline10445 uint8 = _goml_runtime_core_string_byte_get(t5046, t5048)
        t5049 = inline10445
        var inline10442 uint8 = 93
        var inline10443 bool = t5049 == inline10442
        jp5034 = inline10443
    } else {
        jp5034 = false
    }
    if jp5034 {
        var t5035 *ref_int_x = value__233.index
        var t5036 *ref_int_x = value__233.index
        var t5037 int
        var inline10451 int = ref_get__Ref_3int(t5036)
        t5037 = inline10451
        var t5038 int = t5037 + 1
        ref_set__Ref_3int(t5035, t5038)
        var t5039 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: vec_literal__8825,
        }
        var t5040 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5039,
        }
        return t5040
    } else {
        Loop_loop4991:
        for {
            var t4992 *ref_int_x = value__233.index
            var t4993 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4992)
            var t4994 string = value__233.input
            var t4995 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4994)
            var t4996 bool = t4993 < t4995
            if t4996 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp4998 _goml_m_std_p_json_p_Value
                switch mtmp797.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp4998 = x798
                    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(vec_literal__8825, jp4998)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t5000 *ref_int_x = value__233.index
                    var t5001 int
                    var inline10493 int = ref_get__Ref_3int(t5000)
                    t5001 = inline10493
                    var t5002 string = value__233.input
                    var t5003 int
                    var inline10491 int = _goml_runtime_core_string_len(t5002)
                    t5003 = inline10491
                    var t5004 bool = t5001 >= t5003
                    if t5004 {
                        var t5005 string
                        var inline10453 string = "unterminated array"
                        var inline10454 string = "" + inline10453
                        var inline10455 string = inline10454 + " at byte "
                        var inline10456 *ref_int_x = value__233.index
                        var inline10457 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10456)
                        var inline10458 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10457)
                        var inline10459 string = inline10455 + inline10458
                        t5005 = inline10459
                        var t5006 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5005,
                        }
                        return t5006
                    } else {
                        var t5008 string = value__233.input
                        var t5009 *ref_int_x = value__233.index
                        var t5010 int
                        var inline10489 int = ref_get__Ref_3int(t5009)
                        t5010 = inline10489
                        var t5011 uint8
                        var inline10487 uint8 = _goml_runtime_core_string_byte_get(t5008, t5010)
                        t5011 = inline10487
                        var t5012 bool
                        var inline10484 uint8 = 93
                        var inline10485 bool = t5011 == inline10484
                        t5012 = inline10485
                        if t5012 {
                            var t5013 *ref_int_x = value__233.index
                            var t5014 *ref_int_x = value__233.index
                            var t5015 int
                            var inline10463 int = ref_get__Ref_3int(t5014)
                            t5015 = inline10463
                            var t5016 int = t5015 + 1
                            ref_set__Ref_3int(t5013, t5016)
                            var t5017 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: vec_literal__8825,
                            }
                            var t5018 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t5017,
                            }
                            return t5018
                        } else {
                            var t5020 string = value__233.input
                            var t5021 *ref_int_x = value__233.index
                            var t5022 int
                            var inline10482 int = ref_get__Ref_3int(t5021)
                            t5022 = inline10482
                            var t5023 uint8
                            var inline10480 uint8 = _goml_runtime_core_string_byte_get(t5020, t5022)
                            t5023 = inline10480
                            var t5024 bool
                            var inline10477 uint8 = 44
                            var inline10478 bool = t5023 == inline10477
                            t5024 = inline10478
                            if t5024 {
                                var t5025 *ref_int_x = value__233.index
                                var t5026 *ref_int_x = value__233.index
                                var t5027 int
                                var inline10467 int = ref_get__Ref_3int(t5026)
                                t5027 = inline10467
                                var t5028 int = t5027 + 1
                                ref_set__Ref_3int(t5025, t5028)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t5030 string
                                var inline10469 string = "expected array separator"
                                var inline10470 string = "" + inline10469
                                var inline10471 string = inline10470 + " at byte "
                                var inline10472 *ref_int_x = value__233.index
                                var inline10473 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10472)
                                var inline10474 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10473)
                                var inline10475 string = inline10471 + inline10474
                                t5030 = inline10475
                                var t5031 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5030,
                                }
                                return t5031
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x799 string = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t5032 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x799,
                    }
                    return t5032
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4991
            }
        }
        var t4989 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t4990 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4989,
        }
        return t4990
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5053 *ref_int_x = value__236.index
    var t5054 *ref_int_x = value__236.index
    var t5055 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5054)
    var t5056 int = t5055 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5053, t5056)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var vec_literal__10027 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t5136 *ref_int_x = value__236.index
    var t5137 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5136)
    var t5138 string = value__236.input
    var t5139 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5138)
    var t5140 bool = t5137 < t5139
    var jp5129 bool
    if t5140 {
        var t5141 string = value__236.input
        var t5142 *ref_int_x = value__236.index
        var t5143 int
        var inline10500 int = ref_get__Ref_3int(t5142)
        t5143 = inline10500
        var t5144 uint8
        var inline10498 uint8 = _goml_runtime_core_string_byte_get(t5141, t5143)
        t5144 = inline10498
        var inline10495 uint8 = 125
        var inline10496 bool = t5144 == inline10495
        jp5129 = inline10496
    } else {
        jp5129 = false
    }
    if jp5129 {
        var t5130 *ref_int_x = value__236.index
        var t5131 *ref_int_x = value__236.index
        var t5132 int
        var inline10504 int = ref_get__Ref_3int(t5131)
        t5132 = inline10504
        var t5133 int = t5132 + 1
        ref_set__Ref_3int(t5130, t5133)
        var t5134 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10027,
        }
        var t5135 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5134,
        }
        return t5135
    } else {
        Loop_loop5061:
        for {
            var t5062 *ref_int_x = value__236.index
            var t5063 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5062)
            var t5064 string = value__236.input
            var t5065 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5064)
            var t5066 bool = t5063 < t5065
            if t5066 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp5068 string
                switch mtmp809.(type) {
                case Result__string__string_Ok:
                    var x810 string = mtmp809.(Result__string__string_Ok)._0
                    jp5068 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t5116 *ref_int_x = value__236.index
                    var t5117 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5116)
                    var t5118 string = value__236.input
                    var t5119 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5118)
                    var t5120 bool = t5117 >= t5119
                    var jp5108 bool
                    if t5120 {
                        jp5108 = true
                    } else {
                        var t5121 string = value__236.input
                        var t5122 *ref_int_x = value__236.index
                        var t5123 int
                        var inline10511 int = ref_get__Ref_3int(t5122)
                        t5123 = inline10511
                        var t5124 uint8
                        var inline10509 uint8 = _goml_runtime_core_string_byte_get(t5121, t5123)
                        t5124 = inline10509
                        var t5125 bool
                        var inline10506 uint8 = 58
                        var inline10507 bool = t5124 == inline10506
                        t5125 = inline10507
                        var t5126 bool = !t5125
                        jp5108 = t5126
                    }
                    if jp5108 {
                        var t5109 string
                        var inline10513 string = "expected object colon"
                        var inline10514 string = "" + inline10513
                        var inline10515 string = inline10514 + " at byte "
                        var inline10516 *ref_int_x = value__236.index
                        var inline10517 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10516)
                        var inline10518 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10517)
                        var inline10519 string = inline10515 + inline10518
                        t5109 = inline10519
                        var t5110 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5109,
                        }
                        return t5110
                    } else {
                        var t5111 *ref_int_x = value__236.index
                        var t5112 *ref_int_x = value__236.index
                        var t5113 int
                        var inline10523 int = ref_get__Ref_3int(t5112)
                        t5113 = inline10523
                        var t5114 int = t5113 + 1
                        ref_set__Ref_3int(t5111, t5114)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp5071 _goml_m_std_p_json_p_Value
                        switch mtmp815.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp5071 = x816
                            var t5072 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp5068,
                                _1: jp5071,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10027, t5072)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t5074 *ref_int_x = value__236.index
                            var t5075 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5074)
                            var t5076 string = value__236.input
                            var t5077 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5076)
                            var t5078 bool = t5075 >= t5077
                            if t5078 {
                                var t5079 string
                                var inline10525 string = "unterminated object"
                                var inline10526 string = "" + inline10525
                                var inline10527 string = inline10526 + " at byte "
                                var inline10528 *ref_int_x = value__236.index
                                var inline10529 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10528)
                                var inline10530 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10529)
                                var inline10531 string = inline10527 + inline10530
                                t5079 = inline10531
                                var t5080 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5079,
                                }
                                return t5080
                            } else {
                                var t5082 string = value__236.input
                                var t5083 *ref_int_x = value__236.index
                                var t5084 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5083)
                                var t5085 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5082, t5084)
                                var t5086 bool = _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(t5085, 125)
                                if t5086 {
                                    var t5087 *ref_int_x = value__236.index
                                    var t5088 *ref_int_x = value__236.index
                                    var t5089 int
                                    var inline10535 int = ref_get__Ref_3int(t5088)
                                    t5089 = inline10535
                                    var t5090 int = t5089 + 1
                                    ref_set__Ref_3int(t5087, t5090)
                                    var t5091 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10027,
                                    }
                                    var t5092 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t5091,
                                    }
                                    return t5092
                                } else {
                                    var t5094 string = value__236.input
                                    var t5095 *ref_int_x = value__236.index
                                    var t5096 int
                                    var inline10546 int = ref_get__Ref_3int(t5095)
                                    t5096 = inline10546
                                    var t5097 uint8
                                    var inline10544 uint8 = _goml_runtime_core_string_byte_get(t5094, t5096)
                                    t5097 = inline10544
                                    var t5098 bool
                                    var inline10541 uint8 = 44
                                    var inline10542 bool = t5097 == inline10541
                                    t5098 = inline10542
                                    if t5098 {
                                        var t5099 *ref_int_x = value__236.index
                                        var t5100 *ref_int_x = value__236.index
                                        var t5101 int
                                        var inline10539 int = ref_get__Ref_3int(t5100)
                                        t5101 = inline10539
                                        var t5102 int = t5101 + 1
                                        ref_set__Ref_3int(t5099, t5102)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5104 string = _goml_m_std_p_json_p_json__error(value__236, "expected object separator")
                                        var t5105 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t5104,
                                        }
                                        return t5105
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x817 string = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t5106 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x817,
                            }
                            return t5106
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x811 string = mtmp809.(Result__string__string_Err)._0
                    var t5127 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x811,
                    }
                    return t5127
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5061
            }
        }
        var t5059 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t5060 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5059,
        }
        return t5060
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t5150 *ref_int_x = value__240.index
    var t5151 int
    var inline10587 int = ref_get__Ref_3int(t5150)
    t5151 = inline10587
    var t5152 string = value__240.input
    var t5153 int
    var inline10585 int = _goml_runtime_core_string_len(t5152)
    t5153 = inline10585
    var t5154 bool = t5151 >= t5153
    if t5154 {
        var t5155 string
        var inline10548 string = "expected JSON value"
        var inline10549 string = "" + inline10548
        var inline10550 string = inline10549 + " at byte "
        var inline10551 *ref_int_x = value__240.index
        var inline10552 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10551)
        var inline10553 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10552)
        var inline10554 string = inline10550 + inline10553
        t5155 = inline10554
        var t5156 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5155,
        }
        return t5156
    } else {
        var t5157 string = value__240.input
        var t5158 *ref_int_x = value__240.index
        var t5159 int
        var inline10583 int = ref_get__Ref_3int(t5158)
        t5159 = inline10583
        var mtmp824 uint8
        var inline10581 uint8 = _goml_runtime_core_string_byte_get(t5157, t5159)
        mtmp824 = inline10581
        switch mtmp824 {
        case 123:
            var t5162 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t5162
        case 91:
            var t5163 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t5163
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825.(type) {
            case Result__string__string_Ok:
                var x826 string = mtmp825.(Result__string__string_Ok)._0
                var t5166 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t5167 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t5166,
                }
                return t5167
            case Result__string__string_Err:
                var x827 string = mtmp825.(Result__string__string_Err)._0
                var t5168 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x827,
                }
                return t5168
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t5169 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t5170 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t5169)
            return t5170
        case 102:
            var t5171 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t5172 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t5171)
            return t5172
        case 110:
            var t5173 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t5173
        default:
            var t5181 bool
            var inline10578 uint8 = 45
            var inline10579 bool = mtmp824 == inline10578
            t5181 = inline10579
            var jp5177 bool
            if t5181 {
                jp5177 = true
            } else {
                var inline10556 bool = mtmp824 >= 48
                if inline10556 {
                    var inline10557 bool = mtmp824 <= 57
                    jp5177 = inline10557
                } else {
                    jp5177 = false
                }
            }
            if jp5177 {
                var inline10559 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline10561 string
                switch inline10559.(type) {
                case Result__string__string_Ok:
                    var inline10564 string = inline10559.(Result__string__string_Ok)._0
                    inline10561 = inline10564
                    var inline10562 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline10561,
                    }
                    var inline10563 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                        _0: inline10562,
                    }
                    return inline10563
                case Result__string__string_Err:
                    var inline10566 string = inline10559.(Result__string__string_Err)._0
                    var inline10568 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: inline10566,
                    }
                    return inline10568
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5179 string
                var inline10570 string = "unexpected JSON token"
                var inline10571 string = "" + inline10570
                var inline10572 string = inline10571 + " at byte "
                var inline10573 *ref_int_x = value__240.index
                var inline10574 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10573)
                var inline10575 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10574)
                var inline10576 string = inline10572 + inline10575
                t5179 = inline10576
                var t5180 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t5179,
                }
                return t5180
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline10603 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline10604 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline10603,
    }
    parser__245 = inline10604
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5186 _goml_m_std_p_json_p_Value
    switch mtmp828.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x829 _goml_m_std_p_json_p_Value = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp5186 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5189 *ref_int_x = parser__245.index
        var t5190 int
        var inline10601 int = ref_get__Ref_3int(t5189)
        t5190 = inline10601
        var t5191 int
        var inline10599 int = _goml_runtime_core_string_len(input__244)
        t5191 = inline10599
        var t5192 bool
        var inline10597 bool = t5190 == t5191
        t5192 = inline10597
        if t5192 {
            var t5193 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp5186,
            }
            return t5193
        } else {
            var t5194 string
            var inline10589 string = "trailing JSON data"
            var inline10590 string = "" + inline10589
            var inline10591 string = inline10590 + " at byte "
            var inline10592 *ref_int_x = parser__245.index
            var inline10593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10592)
            var inline10594 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10593)
            var inline10595 string = inline10591 + inline10594
            t5194 = inline10595
            var t5195 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t5194,
            }
            return t5195
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x830 string = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t5196 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x830,
        }
        return t5196
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__248, 34)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__249)
    Loop_loop5210:
    for {
        var t5211 bool = for_index833 < for_limit834
        if t5211 {
            var for_item835 int = for_index833
            var t5212 int = for_index833 + 1
            for_index833 = t5212
            var byte__252 uint8
            var inline10665 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline10665
            var t5265 bool
            var inline10662 uint8 = 34
            var inline10663 bool = byte__252 == inline10662
            t5265 = inline10663
            var jp5263 bool
            if t5265 {
                jp5263 = true
            } else {
                var inline10609 uint8 = 92
                var inline10610 bool = byte__252 == inline10609
                jp5263 = inline10610
            }
            var jp5260 bool
            if jp5263 {
                jp5260 = true
            } else {
                var inline10612 uint8 = 8
                var inline10613 bool = byte__252 == inline10612
                jp5260 = inline10613
            }
            var jp5257 bool
            if jp5260 {
                jp5257 = true
            } else {
                var inline10615 uint8 = 9
                var inline10616 bool = byte__252 == inline10615
                jp5257 = inline10616
            }
            var jp5254 bool
            if jp5257 {
                jp5254 = true
            } else {
                var inline10618 uint8 = 10
                var inline10619 bool = byte__252 == inline10618
                jp5254 = inline10619
            }
            var jp5251 bool
            if jp5254 {
                jp5251 = true
            } else {
                var inline10621 uint8 = 12
                var inline10622 bool = byte__252 == inline10621
                jp5251 = inline10622
            }
            var jp5248 bool
            if jp5251 {
                jp5248 = true
            } else {
                var inline10624 uint8 = 13
                var inline10625 bool = byte__252 == inline10624
                jp5248 = inline10625
            }
            var jp5215 bool
            if jp5248 {
                jp5215 = true
            } else {
                var t5249 bool = byte__252 < 32
                jp5215 = t5249
            }
            if jp5215 {
                var t5244 bool = start__250 < for_item835
                if t5244 {
                    var t5245 string
                    var inline10627 string = string_byte_slice(value__249, start__250, for_item835)
                    t5245 = inline10627
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5245)
                } else {}
                var t5219 bool
                var inline10659 uint8 = 34
                var inline10660 bool = byte__252 == inline10659
                t5219 = inline10660
                if t5219 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t5222 bool
                    var inline10656 uint8 = 92
                    var inline10657 bool = byte__252 == inline10656
                    t5222 = inline10657
                    if t5222 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t5225 bool
                        var inline10653 uint8 = 8
                        var inline10654 bool = byte__252 == inline10653
                        t5225 = inline10654
                        if t5225 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t5228 bool
                            var inline10650 uint8 = 9
                            var inline10651 bool = byte__252 == inline10650
                            t5228 = inline10651
                            if t5228 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t5231 bool
                                var inline10647 uint8 = 10
                                var inline10648 bool = byte__252 == inline10647
                                t5231 = inline10648
                                if t5231 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t5234 bool
                                    var inline10644 uint8 = 12
                                    var inline10645 bool = byte__252 == inline10644
                                    t5234 = inline10645
                                    if t5234 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t5237 bool
                                        var inline10641 uint8 = 13
                                        var inline10642 bool = byte__252 == inline10641
                                        t5237 = inline10642
                                        if t5237 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t5239 uint8 = byte__252 / 16
                                            var t5240 rune
                                            var inline10638 int = int(uint8(t5239))
                                            var inline10639 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10638)
                                            t5240 = inline10639
                                            var inline10635 string = _goml_m_inherent_i_char_i_char_i_to__string(t5240)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10635)
                                            var t5241_rhs uint8 = 16
                                            var t5241 uint8 = byte__252 % t5241_rhs
                                            var t5242 rune
                                            var inline10632 int = int(uint8(t5241))
                                            var inline10633 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10632)
                                            t5242 = inline10633
                                            var inline10629 string = _goml_m_inherent_i_char_i_char_i_to__string(t5242)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10629)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t5218 int = for_item835 + 1
                start__250 = t5218
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop5210
        }
    }
    var t5205 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__249)
    var t5206 bool = start__250 < t5205
    if t5206 {
        var t5207 int
        var inline10669 int = _goml_runtime_core_string_len(value__249)
        t5207 = inline10669
        var t5208 string
        var inline10667 string = string_byte_slice(value__249, start__250, t5207)
        t5208 = inline10667
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5208)
    } else {}
    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__248, 34)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline10683 rune = 123
        var inline10684 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10683)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10684)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop5271:
        for {
            var t5272 bool = for_index852 < for_limit851
            if t5272 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t5273 int = for_index852 + 1
                for_index852 = t5273
                var t5279 bool = index__256 > 0
                if t5279 {
                    var inline10671 rune = 44
                    var inline10672 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10671)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10672)
                } else {}
                var t5275 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t5275)
                var inline10675 rune = 58
                var inline10676 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10675)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10676)
                var t5276 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t5276)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t5277 int = compound_old859 + compound_value860
                index__256 = t5277
                continue
            } else {
                break Loop_loop5271
            }
        }
        var inline10679 rune = 125
        var inline10680 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10679)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10680)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline10695 rune = 91
        var inline10696 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10695)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10696)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop5283:
        for {
            var t5284 bool = for_index866 < for_limit865
            if t5284 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t5285 int = for_index866 + 1
                for_index866 = t5285
                var t5289 bool = index__259 > 0
                if t5289 {
                    var inline10687 rune = 44
                    var inline10688 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10687)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10688)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t5287 int = compound_old871 + compound_value872
                index__259 = t5287
                continue
            } else {
                break Loop_loop5283
            }
        }
        var inline10691 rune = 93
        var inline10692 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10691)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10692)
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
        var jp5294 string
        if x848 {
            jp5294 = "true"
        } else {
            jp5294 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp5294)
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
    var inline10705 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline10706 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline10705,
    }
    builder__265 = inline10706
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline10699 *_goml_vec_uint8 = builder__265.values
    var inline10700 Tuple2_4bool_6string = string_from_utf8(inline10699)
    var inline10702 string = inline10700._1
    return inline10702
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop5305:
        for {
            var t5306 bool = for_index883 < for_limit882
            if t5306 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t5307 int = for_index883 + 1
                for_index883 = t5307
                var t5309 string = for_item884._0
                var t5310 bool
                var inline10708 bool = t5309 == name__267
                t5310 = inline10708
                if t5310 {
                    var t5311 _goml_m_std_p_json_p_Value = for_item884._1
                    var t5312 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t5311,
                    }
                    return t5312
                } else {
                    continue
                }
            } else {
                break Loop_loop5305
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__int {
    var t5322 int
    var inline10727 int = _goml_runtime_core_string_len(value__272)
    t5322 = inline10727
    var t5323 bool
    var inline10724 int = 0
    var inline10725 bool = t5322 == inline10724
    t5323 = inline10725
    if t5323 {
        return Option__int_None{}
    } else {
        var t5324 uint8
        var inline10721 int = 0
        var inline10722 uint8 = _goml_runtime_core_string_byte_get(value__272, inline10721)
        t5324 = inline10722
        var negative__273 bool
        var inline10718 uint8 = 45
        var inline10719 bool = t5324 == inline10718
        negative__273 = inline10719
        var jp5326 int
        if negative__273 {
            jp5326 = 1
        } else {
            jp5326 = 0
        }
        var index__274 int = jp5326
        var result__275 int = 0
        var t5347 int
        var inline10716 int = _goml_runtime_core_string_len(value__272)
        t5347 = inline10716
        var t5348 bool
        var inline10714 bool = index__274 == t5347
        t5348 = inline10714
        if t5348 {
            return Option__int_None{}
        } else {
            Loop_loop5333:
            for {
                var t5334 int
                var inline10712 int = _goml_runtime_core_string_len(value__272)
                t5334 = inline10712
                var t5335 bool = index__274 < t5334
                if t5335 {
                    var byte__276 uint8
                    var inline10710 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline10710
                    var t5345 bool = byte__276 < 48
                    var jp5340 bool
                    if t5345 {
                        jp5340 = true
                    } else {
                        var t5346 bool = byte__276 > 57
                        jp5340 = t5346
                    }
                    if jp5340 {
                        return Option__int_None{}
                    } else {
                        var t5341 int = result__275 * 10
                        var t5342 uint8 = byte__276 - 48
                        var t5343 int = int(uint8(t5342))
                        var t5344 int = t5341 + t5343
                        result__275 = t5344
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t5337 int = compound_old895 + compound_value896
                        index__274 = t5337
                        continue
                    }
                } else {
                    break Loop_loop5333
                }
            }
            var jp5330 int
            if negative__273 {
                var t5332 int = 0 - result__275
                jp5330 = t5332
            } else {
                jp5330 = result__275
            }
            var t5331 Option__int = Option__int_Some{
                _0: jp5330,
            }
            return t5331
        }
    }
}

func main0() struct{} {
    var mtmp172 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp6133 _goml_m_std_p_json_p_Value
    switch mtmp172.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x173 _goml_m_std_p_json_p_Value = mtmp172.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp6133 = x173
        var mtmp176 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6133, "name")
        switch mtmp176.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline11244 string = "missing name"
            var inline11245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11244)
            _goml_runtime_core_string_println(inline11245)
            var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6133, "version")
            switch mtmp181.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline11259 string = "missing version"
                var inline11260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11259)
                _goml_runtime_core_string_println(inline11260)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp183 Option__int
                switch x182.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline11270 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline11272 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11270)
                    mtmp183 = inline11272
                default:
                    mtmp183 = Option__int_None{}
                }
                switch mtmp183.(type) {
                case Option__int_None:
                    var inline11263 string = "invalid version"
                    var inline11264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11263)
                    _goml_runtime_core_string_println(inline11264)
                case Option__int_Some:
                    var x184 int = mtmp183.(Option__int_Some)._0
                    var inline11267 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                    _goml_runtime_core_string_println(inline11267)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6133, "stable")
            switch mtmp186.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline11274 string = "missing stable"
                var inline11275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11274)
                _goml_runtime_core_string_println(inline11275)
                var t6137 string = _goml_m_std_p_json_p_encode(jp6133)
                println__T_string(t6137)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field11590 bool
                switch x187.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline11285 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field11590 = inline11285
                    var inline11282 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11590)
                    _goml_runtime_core_string_println(inline11282)
                    var t6137 string = _goml_m_std_p_json_p_encode(jp6133)
                    println__T_string(t6137)
                    return struct{}{}
                default:
                    var inline11278 string = "invalid stable"
                    var inline11279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11278)
                    _goml_runtime_core_string_println(inline11279)
                    var t6137 string = _goml_m_std_p_json_p_encode(jp6133)
                    println__T_string(t6137)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x177 _goml_m_std_p_json_p_Value = mtmp176.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field11596 string
            switch x177.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline11255 string = x177.(_goml_m_std_p_json_p_Value_String)._0
                commute_field11596 = inline11255
                var inline11252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field11596)
                _goml_runtime_core_string_println(inline11252)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6133, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11259 string = "missing version"
                    var inline11260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11259)
                    _goml_runtime_core_string_println(inline11260)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline11270 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline11272 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11270)
                        mtmp183 = inline11272
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline11263 string = "invalid version"
                        var inline11264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11263)
                        _goml_runtime_core_string_println(inline11264)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline11267 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline11267)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6133, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11274 string = "missing stable"
                    var inline11275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11274)
                    _goml_runtime_core_string_println(inline11275)
                    var t6137 string = _goml_m_std_p_json_p_encode(jp6133)
                    println__T_string(t6137)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11590 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline11285 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11590 = inline11285
                        var inline11282 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11590)
                        _goml_runtime_core_string_println(inline11282)
                        var t6137 string = _goml_m_std_p_json_p_encode(jp6133)
                        println__T_string(t6137)
                        return struct{}{}
                    default:
                        var inline11278 string = "invalid stable"
                        var inline11279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11278)
                        _goml_runtime_core_string_println(inline11279)
                        var t6137 string = _goml_m_std_p_json_p_encode(jp6133)
                        println__T_string(t6137)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline11248 string = "invalid name"
                var inline11249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11248)
                _goml_runtime_core_string_println(inline11249)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6133, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11259 string = "missing version"
                    var inline11260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11259)
                    _goml_runtime_core_string_println(inline11260)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline11270 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline11272 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline11270)
                        mtmp183 = inline11272
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline11263 string = "invalid version"
                        var inline11264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11263)
                        _goml_runtime_core_string_println(inline11264)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline11267 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline11267)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6133, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline11274 string = "missing stable"
                    var inline11275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11274)
                    _goml_runtime_core_string_println(inline11275)
                    var t6137 string = _goml_m_std_p_json_p_encode(jp6133)
                    println__T_string(t6137)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field11590 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline11285 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field11590 = inline11285
                        var inline11282 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field11590)
                        _goml_runtime_core_string_println(inline11282)
                        var t6137 string = _goml_m_std_p_json_p_encode(jp6133)
                        println__T_string(t6137)
                        return struct{}{}
                    default:
                        var inline11278 string = "invalid stable"
                        var inline11279 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline11278)
                        _goml_runtime_core_string_println(inline11279)
                        var t6137 string = _goml_m_std_p_json_p_encode(jp6133)
                        println__T_string(t6137)
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
        var inline11241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x174)
        _goml_runtime_core_string_println(inline11241)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t6153 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t6153
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop6193:
    for {
        var t6194 int
        var inline11302 int = _goml_runtime_core_string_len(x12)
        t6194 = inline11302
        var t6195 bool = index__26 < t6194
        if t6195 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t6197 int = compound_old17 + x16
                index__26 = t6197
                continue
            } else {
                var t6199 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6199
            }
        } else {
            break Loop_loop6193
        }
    }
    var t6192 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t6192
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t6241 string = _goml_runtime_core_int_to_string(self__34)
    return t6241
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline11318 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline11319 bool = inline11318._0
    var inline11320 rune = inline11318._1
    if inline11319 {
        return inline11320
    } else {
        var inline11324 rune = _goml_runtime_core_string_get("", -1)
        return inline11324
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t6326 *ref_int_x = ref__Ref_3int(value__257)
    return t6326
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t6329 int = ref_get__Ref_3int(self__258)
    return t6329
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__259 *ref_int_x, value__260 int) struct{} {
    ref_set__Ref_3int(self__259, value__260)
    return struct{}{}
}

func _goml_m_trait__impl_i_PartialEq_i_uint8_i_eq(self__113 uint8, other__114 uint8) bool {
    var t6391 bool = self__113 == other__114
    return t6391
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline11338 uint32 = uint32(rune(self__36))
    var inline11339 bool = utf8_valid_scalar(inline11338)
    if inline11339 {
        var inline11340 string = _goml_runtime_core_char_to_string(self__36)
        return inline11340
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t6397 int = _goml_runtime_core_string_len(self__38)
    return t6397
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t6400 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t6400
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline11343 bool = string_is_char_boundary(self__43, start__44)
    var inline11345 bool
    if inline11343 {
        var inline11348 bool = string_is_char_boundary(self__43, end__45)
        inline11345 = inline11348
    } else {
        inline11345 = false
    }
    if inline11345 {
        var inline11346 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline11346
    } else {
        var inline11347 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline11347
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t6506 bool
    var inline11402 bool = value__32 <= 1114111
    if inline11402 {
        var inline11403 bool = value__32 >= 55296
        var inline11405 bool
        if inline11403 {
            var inline11407 bool = value__32 <= 57343
            inline11405 = inline11407
        } else {
            inline11405 = false
        }
        var inline11406 bool = !inline11405
        t6506 = inline11406
    } else {
        t6506 = false
    }
    if t6506 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t6507 Option__char = Option__char_Some{
            _0: x24,
        }
        return t6507
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t6510 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t6510
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__std_p_json_p_Value(self__176 *_goml_vec__goml_m_std_p_json_p_Value, elem__177 _goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__16std_p_json_p_Value(self__176, elem__177)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t6515 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t6515
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__176 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__177 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__176, elem__177)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t6551 string
    t6551 = value__31
    _goml_runtime_core_string_println(t6551)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t6677 bool = index__6 < 0
    var jp6675 bool
    if t6677 {
        jp6675 = true
    } else {
        var t6678 bool = index__6 >= length__7
        jp6675 = t6678
    }
    if jp6675 {
        var inline11424 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11424
    } else {
        var t6562 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6562))
        var t6565 bool = first__8 < 128
        if t6565 {
            var inline11426 int = 1
            var inline11427 Option__char = char_from_uint32(first__8)
            switch inline11427.(type) {
            case Option__char_None:
                var inline11428 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline11428
            case Option__char_Some:
                var inline11429 rune = inline11427.(Option__char_Some)._0
                var inline11431 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline11429,
                    _2: inline11426,
                }
                return inline11431
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6569 bool = first__8 < 194
            if t6569 {
                var inline11433 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline11433
            } else {
                var t6573 bool = first__8 < 224
                if t6573 {
                    var t6586 int = length__7 - index__6
                    var t6587 bool = t6586 < 2
                    if t6587 {
                        var inline11435 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline11435
                    } else {
                        var t6575 int = index__6 + 1
                        var t6576 uint8
                        var inline11449 uint8 = _goml_runtime_core_string_byte_get(value__5, t6575)
                        t6576 = inline11449
                        var second__9 uint32 = uint32(uint8(t6576))
                        var t6579 bool
                        var inline11446 bool = second__9 < 128
                        if inline11446 {
                            t6579 = true
                        } else {
                            var inline11447 bool = second__9 > 191
                            t6579 = inline11447
                        }
                        if t6579 {
                            var inline11437 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11437
                        } else {
                            var t6581_rhs uint32 = 31
                            var t6581 uint32 = first__8 & t6581_rhs
                            var t6582_rhs int = 6
                            var t6582 uint32 = t6581 << t6582_rhs
                            var t6583_rhs uint32 = 63
                            var t6583 uint32 = second__9 & t6583_rhs
                            var t6584 uint32 = t6582 | t6583
                            var inline11439 int = 2
                            var inline11440 Option__char = char_from_uint32(t6584)
                            switch inline11440.(type) {
                            case Option__char_None:
                                var inline11441 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline11441
                            case Option__char_Some:
                                var inline11442 rune = inline11440.(Option__char_Some)._0
                                var inline11444 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline11442,
                                    _2: inline11439,
                                }
                                return inline11444
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6591 bool = first__8 < 240
                    if t6591 {
                        var t6624 int = length__7 - index__6
                        var t6625 bool = t6624 < 3
                        if t6625 {
                            var inline11451 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline11451
                        } else {
                            var t6593 int = index__6 + 1
                            var t6594 uint8
                            var inline11466 uint8 = _goml_runtime_core_string_byte_get(value__5, t6593)
                            t6594 = inline11466
                            var second__10 uint32 = uint32(uint8(t6594))
                            var t6595 int = index__6 + 2
                            var t6596 uint8
                            var inline11464 uint8 = _goml_runtime_core_string_byte_get(value__5, t6595)
                            t6596 = inline11464
                            var third__11 uint32 = uint32(uint8(t6596))
                            var t6622 bool = utf8_invalid_continuation(second__10)
                            var jp6617 bool
                            if t6622 {
                                jp6617 = true
                            } else {
                                var inline11453 bool = third__11 < 128
                                if inline11453 {
                                    jp6617 = true
                                } else {
                                    var inline11454 bool = third__11 > 191
                                    jp6617 = inline11454
                                }
                            }
                            var jp6611 bool
                            if jp6617 {
                                jp6611 = true
                            } else {
                                var t6620 bool
                                var inline11456 uint32 = 224
                                var inline11457 bool = first__8 == inline11456
                                t6620 = inline11457
                                if t6620 {
                                    var t6621 bool = second__10 < 160
                                    jp6611 = t6621
                                } else {
                                    jp6611 = false
                                }
                            }
                            var jp6600 bool
                            if jp6611 {
                                jp6600 = true
                            } else {
                                var t6614 bool
                                var inline11459 uint32 = 237
                                var inline11460 bool = first__8 == inline11459
                                t6614 = inline11460
                                if t6614 {
                                    var t6615 bool = second__10 >= 160
                                    jp6600 = t6615
                                } else {
                                    jp6600 = false
                                }
                            }
                            if jp6600 {
                                var inline11462 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline11462
                            } else {
                                var t6602_rhs uint32 = 15
                                var t6602 uint32 = first__8 & t6602_rhs
                                var t6603_rhs int = 12
                                var t6603 uint32 = t6602 << t6603_rhs
                                var t6604_rhs uint32 = 63
                                var t6604 uint32 = second__10 & t6604_rhs
                                var t6605_rhs int = 6
                                var t6605 uint32 = t6604 << t6605_rhs
                                var t6606 uint32 = t6603 | t6605
                                var t6607_rhs uint32 = 63
                                var t6607 uint32 = third__11 & t6607_rhs
                                var t6608 uint32 = t6606 | t6607
                                var t6609 Tuple3_4bool_4char_3int = utf8_valid_decode(t6608, 3)
                                return t6609
                            }
                        }
                    } else {
                        var t6629 bool = first__8 < 245
                        if t6629 {
                            var t6670 int = length__7 - index__6
                            var t6671 bool = t6670 < 4
                            if t6671 {
                                var t6672 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t6672
                            } else {
                                var t6631 int = index__6 + 1
                                var t6632 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6631)
                                var second__12 uint32 = uint32(uint8(t6632))
                                var t6633 int = index__6 + 2
                                var t6634 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6633)
                                var third__13 uint32 = uint32(uint8(t6634))
                                var t6635 int = index__6 + 3
                                var t6636 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6635)
                                var fourth__14 uint32 = uint32(uint8(t6636))
                                var t6668 bool = utf8_invalid_continuation(second__12)
                                var jp6666 bool
                                if t6668 {
                                    jp6666 = true
                                } else {
                                    var t6669 bool = utf8_invalid_continuation(third__13)
                                    jp6666 = t6669
                                }
                                var jp6660 bool
                                if jp6666 {
                                    jp6660 = true
                                } else {
                                    var t6667 bool = utf8_invalid_continuation(fourth__14)
                                    jp6660 = t6667
                                }
                                var jp6654 bool
                                if jp6660 {
                                    jp6654 = true
                                } else {
                                    var t6663 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t6663 {
                                        var t6664 bool = second__12 < 144
                                        jp6654 = t6664
                                    } else {
                                        jp6654 = false
                                    }
                                }
                                var jp6640 bool
                                if jp6654 {
                                    jp6640 = true
                                } else {
                                    var t6657 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t6657 {
                                        var t6658 bool = second__12 > 143
                                        jp6640 = t6658
                                    } else {
                                        jp6640 = false
                                    }
                                }
                                if jp6640 {
                                    var t6641 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6641
                                } else {
                                    var t6642_rhs uint32 = 7
                                    var t6642 uint32 = first__8 & t6642_rhs
                                    var t6643_rhs int = 18
                                    var t6643 uint32 = t6642 << t6643_rhs
                                    var t6644_rhs uint32 = 63
                                    var t6644 uint32 = second__12 & t6644_rhs
                                    var t6645_rhs int = 12
                                    var t6645 uint32 = t6644 << t6645_rhs
                                    var t6646 uint32 = t6643 | t6645
                                    var t6647_rhs uint32 = 63
                                    var t6647 uint32 = third__13 & t6647_rhs
                                    var t6648_rhs int = 6
                                    var t6648 uint32 = t6647 << t6648_rhs
                                    var t6649 uint32 = t6646 | t6648
                                    var t6650_rhs uint32 = 63
                                    var t6650 uint32 = fourth__14 & t6650_rhs
                                    var t6651 uint32 = t6649 | t6650
                                    var t6652 Tuple3_4bool_4char_3int = utf8_valid_decode(t6651, 4)
                                    return t6652
                                }
                            }
                        } else {
                            var t6673 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t6673
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t6692 uint32 = uint32(rune(value__29))
    var t6693 bool
    var inline11468 bool = t6692 <= 1114111
    if inline11468 {
        var inline11469 bool = t6692 >= 55296
        var inline11471 bool
        if inline11469 {
            var inline11473 bool = t6692 <= 57343
            inline11471 = inline11473
        } else {
            inline11471 = false
        }
        var inline11472 bool = !inline11471
        t6693 = inline11472
    } else {
        t6693 = false
    }
    if t6693 {
        var t6694 string = _goml_runtime_core_char_to_string(value__29)
        return t6694
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t6709 bool = index__16 < 0
    var jp6700 bool
    if t6709 {
        jp6700 = true
    } else {
        var t6710 int
        var inline11475 int = _goml_runtime_core_string_len(value__15)
        t6710 = inline11475
        var t6711 bool = index__16 > t6710
        jp6700 = t6711
    }
    if jp6700 {
        return false
    } else {
        var t6703 int
        var inline11484 int = _goml_runtime_core_string_len(value__15)
        t6703 = inline11484
        var t6704 bool
        var inline11482 bool = index__16 == t6703
        t6704 = inline11482
        if t6704 {
            return true
        } else {
            var t6705 uint8
            var inline11480 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t6705 = inline11480
            var t6706_rhs uint8 = 192
            var t6706 uint8 = t6705 & t6706_rhs
            var t6707 bool
            var inline11477 uint8 = 128
            var inline11478 bool = t6706 == inline11477
            t6707 = inline11478
            var t6708 bool = !t6707
            return t6708
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t6720 bool = string_is_char_boundary(value__21, start__22)
    var jp6717 bool
    if t6720 {
        var t6721 bool = string_is_char_boundary(value__21, end__23)
        jp6717 = t6721
    } else {
        jp6717 = false
    }
    if jp6717 {
        var t6718 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t6718
    } else {
        var t6719 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t6719
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t6746 bool = value__4 <= 1114111
    if t6746 {
        var t6750 bool = value__4 >= 55296
        var jp6748 bool
        if t6750 {
            var t6751 bool = value__4 <= 57343
            jp6748 = t6751
        } else {
            jp6748 = false
        }
        var t6749 bool = !jp6748
        return t6749
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t6758 string = _goml_runtime_core_int_to_string(self__69)
    return t6758
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t6761 string = _goml_runtime_core_bool_to_string(self__66)
    return t6761
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t6764 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t6764
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field11602 rune
    var inline11488 bool = utf8_valid_scalar(value__0)
    if inline11488 {
        var inline11489 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline11491 rune = inline11489._1
        commute_field11602 = inline11491
        var t6770 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field11602,
            _2: width__1,
        }
        return t6770
    } else {
        var inline11486 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline11486
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t6775 bool = value__3 < 128
    if t6775 {
        return true
    } else {
        var t6776 bool = value__3 > 191
        return t6776
    }
}

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t6779 bool = self__117 == other__118
    return t6779
}

func main() {
    main0()
}
