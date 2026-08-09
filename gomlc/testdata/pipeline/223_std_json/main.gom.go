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

type Tuple2_11Option__int_11Option__int struct {
    _0 Option__int
    _1 Option__int
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
    var inline8177 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    vec_literal__178 = inline8177
    var t2472 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: vec_literal__178,
    }
    return t2472
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline8192 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline8192
    var t2486 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t2486, length__5)
    var for_index1 int = 0
    Loop_loop2488:
    for {
        var t2489 bool = for_index1 < length__5
        if t2489 {
            var for_item3 int = for_index1
            var t2490 int = for_index1 + 1
            for_index1 = t2490
            var t2491 *_goml_vec_uint8 = self__3.values
            var t2492 uint8
            var inline8188 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t2492 = inline8188
            vec_push__Vec_5uint8(t2491, t2492)
            continue
        } else {
            break Loop_loop2488
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t2495 string
    var inline8194 string = char_to_string(value__8)
    t2495 = inline8194
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t2495)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t4436 string = "" + message__201
    var t4437 string = t4436 + " at byte "
    var t4438 *ref_int_x = value__200.index
    var t4439 int
    var inline9713 int = ref_get__Ref_3int(t4438)
    t4439 = inline9713
    var t4440 string
    var inline9711 string = _goml_runtime_core_int_to_string(t4439)
    t4440 = inline9711
    var t4441 string = t4437 + t4440
    return t4441
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop4456:
    for {
        var t4464 *ref_int_x = value__203.index
        var t4465 int
        var inline9734 int = ref_get__Ref_3int(t4464)
        t4465 = inline9734
        var t4466 string = value__203.input
        var t4467 int
        var inline9732 int = _goml_runtime_core_string_len(t4466)
        t4467 = inline9732
        var t4468 bool = t4465 < t4467
        var jp4458 bool
        if t4468 {
            var t4469 string = value__203.input
            var t4470 *ref_int_x = value__203.index
            var t4471 int
            var inline9726 int = ref_get__Ref_3int(t4470)
            t4471 = inline9726
            var t4472 uint8
            var inline9724 uint8 = _goml_runtime_core_string_byte_get(t4469, t4471)
            t4472 = inline9724
            var inline9715 bool = t4472 == 9
            var inline9717 bool
            if inline9715 {
                inline9717 = true
            } else {
                var inline9722 bool = t4472 == 10
                inline9717 = inline9722
            }
            var inline9719 bool
            if inline9717 {
                inline9719 = true
            } else {
                var inline9721 bool = t4472 == 13
                inline9719 = inline9721
            }
            if inline9719 {
                jp4458 = true
            } else {
                var inline9720 bool = t4472 == 32
                jp4458 = inline9720
            }
        } else {
            jp4458 = false
        }
        if jp4458 {
            var t4459 *ref_int_x = value__203.index
            var t4460 *ref_int_x = value__203.index
            var t4461 int
            var inline9730 int = ref_get__Ref_3int(t4460)
            t4461 = inline9730
            var t4462 int = t4461 + 1
            ref_set__Ref_3int(t4459, t4462)
            continue
        } else {
            break Loop_loop4456
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__uint32 {
    var t4503 bool = value__204 >= 48
    var jp4479 bool
    if t4503 {
        var t4504 bool = value__204 <= 57
        jp4479 = t4504
    } else {
        jp4479 = false
    }
    if jp4479 {
        var t4480 uint8 = value__204 - 48
        var t4481 uint32 = uint32(uint8(t4480))
        var t4482 Option__uint32 = Option__uint32_Some{
            _0: t4481,
        }
        return t4482
    } else {
        var t4501 bool = value__204 >= 65
        var jp4486 bool
        if t4501 {
            var t4502 bool = value__204 <= 70
            jp4486 = t4502
        } else {
            jp4486 = false
        }
        if jp4486 {
            var t4487 uint8 = value__204 - 65
            var t4488 uint8 = t4487 + 10
            var t4489 uint32 = uint32(uint8(t4488))
            var t4490 Option__uint32 = Option__uint32_Some{
                _0: t4489,
            }
            return t4490
        } else {
            var t4499 bool = value__204 >= 97
            var jp4494 bool
            if t4499 {
                var t4500 bool = value__204 <= 102
                jp4494 = t4500
            } else {
                jp4494 = false
            }
            if jp4494 {
                var t4495 uint8 = value__204 - 97
                var t4496 uint8 = t4495 + 10
                var t4497 uint32 = uint32(uint8(t4496))
                var t4498 Option__uint32 = Option__uint32_Some{
                    _0: t4497,
                }
                return t4498
            } else {
                return Option__uint32_None{}
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__uint32__string {
    var t4509 *ref_int_x = value__205.index
    var t4510 int
    var inline9762 int = ref_get__Ref_3int(t4509)
    t4510 = inline9762
    var t4511 int = t4510 + 4
    var t4512 string = value__205.input
    var t4513 int
    var inline9760 int = _goml_runtime_core_string_len(t4512)
    t4513 = inline9760
    var t4514 bool = t4511 > t4513
    if t4514 {
        var t4515 string
        var inline9736 string = "incomplete unicode escape"
        var inline9737 string = "" + inline9736
        var inline9738 string = inline9737 + " at byte "
        var inline9739 *ref_int_x = value__205.index
        var inline9740 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9739)
        var inline9741 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9740)
        var inline9742 string = inline9738 + inline9741
        t4515 = inline9742
        var t4516 Result__uint32__string = Result__uint32__string_Err{
            _0: t4515,
        }
        return t4516
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop4523:
        for {
            var t4524 bool = for_index744 < for_limit745
            if t4524 {
                var for_item746 int = for_index744
                var t4525 int = for_index744 + 1
                for_index744 = t4525
                var t4526 string = value__205.input
                var t4527 *ref_int_x = value__205.index
                var t4528 int
                var inline9754 int = ref_get__Ref_3int(t4527)
                t4528 = inline9754
                var t4529 int = t4528 + for_item746
                var t4530 uint8
                var inline9752 uint8 = _goml_runtime_core_string_byte_get(t4526, t4529)
                t4530 = inline9752
                var mtmp748 Option__uint32 = _goml_m_std_p_json_p_hex__digit(t4530)
                switch mtmp748.(type) {
                case Option__uint32_None:
                    var t4532 string
                    var inline9744 string = "invalid unicode escape"
                    var inline9745 string = "" + inline9744
                    var inline9746 string = inline9745 + " at byte "
                    var inline9747 *ref_int_x = value__205.index
                    var inline9748 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9747)
                    var inline9749 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9748)
                    var inline9750 string = inline9746 + inline9749
                    t4532 = inline9750
                    var t4533 Result__uint32__string = Result__uint32__string_Err{
                        _0: t4532,
                    }
                    return t4533
                case Option__uint32_Some:
                    var x749 uint32 = mtmp748.(Option__uint32_Some)._0
                    var t4534 uint32 = result__206 * 16
                    var t4535 uint32 = t4534 + x749
                    result__206 = t4535
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4523
            }
        }
        var t4518 *ref_int_x = value__205.index
        var t4519 *ref_int_x = value__205.index
        var t4520 int
        var inline9758 int = ref_get__Ref_3int(t4519)
        t4520 = inline9758
        var t4521 int = t4520 + 4
        ref_set__Ref_3int(t4518, t4521)
        var t4522 Result__uint32__string = Result__uint32__string_Ok{
            _0: result__206,
        }
        return t4522
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var commute_field10942 rune
    var inline9775 bool = utf8_valid_scalar(codepoint__211)
    if inline9775 {
        var inline9776 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(codepoint__211)
        var inline9777 rune = inline9776._1
        commute_field10942 = inline9777
        var inline9772 string = _goml_m_inherent_i_char_i_char_i_to__string(commute_field10942)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline9772)
        var t4542 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t4542
    } else {
        var t4540 string
        var inline9764 string = "invalid unicode codepoint"
        var inline9765 string = "" + inline9764
        var inline9766 string = inline9765 + " at byte "
        var inline9767 *ref_int_x = value__209.index
        var inline9768 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9767)
        var inline9769 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9768)
        var inline9770 string = inline9766 + inline9769
        t4540 = inline9770
        var t4541 Result__unit__string = Result__unit__string_Err{
            _0: t4540,
        }
        return t4541
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp4546 uint32
    switch mtmp756.(type) {
    case Result__uint32__string_Ok:
        var x757 uint32 = mtmp756.(Result__uint32__string_Ok)._0
        jp4546 = x757
        var t4606 bool = jp4546 >= 55296
        var jp4550 bool
        if t4606 {
            var t4607 bool = jp4546 <= 56319
            jp4550 = t4607
        } else {
            jp4550 = false
        }
        if jp4550 {
            var t4586 *ref_int_x = value__213.index
            var t4587 int
            var inline9819 int = ref_get__Ref_3int(t4586)
            t4587 = inline9819
            var t4588 int = t4587 + 2
            var t4589 string = value__213.input
            var t4590 int
            var inline9817 int = _goml_runtime_core_string_len(t4589)
            t4590 = inline9817
            var t4591 bool = t4588 > t4590
            var jp4579 bool
            if t4591 {
                jp4579 = true
            } else {
                var t4592 string = value__213.input
                var t4593 *ref_int_x = value__213.index
                var t4594 int
                var inline9783 int = ref_get__Ref_3int(t4593)
                t4594 = inline9783
                var t4595 uint8
                var inline9781 uint8 = _goml_runtime_core_string_byte_get(t4592, t4594)
                t4595 = inline9781
                var t4596 bool = t4595 != 92
                jp4579 = t4596
            }
            var jp4554 bool
            if jp4579 {
                jp4554 = true
            } else {
                var t4580 string = value__213.input
                var t4581 *ref_int_x = value__213.index
                var t4582 int
                var inline9787 int = ref_get__Ref_3int(t4581)
                t4582 = inline9787
                var t4583 int = t4582 + 1
                var t4584 uint8
                var inline9785 uint8 = _goml_runtime_core_string_byte_get(t4580, t4583)
                t4584 = inline9785
                var t4585 bool = t4584 != 117
                jp4554 = t4585
            }
            if jp4554 {
                var t4555 string
                var inline9789 string = "missing low surrogate"
                var inline9790 string = "" + inline9789
                var inline9791 string = inline9790 + " at byte "
                var inline9792 *ref_int_x = value__213.index
                var inline9793 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9792)
                var inline9794 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9793)
                var inline9795 string = inline9791 + inline9794
                t4555 = inline9795
                var t4556 Result__unit__string = Result__unit__string_Err{
                    _0: t4555,
                }
                return t4556
            } else {
                var t4557 *ref_int_x = value__213.index
                var t4558 *ref_int_x = value__213.index
                var t4559 int
                var inline9815 int = ref_get__Ref_3int(t4558)
                t4559 = inline9815
                var t4560 int = t4559 + 2
                ref_set__Ref_3int(t4557, t4560)
                var mtmp760 Result__uint32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp4562 uint32
                switch mtmp760.(type) {
                case Result__uint32__string_Ok:
                    var x761 uint32 = mtmp760.(Result__uint32__string_Ok)._0
                    jp4562 = x761
                    var t4575 bool = jp4562 < 56320
                    var jp4566 bool
                    if t4575 {
                        jp4566 = true
                    } else {
                        var t4576 bool = jp4562 > 57343
                        jp4566 = t4576
                    }
                    if jp4566 {
                        var t4567 string
                        var inline9797 string = "invalid low surrogate"
                        var inline9798 string = "" + inline9797
                        var inline9799 string = inline9798 + " at byte "
                        var inline9800 *ref_int_x = value__213.index
                        var inline9801 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9800)
                        var inline9802 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9801)
                        var inline9803 string = inline9799 + inline9802
                        t4567 = inline9803
                        var t4568 Result__unit__string = Result__unit__string_Err{
                            _0: t4567,
                        }
                        return t4568
                    } else {
                        var t4569 uint32 = jp4546 - 55296
                        var t4570 uint32 = t4569 * 1024
                        var t4571 uint32 = 65536 + t4570
                        var t4572 uint32 = t4571 + jp4562
                        var t4573 uint32 = t4572 - 56320
                        var inline9805 Option__char = char_from_uint32(t4573)
                        switch inline9805.(type) {
                        case Option__char_None:
                            var inline9806 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline9807 Result__unit__string = Result__unit__string_Err{
                                _0: inline9806,
                            }
                            return inline9807
                        case Option__char_Some:
                            var inline9808 rune = inline9805.(Option__char_Some)._0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline9808)
                            var inline9811 Result__unit__string = Result__unit__string_Ok{
                                _0: struct{}{},
                            }
                            return inline9811
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__uint32__string_Err:
                    var x762 string = mtmp760.(Result__uint32__string_Err)._0
                    var t4577 Result__unit__string = Result__unit__string_Err{
                        _0: x762,
                    }
                    return t4577
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t4604 bool = jp4546 >= 56320
            var jp4600 bool
            if t4604 {
                var t4605 bool = jp4546 <= 57343
                jp4600 = t4605
            } else {
                jp4600 = false
            }
            if jp4600 {
                var t4601 string
                var inline9821 string = "unexpected low surrogate"
                var inline9822 string = "" + inline9821
                var inline9823 string = inline9822 + " at byte "
                var inline9824 *ref_int_x = value__213.index
                var inline9825 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9824)
                var inline9826 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9825)
                var inline9827 string = inline9823 + inline9826
                t4601 = inline9827
                var t4602 Result__unit__string = Result__unit__string_Err{
                    _0: t4601,
                }
                return t4602
            } else {
                var t4603 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp4546)
                return t4603
            }
        }
    case Result__uint32__string_Err:
        var x758 string = mtmp756.(Result__uint32__string_Err)._0
        var t4608 Result__unit__string = Result__unit__string_Err{
            _0: x758,
        }
        return t4608
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4724 *ref_int_x = value__217.index
    var t4725 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4724)
    var t4726 string = value__217.input
    var t4727 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4726)
    var t4728 bool = t4725 >= t4727
    var jp4716 bool
    if t4728 {
        jp4716 = true
    } else {
        var t4729 string = value__217.input
        var t4730 *ref_int_x = value__217.index
        var t4731 int
        var inline9831 int = ref_get__Ref_3int(t4730)
        t4731 = inline9831
        var t4732 uint8
        var inline9829 uint8 = _goml_runtime_core_string_byte_get(t4729, t4731)
        t4732 = inline9829
        var t4733 bool = t4732 != 34
        jp4716 = t4733
    }
    if jp4716 {
        var t4717 string
        var inline9833 string = "expected string"
        var inline9834 string = "" + inline9833
        var inline9835 string = inline9834 + " at byte "
        var inline9836 *ref_int_x = value__217.index
        var inline9837 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9836)
        var inline9838 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9837)
        var inline9839 string = inline9835 + inline9838
        t4717 = inline9839
        var t4718 Result__string__string = Result__string__string_Err{
            _0: t4717,
        }
        return t4718
    } else {
        var t4719 *ref_int_x = value__217.index
        var t4720 *ref_int_x = value__217.index
        var t4721 int
        var inline9843 int = ref_get__Ref_3int(t4720)
        t4721 = inline9843
        var t4722 int = t4721 + 1
        ref_set__Ref_3int(t4719, t4722)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t4612 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4612)
        Loop_loop4616:
        for {
            var t4617 *ref_int_x = value__217.index
            var t4618 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4617)
            var t4619 string = value__217.input
            var t4620 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4619)
            var t4621 bool = t4618 < t4620
            if t4621 {
                var t4622 string = value__217.input
                var t4623 *ref_int_x = value__217.index
                var t4624 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4623)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4622, t4624)
                var t4626 bool = byte__220 == 34
                if t4626 {
                    var t4634 *ref_int_x = value__217.index
                    var t4635 int
                    var inline9858 int = ref_get__Ref_3int(t4634)
                    t4635 = inline9858
                    var t4636 bool = segment__219 < t4635
                    if t4636 {
                        var t4637 string = value__217.input
                        var t4638 *ref_int_x = value__217.index
                        var t4639 int
                        var inline9847 int = ref_get__Ref_3int(t4638)
                        t4639 = inline9847
                        var t4640 string
                        var inline9845 string = string_byte_slice(t4637, segment__219, t4639)
                        t4640 = inline9845
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4640)
                    } else {}
                    var t4628 *ref_int_x = value__217.index
                    var t4629 *ref_int_x = value__217.index
                    var t4630 int
                    var inline9856 int = ref_get__Ref_3int(t4629)
                    t4630 = inline9856
                    var t4631 int = t4630 + 1
                    ref_set__Ref_3int(t4628, t4631)
                    var t4632 string
                    var inline9849 *_goml_vec_uint8 = builder__218.values
                    var inline9850 Tuple2_4bool_6string = string_from_utf8(inline9849)
                    var inline9851 string = inline9850._1
                    t4632 = inline9851
                    var t4633 Result__string__string = Result__string__string_Ok{
                        _0: t4632,
                    }
                    return t4633
                } else {
                    var t4643 bool = byte__220 == 92
                    if t4643 {
                        var t4698 *ref_int_x = value__217.index
                        var t4699 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4698)
                        var t4700 bool = segment__219 < t4699
                        if t4700 {
                            var t4701 string = value__217.input
                            var t4702 *ref_int_x = value__217.index
                            var t4703 int
                            var inline9862 int = ref_get__Ref_3int(t4702)
                            t4703 = inline9862
                            var t4704 string
                            var inline9860 string = string_byte_slice(t4701, segment__219, t4703)
                            t4704 = inline9860
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t4704)
                        } else {}
                        var t4645 *ref_int_x = value__217.index
                        var t4646 *ref_int_x = value__217.index
                        var t4647 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4646)
                        var t4648 int = t4647 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4645, t4648)
                        var t4691 *ref_int_x = value__217.index
                        var t4692 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4691)
                        var t4693 string = value__217.input
                        var t4694 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4693)
                        var t4695 bool = t4692 >= t4694
                        if t4695 {
                            var t4696 string
                            var inline9864 string = "incomplete escape"
                            var inline9865 string = "" + inline9864
                            var inline9866 string = inline9865 + " at byte "
                            var inline9867 *ref_int_x = value__217.index
                            var inline9868 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9867)
                            var inline9869 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9868)
                            var inline9870 string = inline9866 + inline9869
                            t4696 = inline9870
                            var t4697 Result__string__string = Result__string__string_Err{
                                _0: t4696,
                            }
                            return t4697
                        } else {
                            var t4650 string = value__217.input
                            var t4651 *ref_int_x = value__217.index
                            var t4652 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4651)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4650, t4652)
                            var t4653 *ref_int_x = value__217.index
                            var t4654 *ref_int_x = value__217.index
                            var t4655 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4654)
                            var t4656 int = t4655 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4653, t4656)
                            var t4660 bool = escape__221 == 34
                            if t4660 {
                                var inline9872 rune = 34
                                var inline9873 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9872)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline9873)
                                var t4658 *ref_int_x = value__217.index
                                var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                segment__219 = t4659
                                continue
                            } else {
                                var t4663 bool = escape__221 == 92
                                if t4663 {
                                    var inline9876 rune = 92
                                    var inline9877 string = _goml_m_inherent_i_char_i_char_i_to__string(inline9876)
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline9877)
                                    var t4658 *ref_int_x = value__217.index
                                    var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                    segment__219 = t4659
                                    continue
                                } else {
                                    var t4666 bool = escape__221 == 47
                                    if t4666 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t4658 *ref_int_x = value__217.index
                                        var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                        segment__219 = t4659
                                        continue
                                    } else {
                                        var t4669 bool = escape__221 == 98
                                        if t4669 {
                                            var mtmp770 Option__char = char_from_uint32(8)
                                            switch mtmp770.(type) {
                                            case Option__char_None:
                                                var t4658 *ref_int_x = value__217.index
                                                var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                                segment__219 = t4659
                                                continue
                                            case Option__char_Some:
                                                var x771 rune = mtmp770.(Option__char_Some)._0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t4658 *ref_int_x = value__217.index
                                                var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                                segment__219 = t4659
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t4673 bool = escape__221 == 102
                                            if t4673 {
                                                var mtmp772 Option__char = char_from_uint32(12)
                                                switch mtmp772.(type) {
                                                case Option__char_None:
                                                    var t4658 *ref_int_x = value__217.index
                                                    var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                                    segment__219 = t4659
                                                    continue
                                                case Option__char_Some:
                                                    var x773 rune = mtmp772.(Option__char_Some)._0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t4658 *ref_int_x = value__217.index
                                                    var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                                    segment__219 = t4659
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t4677 bool = escape__221 == 110
                                                if t4677 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t4658 *ref_int_x = value__217.index
                                                    var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                                    segment__219 = t4659
                                                    continue
                                                } else {
                                                    var t4680 bool = escape__221 == 114
                                                    if t4680 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t4658 *ref_int_x = value__217.index
                                                        var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                                        segment__219 = t4659
                                                        continue
                                                    } else {
                                                        var t4683 bool = escape__221 == 116
                                                        if t4683 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t4658 *ref_int_x = value__217.index
                                                            var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                                            segment__219 = t4659
                                                            continue
                                                        } else {
                                                            var t4686 bool = escape__221 == 117
                                                            if t4686 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774.(type) {
                                                                case Result__unit__string_Ok:
                                                                    var t4658 *ref_int_x = value__217.index
                                                                    var t4659 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4658)
                                                                    segment__219 = t4659
                                                                    continue
                                                                case Result__unit__string_Err:
                                                                    var x776 string = mtmp774.(Result__unit__string_Err)._0
                                                                    var t4688 Result__string__string = Result__string__string_Err{
                                                                        _0: x776,
                                                                    }
                                                                    return t4688
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t4689 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t4690 Result__string__string = Result__string__string_Err{
                                                                    _0: t4689,
                                                                }
                                                                return t4690
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
                        var t4707 bool = byte__220 < 32
                        if t4707 {
                            var t4708 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t4709 Result__string__string = Result__string__string_Err{
                                _0: t4708,
                            }
                            return t4709
                        } else {
                            var t4710 *ref_int_x = value__217.index
                            var t4711 *ref_int_x = value__217.index
                            var t4712 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4711)
                            var t4713 int = t4712 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4710, t4713)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop4616
            }
        }
        var t4614 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t4615 Result__string__string = Result__string__string_Err{
            _0: t4614,
        }
        return t4615
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t4742 *ref_int_x = value__225.index
    var start__226 int
    var inline9897 int = ref_get__Ref_3int(t4742)
    start__226 = inline9897
    Loop_loop4747:
    for {
        var t4755 *ref_int_x = value__225.index
        var t4756 int
        var inline9893 int = ref_get__Ref_3int(t4755)
        t4756 = inline9893
        var t4757 string = value__225.input
        var t4758 int
        var inline9891 int = _goml_runtime_core_string_len(t4757)
        t4758 = inline9891
        var t4759 bool = t4756 < t4758
        var jp4749 bool
        if t4759 {
            var t4760 string = value__225.input
            var t4761 *ref_int_x = value__225.index
            var t4762 int
            var inline9885 int = ref_get__Ref_3int(t4761)
            t4762 = inline9885
            var t4763 uint8
            var inline9883 uint8 = _goml_runtime_core_string_byte_get(t4760, t4762)
            t4763 = inline9883
            var inline9880 bool = t4763 >= 48
            if inline9880 {
                var inline9881 bool = t4763 <= 57
                jp4749 = inline9881
            } else {
                jp4749 = false
            }
        } else {
            jp4749 = false
        }
        if jp4749 {
            var t4750 *ref_int_x = value__225.index
            var t4751 *ref_int_x = value__225.index
            var t4752 int
            var inline9889 int = ref_get__Ref_3int(t4751)
            t4752 = inline9889
            var t4753 int = t4752 + 1
            ref_set__Ref_3int(t4750, t4753)
            continue
        } else {
            break Loop_loop4747
        }
    }
    var t4744 *ref_int_x = value__225.index
    var t4745 int
    var inline9895 int = ref_get__Ref_3int(t4744)
    t4745 = inline9895
    var t4746 bool = t4745 > start__226
    return t4746
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t4767 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4767)
    var t4888 string = value__227.input
    var t4889 *ref_int_x = value__227.index
    var t4890 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4889)
    var t4891 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4888, t4890)
    var t4892 bool = t4891 == 45
    if t4892 {
        var t4893 *ref_int_x = value__227.index
        var t4894 *ref_int_x = value__227.index
        var t4895 int
        var inline9901 int = ref_get__Ref_3int(t4894)
        t4895 = inline9901
        var t4896 int = t4895 + 1
        ref_set__Ref_3int(t4893, t4896)
    } else {}
    var t4851 *ref_int_x = value__227.index
    var t4852 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4851)
    var t4853 string = value__227.input
    var t4854 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4853)
    var t4855 bool = t4852 >= t4854
    if t4855 {
        var t4856 string
        var inline9903 string = "incomplete number"
        var inline9904 string = "" + inline9903
        var inline9905 string = inline9904 + " at byte "
        var inline9906 *ref_int_x = value__227.index
        var inline9907 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9906)
        var inline9908 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9907)
        var inline9909 string = inline9905 + inline9908
        t4856 = inline9909
        var t4857 Result__string__string = Result__string__string_Err{
            _0: t4856,
        }
        return t4857
    } else {
        var t4859 string = value__227.input
        var t4860 *ref_int_x = value__227.index
        var t4861 int
        var inline9944 int = ref_get__Ref_3int(t4860)
        t4861 = inline9944
        var t4862 uint8
        var inline9942 uint8 = _goml_runtime_core_string_byte_get(t4859, t4861)
        t4862 = inline9942
        var t4863 bool = t4862 == 48
        if t4863 {
            var t4864 *ref_int_x = value__227.index
            var t4865 *ref_int_x = value__227.index
            var t4866 int
            var inline9932 int = ref_get__Ref_3int(t4865)
            t4866 = inline9932
            var t4867 int = t4866 + 1
            ref_set__Ref_3int(t4864, t4867)
            var t4873 *ref_int_x = value__227.index
            var t4874 int
            var inline9928 int = ref_get__Ref_3int(t4873)
            t4874 = inline9928
            var t4875 string = value__227.input
            var t4876 int
            var inline9926 int = _goml_runtime_core_string_len(t4875)
            t4876 = inline9926
            var t4877 bool = t4874 < t4876
            var jp4870 bool
            if t4877 {
                var t4878 string = value__227.input
                var t4879 *ref_int_x = value__227.index
                var t4880 int
                var inline9916 int = ref_get__Ref_3int(t4879)
                t4880 = inline9916
                var t4881 uint8
                var inline9914 uint8 = _goml_runtime_core_string_byte_get(t4878, t4880)
                t4881 = inline9914
                var inline9911 bool = t4881 >= 48
                if inline9911 {
                    var inline9912 bool = t4881 <= 57
                    jp4870 = inline9912
                } else {
                    jp4870 = false
                }
            } else {
                jp4870 = false
            }
            if jp4870 {
                var t4871 string
                var inline9918 string = "invalid leading zero"
                var inline9919 string = "" + inline9918
                var inline9920 string = inline9919 + " at byte "
                var inline9921 *ref_int_x = value__227.index
                var inline9922 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9921)
                var inline9923 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9922)
                var inline9924 string = inline9920 + inline9923
                t4871 = inline9924
                var t4872 Result__string__string = Result__string__string_Err{
                    _0: t4871,
                }
                return t4872
            } else {
                var t4841 *ref_int_x = value__227.index
                var t4842 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4841)
                var t4843 string = value__227.input
                var t4844 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4843)
                var t4845 bool = t4842 < t4844
                var jp4831 bool
                if t4845 {
                    var t4846 string = value__227.input
                    var t4847 *ref_int_x = value__227.index
                    var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                    var t4849 uint8
                    var inline9946 uint8 = _goml_runtime_core_string_byte_get(t4846, t4848)
                    t4849 = inline9946
                    var t4850 bool = t4849 == 46
                    jp4831 = t4850
                } else {
                    jp4831 = false
                }
                if jp4831 {
                    var t4832 *ref_int_x = value__227.index
                    var t4833 *ref_int_x = value__227.index
                    var t4834 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4833)
                    var t4835 int = t4834 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4832, t4835)
                    var t4837 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t4838 bool = !t4837
                    if t4838 {
                        var t4839 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t4840 Result__string__string = Result__string__string_Err{
                            _0: t4839,
                        }
                        return t4840
                    } else {
                        var t4813 *ref_int_x = value__227.index
                        var t4814 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4813)
                        var t4815 string = value__227.input
                        var t4816 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4815)
                        var t4817 bool = t4814 < t4816
                        var jp4778 bool
                        if t4817 {
                            var t4820 string = value__227.input
                            var t4821 *ref_int_x = value__227.index
                            var t4822 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4821)
                            var t4823 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4820, t4822)
                            var t4824 bool = t4823 == 101
                            if t4824 {
                                jp4778 = true
                            } else {
                                var t4825 string = value__227.input
                                var t4826 *ref_int_x = value__227.index
                                var t4827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4826)
                                var t4828 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4825, t4827)
                                var t4829 bool = t4828 == 69
                                jp4778 = t4829
                            }
                        } else {
                            jp4778 = false
                        }
                        if jp4778 {
                            var t4779 *ref_int_x = value__227.index
                            var t4780 *ref_int_x = value__227.index
                            var t4781 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4780)
                            var t4782 int = t4781 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4779, t4782)
                            var t4796 *ref_int_x = value__227.index
                            var t4797 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4796)
                            var t4798 string = value__227.input
                            var t4799 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4798)
                            var t4800 bool = t4797 < t4799
                            var jp4790 bool
                            if t4800 {
                                var t4803 string = value__227.input
                                var t4804 *ref_int_x = value__227.index
                                var t4805 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4804)
                                var t4806 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4803, t4805)
                                var t4807 bool = t4806 == 43
                                if t4807 {
                                    jp4790 = true
                                } else {
                                    var t4808 string = value__227.input
                                    var t4809 *ref_int_x = value__227.index
                                    var t4810 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4809)
                                    var t4811 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4808, t4810)
                                    var t4812 bool = t4811 == 45
                                    jp4790 = t4812
                                }
                            } else {
                                jp4790 = false
                            }
                            if jp4790 {
                                var t4791 *ref_int_x = value__227.index
                                var t4792 *ref_int_x = value__227.index
                                var t4793 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4792)
                                var t4794 int = t4793 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4791, t4794)
                            } else {}
                            var t4785 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4786 bool = !t4785
                            if t4786 {
                                var t4787 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4788 Result__string__string = Result__string__string_Err{
                                    _0: t4787,
                                }
                                return t4788
                            } else {
                                var t4772 string = value__227.input
                                var t4773 *ref_int_x = value__227.index
                                var t4774 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4773)
                                var t4775 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4772, start__228, t4774)
                                var t4776 Result__string__string = Result__string__string_Ok{
                                    _0: t4775,
                                }
                                return t4776
                            }
                        } else {
                            var t4772 string = value__227.input
                            var t4773 *ref_int_x = value__227.index
                            var t4774 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4773)
                            var t4775 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4772, start__228, t4774)
                            var t4776 Result__string__string = Result__string__string_Ok{
                                _0: t4775,
                            }
                            return t4776
                        }
                    }
                } else {
                    var t4813 *ref_int_x = value__227.index
                    var t4814 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4813)
                    var t4815 string = value__227.input
                    var t4816 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4815)
                    var t4817 bool = t4814 < t4816
                    var jp4778 bool
                    if t4817 {
                        var t4820 string = value__227.input
                        var t4821 *ref_int_x = value__227.index
                        var t4822 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4821)
                        var t4823 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4820, t4822)
                        var t4824 bool = t4823 == 101
                        if t4824 {
                            jp4778 = true
                        } else {
                            var t4825 string = value__227.input
                            var t4826 *ref_int_x = value__227.index
                            var t4827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4826)
                            var t4828 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4825, t4827)
                            var t4829 bool = t4828 == 69
                            jp4778 = t4829
                        }
                    } else {
                        jp4778 = false
                    }
                    if jp4778 {
                        var t4779 *ref_int_x = value__227.index
                        var t4780 *ref_int_x = value__227.index
                        var t4781 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4780)
                        var t4782 int = t4781 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4779, t4782)
                        var t4796 *ref_int_x = value__227.index
                        var t4797 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4796)
                        var t4798 string = value__227.input
                        var t4799 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4798)
                        var t4800 bool = t4797 < t4799
                        var jp4790 bool
                        if t4800 {
                            var t4803 string = value__227.input
                            var t4804 *ref_int_x = value__227.index
                            var t4805 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4804)
                            var t4806 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4803, t4805)
                            var t4807 bool = t4806 == 43
                            if t4807 {
                                jp4790 = true
                            } else {
                                var t4808 string = value__227.input
                                var t4809 *ref_int_x = value__227.index
                                var t4810 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4809)
                                var t4811 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4808, t4810)
                                var t4812 bool = t4811 == 45
                                jp4790 = t4812
                            }
                        } else {
                            jp4790 = false
                        }
                        if jp4790 {
                            var t4791 *ref_int_x = value__227.index
                            var t4792 *ref_int_x = value__227.index
                            var t4793 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4792)
                            var t4794 int = t4793 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4791, t4794)
                        } else {}
                        var t4785 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4786 bool = !t4785
                        if t4786 {
                            var t4787 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4788 Result__string__string = Result__string__string_Err{
                                _0: t4787,
                            }
                            return t4788
                        } else {
                            var t4772 string = value__227.input
                            var t4773 *ref_int_x = value__227.index
                            var t4774 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4773)
                            var t4775 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4772, start__228, t4774)
                            var t4776 Result__string__string = Result__string__string_Ok{
                                _0: t4775,
                            }
                            return t4776
                        }
                    } else {
                        var t4772 string = value__227.input
                        var t4773 *ref_int_x = value__227.index
                        var t4774 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4773)
                        var t4775 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4772, start__228, t4774)
                        var t4776 Result__string__string = Result__string__string_Ok{
                            _0: t4775,
                        }
                        return t4776
                    }
                }
            }
        } else {
            var t4884 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t4885 bool = !t4884
            if t4885 {
                var t4886 string
                var inline9934 string = "expected number"
                var inline9935 string = "" + inline9934
                var inline9936 string = inline9935 + " at byte "
                var inline9937 *ref_int_x = value__227.index
                var inline9938 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9937)
                var inline9939 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9938)
                var inline9940 string = inline9936 + inline9939
                t4886 = inline9940
                var t4887 Result__string__string = Result__string__string_Err{
                    _0: t4886,
                }
                return t4887
            } else {
                var t4841 *ref_int_x = value__227.index
                var t4842 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4841)
                var t4843 string = value__227.input
                var t4844 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4843)
                var t4845 bool = t4842 < t4844
                var jp4831 bool
                if t4845 {
                    var t4846 string = value__227.input
                    var t4847 *ref_int_x = value__227.index
                    var t4848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4847)
                    var t4849 uint8
                    var inline9946 uint8 = _goml_runtime_core_string_byte_get(t4846, t4848)
                    t4849 = inline9946
                    var t4850 bool = t4849 == 46
                    jp4831 = t4850
                } else {
                    jp4831 = false
                }
                if jp4831 {
                    var t4832 *ref_int_x = value__227.index
                    var t4833 *ref_int_x = value__227.index
                    var t4834 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4833)
                    var t4835 int = t4834 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4832, t4835)
                    var t4837 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t4838 bool = !t4837
                    if t4838 {
                        var t4839 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t4840 Result__string__string = Result__string__string_Err{
                            _0: t4839,
                        }
                        return t4840
                    } else {
                        var t4813 *ref_int_x = value__227.index
                        var t4814 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4813)
                        var t4815 string = value__227.input
                        var t4816 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4815)
                        var t4817 bool = t4814 < t4816
                        var jp4778 bool
                        if t4817 {
                            var t4820 string = value__227.input
                            var t4821 *ref_int_x = value__227.index
                            var t4822 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4821)
                            var t4823 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4820, t4822)
                            var t4824 bool = t4823 == 101
                            if t4824 {
                                jp4778 = true
                            } else {
                                var t4825 string = value__227.input
                                var t4826 *ref_int_x = value__227.index
                                var t4827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4826)
                                var t4828 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4825, t4827)
                                var t4829 bool = t4828 == 69
                                jp4778 = t4829
                            }
                        } else {
                            jp4778 = false
                        }
                        if jp4778 {
                            var t4779 *ref_int_x = value__227.index
                            var t4780 *ref_int_x = value__227.index
                            var t4781 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4780)
                            var t4782 int = t4781 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4779, t4782)
                            var t4796 *ref_int_x = value__227.index
                            var t4797 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4796)
                            var t4798 string = value__227.input
                            var t4799 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4798)
                            var t4800 bool = t4797 < t4799
                            var jp4790 bool
                            if t4800 {
                                var t4803 string = value__227.input
                                var t4804 *ref_int_x = value__227.index
                                var t4805 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4804)
                                var t4806 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4803, t4805)
                                var t4807 bool = t4806 == 43
                                if t4807 {
                                    jp4790 = true
                                } else {
                                    var t4808 string = value__227.input
                                    var t4809 *ref_int_x = value__227.index
                                    var t4810 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4809)
                                    var t4811 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4808, t4810)
                                    var t4812 bool = t4811 == 45
                                    jp4790 = t4812
                                }
                            } else {
                                jp4790 = false
                            }
                            if jp4790 {
                                var t4791 *ref_int_x = value__227.index
                                var t4792 *ref_int_x = value__227.index
                                var t4793 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4792)
                                var t4794 int = t4793 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4791, t4794)
                            } else {}
                            var t4785 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t4786 bool = !t4785
                            if t4786 {
                                var t4787 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t4788 Result__string__string = Result__string__string_Err{
                                    _0: t4787,
                                }
                                return t4788
                            } else {
                                var t4772 string = value__227.input
                                var t4773 *ref_int_x = value__227.index
                                var t4774 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4773)
                                var t4775 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4772, start__228, t4774)
                                var t4776 Result__string__string = Result__string__string_Ok{
                                    _0: t4775,
                                }
                                return t4776
                            }
                        } else {
                            var t4772 string = value__227.input
                            var t4773 *ref_int_x = value__227.index
                            var t4774 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4773)
                            var t4775 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4772, start__228, t4774)
                            var t4776 Result__string__string = Result__string__string_Ok{
                                _0: t4775,
                            }
                            return t4776
                        }
                    }
                } else {
                    var t4813 *ref_int_x = value__227.index
                    var t4814 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4813)
                    var t4815 string = value__227.input
                    var t4816 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4815)
                    var t4817 bool = t4814 < t4816
                    var jp4778 bool
                    if t4817 {
                        var t4820 string = value__227.input
                        var t4821 *ref_int_x = value__227.index
                        var t4822 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4821)
                        var t4823 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4820, t4822)
                        var t4824 bool = t4823 == 101
                        if t4824 {
                            jp4778 = true
                        } else {
                            var t4825 string = value__227.input
                            var t4826 *ref_int_x = value__227.index
                            var t4827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4826)
                            var t4828 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4825, t4827)
                            var t4829 bool = t4828 == 69
                            jp4778 = t4829
                        }
                    } else {
                        jp4778 = false
                    }
                    if jp4778 {
                        var t4779 *ref_int_x = value__227.index
                        var t4780 *ref_int_x = value__227.index
                        var t4781 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4780)
                        var t4782 int = t4781 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4779, t4782)
                        var t4796 *ref_int_x = value__227.index
                        var t4797 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4796)
                        var t4798 string = value__227.input
                        var t4799 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4798)
                        var t4800 bool = t4797 < t4799
                        var jp4790 bool
                        if t4800 {
                            var t4803 string = value__227.input
                            var t4804 *ref_int_x = value__227.index
                            var t4805 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4804)
                            var t4806 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4803, t4805)
                            var t4807 bool = t4806 == 43
                            if t4807 {
                                jp4790 = true
                            } else {
                                var t4808 string = value__227.input
                                var t4809 *ref_int_x = value__227.index
                                var t4810 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4809)
                                var t4811 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t4808, t4810)
                                var t4812 bool = t4811 == 45
                                jp4790 = t4812
                            }
                        } else {
                            jp4790 = false
                        }
                        if jp4790 {
                            var t4791 *ref_int_x = value__227.index
                            var t4792 *ref_int_x = value__227.index
                            var t4793 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4792)
                            var t4794 int = t4793 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4791, t4794)
                        } else {}
                        var t4785 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t4786 bool = !t4785
                        if t4786 {
                            var t4787 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t4788 Result__string__string = Result__string__string_Err{
                                _0: t4787,
                            }
                            return t4788
                        } else {
                            var t4772 string = value__227.input
                            var t4773 *ref_int_x = value__227.index
                            var t4774 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4773)
                            var t4775 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4772, start__228, t4774)
                            var t4776 Result__string__string = Result__string__string_Ok{
                                _0: t4775,
                            }
                            return t4776
                        }
                    } else {
                        var t4772 string = value__227.input
                        var t4773 *ref_int_x = value__227.index
                        var t4774 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4773)
                        var t4775 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t4772, start__228, t4774)
                        var t4776 Result__string__string = Result__string__string_Ok{
                            _0: t4775,
                        }
                        return t4776
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t4919 *ref_int_x = value__230.index
    var t4920 int
    var inline9974 int = ref_get__Ref_3int(t4919)
    t4920 = inline9974
    var t4921 int
    var inline9972 int = _goml_runtime_core_string_len(expected__231)
    t4921 = inline9972
    var t4922 int = t4920 + t4921
    var t4923 string = value__230.input
    var t4924 int
    var inline9970 int = _goml_runtime_core_string_len(t4923)
    t4924 = inline9970
    var t4925 bool = t4922 <= t4924
    var jp4910 bool
    if t4925 {
        var t4926 string = value__230.input
        var t4927 *ref_int_x = value__230.index
        var t4928 int
        var inline9954 int = ref_get__Ref_3int(t4927)
        t4928 = inline9954
        var t4929 *ref_int_x = value__230.index
        var t4930 int
        var inline9952 int = ref_get__Ref_3int(t4929)
        t4930 = inline9952
        var t4931 int
        var inline9950 int = _goml_runtime_core_string_len(expected__231)
        t4931 = inline9950
        var t4932 int = t4930 + t4931
        var t4933 string
        var inline9948 string = string_byte_slice(t4926, t4928, t4932)
        t4933 = inline9948
        var t4934 bool = t4933 == expected__231
        jp4910 = t4934
    } else {
        jp4910 = false
    }
    if jp4910 {
        var t4911 *ref_int_x = value__230.index
        var t4912 *ref_int_x = value__230.index
        var t4913 int
        var inline9960 int = ref_get__Ref_3int(t4912)
        t4913 = inline9960
        var t4914 int
        var inline9958 int = _goml_runtime_core_string_len(expected__231)
        t4914 = inline9958
        var t4915 int = t4913 + t4914
        ref_set__Ref_3int(t4911, t4915)
        var t4916 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: result__232,
        }
        return t4916
    } else {
        var t4917 string
        var inline9962 string = "invalid literal"
        var inline9963 string = "" + inline9962
        var inline9964 string = inline9963 + " at byte "
        var inline9965 *ref_int_x = value__230.index
        var inline9966 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9965)
        var inline9967 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9966)
        var inline9968 string = inline9964 + inline9967
        t4917 = inline9968
        var t4918 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4917,
        }
        return t4918
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t4937 *ref_int_x = value__233.index
    var t4938 *ref_int_x = value__233.index
    var t4939 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4938)
    var t4940 int = t4939 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t4937, t4940)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var vec_literal__8833 *_goml_vec__goml_m_std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value()
    var t4995 *ref_int_x = value__233.index
    var t4996 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t4995)
    var t4997 string = value__233.input
    var t4998 int = _goml_m_inherent_i_string_i_string_i_byte__len(t4997)
    var t4999 bool = t4996 < t4998
    var jp4988 bool
    if t4999 {
        var t5000 string = value__233.input
        var t5001 *ref_int_x = value__233.index
        var t5002 int
        var inline9978 int = ref_get__Ref_3int(t5001)
        t5002 = inline9978
        var t5003 uint8
        var inline9976 uint8 = _goml_runtime_core_string_byte_get(t5000, t5002)
        t5003 = inline9976
        var t5004 bool = t5003 == 93
        jp4988 = t5004
    } else {
        jp4988 = false
    }
    if jp4988 {
        var t4989 *ref_int_x = value__233.index
        var t4990 *ref_int_x = value__233.index
        var t4991 int
        var inline9982 int = ref_get__Ref_3int(t4990)
        t4991 = inline9982
        var t4992 int = t4991 + 1
        ref_set__Ref_3int(t4989, t4992)
        var t4993 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: vec_literal__8833,
        }
        var t4994 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t4993,
        }
        return t4994
    } else {
        Loop_loop4945:
        for {
            var t4946 *ref_int_x = value__233.index
            var t4947 int
            var inline10024 int = ref_get__Ref_3int(t4946)
            t4947 = inline10024
            var t4948 string = value__233.input
            var t4949 int
            var inline10022 int = _goml_runtime_core_string_len(t4948)
            t4949 = inline10022
            var t4950 bool = t4947 < t4949
            if t4950 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp4952 _goml_m_std_p_json_p_Value
                switch mtmp797.(type) {
                case _goml_m_Result____std_p_json_p_Value____string_Ok:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                    jp4952 = x798
                    vec_push___goml_m_Vec__16std_p_json_p_Value(vec_literal__8833, jp4952)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t4954 *ref_int_x = value__233.index
                    var t4955 int
                    var inline10018 int = ref_get__Ref_3int(t4954)
                    t4955 = inline10018
                    var t4956 string = value__233.input
                    var t4957 int
                    var inline10016 int = _goml_runtime_core_string_len(t4956)
                    t4957 = inline10016
                    var t4958 bool = t4955 >= t4957
                    if t4958 {
                        var t4959 string
                        var inline9984 string = "unterminated array"
                        var inline9985 string = "" + inline9984
                        var inline9986 string = inline9985 + " at byte "
                        var inline9987 *ref_int_x = value__233.index
                        var inline9988 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline9987)
                        var inline9989 string = _goml_m_inherent_i_int_i_int_i_to__string(inline9988)
                        var inline9990 string = inline9986 + inline9989
                        t4959 = inline9990
                        var t4960 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t4959,
                        }
                        return t4960
                    } else {
                        var t4962 string = value__233.input
                        var t4963 *ref_int_x = value__233.index
                        var t4964 int
                        var inline10014 int = ref_get__Ref_3int(t4963)
                        t4964 = inline10014
                        var t4965 uint8
                        var inline10012 uint8 = _goml_runtime_core_string_byte_get(t4962, t4964)
                        t4965 = inline10012
                        var t4966 bool = t4965 == 93
                        if t4966 {
                            var t4967 *ref_int_x = value__233.index
                            var t4968 *ref_int_x = value__233.index
                            var t4969 int
                            var inline9994 int = ref_get__Ref_3int(t4968)
                            t4969 = inline9994
                            var t4970 int = t4969 + 1
                            ref_set__Ref_3int(t4967, t4970)
                            var t4971 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: vec_literal__8833,
                            }
                            var t4972 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                _0: t4971,
                            }
                            return t4972
                        } else {
                            var t4974 string = value__233.input
                            var t4975 *ref_int_x = value__233.index
                            var t4976 int
                            var inline10010 int = ref_get__Ref_3int(t4975)
                            t4976 = inline10010
                            var t4977 uint8
                            var inline10008 uint8 = _goml_runtime_core_string_byte_get(t4974, t4976)
                            t4977 = inline10008
                            var t4978 bool = t4977 == 44
                            if t4978 {
                                var t4979 *ref_int_x = value__233.index
                                var t4980 *ref_int_x = value__233.index
                                var t4981 int
                                var inline9998 int = ref_get__Ref_3int(t4980)
                                t4981 = inline9998
                                var t4982 int = t4981 + 1
                                ref_set__Ref_3int(t4979, t4982)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t4984 string
                                var inline10000 string = "expected array separator"
                                var inline10001 string = "" + inline10000
                                var inline10002 string = inline10001 + " at byte "
                                var inline10003 *ref_int_x = value__233.index
                                var inline10004 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10003)
                                var inline10005 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10004)
                                var inline10006 string = inline10002 + inline10005
                                t4984 = inline10006
                                var t4985 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t4984,
                                }
                                return t4985
                            }
                        }
                    }
                case _goml_m_Result____std_p_json_p_Value____string_Err:
                    var x799 string = mtmp797.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                    var t4986 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x799,
                    }
                    return t4986
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop4945
            }
        }
        var t4943 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t4944 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t4943,
        }
        return t4944
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5007 *ref_int_x = value__236.index
    var t5008 *ref_int_x = value__236.index
    var t5009 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5008)
    var t5010 int = t5009 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(t5007, t5010)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var vec_literal__10035 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_()
    var t5089 *ref_int_x = value__236.index
    var t5090 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5089)
    var t5091 string = value__236.input
    var t5092 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5091)
    var t5093 bool = t5090 < t5092
    var jp5082 bool
    if t5093 {
        var t5094 string = value__236.input
        var t5095 *ref_int_x = value__236.index
        var t5096 int
        var inline10028 int = ref_get__Ref_3int(t5095)
        t5096 = inline10028
        var t5097 uint8
        var inline10026 uint8 = _goml_runtime_core_string_byte_get(t5094, t5096)
        t5097 = inline10026
        var t5098 bool = t5097 == 125
        jp5082 = t5098
    } else {
        jp5082 = false
    }
    if jp5082 {
        var t5083 *ref_int_x = value__236.index
        var t5084 *ref_int_x = value__236.index
        var t5085 int
        var inline10032 int = ref_get__Ref_3int(t5084)
        t5085 = inline10032
        var t5086 int = t5085 + 1
        ref_set__Ref_3int(t5083, t5086)
        var t5087 _goml_m_std_p_json_p_Value = Object{
            _0: vec_literal__10035,
        }
        var t5088 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
            _0: t5087,
        }
        return t5088
    } else {
        Loop_loop5015:
        for {
            var t5016 *ref_int_x = value__236.index
            var t5017 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5016)
            var t5018 string = value__236.input
            var t5019 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5018)
            var t5020 bool = t5017 < t5019
            if t5020 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp5022 string
                switch mtmp809.(type) {
                case Result__string__string_Ok:
                    var x810 string = mtmp809.(Result__string__string_Ok)._0
                    jp5022 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t5070 *ref_int_x = value__236.index
                    var t5071 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5070)
                    var t5072 string = value__236.input
                    var t5073 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5072)
                    var t5074 bool = t5071 >= t5073
                    var jp5062 bool
                    if t5074 {
                        jp5062 = true
                    } else {
                        var t5075 string = value__236.input
                        var t5076 *ref_int_x = value__236.index
                        var t5077 int
                        var inline10036 int = ref_get__Ref_3int(t5076)
                        t5077 = inline10036
                        var t5078 uint8
                        var inline10034 uint8 = _goml_runtime_core_string_byte_get(t5075, t5077)
                        t5078 = inline10034
                        var t5079 bool = t5078 != 58
                        jp5062 = t5079
                    }
                    if jp5062 {
                        var t5063 string
                        var inline10038 string = "expected object colon"
                        var inline10039 string = "" + inline10038
                        var inline10040 string = inline10039 + " at byte "
                        var inline10041 *ref_int_x = value__236.index
                        var inline10042 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10041)
                        var inline10043 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10042)
                        var inline10044 string = inline10040 + inline10043
                        t5063 = inline10044
                        var t5064 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                            _0: t5063,
                        }
                        return t5064
                    } else {
                        var t5065 *ref_int_x = value__236.index
                        var t5066 *ref_int_x = value__236.index
                        var t5067 int
                        var inline10048 int = ref_get__Ref_3int(t5066)
                        t5067 = inline10048
                        var t5068 int = t5067 + 1
                        ref_set__Ref_3int(t5065, t5068)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp5025 _goml_m_std_p_json_p_Value
                        switch mtmp815.(type) {
                        case _goml_m_Result____std_p_json_p_Value____string_Ok:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
                            jp5025 = x816
                            var t5026 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp5022,
                                _1: jp5025,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(vec_literal__10035, t5026)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t5028 *ref_int_x = value__236.index
                            var t5029 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5028)
                            var t5030 string = value__236.input
                            var t5031 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5030)
                            var t5032 bool = t5029 >= t5031
                            if t5032 {
                                var t5033 string
                                var inline10050 string = "unterminated object"
                                var inline10051 string = "" + inline10050
                                var inline10052 string = inline10051 + " at byte "
                                var inline10053 *ref_int_x = value__236.index
                                var inline10054 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10053)
                                var inline10055 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10054)
                                var inline10056 string = inline10052 + inline10055
                                t5033 = inline10056
                                var t5034 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                    _0: t5033,
                                }
                                return t5034
                            } else {
                                var t5036 string = value__236.input
                                var t5037 *ref_int_x = value__236.index
                                var t5038 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5037)
                                var t5039 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5036, t5038)
                                var t5040 bool = t5039 == 125
                                if t5040 {
                                    var t5041 *ref_int_x = value__236.index
                                    var t5042 *ref_int_x = value__236.index
                                    var t5043 int
                                    var inline10060 int = ref_get__Ref_3int(t5042)
                                    t5043 = inline10060
                                    var t5044 int = t5043 + 1
                                    ref_set__Ref_3int(t5041, t5044)
                                    var t5045 _goml_m_std_p_json_p_Value = Object{
                                        _0: vec_literal__10035,
                                    }
                                    var t5046 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                                        _0: t5045,
                                    }
                                    return t5046
                                } else {
                                    var t5048 string = value__236.input
                                    var t5049 *ref_int_x = value__236.index
                                    var t5050 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(t5049)
                                    var t5051 uint8
                                    var inline10074 uint8 = _goml_runtime_core_string_byte_get(t5048, t5050)
                                    t5051 = inline10074
                                    var t5052 bool = t5051 == 44
                                    if t5052 {
                                        var t5053 *ref_int_x = value__236.index
                                        var t5054 *ref_int_x = value__236.index
                                        var t5055 int
                                        var inline10064 int = ref_get__Ref_3int(t5054)
                                        t5055 = inline10064
                                        var t5056 int = t5055 + 1
                                        ref_set__Ref_3int(t5053, t5056)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5058 string
                                        var inline10066 string = "expected object separator"
                                        var inline10067 string = "" + inline10066
                                        var inline10068 string = inline10067 + " at byte "
                                        var inline10069 *ref_int_x = value__236.index
                                        var inline10070 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10069)
                                        var inline10071 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10070)
                                        var inline10072 string = inline10068 + inline10071
                                        t5058 = inline10072
                                        var t5059 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                            _0: t5058,
                                        }
                                        return t5059
                                    }
                                }
                            }
                        case _goml_m_Result____std_p_json_p_Value____string_Err:
                            var x817 string = mtmp815.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
                            var t5060 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                                _0: x817,
                            }
                            return t5060
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case Result__string__string_Err:
                    var x811 string = mtmp809.(Result__string__string_Err)._0
                    var t5080 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: x811,
                    }
                    return t5080
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5015
            }
        }
        var t5013 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t5014 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5013,
        }
        return t5014
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t5103 *ref_int_x = value__240.index
    var t5104 int
    var inline10112 int = ref_get__Ref_3int(t5103)
    t5104 = inline10112
    var t5105 string = value__240.input
    var t5106 int
    var inline10110 int = _goml_runtime_core_string_len(t5105)
    t5106 = inline10110
    var t5107 bool = t5104 >= t5106
    if t5107 {
        var t5108 string
        var inline10076 string = "expected JSON value"
        var inline10077 string = "" + inline10076
        var inline10078 string = inline10077 + " at byte "
        var inline10079 *ref_int_x = value__240.index
        var inline10080 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10079)
        var inline10081 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10080)
        var inline10082 string = inline10078 + inline10081
        t5108 = inline10082
        var t5109 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: t5108,
        }
        return t5109
    } else {
        var t5110 string = value__240.input
        var t5111 *ref_int_x = value__240.index
        var t5112 int
        var inline10108 int = ref_get__Ref_3int(t5111)
        t5112 = inline10108
        var mtmp824 uint8
        var inline10106 uint8 = _goml_runtime_core_string_byte_get(t5110, t5112)
        mtmp824 = inline10106
        switch mtmp824 {
        case 123:
            var t5115 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t5115
        case 91:
            var t5116 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t5116
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825.(type) {
            case Result__string__string_Ok:
                var x826 string = mtmp825.(Result__string__string_Ok)._0
                var t5119 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t5120 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                    _0: t5119,
                }
                return t5120
            case Result__string__string_Err:
                var x827 string = mtmp825.(Result__string__string_Err)._0
                var t5121 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: x827,
                }
                return t5121
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t5122 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t5123 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t5122)
            return t5123
        case 102:
            var t5124 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t5125 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t5124)
            return t5125
        case 110:
            var t5126 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t5126
        default:
            var t5134 bool = mtmp824 == 45
            var jp5130 bool
            if t5134 {
                jp5130 = true
            } else {
                var inline10084 bool = mtmp824 >= 48
                if inline10084 {
                    var inline10085 bool = mtmp824 <= 57
                    jp5130 = inline10085
                } else {
                    jp5130 = false
                }
            }
            if jp5130 {
                var inline10087 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline10089 string
                switch inline10087.(type) {
                case Result__string__string_Ok:
                    var inline10092 string = inline10087.(Result__string__string_Ok)._0
                    inline10089 = inline10092
                    var inline10090 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline10089,
                    }
                    var inline10091 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                        _0: inline10090,
                    }
                    return inline10091
                case Result__string__string_Err:
                    var inline10094 string = inline10087.(Result__string__string_Err)._0
                    var inline10096 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                        _0: inline10094,
                    }
                    return inline10096
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5132 string
                var inline10098 string = "unexpected JSON token"
                var inline10099 string = "" + inline10098
                var inline10100 string = inline10099 + " at byte "
                var inline10101 *ref_int_x = value__240.index
                var inline10102 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10101)
                var inline10103 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10102)
                var inline10104 string = inline10100 + inline10103
                t5132 = inline10104
                var t5133 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                    _0: t5132,
                }
                return t5133
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline10126 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var inline10127 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline10126,
    }
    parser__245 = inline10127
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5139 _goml_m_std_p_json_p_Value
    switch mtmp828.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x829 _goml_m_std_p_json_p_Value = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp5139 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5142 *ref_int_x = parser__245.index
        var t5143 int
        var inline10124 int = ref_get__Ref_3int(t5142)
        t5143 = inline10124
        var t5144 int
        var inline10122 int = _goml_runtime_core_string_len(input__244)
        t5144 = inline10122
        var t5145 bool = t5143 == t5144
        if t5145 {
            var t5146 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Ok{
                _0: jp5139,
            }
            return t5146
        } else {
            var t5147 string
            var inline10114 string = "trailing JSON data"
            var inline10115 string = "" + inline10114
            var inline10116 string = inline10115 + " at byte "
            var inline10117 *ref_int_x = parser__245.index
            var inline10118 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(inline10117)
            var inline10119 string = _goml_m_inherent_i_int_i_int_i_to__string(inline10118)
            var inline10120 string = inline10116 + inline10119
            t5147 = inline10120
            var t5148 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
                _0: t5147,
            }
            return t5148
        }
    case _goml_m_Result____std_p_json_p_Value____string_Err:
        var x830 string = mtmp828.(_goml_m_Result____std_p_json_p_Value____string_Err)._0
        var t5149 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string_Err{
            _0: x830,
        }
        return t5149
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    var inline10160 rune = 34
    var inline10161 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10160)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10161)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int
    var inline10158 int = _goml_runtime_core_string_len(value__249)
    for_limit834 = inline10158
    Loop_loop5163:
    for {
        var t5164 bool = for_index833 < for_limit834
        if t5164 {
            var for_item835 int = for_index833
            var t5165 int = for_index833 + 1
            for_index833 = t5165
            var byte__252 uint8
            var inline10146 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline10146
            var t5218 bool = byte__252 == 34
            var jp5216 bool
            if t5218 {
                jp5216 = true
            } else {
                var t5219 bool = byte__252 == 92
                jp5216 = t5219
            }
            var jp5213 bool
            if jp5216 {
                jp5213 = true
            } else {
                var t5217 bool = byte__252 == 8
                jp5213 = t5217
            }
            var jp5210 bool
            if jp5213 {
                jp5210 = true
            } else {
                var t5214 bool = byte__252 == 9
                jp5210 = t5214
            }
            var jp5207 bool
            if jp5210 {
                jp5207 = true
            } else {
                var t5211 bool = byte__252 == 10
                jp5207 = t5211
            }
            var jp5204 bool
            if jp5207 {
                jp5204 = true
            } else {
                var t5208 bool = byte__252 == 12
                jp5204 = t5208
            }
            var jp5201 bool
            if jp5204 {
                jp5201 = true
            } else {
                var t5205 bool = byte__252 == 13
                jp5201 = t5205
            }
            var jp5168 bool
            if jp5201 {
                jp5168 = true
            } else {
                var t5202 bool = byte__252 < 32
                jp5168 = t5202
            }
            if jp5168 {
                var t5197 bool = start__250 < for_item835
                if t5197 {
                    var t5198 string
                    var inline10132 string = string_byte_slice(value__249, start__250, for_item835)
                    t5198 = inline10132
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5198)
                } else {}
                var t5172 bool = byte__252 == 34
                if t5172 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t5175 bool = byte__252 == 92
                    if t5175 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t5178 bool = byte__252 == 8
                        if t5178 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t5181 bool = byte__252 == 9
                            if t5181 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t5184 bool = byte__252 == 10
                                if t5184 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t5187 bool = byte__252 == 12
                                    if t5187 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t5190 bool = byte__252 == 13
                                        if t5190 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t5192 uint8 = byte__252 / 16
                                            var t5193 rune
                                            var inline10143 int = int(uint8(t5192))
                                            var inline10144 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10143)
                                            t5193 = inline10144
                                            var inline10140 string = _goml_m_inherent_i_char_i_char_i_to__string(t5193)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10140)
                                            var t5194_rhs uint8 = 16
                                            var t5194 uint8 = byte__252 % t5194_rhs
                                            var t5195 rune
                                            var inline10137 int = int(uint8(t5194))
                                            var inline10138 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline10137)
                                            t5195 = inline10138
                                            var inline10134 string = _goml_m_inherent_i_char_i_char_i_to__string(t5195)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10134)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t5171 int = for_item835 + 1
                start__250 = t5171
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop5163
        }
    }
    var t5158 int
    var inline10156 int = _goml_runtime_core_string_len(value__249)
    t5158 = inline10156
    var t5159 bool = start__250 < t5158
    if t5159 {
        var t5160 int
        var inline10150 int = _goml_runtime_core_string_len(value__249)
        t5160 = inline10150
        var t5161 string
        var inline10148 string = string_byte_slice(value__249, start__250, t5160)
        t5161 = inline10148
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5161)
    } else {}
    var inline10152 rune = 34
    var inline10153 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10152)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline10153)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline10176 rune = 123
        var inline10177 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10176)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10177)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop5224:
        for {
            var t5225 bool = for_index852 < for_limit851
            if t5225 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t5226 int = for_index852 + 1
                for_index852 = t5226
                var t5232 bool = index__256 > 0
                if t5232 {
                    var inline10164 rune = 44
                    var inline10165 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10164)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10165)
                } else {}
                var t5228 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t5228)
                var inline10168 rune = 58
                var inline10169 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10168)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10169)
                var t5229 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t5229)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t5230 int = compound_old859 + compound_value860
                index__256 = t5230
                continue
            } else {
                break Loop_loop5224
            }
        }
        var inline10172 rune = 125
        var inline10173 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10172)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10173)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline10188 rune = 91
        var inline10189 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10188)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10189)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop5236:
        for {
            var t5237 bool = for_index866 < for_limit865
            if t5237 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t5238 int = for_index866 + 1
                for_index866 = t5238
                var t5242 bool = index__259 > 0
                if t5242 {
                    var inline10180 rune = 44
                    var inline10181 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10180)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10181)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t5240 int = compound_old871 + compound_value872
                index__259 = t5240
                continue
            } else {
                break Loop_loop5236
            }
        }
        var inline10184 rune = 93
        var inline10185 string = _goml_m_inherent_i_char_i_char_i_to__string(inline10184)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline10185)
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
        var jp5247 string
        if x848 {
            jp5247 = "true"
        } else {
            jp5247 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp5247)
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
    var inline10197 *_goml_vec_uint8 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8()
    var inline10198 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline10197,
    }
    builder__265 = inline10198
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline10192 *_goml_vec_uint8 = builder__265.values
    var inline10193 Tuple2_4bool_6string = string_from_utf8(inline10192)
    var inline10194 string = inline10193._1
    return inline10194
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop5258:
        for {
            var t5259 bool = for_index883 < for_limit882
            if t5259 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t5260 int = for_index883 + 1
                for_index883 = t5260
                var t5262 string = for_item884._0
                var t5263 bool = t5262 == name__267
                if t5263 {
                    var t5264 _goml_m_std_p_json_p_Value = for_item884._1
                    var t5265 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value_Some{
                        _0: t5264,
                    }
                    return t5265
                } else {
                    continue
                }
            } else {
                break Loop_loop5258
            }
        }
        return _goml_m_Option____std_p_json_p_Value_None{}
    default:
        return _goml_m_Option____std_p_json_p_Value_None{}
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__int {
    var t5275 int
    var inline10209 int = _goml_runtime_core_string_len(value__272)
    t5275 = inline10209
    var t5276 bool = t5275 == 0
    if t5276 {
        return Option__int_None{}
    } else {
        var t5277 uint8
        var inline10206 int = 0
        var inline10207 uint8 = _goml_runtime_core_string_byte_get(value__272, inline10206)
        t5277 = inline10207
        var negative__273 bool = t5277 == 45
        var jp5279 int
        if negative__273 {
            jp5279 = 1
        } else {
            jp5279 = 0
        }
        var index__274 int = jp5279
        var result__275 int = 0
        var t5300 int
        var inline10204 int = _goml_runtime_core_string_len(value__272)
        t5300 = inline10204
        var t5301 bool = index__274 == t5300
        if t5301 {
            return Option__int_None{}
        } else {
            Loop_loop5286:
            for {
                var t5287 int
                var inline10202 int = _goml_runtime_core_string_len(value__272)
                t5287 = inline10202
                var t5288 bool = index__274 < t5287
                if t5288 {
                    var byte__276 uint8
                    var inline10200 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline10200
                    var t5298 bool = byte__276 < 48
                    var jp5293 bool
                    if t5298 {
                        jp5293 = true
                    } else {
                        var t5299 bool = byte__276 > 57
                        jp5293 = t5299
                    }
                    if jp5293 {
                        return Option__int_None{}
                    } else {
                        var t5294 int = result__275 * 10
                        var t5295 uint8 = byte__276 - 48
                        var t5296 int = int(uint8(t5295))
                        var t5297 int = t5294 + t5296
                        result__275 = t5297
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t5290 int = compound_old895 + compound_value896
                        index__274 = t5290
                        continue
                    }
                } else {
                    break Loop_loop5286
                }
            }
            var jp5283 int
            if negative__273 {
                var t5285 int = 0 - result__275
                jp5283 = t5285
            } else {
                jp5283 = result__275
            }
            var t5284 Option__int = Option__int_Some{
                _0: jp5283,
            }
            return t5284
        }
    }
}

func main0() struct{} {
    var mtmp172 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp6073 _goml_m_std_p_json_p_Value
    switch mtmp172.(type) {
    case _goml_m_Result____std_p_json_p_Value____string_Ok:
        var x173 _goml_m_std_p_json_p_Value = mtmp172.(_goml_m_Result____std_p_json_p_Value____string_Ok)._0
        jp6073 = x173
        var mtmp176 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6073, "name")
        switch mtmp176.(type) {
        case _goml_m_Option____std_p_json_p_Value_None:
            var inline10656 string = "missing name"
            var inline10657 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10656)
            _goml_runtime_core_string_println(inline10657)
            var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6073, "version")
            switch mtmp181.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10671 string = "missing version"
                var inline10672 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10671)
                _goml_runtime_core_string_println(inline10672)
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var mtmp183 Option__int
                switch x182.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline10682 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline10684 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10682)
                    mtmp183 = inline10684
                default:
                    mtmp183 = Option__int_None{}
                }
                switch mtmp183.(type) {
                case Option__int_None:
                    var inline10675 string = "invalid version"
                    var inline10676 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10675)
                    _goml_runtime_core_string_println(inline10676)
                case Option__int_Some:
                    var x184 int = mtmp183.(Option__int_Some)._0
                    var inline10679 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                    _goml_runtime_core_string_println(inline10679)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6073, "stable")
            switch mtmp186.(type) {
            case _goml_m_Option____std_p_json_p_Value_None:
                var inline10686 string = "missing stable"
                var inline10687 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10686)
                _goml_runtime_core_string_println(inline10687)
                var t6077 string = _goml_m_std_p_json_p_encode(jp6073)
                println__T_string(t6077)
                return struct{}{}
            case _goml_m_Option____std_p_json_p_Value_Some:
                var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                var commute_field10952 bool
                switch x187.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline10697 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field10952 = inline10697
                    var inline10694 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field10952)
                    _goml_runtime_core_string_println(inline10694)
                    var t6077 string = _goml_m_std_p_json_p_encode(jp6073)
                    println__T_string(t6077)
                    return struct{}{}
                default:
                    var inline10690 string = "invalid stable"
                    var inline10691 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10690)
                    _goml_runtime_core_string_println(inline10691)
                    var t6077 string = _goml_m_std_p_json_p_encode(jp6073)
                    println__T_string(t6077)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case _goml_m_Option____std_p_json_p_Value_Some:
            var x177 _goml_m_std_p_json_p_Value = mtmp176.(_goml_m_Option____std_p_json_p_Value_Some)._0
            var commute_field10958 string
            switch x177.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline10667 string = x177.(_goml_m_std_p_json_p_Value_String)._0
                commute_field10958 = inline10667
                var inline10664 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field10958)
                _goml_runtime_core_string_println(inline10664)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6073, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10671 string = "missing version"
                    var inline10672 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10671)
                    _goml_runtime_core_string_println(inline10672)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10682 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10684 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10682)
                        mtmp183 = inline10684
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline10675 string = "invalid version"
                        var inline10676 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10675)
                        _goml_runtime_core_string_println(inline10676)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline10679 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline10679)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6073, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10686 string = "missing stable"
                    var inline10687 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10686)
                    _goml_runtime_core_string_println(inline10687)
                    var t6077 string = _goml_m_std_p_json_p_encode(jp6073)
                    println__T_string(t6077)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field10952 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10697 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field10952 = inline10697
                        var inline10694 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field10952)
                        _goml_runtime_core_string_println(inline10694)
                        var t6077 string = _goml_m_std_p_json_p_encode(jp6073)
                        println__T_string(t6077)
                        return struct{}{}
                    default:
                        var inline10690 string = "invalid stable"
                        var inline10691 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10690)
                        _goml_runtime_core_string_println(inline10691)
                        var t6077 string = _goml_m_std_p_json_p_encode(jp6073)
                        println__T_string(t6077)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline10660 string = "invalid name"
                var inline10661 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10660)
                _goml_runtime_core_string_println(inline10661)
                var mtmp181 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6073, "version")
                switch mtmp181.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10671 string = "missing version"
                    var inline10672 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10671)
                    _goml_runtime_core_string_println(inline10672)
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x182 _goml_m_std_p_json_p_Value = mtmp181.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var mtmp183 Option__int
                    switch x182.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline10682 string = x182.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline10684 Option__int = _goml_m_std_p_json_p_parse__json__int__text(inline10682)
                        mtmp183 = inline10684
                    default:
                        mtmp183 = Option__int_None{}
                    }
                    switch mtmp183.(type) {
                    case Option__int_None:
                        var inline10675 string = "invalid version"
                        var inline10676 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10675)
                        _goml_runtime_core_string_println(inline10676)
                    case Option__int_Some:
                        var x184 int = mtmp183.(Option__int_Some)._0
                        var inline10679 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(x184)
                        _goml_runtime_core_string_println(inline10679)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp186 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6073, "stable")
                switch mtmp186.(type) {
                case _goml_m_Option____std_p_json_p_Value_None:
                    var inline10686 string = "missing stable"
                    var inline10687 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10686)
                    _goml_runtime_core_string_println(inline10687)
                    var t6077 string = _goml_m_std_p_json_p_encode(jp6073)
                    println__T_string(t6077)
                    return struct{}{}
                case _goml_m_Option____std_p_json_p_Value_Some:
                    var x187 _goml_m_std_p_json_p_Value = mtmp186.(_goml_m_Option____std_p_json_p_Value_Some)._0
                    var commute_field10952 bool
                    switch x187.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline10697 bool = x187.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field10952 = inline10697
                        var inline10694 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field10952)
                        _goml_runtime_core_string_println(inline10694)
                        var t6077 string = _goml_m_std_p_json_p_encode(jp6073)
                        println__T_string(t6077)
                        return struct{}{}
                    default:
                        var inline10690 string = "invalid stable"
                        var inline10691 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline10690)
                        _goml_runtime_core_string_println(inline10691)
                        var t6077 string = _goml_m_std_p_json_p_encode(jp6073)
                        println__T_string(t6077)
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
        var inline10653 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x174)
        _goml_runtime_core_string_println(inline10653)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__uint8() *_goml_vec_uint8 {
    var t6093 *_goml_vec_uint8 = vec_new__Vec_5uint8()
    return t6093
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop6133:
    for {
        var t6134 int
        var inline10708 int = _goml_runtime_core_string_len(x12)
        t6134 = inline10708
        var t6135 bool = index__26 < t6134
        if t6135 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t6137 int = compound_old17 + x16
                index__26 = t6137
                continue
            } else {
                var t6139 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6139
            }
        } else {
            break Loop_loop6133
        }
    }
    var t6132 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t6132
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t6169 string = _goml_runtime_core_int_to_string(self__34)
    return t6169
}

func _goml_m_inherent_i_string_i_string_i_get(self__39 string, index__40 int) rune {
    var inline10718 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__39, index__40)
    var inline10719 bool = inline10718._0
    var inline10720 rune = inline10718._1
    if inline10719 {
        return inline10720
    } else {
        var inline10723 rune = _goml_runtime_core_string_get("", -1)
        return inline10723
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__257 int) *ref_int_x {
    var t6254 *ref_int_x = ref__Ref_3int(value__257)
    return t6254
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__258 *ref_int_x) int {
    var t6257 int = ref_get__Ref_3int(self__258)
    return t6257
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__259 *ref_int_x, value__260 int) struct{} {
    ref_set__Ref_3int(self__259, value__260)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__36 rune) string {
    var inline10730 uint32 = uint32(rune(self__36))
    var inline10731 bool = utf8_valid_scalar(inline10730)
    if inline10731 {
        var inline10732 string = _goml_runtime_core_char_to_string(self__36)
        return inline10732
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t6322 int = _goml_runtime_core_string_len(self__38)
    return t6322
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t6325 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t6325
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__43 string, start__44 int, end__45 int) string {
    var inline10735 bool = string_is_char_boundary(self__43, start__44)
    var inline10737 bool
    if inline10735 {
        var inline10740 bool = string_is_char_boundary(self__43, end__45)
        inline10737 = inline10740
    } else {
        inline10737 = false
    }
    if inline10737 {
        var inline10738 string = _goml_runtime_core_string_byte_slice(self__43, start__44, end__45)
        return inline10738
    } else {
        var inline10739 string = _goml_runtime_core_string_byte_slice(self__43, -1, -1)
        return inline10739
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t6433 bool
    var inline10782 bool = value__32 <= 1114111
    if inline10782 {
        var inline10783 bool = value__32 >= 55296
        var inline10785 bool
        if inline10783 {
            var inline10787 bool = value__32 <= 57343
            inline10785 = inline10787
        } else {
            inline10785 = false
        }
        var inline10786 bool = !inline10785
        t6433 = inline10786
    } else {
        t6433 = false
    }
    if t6433 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t6434 Option__char = Option__char_Some{
            _0: x24,
        }
        return t6434
    } else {
        return Option__char_None{}
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__std_p_json_p_Value() *_goml_vec__goml_m_std_p_json_p_Value {
    var t6437 *_goml_vec__goml_m_std_p_json_p_Value = vec_new___goml_m_Vec__16std_p_json_p_Value()
    return t6437
}

func _goml_m_inherent_i_Vec_i_Vec_l_h57e42dbef834e6b8ee6cf77cf9eb9d23_json_p_Value_q_() *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
    var t6442 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = vec_new___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value()
    return t6442
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__176 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__177 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__176, elem__177)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t6478 string
    t6478 = value__31
    _goml_runtime_core_string_println(t6478)
    return struct{}{}
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t6604 bool = index__6 < 0
    var jp6602 bool
    if t6604 {
        jp6602 = true
    } else {
        var t6605 bool = index__6 >= length__7
        jp6602 = t6605
    }
    if jp6602 {
        var inline10798 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline10798
    } else {
        var t6489 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t6489))
        var t6492 bool = first__8 < 128
        if t6492 {
            var inline10800 int = 1
            var inline10801 Option__char = char_from_uint32(first__8)
            switch inline10801.(type) {
            case Option__char_None:
                var inline10802 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline10802
            case Option__char_Some:
                var inline10803 rune = inline10801.(Option__char_Some)._0
                var inline10805 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline10803,
                    _2: inline10800,
                }
                return inline10805
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t6496 bool = first__8 < 194
            if t6496 {
                var inline10807 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline10807
            } else {
                var t6500 bool = first__8 < 224
                if t6500 {
                    var t6513 int = length__7 - index__6
                    var t6514 bool = t6513 < 2
                    if t6514 {
                        var inline10809 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline10809
                    } else {
                        var t6502 int = index__6 + 1
                        var t6503 uint8
                        var inline10823 uint8 = _goml_runtime_core_string_byte_get(value__5, t6502)
                        t6503 = inline10823
                        var second__9 uint32 = uint32(uint8(t6503))
                        var t6506 bool
                        var inline10820 bool = second__9 < 128
                        if inline10820 {
                            t6506 = true
                        } else {
                            var inline10821 bool = second__9 > 191
                            t6506 = inline10821
                        }
                        if t6506 {
                            var inline10811 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline10811
                        } else {
                            var t6508_rhs uint32 = 31
                            var t6508 uint32 = first__8 & t6508_rhs
                            var t6509_rhs int = 6
                            var t6509 uint32 = t6508 << t6509_rhs
                            var t6510_rhs uint32 = 63
                            var t6510 uint32 = second__9 & t6510_rhs
                            var t6511 uint32 = t6509 | t6510
                            var inline10813 int = 2
                            var inline10814 Option__char = char_from_uint32(t6511)
                            switch inline10814.(type) {
                            case Option__char_None:
                                var inline10815 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline10815
                            case Option__char_Some:
                                var inline10816 rune = inline10814.(Option__char_Some)._0
                                var inline10818 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline10816,
                                    _2: inline10813,
                                }
                                return inline10818
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t6518 bool = first__8 < 240
                    if t6518 {
                        var t6551 int = length__7 - index__6
                        var t6552 bool = t6551 < 3
                        if t6552 {
                            var inline10825 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline10825
                        } else {
                            var t6520 int = index__6 + 1
                            var t6521 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6520)
                            var second__10 uint32 = uint32(uint8(t6521))
                            var t6522 int = index__6 + 2
                            var t6523 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6522)
                            var third__11 uint32 = uint32(uint8(t6523))
                            var t6549 bool = utf8_invalid_continuation(second__10)
                            var jp6544 bool
                            if t6549 {
                                jp6544 = true
                            } else {
                                var inline10827 bool = third__11 < 128
                                if inline10827 {
                                    jp6544 = true
                                } else {
                                    var inline10828 bool = third__11 > 191
                                    jp6544 = inline10828
                                }
                            }
                            var jp6538 bool
                            if jp6544 {
                                jp6538 = true
                            } else {
                                var t6547 bool = first__8 == 224
                                if t6547 {
                                    var t6548 bool = second__10 < 160
                                    jp6538 = t6548
                                } else {
                                    jp6538 = false
                                }
                            }
                            var jp6527 bool
                            if jp6538 {
                                jp6527 = true
                            } else {
                                var t6541 bool = first__8 == 237
                                if t6541 {
                                    var t6542 bool = second__10 >= 160
                                    jp6527 = t6542
                                } else {
                                    jp6527 = false
                                }
                            }
                            if jp6527 {
                                var inline10830 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline10830
                            } else {
                                var t6529_rhs uint32 = 15
                                var t6529 uint32 = first__8 & t6529_rhs
                                var t6530_rhs int = 12
                                var t6530 uint32 = t6529 << t6530_rhs
                                var t6531_rhs uint32 = 63
                                var t6531 uint32 = second__10 & t6531_rhs
                                var t6532_rhs int = 6
                                var t6532 uint32 = t6531 << t6532_rhs
                                var t6533 uint32 = t6530 | t6532
                                var t6534_rhs uint32 = 63
                                var t6534 uint32 = third__11 & t6534_rhs
                                var t6535 uint32 = t6533 | t6534
                                var inline10832 int = 3
                                var inline10833 Option__char = char_from_uint32(t6535)
                                switch inline10833.(type) {
                                case Option__char_None:
                                    var inline10834 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline10834
                                case Option__char_Some:
                                    var inline10835 rune = inline10833.(Option__char_Some)._0
                                    var inline10837 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline10835,
                                        _2: inline10832,
                                    }
                                    return inline10837
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t6556 bool = first__8 < 245
                        if t6556 {
                            var t6597 int = length__7 - index__6
                            var t6598 bool = t6597 < 4
                            if t6598 {
                                var t6599 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t6599
                            } else {
                                var t6558 int = index__6 + 1
                                var t6559 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6558)
                                var second__12 uint32 = uint32(uint8(t6559))
                                var t6560 int = index__6 + 2
                                var t6561 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6560)
                                var third__13 uint32 = uint32(uint8(t6561))
                                var t6562 int = index__6 + 3
                                var t6563 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t6562)
                                var fourth__14 uint32 = uint32(uint8(t6563))
                                var t6595 bool = utf8_invalid_continuation(second__12)
                                var jp6593 bool
                                if t6595 {
                                    jp6593 = true
                                } else {
                                    var t6596 bool = utf8_invalid_continuation(third__13)
                                    jp6593 = t6596
                                }
                                var jp6587 bool
                                if jp6593 {
                                    jp6587 = true
                                } else {
                                    var t6594 bool = utf8_invalid_continuation(fourth__14)
                                    jp6587 = t6594
                                }
                                var jp6581 bool
                                if jp6587 {
                                    jp6581 = true
                                } else {
                                    var t6590 bool = first__8 == 240
                                    if t6590 {
                                        var t6591 bool = second__12 < 144
                                        jp6581 = t6591
                                    } else {
                                        jp6581 = false
                                    }
                                }
                                var jp6567 bool
                                if jp6581 {
                                    jp6567 = true
                                } else {
                                    var t6584 bool = first__8 == 244
                                    if t6584 {
                                        var t6585 bool = second__12 > 143
                                        jp6567 = t6585
                                    } else {
                                        jp6567 = false
                                    }
                                }
                                if jp6567 {
                                    var t6568 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t6568
                                } else {
                                    var t6569_rhs uint32 = 7
                                    var t6569 uint32 = first__8 & t6569_rhs
                                    var t6570_rhs int = 18
                                    var t6570 uint32 = t6569 << t6570_rhs
                                    var t6571_rhs uint32 = 63
                                    var t6571 uint32 = second__12 & t6571_rhs
                                    var t6572_rhs int = 12
                                    var t6572 uint32 = t6571 << t6572_rhs
                                    var t6573 uint32 = t6570 | t6572
                                    var t6574_rhs uint32 = 63
                                    var t6574 uint32 = third__13 & t6574_rhs
                                    var t6575_rhs int = 6
                                    var t6575 uint32 = t6574 << t6575_rhs
                                    var t6576 uint32 = t6573 | t6575
                                    var t6577_rhs uint32 = 63
                                    var t6577 uint32 = fourth__14 & t6577_rhs
                                    var t6578 uint32 = t6576 | t6577
                                    var t6579 Tuple3_4bool_4char_3int = utf8_valid_decode(t6578, 4)
                                    return t6579
                                }
                            }
                        } else {
                            var t6600 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t6600
                        }
                    }
                }
            }
        }
    }
}

func char_to_string(value__29 rune) string {
    var t6625 uint32 = uint32(rune(value__29))
    var t6626 bool
    var inline10839 bool = t6625 <= 1114111
    if inline10839 {
        var inline10840 bool = t6625 >= 55296
        var inline10842 bool
        if inline10840 {
            var inline10844 bool = t6625 <= 57343
            inline10842 = inline10844
        } else {
            inline10842 = false
        }
        var inline10843 bool = !inline10842
        t6626 = inline10843
    } else {
        t6626 = false
    }
    if t6626 {
        var t6627 string = _goml_runtime_core_char_to_string(value__29)
        return t6627
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t6641 bool = index__16 < 0
    var jp6633 bool
    if t6641 {
        jp6633 = true
    } else {
        var t6642 int
        var inline10846 int = _goml_runtime_core_string_len(value__15)
        t6642 = inline10846
        var t6643 bool = index__16 > t6642
        jp6633 = t6643
    }
    if jp6633 {
        return false
    } else {
        var t6636 int
        var inline10850 int = _goml_runtime_core_string_len(value__15)
        t6636 = inline10850
        var t6637 bool = index__16 == t6636
        if t6637 {
            return true
        } else {
            var t6638 uint8
            var inline10848 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t6638 = inline10848
            var t6639_rhs uint8 = 192
            var t6639 uint8 = t6638 & t6639_rhs
            var t6640 bool = t6639 != 128
            return t6640
        }
    }
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t6652 bool = string_is_char_boundary(value__21, start__22)
    var jp6649 bool
    if t6652 {
        var t6653 bool = string_is_char_boundary(value__21, end__23)
        jp6649 = t6653
    } else {
        jp6649 = false
    }
    if jp6649 {
        var t6650 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t6650
    } else {
        var t6651 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t6651
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t6679 bool = value__4 <= 1114111
    if t6679 {
        var t6683 bool = value__4 >= 55296
        var jp6681 bool
        if t6683 {
            var t6684 bool = value__4 <= 57343
            jp6681 = t6684
        } else {
            jp6681 = false
        }
        var t6682 bool = !jp6681
        return t6682
    } else {
        return false
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t6691 string = _goml_runtime_core_int_to_string(self__69)
    return t6691
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t6694 string = _goml_runtime_core_bool_to_string(self__66)
    return t6694
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t6697 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t6697
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field10964 rune
    var inline10856 bool = utf8_valid_scalar(value__0)
    if inline10856 {
        var inline10857 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline10858 rune = inline10857._1
        commute_field10964 = inline10858
        var t6703 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field10964,
            _2: width__1,
        }
        return t6703
    } else {
        var inline10854 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline10854
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t6708 bool = value__3 < 128
    if t6708 {
        return true
    } else {
        var t6709 bool = value__3 > 191
        return t6709
    }
}

func main() {
    main0()
}
