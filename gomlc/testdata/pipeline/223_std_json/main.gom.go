package main

import (
    _goml_context "context"
    _goml_os "os"
    _goml_sync "sync"
)

type _goml_task_scope_state struct {
    mu _goml_sync.Mutex
    wg _goml_sync.WaitGroup
    state int
    ctx _goml_context.Context
    cancel _goml_context.CancelFunc
    panicked bool
    panic_value any
}

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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_string struct {
    items []string
}

type _goml_vec_uint8 struct {
    items []uint8
}

func vec_with_capacity__Vec_5uint8(capacity int) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: make([]uint8, 0, capacity),
    }
}

func vec_push__Vec_5uint8(vec *_goml_vec_uint8, elem uint8) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5uint8(vec *_goml_vec_uint8, index int) uint8 {
    return vec.items[index]
}

func vec_len__Vec_5uint8(vec *_goml_vec_uint8) int {
    return int(len(vec.items))
}

func vec_reserve__Vec_5uint8(vec *_goml_vec_uint8, additional int) struct{} {
    if additional < 0 {
        panic("negative vector capacity")
    }
    var length int = len(vec.items)
    var required int = length + additional
    if required < length {
        panic("vector capacity overflow")
    }
    if required > cap(vec.items) {
        var next_capacity int = cap(vec.items) * 2
        if next_capacity < required {
            next_capacity = required
        }
        var next_items []uint8 = make([]uint8, length, next_capacity)
        copy(next_items, vec.items)
        vec.items = next_items
    }
    return struct{}{}
}

type _goml_vec_Tuple2_6string_6string struct {
    items []Tuple2_6string_6string
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

type _goml_vec__goml_m_std_p_json_p_JsonDeserializeFrame struct {
    items []_goml_m_std_p_json_p_JsonDeserializeFrame
}

type _goml_vec__goml_m_std_p_json_p_Value struct {
    items []_goml_m_std_p_json_p_Value
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

type _goml_vec_uint32 struct {
    items []uint32
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

type Tuple3_4bool_6string_6string struct {
    _0 bool
    _1 string
    _2 string
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_10Vec_5uint8_6string struct {
    _0 bool
    _1 *_goml_vec_uint8
    _2 string
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

type Tuple9_4bool_3int_5int64_6uint32_5int64_3int_4bool_3int_6string struct {
    _0 bool
    _1 int
    _2 int64
    _3 uint32
    _4 int64
    _5 int
    _6 bool
    _7 int
    _8 string
}

type Tuple3_4bool_11Vec_6string_6string struct {
    _0 bool
    _1 *_goml_vec_string
    _2 string
}

type Tuple6_4bool_11Vec_6string_3int_4bool_3int_6string struct {
    _0 bool
    _1 *_goml_vec_string
    _2 int
    _3 bool
    _4 int
    _5 string
}

type Tuple2_6string_6string struct {
    _0 string
    _1 string
}

type Tuple5_4bool_3int_10Vec_5uint8_10Vec_5uint8_6string struct {
    _0 bool
    _1 int
    _2 *_goml_vec_uint8
    _3 *_goml_vec_uint8
    _4 string
}

type Tuple6_4bool_3int_10Vec_5uint8_10Vec_5uint8_6string_4bool struct {
    _0 bool
    _1 int
    _2 *_goml_vec_uint8
    _3 *_goml_vec_uint8
    _4 string
    _5 bool
}

type Tuple3_4bool_3int_6string struct {
    _0 bool
    _1 int
    _2 string
}

type Tuple4_4bool_3int_6string_4bool struct {
    _0 bool
    _1 int
    _2 string
    _3 bool
}

type Tuple2_5int64_14Receiver_4unit struct {
    _0 int64
    _1 <-chan struct{}
}

type Tuple4_4bool_3int_3int_4bool struct {
    _0 bool
    _1 int
    _2 int
    _3 bool
}

type Tuple3_4bool_6uint64_6string struct {
    _0 bool
    _1 uint64
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

type Tuple2_4bool_7float64 struct {
    _0 bool
    _1 float64
}

type Tuple2_4bool_6uint64 struct {
    _0 bool
    _1 uint64
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

type Tuple2_6string_26_goml_m_std_p_json_p_Value struct {
    _0 string
    _1 _goml_m_std_p_json_p_Value
}

type Tuple2_13Option__isize_13Option__isize struct {
    _0 Option__isize
    _1 Option__isize
}

type Tuple2_3int_4char struct {
    _0 int
    _1 rune
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
}

type Tuple2_6uint64_4bool struct {
    _0 uint64
    _1 bool
}

type Tuple2_4bool_3int struct {
    _0 bool
    _1 int
}

type Tuple2_6string_4bool struct {
    _0 string
    _1 bool
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type _goml_m_std_p_io_p_ErrorDetails struct {
    kind_value _goml_m_std_p_io_p_ErrorKind
    operation_value string
    context_value Option__string
    raw_os_code_value Option__isize
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

type _goml_m_FnIterator_____o_isize_c_char_q_ struct {
    next_fn func() _goml_m_Option_____o_isize_c_char_q_
}

type FnIterator__char struct {
    next_fn func() Option__char
}

type FnIterator__isize struct {
    next_fn func() Option__isize
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

type Ordering int32

type _goml_m_std_p_io_p_ErrorKind int32

type _goml_m_std_p_serde_p_FieldKey struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type _goml_m_std_p_serde_p_VariantKey struct {
    _tag int32
    _v0_0 int
    _v1_0 string
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

type Option__u8 struct {
    _tag int32
    _v1_0 uint8
}

type Result__string__string struct {
    _tag int32
    _v0_0 string
    _v1_0 string
}

type Option__string struct {
    _tag int32
    _v1_0 string
}

type Option__isize struct {
    _tag int32
    _v1_0 int
}

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

type _goml_m_Result____std_p_bytes_p_Bytes____string struct {
    _tag int32
    _v0_0 _goml_m_std_p_bytes_p_Bytes
    _v1_0 string
}

type Result__unit__string struct {
    _tag int32
    _v0_0 struct{}
    _v1_0 string
}

type Result__isize__string struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type Result__usize__string struct {
    _tag int32
    _v0_0 uint
    _v1_0 string
}

type Result__f32__string struct {
    _tag int32
    _v0_0 float32
    _v1_0 string
}

type Result__f64__string struct {
    _tag int32
    _v0_0 float64
    _v1_0 string
}

type _goml_m_Result____isize____std_p_num_p_ParseIntError interface {
    is_goml_m_Result____isize____std_p_num_p_ParseIntError()
}

type _goml_m_Result____isize____std_p_num_p_ParseIntError_Ok struct {
    _0 int
}

func (_ _goml_m_Result____isize____std_p_num_p_ParseIntError_Ok) is_goml_m_Result____isize____std_p_num_p_ParseIntError() {}

type _goml_m_Result____isize____std_p_num_p_ParseIntError_Err struct {
    _0 _goml_m_std_p_num_p_ParseIntError
}

func (_ _goml_m_Result____isize____std_p_num_p_ParseIntError_Err) is_goml_m_Result____isize____std_p_num_p_ParseIntError() {}

type _goml_m_Result____usize____std_p_num_p_ParseIntError interface {
    is_goml_m_Result____usize____std_p_num_p_ParseIntError()
}

type _goml_m_Result____usize____std_p_num_p_ParseIntError_Ok struct {
    _0 uint
}

func (_ _goml_m_Result____usize____std_p_num_p_ParseIntError_Ok) is_goml_m_Result____usize____std_p_num_p_ParseIntError() {}

type _goml_m_Result____usize____std_p_num_p_ParseIntError_Err struct {
    _0 _goml_m_std_p_num_p_ParseIntError
}

func (_ _goml_m_Result____usize____std_p_num_p_ParseIntError_Err) is_goml_m_Result____usize____std_p_num_p_ParseIntError() {}

type _goml_m_Result____f32____std_p_num_p_ParseFloatError interface {
    is_goml_m_Result____f32____std_p_num_p_ParseFloatError()
}

type _goml_m_Result____f32____std_p_num_p_ParseFloatError_Ok struct {
    _0 float32
}

func (_ _goml_m_Result____f32____std_p_num_p_ParseFloatError_Ok) is_goml_m_Result____f32____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____f32____std_p_num_p_ParseFloatError_Err struct {
    _0 _goml_m_std_p_num_p_ParseFloatError
}

func (_ _goml_m_Result____f32____std_p_num_p_ParseFloatError_Err) is_goml_m_Result____f32____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____f64____std_p_num_p_ParseFloatError interface {
    is_goml_m_Result____f64____std_p_num_p_ParseFloatError()
}

type _goml_m_Result____f64____std_p_num_p_ParseFloatError_Ok struct {
    _0 float64
}

func (_ _goml_m_Result____f64____std_p_num_p_ParseFloatError_Ok) is_goml_m_Result____f64____std_p_num_p_ParseFloatError() {}

type _goml_m_Result____f64____std_p_num_p_ParseFloatError_Err struct {
    _0 _goml_m_std_p_num_p_ParseFloatError
}

func (_ _goml_m_Result____f64____std_p_num_p_ParseFloatError_Err) is_goml_m_Result____f64____std_p_num_p_ParseFloatError() {}

type Option__i64 struct {
    _tag int32
    _v1_0 int64
}

type _goml_m_Option____std_p_serde_p_FieldKey struct {
    _tag int32
    _v1_0 _goml_m_std_p_serde_p_FieldKey
}

type _goml_m_Result____std_p_serde_p_Value____string struct {
    _tag int32
    _v0_0 _goml_m_std_p_serde_p_Value
    _v1_0 string
}

type _goml_m_Result_____o_string_c__h0fd03a31a9a9f0946c70cfe7367b8f54_r__q_____string struct {
    _tag int32
    _v0_0 Tuple3_6string_3int_50Vec_44Tuple2_6string_27_goml_m_std_p_serde_p_Value
    _v1_0 string
}

type _goml_m_Result____Vec_l_std_p_serde_p_Value_r_____string struct {
    _tag int32
    _v0_0 *_goml_vec__goml_m_std_p_serde_p_Value
    _v1_0 string
}

type _goml_m_Option____std_p_serde_p_Value struct {
    _tag int32
    _v1_0 _goml_m_std_p_serde_p_Value
}

type _goml_m_Option____std_p_serde_p_ValueDeserializeFrame struct {
    _tag int32
    _v1_0 _goml_m_std_p_serde_p_ValueDeserializeFrame
}

type _goml_m_Result____Vec_l_u8_r_____string struct {
    _tag int32
    _v0_0 *_goml_vec_uint8
    _v1_0 string
}

type Result__bool__string struct {
    _tag int32
    _v0_0 bool
    _v1_0 string
}

type Result__i8__string struct {
    _tag int32
    _v0_0 int8
    _v1_0 string
}

type Result__i16__string struct {
    _tag int32
    _v0_0 int16
    _v1_0 string
}

type Result__i32__string struct {
    _tag int32
    _v0_0 int32
    _v1_0 string
}

type Result__i64__string struct {
    _tag int32
    _v0_0 int64
    _v1_0 string
}

type Result__u8__string struct {
    _tag int32
    _v0_0 uint8
    _v1_0 string
}

type Result__u16__string struct {
    _tag int32
    _v0_0 uint16
    _v1_0 string
}

type Result__u32__string struct {
    _tag int32
    _v0_0 uint32
    _v1_0 string
}

type Result__u64__string struct {
    _tag int32
    _v0_0 uint64
    _v1_0 string
}

type Result__char__string struct {
    _tag int32
    _v0_0 rune
    _v1_0 string
}

type Result__Option__isize__string struct {
    _tag int32
    _v0_0 Option__isize
    _v1_0 string
}

type _goml_m_Result____Option____std_p_serde_p_FieldKey____string struct {
    _tag int32
    _v0_0 _goml_m_Option____std_p_serde_p_FieldKey
    _v1_0 string
}

type _goml_m_Result____std_p_serde_p_VariantKey____string struct {
    _tag int32
    _v0_0 _goml_m_std_p_serde_p_VariantKey
    _v1_0 string
}

type _goml_m_Option____std_p_serde_p_ValueSerializeFrame struct {
    _tag int32
    _v1_0 _goml_m_std_p_serde_p_ValueSerializeFrame
}

type _goml_m_Option_____o_isize_c_char_q_ struct {
    _tag int32
    _v1_0 Tuple2_3int_4char
}

type _goml_m_Option_____o_string_c_string_q_ struct {
    _tag int32
    _v1_0 Tuple2_6string_6string
}

type _goml_m_Option____std_p_json_p_JsonDeserializeFrame struct {
    _tag int32
    _v1_0 _goml_m_std_p_json_p_JsonDeserializeFrame
}

type _goml_m_Result_____o_bool_c_bool_c_bool_q_____string struct {
    _tag int32
    _v0_0 Tuple3_4bool_4bool_4bool
    _v1_0 string
}

type _goml_m_Result_____o_bool_c_bool_c_Option____string_q_____string struct {
    _tag int32
    _v0_0 Tuple3_4bool_4bool_14Option__string
    _v1_0 string
}

type Result__Option__string__string struct {
    _tag int32
    _v0_0 Option__string
    _v1_0 string
}

type _goml_m_Result____std_p_json_p_Value____string struct {
    _tag int32
    _v0_0 _goml_m_std_p_json_p_Value
    _v1_0 string
}

type _goml_m_Option_____o_char_c_isize_q_ struct {
    _tag int32
    _v1_0 Tuple2_4char_3int
}

type Option__u32 struct {
    _tag int32
    _v1_0 uint32
}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

type _goml_m_Option____std_p_json_p_Value struct {
    _tag int32
    _v1_0 _goml_m_std_p_json_p_Value
}

type Option__bool struct {
    _tag int32
    _v1_0 bool
}

type _goml_m_Option____Vec_l_std_p_json_p_Value_r_ struct {
    _tag int32
    _v1_0 *_goml_vec__goml_m_std_p_json_p_Value
}

type _goml_m_Option____std_p_json_p_JsonSerializeFrame struct {
    _tag int32
    _v1_0 _goml_m_std_p_json_p_JsonSerializeFrame
}

func _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new() _goml_m_std_p_text_p_StringBuilder {
    var t3499 [0]uint8 = [0]uint8{}
    var t3500 *_goml_vec_uint8 = func(values [0]uint8) *_goml_vec_uint8 {
        return &_goml_vec_uint8{
            items: values[0:len(values)],
        }
    }(t3499)
    var t3501 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: t3500,
    }
    return t3501
}

func _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__3 _goml_m_std_p_text_p_StringBuilder, value__4 string) struct{} {
    var length__5 int
    var inline10522 int = _goml_runtime_core_string_len(value__4)
    length__5 = inline10522
    var t3515 *_goml_vec_uint8 = self__3.values
    vec_reserve__Vec_5uint8(t3515, length__5)
    var for_index1 int = 0
    Loop_loop3517:
    for {
        var t3518 bool = for_index1 < length__5
        if t3518 {
            var for_item3 int = for_index1
            var t3519 int = for_index1 + 1
            for_index1 = t3519
            var t3520 *_goml_vec_uint8 = self__3.values
            var t3521 uint8
            var inline10518 uint8 = _goml_runtime_core_string_byte_get(value__4, for_item3)
            t3521 = inline10518
            vec_push__Vec_5uint8(t3520, t3521)
            continue
        } else {
            break Loop_loop3517
        }
    }
    return struct{}{}
}

func _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(self__7 _goml_m_std_p_text_p_StringBuilder, value__8 rune) struct{} {
    var t3524 string
    var inline10524 string = char_to_string(value__8)
    t3524 = inline10524
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(self__7, t3524)
    return struct{}{}
}

func _goml_m_std_p_json_p_json__error(value__200 _goml_m_std_p_json_p_JsonParser, message__201 string) string {
    var t5232 string = "" + message__201
    var t5233 string = t5232 + " at byte "
    var t5234 *ref_int_x = value__200.index
    var t5235 int
    var inline11859 int = ref_get__Ref_3int(t5234)
    t5235 = inline11859
    var t5236 string
    var inline11857 string = __goml_builtin_int_to_string(t5235)
    t5236 = inline11857
    var t5237 string = t5233 + t5236
    return t5237
}

func _goml_m_std_p_json_p_skip__json__whitespace(value__203 _goml_m_std_p_json_p_JsonParser) struct{} {
    Loop_loop5252:
    for {
        var t5260 *ref_int_x = value__203.index
        var t5261 int
        var inline11880 int = ref_get__Ref_3int(t5260)
        t5261 = inline11880
        var t5262 string = value__203.input
        var t5263 int
        var inline11878 int = _goml_runtime_core_string_len(t5262)
        t5263 = inline11878
        var t5264 bool = t5261 < t5263
        var jp5254 bool
        if t5264 {
            var t5265 string = value__203.input
            var t5266 *ref_int_x = value__203.index
            var t5267 int
            var inline11872 int = ref_get__Ref_3int(t5266)
            t5267 = inline11872
            var t5268 uint8
            var inline11870 uint8 = _goml_runtime_core_string_byte_get(t5265, t5267)
            t5268 = inline11870
            var inline11861 bool = t5268 == 9
            var inline11863 bool
            if inline11861 {
                inline11863 = true
            } else {
                var inline11868 bool = t5268 == 10
                inline11863 = inline11868
            }
            var inline11865 bool
            if inline11863 {
                inline11865 = true
            } else {
                var inline11867 bool = t5268 == 13
                inline11865 = inline11867
            }
            if inline11865 {
                jp5254 = true
            } else {
                var inline11866 bool = t5268 == 32
                jp5254 = inline11866
            }
        } else {
            jp5254 = false
        }
        if jp5254 {
            var t5255 *ref_int_x = value__203.index
            var t5256 *ref_int_x = value__203.index
            var t5257 int
            var inline11876 int = ref_get__Ref_3int(t5256)
            t5257 = inline11876
            var t5258 int = t5257 + 1
            ref_set__Ref_3int(t5255, t5258)
            continue
        } else {
            break Loop_loop5252
        }
    }
    return struct{}{}
}

func _goml_m_std_p_json_p_hex__digit(value__204 uint8) Option__u32 {
    var t5299 bool = value__204 >= 48
    var jp5275 bool
    if t5299 {
        var t5300 bool = value__204 <= 57
        jp5275 = t5300
    } else {
        jp5275 = false
    }
    if jp5275 {
        var t5276 uint8 = value__204 - 48
        var t5277 uint32 = uint32(uint8(t5276))
        var t5278 Option__u32 = Option__u32{
            _tag: 1,
            _v1_0: t5277,
        }
        return t5278
    } else {
        var t5297 bool = value__204 >= 65
        var jp5282 bool
        if t5297 {
            var t5298 bool = value__204 <= 70
            jp5282 = t5298
        } else {
            jp5282 = false
        }
        if jp5282 {
            var t5283 uint8 = value__204 - 65
            var t5284 uint8 = t5283 + 10
            var t5285 uint32 = uint32(uint8(t5284))
            var t5286 Option__u32 = Option__u32{
                _tag: 1,
                _v1_0: t5285,
            }
            return t5286
        } else {
            var t5295 bool = value__204 >= 97
            var jp5290 bool
            if t5295 {
                var t5296 bool = value__204 <= 102
                jp5290 = t5296
            } else {
                jp5290 = false
            }
            if jp5290 {
                var t5291 uint8 = value__204 - 97
                var t5292 uint8 = t5291 + 10
                var t5293 uint32 = uint32(uint8(t5292))
                var t5294 Option__u32 = Option__u32{
                    _tag: 1,
                    _v1_0: t5293,
                }
                return t5294
            } else {
                return Option__u32{
                    _tag: 0,
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__hex__quad(value__205 _goml_m_std_p_json_p_JsonParser) Result__u32__string {
    var t5305 *ref_int_x = value__205.index
    var t5306 int
    var inline11908 int = ref_get__Ref_3int(t5305)
    t5306 = inline11908
    var t5307 int = t5306 + 4
    var t5308 string = value__205.input
    var t5309 int
    var inline11906 int = _goml_runtime_core_string_len(t5308)
    t5309 = inline11906
    var t5310 bool = t5307 > t5309
    if t5310 {
        var t5311 string
        var inline11882 string = "incomplete unicode escape"
        var inline11883 string = "" + inline11882
        var inline11884 string = inline11883 + " at byte "
        var inline11885 *ref_int_x = value__205.index
        var inline11886 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11885)
        var inline11887 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11886)
        var inline11888 string = inline11884 + inline11887
        t5311 = inline11888
        var t5312 Result__u32__string = Result__u32__string{
            _tag: 1,
            _v1_0: t5311,
        }
        return t5312
    } else {
        var result__206_source int = 0
        var result__206 uint32 = uint32(int(result__206_source))
        var for_index744 int = 0
        var for_limit745 int = 4
        Loop_loop5319:
        for {
            var t5320 bool = for_index744 < for_limit745
            if t5320 {
                var for_item746 int = for_index744
                var t5321 int = for_index744 + 1
                for_index744 = t5321
                var t5322 string = value__205.input
                var t5323 *ref_int_x = value__205.index
                var t5324 int
                var inline11900 int = ref_get__Ref_3int(t5323)
                t5324 = inline11900
                var t5325 int = t5324 + for_item746
                var t5326 uint8
                var inline11898 uint8 = _goml_runtime_core_string_byte_get(t5322, t5325)
                t5326 = inline11898
                var mtmp748 Option__u32 = _goml_m_std_p_json_p_hex__digit(t5326)
                switch mtmp748._tag {
                case 0:
                    var t5328 string
                    var inline11890 string = "invalid unicode escape"
                    var inline11891 string = "" + inline11890
                    var inline11892 string = inline11891 + " at byte "
                    var inline11893 *ref_int_x = value__205.index
                    var inline11894 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11893)
                    var inline11895 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11894)
                    var inline11896 string = inline11892 + inline11895
                    t5328 = inline11896
                    var t5329 Result__u32__string = Result__u32__string{
                        _tag: 1,
                        _v1_0: t5328,
                    }
                    return t5329
                case 1:
                    var x749 uint32 = mtmp748._v1_0
                    var t5330 uint32 = result__206 * 16
                    var t5331 uint32 = t5330 + x749
                    result__206 = t5331
                    continue
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5319
            }
        }
        var t5314 *ref_int_x = value__205.index
        var t5315 *ref_int_x = value__205.index
        var t5316 int
        var inline11904 int = ref_get__Ref_3int(t5315)
        t5316 = inline11904
        var t5317 int = t5316 + 4
        ref_set__Ref_3int(t5314, t5317)
        var t5318 Result__u32__string = Result__u32__string{
            _tag: 0,
            _v0_0: result__206,
        }
        return t5318
    }
}

func _goml_m_std_p_json_p_write__codepoint(value__209 _goml_m_std_p_json_p_JsonParser, builder__210 _goml_m_std_p_text_p_StringBuilder, codepoint__211 uint32) Result__unit__string {
    var mtmp753 Option__char
    var inline11921 Option__char = __goml_builtin_char_from_uint32(codepoint__211)
    mtmp753 = inline11921
    switch mtmp753._tag {
    case 0:
        var t5336 string
        var inline11910 string = "invalid unicode codepoint"
        var inline11911 string = "" + inline11910
        var inline11912 string = inline11911 + " at byte "
        var inline11913 *ref_int_x = value__209.index
        var inline11914 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11913)
        var inline11915 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11914)
        var inline11916 string = inline11912 + inline11915
        t5336 = inline11916
        var t5337 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: t5336,
        }
        return t5337
    case 1:
        var x754 rune = mtmp753._v1_0
        var inline11918 string = _goml_m_inherent_i_char_i_char_i_to__string(x754)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__210, inline11918)
        var t5338 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t5338
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__unicode__escape(value__213 _goml_m_std_p_json_p_JsonParser, builder__214 _goml_m_std_p_text_p_StringBuilder) Result__unit__string {
    var mtmp756 Result__u32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
    var jp5342 uint32
    switch mtmp756._tag {
    case 0:
        var x757 uint32 = mtmp756._v0_0
        jp5342 = x757
        var t5402 bool = jp5342 >= 55296
        var jp5346 bool
        if t5402 {
            var t5403 bool = jp5342 <= 56319
            jp5346 = t5403
        } else {
            jp5346 = false
        }
        if jp5346 {
            var t5382 *ref_int_x = value__213.index
            var t5383 int
            var inline11961 int = ref_get__Ref_3int(t5382)
            t5383 = inline11961
            var t5384 int = t5383 + 2
            var t5385 string = value__213.input
            var t5386 int
            var inline11959 int = _goml_runtime_core_string_len(t5385)
            t5386 = inline11959
            var t5387 bool = t5384 > t5386
            var jp5375 bool
            if t5387 {
                jp5375 = true
            } else {
                var t5388 string = value__213.input
                var t5389 *ref_int_x = value__213.index
                var t5390 int
                var inline11925 int = ref_get__Ref_3int(t5389)
                t5390 = inline11925
                var t5391 uint8
                var inline11923 uint8 = _goml_runtime_core_string_byte_get(t5388, t5390)
                t5391 = inline11923
                var t5392 bool = t5391 != 92
                jp5375 = t5392
            }
            var jp5350 bool
            if jp5375 {
                jp5350 = true
            } else {
                var t5376 string = value__213.input
                var t5377 *ref_int_x = value__213.index
                var t5378 int
                var inline11929 int = ref_get__Ref_3int(t5377)
                t5378 = inline11929
                var t5379 int = t5378 + 1
                var t5380 uint8
                var inline11927 uint8 = _goml_runtime_core_string_byte_get(t5376, t5379)
                t5380 = inline11927
                var t5381 bool = t5380 != 117
                jp5350 = t5381
            }
            if jp5350 {
                var t5351 string
                var inline11931 string = "missing low surrogate"
                var inline11932 string = "" + inline11931
                var inline11933 string = inline11932 + " at byte "
                var inline11934 *ref_int_x = value__213.index
                var inline11935 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11934)
                var inline11936 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11935)
                var inline11937 string = inline11933 + inline11936
                t5351 = inline11937
                var t5352 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: t5351,
                }
                return t5352
            } else {
                var t5353 *ref_int_x = value__213.index
                var t5354 *ref_int_x = value__213.index
                var t5355 int
                var inline11957 int = ref_get__Ref_3int(t5354)
                t5355 = inline11957
                var t5356 int = t5355 + 2
                ref_set__Ref_3int(t5353, t5356)
                var mtmp760 Result__u32__string = _goml_m_std_p_json_p_parse__hex__quad(value__213)
                var jp5358 uint32
                switch mtmp760._tag {
                case 0:
                    var x761 uint32 = mtmp760._v0_0
                    jp5358 = x761
                    var t5371 bool = jp5358 < 56320
                    var jp5362 bool
                    if t5371 {
                        jp5362 = true
                    } else {
                        var t5372 bool = jp5358 > 57343
                        jp5362 = t5372
                    }
                    if jp5362 {
                        var t5363 string
                        var inline11939 string = "invalid low surrogate"
                        var inline11940 string = "" + inline11939
                        var inline11941 string = inline11940 + " at byte "
                        var inline11942 *ref_int_x = value__213.index
                        var inline11943 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11942)
                        var inline11944 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11943)
                        var inline11945 string = inline11941 + inline11944
                        t5363 = inline11945
                        var t5364 Result__unit__string = Result__unit__string{
                            _tag: 1,
                            _v1_0: t5363,
                        }
                        return t5364
                    } else {
                        var t5365 uint32 = jp5342 - 55296
                        var t5366 uint32 = t5365 * 1024
                        var t5367 uint32 = 65536 + t5366
                        var t5368 uint32 = t5367 + jp5358
                        var t5369 uint32 = t5368 - 56320
                        var inline11947 Option__char = char_from_u32(t5369)
                        switch inline11947._tag {
                        case 0:
                            var inline11948 string = _goml_m_std_p_json_p_json__error(value__213, "invalid unicode codepoint")
                            var inline11949 Result__unit__string = Result__unit__string{
                                _tag: 1,
                                _v1_0: inline11948,
                            }
                            return inline11949
                        case 1:
                            var inline11950 rune = inline11947._v1_0
                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__214, inline11950)
                            var inline11953 Result__unit__string = Result__unit__string{
                                _tag: 0,
                                _v0_0: struct{}{},
                            }
                            return inline11953
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case 1:
                    var x762 string = mtmp760._v1_0
                    var t5373 Result__unit__string = Result__unit__string{
                        _tag: 1,
                        _v1_0: x762,
                    }
                    return t5373
                default:
                    panic("non-exhaustive match")
                }
            }
        } else {
            var t5400 bool = jp5342 >= 56320
            var jp5396 bool
            if t5400 {
                var t5401 bool = jp5342 <= 57343
                jp5396 = t5401
            } else {
                jp5396 = false
            }
            if jp5396 {
                var t5397 string
                var inline11963 string = "unexpected low surrogate"
                var inline11964 string = "" + inline11963
                var inline11965 string = inline11964 + " at byte "
                var inline11966 *ref_int_x = value__213.index
                var inline11967 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11966)
                var inline11968 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11967)
                var inline11969 string = inline11965 + inline11968
                t5397 = inline11969
                var t5398 Result__unit__string = Result__unit__string{
                    _tag: 1,
                    _v1_0: t5397,
                }
                return t5398
            } else {
                var t5399 Result__unit__string = _goml_m_std_p_json_p_write__codepoint(value__213, builder__214, jp5342)
                return t5399
            }
        }
    case 1:
        var x758 string = mtmp756._v1_0
        var t5404 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x758,
        }
        return t5404
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_parse__json__string(value__217 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t5520 *ref_int_x = value__217.index
    var t5521 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5520)
    var t5522 string = value__217.input
    var t5523 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5522)
    var t5524 bool = t5521 >= t5523
    var jp5512 bool
    if t5524 {
        jp5512 = true
    } else {
        var t5525 string = value__217.input
        var t5526 *ref_int_x = value__217.index
        var t5527 int
        var inline11973 int = ref_get__Ref_3int(t5526)
        t5527 = inline11973
        var t5528 uint8
        var inline11971 uint8 = _goml_runtime_core_string_byte_get(t5525, t5527)
        t5528 = inline11971
        var t5529 bool = t5528 != 34
        jp5512 = t5529
    }
    if jp5512 {
        var t5513 string
        var inline11975 string = "expected string"
        var inline11976 string = "" + inline11975
        var inline11977 string = inline11976 + " at byte "
        var inline11978 *ref_int_x = value__217.index
        var inline11979 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline11978)
        var inline11980 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline11979)
        var inline11981 string = inline11977 + inline11980
        t5513 = inline11981
        var t5514 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: t5513,
        }
        return t5514
    } else {
        var t5515 *ref_int_x = value__217.index
        var t5516 *ref_int_x = value__217.index
        var t5517 int
        var inline11985 int = ref_get__Ref_3int(t5516)
        t5517 = inline11985
        var t5518 int = t5517 + 1
        ref_set__Ref_3int(t5515, t5518)
        var builder__218 _goml_m_std_p_text_p_StringBuilder = _goml_m_inherent_i_std_p_text_p_StringBuilder_i_std_p_text_p_StringBuilder_i_new()
        var t5408 *ref_int_x = value__217.index
        var segment__219 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5408)
        Loop_loop5412:
        for {
            var t5413 *ref_int_x = value__217.index
            var t5414 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5413)
            var t5415 string = value__217.input
            var t5416 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5415)
            var t5417 bool = t5414 < t5416
            if t5417 {
                var t5418 string = value__217.input
                var t5419 *ref_int_x = value__217.index
                var t5420 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5419)
                var byte__220 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5418, t5420)
                var t5422 bool = byte__220 == 34
                if t5422 {
                    var t5430 *ref_int_x = value__217.index
                    var t5431 int
                    var inline12000 int = ref_get__Ref_3int(t5430)
                    t5431 = inline12000
                    var t5432 bool = segment__219 < t5431
                    if t5432 {
                        var t5433 string = value__217.input
                        var t5434 *ref_int_x = value__217.index
                        var t5435 int
                        var inline11989 int = ref_get__Ref_3int(t5434)
                        t5435 = inline11989
                        var t5436 string
                        var inline11987 string = string_byte_slice(t5433, segment__219, t5435)
                        t5436 = inline11987
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t5436)
                    } else {}
                    var t5424 *ref_int_x = value__217.index
                    var t5425 *ref_int_x = value__217.index
                    var t5426 int
                    var inline11998 int = ref_get__Ref_3int(t5425)
                    t5426 = inline11998
                    var t5427 int = t5426 + 1
                    ref_set__Ref_3int(t5424, t5427)
                    var t5428 string
                    var inline11991 *_goml_vec_uint8 = builder__218.values
                    var inline11992 Tuple2_4bool_6string = string_from_utf8(inline11991)
                    var inline11993 string = inline11992._1
                    t5428 = inline11993
                    var t5429 Result__string__string = Result__string__string{
                        _tag: 0,
                        _v0_0: t5428,
                    }
                    return t5429
                } else {
                    var t5439 bool = byte__220 == 92
                    if t5439 {
                        var t5494 *ref_int_x = value__217.index
                        var t5495 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5494)
                        var t5496 bool = segment__219 < t5495
                        if t5496 {
                            var t5497 string = value__217.input
                            var t5498 *ref_int_x = value__217.index
                            var t5499 int
                            var inline12004 int = ref_get__Ref_3int(t5498)
                            t5499 = inline12004
                            var t5500 string
                            var inline12002 string = string_byte_slice(t5497, segment__219, t5499)
                            t5500 = inline12002
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, t5500)
                        } else {}
                        var t5441 *ref_int_x = value__217.index
                        var t5442 *ref_int_x = value__217.index
                        var t5443 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5442)
                        var t5444 int = t5443 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5441, t5444)
                        var t5487 *ref_int_x = value__217.index
                        var t5488 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5487)
                        var t5489 string = value__217.input
                        var t5490 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5489)
                        var t5491 bool = t5488 >= t5490
                        if t5491 {
                            var t5492 string
                            var inline12006 string = "incomplete escape"
                            var inline12007 string = "" + inline12006
                            var inline12008 string = inline12007 + " at byte "
                            var inline12009 *ref_int_x = value__217.index
                            var inline12010 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12009)
                            var inline12011 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12010)
                            var inline12012 string = inline12008 + inline12011
                            t5492 = inline12012
                            var t5493 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t5492,
                            }
                            return t5493
                        } else {
                            var t5446 string = value__217.input
                            var t5447 *ref_int_x = value__217.index
                            var t5448 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5447)
                            var escape__221 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5446, t5448)
                            var t5449 *ref_int_x = value__217.index
                            var t5450 *ref_int_x = value__217.index
                            var t5451 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5450)
                            var t5452 int = t5451 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5449, t5452)
                            var t5456 bool = escape__221 == 34
                            if t5456 {
                                var inline12014 rune = 34
                                var inline12015 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12014)
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline12015)
                                var t5454 *ref_int_x = value__217.index
                                var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                segment__219 = t5455
                                continue
                            } else {
                                var t5459 bool = escape__221 == 92
                                if t5459 {
                                    var inline12018 rune = 92
                                    var inline12019 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12018)
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__218, inline12019)
                                    var t5454 *ref_int_x = value__217.index
                                    var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                    segment__219 = t5455
                                    continue
                                } else {
                                    var t5462 bool = escape__221 == 47
                                    if t5462 {
                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 47)
                                        var t5454 *ref_int_x = value__217.index
                                        var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                        segment__219 = t5455
                                        continue
                                    } else {
                                        var t5465 bool = escape__221 == 98
                                        if t5465 {
                                            var mtmp770 Option__char = char_from_u32(8)
                                            switch mtmp770._tag {
                                            case 0:
                                                var t5454 *ref_int_x = value__217.index
                                                var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                                segment__219 = t5455
                                                continue
                                            case 1:
                                                var x771 rune = mtmp770._v1_0
                                                _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x771)
                                                var t5454 *ref_int_x = value__217.index
                                                var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                                segment__219 = t5455
                                                continue
                                            default:
                                                panic("non-exhaustive match")
                                            }
                                        } else {
                                            var t5469 bool = escape__221 == 102
                                            if t5469 {
                                                var mtmp772 Option__char = char_from_u32(12)
                                                switch mtmp772._tag {
                                                case 0:
                                                    var t5454 *ref_int_x = value__217.index
                                                    var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                                    segment__219 = t5455
                                                    continue
                                                case 1:
                                                    var x773 rune = mtmp772._v1_0
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, x773)
                                                    var t5454 *ref_int_x = value__217.index
                                                    var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                                    segment__219 = t5455
                                                    continue
                                                default:
                                                    panic("non-exhaustive match")
                                                }
                                            } else {
                                                var t5473 bool = escape__221 == 110
                                                if t5473 {
                                                    _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 10)
                                                    var t5454 *ref_int_x = value__217.index
                                                    var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                                    segment__219 = t5455
                                                    continue
                                                } else {
                                                    var t5476 bool = escape__221 == 114
                                                    if t5476 {
                                                        _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 13)
                                                        var t5454 *ref_int_x = value__217.index
                                                        var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                                        segment__219 = t5455
                                                        continue
                                                    } else {
                                                        var t5479 bool = escape__221 == 116
                                                        if t5479 {
                                                            _goml_m_inherent_i_std_p_text__he83b4afafc069a3c24c64018b13ca033_r_i_write__char(builder__218, 9)
                                                            var t5454 *ref_int_x = value__217.index
                                                            var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                                            segment__219 = t5455
                                                            continue
                                                        } else {
                                                            var t5482 bool = escape__221 == 117
                                                            if t5482 {
                                                                var mtmp774 Result__unit__string = _goml_m_std_p_json_p_parse__unicode__escape(value__217, builder__218)
                                                                switch mtmp774._tag {
                                                                case 0:
                                                                    var t5454 *ref_int_x = value__217.index
                                                                    var t5455 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5454)
                                                                    segment__219 = t5455
                                                                    continue
                                                                case 1:
                                                                    var x776 string = mtmp774._v1_0
                                                                    var t5484 Result__string__string = Result__string__string{
                                                                        _tag: 1,
                                                                        _v1_0: x776,
                                                                    }
                                                                    return t5484
                                                                default:
                                                                    panic("non-exhaustive match")
                                                                }
                                                            } else {
                                                                var t5485 string = _goml_m_std_p_json_p_json__error(value__217, "invalid escape")
                                                                var t5486 Result__string__string = Result__string__string{
                                                                    _tag: 1,
                                                                    _v1_0: t5485,
                                                                }
                                                                return t5486
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
                        var t5503 bool = byte__220 < 32
                        if t5503 {
                            var t5504 string = _goml_m_std_p_json_p_json__error(value__217, "unescaped control character")
                            var t5505 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t5504,
                            }
                            return t5505
                        } else {
                            var t5506 *ref_int_x = value__217.index
                            var t5507 *ref_int_x = value__217.index
                            var t5508 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5507)
                            var t5509 int = t5508 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5506, t5509)
                            continue
                        }
                    }
                }
            } else {
                break Loop_loop5412
            }
        }
        var t5410 string = _goml_m_std_p_json_p_json__error(value__217, "unterminated string")
        var t5411 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: t5410,
        }
        return t5411
    }
}

func _goml_m_std_p_json_p_parse__digits(value__225 _goml_m_std_p_json_p_JsonParser) bool {
    var t5538 *ref_int_x = value__225.index
    var start__226 int
    var inline12039 int = ref_get__Ref_3int(t5538)
    start__226 = inline12039
    Loop_loop5543:
    for {
        var t5551 *ref_int_x = value__225.index
        var t5552 int
        var inline12035 int = ref_get__Ref_3int(t5551)
        t5552 = inline12035
        var t5553 string = value__225.input
        var t5554 int
        var inline12033 int = _goml_runtime_core_string_len(t5553)
        t5554 = inline12033
        var t5555 bool = t5552 < t5554
        var jp5545 bool
        if t5555 {
            var t5556 string = value__225.input
            var t5557 *ref_int_x = value__225.index
            var t5558 int
            var inline12027 int = ref_get__Ref_3int(t5557)
            t5558 = inline12027
            var t5559 uint8
            var inline12025 uint8 = _goml_runtime_core_string_byte_get(t5556, t5558)
            t5559 = inline12025
            var inline12022 bool = t5559 >= 48
            if inline12022 {
                var inline12023 bool = t5559 <= 57
                jp5545 = inline12023
            } else {
                jp5545 = false
            }
        } else {
            jp5545 = false
        }
        if jp5545 {
            var t5546 *ref_int_x = value__225.index
            var t5547 *ref_int_x = value__225.index
            var t5548 int
            var inline12031 int = ref_get__Ref_3int(t5547)
            t5548 = inline12031
            var t5549 int = t5548 + 1
            ref_set__Ref_3int(t5546, t5549)
            continue
        } else {
            break Loop_loop5543
        }
    }
    var t5540 *ref_int_x = value__225.index
    var t5541 int
    var inline12037 int = ref_get__Ref_3int(t5540)
    t5541 = inline12037
    var t5542 bool = t5541 > start__226
    return t5542
}

func _goml_m_std_p_json_p_parse__json__number__text(value__227 _goml_m_std_p_json_p_JsonParser) Result__string__string {
    var t5563 *ref_int_x = value__227.index
    var start__228 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5563)
    var t5684 string = value__227.input
    var t5685 *ref_int_x = value__227.index
    var t5686 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5685)
    var t5687 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5684, t5686)
    var t5688 bool = t5687 == 45
    if t5688 {
        var t5689 *ref_int_x = value__227.index
        var t5690 *ref_int_x = value__227.index
        var t5691 int
        var inline12043 int = ref_get__Ref_3int(t5690)
        t5691 = inline12043
        var t5692 int = t5691 + 1
        ref_set__Ref_3int(t5689, t5692)
    } else {}
    var t5647 *ref_int_x = value__227.index
    var t5648 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5647)
    var t5649 string = value__227.input
    var t5650 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5649)
    var t5651 bool = t5648 >= t5650
    if t5651 {
        var t5652 string
        var inline12045 string = "incomplete number"
        var inline12046 string = "" + inline12045
        var inline12047 string = inline12046 + " at byte "
        var inline12048 *ref_int_x = value__227.index
        var inline12049 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12048)
        var inline12050 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12049)
        var inline12051 string = inline12047 + inline12050
        t5652 = inline12051
        var t5653 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: t5652,
        }
        return t5653
    } else {
        var t5655 string = value__227.input
        var t5656 *ref_int_x = value__227.index
        var t5657 int
        var inline12086 int = ref_get__Ref_3int(t5656)
        t5657 = inline12086
        var t5658 uint8
        var inline12084 uint8 = _goml_runtime_core_string_byte_get(t5655, t5657)
        t5658 = inline12084
        var t5659 bool = t5658 == 48
        if t5659 {
            var t5660 *ref_int_x = value__227.index
            var t5661 *ref_int_x = value__227.index
            var t5662 int
            var inline12074 int = ref_get__Ref_3int(t5661)
            t5662 = inline12074
            var t5663 int = t5662 + 1
            ref_set__Ref_3int(t5660, t5663)
            var t5669 *ref_int_x = value__227.index
            var t5670 int
            var inline12070 int = ref_get__Ref_3int(t5669)
            t5670 = inline12070
            var t5671 string = value__227.input
            var t5672 int
            var inline12068 int = _goml_runtime_core_string_len(t5671)
            t5672 = inline12068
            var t5673 bool = t5670 < t5672
            var jp5666 bool
            if t5673 {
                var t5674 string = value__227.input
                var t5675 *ref_int_x = value__227.index
                var t5676 int
                var inline12058 int = ref_get__Ref_3int(t5675)
                t5676 = inline12058
                var t5677 uint8
                var inline12056 uint8 = _goml_runtime_core_string_byte_get(t5674, t5676)
                t5677 = inline12056
                var inline12053 bool = t5677 >= 48
                if inline12053 {
                    var inline12054 bool = t5677 <= 57
                    jp5666 = inline12054
                } else {
                    jp5666 = false
                }
            } else {
                jp5666 = false
            }
            if jp5666 {
                var t5667 string
                var inline12060 string = "invalid leading zero"
                var inline12061 string = "" + inline12060
                var inline12062 string = inline12061 + " at byte "
                var inline12063 *ref_int_x = value__227.index
                var inline12064 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12063)
                var inline12065 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12064)
                var inline12066 string = inline12062 + inline12065
                t5667 = inline12066
                var t5668 Result__string__string = Result__string__string{
                    _tag: 1,
                    _v1_0: t5667,
                }
                return t5668
            } else {
                var t5637 *ref_int_x = value__227.index
                var t5638 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5637)
                var t5639 string = value__227.input
                var t5640 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5639)
                var t5641 bool = t5638 < t5640
                var jp5627 bool
                if t5641 {
                    var t5642 string = value__227.input
                    var t5643 *ref_int_x = value__227.index
                    var t5644 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5643)
                    var t5645 uint8
                    var inline12088 uint8 = _goml_runtime_core_string_byte_get(t5642, t5644)
                    t5645 = inline12088
                    var t5646 bool = t5645 == 46
                    jp5627 = t5646
                } else {
                    jp5627 = false
                }
                if jp5627 {
                    var t5628 *ref_int_x = value__227.index
                    var t5629 *ref_int_x = value__227.index
                    var t5630 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5629)
                    var t5631 int = t5630 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5628, t5631)
                    var t5633 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5634 bool = !t5633
                    if t5634 {
                        var t5635 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5636 Result__string__string = Result__string__string{
                            _tag: 1,
                            _v1_0: t5635,
                        }
                        return t5636
                    } else {
                        var t5609 *ref_int_x = value__227.index
                        var t5610 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5609)
                        var t5611 string = value__227.input
                        var t5612 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5611)
                        var t5613 bool = t5610 < t5612
                        var jp5574 bool
                        if t5613 {
                            var t5616 string = value__227.input
                            var t5617 *ref_int_x = value__227.index
                            var t5618 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5617)
                            var t5619 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5616, t5618)
                            var t5620 bool = t5619 == 101
                            if t5620 {
                                jp5574 = true
                            } else {
                                var t5621 string = value__227.input
                                var t5622 *ref_int_x = value__227.index
                                var t5623 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5622)
                                var t5624 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5621, t5623)
                                var t5625 bool = t5624 == 69
                                jp5574 = t5625
                            }
                        } else {
                            jp5574 = false
                        }
                        if jp5574 {
                            var t5575 *ref_int_x = value__227.index
                            var t5576 *ref_int_x = value__227.index
                            var t5577 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5576)
                            var t5578 int = t5577 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5575, t5578)
                            var t5592 *ref_int_x = value__227.index
                            var t5593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5592)
                            var t5594 string = value__227.input
                            var t5595 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5594)
                            var t5596 bool = t5593 < t5595
                            var jp5586 bool
                            if t5596 {
                                var t5599 string = value__227.input
                                var t5600 *ref_int_x = value__227.index
                                var t5601 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5600)
                                var t5602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5599, t5601)
                                var t5603 bool = t5602 == 43
                                if t5603 {
                                    jp5586 = true
                                } else {
                                    var t5604 string = value__227.input
                                    var t5605 *ref_int_x = value__227.index
                                    var t5606 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5605)
                                    var t5607 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5604, t5606)
                                    var t5608 bool = t5607 == 45
                                    jp5586 = t5608
                                }
                            } else {
                                jp5586 = false
                            }
                            if jp5586 {
                                var t5587 *ref_int_x = value__227.index
                                var t5588 *ref_int_x = value__227.index
                                var t5589 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5588)
                                var t5590 int = t5589 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5587, t5590)
                            } else {}
                            var t5581 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t5582 bool = !t5581
                            if t5582 {
                                var t5583 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t5584 Result__string__string = Result__string__string{
                                    _tag: 1,
                                    _v1_0: t5583,
                                }
                                return t5584
                            } else {
                                var t5568 string = value__227.input
                                var t5569 *ref_int_x = value__227.index
                                var t5570 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5569)
                                var t5571 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5568, start__228, t5570)
                                var t5572 Result__string__string = Result__string__string{
                                    _tag: 0,
                                    _v0_0: t5571,
                                }
                                return t5572
                            }
                        } else {
                            var t5568 string = value__227.input
                            var t5569 *ref_int_x = value__227.index
                            var t5570 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5569)
                            var t5571 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5568, start__228, t5570)
                            var t5572 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t5571,
                            }
                            return t5572
                        }
                    }
                } else {
                    var t5609 *ref_int_x = value__227.index
                    var t5610 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5609)
                    var t5611 string = value__227.input
                    var t5612 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5611)
                    var t5613 bool = t5610 < t5612
                    var jp5574 bool
                    if t5613 {
                        var t5616 string = value__227.input
                        var t5617 *ref_int_x = value__227.index
                        var t5618 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5617)
                        var t5619 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5616, t5618)
                        var t5620 bool = t5619 == 101
                        if t5620 {
                            jp5574 = true
                        } else {
                            var t5621 string = value__227.input
                            var t5622 *ref_int_x = value__227.index
                            var t5623 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5622)
                            var t5624 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5621, t5623)
                            var t5625 bool = t5624 == 69
                            jp5574 = t5625
                        }
                    } else {
                        jp5574 = false
                    }
                    if jp5574 {
                        var t5575 *ref_int_x = value__227.index
                        var t5576 *ref_int_x = value__227.index
                        var t5577 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5576)
                        var t5578 int = t5577 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5575, t5578)
                        var t5592 *ref_int_x = value__227.index
                        var t5593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5592)
                        var t5594 string = value__227.input
                        var t5595 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5594)
                        var t5596 bool = t5593 < t5595
                        var jp5586 bool
                        if t5596 {
                            var t5599 string = value__227.input
                            var t5600 *ref_int_x = value__227.index
                            var t5601 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5600)
                            var t5602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5599, t5601)
                            var t5603 bool = t5602 == 43
                            if t5603 {
                                jp5586 = true
                            } else {
                                var t5604 string = value__227.input
                                var t5605 *ref_int_x = value__227.index
                                var t5606 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5605)
                                var t5607 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5604, t5606)
                                var t5608 bool = t5607 == 45
                                jp5586 = t5608
                            }
                        } else {
                            jp5586 = false
                        }
                        if jp5586 {
                            var t5587 *ref_int_x = value__227.index
                            var t5588 *ref_int_x = value__227.index
                            var t5589 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5588)
                            var t5590 int = t5589 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5587, t5590)
                        } else {}
                        var t5581 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t5582 bool = !t5581
                        if t5582 {
                            var t5583 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t5584 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t5583,
                            }
                            return t5584
                        } else {
                            var t5568 string = value__227.input
                            var t5569 *ref_int_x = value__227.index
                            var t5570 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5569)
                            var t5571 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5568, start__228, t5570)
                            var t5572 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t5571,
                            }
                            return t5572
                        }
                    } else {
                        var t5568 string = value__227.input
                        var t5569 *ref_int_x = value__227.index
                        var t5570 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5569)
                        var t5571 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5568, start__228, t5570)
                        var t5572 Result__string__string = Result__string__string{
                            _tag: 0,
                            _v0_0: t5571,
                        }
                        return t5572
                    }
                }
            }
        } else {
            var t5680 bool = _goml_m_std_p_json_p_parse__digits(value__227)
            var t5681 bool = !t5680
            if t5681 {
                var t5682 string
                var inline12076 string = "expected number"
                var inline12077 string = "" + inline12076
                var inline12078 string = inline12077 + " at byte "
                var inline12079 *ref_int_x = value__227.index
                var inline12080 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12079)
                var inline12081 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12080)
                var inline12082 string = inline12078 + inline12081
                t5682 = inline12082
                var t5683 Result__string__string = Result__string__string{
                    _tag: 1,
                    _v1_0: t5682,
                }
                return t5683
            } else {
                var t5637 *ref_int_x = value__227.index
                var t5638 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5637)
                var t5639 string = value__227.input
                var t5640 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5639)
                var t5641 bool = t5638 < t5640
                var jp5627 bool
                if t5641 {
                    var t5642 string = value__227.input
                    var t5643 *ref_int_x = value__227.index
                    var t5644 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5643)
                    var t5645 uint8
                    var inline12088 uint8 = _goml_runtime_core_string_byte_get(t5642, t5644)
                    t5645 = inline12088
                    var t5646 bool = t5645 == 46
                    jp5627 = t5646
                } else {
                    jp5627 = false
                }
                if jp5627 {
                    var t5628 *ref_int_x = value__227.index
                    var t5629 *ref_int_x = value__227.index
                    var t5630 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5629)
                    var t5631 int = t5630 + 1
                    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5628, t5631)
                    var t5633 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                    var t5634 bool = !t5633
                    if t5634 {
                        var t5635 string = _goml_m_std_p_json_p_json__error(value__227, "missing fraction digits")
                        var t5636 Result__string__string = Result__string__string{
                            _tag: 1,
                            _v1_0: t5635,
                        }
                        return t5636
                    } else {
                        var t5609 *ref_int_x = value__227.index
                        var t5610 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5609)
                        var t5611 string = value__227.input
                        var t5612 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5611)
                        var t5613 bool = t5610 < t5612
                        var jp5574 bool
                        if t5613 {
                            var t5616 string = value__227.input
                            var t5617 *ref_int_x = value__227.index
                            var t5618 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5617)
                            var t5619 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5616, t5618)
                            var t5620 bool = t5619 == 101
                            if t5620 {
                                jp5574 = true
                            } else {
                                var t5621 string = value__227.input
                                var t5622 *ref_int_x = value__227.index
                                var t5623 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5622)
                                var t5624 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5621, t5623)
                                var t5625 bool = t5624 == 69
                                jp5574 = t5625
                            }
                        } else {
                            jp5574 = false
                        }
                        if jp5574 {
                            var t5575 *ref_int_x = value__227.index
                            var t5576 *ref_int_x = value__227.index
                            var t5577 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5576)
                            var t5578 int = t5577 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5575, t5578)
                            var t5592 *ref_int_x = value__227.index
                            var t5593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5592)
                            var t5594 string = value__227.input
                            var t5595 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5594)
                            var t5596 bool = t5593 < t5595
                            var jp5586 bool
                            if t5596 {
                                var t5599 string = value__227.input
                                var t5600 *ref_int_x = value__227.index
                                var t5601 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5600)
                                var t5602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5599, t5601)
                                var t5603 bool = t5602 == 43
                                if t5603 {
                                    jp5586 = true
                                } else {
                                    var t5604 string = value__227.input
                                    var t5605 *ref_int_x = value__227.index
                                    var t5606 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5605)
                                    var t5607 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5604, t5606)
                                    var t5608 bool = t5607 == 45
                                    jp5586 = t5608
                                }
                            } else {
                                jp5586 = false
                            }
                            if jp5586 {
                                var t5587 *ref_int_x = value__227.index
                                var t5588 *ref_int_x = value__227.index
                                var t5589 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5588)
                                var t5590 int = t5589 + 1
                                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5587, t5590)
                            } else {}
                            var t5581 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                            var t5582 bool = !t5581
                            if t5582 {
                                var t5583 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                                var t5584 Result__string__string = Result__string__string{
                                    _tag: 1,
                                    _v1_0: t5583,
                                }
                                return t5584
                            } else {
                                var t5568 string = value__227.input
                                var t5569 *ref_int_x = value__227.index
                                var t5570 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5569)
                                var t5571 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5568, start__228, t5570)
                                var t5572 Result__string__string = Result__string__string{
                                    _tag: 0,
                                    _v0_0: t5571,
                                }
                                return t5572
                            }
                        } else {
                            var t5568 string = value__227.input
                            var t5569 *ref_int_x = value__227.index
                            var t5570 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5569)
                            var t5571 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5568, start__228, t5570)
                            var t5572 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t5571,
                            }
                            return t5572
                        }
                    }
                } else {
                    var t5609 *ref_int_x = value__227.index
                    var t5610 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5609)
                    var t5611 string = value__227.input
                    var t5612 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5611)
                    var t5613 bool = t5610 < t5612
                    var jp5574 bool
                    if t5613 {
                        var t5616 string = value__227.input
                        var t5617 *ref_int_x = value__227.index
                        var t5618 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5617)
                        var t5619 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5616, t5618)
                        var t5620 bool = t5619 == 101
                        if t5620 {
                            jp5574 = true
                        } else {
                            var t5621 string = value__227.input
                            var t5622 *ref_int_x = value__227.index
                            var t5623 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5622)
                            var t5624 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5621, t5623)
                            var t5625 bool = t5624 == 69
                            jp5574 = t5625
                        }
                    } else {
                        jp5574 = false
                    }
                    if jp5574 {
                        var t5575 *ref_int_x = value__227.index
                        var t5576 *ref_int_x = value__227.index
                        var t5577 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5576)
                        var t5578 int = t5577 + 1
                        _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5575, t5578)
                        var t5592 *ref_int_x = value__227.index
                        var t5593 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5592)
                        var t5594 string = value__227.input
                        var t5595 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5594)
                        var t5596 bool = t5593 < t5595
                        var jp5586 bool
                        if t5596 {
                            var t5599 string = value__227.input
                            var t5600 *ref_int_x = value__227.index
                            var t5601 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5600)
                            var t5602 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5599, t5601)
                            var t5603 bool = t5602 == 43
                            if t5603 {
                                jp5586 = true
                            } else {
                                var t5604 string = value__227.input
                                var t5605 *ref_int_x = value__227.index
                                var t5606 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5605)
                                var t5607 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5604, t5606)
                                var t5608 bool = t5607 == 45
                                jp5586 = t5608
                            }
                        } else {
                            jp5586 = false
                        }
                        if jp5586 {
                            var t5587 *ref_int_x = value__227.index
                            var t5588 *ref_int_x = value__227.index
                            var t5589 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5588)
                            var t5590 int = t5589 + 1
                            _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5587, t5590)
                        } else {}
                        var t5581 bool = _goml_m_std_p_json_p_parse__digits(value__227)
                        var t5582 bool = !t5581
                        if t5582 {
                            var t5583 string = _goml_m_std_p_json_p_json__error(value__227, "missing exponent digits")
                            var t5584 Result__string__string = Result__string__string{
                                _tag: 1,
                                _v1_0: t5583,
                            }
                            return t5584
                        } else {
                            var t5568 string = value__227.input
                            var t5569 *ref_int_x = value__227.index
                            var t5570 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5569)
                            var t5571 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5568, start__228, t5570)
                            var t5572 Result__string__string = Result__string__string{
                                _tag: 0,
                                _v0_0: t5571,
                            }
                            return t5572
                        }
                    } else {
                        var t5568 string = value__227.input
                        var t5569 *ref_int_x = value__227.index
                        var t5570 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5569)
                        var t5571 string = _goml_m_inherent_i_string_i_string_i_byte__slice(t5568, start__228, t5570)
                        var t5572 Result__string__string = Result__string__string{
                            _tag: 0,
                            _v0_0: t5571,
                        }
                        return t5572
                    }
                }
            }
        }
    }
}

func _goml_m_std_p_json_p_parse__json__literal(value__230 _goml_m_std_p_json_p_JsonParser, expected__231 string, result__232 _goml_m_std_p_json_p_Value) _goml_m_Result____std_p_json_p_Value____string {
    var t5715 *ref_int_x = value__230.index
    var t5716 int
    var inline12116 int = ref_get__Ref_3int(t5715)
    t5716 = inline12116
    var t5717 int
    var inline12114 int = _goml_runtime_core_string_len(expected__231)
    t5717 = inline12114
    var t5718 int = t5716 + t5717
    var t5719 string = value__230.input
    var t5720 int
    var inline12112 int = _goml_runtime_core_string_len(t5719)
    t5720 = inline12112
    var t5721 bool = t5718 <= t5720
    var jp5706 bool
    if t5721 {
        var t5722 string = value__230.input
        var t5723 *ref_int_x = value__230.index
        var t5724 int
        var inline12096 int = ref_get__Ref_3int(t5723)
        t5724 = inline12096
        var t5725 *ref_int_x = value__230.index
        var t5726 int
        var inline12094 int = ref_get__Ref_3int(t5725)
        t5726 = inline12094
        var t5727 int
        var inline12092 int = _goml_runtime_core_string_len(expected__231)
        t5727 = inline12092
        var t5728 int = t5726 + t5727
        var t5729 string
        var inline12090 string = string_byte_slice(t5722, t5724, t5728)
        t5729 = inline12090
        var t5730 bool = t5729 == expected__231
        jp5706 = t5730
    } else {
        jp5706 = false
    }
    if jp5706 {
        var t5707 *ref_int_x = value__230.index
        var t5708 *ref_int_x = value__230.index
        var t5709 int
        var inline12102 int = ref_get__Ref_3int(t5708)
        t5709 = inline12102
        var t5710 int
        var inline12100 int = _goml_runtime_core_string_len(expected__231)
        t5710 = inline12100
        var t5711 int = t5709 + t5710
        ref_set__Ref_3int(t5707, t5711)
        var t5712 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 0,
            _v0_0: result__232,
        }
        return t5712
    } else {
        var t5713 string
        var inline12104 string = "invalid literal"
        var inline12105 string = "" + inline12104
        var inline12106 string = inline12105 + " at byte "
        var inline12107 *ref_int_x = value__230.index
        var inline12108 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12107)
        var inline12109 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12108)
        var inline12110 string = inline12106 + inline12109
        t5713 = inline12110
        var t5714 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5713,
        }
        return t5714
    }
}

func _goml_m_std_p_json_p_parse__json__array(value__233 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5733 *ref_int_x = value__233.index
    var t5734 *ref_int_x = value__233.index
    var t5735 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5734)
    var t5736 int = t5735 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5733, t5736)
    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
    var t5737 [0]_goml_m_std_p_json_p_Value = [0]_goml_m_std_p_json_p_Value{}
    var result__234 *_goml_vec__goml_m_std_p_json_p_Value = func(values [0]_goml_m_std_p_json_p_Value) *_goml_vec__goml_m_std_p_json_p_Value {
        return &_goml_vec__goml_m_std_p_json_p_Value{
            items: values[0:len(values)],
        }
    }(t5737)
    var t5792 *ref_int_x = value__233.index
    var t5793 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5792)
    var t5794 string = value__233.input
    var t5795 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5794)
    var t5796 bool = t5793 < t5795
    var jp5785 bool
    if t5796 {
        var t5797 string = value__233.input
        var t5798 *ref_int_x = value__233.index
        var t5799 int
        var inline12120 int = ref_get__Ref_3int(t5798)
        t5799 = inline12120
        var t5800 uint8
        var inline12118 uint8 = _goml_runtime_core_string_byte_get(t5797, t5799)
        t5800 = inline12118
        var t5801 bool = t5800 == 93
        jp5785 = t5801
    } else {
        jp5785 = false
    }
    if jp5785 {
        var t5786 *ref_int_x = value__233.index
        var t5787 *ref_int_x = value__233.index
        var t5788 int
        var inline12124 int = ref_get__Ref_3int(t5787)
        t5788 = inline12124
        var t5789 int = t5788 + 1
        ref_set__Ref_3int(t5786, t5789)
        var t5790 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
            _0: result__234,
        }
        var t5791 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 0,
            _v0_0: t5790,
        }
        return t5791
    } else {
        Loop_loop5742:
        for {
            var t5743 *ref_int_x = value__233.index
            var t5744 int
            var inline12166 int = ref_get__Ref_3int(t5743)
            t5744 = inline12166
            var t5745 string = value__233.input
            var t5746 int
            var inline12164 int = _goml_runtime_core_string_len(t5745)
            t5746 = inline12164
            var t5747 bool = t5744 < t5746
            if t5747 {
                var mtmp797 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__233)
                var jp5749 _goml_m_std_p_json_p_Value
                switch mtmp797._tag {
                case 0:
                    var x798 _goml_m_std_p_json_p_Value = mtmp797._v0_0
                    jp5749 = x798
                    vec_push___goml_m_Vec__16std_p_json_p_Value(result__234, jp5749)
                    _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                    var t5751 *ref_int_x = value__233.index
                    var t5752 int
                    var inline12160 int = ref_get__Ref_3int(t5751)
                    t5752 = inline12160
                    var t5753 string = value__233.input
                    var t5754 int
                    var inline12158 int = _goml_runtime_core_string_len(t5753)
                    t5754 = inline12158
                    var t5755 bool = t5752 >= t5754
                    if t5755 {
                        var t5756 string
                        var inline12126 string = "unterminated array"
                        var inline12127 string = "" + inline12126
                        var inline12128 string = inline12127 + " at byte "
                        var inline12129 *ref_int_x = value__233.index
                        var inline12130 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12129)
                        var inline12131 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12130)
                        var inline12132 string = inline12128 + inline12131
                        t5756 = inline12132
                        var t5757 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                            _tag: 1,
                            _v1_0: t5756,
                        }
                        return t5757
                    } else {
                        var t5759 string = value__233.input
                        var t5760 *ref_int_x = value__233.index
                        var t5761 int
                        var inline12156 int = ref_get__Ref_3int(t5760)
                        t5761 = inline12156
                        var t5762 uint8
                        var inline12154 uint8 = _goml_runtime_core_string_byte_get(t5759, t5761)
                        t5762 = inline12154
                        var t5763 bool = t5762 == 93
                        if t5763 {
                            var t5764 *ref_int_x = value__233.index
                            var t5765 *ref_int_x = value__233.index
                            var t5766 int
                            var inline12136 int = ref_get__Ref_3int(t5765)
                            t5766 = inline12136
                            var t5767 int = t5766 + 1
                            ref_set__Ref_3int(t5764, t5767)
                            var t5768 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Array{
                                _0: result__234,
                            }
                            var t5769 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                _tag: 0,
                                _v0_0: t5768,
                            }
                            return t5769
                        } else {
                            var t5771 string = value__233.input
                            var t5772 *ref_int_x = value__233.index
                            var t5773 int
                            var inline12152 int = ref_get__Ref_3int(t5772)
                            t5773 = inline12152
                            var t5774 uint8
                            var inline12150 uint8 = _goml_runtime_core_string_byte_get(t5771, t5773)
                            t5774 = inline12150
                            var t5775 bool = t5774 == 44
                            if t5775 {
                                var t5776 *ref_int_x = value__233.index
                                var t5777 *ref_int_x = value__233.index
                                var t5778 int
                                var inline12140 int = ref_get__Ref_3int(t5777)
                                t5778 = inline12140
                                var t5779 int = t5778 + 1
                                ref_set__Ref_3int(t5776, t5779)
                                _goml_m_std_p_json_p_skip__json__whitespace(value__233)
                                continue
                            } else {
                                var t5781 string
                                var inline12142 string = "expected array separator"
                                var inline12143 string = "" + inline12142
                                var inline12144 string = inline12143 + " at byte "
                                var inline12145 *ref_int_x = value__233.index
                                var inline12146 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12145)
                                var inline12147 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12146)
                                var inline12148 string = inline12144 + inline12147
                                t5781 = inline12148
                                var t5782 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                    _tag: 1,
                                    _v1_0: t5781,
                                }
                                return t5782
                            }
                        }
                    }
                case 1:
                    var x799 string = mtmp797._v1_0
                    var t5783 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 1,
                        _v1_0: x799,
                    }
                    return t5783
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5742
            }
        }
        var t5740 string = _goml_m_std_p_json_p_json__error(value__233, "unterminated array")
        var t5741 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5740,
        }
        return t5741
    }
}

func _goml_m_std_p_json_p_parse__json__object(value__236 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    var t5804 *ref_int_x = value__236.index
    var t5805 *ref_int_x = value__236.index
    var t5806 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5805)
    var t5807 int = t5806 + 1
    _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(t5804, t5807)
    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
    var t5808 [0]Tuple2_6string_26_goml_m_std_p_json_p_Value = [0]Tuple2_6string_26_goml_m_std_p_json_p_Value{}
    var result__237 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = func(values [0]Tuple2_6string_26_goml_m_std_p_json_p_Value) *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value {
        return &_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value{
            items: values[0:len(values)],
        }
    }(t5808)
    var t5887 *ref_int_x = value__236.index
    var t5888 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5887)
    var t5889 string = value__236.input
    var t5890 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5889)
    var t5891 bool = t5888 < t5890
    var jp5880 bool
    if t5891 {
        var t5892 string = value__236.input
        var t5893 *ref_int_x = value__236.index
        var t5894 int
        var inline12170 int = ref_get__Ref_3int(t5893)
        t5894 = inline12170
        var t5895 uint8
        var inline12168 uint8 = _goml_runtime_core_string_byte_get(t5892, t5894)
        t5895 = inline12168
        var t5896 bool = t5895 == 125
        jp5880 = t5896
    } else {
        jp5880 = false
    }
    if jp5880 {
        var t5881 *ref_int_x = value__236.index
        var t5882 *ref_int_x = value__236.index
        var t5883 int
        var inline12174 int = ref_get__Ref_3int(t5882)
        t5883 = inline12174
        var t5884 int = t5883 + 1
        ref_set__Ref_3int(t5881, t5884)
        var t5885 _goml_m_std_p_json_p_Value = Object{
            _0: result__237,
        }
        var t5886 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 0,
            _v0_0: t5885,
        }
        return t5886
    } else {
        Loop_loop5813:
        for {
            var t5814 *ref_int_x = value__236.index
            var t5815 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5814)
            var t5816 string = value__236.input
            var t5817 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5816)
            var t5818 bool = t5815 < t5817
            if t5818 {
                var mtmp809 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__236)
                var jp5820 string
                switch mtmp809._tag {
                case 0:
                    var x810 string = mtmp809._v0_0
                    jp5820 = x810
                    _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                    var t5868 *ref_int_x = value__236.index
                    var t5869 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5868)
                    var t5870 string = value__236.input
                    var t5871 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5870)
                    var t5872 bool = t5869 >= t5871
                    var jp5860 bool
                    if t5872 {
                        jp5860 = true
                    } else {
                        var t5873 string = value__236.input
                        var t5874 *ref_int_x = value__236.index
                        var t5875 int
                        var inline12178 int = ref_get__Ref_3int(t5874)
                        t5875 = inline12178
                        var t5876 uint8
                        var inline12176 uint8 = _goml_runtime_core_string_byte_get(t5873, t5875)
                        t5876 = inline12176
                        var t5877 bool = t5876 != 58
                        jp5860 = t5877
                    }
                    if jp5860 {
                        var t5861 string
                        var inline12180 string = "expected object colon"
                        var inline12181 string = "" + inline12180
                        var inline12182 string = inline12181 + " at byte "
                        var inline12183 *ref_int_x = value__236.index
                        var inline12184 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12183)
                        var inline12185 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12184)
                        var inline12186 string = inline12182 + inline12185
                        t5861 = inline12186
                        var t5862 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                            _tag: 1,
                            _v1_0: t5861,
                        }
                        return t5862
                    } else {
                        var t5863 *ref_int_x = value__236.index
                        var t5864 *ref_int_x = value__236.index
                        var t5865 int
                        var inline12190 int = ref_get__Ref_3int(t5864)
                        t5865 = inline12190
                        var t5866 int = t5865 + 1
                        ref_set__Ref_3int(t5863, t5866)
                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                        var mtmp815 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(value__236)
                        var jp5823 _goml_m_std_p_json_p_Value
                        switch mtmp815._tag {
                        case 0:
                            var x816 _goml_m_std_p_json_p_Value = mtmp815._v0_0
                            jp5823 = x816
                            var t5824 Tuple2_6string_26_goml_m_std_p_json_p_Value = Tuple2_6string_26_goml_m_std_p_json_p_Value{
                                _0: jp5820,
                                _1: jp5823,
                            }
                            _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(result__237, t5824)
                            _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                            var t5826 *ref_int_x = value__236.index
                            var t5827 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5826)
                            var t5828 string = value__236.input
                            var t5829 int = _goml_m_inherent_i_string_i_string_i_byte__len(t5828)
                            var t5830 bool = t5827 >= t5829
                            if t5830 {
                                var t5831 string
                                var inline12192 string = "unterminated object"
                                var inline12193 string = "" + inline12192
                                var inline12194 string = inline12193 + " at byte "
                                var inline12195 *ref_int_x = value__236.index
                                var inline12196 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12195)
                                var inline12197 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12196)
                                var inline12198 string = inline12194 + inline12197
                                t5831 = inline12198
                                var t5832 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                    _tag: 1,
                                    _v1_0: t5831,
                                }
                                return t5832
                            } else {
                                var t5834 string = value__236.input
                                var t5835 *ref_int_x = value__236.index
                                var t5836 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5835)
                                var t5837 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(t5834, t5836)
                                var t5838 bool = t5837 == 125
                                if t5838 {
                                    var t5839 *ref_int_x = value__236.index
                                    var t5840 *ref_int_x = value__236.index
                                    var t5841 int
                                    var inline12202 int = ref_get__Ref_3int(t5840)
                                    t5841 = inline12202
                                    var t5842 int = t5841 + 1
                                    ref_set__Ref_3int(t5839, t5842)
                                    var t5843 _goml_m_std_p_json_p_Value = Object{
                                        _0: result__237,
                                    }
                                    var t5844 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                        _tag: 0,
                                        _v0_0: t5843,
                                    }
                                    return t5844
                                } else {
                                    var t5846 string = value__236.input
                                    var t5847 *ref_int_x = value__236.index
                                    var t5848 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(t5847)
                                    var t5849 uint8
                                    var inline12216 uint8 = _goml_runtime_core_string_byte_get(t5846, t5848)
                                    t5849 = inline12216
                                    var t5850 bool = t5849 == 44
                                    if t5850 {
                                        var t5851 *ref_int_x = value__236.index
                                        var t5852 *ref_int_x = value__236.index
                                        var t5853 int
                                        var inline12206 int = ref_get__Ref_3int(t5852)
                                        t5853 = inline12206
                                        var t5854 int = t5853 + 1
                                        ref_set__Ref_3int(t5851, t5854)
                                        _goml_m_std_p_json_p_skip__json__whitespace(value__236)
                                        continue
                                    } else {
                                        var t5856 string
                                        var inline12208 string = "expected object separator"
                                        var inline12209 string = "" + inline12208
                                        var inline12210 string = inline12209 + " at byte "
                                        var inline12211 *ref_int_x = value__236.index
                                        var inline12212 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12211)
                                        var inline12213 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12212)
                                        var inline12214 string = inline12210 + inline12213
                                        t5856 = inline12214
                                        var t5857 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                            _tag: 1,
                                            _v1_0: t5856,
                                        }
                                        return t5857
                                    }
                                }
                            }
                        case 1:
                            var x817 string = mtmp815._v1_0
                            var t5858 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                                _tag: 1,
                                _v1_0: x817,
                            }
                            return t5858
                        default:
                            panic("non-exhaustive match")
                        }
                    }
                case 1:
                    var x811 string = mtmp809._v1_0
                    var t5878 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 1,
                        _v1_0: x811,
                    }
                    return t5878
                default:
                    panic("non-exhaustive match")
                }
            } else {
                break Loop_loop5813
            }
        }
        var t5811 string = _goml_m_std_p_json_p_json__error(value__236, "unterminated object")
        var t5812 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5811,
        }
        return t5812
    }
}

func _goml_m_std_p_json_p_parse__json__value(value__240 _goml_m_std_p_json_p_JsonParser) _goml_m_Result____std_p_json_p_Value____string {
    _goml_m_std_p_json_p_skip__json__whitespace(value__240)
    var t5901 *ref_int_x = value__240.index
    var t5902 int
    var inline12254 int = ref_get__Ref_3int(t5901)
    t5902 = inline12254
    var t5903 string = value__240.input
    var t5904 int
    var inline12252 int = _goml_runtime_core_string_len(t5903)
    t5904 = inline12252
    var t5905 bool = t5902 >= t5904
    if t5905 {
        var t5906 string
        var inline12218 string = "expected JSON value"
        var inline12219 string = "" + inline12218
        var inline12220 string = inline12219 + " at byte "
        var inline12221 *ref_int_x = value__240.index
        var inline12222 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12221)
        var inline12223 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12222)
        var inline12224 string = inline12220 + inline12223
        t5906 = inline12224
        var t5907 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: t5906,
        }
        return t5907
    } else {
        var t5908 string = value__240.input
        var t5909 *ref_int_x = value__240.index
        var t5910 int
        var inline12250 int = ref_get__Ref_3int(t5909)
        t5910 = inline12250
        var mtmp824 uint8
        var inline12248 uint8 = _goml_runtime_core_string_byte_get(t5908, t5910)
        mtmp824 = inline12248
        switch mtmp824 {
        case 123:
            var t5913 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__object(value__240)
            return t5913
        case 91:
            var t5914 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__array(value__240)
            return t5914
        case 34:
            var mtmp825 Result__string__string = _goml_m_std_p_json_p_parse__json__string(value__240)
            switch mtmp825._tag {
            case 0:
                var x826 string = mtmp825._v0_0
                var t5917 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_String{
                    _0: x826,
                }
                var t5918 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                    _tag: 0,
                    _v0_0: t5917,
                }
                return t5918
            case 1:
                var x827 string = mtmp825._v1_0
                var t5919 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                    _tag: 1,
                    _v1_0: x827,
                }
                return t5919
            default:
                panic("non-exhaustive match")
            }
        case 116:
            var t5920 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: true,
            }
            var t5921 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "true", t5920)
            return t5921
        case 102:
            var t5922 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Bool{
                _0: false,
            }
            var t5923 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "false", t5922)
            return t5923
        case 110:
            var t5924 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__literal(value__240, "null", Null{})
            return t5924
        default:
            var t5932 bool = mtmp824 == 45
            var jp5928 bool
            if t5932 {
                jp5928 = true
            } else {
                var inline12226 bool = mtmp824 >= 48
                if inline12226 {
                    var inline12227 bool = mtmp824 <= 57
                    jp5928 = inline12227
                } else {
                    jp5928 = false
                }
            }
            if jp5928 {
                var inline12229 Result__string__string = _goml_m_std_p_json_p_parse__json__number__text(value__240)
                var inline12231 string
                switch inline12229._tag {
                case 0:
                    var inline12234 string = inline12229._v0_0
                    inline12231 = inline12234
                    var inline12232 _goml_m_std_p_json_p_Value = _goml_m_std_p_json_p_Value_Number{
                        _0: inline12231,
                    }
                    var inline12233 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 0,
                        _v0_0: inline12232,
                    }
                    return inline12233
                case 1:
                    var inline12236 string = inline12229._v1_0
                    var inline12238 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                        _tag: 1,
                        _v1_0: inline12236,
                    }
                    return inline12238
                default:
                    panic("non-exhaustive match")
                }
            } else {
                var t5930 string
                var inline12240 string = "unexpected JSON token"
                var inline12241 string = "" + inline12240
                var inline12242 string = inline12241 + " at byte "
                var inline12243 *ref_int_x = value__240.index
                var inline12244 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12243)
                var inline12245 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12244)
                var inline12246 string = inline12242 + inline12245
                t5930 = inline12246
                var t5931 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                    _tag: 1,
                    _v1_0: t5930,
                }
                return t5931
            }
        }
    }
}

func _goml_m_std_p_json_p_parse(input__244 string) _goml_m_Result____std_p_json_p_Value____string {
    var parser__245 _goml_m_std_p_json_p_JsonParser
    var inline12268 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(0)
    var inline12269 _goml_m_std_p_json_p_JsonParser = _goml_m_std_p_json_p_JsonParser{
        input: input__244,
        index: inline12268,
    }
    parser__245 = inline12269
    var mtmp828 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse__json__value(parser__245)
    var jp5937 _goml_m_std_p_json_p_Value
    switch mtmp828._tag {
    case 0:
        var x829 _goml_m_std_p_json_p_Value = mtmp828._v0_0
        jp5937 = x829
        _goml_m_std_p_json_p_skip__json__whitespace(parser__245)
        var t5940 *ref_int_x = parser__245.index
        var t5941 int
        var inline12266 int = ref_get__Ref_3int(t5940)
        t5941 = inline12266
        var t5942 int
        var inline12264 int = _goml_runtime_core_string_len(input__244)
        t5942 = inline12264
        var t5943 bool = t5941 == t5942
        if t5943 {
            var t5944 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                _tag: 0,
                _v0_0: jp5937,
            }
            return t5944
        } else {
            var t5945 string
            var inline12256 string = "trailing JSON data"
            var inline12257 string = "" + inline12256
            var inline12258 string = inline12257 + " at byte "
            var inline12259 *ref_int_x = parser__245.index
            var inline12260 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(inline12259)
            var inline12261 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline12260)
            var inline12262 string = inline12258 + inline12261
            t5945 = inline12262
            var t5946 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
                _tag: 1,
                _v1_0: t5945,
            }
            return t5946
        }
    case 1:
        var x830 string = mtmp828._v1_0
        var t5947 _goml_m_Result____std_p_json_p_Value____string = _goml_m_Result____std_p_json_p_Value____string{
            _tag: 1,
            _v1_0: x830,
        }
        return t5947
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_json_p_write__json__string(builder__248 _goml_m_std_p_text_p_StringBuilder, value__249 string) struct{} {
    var inline12302 rune = 34
    var inline12303 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12302)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline12303)
    var start__250 int = 0
    var for_index833 int = 0
    var for_limit834 int
    var inline12300 int = _goml_runtime_core_string_len(value__249)
    for_limit834 = inline12300
    Loop_loop5961:
    for {
        var t5962 bool = for_index833 < for_limit834
        if t5962 {
            var for_item835 int = for_index833
            var t5963 int = for_index833 + 1
            for_index833 = t5963
            var byte__252 uint8
            var inline12288 uint8 = _goml_runtime_core_string_byte_get(value__249, for_item835)
            byte__252 = inline12288
            var t6016 bool = byte__252 == 34
            var jp6014 bool
            if t6016 {
                jp6014 = true
            } else {
                var t6017 bool = byte__252 == 92
                jp6014 = t6017
            }
            var jp6011 bool
            if jp6014 {
                jp6011 = true
            } else {
                var t6015 bool = byte__252 == 8
                jp6011 = t6015
            }
            var jp6008 bool
            if jp6011 {
                jp6008 = true
            } else {
                var t6012 bool = byte__252 == 9
                jp6008 = t6012
            }
            var jp6005 bool
            if jp6008 {
                jp6005 = true
            } else {
                var t6009 bool = byte__252 == 10
                jp6005 = t6009
            }
            var jp6002 bool
            if jp6005 {
                jp6002 = true
            } else {
                var t6006 bool = byte__252 == 12
                jp6002 = t6006
            }
            var jp5999 bool
            if jp6002 {
                jp5999 = true
            } else {
                var t6003 bool = byte__252 == 13
                jp5999 = t6003
            }
            var jp5966 bool
            if jp5999 {
                jp5966 = true
            } else {
                var t6000 bool = byte__252 < 32
                jp5966 = t6000
            }
            if jp5966 {
                var t5995 bool = start__250 < for_item835
                if t5995 {
                    var t5996 string
                    var inline12274 string = string_byte_slice(value__249, start__250, for_item835)
                    t5996 = inline12274
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5996)
                } else {}
                var t5970 bool = byte__252 == 34
                if t5970 {
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\"")
                } else {
                    var t5973 bool = byte__252 == 92
                    if t5973 {
                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\\\")
                    } else {
                        var t5976 bool = byte__252 == 8
                        if t5976 {
                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\b")
                        } else {
                            var t5979 bool = byte__252 == 9
                            if t5979 {
                                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\t")
                            } else {
                                var t5982 bool = byte__252 == 10
                                if t5982 {
                                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\n")
                                } else {
                                    var t5985 bool = byte__252 == 12
                                    if t5985 {
                                        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\f")
                                    } else {
                                        var t5988 bool = byte__252 == 13
                                        if t5988 {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\r")
                                        } else {
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, "\\u00")
                                            var t5990 uint8 = byte__252 / 16
                                            var t5991 rune
                                            var inline12285 int = int(uint8(t5990))
                                            var inline12286 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline12285)
                                            t5991 = inline12286
                                            var inline12282 string = _goml_m_inherent_i_char_i_char_i_to__string(t5991)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline12282)
                                            var t5992_rhs uint8 = 16
                                            var t5992 uint8 = byte__252 % t5992_rhs
                                            var t5993 rune
                                            var inline12279 int = int(uint8(t5992))
                                            var inline12280 rune = _goml_m_inherent_i_string_i_string_i_get("0123456789abcdef", inline12279)
                                            t5993 = inline12280
                                            var inline12276 string = _goml_m_inherent_i_char_i_char_i_to__string(t5993)
                                            _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline12276)
                                        }
                                    }
                                }
                            }
                        }
                    }
                }
                var t5969 int = for_item835 + 1
                start__250 = t5969
                continue
            } else {
                continue
            }
        } else {
            break Loop_loop5961
        }
    }
    var t5956 int
    var inline12298 int = _goml_runtime_core_string_len(value__249)
    t5956 = inline12298
    var t5957 bool = start__250 < t5956
    if t5957 {
        var t5958 int
        var inline12292 int = _goml_runtime_core_string_len(value__249)
        t5958 = inline12292
        var t5959 string
        var inline12290 string = string_byte_slice(value__249, start__250, t5958)
        t5959 = inline12290
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, t5959)
    } else {}
    var inline12294 rune = 34
    var inline12295 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12294)
    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__248, inline12295)
    return struct{}{}
}

func _goml_m_std_p_json_p_write__json__value(builder__253 _goml_m_std_p_text_p_StringBuilder, value__254 _goml_m_std_p_json_p_Value) struct{} {
    switch value__254.(type) {
    case Object:
        var x844 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__254.(Object)._0
        var inline12318 rune = 123
        var inline12319 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12318)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12319)
        var index__256 int = 0
        var for_limit851 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844)
        var for_index852 int = 0
        Loop_loop6022:
        for {
            var t6023 bool = for_index852 < for_limit851
            if t6023 {
                var for_item853 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x844, for_index852)
                var t6024 int = for_index852 + 1
                for_index852 = t6024
                var t6030 bool = index__256 > 0
                if t6030 {
                    var inline12306 rune = 44
                    var inline12307 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12306)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12307)
                } else {}
                var t6026 string = for_item853._0
                _goml_m_std_p_json_p_write__json__string(builder__253, t6026)
                var inline12310 rune = 58
                var inline12311 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12310)
                _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12311)
                var t6027 _goml_m_std_p_json_p_Value = for_item853._1
                _goml_m_std_p_json_p_write__json__value(builder__253, t6027)
                var compound_old859 int = index__256
                var compound_value860 int = 1
                var t6028 int = compound_old859 + compound_value860
                index__256 = t6028
                continue
            } else {
                break Loop_loop6022
            }
        }
        var inline12314 rune = 125
        var inline12315 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12314)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12315)
        return struct{}{}
    case _goml_m_std_p_json_p_Value_Array:
        var x845 *_goml_vec__goml_m_std_p_json_p_Value = value__254.(_goml_m_std_p_json_p_Value_Array)._0
        var inline12330 rune = 91
        var inline12331 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12330)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12331)
        var index__259 int = 0
        var for_limit865 int = vec_len___goml_m_Vec__16std_p_json_p_Value(x845)
        var for_index866 int = 0
        Loop_loop6034:
        for {
            var t6035 bool = for_index866 < for_limit865
            if t6035 {
                var for_item867 _goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__16std_p_json_p_Value(x845, for_index866)
                var t6036 int = for_index866 + 1
                for_index866 = t6036
                var t6040 bool = index__259 > 0
                if t6040 {
                    var inline12322 rune = 44
                    var inline12323 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12322)
                    _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12323)
                } else {}
                _goml_m_std_p_json_p_write__json__value(builder__253, for_item867)
                var compound_old871 int = index__259
                var compound_value872 int = 1
                var t6038 int = compound_old871 + compound_value872
                index__259 = t6038
                continue
            } else {
                break Loop_loop6034
            }
        }
        var inline12326 rune = 93
        var inline12327 string = _goml_m_inherent_i_char_i_char_i_to__string(inline12326)
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, inline12327)
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
        var jp6045 string
        if x848 {
            jp6045 = "true"
        } else {
            jp6045 = "false"
        }
        _goml_m_inherent_i_std_p_text__h0034629766b91c65ed1f7160ea470eda_i_write__string(builder__253, jp6045)
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
    var inline12339 [0]uint8 = [0]uint8{}
    var inline12340 *_goml_vec_uint8 = func(values [0]uint8) *_goml_vec_uint8 {
        return &_goml_vec_uint8{
            items: values[0:len(values)],
        }
    }(inline12339)
    var inline12341 _goml_m_std_p_text_p_StringBuilder = _goml_m_std_p_text_p_StringBuilder{
        values: inline12340,
    }
    builder__265 = inline12341
    _goml_m_std_p_json_p_write__json__value(builder__265, value__264)
    var inline12334 *_goml_vec_uint8 = builder__265.values
    var inline12335 Tuple2_4bool_6string = string_from_utf8(inline12334)
    var inline12336 string = inline12335._1
    return inline12336
}

func _goml_m_std_p_json_p_field(value__266 _goml_m_std_p_json_p_Value, name__267 string) _goml_m_Option____std_p_json_p_Value {
    switch value__266.(type) {
    case Object:
        var x876 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value = value__266.(Object)._0
        var for_limit882 int = vec_len___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876)
        var for_index883 int = 0
        Loop_loop6056:
        for {
            var t6057 bool = for_index883 < for_limit882
            if t6057 {
                var for_item884 Tuple2_6string_26_goml_m_std_p_json_p_Value = vec_get___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(x876, for_index883)
                var t6058 int = for_index883 + 1
                for_index883 = t6058
                var t6060 string = for_item884._0
                var t6061 bool = t6060 == name__267
                if t6061 {
                    var t6062 _goml_m_std_p_json_p_Value = for_item884._1
                    var t6063 _goml_m_Option____std_p_json_p_Value = _goml_m_Option____std_p_json_p_Value{
                        _tag: 1,
                        _v1_0: t6062,
                    }
                    return t6063
                } else {
                    continue
                }
            } else {
                break Loop_loop6056
            }
        }
        return _goml_m_Option____std_p_json_p_Value{
            _tag: 0,
        }
    default:
        return _goml_m_Option____std_p_json_p_Value{
            _tag: 0,
        }
    }
}

func _goml_m_std_p_json_p_parse__json__int__text(value__272 string) Option__isize {
    var t6073 int
    var inline12352 int = _goml_runtime_core_string_len(value__272)
    t6073 = inline12352
    var t6074 bool = t6073 == 0
    if t6074 {
        return Option__isize{
            _tag: 0,
        }
    } else {
        var t6075 uint8
        var inline12349 int = 0
        var inline12350 uint8 = _goml_runtime_core_string_byte_get(value__272, inline12349)
        t6075 = inline12350
        var negative__273 bool = t6075 == 45
        var jp6077 int
        if negative__273 {
            jp6077 = 1
        } else {
            jp6077 = 0
        }
        var index__274 int = jp6077
        var result__275 int = 0
        var t6098 int
        var inline12347 int = _goml_runtime_core_string_len(value__272)
        t6098 = inline12347
        var t6099 bool = index__274 == t6098
        if t6099 {
            return Option__isize{
                _tag: 0,
            }
        } else {
            Loop_loop6084:
            for {
                var t6085 int
                var inline12345 int = _goml_runtime_core_string_len(value__272)
                t6085 = inline12345
                var t6086 bool = index__274 < t6085
                if t6086 {
                    var byte__276 uint8
                    var inline12343 uint8 = _goml_runtime_core_string_byte_get(value__272, index__274)
                    byte__276 = inline12343
                    var t6096 bool = byte__276 < 48
                    var jp6091 bool
                    if t6096 {
                        jp6091 = true
                    } else {
                        var t6097 bool = byte__276 > 57
                        jp6091 = t6097
                    }
                    if jp6091 {
                        return Option__isize{
                            _tag: 0,
                        }
                    } else {
                        var t6092 int = result__275 * 10
                        var t6093 uint8 = byte__276 - 48
                        var t6094 int = int(uint8(t6093))
                        var t6095 int = t6092 + t6094
                        result__275 = t6095
                        var compound_old895 int = index__274
                        var compound_value896 int = 1
                        var t6088 int = compound_old895 + compound_value896
                        index__274 = t6088
                        continue
                    }
                } else {
                    break Loop_loop6084
                }
            }
            var jp6081 int
            if negative__273 {
                var t6083 int = 0 - result__275
                jp6081 = t6083
            } else {
                jp6081 = result__275
            }
            var t6082 Option__isize = Option__isize{
                _tag: 1,
                _v1_0: jp6081,
            }
            return t6082
        }
    }
}

func main0() struct{} {
    var mtmp796 _goml_m_Result____std_p_json_p_Value____string = _goml_m_std_p_json_p_parse("{\"name\":\"goml\",\"version\":1,\"stable\":true}")
    var jp6876 _goml_m_std_p_json_p_Value
    switch mtmp796._tag {
    case 0:
        var x797 _goml_m_std_p_json_p_Value = mtmp796._v0_0
        jp6876 = x797
        var mtmp800 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6876, "name")
        switch mtmp800._tag {
        case 0:
            var inline12790 string = "missing name"
            var inline12791 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12790)
            _goml_runtime_core_string_println(inline12791)
            var mtmp805 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6876, "version")
            switch mtmp805._tag {
            case 0:
                var inline12805 string = "missing version"
                var inline12806 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12805)
                _goml_runtime_core_string_println(inline12806)
            case 1:
                var x806 _goml_m_std_p_json_p_Value = mtmp805._v1_0
                var mtmp807 Option__isize
                switch x806.(type) {
                case _goml_m_std_p_json_p_Value_Number:
                    var inline12816 string = x806.(_goml_m_std_p_json_p_Value_Number)._0
                    var inline12818 Option__isize = _goml_m_std_p_json_p_parse__json__int__text(inline12816)
                    mtmp807 = inline12818
                default:
                    mtmp807 = Option__isize{
                        _tag: 0,
                    }
                }
                switch mtmp807._tag {
                case 0:
                    var inline12809 string = "invalid version"
                    var inline12810 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12809)
                    _goml_runtime_core_string_println(inline12810)
                case 1:
                    var x808 int = mtmp807._v1_0
                    var inline12813 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x808)
                    _goml_runtime_core_string_println(inline12813)
                default:
                    panic("non-exhaustive match")
                }
            default:
                panic("non-exhaustive match")
            }
            var mtmp810 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6876, "stable")
            switch mtmp810._tag {
            case 0:
                var inline12820 string = "missing stable"
                var inline12821 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12820)
                _goml_runtime_core_string_println(inline12821)
                var t6880 string = _goml_m_std_p_json_p_encode(jp6876)
                println__T_string(t6880)
                return struct{}{}
            case 1:
                var x811 _goml_m_std_p_json_p_Value = mtmp810._v1_0
                var commute_field13762 bool
                switch x811.(type) {
                case _goml_m_std_p_json_p_Value_Bool:
                    var inline12831 bool = x811.(_goml_m_std_p_json_p_Value_Bool)._0
                    commute_field13762 = inline12831
                    var inline12828 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field13762)
                    _goml_runtime_core_string_println(inline12828)
                    var t6880 string = _goml_m_std_p_json_p_encode(jp6876)
                    println__T_string(t6880)
                    return struct{}{}
                default:
                    var inline12824 string = "invalid stable"
                    var inline12825 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12824)
                    _goml_runtime_core_string_println(inline12825)
                    var t6880 string = _goml_m_std_p_json_p_encode(jp6876)
                    println__T_string(t6880)
                    return struct{}{}
                }
            default:
                panic("non-exhaustive match")
            }
        case 1:
            var x801 _goml_m_std_p_json_p_Value = mtmp800._v1_0
            var commute_field13768 string
            switch x801.(type) {
            case _goml_m_std_p_json_p_Value_String:
                var inline12801 string = x801.(_goml_m_std_p_json_p_Value_String)._0
                commute_field13768 = inline12801
                var inline12798 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(commute_field13768)
                _goml_runtime_core_string_println(inline12798)
                var mtmp805 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6876, "version")
                switch mtmp805._tag {
                case 0:
                    var inline12805 string = "missing version"
                    var inline12806 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12805)
                    _goml_runtime_core_string_println(inline12806)
                case 1:
                    var x806 _goml_m_std_p_json_p_Value = mtmp805._v1_0
                    var mtmp807 Option__isize
                    switch x806.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline12816 string = x806.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline12818 Option__isize = _goml_m_std_p_json_p_parse__json__int__text(inline12816)
                        mtmp807 = inline12818
                    default:
                        mtmp807 = Option__isize{
                            _tag: 0,
                        }
                    }
                    switch mtmp807._tag {
                    case 0:
                        var inline12809 string = "invalid version"
                        var inline12810 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12809)
                        _goml_runtime_core_string_println(inline12810)
                    case 1:
                        var x808 int = mtmp807._v1_0
                        var inline12813 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x808)
                        _goml_runtime_core_string_println(inline12813)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp810 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6876, "stable")
                switch mtmp810._tag {
                case 0:
                    var inline12820 string = "missing stable"
                    var inline12821 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12820)
                    _goml_runtime_core_string_println(inline12821)
                    var t6880 string = _goml_m_std_p_json_p_encode(jp6876)
                    println__T_string(t6880)
                    return struct{}{}
                case 1:
                    var x811 _goml_m_std_p_json_p_Value = mtmp810._v1_0
                    var commute_field13762 bool
                    switch x811.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline12831 bool = x811.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field13762 = inline12831
                        var inline12828 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field13762)
                        _goml_runtime_core_string_println(inline12828)
                        var t6880 string = _goml_m_std_p_json_p_encode(jp6876)
                        println__T_string(t6880)
                        return struct{}{}
                    default:
                        var inline12824 string = "invalid stable"
                        var inline12825 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12824)
                        _goml_runtime_core_string_println(inline12825)
                        var t6880 string = _goml_m_std_p_json_p_encode(jp6876)
                        println__T_string(t6880)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            default:
                var inline12794 string = "invalid name"
                var inline12795 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12794)
                _goml_runtime_core_string_println(inline12795)
                var mtmp805 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6876, "version")
                switch mtmp805._tag {
                case 0:
                    var inline12805 string = "missing version"
                    var inline12806 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12805)
                    _goml_runtime_core_string_println(inline12806)
                case 1:
                    var x806 _goml_m_std_p_json_p_Value = mtmp805._v1_0
                    var mtmp807 Option__isize
                    switch x806.(type) {
                    case _goml_m_std_p_json_p_Value_Number:
                        var inline12816 string = x806.(_goml_m_std_p_json_p_Value_Number)._0
                        var inline12818 Option__isize = _goml_m_std_p_json_p_parse__json__int__text(inline12816)
                        mtmp807 = inline12818
                    default:
                        mtmp807 = Option__isize{
                            _tag: 0,
                        }
                    }
                    switch mtmp807._tag {
                    case 0:
                        var inline12809 string = "invalid version"
                        var inline12810 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12809)
                        _goml_runtime_core_string_println(inline12810)
                    case 1:
                        var x808 int = mtmp807._v1_0
                        var inline12813 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(x808)
                        _goml_runtime_core_string_println(inline12813)
                    default:
                        panic("non-exhaustive match")
                    }
                default:
                    panic("non-exhaustive match")
                }
                var mtmp810 _goml_m_Option____std_p_json_p_Value = _goml_m_std_p_json_p_field(jp6876, "stable")
                switch mtmp810._tag {
                case 0:
                    var inline12820 string = "missing stable"
                    var inline12821 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12820)
                    _goml_runtime_core_string_println(inline12821)
                    var t6880 string = _goml_m_std_p_json_p_encode(jp6876)
                    println__T_string(t6880)
                    return struct{}{}
                case 1:
                    var x811 _goml_m_std_p_json_p_Value = mtmp810._v1_0
                    var commute_field13762 bool
                    switch x811.(type) {
                    case _goml_m_std_p_json_p_Value_Bool:
                        var inline12831 bool = x811.(_goml_m_std_p_json_p_Value_Bool)._0
                        commute_field13762 = inline12831
                        var inline12828 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(commute_field13762)
                        _goml_runtime_core_string_println(inline12828)
                        var t6880 string = _goml_m_std_p_json_p_encode(jp6876)
                        println__T_string(t6880)
                        return struct{}{}
                    default:
                        var inline12824 string = "invalid stable"
                        var inline12825 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline12824)
                        _goml_runtime_core_string_println(inline12825)
                        var t6880 string = _goml_m_std_p_json_p_encode(jp6876)
                        println__T_string(t6880)
                        return struct{}{}
                    }
                default:
                    panic("non-exhaustive match")
                }
            }
        default:
            panic("non-exhaustive match")
        }
    case 1:
        var x798 string = mtmp796._v1_0
        var inline12787 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x798)
        _goml_runtime_core_string_println(inline12787)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func string_from_utf8(bytes__277 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp395 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__277)
    var x397 string = mtmp395._1
    var index__279 int = 0
    Loop_loop6933:
    for {
        var t6934 int
        var inline12842 int = _goml_runtime_core_string_len(x397)
        t6934 = inline12842
        var t6935 bool = index__279 < t6934
        if t6935 {
            var mtmp398 Tuple3_4bool_4char_3int = string_decode_utf8_at(x397, index__279)
            var x399 bool = mtmp398._0
            var x401 int = mtmp398._2
            if x399 {
                var compound_old402 int = index__279
                var t6937 int = compound_old402 + x401
                index__279 = t6937
                continue
            } else {
                var t6939 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t6939
            }
        } else {
            break Loop_loop6933
        }
    }
    var t6932 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x397,
    }
    return t6932
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t6960 int = _goml_runtime_core_string_len(self__289)
    return t6960
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t6963 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t6963
}

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__285 int) string {
    var inline12852 int64 = int64(int(self__285))
    var inline12853 string = signed_decimal_string(inline12852)
    return inline12853
}

func _goml_m_inherent_i_string_i_string_i_get(self__290 string, index__291 int) rune {
    var inline12891 Tuple3_4bool_4char_3int = string_decode_utf8_at(self__290, index__291)
    var inline12892 bool = inline12891._0
    var inline12893 rune = inline12891._1
    if inline12892 {
        return inline12893
    } else {
        var inline12896 rune = _goml_runtime_core_string_get("", -1)
        return inline12896
    }
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__isize(value__684 int) *ref_int_x {
    var t7067 *ref_int_x = ref__Ref_3int(value__684)
    return t7067
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__isize(self__685 *ref_int_x) int {
    var t7070 int = ref_get__Ref_3int(self__685)
    return t7070
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__isize(self__686 *ref_int_x, value__687 int) struct{} {
    ref_set__Ref_3int(self__686, value__687)
    return struct{}{}
}

func _goml_m_inherent_i_char_i_char_i_to__string(self__287 rune) string {
    var inline12903 uint32 = uint32(rune(self__287))
    var inline12904 bool = utf8_valid_scalar(inline12903)
    if inline12904 {
        var inline12905 string = _goml_runtime_core_char_to_string(self__287)
        return inline12905
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_inherent_i_string_i_string_i_byte__slice(self__294 string, start__295 int, end__296 int) string {
    var inline13203 bool = string_is_char_boundary(self__294, start__295)
    var inline13205 bool
    if inline13203 {
        var inline13208 bool = string_is_char_boundary(self__294, end__296)
        inline13205 = inline13208
    } else {
        inline13205 = false
    }
    if inline13205 {
        var inline13206 string = _goml_runtime_core_string_byte_slice(self__294, start__295, end__296)
        return inline13206
    } else {
        var inline13207 string = _goml_runtime_core_string_byte_slice(self__294, -1, -1)
        return inline13207
    }
}

func char_from_u32(value__2 uint32) Option__char {
    var inline13214 bool = utf8_valid_scalar(value__2)
    if inline13214 {
        var inline13215 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__2)
        var inline13216 rune = inline13215._1
        var inline13218 Option__char = Option__char{
            _tag: 1,
            _v1_0: inline13216,
        }
        return inline13218
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_inherent_i_Vec_i_Vec_l_h153048c8bd06f0dfabad32cddaecb150_json_p_Value_q_(self__511 *_goml_vec__goml_m_Tuple2__6string__16std_p_json_p_Value, elem__512 Tuple2_6string_26_goml_m_std_p_json_p_Value) struct{} {
    vec_push___goml_m_Vec__33Tuple2__6string__16std_p_json_p_Value(self__511, elem__512)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t7493 string
    t7493 = value__1
    _goml_runtime_core_string_println(t7493)
    return struct{}{}
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t7619 bool = index__259 < 0
    var jp7617 bool
    if t7619 {
        jp7617 = true
    } else {
        var t7620 bool = index__259 >= length__260
        jp7617 = t7620
    }
    if jp7617 {
        var inline13229 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline13229
    } else {
        var t7504 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t7504))
        var t7507 bool = first__261 < 128
        if t7507 {
            var inline13231 int = 1
            var inline13232 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline13232._tag {
            case 0:
                var inline13233 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline13233
            case 1:
                var inline13234 rune = inline13232._v1_0
                var inline13236 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline13234,
                    _2: inline13231,
                }
                return inline13236
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t7511 bool = first__261 < 194
            if t7511 {
                var inline13238 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline13238
            } else {
                var t7515 bool = first__261 < 224
                if t7515 {
                    var t7528 int = length__260 - index__259
                    var t7529 bool = t7528 < 2
                    if t7529 {
                        var inline13240 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline13240
                    } else {
                        var t7517 int = index__259 + 1
                        var t7518 uint8
                        var inline13254 uint8 = _goml_runtime_core_string_byte_get(value__258, t7517)
                        t7518 = inline13254
                        var second__262 uint32 = uint32(uint8(t7518))
                        var t7521 bool
                        var inline13251 bool = second__262 < 128
                        if inline13251 {
                            t7521 = true
                        } else {
                            var inline13252 bool = second__262 > 191
                            t7521 = inline13252
                        }
                        if t7521 {
                            var inline13242 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline13242
                        } else {
                            var t7523_rhs uint32 = 31
                            var t7523 uint32 = first__261 & t7523_rhs
                            var t7524_rhs int = 6
                            var t7524 uint32 = t7523 << t7524_rhs
                            var t7525_rhs uint32 = 63
                            var t7525 uint32 = second__262 & t7525_rhs
                            var t7526 uint32 = t7524 | t7525
                            var inline13244 int = 2
                            var inline13245 Option__char = __goml_builtin_char_from_uint32(t7526)
                            switch inline13245._tag {
                            case 0:
                                var inline13246 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline13246
                            case 1:
                                var inline13247 rune = inline13245._v1_0
                                var inline13249 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline13247,
                                    _2: inline13244,
                                }
                                return inline13249
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t7533 bool = first__261 < 240
                    if t7533 {
                        var t7566 int = length__260 - index__259
                        var t7567 bool = t7566 < 3
                        if t7567 {
                            var inline13256 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline13256
                        } else {
                            var t7535 int = index__259 + 1
                            var t7536 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t7535)
                            var second__263 uint32 = uint32(uint8(t7536))
                            var t7537 int = index__259 + 2
                            var t7538 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t7537)
                            var third__264 uint32 = uint32(uint8(t7538))
                            var t7564 bool = utf8_invalid_continuation(second__263)
                            var jp7559 bool
                            if t7564 {
                                jp7559 = true
                            } else {
                                var inline13258 bool = third__264 < 128
                                if inline13258 {
                                    jp7559 = true
                                } else {
                                    var inline13259 bool = third__264 > 191
                                    jp7559 = inline13259
                                }
                            }
                            var jp7553 bool
                            if jp7559 {
                                jp7553 = true
                            } else {
                                var t7562 bool = first__261 == 224
                                if t7562 {
                                    var t7563 bool = second__263 < 160
                                    jp7553 = t7563
                                } else {
                                    jp7553 = false
                                }
                            }
                            var jp7542 bool
                            if jp7553 {
                                jp7542 = true
                            } else {
                                var t7556 bool = first__261 == 237
                                if t7556 {
                                    var t7557 bool = second__263 >= 160
                                    jp7542 = t7557
                                } else {
                                    jp7542 = false
                                }
                            }
                            if jp7542 {
                                var inline13261 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline13261
                            } else {
                                var t7544_rhs uint32 = 15
                                var t7544 uint32 = first__261 & t7544_rhs
                                var t7545_rhs int = 12
                                var t7545 uint32 = t7544 << t7545_rhs
                                var t7546_rhs uint32 = 63
                                var t7546 uint32 = second__263 & t7546_rhs
                                var t7547_rhs int = 6
                                var t7547 uint32 = t7546 << t7547_rhs
                                var t7548 uint32 = t7545 | t7547
                                var t7549_rhs uint32 = 63
                                var t7549 uint32 = third__264 & t7549_rhs
                                var t7550 uint32 = t7548 | t7549
                                var inline13263 int = 3
                                var inline13264 Option__char = __goml_builtin_char_from_uint32(t7550)
                                switch inline13264._tag {
                                case 0:
                                    var inline13265 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline13265
                                case 1:
                                    var inline13266 rune = inline13264._v1_0
                                    var inline13268 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline13266,
                                        _2: inline13263,
                                    }
                                    return inline13268
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t7571 bool = first__261 < 245
                        if t7571 {
                            var t7612 int = length__260 - index__259
                            var t7613 bool = t7612 < 4
                            if t7613 {
                                var t7614 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t7614
                            } else {
                                var t7573 int = index__259 + 1
                                var t7574 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t7573)
                                var second__265 uint32 = uint32(uint8(t7574))
                                var t7575 int = index__259 + 2
                                var t7576 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t7575)
                                var third__266 uint32 = uint32(uint8(t7576))
                                var t7577 int = index__259 + 3
                                var t7578 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t7577)
                                var fourth__267 uint32 = uint32(uint8(t7578))
                                var t7610 bool = utf8_invalid_continuation(second__265)
                                var jp7608 bool
                                if t7610 {
                                    jp7608 = true
                                } else {
                                    var t7611 bool = utf8_invalid_continuation(third__266)
                                    jp7608 = t7611
                                }
                                var jp7602 bool
                                if jp7608 {
                                    jp7602 = true
                                } else {
                                    var t7609 bool = utf8_invalid_continuation(fourth__267)
                                    jp7602 = t7609
                                }
                                var jp7596 bool
                                if jp7602 {
                                    jp7596 = true
                                } else {
                                    var t7605 bool = first__261 == 240
                                    if t7605 {
                                        var t7606 bool = second__265 < 144
                                        jp7596 = t7606
                                    } else {
                                        jp7596 = false
                                    }
                                }
                                var jp7582 bool
                                if jp7596 {
                                    jp7582 = true
                                } else {
                                    var t7599 bool = first__261 == 244
                                    if t7599 {
                                        var t7600 bool = second__265 > 143
                                        jp7582 = t7600
                                    } else {
                                        jp7582 = false
                                    }
                                }
                                if jp7582 {
                                    var t7583 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t7583
                                } else {
                                    var t7584_rhs uint32 = 7
                                    var t7584 uint32 = first__261 & t7584_rhs
                                    var t7585_rhs int = 18
                                    var t7585 uint32 = t7584 << t7585_rhs
                                    var t7586_rhs uint32 = 63
                                    var t7586 uint32 = second__265 & t7586_rhs
                                    var t7587_rhs int = 12
                                    var t7587 uint32 = t7586 << t7587_rhs
                                    var t7588 uint32 = t7585 | t7587
                                    var t7589_rhs uint32 = 63
                                    var t7589 uint32 = third__266 & t7589_rhs
                                    var t7590_rhs int = 6
                                    var t7590 uint32 = t7589 << t7590_rhs
                                    var t7591 uint32 = t7588 | t7590
                                    var t7592_rhs uint32 = 63
                                    var t7592 uint32 = fourth__267 & t7592_rhs
                                    var t7593 uint32 = t7591 | t7592
                                    var t7594 Tuple3_4bool_4char_3int = utf8_valid_decode(t7593, 4)
                                    return t7594
                                }
                            }
                        } else {
                            var t7615 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t7615
                        }
                    }
                }
            }
        }
    }
}

func __goml_builtin_int_to_string(value__222 int) string {
    var t7774 int64 = int64(int(value__222))
    var inline13277 bool = t7774 < 0
    if inline13277 {
        var inline13278 uint64 = uint64(int64(t7774))
        var inline13279 uint64 = 0 - inline13278
        var inline13280 string = decimal_string(inline13279)
        var inline13281 string = "-" + inline13280
        return inline13281
    } else {
        var inline13282 uint64 = uint64(int64(t7774))
        var inline13283 string = decimal_string(inline13282)
        return inline13283
    }
}

func char_to_string(value__282 rune) string {
    var t7832 uint32 = uint32(rune(value__282))
    var t7833 bool
    var inline13317 bool = t7832 <= 1114111
    if inline13317 {
        var inline13318 bool = t7832 >= 55296
        var inline13320 bool
        if inline13318 {
            var inline13322 bool = t7832 <= 57343
            inline13320 = inline13322
        } else {
            inline13320 = false
        }
        var inline13321 bool = !inline13320
        t7833 = inline13321
    } else {
        t7833 = false
    }
    if t7833 {
        var t7834 string = _goml_runtime_core_char_to_string(value__282)
        return t7834
    } else {
        _goml_runtime_core_string_get("", -1)
        return ""
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t7987 bool = string_is_char_boundary(value__274, start__275)
    var jp7984 bool
    if t7987 {
        var t7988 bool = string_is_char_boundary(value__274, end__276)
        jp7984 = t7988
    } else {
        jp7984 = false
    }
    if jp7984 {
        var t7985 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t7985
    } else {
        var t7986 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t7986
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t7995 bool
    var inline13362 bool = value__283 <= 1114111
    if inline13362 {
        var inline13363 bool = value__283 >= 55296
        var inline13365 bool
        if inline13363 {
            var inline13367 bool = value__283 <= 57343
            inline13365 = inline13367
        } else {
            inline13365 = false
        }
        var inline13366 bool = !inline13365
        t7995 = inline13366
    } else {
        t7995 = false
    }
    if t7995 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t7996 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t7996
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__404 int) string {
    var inline13369 int64 = int64(int(self__404))
    var inline13370 string = signed_decimal_string(inline13369)
    return inline13370
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t8004 string = _goml_runtime_core_bool_to_string(self__401)
    return t8004
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t8007 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t8007
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field13805 rune
    var inline13374 bool = utf8_valid_scalar(value__253)
    if inline13374 {
        var inline13375 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline13376 rune = inline13375._1
        commute_field13805 = inline13376
        var t8013 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field13805,
            _2: width__254,
        }
        return t8013
    } else {
        var inline13372 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline13372
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t8018 bool = value__256 < 128
    if t8018 {
        return true
    } else {
        var t8019 bool = value__256 > 191
        return t8019
    }
}

func signed_decimal_string(value__214 int64) string {
    var t8377 bool = value__214 < 0
    if t8377 {
        var t8378 uint64 = uint64(int64(value__214))
        var t8379 uint64 = 0 - t8378
        var t8380 string = decimal_string(t8379)
        var t8381 string = "-" + t8380
        return t8381
    } else {
        var t8382 uint64 = uint64(int64(value__214))
        var t8383 string = decimal_string(t8382)
        return t8383
    }
}

func decimal_string(value__208 uint64) string {
    var t8406 bool = value__208 == 0
    if t8406 {
        return "0"
    } else {
        var reversed__209 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(20)
        var remaining__210 uint64 = value__208
        Loop_loop8399:
        for {
            var t8400 bool = remaining__210 > 0
            if t8400 {
                var t8401_rhs uint64 = 10
                var t8401 uint64 = remaining__210 % t8401_rhs
                var t8402 uint8 = uint8(uint64(t8401))
                var t8403 uint8 = t8402 + 48
                vec_push__Vec_5uint8(reversed__209, t8403)
                var compound_old353 uint64 = remaining__210
                var compound_value354 uint64 = 10
                var t8404 uint64 = compound_old353 / compound_value354
                remaining__210 = t8404
                continue
            } else {
                break Loop_loop8399
            }
        }
        var t8388 int
        var inline13459 int = vec_len__Vec_5uint8(reversed__209)
        t8388 = inline13459
        var bytes__211 *_goml_vec_uint8 = vec_with_capacity__Vec_5uint8(t8388)
        var offset__212 int = 0
        Loop_loop8390:
        for {
            var t8391 int
            var inline13457 int = vec_len__Vec_5uint8(reversed__209)
            t8391 = inline13457
            var t8392 bool = offset__212 < t8391
            if t8392 {
                var t8393 int
                var inline13455 int = vec_len__Vec_5uint8(reversed__209)
                t8393 = inline13455
                var t8394 int = t8393 - offset__212
                var t8395 int = t8394 - 1
                var t8396 uint8 = vec_get__Vec_5uint8(reversed__209, t8395)
                vec_push__Vec_5uint8(bytes__211, t8396)
                var compound_old358 int = offset__212
                var compound_value359 int = 1
                var t8397 int = compound_old358 + compound_value359
                offset__212 = t8397
                continue
            } else {
                break Loop_loop8390
            }
        }
        var mtmp362 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__211)
        var x364 string = mtmp362._1
        return x364
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t8492 bool = value__257 <= 1114111
    if t8492 {
        var t8496 bool = value__257 >= 55296
        var jp8494 bool
        if t8496 {
            var t8497 bool = value__257 <= 57343
            jp8494 = t8497
        } else {
            jp8494 = false
        }
        var t8495 bool = !jp8494
        return t8495
    } else {
        return false
    }
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t8511 bool = index__269 < 0
    var jp8503 bool
    if t8511 {
        jp8503 = true
    } else {
        var t8512 int
        var inline13475 int = _goml_runtime_core_string_len(value__268)
        t8512 = inline13475
        var t8513 bool = index__269 > t8512
        jp8503 = t8513
    }
    if jp8503 {
        return false
    } else {
        var t8506 int
        var inline13479 int = _goml_runtime_core_string_len(value__268)
        t8506 = inline13479
        var t8507 bool = index__269 == t8506
        if t8507 {
            return true
        } else {
            var t8508 uint8
            var inline13477 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t8508 = inline13477
            var t8509_rhs uint8 = 192
            var t8509 uint8 = t8508 & t8509_rhs
            var t8510 bool = t8509 != 128
            return t8510
        }
    }
}

func main() {
    main0()
}
