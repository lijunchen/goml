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

func _goml_runtime_core_string_len(s string) int {
    return int(len(s))
}

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
}

func _goml_runtime_core_string_byte_slice(s string, start int, end int) string {
    return s[start:end]
}

func _goml_runtime_core_string_to_bytes(s string) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: []byte(s),
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
    }
}

func _goml_runtime_core_char_from_uint32(value uint32) Tuple2_4bool_4char {
    return Tuple2_4bool_4char{
        _0: true,
        _1: rune(value),
    }
}

func _goml_runtime_std_fs_read_bytes(path string) Tuple3_4bool_10Vec_5uint8_6string {
    var data []uint8
    var err error
    data, err = _goml_os.ReadFile(path)
    if err != nil {
        return Tuple3_4bool_10Vec_5uint8_6string{
            _0: false,
            _1: &_goml_vec_uint8{
                items: nil,
            },
            _2: err.Error(),
        }
    }
    return Tuple3_4bool_10Vec_5uint8_6string{
        _0: true,
        _1: &_goml_vec_uint8{
            items: data,
        },
        _2: "",
    }
}

func _goml_runtime_std_fs_write_bytes(path string, data *_goml_vec_uint8) Tuple2_4bool_6string {
    var err error = _goml_os.WriteFile(path, data.items, 0644)
    if err != nil {
        return Tuple2_4bool_6string{
            _0: false,
            _1: err.Error(),
        }
    }
    return Tuple2_4bool_6string{
        _0: true,
        _1: "",
    }
}

func _goml_runtime_std_fs_create_dir_all(path string) Tuple2_4bool_6string {
    var err error = _goml_os.MkdirAll(path, 0755)
    if err != nil {
        return Tuple2_4bool_6string{
            _0: false,
            _1: err.Error(),
        }
    }
    return Tuple2_4bool_6string{
        _0: true,
        _1: "",
    }
}

func _goml_runtime_std_io_println(value string) struct{} {
    _goml_os.Stdout.WriteString(value + "\n")
    return struct{}{}
}

func _goml_runtime_std_io_eprint(value string) struct{} {
    _goml_os.Stderr.WriteString(value)
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

type _goml_vec_string struct {
    items []string
}

type _goml_vec_Tuple2_6string_6string struct {
    items []Tuple2_6string_6string
}

type _goml_vec__goml_m_std_p_fs_p_DirEntry struct {
    items []_goml_m_std_p_fs_p_DirEntry
}

type _goml_vec_uint32 struct {
    items []uint32
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_6string_6string struct {
    _0 bool
    _1 string
    _2 string
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

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
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

type _goml_m_std_p_fs_p_Error struct {
    details _goml_m_std_p_io_p_ErrorDetails
}

type _goml_m_std_p_fs_p_Permissions struct {
    mode_value uint32
}

type _goml_m_std_p_fs_p_Metadata struct {
    file_type_value _goml_m_std_p_fs_p_FileType
    length_value int64
    permissions_value _goml_m_std_p_fs_p_Permissions
    modified_unix_nanoseconds_value int64
}

type _goml_m_std_p_fs_p_DirEntry struct {
    name_value string
    path_value string
    file_type_value _goml_m_std_p_fs_p_FileType
}

type Ordering int32

type _goml_m_std_p_io_p_ErrorKind int32

type _goml_m_std_p_fs_p_FileType int32

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

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error interface {
    is_goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error()
}

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error_Ok struct {
    _0 _goml_m_std_p_bytes_p_Bytes
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error_Ok) is_goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error() {}

type _goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error_Err) is_goml_m_Result____std_p_bytes_p_Bytes____std_p_fs_p_Error() {}

type _goml_m_Result____string____std_p_fs_p_Error interface {
    is_goml_m_Result____string____std_p_fs_p_Error()
}

type _goml_m_Result____string____std_p_fs_p_Error_Ok struct {
    _0 string
}

func (_ _goml_m_Result____string____std_p_fs_p_Error_Ok) is_goml_m_Result____string____std_p_fs_p_Error() {}

type _goml_m_Result____string____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____string____std_p_fs_p_Error_Err) is_goml_m_Result____string____std_p_fs_p_Error() {}

type _goml_m_Result____unit____std_p_fs_p_Error interface {
    is_goml_m_Result____unit____std_p_fs_p_Error()
}

type _goml_m_Result____unit____std_p_fs_p_Error_Ok struct {
    _0 struct{}
}

func (_ _goml_m_Result____unit____std_p_fs_p_Error_Ok) is_goml_m_Result____unit____std_p_fs_p_Error() {}

type _goml_m_Result____unit____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____unit____std_p_fs_p_Error_Err) is_goml_m_Result____unit____std_p_fs_p_Error() {}

type _goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error interface {
    is_goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error()
}

type _goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error_Ok struct {
    _0 _goml_m_std_p_fs_p_Metadata
}

func (_ _goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error_Ok) is_goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error() {}

type _goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error_Err) is_goml_m_Result____std_p_fs_p_Metadata____std_p_fs_p_Error() {}

type _goml_m_Result____bool____std_p_fs_p_Error interface {
    is_goml_m_Result____bool____std_p_fs_p_Error()
}

type _goml_m_Result____bool____std_p_fs_p_Error_Ok struct {
    _0 bool
}

func (_ _goml_m_Result____bool____std_p_fs_p_Error_Ok) is_goml_m_Result____bool____std_p_fs_p_Error() {}

type _goml_m_Result____bool____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____bool____std_p_fs_p_Error_Err) is_goml_m_Result____bool____std_p_fs_p_Error() {}

type _goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error interface {
    is_goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error()
}

type _goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error_Ok struct {
    _0 *_goml_vec__goml_m_std_p_fs_p_DirEntry
}

func (_ _goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error_Ok) is_goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error() {}

type _goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error_Err struct {
    _0 _goml_m_std_p_fs_p_Error
}

func (_ _goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error_Err) is_goml_m_Result____Vec_l_std_p_fs_p_DirEntry_r_____std_p_fs_p_Error() {}

type _goml_m_Result____Vec_l_string_r_____string struct {
    _tag int32
    _v0_0 *_goml_vec_string
    _v1_0 string
}

type Option__char struct {
    _tag int32
    _v1_0 rune
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t819 *_goml_vec_uint8
    var inline1847 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t819 = inline1847
    var t820 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t819,
    }
    return t820
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t864 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t864)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t867 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: x8,
        }
        return t867
    } else {
        var t868 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "invalid UTF-8",
        }
        return t868
    }
}

func _goml_m_std_p_internal_p_host_p_println(value__28 string) struct{} {
    _goml_runtime_std_io_println(value__28)
    return struct{}{}
}

func _goml_m_std_p_internal_p_host_p_eprint(value__29 string) struct{} {
    _goml_runtime_std_io_eprint(value__29)
    return struct{}{}
}

func _goml_m_std_p_fs_p_read__bytes(path__118 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var mtmp66 Tuple3_4bool_10Vec_5uint8_6string
    var inline2202 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__118)
    mtmp66 = inline2202
    var x67 bool = mtmp66._0
    var x68 *_goml_vec_uint8 = mtmp66._1
    var x69 string = mtmp66._2
    if x67 {
        var t1503 _goml_m_std_p_bytes_p_Bytes
        var inline2200 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x68,
        }
        t1503 = inline2200
        var t1504 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string{
            _tag: 0,
            _v0_0: t1503,
        }
        return t1504
    } else {
        var t1505 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string{
            _tag: 1,
            _v1_0: x69,
        }
        return t1505
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t1508 *_goml_vec_uint8
    var inline2206 *_goml_vec_uint8 = data__123.values
    t1508 = inline2206
    var mtmp70 Tuple2_4bool_6string
    var inline2204 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t1508)
    mtmp70 = inline2204
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t1511 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1511
    } else {
        var t1512 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x72,
        }
        return t1512
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__126 string) Result__unit__string {
    var mtmp73 Tuple2_4bool_6string
    var inline2208 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__126)
    mtmp73 = inline2208
    var x74 bool = mtmp73._0
    var x75 string = mtmp73._1
    if x74 {
        var t1517 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1517
    } else {
        var t1518 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x75,
        }
        return t1518
    }
}

func main0() struct{} {
    var t1569 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t1569)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline2263 string = ""
    var inline2264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2263)
    var inline2265 string = inline2264 + "\n"
    _goml_m_std_p_internal_p_host_p_eprint(inline2265)
    var t1570 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t1571 string
    switch t1570._tag {
    case 0:
        t1571 = "ok"
    case 1:
        var inline2259 string = t1570._v1_0
        var inline2261 string = "err " + inline2259
        t1571 = inline2261
    default:
        panic("non-exhaustive match")
    }
    var inline2256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1571)
    _goml_m_std_p_internal_p_host_p_println(inline2256)
    var t1572 Result__unit__string
    var inline2251 string = "goml-self-host/nested/output.txt"
    var inline2252 string = "boot"
    var inline2253 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline2252)
    var inline2254 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline2251, inline2253)
    t1572 = inline2254
    var t1573 string
    switch t1572._tag {
    case 0:
        t1573 = "ok"
    case 1:
        var inline2247 string = t1572._v1_0
        var inline2249 string = "err " + inline2247
        t1573 = inline2249
    default:
        panic("non-exhaustive match")
    }
    var inline2244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1573)
    _goml_m_std_p_internal_p_host_p_println(inline2244)
    var t1574 Result__string__string
    var inline2235 string = "goml-self-host/nested/output.txt"
    var inline2236 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline2235)
    switch inline2236._tag {
    case 0:
        var inline2237 _goml_m_std_p_bytes_p_Bytes = inline2236._v0_0
        var inline2239 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline2237)
        t1574 = inline2239
    case 1:
        var inline2240 string = inline2236._v1_0
        var inline2242 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline2240,
        }
        t1574 = inline2242
    default:
        panic("non-exhaustive match")
    }
    var t1575 string
    switch t1574._tag {
    case 0:
        var inline2229 string = t1574._v0_0
        t1575 = inline2229
    case 1:
        var inline2231 string = t1574._v1_0
        var inline2233 string = "err " + inline2231
        t1575 = inline2233
    default:
        panic("non-exhaustive match")
    }
    var inline2226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1575)
    _goml_m_std_p_internal_p_host_p_println(inline2226)
    return struct{}{}
}

func string_from_utf8(bytes__277 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp395 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__277)
    var x397 string = mtmp395._1
    var index__279 int = 0
    Loop_loop1616:
    for {
        var t1617 int
        var inline2275 int = _goml_runtime_core_string_len(x397)
        t1617 = inline2275
        var t1618 bool = index__279 < t1617
        if t1618 {
            var mtmp398 Tuple3_4bool_4char_3int = string_decode_utf8_at(x397, index__279)
            var x399 bool = mtmp398._0
            var x401 int = mtmp398._2
            if x399 {
                var compound_old402 int = index__279
                var t1620 int = compound_old402 + x401
                index__279 = t1620
                continue
            } else {
                var t1622 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1622
            }
        } else {
            break Loop_loop1616
        }
    }
    var t1615 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x397,
    }
    return t1615
}

func _goml_m_std_p_io_p_println____T__string(value__68 string) struct{} {
    var t1644 string
    t1644 = value__68
    _goml_runtime_std_io_println(t1644)
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1654 bool = string_is_char_boundary(value__274, start__275)
    var jp1651 bool
    if t1654 {
        var t1655 bool = string_is_char_boundary(value__274, end__276)
        jp1651 = t1655
    } else {
        jp1651 = false
    }
    if jp1651 {
        var t1652 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1652
    } else {
        var t1653 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1653
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__69 string) struct{} {
    var t1657 string
    t1657 = value__69
    _goml_runtime_std_io_eprint(t1657)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1665 int = _goml_runtime_core_string_len(self__289)
    return t1665
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t1784 bool = index__259 < 0
    var jp1782 bool
    if t1784 {
        jp1782 = true
    } else {
        var t1785 bool = index__259 >= length__260
        jp1782 = t1785
    }
    if jp1782 {
        var inline2290 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2290
    } else {
        var t1669 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t1669))
        var t1672 bool = first__261 < 128
        if t1672 {
            var inline2292 int = 1
            var inline2293 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline2293._tag {
            case 0:
                var inline2294 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2294
            case 1:
                var inline2295 rune = inline2293._v1_0
                var inline2297 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2295,
                    _2: inline2292,
                }
                return inline2297
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1676 bool = first__261 < 194
            if t1676 {
                var inline2299 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2299
            } else {
                var t1680 bool = first__261 < 224
                if t1680 {
                    var t1693 int = length__260 - index__259
                    var t1694 bool = t1693 < 2
                    if t1694 {
                        var inline2301 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2301
                    } else {
                        var t1682 int = index__259 + 1
                        var t1683 uint8
                        var inline2315 uint8 = _goml_runtime_core_string_byte_get(value__258, t1682)
                        t1683 = inline2315
                        var second__262 uint32 = uint32(uint8(t1683))
                        var t1686 bool
                        var inline2312 bool = second__262 < 128
                        if inline2312 {
                            t1686 = true
                        } else {
                            var inline2313 bool = second__262 > 191
                            t1686 = inline2313
                        }
                        if t1686 {
                            var inline2303 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2303
                        } else {
                            var t1688_rhs uint32 = 31
                            var t1688 uint32 = first__261 & t1688_rhs
                            var t1689_rhs int = 6
                            var t1689 uint32 = t1688 << t1689_rhs
                            var t1690_rhs uint32 = 63
                            var t1690 uint32 = second__262 & t1690_rhs
                            var t1691 uint32 = t1689 | t1690
                            var inline2305 int = 2
                            var inline2306 Option__char = __goml_builtin_char_from_uint32(t1691)
                            switch inline2306._tag {
                            case 0:
                                var inline2307 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2307
                            case 1:
                                var inline2308 rune = inline2306._v1_0
                                var inline2310 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2308,
                                    _2: inline2305,
                                }
                                return inline2310
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1698 bool = first__261 < 240
                    if t1698 {
                        var t1731 int = length__260 - index__259
                        var t1732 bool = t1731 < 3
                        if t1732 {
                            var inline2317 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2317
                        } else {
                            var t1700 int = index__259 + 1
                            var t1701 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1700)
                            var second__263 uint32 = uint32(uint8(t1701))
                            var t1702 int = index__259 + 2
                            var t1703 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1702)
                            var third__264 uint32 = uint32(uint8(t1703))
                            var t1729 bool = utf8_invalid_continuation(second__263)
                            var jp1724 bool
                            if t1729 {
                                jp1724 = true
                            } else {
                                var inline2319 bool = third__264 < 128
                                if inline2319 {
                                    jp1724 = true
                                } else {
                                    var inline2320 bool = third__264 > 191
                                    jp1724 = inline2320
                                }
                            }
                            var jp1718 bool
                            if jp1724 {
                                jp1718 = true
                            } else {
                                var t1727 bool = first__261 == 224
                                if t1727 {
                                    var t1728 bool = second__263 < 160
                                    jp1718 = t1728
                                } else {
                                    jp1718 = false
                                }
                            }
                            var jp1707 bool
                            if jp1718 {
                                jp1707 = true
                            } else {
                                var t1721 bool = first__261 == 237
                                if t1721 {
                                    var t1722 bool = second__263 >= 160
                                    jp1707 = t1722
                                } else {
                                    jp1707 = false
                                }
                            }
                            if jp1707 {
                                var inline2322 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2322
                            } else {
                                var t1709_rhs uint32 = 15
                                var t1709 uint32 = first__261 & t1709_rhs
                                var t1710_rhs int = 12
                                var t1710 uint32 = t1709 << t1710_rhs
                                var t1711_rhs uint32 = 63
                                var t1711 uint32 = second__263 & t1711_rhs
                                var t1712_rhs int = 6
                                var t1712 uint32 = t1711 << t1712_rhs
                                var t1713 uint32 = t1710 | t1712
                                var t1714_rhs uint32 = 63
                                var t1714 uint32 = third__264 & t1714_rhs
                                var t1715 uint32 = t1713 | t1714
                                var inline2324 int = 3
                                var inline2325 Option__char = __goml_builtin_char_from_uint32(t1715)
                                switch inline2325._tag {
                                case 0:
                                    var inline2326 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline2326
                                case 1:
                                    var inline2327 rune = inline2325._v1_0
                                    var inline2329 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline2327,
                                        _2: inline2324,
                                    }
                                    return inline2329
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1736 bool = first__261 < 245
                        if t1736 {
                            var t1777 int = length__260 - index__259
                            var t1778 bool = t1777 < 4
                            if t1778 {
                                var t1779 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1779
                            } else {
                                var t1738 int = index__259 + 1
                                var t1739 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1738)
                                var second__265 uint32 = uint32(uint8(t1739))
                                var t1740 int = index__259 + 2
                                var t1741 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1740)
                                var third__266 uint32 = uint32(uint8(t1741))
                                var t1742 int = index__259 + 3
                                var t1743 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1742)
                                var fourth__267 uint32 = uint32(uint8(t1743))
                                var t1775 bool = utf8_invalid_continuation(second__265)
                                var jp1773 bool
                                if t1775 {
                                    jp1773 = true
                                } else {
                                    var t1776 bool = utf8_invalid_continuation(third__266)
                                    jp1773 = t1776
                                }
                                var jp1767 bool
                                if jp1773 {
                                    jp1767 = true
                                } else {
                                    var t1774 bool = utf8_invalid_continuation(fourth__267)
                                    jp1767 = t1774
                                }
                                var jp1761 bool
                                if jp1767 {
                                    jp1761 = true
                                } else {
                                    var t1770 bool = first__261 == 240
                                    if t1770 {
                                        var t1771 bool = second__265 < 144
                                        jp1761 = t1771
                                    } else {
                                        jp1761 = false
                                    }
                                }
                                var jp1747 bool
                                if jp1761 {
                                    jp1747 = true
                                } else {
                                    var t1764 bool = first__261 == 244
                                    if t1764 {
                                        var t1765 bool = second__265 > 143
                                        jp1747 = t1765
                                    } else {
                                        jp1747 = false
                                    }
                                }
                                if jp1747 {
                                    var t1748 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1748
                                } else {
                                    var t1749_rhs uint32 = 7
                                    var t1749 uint32 = first__261 & t1749_rhs
                                    var t1750_rhs int = 18
                                    var t1750 uint32 = t1749 << t1750_rhs
                                    var t1751_rhs uint32 = 63
                                    var t1751 uint32 = second__265 & t1751_rhs
                                    var t1752_rhs int = 12
                                    var t1752 uint32 = t1751 << t1752_rhs
                                    var t1753 uint32 = t1750 | t1752
                                    var t1754_rhs uint32 = 63
                                    var t1754 uint32 = third__266 & t1754_rhs
                                    var t1755_rhs int = 6
                                    var t1755 uint32 = t1754 << t1755_rhs
                                    var t1756 uint32 = t1753 | t1755
                                    var t1757_rhs uint32 = 63
                                    var t1757 uint32 = fourth__267 & t1757_rhs
                                    var t1758 uint32 = t1756 | t1757
                                    var t1759 Tuple3_4bool_4char_3int = utf8_valid_decode(t1758, 4)
                                    return t1759
                                }
                            }
                        } else {
                            var t1780 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1780
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func string_is_char_boundary(value__268 string, index__269 int) bool {
    var t1807 bool = index__269 < 0
    var jp1799 bool
    if t1807 {
        jp1799 = true
    } else {
        var t1808 int
        var inline2331 int = _goml_runtime_core_string_len(value__268)
        t1808 = inline2331
        var t1809 bool = index__269 > t1808
        jp1799 = t1809
    }
    if jp1799 {
        return false
    } else {
        var t1802 int
        var inline2335 int = _goml_runtime_core_string_len(value__268)
        t1802 = inline2335
        var t1803 bool = index__269 == t1802
        if t1803 {
            return true
        } else {
            var t1804 uint8
            var inline2333 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1804 = inline2333
            var t1805_rhs uint8 = 192
            var t1805 uint8 = t1804 & t1805_rhs
            var t1806 bool = t1805 != 128
            return t1806
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1812 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1812
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1815 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1815
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field2371 rune
    var inline2339 bool = utf8_valid_scalar(value__253)
    if inline2339 {
        var inline2340 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline2341 rune = inline2340._1
        commute_field2371 = inline2341
        var t1821 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2371,
            _2: width__254,
        }
        return t1821
    } else {
        var inline2337 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2337
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t1826 bool = value__256 < 128
    if t1826 {
        return true
    } else {
        var t1827 bool = value__256 > 191
        return t1827
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t1832 bool
    var inline2345 bool = value__283 <= 1114111
    if inline2345 {
        var inline2346 bool = value__283 >= 55296
        var inline2348 bool
        if inline2346 {
            var inline2350 bool = value__283 <= 57343
            inline2348 = inline2350
        } else {
            inline2348 = false
        }
        var inline2349 bool = !inline2348
        t1832 = inline2349
    } else {
        t1832 = false
    }
    if t1832 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t1833 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t1833
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t1838 bool = value__257 <= 1114111
    if t1838 {
        var t1842 bool = value__257 >= 55296
        var jp1840 bool
        if t1842 {
            var t1843 bool = value__257 <= 57343
            jp1840 = t1843
        } else {
            jp1840 = false
        }
        var t1841 bool = !jp1840
        return t1841
    } else {
        return false
    }
}

func main() {
    main0()
}
