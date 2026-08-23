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
    var t818 *_goml_vec_uint8
    var inline1853 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t818 = inline1853
    var t819 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t818,
    }
    return t819
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t863 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t863)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t866 Result__string__string = Result__string__string{
            _tag: 0,
            _v0_0: x8,
        }
        return t866
    } else {
        var t867 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: "invalid UTF-8",
        }
        return t867
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
    var inline2210 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__118)
    mtmp66 = inline2210
    var x67 bool = mtmp66._0
    var x68 *_goml_vec_uint8 = mtmp66._1
    var x69 string = mtmp66._2
    if x67 {
        var t1501 _goml_m_std_p_bytes_p_Bytes
        var inline2208 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x68,
        }
        t1501 = inline2208
        var t1502 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string{
            _tag: 0,
            _v0_0: t1501,
        }
        return t1502
    } else {
        var t1503 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string{
            _tag: 1,
            _v1_0: x69,
        }
        return t1503
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t1506 *_goml_vec_uint8
    var inline2214 *_goml_vec_uint8 = data__123.values
    t1506 = inline2214
    var mtmp70 Tuple2_4bool_6string
    var inline2212 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t1506)
    mtmp70 = inline2212
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t1509 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1509
    } else {
        var t1510 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x72,
        }
        return t1510
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__126 string) Result__unit__string {
    var mtmp73 Tuple2_4bool_6string
    var inline2216 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__126)
    mtmp73 = inline2216
    var x74 bool = mtmp73._0
    var x75 string = mtmp73._1
    if x74 {
        var t1515 Result__unit__string = Result__unit__string{
            _tag: 0,
            _v0_0: struct{}{},
        }
        return t1515
    } else {
        var t1516 Result__unit__string = Result__unit__string{
            _tag: 1,
            _v1_0: x75,
        }
        return t1516
    }
}

func main0() struct{} {
    var t1567 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t1567)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline2271 string = ""
    var inline2272 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline2271)
    var inline2273 string = inline2272 + "\n"
    _goml_m_std_p_internal_p_host_p_eprint(inline2273)
    var t1568 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t1569 string
    switch t1568._tag {
    case 0:
        t1569 = "ok"
    case 1:
        var inline2267 string = t1568._v1_0
        var inline2269 string = "err " + inline2267
        t1569 = inline2269
    default:
        panic("non-exhaustive match")
    }
    var inline2264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1569)
    _goml_m_std_p_internal_p_host_p_println(inline2264)
    var t1570 Result__unit__string
    var inline2259 string = "goml-self-host/nested/output.txt"
    var inline2260 string = "boot"
    var inline2261 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline2260)
    var inline2262 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline2259, inline2261)
    t1570 = inline2262
    var t1571 string
    switch t1570._tag {
    case 0:
        t1571 = "ok"
    case 1:
        var inline2255 string = t1570._v1_0
        var inline2257 string = "err " + inline2255
        t1571 = inline2257
    default:
        panic("non-exhaustive match")
    }
    var inline2252 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1571)
    _goml_m_std_p_internal_p_host_p_println(inline2252)
    var t1572 Result__string__string
    var inline2243 string = "goml-self-host/nested/output.txt"
    var inline2244 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline2243)
    switch inline2244._tag {
    case 0:
        var inline2245 _goml_m_std_p_bytes_p_Bytes = inline2244._v0_0
        var inline2247 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline2245)
        t1572 = inline2247
    case 1:
        var inline2248 string = inline2244._v1_0
        var inline2250 Result__string__string = Result__string__string{
            _tag: 1,
            _v1_0: inline2248,
        }
        t1572 = inline2250
    default:
        panic("non-exhaustive match")
    }
    var t1573 string
    switch t1572._tag {
    case 0:
        var inline2237 string = t1572._v0_0
        t1573 = inline2237
    case 1:
        var inline2239 string = t1572._v1_0
        var inline2241 string = "err " + inline2239
        t1573 = inline2241
    default:
        panic("non-exhaustive match")
    }
    var inline2234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1573)
    _goml_m_std_p_internal_p_host_p_println(inline2234)
    return struct{}{}
}

func string_from_utf8(bytes__277 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp395 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__277)
    var x397 string = mtmp395._1
    var index__279 int = 0
    Loop_loop1617:
    for {
        var t1618 int
        var inline2283 int = _goml_runtime_core_string_len(x397)
        t1618 = inline2283
        var t1619 bool = index__279 < t1618
        if t1619 {
            var mtmp398 Tuple3_4bool_4char_3int = string_decode_utf8_at(x397, index__279)
            var x399 bool = mtmp398._0
            var x401 int = mtmp398._2
            if x399 {
                var compound_old402 int = index__279
                var t1621 int = compound_old402 + x401
                index__279 = t1621
                continue
            } else {
                var t1623 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1623
            }
        } else {
            break Loop_loop1617
        }
    }
    var t1616 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x397,
    }
    return t1616
}

func _goml_m_std_p_io_p_println____T__string(value__68 string) struct{} {
    var t1648 string
    t1648 = value__68
    _goml_runtime_std_io_println(t1648)
    return struct{}{}
}

func string_byte_slice(value__274 string, start__275 int, end__276 int) string {
    var t1658 bool = string_is_char_boundary(value__274, start__275)
    var jp1655 bool
    if t1658 {
        var t1659 bool = string_is_char_boundary(value__274, end__276)
        jp1655 = t1659
    } else {
        jp1655 = false
    }
    if jp1655 {
        var t1656 string = _goml_runtime_core_string_byte_slice(value__274, start__275, end__276)
        return t1656
    } else {
        var t1657 string = _goml_runtime_core_string_byte_slice(value__274, -1, -1)
        return t1657
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__69 string) struct{} {
    var t1661 string
    t1661 = value__69
    _goml_runtime_std_io_eprint(t1661)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__289 string) int {
    var t1669 int = _goml_runtime_core_string_len(self__289)
    return t1669
}

func string_decode_utf8_at(value__258 string, index__259 int) Tuple3_4bool_4char_3int {
    var length__260 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__258)
    var t1788 bool = index__259 < 0
    var jp1786 bool
    if t1788 {
        jp1786 = true
    } else {
        var t1789 bool = index__259 >= length__260
        jp1786 = t1789
    }
    if jp1786 {
        var inline2298 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2298
    } else {
        var t1673 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, index__259)
        var first__261 uint32 = uint32(uint8(t1673))
        var t1676 bool = first__261 < 128
        if t1676 {
            var inline2300 int = 1
            var inline2301 Option__char = __goml_builtin_char_from_uint32(first__261)
            switch inline2301._tag {
            case 0:
                var inline2302 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline2302
            case 1:
                var inline2303 rune = inline2301._v1_0
                var inline2305 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline2303,
                    _2: inline2300,
                }
                return inline2305
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1680 bool = first__261 < 194
            if t1680 {
                var inline2307 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline2307
            } else {
                var t1684 bool = first__261 < 224
                if t1684 {
                    var t1697 int = length__260 - index__259
                    var t1698 bool = t1697 < 2
                    if t1698 {
                        var inline2309 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline2309
                    } else {
                        var t1686 int = index__259 + 1
                        var t1687 uint8
                        var inline2323 uint8 = _goml_runtime_core_string_byte_get(value__258, t1686)
                        t1687 = inline2323
                        var second__262 uint32 = uint32(uint8(t1687))
                        var t1690 bool
                        var inline2320 bool = second__262 < 128
                        if inline2320 {
                            t1690 = true
                        } else {
                            var inline2321 bool = second__262 > 191
                            t1690 = inline2321
                        }
                        if t1690 {
                            var inline2311 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2311
                        } else {
                            var t1692_rhs uint32 = 31
                            var t1692 uint32 = first__261 & t1692_rhs
                            var t1693_rhs int = 6
                            var t1693 uint32 = t1692 << t1693_rhs
                            var t1694_rhs uint32 = 63
                            var t1694 uint32 = second__262 & t1694_rhs
                            var t1695 uint32 = t1693 | t1694
                            var inline2313 int = 2
                            var inline2314 Option__char = __goml_builtin_char_from_uint32(t1695)
                            switch inline2314._tag {
                            case 0:
                                var inline2315 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline2315
                            case 1:
                                var inline2316 rune = inline2314._v1_0
                                var inline2318 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline2316,
                                    _2: inline2313,
                                }
                                return inline2318
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1702 bool = first__261 < 240
                    if t1702 {
                        var t1735 int = length__260 - index__259
                        var t1736 bool = t1735 < 3
                        if t1736 {
                            var inline2325 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline2325
                        } else {
                            var t1704 int = index__259 + 1
                            var t1705 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1704)
                            var second__263 uint32 = uint32(uint8(t1705))
                            var t1706 int = index__259 + 2
                            var t1707 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1706)
                            var third__264 uint32 = uint32(uint8(t1707))
                            var t1733 bool = utf8_invalid_continuation(second__263)
                            var jp1728 bool
                            if t1733 {
                                jp1728 = true
                            } else {
                                var inline2327 bool = third__264 < 128
                                if inline2327 {
                                    jp1728 = true
                                } else {
                                    var inline2328 bool = third__264 > 191
                                    jp1728 = inline2328
                                }
                            }
                            var jp1722 bool
                            if jp1728 {
                                jp1722 = true
                            } else {
                                var t1731 bool = first__261 == 224
                                if t1731 {
                                    var t1732 bool = second__263 < 160
                                    jp1722 = t1732
                                } else {
                                    jp1722 = false
                                }
                            }
                            var jp1711 bool
                            if jp1722 {
                                jp1711 = true
                            } else {
                                var t1725 bool = first__261 == 237
                                if t1725 {
                                    var t1726 bool = second__263 >= 160
                                    jp1711 = t1726
                                } else {
                                    jp1711 = false
                                }
                            }
                            if jp1711 {
                                var inline2330 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline2330
                            } else {
                                var t1713_rhs uint32 = 15
                                var t1713 uint32 = first__261 & t1713_rhs
                                var t1714_rhs int = 12
                                var t1714 uint32 = t1713 << t1714_rhs
                                var t1715_rhs uint32 = 63
                                var t1715 uint32 = second__263 & t1715_rhs
                                var t1716_rhs int = 6
                                var t1716 uint32 = t1715 << t1716_rhs
                                var t1717 uint32 = t1714 | t1716
                                var t1718_rhs uint32 = 63
                                var t1718 uint32 = third__264 & t1718_rhs
                                var t1719 uint32 = t1717 | t1718
                                var inline2332 int = 3
                                var inline2333 Option__char = __goml_builtin_char_from_uint32(t1719)
                                switch inline2333._tag {
                                case 0:
                                    var inline2334 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline2334
                                case 1:
                                    var inline2335 rune = inline2333._v1_0
                                    var inline2337 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline2335,
                                        _2: inline2332,
                                    }
                                    return inline2337
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1740 bool = first__261 < 245
                        if t1740 {
                            var t1781 int = length__260 - index__259
                            var t1782 bool = t1781 < 4
                            if t1782 {
                                var t1783 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1783
                            } else {
                                var t1742 int = index__259 + 1
                                var t1743 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1742)
                                var second__265 uint32 = uint32(uint8(t1743))
                                var t1744 int = index__259 + 2
                                var t1745 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1744)
                                var third__266 uint32 = uint32(uint8(t1745))
                                var t1746 int = index__259 + 3
                                var t1747 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__258, t1746)
                                var fourth__267 uint32 = uint32(uint8(t1747))
                                var t1779 bool = utf8_invalid_continuation(second__265)
                                var jp1777 bool
                                if t1779 {
                                    jp1777 = true
                                } else {
                                    var t1780 bool = utf8_invalid_continuation(third__266)
                                    jp1777 = t1780
                                }
                                var jp1771 bool
                                if jp1777 {
                                    jp1771 = true
                                } else {
                                    var t1778 bool = utf8_invalid_continuation(fourth__267)
                                    jp1771 = t1778
                                }
                                var jp1765 bool
                                if jp1771 {
                                    jp1765 = true
                                } else {
                                    var t1774 bool = first__261 == 240
                                    if t1774 {
                                        var t1775 bool = second__265 < 144
                                        jp1765 = t1775
                                    } else {
                                        jp1765 = false
                                    }
                                }
                                var jp1751 bool
                                if jp1765 {
                                    jp1751 = true
                                } else {
                                    var t1768 bool = first__261 == 244
                                    if t1768 {
                                        var t1769 bool = second__265 > 143
                                        jp1751 = t1769
                                    } else {
                                        jp1751 = false
                                    }
                                }
                                if jp1751 {
                                    var t1752 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1752
                                } else {
                                    var t1753_rhs uint32 = 7
                                    var t1753 uint32 = first__261 & t1753_rhs
                                    var t1754_rhs int = 18
                                    var t1754 uint32 = t1753 << t1754_rhs
                                    var t1755_rhs uint32 = 63
                                    var t1755 uint32 = second__265 & t1755_rhs
                                    var t1756_rhs int = 12
                                    var t1756 uint32 = t1755 << t1756_rhs
                                    var t1757 uint32 = t1754 | t1756
                                    var t1758_rhs uint32 = 63
                                    var t1758 uint32 = third__266 & t1758_rhs
                                    var t1759_rhs int = 6
                                    var t1759 uint32 = t1758 << t1759_rhs
                                    var t1760 uint32 = t1757 | t1759
                                    var t1761_rhs uint32 = 63
                                    var t1761 uint32 = fourth__267 & t1761_rhs
                                    var t1762 uint32 = t1760 | t1761
                                    var t1763 Tuple3_4bool_4char_3int = utf8_valid_decode(t1762, 4)
                                    return t1763
                                }
                            }
                        } else {
                            var t1784 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1784
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
    var t1811 bool = index__269 < 0
    var jp1803 bool
    if t1811 {
        jp1803 = true
    } else {
        var t1812 int
        var inline2339 int = _goml_runtime_core_string_len(value__268)
        t1812 = inline2339
        var t1813 bool = index__269 > t1812
        jp1803 = t1813
    }
    if jp1803 {
        return false
    } else {
        var t1806 int
        var inline2343 int = _goml_runtime_core_string_len(value__268)
        t1806 = inline2343
        var t1807 bool = index__269 == t1806
        if t1807 {
            return true
        } else {
            var t1808 uint8
            var inline2341 uint8 = _goml_runtime_core_string_byte_get(value__268, index__269)
            t1808 = inline2341
            var t1809_rhs uint8 = 192
            var t1809 uint8 = t1808 & t1809_rhs
            var t1810 bool = t1809 != 128
            return t1810
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1816 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1816
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__292 string, index__293 int) uint8 {
    var t1819 uint8 = _goml_runtime_core_string_byte_get(self__292, index__293)
    return t1819
}

func utf8_valid_decode(value__253 uint32, width__254 int) Tuple3_4bool_4char_3int {
    var commute_field2379 rune
    var inline2347 bool = utf8_valid_scalar(value__253)
    if inline2347 {
        var inline2348 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__253)
        var inline2349 rune = inline2348._1
        commute_field2379 = inline2349
        var t1825 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field2379,
            _2: width__254,
        }
        return t1825
    } else {
        var inline2345 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline2345
    }
}

func utf8_invalid_continuation(value__256 uint32) bool {
    var t1830 bool = value__256 < 128
    if t1830 {
        return true
    } else {
        var t1831 bool = value__256 > 191
        return t1831
    }
}

func __goml_builtin_char_from_uint32(value__283 uint32) Option__char {
    var t1836 bool
    var inline2353 bool = value__283 <= 1114111
    if inline2353 {
        var inline2354 bool = value__283 >= 55296
        var inline2356 bool
        if inline2354 {
            var inline2358 bool = value__283 <= 57343
            inline2356 = inline2358
        } else {
            inline2356 = false
        }
        var inline2357 bool = !inline2356
        t1836 = inline2357
    } else {
        t1836 = false
    }
    if t1836 {
        var mtmp407 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__283)
        var x409 rune = mtmp407._1
        var t1837 Option__char = Option__char{
            _tag: 1,
            _v1_0: x409,
        }
        return t1837
    } else {
        return Option__char{
            _tag: 0,
        }
    }
}

func utf8_valid_scalar(value__257 uint32) bool {
    var t1842 bool = value__257 <= 1114111
    if t1842 {
        var t1846 bool = value__257 >= 55296
        var jp1844 bool
        if t1846 {
            var t1847 bool = value__257 <= 57343
            jp1844 = t1847
        } else {
            jp1844 = false
        }
        var t1845 bool = !jp1844
        return t1845
    } else {
        return false
    }
}

func main() {
    main0()
}
