package main

import (
    _goml_context "context"
    _goml_fmt "fmt"
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
    _goml_fmt.Println(value)
    return struct{}{}
}

func _goml_runtime_std_io_eprint(value string) struct{} {
    _goml_fmt.Fprint(_goml_os.Stderr, value)
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

type Tuple3_4bool_4char_3int struct {
    _0 bool
    _1 rune
    _2 int
}

type Tuple2_4bool_4char struct {
    _0 bool
    _1 rune
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

type _goml_m_Result____Vec_l_string_r_____string interface {
    is_goml_m_Result____Vec_l_string_r_____string()
}

type _goml_m_Result____Vec_l_string_r_____string_Ok struct {
    _0 *_goml_vec_string
}

func (_ _goml_m_Result____Vec_l_string_r_____string_Ok) is_goml_m_Result____Vec_l_string_r_____string() {}

type _goml_m_Result____Vec_l_string_r_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Vec_l_string_r_____string_Err) is_goml_m_Result____Vec_l_string_r_____string() {}

type Option__char interface {
    isOption__char()
}

type Option__char_None struct {}

func (_ Option__char_None) isOption__char() {}

type Option__char_Some struct {
    _0 rune
}

func (_ Option__char_Some) isOption__char() {}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t429 *_goml_vec_uint8
    var inline1470 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t429 = inline1470
    var t430 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t429,
    }
    return t430
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t474 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t474)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t477 Result__string__string = Result__string__string_Ok{
            _0: x8,
        }
        return t477
    } else {
        var t478 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        return t478
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
    var inline1827 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__118)
    mtmp66 = inline1827
    var x67 bool = mtmp66._0
    var x68 *_goml_vec_uint8 = mtmp66._1
    var x69 string = mtmp66._2
    if x67 {
        var t1118 _goml_m_std_p_bytes_p_Bytes
        var inline1825 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x68,
        }
        t1118 = inline1825
        var t1119 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t1118,
        }
        return t1119
    } else {
        var t1120 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: x69,
        }
        return t1120
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t1123 *_goml_vec_uint8
    var inline1831 *_goml_vec_uint8 = data__123.values
    t1123 = inline1831
    var mtmp70 Tuple2_4bool_6string
    var inline1829 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t1123)
    mtmp70 = inline1829
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t1126 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t1126
    } else {
        var t1127 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t1127
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__126 string) Result__unit__string {
    var mtmp73 Tuple2_4bool_6string
    var inline1833 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__126)
    mtmp73 = inline1833
    var x74 bool = mtmp73._0
    var x75 string = mtmp73._1
    if x74 {
        var t1132 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t1132
    } else {
        var t1133 Result__unit__string = Result__unit__string_Err{
            _0: x75,
        }
        return t1133
    }
}

func main0() struct{} {
    var t1184 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t1184)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline1888 string = ""
    var inline1889 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1888)
    var inline1890 string = inline1889 + "\n"
    _goml_m_std_p_internal_p_host_p_eprint(inline1890)
    var t1185 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t1186 string
    switch t1185.(type) {
    case Result__unit__string_Ok:
        t1186 = "ok"
    case Result__unit__string_Err:
        var inline1884 string = t1185.(Result__unit__string_Err)._0
        var inline1886 string = "err " + inline1884
        t1186 = inline1886
    default:
        panic("non-exhaustive match")
    }
    var inline1881 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1186)
    _goml_m_std_p_internal_p_host_p_println(inline1881)
    var t1187 Result__unit__string
    var inline1876 string = "goml-self-host/nested/output.txt"
    var inline1877 string = "boot"
    var inline1878 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1877)
    var inline1879 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline1876, inline1878)
    t1187 = inline1879
    var t1188 string
    switch t1187.(type) {
    case Result__unit__string_Ok:
        t1188 = "ok"
    case Result__unit__string_Err:
        var inline1872 string = t1187.(Result__unit__string_Err)._0
        var inline1874 string = "err " + inline1872
        t1188 = inline1874
    default:
        panic("non-exhaustive match")
    }
    var inline1869 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1188)
    _goml_m_std_p_internal_p_host_p_println(inline1869)
    var t1189 Result__string__string
    var inline1860 string = "goml-self-host/nested/output.txt"
    var inline1861 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline1860)
    switch inline1861.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var inline1862 _goml_m_std_p_bytes_p_Bytes = inline1861.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline1864 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline1862)
        t1189 = inline1864
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var inline1865 string = inline1861.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var inline1867 Result__string__string = Result__string__string_Err{
            _0: inline1865,
        }
        t1189 = inline1867
    default:
        panic("non-exhaustive match")
    }
    var t1190 string
    switch t1189.(type) {
    case Result__string__string_Ok:
        var inline1854 string = t1189.(Result__string__string_Ok)._0
        t1190 = inline1854
    case Result__string__string_Err:
        var inline1856 string = t1189.(Result__string__string_Err)._0
        var inline1858 string = "err " + inline1856
        t1190 = inline1858
    default:
        panic("non-exhaustive match")
    }
    var inline1851 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t1190)
    _goml_m_std_p_internal_p_host_p_println(inline1851)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1234:
    for {
        var t1235 int
        var inline1900 int = _goml_runtime_core_string_len(x12)
        t1235 = inline1900
        var t1236 bool = index__26 < t1235
        if t1236 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1238 int = compound_old17 + x16
                index__26 = t1238
                continue
            } else {
                var t1240 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1240
            }
        } else {
            break Loop_loop1234
        }
    }
    var t1233 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t1233
}

func _goml_m_std_p_io_p_println____T__string(value__68 string) struct{} {
    var t1265 string
    t1265 = value__68
    _goml_runtime_std_io_println(t1265)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t1275 bool = string_is_char_boundary(value__21, start__22)
    var jp1272 bool
    if t1275 {
        var t1276 bool = string_is_char_boundary(value__21, end__23)
        jp1272 = t1276
    } else {
        jp1272 = false
    }
    if jp1272 {
        var t1273 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t1273
    } else {
        var t1274 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t1274
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__69 string) struct{} {
    var t1278 string
    t1278 = value__69
    _goml_runtime_std_io_eprint(t1278)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1286 int = _goml_runtime_core_string_len(self__36)
    return t1286
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1405 bool = index__6 < 0
    var jp1403 bool
    if t1405 {
        jp1403 = true
    } else {
        var t1406 bool = index__6 >= length__7
        jp1403 = t1406
    }
    if jp1403 {
        var inline1915 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1915
    } else {
        var t1290 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1290))
        var t1293 bool = first__8 < 128
        if t1293 {
            var inline1917 int = 1
            var inline1918 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1918.(type) {
            case Option__char_None:
                var inline1919 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1919
            case Option__char_Some:
                var inline1920 rune = inline1918.(Option__char_Some)._0
                var inline1922 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1920,
                    _2: inline1917,
                }
                return inline1922
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1297 bool = first__8 < 194
            if t1297 {
                var inline1924 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1924
            } else {
                var t1301 bool = first__8 < 224
                if t1301 {
                    var t1314 int = length__7 - index__6
                    var t1315 bool = t1314 < 2
                    if t1315 {
                        var inline1926 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1926
                    } else {
                        var t1303 int = index__6 + 1
                        var t1304 uint8
                        var inline1940 uint8 = _goml_runtime_core_string_byte_get(value__5, t1303)
                        t1304 = inline1940
                        var second__9 uint32 = uint32(uint8(t1304))
                        var t1307 bool
                        var inline1937 bool = second__9 < 128
                        if inline1937 {
                            t1307 = true
                        } else {
                            var inline1938 bool = second__9 > 191
                            t1307 = inline1938
                        }
                        if t1307 {
                            var inline1928 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1928
                        } else {
                            var t1309_rhs uint32 = 31
                            var t1309 uint32 = first__8 & t1309_rhs
                            var t1310_rhs int = 6
                            var t1310 uint32 = t1309 << t1310_rhs
                            var t1311_rhs uint32 = 63
                            var t1311 uint32 = second__9 & t1311_rhs
                            var t1312 uint32 = t1310 | t1311
                            var inline1930 int = 2
                            var inline1931 Option__char = __goml_builtin_char_from_uint32(t1312)
                            switch inline1931.(type) {
                            case Option__char_None:
                                var inline1932 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1932
                            case Option__char_Some:
                                var inline1933 rune = inline1931.(Option__char_Some)._0
                                var inline1935 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1933,
                                    _2: inline1930,
                                }
                                return inline1935
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1319 bool = first__8 < 240
                    if t1319 {
                        var t1352 int = length__7 - index__6
                        var t1353 bool = t1352 < 3
                        if t1353 {
                            var inline1942 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1942
                        } else {
                            var t1321 int = index__6 + 1
                            var t1322 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1321)
                            var second__10 uint32 = uint32(uint8(t1322))
                            var t1323 int = index__6 + 2
                            var t1324 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1323)
                            var third__11 uint32 = uint32(uint8(t1324))
                            var t1350 bool = utf8_invalid_continuation(second__10)
                            var jp1345 bool
                            if t1350 {
                                jp1345 = true
                            } else {
                                var inline1944 bool = third__11 < 128
                                if inline1944 {
                                    jp1345 = true
                                } else {
                                    var inline1945 bool = third__11 > 191
                                    jp1345 = inline1945
                                }
                            }
                            var jp1339 bool
                            if jp1345 {
                                jp1339 = true
                            } else {
                                var t1348 bool = first__8 == 224
                                if t1348 {
                                    var t1349 bool = second__10 < 160
                                    jp1339 = t1349
                                } else {
                                    jp1339 = false
                                }
                            }
                            var jp1328 bool
                            if jp1339 {
                                jp1328 = true
                            } else {
                                var t1342 bool = first__8 == 237
                                if t1342 {
                                    var t1343 bool = second__10 >= 160
                                    jp1328 = t1343
                                } else {
                                    jp1328 = false
                                }
                            }
                            if jp1328 {
                                var inline1947 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1947
                            } else {
                                var t1330_rhs uint32 = 15
                                var t1330 uint32 = first__8 & t1330_rhs
                                var t1331_rhs int = 12
                                var t1331 uint32 = t1330 << t1331_rhs
                                var t1332_rhs uint32 = 63
                                var t1332 uint32 = second__10 & t1332_rhs
                                var t1333_rhs int = 6
                                var t1333 uint32 = t1332 << t1333_rhs
                                var t1334 uint32 = t1331 | t1333
                                var t1335_rhs uint32 = 63
                                var t1335 uint32 = third__11 & t1335_rhs
                                var t1336 uint32 = t1334 | t1335
                                var inline1949 int = 3
                                var inline1950 Option__char = __goml_builtin_char_from_uint32(t1336)
                                switch inline1950.(type) {
                                case Option__char_None:
                                    var inline1951 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1951
                                case Option__char_Some:
                                    var inline1952 rune = inline1950.(Option__char_Some)._0
                                    var inline1954 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1952,
                                        _2: inline1949,
                                    }
                                    return inline1954
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1357 bool = first__8 < 245
                        if t1357 {
                            var t1398 int = length__7 - index__6
                            var t1399 bool = t1398 < 4
                            if t1399 {
                                var t1400 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1400
                            } else {
                                var t1359 int = index__6 + 1
                                var t1360 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1359)
                                var second__12 uint32 = uint32(uint8(t1360))
                                var t1361 int = index__6 + 2
                                var t1362 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1361)
                                var third__13 uint32 = uint32(uint8(t1362))
                                var t1363 int = index__6 + 3
                                var t1364 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1363)
                                var fourth__14 uint32 = uint32(uint8(t1364))
                                var t1396 bool = utf8_invalid_continuation(second__12)
                                var jp1394 bool
                                if t1396 {
                                    jp1394 = true
                                } else {
                                    var t1397 bool = utf8_invalid_continuation(third__13)
                                    jp1394 = t1397
                                }
                                var jp1388 bool
                                if jp1394 {
                                    jp1388 = true
                                } else {
                                    var t1395 bool = utf8_invalid_continuation(fourth__14)
                                    jp1388 = t1395
                                }
                                var jp1382 bool
                                if jp1388 {
                                    jp1382 = true
                                } else {
                                    var t1391 bool = first__8 == 240
                                    if t1391 {
                                        var t1392 bool = second__12 < 144
                                        jp1382 = t1392
                                    } else {
                                        jp1382 = false
                                    }
                                }
                                var jp1368 bool
                                if jp1382 {
                                    jp1368 = true
                                } else {
                                    var t1385 bool = first__8 == 244
                                    if t1385 {
                                        var t1386 bool = second__12 > 143
                                        jp1368 = t1386
                                    } else {
                                        jp1368 = false
                                    }
                                }
                                if jp1368 {
                                    var t1369 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1369
                                } else {
                                    var t1370_rhs uint32 = 7
                                    var t1370 uint32 = first__8 & t1370_rhs
                                    var t1371_rhs int = 18
                                    var t1371 uint32 = t1370 << t1371_rhs
                                    var t1372_rhs uint32 = 63
                                    var t1372 uint32 = second__12 & t1372_rhs
                                    var t1373_rhs int = 12
                                    var t1373 uint32 = t1372 << t1373_rhs
                                    var t1374 uint32 = t1371 | t1373
                                    var t1375_rhs uint32 = 63
                                    var t1375 uint32 = third__13 & t1375_rhs
                                    var t1376_rhs int = 6
                                    var t1376 uint32 = t1375 << t1376_rhs
                                    var t1377 uint32 = t1374 | t1376
                                    var t1378_rhs uint32 = 63
                                    var t1378 uint32 = fourth__14 & t1378_rhs
                                    var t1379 uint32 = t1377 | t1378
                                    var t1380 Tuple3_4bool_4char_3int = utf8_valid_decode(t1379, 4)
                                    return t1380
                                }
                            }
                        } else {
                            var t1401 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1401
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t1428 bool = index__16 < 0
    var jp1420 bool
    if t1428 {
        jp1420 = true
    } else {
        var t1429 int
        var inline1956 int = _goml_runtime_core_string_len(value__15)
        t1429 = inline1956
        var t1430 bool = index__16 > t1429
        jp1420 = t1430
    }
    if jp1420 {
        return false
    } else {
        var t1423 int
        var inline1960 int = _goml_runtime_core_string_len(value__15)
        t1423 = inline1960
        var t1424 bool = index__16 == t1423
        if t1424 {
            return true
        } else {
            var t1425 uint8
            var inline1958 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1425 = inline1958
            var t1426_rhs uint8 = 192
            var t1426 uint8 = t1425 & t1426_rhs
            var t1427 bool = t1426 != 128
            return t1427
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1433 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1433
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1436 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1436
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1996 rune
    var inline1964 bool = utf8_valid_scalar(value__0)
    if inline1964 {
        var inline1965 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1966 rune = inline1965._1
        commute_field1996 = inline1966
        var t1442 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1996,
            _2: width__1,
        }
        return t1442
    } else {
        var inline1962 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1962
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1447 bool = value__3 < 128
    if t1447 {
        return true
    } else {
        var t1448 bool = value__3 > 191
        return t1448
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1453 bool
    var inline1970 bool = value__30 <= 1114111
    if inline1970 {
        var inline1971 bool = value__30 >= 55296
        var inline1973 bool
        if inline1971 {
            var inline1975 bool = value__30 <= 57343
            inline1973 = inline1975
        } else {
            inline1973 = false
        }
        var inline1974 bool = !inline1973
        t1453 = inline1974
    } else {
        t1453 = false
    }
    if t1453 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1454 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1454
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1459 bool = value__4 <= 1114111
    if t1459 {
        var t1463 bool = value__4 >= 55296
        var jp1461 bool
        if t1463 {
            var t1464 bool = value__4 <= 57343
            jp1461 = t1464
        } else {
            jp1461 = false
        }
        var t1462 bool = !jp1461
        return t1462
    } else {
        return false
    }
}

func main() {
    main0()
}
