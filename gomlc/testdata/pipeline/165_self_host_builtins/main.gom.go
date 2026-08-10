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
    var t193 *_goml_vec_uint8
    var inline1234 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t193 = inline1234
    var t194 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t193,
    }
    return t194
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t238 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t238)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t241 Result__string__string = Result__string__string_Ok{
            _0: x8,
        }
        return t241
    } else {
        var t242 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        return t242
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
    var inline1591 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__118)
    mtmp66 = inline1591
    var x67 bool = mtmp66._0
    var x68 *_goml_vec_uint8 = mtmp66._1
    var x69 string = mtmp66._2
    if x67 {
        var t882 _goml_m_std_p_bytes_p_Bytes
        var inline1589 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x68,
        }
        t882 = inline1589
        var t883 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t882,
        }
        return t883
    } else {
        var t884 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: x69,
        }
        return t884
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t887 *_goml_vec_uint8
    var inline1595 *_goml_vec_uint8 = data__123.values
    t887 = inline1595
    var mtmp70 Tuple2_4bool_6string
    var inline1593 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t887)
    mtmp70 = inline1593
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t890 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t890
    } else {
        var t891 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t891
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__126 string) Result__unit__string {
    var mtmp73 Tuple2_4bool_6string
    var inline1597 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__126)
    mtmp73 = inline1597
    var x74 bool = mtmp73._0
    var x75 string = mtmp73._1
    if x74 {
        var t896 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t896
    } else {
        var t897 Result__unit__string = Result__unit__string_Err{
            _0: x75,
        }
        return t897
    }
}

func main0() struct{} {
    var t948 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t948)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline1652 string = ""
    var inline1653 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1652)
    var inline1654 string = inline1653 + "\n"
    _goml_m_std_p_internal_p_host_p_eprint(inline1654)
    var t949 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t950 string
    switch t949.(type) {
    case Result__unit__string_Ok:
        t950 = "ok"
    case Result__unit__string_Err:
        var inline1648 string = t949.(Result__unit__string_Err)._0
        var inline1650 string = "err " + inline1648
        t950 = inline1650
    default:
        panic("non-exhaustive match")
    }
    var inline1645 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t950)
    _goml_m_std_p_internal_p_host_p_println(inline1645)
    var t951 Result__unit__string
    var inline1640 string = "goml-self-host/nested/output.txt"
    var inline1641 string = "boot"
    var inline1642 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1641)
    var inline1643 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline1640, inline1642)
    t951 = inline1643
    var t952 string
    switch t951.(type) {
    case Result__unit__string_Ok:
        t952 = "ok"
    case Result__unit__string_Err:
        var inline1636 string = t951.(Result__unit__string_Err)._0
        var inline1638 string = "err " + inline1636
        t952 = inline1638
    default:
        panic("non-exhaustive match")
    }
    var inline1633 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t952)
    _goml_m_std_p_internal_p_host_p_println(inline1633)
    var t953 Result__string__string
    var inline1624 string = "goml-self-host/nested/output.txt"
    var inline1625 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline1624)
    switch inline1625.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var inline1626 _goml_m_std_p_bytes_p_Bytes = inline1625.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline1628 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline1626)
        t953 = inline1628
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var inline1629 string = inline1625.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var inline1631 Result__string__string = Result__string__string_Err{
            _0: inline1629,
        }
        t953 = inline1631
    default:
        panic("non-exhaustive match")
    }
    var t954 string
    switch t953.(type) {
    case Result__string__string_Ok:
        var inline1618 string = t953.(Result__string__string_Ok)._0
        t954 = inline1618
    case Result__string__string_Err:
        var inline1620 string = t953.(Result__string__string_Err)._0
        var inline1622 string = "err " + inline1620
        t954 = inline1622
    default:
        panic("non-exhaustive match")
    }
    var inline1615 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t954)
    _goml_m_std_p_internal_p_host_p_println(inline1615)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop998:
    for {
        var t999 int
        var inline1664 int = _goml_runtime_core_string_len(x12)
        t999 = inline1664
        var t1000 bool = index__26 < t999
        if t1000 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1002 int = compound_old17 + x16
                index__26 = t1002
                continue
            } else {
                var t1004 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1004
            }
        } else {
            break Loop_loop998
        }
    }
    var t997 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t997
}

func _goml_m_std_p_io_p_println____T__string(value__68 string) struct{} {
    var t1029 string
    t1029 = value__68
    _goml_runtime_std_io_println(t1029)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t1039 bool = string_is_char_boundary(value__21, start__22)
    var jp1036 bool
    if t1039 {
        var t1040 bool = string_is_char_boundary(value__21, end__23)
        jp1036 = t1040
    } else {
        jp1036 = false
    }
    if jp1036 {
        var t1037 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t1037
    } else {
        var t1038 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t1038
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__69 string) struct{} {
    var t1042 string
    t1042 = value__69
    _goml_runtime_std_io_eprint(t1042)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1050 int = _goml_runtime_core_string_len(self__36)
    return t1050
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1169 bool = index__6 < 0
    var jp1167 bool
    if t1169 {
        jp1167 = true
    } else {
        var t1170 bool = index__6 >= length__7
        jp1167 = t1170
    }
    if jp1167 {
        var inline1679 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1679
    } else {
        var t1054 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1054))
        var t1057 bool = first__8 < 128
        if t1057 {
            var inline1681 int = 1
            var inline1682 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1682.(type) {
            case Option__char_None:
                var inline1683 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1683
            case Option__char_Some:
                var inline1684 rune = inline1682.(Option__char_Some)._0
                var inline1686 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1684,
                    _2: inline1681,
                }
                return inline1686
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1061 bool = first__8 < 194
            if t1061 {
                var inline1688 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1688
            } else {
                var t1065 bool = first__8 < 224
                if t1065 {
                    var t1078 int = length__7 - index__6
                    var t1079 bool = t1078 < 2
                    if t1079 {
                        var inline1690 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1690
                    } else {
                        var t1067 int = index__6 + 1
                        var t1068 uint8
                        var inline1704 uint8 = _goml_runtime_core_string_byte_get(value__5, t1067)
                        t1068 = inline1704
                        var second__9 uint32 = uint32(uint8(t1068))
                        var t1071 bool
                        var inline1701 bool = second__9 < 128
                        if inline1701 {
                            t1071 = true
                        } else {
                            var inline1702 bool = second__9 > 191
                            t1071 = inline1702
                        }
                        if t1071 {
                            var inline1692 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1692
                        } else {
                            var t1073_rhs uint32 = 31
                            var t1073 uint32 = first__8 & t1073_rhs
                            var t1074_rhs int = 6
                            var t1074 uint32 = t1073 << t1074_rhs
                            var t1075_rhs uint32 = 63
                            var t1075 uint32 = second__9 & t1075_rhs
                            var t1076 uint32 = t1074 | t1075
                            var inline1694 int = 2
                            var inline1695 Option__char = __goml_builtin_char_from_uint32(t1076)
                            switch inline1695.(type) {
                            case Option__char_None:
                                var inline1696 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1696
                            case Option__char_Some:
                                var inline1697 rune = inline1695.(Option__char_Some)._0
                                var inline1699 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1697,
                                    _2: inline1694,
                                }
                                return inline1699
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1083 bool = first__8 < 240
                    if t1083 {
                        var t1116 int = length__7 - index__6
                        var t1117 bool = t1116 < 3
                        if t1117 {
                            var inline1706 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1706
                        } else {
                            var t1085 int = index__6 + 1
                            var t1086 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1085)
                            var second__10 uint32 = uint32(uint8(t1086))
                            var t1087 int = index__6 + 2
                            var t1088 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1087)
                            var third__11 uint32 = uint32(uint8(t1088))
                            var t1114 bool = utf8_invalid_continuation(second__10)
                            var jp1109 bool
                            if t1114 {
                                jp1109 = true
                            } else {
                                var inline1708 bool = third__11 < 128
                                if inline1708 {
                                    jp1109 = true
                                } else {
                                    var inline1709 bool = third__11 > 191
                                    jp1109 = inline1709
                                }
                            }
                            var jp1103 bool
                            if jp1109 {
                                jp1103 = true
                            } else {
                                var t1112 bool = first__8 == 224
                                if t1112 {
                                    var t1113 bool = second__10 < 160
                                    jp1103 = t1113
                                } else {
                                    jp1103 = false
                                }
                            }
                            var jp1092 bool
                            if jp1103 {
                                jp1092 = true
                            } else {
                                var t1106 bool = first__8 == 237
                                if t1106 {
                                    var t1107 bool = second__10 >= 160
                                    jp1092 = t1107
                                } else {
                                    jp1092 = false
                                }
                            }
                            if jp1092 {
                                var inline1711 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1711
                            } else {
                                var t1094_rhs uint32 = 15
                                var t1094 uint32 = first__8 & t1094_rhs
                                var t1095_rhs int = 12
                                var t1095 uint32 = t1094 << t1095_rhs
                                var t1096_rhs uint32 = 63
                                var t1096 uint32 = second__10 & t1096_rhs
                                var t1097_rhs int = 6
                                var t1097 uint32 = t1096 << t1097_rhs
                                var t1098 uint32 = t1095 | t1097
                                var t1099_rhs uint32 = 63
                                var t1099 uint32 = third__11 & t1099_rhs
                                var t1100 uint32 = t1098 | t1099
                                var inline1713 int = 3
                                var inline1714 Option__char = __goml_builtin_char_from_uint32(t1100)
                                switch inline1714.(type) {
                                case Option__char_None:
                                    var inline1715 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1715
                                case Option__char_Some:
                                    var inline1716 rune = inline1714.(Option__char_Some)._0
                                    var inline1718 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1716,
                                        _2: inline1713,
                                    }
                                    return inline1718
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1121 bool = first__8 < 245
                        if t1121 {
                            var t1162 int = length__7 - index__6
                            var t1163 bool = t1162 < 4
                            if t1163 {
                                var t1164 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1164
                            } else {
                                var t1123 int = index__6 + 1
                                var t1124 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1123)
                                var second__12 uint32 = uint32(uint8(t1124))
                                var t1125 int = index__6 + 2
                                var t1126 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1125)
                                var third__13 uint32 = uint32(uint8(t1126))
                                var t1127 int = index__6 + 3
                                var t1128 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1127)
                                var fourth__14 uint32 = uint32(uint8(t1128))
                                var t1160 bool = utf8_invalid_continuation(second__12)
                                var jp1158 bool
                                if t1160 {
                                    jp1158 = true
                                } else {
                                    var t1161 bool = utf8_invalid_continuation(third__13)
                                    jp1158 = t1161
                                }
                                var jp1152 bool
                                if jp1158 {
                                    jp1152 = true
                                } else {
                                    var t1159 bool = utf8_invalid_continuation(fourth__14)
                                    jp1152 = t1159
                                }
                                var jp1146 bool
                                if jp1152 {
                                    jp1146 = true
                                } else {
                                    var t1155 bool = first__8 == 240
                                    if t1155 {
                                        var t1156 bool = second__12 < 144
                                        jp1146 = t1156
                                    } else {
                                        jp1146 = false
                                    }
                                }
                                var jp1132 bool
                                if jp1146 {
                                    jp1132 = true
                                } else {
                                    var t1149 bool = first__8 == 244
                                    if t1149 {
                                        var t1150 bool = second__12 > 143
                                        jp1132 = t1150
                                    } else {
                                        jp1132 = false
                                    }
                                }
                                if jp1132 {
                                    var t1133 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1133
                                } else {
                                    var t1134_rhs uint32 = 7
                                    var t1134 uint32 = first__8 & t1134_rhs
                                    var t1135_rhs int = 18
                                    var t1135 uint32 = t1134 << t1135_rhs
                                    var t1136_rhs uint32 = 63
                                    var t1136 uint32 = second__12 & t1136_rhs
                                    var t1137_rhs int = 12
                                    var t1137 uint32 = t1136 << t1137_rhs
                                    var t1138 uint32 = t1135 | t1137
                                    var t1139_rhs uint32 = 63
                                    var t1139 uint32 = third__13 & t1139_rhs
                                    var t1140_rhs int = 6
                                    var t1140 uint32 = t1139 << t1140_rhs
                                    var t1141 uint32 = t1138 | t1140
                                    var t1142_rhs uint32 = 63
                                    var t1142 uint32 = fourth__14 & t1142_rhs
                                    var t1143 uint32 = t1141 | t1142
                                    var t1144 Tuple3_4bool_4char_3int = utf8_valid_decode(t1143, 4)
                                    return t1144
                                }
                            }
                        } else {
                            var t1165 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1165
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func string_is_char_boundary(value__15 string, index__16 int) bool {
    var t1192 bool = index__16 < 0
    var jp1184 bool
    if t1192 {
        jp1184 = true
    } else {
        var t1193 int
        var inline1720 int = _goml_runtime_core_string_len(value__15)
        t1193 = inline1720
        var t1194 bool = index__16 > t1193
        jp1184 = t1194
    }
    if jp1184 {
        return false
    } else {
        var t1187 int
        var inline1724 int = _goml_runtime_core_string_len(value__15)
        t1187 = inline1724
        var t1188 bool = index__16 == t1187
        if t1188 {
            return true
        } else {
            var t1189 uint8
            var inline1722 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1189 = inline1722
            var t1190_rhs uint8 = 192
            var t1190 uint8 = t1189 & t1190_rhs
            var t1191 bool = t1190 != 128
            return t1191
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1197 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1197
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1200 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1200
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1760 rune
    var inline1728 bool = utf8_valid_scalar(value__0)
    if inline1728 {
        var inline1729 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1730 rune = inline1729._1
        commute_field1760 = inline1730
        var t1206 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1760,
            _2: width__1,
        }
        return t1206
    } else {
        var inline1726 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1726
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1211 bool = value__3 < 128
    if t1211 {
        return true
    } else {
        var t1212 bool = value__3 > 191
        return t1212
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1217 bool
    var inline1734 bool = value__30 <= 1114111
    if inline1734 {
        var inline1735 bool = value__30 >= 55296
        var inline1737 bool
        if inline1735 {
            var inline1739 bool = value__30 <= 57343
            inline1737 = inline1739
        } else {
            inline1737 = false
        }
        var inline1738 bool = !inline1737
        t1217 = inline1738
    } else {
        t1217 = false
    }
    if t1217 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1218 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1218
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1223 bool = value__4 <= 1114111
    if t1223 {
        var t1227 bool = value__4 >= 55296
        var jp1225 bool
        if t1227 {
            var t1228 bool = value__4 <= 57343
            jp1225 = t1228
        } else {
            jp1225 = false
        }
        var t1226 bool = !jp1225
        return t1226
    } else {
        return false
    }
}

func main() {
    main0()
}
