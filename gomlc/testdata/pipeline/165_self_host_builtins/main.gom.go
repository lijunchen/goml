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
    var t208 *_goml_vec_uint8
    var inline1249 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t208 = inline1249
    var t209 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t208,
    }
    return t209
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t253 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t253)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t256 Result__string__string = Result__string__string_Ok{
            _0: x8,
        }
        return t256
    } else {
        var t257 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        return t257
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
    var inline1606 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__118)
    mtmp66 = inline1606
    var x67 bool = mtmp66._0
    var x68 *_goml_vec_uint8 = mtmp66._1
    var x69 string = mtmp66._2
    if x67 {
        var t897 _goml_m_std_p_bytes_p_Bytes
        var inline1604 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x68,
        }
        t897 = inline1604
        var t898 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t897,
        }
        return t898
    } else {
        var t899 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: x69,
        }
        return t899
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t902 *_goml_vec_uint8
    var inline1610 *_goml_vec_uint8 = data__123.values
    t902 = inline1610
    var mtmp70 Tuple2_4bool_6string
    var inline1608 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t902)
    mtmp70 = inline1608
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t905 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t905
    } else {
        var t906 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t906
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__126 string) Result__unit__string {
    var mtmp73 Tuple2_4bool_6string
    var inline1612 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__126)
    mtmp73 = inline1612
    var x74 bool = mtmp73._0
    var x75 string = mtmp73._1
    if x74 {
        var t911 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t911
    } else {
        var t912 Result__unit__string = Result__unit__string_Err{
            _0: x75,
        }
        return t912
    }
}

func main0() struct{} {
    var t963 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t963)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline1667 string = ""
    var inline1668 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1667)
    var inline1669 string = inline1668 + "\n"
    _goml_m_std_p_internal_p_host_p_eprint(inline1669)
    var t964 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t965 string
    switch t964.(type) {
    case Result__unit__string_Ok:
        t965 = "ok"
    case Result__unit__string_Err:
        var inline1663 string = t964.(Result__unit__string_Err)._0
        var inline1665 string = "err " + inline1663
        t965 = inline1665
    default:
        panic("non-exhaustive match")
    }
    var inline1660 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t965)
    _goml_m_std_p_internal_p_host_p_println(inline1660)
    var t966 Result__unit__string
    var inline1655 string = "goml-self-host/nested/output.txt"
    var inline1656 string = "boot"
    var inline1657 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1656)
    var inline1658 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline1655, inline1657)
    t966 = inline1658
    var t967 string
    switch t966.(type) {
    case Result__unit__string_Ok:
        t967 = "ok"
    case Result__unit__string_Err:
        var inline1651 string = t966.(Result__unit__string_Err)._0
        var inline1653 string = "err " + inline1651
        t967 = inline1653
    default:
        panic("non-exhaustive match")
    }
    var inline1648 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t967)
    _goml_m_std_p_internal_p_host_p_println(inline1648)
    var t968 Result__string__string
    var inline1639 string = "goml-self-host/nested/output.txt"
    var inline1640 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline1639)
    switch inline1640.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var inline1641 _goml_m_std_p_bytes_p_Bytes = inline1640.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline1643 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline1641)
        t968 = inline1643
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var inline1644 string = inline1640.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var inline1646 Result__string__string = Result__string__string_Err{
            _0: inline1644,
        }
        t968 = inline1646
    default:
        panic("non-exhaustive match")
    }
    var t969 string
    switch t968.(type) {
    case Result__string__string_Ok:
        var inline1633 string = t968.(Result__string__string_Ok)._0
        t969 = inline1633
    case Result__string__string_Err:
        var inline1635 string = t968.(Result__string__string_Err)._0
        var inline1637 string = "err " + inline1635
        t969 = inline1637
    default:
        panic("non-exhaustive match")
    }
    var inline1630 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t969)
    _goml_m_std_p_internal_p_host_p_println(inline1630)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1013:
    for {
        var t1014 int
        var inline1679 int = _goml_runtime_core_string_len(x12)
        t1014 = inline1679
        var t1015 bool = index__26 < t1014
        if t1015 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1017 int = compound_old17 + x16
                index__26 = t1017
                continue
            } else {
                var t1019 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1019
            }
        } else {
            break Loop_loop1013
        }
    }
    var t1012 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t1012
}

func _goml_m_std_p_io_p_println____T__string(value__68 string) struct{} {
    var t1044 string
    t1044 = value__68
    _goml_runtime_std_io_println(t1044)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t1054 bool = string_is_char_boundary(value__21, start__22)
    var jp1051 bool
    if t1054 {
        var t1055 bool = string_is_char_boundary(value__21, end__23)
        jp1051 = t1055
    } else {
        jp1051 = false
    }
    if jp1051 {
        var t1052 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t1052
    } else {
        var t1053 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t1053
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__69 string) struct{} {
    var t1057 string
    t1057 = value__69
    _goml_runtime_std_io_eprint(t1057)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1065 int = _goml_runtime_core_string_len(self__36)
    return t1065
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1184 bool = index__6 < 0
    var jp1182 bool
    if t1184 {
        jp1182 = true
    } else {
        var t1185 bool = index__6 >= length__7
        jp1182 = t1185
    }
    if jp1182 {
        var inline1694 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1694
    } else {
        var t1069 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1069))
        var t1072 bool = first__8 < 128
        if t1072 {
            var inline1696 int = 1
            var inline1697 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1697.(type) {
            case Option__char_None:
                var inline1698 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1698
            case Option__char_Some:
                var inline1699 rune = inline1697.(Option__char_Some)._0
                var inline1701 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1699,
                    _2: inline1696,
                }
                return inline1701
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1076 bool = first__8 < 194
            if t1076 {
                var inline1703 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1703
            } else {
                var t1080 bool = first__8 < 224
                if t1080 {
                    var t1093 int = length__7 - index__6
                    var t1094 bool = t1093 < 2
                    if t1094 {
                        var inline1705 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1705
                    } else {
                        var t1082 int = index__6 + 1
                        var t1083 uint8
                        var inline1719 uint8 = _goml_runtime_core_string_byte_get(value__5, t1082)
                        t1083 = inline1719
                        var second__9 uint32 = uint32(uint8(t1083))
                        var t1086 bool
                        var inline1716 bool = second__9 < 128
                        if inline1716 {
                            t1086 = true
                        } else {
                            var inline1717 bool = second__9 > 191
                            t1086 = inline1717
                        }
                        if t1086 {
                            var inline1707 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1707
                        } else {
                            var t1088_rhs uint32 = 31
                            var t1088 uint32 = first__8 & t1088_rhs
                            var t1089_rhs int = 6
                            var t1089 uint32 = t1088 << t1089_rhs
                            var t1090_rhs uint32 = 63
                            var t1090 uint32 = second__9 & t1090_rhs
                            var t1091 uint32 = t1089 | t1090
                            var inline1709 int = 2
                            var inline1710 Option__char = __goml_builtin_char_from_uint32(t1091)
                            switch inline1710.(type) {
                            case Option__char_None:
                                var inline1711 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1711
                            case Option__char_Some:
                                var inline1712 rune = inline1710.(Option__char_Some)._0
                                var inline1714 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1712,
                                    _2: inline1709,
                                }
                                return inline1714
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1098 bool = first__8 < 240
                    if t1098 {
                        var t1131 int = length__7 - index__6
                        var t1132 bool = t1131 < 3
                        if t1132 {
                            var inline1721 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1721
                        } else {
                            var t1100 int = index__6 + 1
                            var t1101 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1100)
                            var second__10 uint32 = uint32(uint8(t1101))
                            var t1102 int = index__6 + 2
                            var t1103 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1102)
                            var third__11 uint32 = uint32(uint8(t1103))
                            var t1129 bool = utf8_invalid_continuation(second__10)
                            var jp1124 bool
                            if t1129 {
                                jp1124 = true
                            } else {
                                var inline1723 bool = third__11 < 128
                                if inline1723 {
                                    jp1124 = true
                                } else {
                                    var inline1724 bool = third__11 > 191
                                    jp1124 = inline1724
                                }
                            }
                            var jp1118 bool
                            if jp1124 {
                                jp1118 = true
                            } else {
                                var t1127 bool = first__8 == 224
                                if t1127 {
                                    var t1128 bool = second__10 < 160
                                    jp1118 = t1128
                                } else {
                                    jp1118 = false
                                }
                            }
                            var jp1107 bool
                            if jp1118 {
                                jp1107 = true
                            } else {
                                var t1121 bool = first__8 == 237
                                if t1121 {
                                    var t1122 bool = second__10 >= 160
                                    jp1107 = t1122
                                } else {
                                    jp1107 = false
                                }
                            }
                            if jp1107 {
                                var inline1726 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1726
                            } else {
                                var t1109_rhs uint32 = 15
                                var t1109 uint32 = first__8 & t1109_rhs
                                var t1110_rhs int = 12
                                var t1110 uint32 = t1109 << t1110_rhs
                                var t1111_rhs uint32 = 63
                                var t1111 uint32 = second__10 & t1111_rhs
                                var t1112_rhs int = 6
                                var t1112 uint32 = t1111 << t1112_rhs
                                var t1113 uint32 = t1110 | t1112
                                var t1114_rhs uint32 = 63
                                var t1114 uint32 = third__11 & t1114_rhs
                                var t1115 uint32 = t1113 | t1114
                                var inline1728 int = 3
                                var inline1729 Option__char = __goml_builtin_char_from_uint32(t1115)
                                switch inline1729.(type) {
                                case Option__char_None:
                                    var inline1730 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1730
                                case Option__char_Some:
                                    var inline1731 rune = inline1729.(Option__char_Some)._0
                                    var inline1733 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1731,
                                        _2: inline1728,
                                    }
                                    return inline1733
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1136 bool = first__8 < 245
                        if t1136 {
                            var t1177 int = length__7 - index__6
                            var t1178 bool = t1177 < 4
                            if t1178 {
                                var t1179 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1179
                            } else {
                                var t1138 int = index__6 + 1
                                var t1139 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1138)
                                var second__12 uint32 = uint32(uint8(t1139))
                                var t1140 int = index__6 + 2
                                var t1141 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1140)
                                var third__13 uint32 = uint32(uint8(t1141))
                                var t1142 int = index__6 + 3
                                var t1143 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1142)
                                var fourth__14 uint32 = uint32(uint8(t1143))
                                var t1175 bool = utf8_invalid_continuation(second__12)
                                var jp1173 bool
                                if t1175 {
                                    jp1173 = true
                                } else {
                                    var t1176 bool = utf8_invalid_continuation(third__13)
                                    jp1173 = t1176
                                }
                                var jp1167 bool
                                if jp1173 {
                                    jp1167 = true
                                } else {
                                    var t1174 bool = utf8_invalid_continuation(fourth__14)
                                    jp1167 = t1174
                                }
                                var jp1161 bool
                                if jp1167 {
                                    jp1161 = true
                                } else {
                                    var t1170 bool = first__8 == 240
                                    if t1170 {
                                        var t1171 bool = second__12 < 144
                                        jp1161 = t1171
                                    } else {
                                        jp1161 = false
                                    }
                                }
                                var jp1147 bool
                                if jp1161 {
                                    jp1147 = true
                                } else {
                                    var t1164 bool = first__8 == 244
                                    if t1164 {
                                        var t1165 bool = second__12 > 143
                                        jp1147 = t1165
                                    } else {
                                        jp1147 = false
                                    }
                                }
                                if jp1147 {
                                    var t1148 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1148
                                } else {
                                    var t1149_rhs uint32 = 7
                                    var t1149 uint32 = first__8 & t1149_rhs
                                    var t1150_rhs int = 18
                                    var t1150 uint32 = t1149 << t1150_rhs
                                    var t1151_rhs uint32 = 63
                                    var t1151 uint32 = second__12 & t1151_rhs
                                    var t1152_rhs int = 12
                                    var t1152 uint32 = t1151 << t1152_rhs
                                    var t1153 uint32 = t1150 | t1152
                                    var t1154_rhs uint32 = 63
                                    var t1154 uint32 = third__13 & t1154_rhs
                                    var t1155_rhs int = 6
                                    var t1155 uint32 = t1154 << t1155_rhs
                                    var t1156 uint32 = t1153 | t1155
                                    var t1157_rhs uint32 = 63
                                    var t1157 uint32 = fourth__14 & t1157_rhs
                                    var t1158 uint32 = t1156 | t1157
                                    var t1159 Tuple3_4bool_4char_3int = utf8_valid_decode(t1158, 4)
                                    return t1159
                                }
                            }
                        } else {
                            var t1180 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1180
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
    var t1207 bool = index__16 < 0
    var jp1199 bool
    if t1207 {
        jp1199 = true
    } else {
        var t1208 int
        var inline1735 int = _goml_runtime_core_string_len(value__15)
        t1208 = inline1735
        var t1209 bool = index__16 > t1208
        jp1199 = t1209
    }
    if jp1199 {
        return false
    } else {
        var t1202 int
        var inline1739 int = _goml_runtime_core_string_len(value__15)
        t1202 = inline1739
        var t1203 bool = index__16 == t1202
        if t1203 {
            return true
        } else {
            var t1204 uint8
            var inline1737 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1204 = inline1737
            var t1205_rhs uint8 = 192
            var t1205 uint8 = t1204 & t1205_rhs
            var t1206 bool = t1205 != 128
            return t1206
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1212 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1212
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1215 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1215
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1775 rune
    var inline1743 bool = utf8_valid_scalar(value__0)
    if inline1743 {
        var inline1744 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1745 rune = inline1744._1
        commute_field1775 = inline1745
        var t1221 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1775,
            _2: width__1,
        }
        return t1221
    } else {
        var inline1741 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1741
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1226 bool = value__3 < 128
    if t1226 {
        return true
    } else {
        var t1227 bool = value__3 > 191
        return t1227
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1232 bool
    var inline1749 bool = value__30 <= 1114111
    if inline1749 {
        var inline1750 bool = value__30 >= 55296
        var inline1752 bool
        if inline1750 {
            var inline1754 bool = value__30 <= 57343
            inline1752 = inline1754
        } else {
            inline1752 = false
        }
        var inline1753 bool = !inline1752
        t1232 = inline1753
    } else {
        t1232 = false
    }
    if t1232 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1233 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1233
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1238 bool = value__4 <= 1114111
    if t1238 {
        var t1242 bool = value__4 >= 55296
        var jp1240 bool
        if t1242 {
            var t1243 bool = value__4 <= 57343
            jp1240 = t1243
        } else {
            jp1240 = false
        }
        var t1241 bool = !jp1240
        return t1241
    } else {
        return false
    }
}

func main() {
    main0()
}
