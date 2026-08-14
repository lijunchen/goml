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
    var t203 *_goml_vec_uint8
    var inline1244 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t203 = inline1244
    var t204 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t203,
    }
    return t204
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t248 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = string_from_utf8(t248)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t251 Result__string__string = Result__string__string_Ok{
            _0: x8,
        }
        return t251
    } else {
        var t252 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        return t252
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
    var inline1601 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__118)
    mtmp66 = inline1601
    var x67 bool = mtmp66._0
    var x68 *_goml_vec_uint8 = mtmp66._1
    var x69 string = mtmp66._2
    if x67 {
        var t892 _goml_m_std_p_bytes_p_Bytes
        var inline1599 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
            values: x68,
        }
        t892 = inline1599
        var t893 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t892,
        }
        return t893
    } else {
        var t894 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: x69,
        }
        return t894
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t897 *_goml_vec_uint8
    var inline1605 *_goml_vec_uint8 = data__123.values
    t897 = inline1605
    var mtmp70 Tuple2_4bool_6string
    var inline1603 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t897)
    mtmp70 = inline1603
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t900 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t900
    } else {
        var t901 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t901
    }
}

func _goml_m_std_p_fs_p_create__dir__all(path__126 string) Result__unit__string {
    var mtmp73 Tuple2_4bool_6string
    var inline1607 Tuple2_4bool_6string = _goml_runtime_std_fs_create_dir_all(path__126)
    mtmp73 = inline1607
    var x74 bool = mtmp73._0
    var x75 string = mtmp73._1
    if x74 {
        var t906 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t906
    } else {
        var t907 Result__unit__string = Result__unit__string_Err{
            _0: x75,
        }
        return t907
    }
}

func main0() struct{} {
    var t958 string = string_byte_slice("a你好z", 1, 7)
    _goml_m_std_p_io_p_println____T__string(t958)
    _goml_m_std_p_io_p_eprint____T__string("")
    var inline1662 string = ""
    var inline1663 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline1662)
    var inline1664 string = inline1663 + "\n"
    _goml_m_std_p_internal_p_host_p_eprint(inline1664)
    var t959 Result__unit__string = _goml_m_std_p_fs_p_create__dir__all("goml-self-host/nested")
    var t960 string
    switch t959.(type) {
    case Result__unit__string_Ok:
        t960 = "ok"
    case Result__unit__string_Err:
        var inline1658 string = t959.(Result__unit__string_Err)._0
        var inline1660 string = "err " + inline1658
        t960 = inline1660
    default:
        panic("non-exhaustive match")
    }
    var inline1655 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t960)
    _goml_m_std_p_internal_p_host_p_println(inline1655)
    var t961 Result__unit__string
    var inline1650 string = "goml-self-host/nested/output.txt"
    var inline1651 string = "boot"
    var inline1652 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1651)
    var inline1653 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(inline1650, inline1652)
    t961 = inline1653
    var t962 string
    switch t961.(type) {
    case Result__unit__string_Ok:
        t962 = "ok"
    case Result__unit__string_Err:
        var inline1646 string = t961.(Result__unit__string_Err)._0
        var inline1648 string = "err " + inline1646
        t962 = inline1648
    default:
        panic("non-exhaustive match")
    }
    var inline1643 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t962)
    _goml_m_std_p_internal_p_host_p_println(inline1643)
    var t963 Result__string__string
    var inline1634 string = "goml-self-host/nested/output.txt"
    var inline1635 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(inline1634)
    switch inline1635.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var inline1636 _goml_m_std_p_bytes_p_Bytes = inline1635.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline1638 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(inline1636)
        t963 = inline1638
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var inline1639 string = inline1635.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var inline1641 Result__string__string = Result__string__string_Err{
            _0: inline1639,
        }
        t963 = inline1641
    default:
        panic("non-exhaustive match")
    }
    var t964 string
    switch t963.(type) {
    case Result__string__string_Ok:
        var inline1628 string = t963.(Result__string__string_Ok)._0
        t964 = inline1628
    case Result__string__string_Err:
        var inline1630 string = t963.(Result__string__string_Err)._0
        var inline1632 string = "err " + inline1630
        t964 = inline1632
    default:
        panic("non-exhaustive match")
    }
    var inline1625 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t964)
    _goml_m_std_p_internal_p_host_p_println(inline1625)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1008:
    for {
        var t1009 int
        var inline1674 int = _goml_runtime_core_string_len(x12)
        t1009 = inline1674
        var t1010 bool = index__26 < t1009
        if t1010 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t1012 int = compound_old17 + x16
                index__26 = t1012
                continue
            } else {
                var t1014 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t1014
            }
        } else {
            break Loop_loop1008
        }
    }
    var t1007 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t1007
}

func _goml_m_std_p_io_p_println____T__string(value__68 string) struct{} {
    var t1039 string
    t1039 = value__68
    _goml_runtime_std_io_println(t1039)
    return struct{}{}
}

func string_byte_slice(value__21 string, start__22 int, end__23 int) string {
    var t1049 bool = string_is_char_boundary(value__21, start__22)
    var jp1046 bool
    if t1049 {
        var t1050 bool = string_is_char_boundary(value__21, end__23)
        jp1046 = t1050
    } else {
        jp1046 = false
    }
    if jp1046 {
        var t1047 string = _goml_runtime_core_string_byte_slice(value__21, start__22, end__23)
        return t1047
    } else {
        var t1048 string = _goml_runtime_core_string_byte_slice(value__21, -1, -1)
        return t1048
    }
}

func _goml_m_std_p_io_p_eprint____T__string(value__69 string) struct{} {
    var t1052 string
    t1052 = value__69
    _goml_runtime_std_io_eprint(t1052)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1060 int = _goml_runtime_core_string_len(self__36)
    return t1060
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1179 bool = index__6 < 0
    var jp1177 bool
    if t1179 {
        jp1177 = true
    } else {
        var t1180 bool = index__6 >= length__7
        jp1177 = t1180
    }
    if jp1177 {
        var inline1689 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1689
    } else {
        var t1064 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1064))
        var t1067 bool = first__8 < 128
        if t1067 {
            var inline1691 int = 1
            var inline1692 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1692.(type) {
            case Option__char_None:
                var inline1693 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1693
            case Option__char_Some:
                var inline1694 rune = inline1692.(Option__char_Some)._0
                var inline1696 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1694,
                    _2: inline1691,
                }
                return inline1696
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1071 bool = first__8 < 194
            if t1071 {
                var inline1698 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1698
            } else {
                var t1075 bool = first__8 < 224
                if t1075 {
                    var t1088 int = length__7 - index__6
                    var t1089 bool = t1088 < 2
                    if t1089 {
                        var inline1700 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1700
                    } else {
                        var t1077 int = index__6 + 1
                        var t1078 uint8
                        var inline1714 uint8 = _goml_runtime_core_string_byte_get(value__5, t1077)
                        t1078 = inline1714
                        var second__9 uint32 = uint32(uint8(t1078))
                        var t1081 bool
                        var inline1711 bool = second__9 < 128
                        if inline1711 {
                            t1081 = true
                        } else {
                            var inline1712 bool = second__9 > 191
                            t1081 = inline1712
                        }
                        if t1081 {
                            var inline1702 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1702
                        } else {
                            var t1083_rhs uint32 = 31
                            var t1083 uint32 = first__8 & t1083_rhs
                            var t1084_rhs int = 6
                            var t1084 uint32 = t1083 << t1084_rhs
                            var t1085_rhs uint32 = 63
                            var t1085 uint32 = second__9 & t1085_rhs
                            var t1086 uint32 = t1084 | t1085
                            var inline1704 int = 2
                            var inline1705 Option__char = __goml_builtin_char_from_uint32(t1086)
                            switch inline1705.(type) {
                            case Option__char_None:
                                var inline1706 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1706
                            case Option__char_Some:
                                var inline1707 rune = inline1705.(Option__char_Some)._0
                                var inline1709 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1707,
                                    _2: inline1704,
                                }
                                return inline1709
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1093 bool = first__8 < 240
                    if t1093 {
                        var t1126 int = length__7 - index__6
                        var t1127 bool = t1126 < 3
                        if t1127 {
                            var inline1716 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1716
                        } else {
                            var t1095 int = index__6 + 1
                            var t1096 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1095)
                            var second__10 uint32 = uint32(uint8(t1096))
                            var t1097 int = index__6 + 2
                            var t1098 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1097)
                            var third__11 uint32 = uint32(uint8(t1098))
                            var t1124 bool = utf8_invalid_continuation(second__10)
                            var jp1119 bool
                            if t1124 {
                                jp1119 = true
                            } else {
                                var inline1718 bool = third__11 < 128
                                if inline1718 {
                                    jp1119 = true
                                } else {
                                    var inline1719 bool = third__11 > 191
                                    jp1119 = inline1719
                                }
                            }
                            var jp1113 bool
                            if jp1119 {
                                jp1113 = true
                            } else {
                                var t1122 bool = first__8 == 224
                                if t1122 {
                                    var t1123 bool = second__10 < 160
                                    jp1113 = t1123
                                } else {
                                    jp1113 = false
                                }
                            }
                            var jp1102 bool
                            if jp1113 {
                                jp1102 = true
                            } else {
                                var t1116 bool = first__8 == 237
                                if t1116 {
                                    var t1117 bool = second__10 >= 160
                                    jp1102 = t1117
                                } else {
                                    jp1102 = false
                                }
                            }
                            if jp1102 {
                                var inline1721 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1721
                            } else {
                                var t1104_rhs uint32 = 15
                                var t1104 uint32 = first__8 & t1104_rhs
                                var t1105_rhs int = 12
                                var t1105 uint32 = t1104 << t1105_rhs
                                var t1106_rhs uint32 = 63
                                var t1106 uint32 = second__10 & t1106_rhs
                                var t1107_rhs int = 6
                                var t1107 uint32 = t1106 << t1107_rhs
                                var t1108 uint32 = t1105 | t1107
                                var t1109_rhs uint32 = 63
                                var t1109 uint32 = third__11 & t1109_rhs
                                var t1110 uint32 = t1108 | t1109
                                var inline1723 int = 3
                                var inline1724 Option__char = __goml_builtin_char_from_uint32(t1110)
                                switch inline1724.(type) {
                                case Option__char_None:
                                    var inline1725 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1725
                                case Option__char_Some:
                                    var inline1726 rune = inline1724.(Option__char_Some)._0
                                    var inline1728 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1726,
                                        _2: inline1723,
                                    }
                                    return inline1728
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1131 bool = first__8 < 245
                        if t1131 {
                            var t1172 int = length__7 - index__6
                            var t1173 bool = t1172 < 4
                            if t1173 {
                                var t1174 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1174
                            } else {
                                var t1133 int = index__6 + 1
                                var t1134 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1133)
                                var second__12 uint32 = uint32(uint8(t1134))
                                var t1135 int = index__6 + 2
                                var t1136 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1135)
                                var third__13 uint32 = uint32(uint8(t1136))
                                var t1137 int = index__6 + 3
                                var t1138 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1137)
                                var fourth__14 uint32 = uint32(uint8(t1138))
                                var t1170 bool = utf8_invalid_continuation(second__12)
                                var jp1168 bool
                                if t1170 {
                                    jp1168 = true
                                } else {
                                    var t1171 bool = utf8_invalid_continuation(third__13)
                                    jp1168 = t1171
                                }
                                var jp1162 bool
                                if jp1168 {
                                    jp1162 = true
                                } else {
                                    var t1169 bool = utf8_invalid_continuation(fourth__14)
                                    jp1162 = t1169
                                }
                                var jp1156 bool
                                if jp1162 {
                                    jp1156 = true
                                } else {
                                    var t1165 bool = first__8 == 240
                                    if t1165 {
                                        var t1166 bool = second__12 < 144
                                        jp1156 = t1166
                                    } else {
                                        jp1156 = false
                                    }
                                }
                                var jp1142 bool
                                if jp1156 {
                                    jp1142 = true
                                } else {
                                    var t1159 bool = first__8 == 244
                                    if t1159 {
                                        var t1160 bool = second__12 > 143
                                        jp1142 = t1160
                                    } else {
                                        jp1142 = false
                                    }
                                }
                                if jp1142 {
                                    var t1143 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1143
                                } else {
                                    var t1144_rhs uint32 = 7
                                    var t1144 uint32 = first__8 & t1144_rhs
                                    var t1145_rhs int = 18
                                    var t1145 uint32 = t1144 << t1145_rhs
                                    var t1146_rhs uint32 = 63
                                    var t1146 uint32 = second__12 & t1146_rhs
                                    var t1147_rhs int = 12
                                    var t1147 uint32 = t1146 << t1147_rhs
                                    var t1148 uint32 = t1145 | t1147
                                    var t1149_rhs uint32 = 63
                                    var t1149 uint32 = third__13 & t1149_rhs
                                    var t1150_rhs int = 6
                                    var t1150 uint32 = t1149 << t1150_rhs
                                    var t1151 uint32 = t1148 | t1150
                                    var t1152_rhs uint32 = 63
                                    var t1152 uint32 = fourth__14 & t1152_rhs
                                    var t1153 uint32 = t1151 | t1152
                                    var t1154 Tuple3_4bool_4char_3int = utf8_valid_decode(t1153, 4)
                                    return t1154
                                }
                            }
                        } else {
                            var t1175 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1175
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
    var t1202 bool = index__16 < 0
    var jp1194 bool
    if t1202 {
        jp1194 = true
    } else {
        var t1203 int
        var inline1730 int = _goml_runtime_core_string_len(value__15)
        t1203 = inline1730
        var t1204 bool = index__16 > t1203
        jp1194 = t1204
    }
    if jp1194 {
        return false
    } else {
        var t1197 int
        var inline1734 int = _goml_runtime_core_string_len(value__15)
        t1197 = inline1734
        var t1198 bool = index__16 == t1197
        if t1198 {
            return true
        } else {
            var t1199 uint8
            var inline1732 uint8 = _goml_runtime_core_string_byte_get(value__15, index__16)
            t1199 = inline1732
            var t1200_rhs uint8 = 192
            var t1200 uint8 = t1199 & t1200_rhs
            var t1201 bool = t1200 != 128
            return t1201
        }
    }
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1207 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1207
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1210 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1210
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1770 rune
    var inline1738 bool = utf8_valid_scalar(value__0)
    if inline1738 {
        var inline1739 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1740 rune = inline1739._1
        commute_field1770 = inline1740
        var t1216 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1770,
            _2: width__1,
        }
        return t1216
    } else {
        var inline1736 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1736
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1221 bool = value__3 < 128
    if t1221 {
        return true
    } else {
        var t1222 bool = value__3 > 191
        return t1222
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1227 bool
    var inline1744 bool = value__30 <= 1114111
    if inline1744 {
        var inline1745 bool = value__30 >= 55296
        var inline1747 bool
        if inline1745 {
            var inline1749 bool = value__30 <= 57343
            inline1747 = inline1749
        } else {
            inline1747 = false
        }
        var inline1748 bool = !inline1747
        t1227 = inline1748
    } else {
        t1227 = false
    }
    if t1227 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1228 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1228
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1233 bool = value__4 <= 1114111
    if t1233 {
        var t1237 bool = value__4 >= 55296
        var jp1235 bool
        if t1237 {
            var t1238 bool = value__4 <= 57343
            jp1235 = t1238
        } else {
            jp1235 = false
        }
        var t1236 bool = !jp1235
        return t1236
    } else {
        return false
    }
}

func main() {
    main0()
}
