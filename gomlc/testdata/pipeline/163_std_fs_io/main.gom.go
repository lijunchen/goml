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

func _goml_runtime_core_string_byte_get(s string, i int) uint8 {
    return s[i]
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

func _goml_runtime_std_fs_file_exists(path string) bool {
    var err error
    _, err = _goml_os.Stat(path)
    return err == nil
}

func _goml_runtime_std_fs_read_dir(path string) Tuple3_4bool_11Vec_6string_6string {
    var entries []_goml_os.DirEntry
    var err error
    entries, err = _goml_os.ReadDir(path)
    if err != nil {
        return Tuple3_4bool_11Vec_6string_6string{
            _0: false,
            _1: &_goml_vec_string{
                items: nil,
            },
            _2: err.Error(),
        }
    }
    var names []string
    var i int = 0
    for {
        if i >= int(len(entries)) {
            break
        }
        var entry _goml_os.DirEntry = entries[i]
        names = append(names, entry.Name())
        i = i + 1
    }
    return Tuple3_4bool_11Vec_6string_6string{
        _0: true,
        _1: &_goml_vec_string{
            items: names,
        },
        _2: "",
    }
}

func _goml_runtime_std_io_println(value string) struct{} {
    _goml_fmt.Println(value)
    return struct{}{}
}

type _goml_vec_uint8 struct {
    items []uint8
}

type _goml_vec_string struct {
    items []string
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
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

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(values__1 *_goml_vec_uint8) _goml_m_std_p_bytes_p_Bytes {
    var t199 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t199
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t202 *_goml_vec_uint8
    var inline1217 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t202 = inline1217
    var t203 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t202,
    }
    return t203
}

func _goml_m_std_p_internal_p_host_p_read__bytes(path__4 string) Tuple3_4bool_10Vec_5uint8_6string {
    var t275 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__4)
    return t275
}

func _goml_m_std_p_internal_p_host_p_file__exists(path__20 string) bool {
    var t305 bool = _goml_runtime_std_fs_file_exists(path__20)
    return t305
}

func _goml_m_std_p_internal_p_host_p_read__dir(path__24 string) Tuple3_4bool_11Vec_6string_6string {
    var t317 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__24)
    return t317
}

func _goml_m_std_p_internal_p_host_p_println(value__28 string) struct{} {
    _goml_runtime_std_io_println(value__28)
    return struct{}{}
}

func _goml_m_std_p_fs_p_read__file(path__113 string) Result__string__string {
    var commute_field1733 _goml_m_std_p_bytes_p_Bytes
    var commute_field1735 string
    var inline1549 Tuple3_4bool_10Vec_5uint8_6string = _goml_m_std_p_internal_p_host_p_read__bytes(path__113)
    var inline1550 bool = inline1549._0
    var inline1551 *_goml_vec_uint8 = inline1549._1
    var inline1552 string = inline1549._2
    if inline1550 {
        var inline1556 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline1551)
        commute_field1733 = inline1556
        var inline1540 *_goml_vec_uint8 = commute_field1733.values
        var inline1541 Tuple2_4bool_6string = string_from_utf8(inline1540)
        var inline1542 bool = inline1541._0
        var inline1543 string = inline1541._1
        if inline1542 {
            var inline1546 Result__string__string = Result__string__string_Ok{
                _0: inline1543,
            }
            return inline1546
        } else {
            var inline1547 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline1547
        }
    } else {
        commute_field1735 = inline1552
        var t882 Result__string__string = Result__string__string_Err{
            _0: commute_field1735,
        }
        return t882
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t896 *_goml_vec_uint8
    var inline1578 *_goml_vec_uint8 = data__123.values
    t896 = inline1578
    var mtmp70 Tuple2_4bool_6string
    var inline1576 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t896)
    mtmp70 = inline1576
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t899 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t899
    } else {
        var t900 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t900
    }
}

func main0() struct{} {
    var inline1642 string = "goml-std-test.txt"
    var inline1643 string = "std-ok"
    var inline1644 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1643)
    _goml_m_std_p_fs_p_write__bytes(inline1642, inline1644)
    var t960 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t961 string
    switch t960.(type) {
    case Result__string__string_Ok:
        var inline1636 string = t960.(Result__string__string_Ok)._0
        t961 = inline1636
    case Result__string__string_Err:
        var inline1638 string = t960.(Result__string__string_Err)._0
        var inline1640 string = "err " + inline1638
        t961 = inline1640
    default:
        panic("non-exhaustive match")
    }
    var inline1633 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t961)
    _goml_m_std_p_internal_p_host_p_println(inline1633)
    var t962 bool
    var inline1630 string = "goml-std-test.txt"
    var inline1631 bool = _goml_m_std_p_internal_p_host_p_file__exists(inline1630)
    t962 = inline1631
    var t963 string
    var inline1628 string = _goml_runtime_core_bool_to_string(t962)
    t963 = inline1628
    var inline1625 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t963)
    _goml_m_std_p_internal_p_host_p_println(inline1625)
    var t964 _goml_m_Result____Vec_l_string_r_____string
    var inline1614 string = "."
    var inline1615 Tuple3_4bool_11Vec_6string_6string = _goml_m_std_p_internal_p_host_p_read__dir(inline1614)
    var inline1616 bool = inline1615._0
    var inline1617 *_goml_vec_string = inline1615._1
    var inline1618 string = inline1615._2
    if inline1616 {
        var inline1622 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline1617,
        }
        t964 = inline1622
    } else {
        var inline1623 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline1618,
        }
        t964 = inline1623
    }
    var t965 string
    switch t964.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline1605 *_goml_vec_string = t964.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline1607 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline1605)
        var inline1608 bool = inline1607 > 0
        var inline1609 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1608)
        t965 = inline1609
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline1610 string = t964.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline1612 string = "err " + inline1610
        t965 = inline1612
    default:
        panic("non-exhaustive match")
    }
    var inline1602 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t965)
    _goml_m_std_p_internal_p_host_p_println(inline1602)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1008:
    for {
        var t1009 int
        var inline1654 int = _goml_runtime_core_string_len(x12)
        t1009 = inline1654
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__189 *_goml_vec_string) int {
    var t1040 int = vec_len__Vec_6string(self__189)
    return t1040
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t1043 string = _goml_runtime_core_bool_to_string(self__64)
    return t1043
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1049 int = _goml_runtime_core_string_len(self__36)
    return t1049
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1168 bool = index__6 < 0
    var jp1166 bool
    if t1168 {
        jp1166 = true
    } else {
        var t1169 bool = index__6 >= length__7
        jp1166 = t1169
    }
    if jp1166 {
        var inline1663 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1663
    } else {
        var t1053 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1053))
        var t1056 bool = first__8 < 128
        if t1056 {
            var inline1665 int = 1
            var inline1666 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1666.(type) {
            case Option__char_None:
                var inline1667 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1667
            case Option__char_Some:
                var inline1668 rune = inline1666.(Option__char_Some)._0
                var inline1670 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1668,
                    _2: inline1665,
                }
                return inline1670
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1060 bool = first__8 < 194
            if t1060 {
                var inline1672 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1672
            } else {
                var t1064 bool = first__8 < 224
                if t1064 {
                    var t1077 int = length__7 - index__6
                    var t1078 bool = t1077 < 2
                    if t1078 {
                        var inline1674 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1674
                    } else {
                        var t1066 int = index__6 + 1
                        var t1067 uint8
                        var inline1688 uint8 = _goml_runtime_core_string_byte_get(value__5, t1066)
                        t1067 = inline1688
                        var second__9 uint32 = uint32(uint8(t1067))
                        var t1070 bool
                        var inline1685 bool = second__9 < 128
                        if inline1685 {
                            t1070 = true
                        } else {
                            var inline1686 bool = second__9 > 191
                            t1070 = inline1686
                        }
                        if t1070 {
                            var inline1676 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1676
                        } else {
                            var t1072_rhs uint32 = 31
                            var t1072 uint32 = first__8 & t1072_rhs
                            var t1073_rhs int = 6
                            var t1073 uint32 = t1072 << t1073_rhs
                            var t1074_rhs uint32 = 63
                            var t1074 uint32 = second__9 & t1074_rhs
                            var t1075 uint32 = t1073 | t1074
                            var inline1678 int = 2
                            var inline1679 Option__char = __goml_builtin_char_from_uint32(t1075)
                            switch inline1679.(type) {
                            case Option__char_None:
                                var inline1680 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1680
                            case Option__char_Some:
                                var inline1681 rune = inline1679.(Option__char_Some)._0
                                var inline1683 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1681,
                                    _2: inline1678,
                                }
                                return inline1683
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1082 bool = first__8 < 240
                    if t1082 {
                        var t1115 int = length__7 - index__6
                        var t1116 bool = t1115 < 3
                        if t1116 {
                            var inline1690 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1690
                        } else {
                            var t1084 int = index__6 + 1
                            var t1085 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1084)
                            var second__10 uint32 = uint32(uint8(t1085))
                            var t1086 int = index__6 + 2
                            var t1087 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1086)
                            var third__11 uint32 = uint32(uint8(t1087))
                            var t1113 bool = utf8_invalid_continuation(second__10)
                            var jp1108 bool
                            if t1113 {
                                jp1108 = true
                            } else {
                                var inline1692 bool = third__11 < 128
                                if inline1692 {
                                    jp1108 = true
                                } else {
                                    var inline1693 bool = third__11 > 191
                                    jp1108 = inline1693
                                }
                            }
                            var jp1102 bool
                            if jp1108 {
                                jp1102 = true
                            } else {
                                var t1111 bool = first__8 == 224
                                if t1111 {
                                    var t1112 bool = second__10 < 160
                                    jp1102 = t1112
                                } else {
                                    jp1102 = false
                                }
                            }
                            var jp1091 bool
                            if jp1102 {
                                jp1091 = true
                            } else {
                                var t1105 bool = first__8 == 237
                                if t1105 {
                                    var t1106 bool = second__10 >= 160
                                    jp1091 = t1106
                                } else {
                                    jp1091 = false
                                }
                            }
                            if jp1091 {
                                var inline1695 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1695
                            } else {
                                var t1093_rhs uint32 = 15
                                var t1093 uint32 = first__8 & t1093_rhs
                                var t1094_rhs int = 12
                                var t1094 uint32 = t1093 << t1094_rhs
                                var t1095_rhs uint32 = 63
                                var t1095 uint32 = second__10 & t1095_rhs
                                var t1096_rhs int = 6
                                var t1096 uint32 = t1095 << t1096_rhs
                                var t1097 uint32 = t1094 | t1096
                                var t1098_rhs uint32 = 63
                                var t1098 uint32 = third__11 & t1098_rhs
                                var t1099 uint32 = t1097 | t1098
                                var inline1697 int = 3
                                var inline1698 Option__char = __goml_builtin_char_from_uint32(t1099)
                                switch inline1698.(type) {
                                case Option__char_None:
                                    var inline1699 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1699
                                case Option__char_Some:
                                    var inline1700 rune = inline1698.(Option__char_Some)._0
                                    var inline1702 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1700,
                                        _2: inline1697,
                                    }
                                    return inline1702
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1120 bool = first__8 < 245
                        if t1120 {
                            var t1161 int = length__7 - index__6
                            var t1162 bool = t1161 < 4
                            if t1162 {
                                var t1163 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1163
                            } else {
                                var t1122 int = index__6 + 1
                                var t1123 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1122)
                                var second__12 uint32 = uint32(uint8(t1123))
                                var t1124 int = index__6 + 2
                                var t1125 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1124)
                                var third__13 uint32 = uint32(uint8(t1125))
                                var t1126 int = index__6 + 3
                                var t1127 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1126)
                                var fourth__14 uint32 = uint32(uint8(t1127))
                                var t1159 bool = utf8_invalid_continuation(second__12)
                                var jp1157 bool
                                if t1159 {
                                    jp1157 = true
                                } else {
                                    var t1160 bool = utf8_invalid_continuation(third__13)
                                    jp1157 = t1160
                                }
                                var jp1151 bool
                                if jp1157 {
                                    jp1151 = true
                                } else {
                                    var t1158 bool = utf8_invalid_continuation(fourth__14)
                                    jp1151 = t1158
                                }
                                var jp1145 bool
                                if jp1151 {
                                    jp1145 = true
                                } else {
                                    var t1154 bool = first__8 == 240
                                    if t1154 {
                                        var t1155 bool = second__12 < 144
                                        jp1145 = t1155
                                    } else {
                                        jp1145 = false
                                    }
                                }
                                var jp1131 bool
                                if jp1145 {
                                    jp1131 = true
                                } else {
                                    var t1148 bool = first__8 == 244
                                    if t1148 {
                                        var t1149 bool = second__12 > 143
                                        jp1131 = t1149
                                    } else {
                                        jp1131 = false
                                    }
                                }
                                if jp1131 {
                                    var t1132 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1132
                                } else {
                                    var t1133_rhs uint32 = 7
                                    var t1133 uint32 = first__8 & t1133_rhs
                                    var t1134_rhs int = 18
                                    var t1134 uint32 = t1133 << t1134_rhs
                                    var t1135_rhs uint32 = 63
                                    var t1135 uint32 = second__12 & t1135_rhs
                                    var t1136_rhs int = 12
                                    var t1136 uint32 = t1135 << t1136_rhs
                                    var t1137 uint32 = t1134 | t1136
                                    var t1138_rhs uint32 = 63
                                    var t1138 uint32 = third__13 & t1138_rhs
                                    var t1139_rhs int = 6
                                    var t1139 uint32 = t1138 << t1139_rhs
                                    var t1140 uint32 = t1137 | t1139
                                    var t1141_rhs uint32 = 63
                                    var t1141 uint32 = fourth__14 & t1141_rhs
                                    var t1142 uint32 = t1140 | t1141
                                    var t1143 Tuple3_4bool_4char_3int = utf8_valid_decode(t1142, 4)
                                    return t1143
                                }
                            }
                        } else {
                            var t1164 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1164
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

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t1180 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1180
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1183 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1183
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1738 rune
    var inline1706 bool = utf8_valid_scalar(value__0)
    if inline1706 {
        var inline1707 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1708 rune = inline1707._1
        commute_field1738 = inline1708
        var t1189 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1738,
            _2: width__1,
        }
        return t1189
    } else {
        var inline1704 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1704
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1194 bool = value__3 < 128
    if t1194 {
        return true
    } else {
        var t1195 bool = value__3 > 191
        return t1195
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1200 bool
    var inline1712 bool = value__30 <= 1114111
    if inline1712 {
        var inline1713 bool = value__30 >= 55296
        var inline1715 bool
        if inline1713 {
            var inline1717 bool = value__30 <= 57343
            inline1715 = inline1717
        } else {
            inline1715 = false
        }
        var inline1716 bool = !inline1715
        t1200 = inline1716
    } else {
        t1200 = false
    }
    if t1200 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1201 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1201
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1206 bool = value__4 <= 1114111
    if t1206 {
        var t1210 bool = value__4 >= 55296
        var jp1208 bool
        if t1210 {
            var t1211 bool = value__4 <= 57343
            jp1208 = t1211
        } else {
            jp1208 = false
        }
        var t1209 bool = !jp1208
        return t1209
    } else {
        return false
    }
}

func main() {
    main0()
}
