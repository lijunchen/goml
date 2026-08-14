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
    var t204 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t204
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t207 *_goml_vec_uint8
    var inline1222 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t207 = inline1222
    var t208 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t207,
    }
    return t208
}

func _goml_m_std_p_internal_p_host_p_read__bytes(path__4 string) Tuple3_4bool_10Vec_5uint8_6string {
    var t280 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__4)
    return t280
}

func _goml_m_std_p_internal_p_host_p_file__exists(path__20 string) bool {
    var t310 bool = _goml_runtime_std_fs_file_exists(path__20)
    return t310
}

func _goml_m_std_p_internal_p_host_p_read__dir(path__24 string) Tuple3_4bool_11Vec_6string_6string {
    var t322 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__24)
    return t322
}

func _goml_m_std_p_internal_p_host_p_println(value__28 string) struct{} {
    _goml_runtime_std_io_println(value__28)
    return struct{}{}
}

func _goml_m_std_p_fs_p_read__file(path__113 string) Result__string__string {
    var commute_field1738 _goml_m_std_p_bytes_p_Bytes
    var commute_field1740 string
    var inline1554 Tuple3_4bool_10Vec_5uint8_6string = _goml_m_std_p_internal_p_host_p_read__bytes(path__113)
    var inline1555 bool = inline1554._0
    var inline1556 *_goml_vec_uint8 = inline1554._1
    var inline1557 string = inline1554._2
    if inline1555 {
        var inline1561 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline1556)
        commute_field1738 = inline1561
        var inline1545 *_goml_vec_uint8 = commute_field1738.values
        var inline1546 Tuple2_4bool_6string = string_from_utf8(inline1545)
        var inline1547 bool = inline1546._0
        var inline1548 string = inline1546._1
        if inline1547 {
            var inline1551 Result__string__string = Result__string__string_Ok{
                _0: inline1548,
            }
            return inline1551
        } else {
            var inline1552 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline1552
        }
    } else {
        commute_field1740 = inline1557
        var t887 Result__string__string = Result__string__string_Err{
            _0: commute_field1740,
        }
        return t887
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t901 *_goml_vec_uint8
    var inline1583 *_goml_vec_uint8 = data__123.values
    t901 = inline1583
    var mtmp70 Tuple2_4bool_6string
    var inline1581 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t901)
    mtmp70 = inline1581
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t904 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t904
    } else {
        var t905 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t905
    }
}

func main0() struct{} {
    var inline1647 string = "goml-std-test.txt"
    var inline1648 string = "std-ok"
    var inline1649 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1648)
    _goml_m_std_p_fs_p_write__bytes(inline1647, inline1649)
    var t965 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t966 string
    switch t965.(type) {
    case Result__string__string_Ok:
        var inline1641 string = t965.(Result__string__string_Ok)._0
        t966 = inline1641
    case Result__string__string_Err:
        var inline1643 string = t965.(Result__string__string_Err)._0
        var inline1645 string = "err " + inline1643
        t966 = inline1645
    default:
        panic("non-exhaustive match")
    }
    var inline1638 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t966)
    _goml_m_std_p_internal_p_host_p_println(inline1638)
    var t967 bool
    var inline1635 string = "goml-std-test.txt"
    var inline1636 bool = _goml_m_std_p_internal_p_host_p_file__exists(inline1635)
    t967 = inline1636
    var t968 string
    var inline1633 string = _goml_runtime_core_bool_to_string(t967)
    t968 = inline1633
    var inline1630 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t968)
    _goml_m_std_p_internal_p_host_p_println(inline1630)
    var t969 _goml_m_Result____Vec_l_string_r_____string
    var inline1619 string = "."
    var inline1620 Tuple3_4bool_11Vec_6string_6string = _goml_m_std_p_internal_p_host_p_read__dir(inline1619)
    var inline1621 bool = inline1620._0
    var inline1622 *_goml_vec_string = inline1620._1
    var inline1623 string = inline1620._2
    if inline1621 {
        var inline1627 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline1622,
        }
        t969 = inline1627
    } else {
        var inline1628 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline1623,
        }
        t969 = inline1628
    }
    var t970 string
    switch t969.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline1610 *_goml_vec_string = t969.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline1612 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline1610)
        var inline1613 bool = inline1612 > 0
        var inline1614 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1613)
        t970 = inline1614
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline1615 string = t969.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline1617 string = "err " + inline1615
        t970 = inline1617
    default:
        panic("non-exhaustive match")
    }
    var inline1607 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t970)
    _goml_m_std_p_internal_p_host_p_println(inline1607)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop1013:
    for {
        var t1014 int
        var inline1659 int = _goml_runtime_core_string_len(x12)
        t1014 = inline1659
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

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__189 *_goml_vec_string) int {
    var t1045 int = vec_len__Vec_6string(self__189)
    return t1045
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t1048 string = _goml_runtime_core_bool_to_string(self__64)
    return t1048
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__36 string) int {
    var t1054 int = _goml_runtime_core_string_len(self__36)
    return t1054
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t1173 bool = index__6 < 0
    var jp1171 bool
    if t1173 {
        jp1171 = true
    } else {
        var t1174 bool = index__6 >= length__7
        jp1171 = t1174
    }
    if jp1171 {
        var inline1668 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1668
    } else {
        var t1058 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t1058))
        var t1061 bool = first__8 < 128
        if t1061 {
            var inline1670 int = 1
            var inline1671 Option__char = __goml_builtin_char_from_uint32(first__8)
            switch inline1671.(type) {
            case Option__char_None:
                var inline1672 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1672
            case Option__char_Some:
                var inline1673 rune = inline1671.(Option__char_Some)._0
                var inline1675 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1673,
                    _2: inline1670,
                }
                return inline1675
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t1065 bool = first__8 < 194
            if t1065 {
                var inline1677 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1677
            } else {
                var t1069 bool = first__8 < 224
                if t1069 {
                    var t1082 int = length__7 - index__6
                    var t1083 bool = t1082 < 2
                    if t1083 {
                        var inline1679 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1679
                    } else {
                        var t1071 int = index__6 + 1
                        var t1072 uint8
                        var inline1693 uint8 = _goml_runtime_core_string_byte_get(value__5, t1071)
                        t1072 = inline1693
                        var second__9 uint32 = uint32(uint8(t1072))
                        var t1075 bool
                        var inline1690 bool = second__9 < 128
                        if inline1690 {
                            t1075 = true
                        } else {
                            var inline1691 bool = second__9 > 191
                            t1075 = inline1691
                        }
                        if t1075 {
                            var inline1681 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1681
                        } else {
                            var t1077_rhs uint32 = 31
                            var t1077 uint32 = first__8 & t1077_rhs
                            var t1078_rhs int = 6
                            var t1078 uint32 = t1077 << t1078_rhs
                            var t1079_rhs uint32 = 63
                            var t1079 uint32 = second__9 & t1079_rhs
                            var t1080 uint32 = t1078 | t1079
                            var inline1683 int = 2
                            var inline1684 Option__char = __goml_builtin_char_from_uint32(t1080)
                            switch inline1684.(type) {
                            case Option__char_None:
                                var inline1685 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1685
                            case Option__char_Some:
                                var inline1686 rune = inline1684.(Option__char_Some)._0
                                var inline1688 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1686,
                                    _2: inline1683,
                                }
                                return inline1688
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t1087 bool = first__8 < 240
                    if t1087 {
                        var t1120 int = length__7 - index__6
                        var t1121 bool = t1120 < 3
                        if t1121 {
                            var inline1695 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1695
                        } else {
                            var t1089 int = index__6 + 1
                            var t1090 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1089)
                            var second__10 uint32 = uint32(uint8(t1090))
                            var t1091 int = index__6 + 2
                            var t1092 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1091)
                            var third__11 uint32 = uint32(uint8(t1092))
                            var t1118 bool = utf8_invalid_continuation(second__10)
                            var jp1113 bool
                            if t1118 {
                                jp1113 = true
                            } else {
                                var inline1697 bool = third__11 < 128
                                if inline1697 {
                                    jp1113 = true
                                } else {
                                    var inline1698 bool = third__11 > 191
                                    jp1113 = inline1698
                                }
                            }
                            var jp1107 bool
                            if jp1113 {
                                jp1107 = true
                            } else {
                                var t1116 bool = first__8 == 224
                                if t1116 {
                                    var t1117 bool = second__10 < 160
                                    jp1107 = t1117
                                } else {
                                    jp1107 = false
                                }
                            }
                            var jp1096 bool
                            if jp1107 {
                                jp1096 = true
                            } else {
                                var t1110 bool = first__8 == 237
                                if t1110 {
                                    var t1111 bool = second__10 >= 160
                                    jp1096 = t1111
                                } else {
                                    jp1096 = false
                                }
                            }
                            if jp1096 {
                                var inline1700 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1700
                            } else {
                                var t1098_rhs uint32 = 15
                                var t1098 uint32 = first__8 & t1098_rhs
                                var t1099_rhs int = 12
                                var t1099 uint32 = t1098 << t1099_rhs
                                var t1100_rhs uint32 = 63
                                var t1100 uint32 = second__10 & t1100_rhs
                                var t1101_rhs int = 6
                                var t1101 uint32 = t1100 << t1101_rhs
                                var t1102 uint32 = t1099 | t1101
                                var t1103_rhs uint32 = 63
                                var t1103 uint32 = third__11 & t1103_rhs
                                var t1104 uint32 = t1102 | t1103
                                var inline1702 int = 3
                                var inline1703 Option__char = __goml_builtin_char_from_uint32(t1104)
                                switch inline1703.(type) {
                                case Option__char_None:
                                    var inline1704 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1704
                                case Option__char_Some:
                                    var inline1705 rune = inline1703.(Option__char_Some)._0
                                    var inline1707 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1705,
                                        _2: inline1702,
                                    }
                                    return inline1707
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t1125 bool = first__8 < 245
                        if t1125 {
                            var t1166 int = length__7 - index__6
                            var t1167 bool = t1166 < 4
                            if t1167 {
                                var t1168 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t1168
                            } else {
                                var t1127 int = index__6 + 1
                                var t1128 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1127)
                                var second__12 uint32 = uint32(uint8(t1128))
                                var t1129 int = index__6 + 2
                                var t1130 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1129)
                                var third__13 uint32 = uint32(uint8(t1130))
                                var t1131 int = index__6 + 3
                                var t1132 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t1131)
                                var fourth__14 uint32 = uint32(uint8(t1132))
                                var t1164 bool = utf8_invalid_continuation(second__12)
                                var jp1162 bool
                                if t1164 {
                                    jp1162 = true
                                } else {
                                    var t1165 bool = utf8_invalid_continuation(third__13)
                                    jp1162 = t1165
                                }
                                var jp1156 bool
                                if jp1162 {
                                    jp1156 = true
                                } else {
                                    var t1163 bool = utf8_invalid_continuation(fourth__14)
                                    jp1156 = t1163
                                }
                                var jp1150 bool
                                if jp1156 {
                                    jp1150 = true
                                } else {
                                    var t1159 bool = first__8 == 240
                                    if t1159 {
                                        var t1160 bool = second__12 < 144
                                        jp1150 = t1160
                                    } else {
                                        jp1150 = false
                                    }
                                }
                                var jp1136 bool
                                if jp1150 {
                                    jp1136 = true
                                } else {
                                    var t1153 bool = first__8 == 244
                                    if t1153 {
                                        var t1154 bool = second__12 > 143
                                        jp1136 = t1154
                                    } else {
                                        jp1136 = false
                                    }
                                }
                                if jp1136 {
                                    var t1137 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t1137
                                } else {
                                    var t1138_rhs uint32 = 7
                                    var t1138 uint32 = first__8 & t1138_rhs
                                    var t1139_rhs int = 18
                                    var t1139 uint32 = t1138 << t1139_rhs
                                    var t1140_rhs uint32 = 63
                                    var t1140 uint32 = second__12 & t1140_rhs
                                    var t1141_rhs int = 12
                                    var t1141 uint32 = t1140 << t1141_rhs
                                    var t1142 uint32 = t1139 | t1141
                                    var t1143_rhs uint32 = 63
                                    var t1143 uint32 = third__13 & t1143_rhs
                                    var t1144_rhs int = 6
                                    var t1144 uint32 = t1143 << t1144_rhs
                                    var t1145 uint32 = t1142 | t1144
                                    var t1146_rhs uint32 = 63
                                    var t1146 uint32 = fourth__14 & t1146_rhs
                                    var t1147 uint32 = t1145 | t1146
                                    var t1148 Tuple3_4bool_4char_3int = utf8_valid_decode(t1147, 4)
                                    return t1148
                                }
                            }
                        } else {
                            var t1169 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t1169
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
    var t1185 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t1185
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__39 string, index__40 int) uint8 {
    var t1188 uint8 = _goml_runtime_core_string_byte_get(self__39, index__40)
    return t1188
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1743 rune
    var inline1711 bool = utf8_valid_scalar(value__0)
    if inline1711 {
        var inline1712 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1713 rune = inline1712._1
        commute_field1743 = inline1713
        var t1194 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1743,
            _2: width__1,
        }
        return t1194
    } else {
        var inline1709 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1709
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t1199 bool = value__3 < 128
    if t1199 {
        return true
    } else {
        var t1200 bool = value__3 > 191
        return t1200
    }
}

func __goml_builtin_char_from_uint32(value__30 uint32) Option__char {
    var t1205 bool
    var inline1717 bool = value__30 <= 1114111
    if inline1717 {
        var inline1718 bool = value__30 >= 55296
        var inline1720 bool
        if inline1718 {
            var inline1722 bool = value__30 <= 57343
            inline1720 = inline1722
        } else {
            inline1720 = false
        }
        var inline1721 bool = !inline1720
        t1205 = inline1721
    } else {
        t1205 = false
    }
    if t1205 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__30)
        var x24 rune = mtmp22._1
        var t1206 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1206
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1211 bool = value__4 <= 1114111
    if t1211 {
        var t1215 bool = value__4 >= 55296
        var jp1213 bool
        if t1215 {
            var t1216 bool = value__4 <= 57343
            jp1213 = t1216
        } else {
            jp1213 = false
        }
        var t1214 bool = !jp1213
        return t1214
    } else {
        return false
    }
}

func main() {
    main0()
}
