package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
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

type _goml_vec__goml_m_std_p_fs_p_DirEntry struct {
    items []_goml_m_std_p_fs_p_DirEntry
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

type Tuple6_4bool_11Vec_6string_3int_4bool_3int_6string struct {
    _0 bool
    _1 *_goml_vec_string
    _2 int
    _3 bool
    _4 int
    _5 string
}

type Tuple3_4bool_6string_6string struct {
    _0 bool
    _1 string
    _2 string
}

type Tuple3_4bool_11Vec_6string_6string struct {
    _0 bool
    _1 *_goml_vec_string
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
    var t189 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t189
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t192 *_goml_vec_uint8
    var inline1018 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t192 = inline1018
    var t193 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t192,
    }
    return t193
}

func _goml_m_std_p_fs_p_read__file(path__113 string) Result__string__string {
    var commute_field1480 _goml_m_std_p_bytes_p_Bytes
    var commute_field1482 string
    var inline1320 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__113)
    var inline1321 bool = inline1320._0
    var inline1322 *_goml_vec_uint8 = inline1320._1
    var inline1323 string = inline1320._2
    if inline1321 {
        var inline1327 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline1322)
        commute_field1480 = inline1327
        var inline1311 *_goml_vec_uint8 = commute_field1480.values
        var inline1312 Tuple2_4bool_6string = string_from_utf8(inline1311)
        var inline1313 bool = inline1312._0
        var inline1314 string = inline1312._1
        if inline1313 {
            var inline1317 Result__string__string = Result__string__string_Ok{
                _0: inline1314,
            }
            return inline1317
        } else {
            var inline1318 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline1318
        }
    } else {
        commute_field1482 = inline1323
        var t683 Result__string__string = Result__string__string_Err{
            _0: commute_field1482,
        }
        return t683
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t697 *_goml_vec_uint8
    var inline1345 *_goml_vec_uint8 = data__123.values
    t697 = inline1345
    var mtmp70 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__122, t697)
    var x71 bool = mtmp70._0
    var x72 string = mtmp70._1
    if x71 {
        var t700 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t700
    } else {
        var t701 Result__unit__string = Result__unit__string_Err{
            _0: x72,
        }
        return t701
    }
}

func main0() struct{} {
    var inline1391 string = "goml-std-test.txt"
    var inline1392 string = "std-ok"
    var inline1393 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1392)
    _goml_m_std_p_fs_p_write__bytes(inline1391, inline1393)
    var t761 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t762 string
    switch t761.(type) {
    case Result__string__string_Ok:
        var inline1385 string = t761.(Result__string__string_Ok)._0
        t762 = inline1385
    case Result__string__string_Err:
        var inline1387 string = t761.(Result__string__string_Err)._0
        var inline1389 string = "err " + inline1387
        t762 = inline1389
    default:
        panic("non-exhaustive match")
    }
    var inline1382 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t762)
    _goml_runtime_std_io_println(inline1382)
    var t763 bool
    var inline1379 string = "goml-std-test.txt"
    var inline1380 bool = _goml_runtime_std_fs_file_exists(inline1379)
    t763 = inline1380
    var t764 string
    var inline1377 string = _goml_runtime_core_bool_to_string(t763)
    t764 = inline1377
    var inline1374 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t764)
    _goml_runtime_std_io_println(inline1374)
    var t765 _goml_m_Result____Vec_l_string_r_____string
    var inline1363 string = "."
    var inline1364 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(inline1363)
    var inline1365 bool = inline1364._0
    var inline1366 *_goml_vec_string = inline1364._1
    var inline1367 string = inline1364._2
    if inline1365 {
        var inline1371 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline1366,
        }
        t765 = inline1371
    } else {
        var inline1372 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline1367,
        }
        t765 = inline1372
    }
    var t766 string
    switch t765.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline1354 *_goml_vec_string = t765.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline1356 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline1354)
        var inline1357 bool = inline1356 > 0
        var inline1358 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1357)
        t766 = inline1358
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline1359 string = t765.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline1361 string = "err " + inline1359
        t766 = inline1361
    default:
        panic("non-exhaustive match")
    }
    var inline1351 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t766)
    _goml_runtime_std_io_println(inline1351)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop809:
    for {
        var t810 int
        var inline1403 int = _goml_runtime_core_string_len(x12)
        t810 = inline1403
        var t811 bool = index__26 < t810
        if t811 {
            var mtmp13 Tuple3_4bool_4char_3int = string_decode_utf8_at(x12, index__26)
            var x14 bool = mtmp13._0
            var x16 int = mtmp13._2
            if x14 {
                var compound_old17 int = index__26
                var t813 int = compound_old17 + x16
                index__26 = t813
                continue
            } else {
                var t815 Tuple2_4bool_6string = Tuple2_4bool_6string{
                    _0: false,
                    _1: "",
                }
                return t815
            }
        } else {
            break Loop_loop809
        }
    }
    var t808 Tuple2_4bool_6string = Tuple2_4bool_6string{
        _0: true,
        _1: x12,
    }
    return t808
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__187 *_goml_vec_string) int {
    var t841 int = vec_len__Vec_6string(self__187)
    return t841
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t844 string = _goml_runtime_core_bool_to_string(self__66)
    return t844
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t850 int = _goml_runtime_core_string_len(self__38)
    return t850
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t969 bool = index__6 < 0
    var jp967 bool
    if t969 {
        jp967 = true
    } else {
        var t970 bool = index__6 >= length__7
        jp967 = t970
    }
    if jp967 {
        var inline1410 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1410
    } else {
        var t854 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t854))
        var t857 bool = first__8 < 128
        if t857 {
            var inline1412 int = 1
            var inline1413 Option__char = char_from_uint32(first__8)
            switch inline1413.(type) {
            case Option__char_None:
                var inline1414 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1414
            case Option__char_Some:
                var inline1415 rune = inline1413.(Option__char_Some)._0
                var inline1417 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1415,
                    _2: inline1412,
                }
                return inline1417
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t861 bool = first__8 < 194
            if t861 {
                var inline1419 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1419
            } else {
                var t865 bool = first__8 < 224
                if t865 {
                    var t878 int = length__7 - index__6
                    var t879 bool = t878 < 2
                    if t879 {
                        var inline1421 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1421
                    } else {
                        var t867 int = index__6 + 1
                        var t868 uint8
                        var inline1435 uint8 = _goml_runtime_core_string_byte_get(value__5, t867)
                        t868 = inline1435
                        var second__9 uint32 = uint32(uint8(t868))
                        var t871 bool
                        var inline1432 bool = second__9 < 128
                        if inline1432 {
                            t871 = true
                        } else {
                            var inline1433 bool = second__9 > 191
                            t871 = inline1433
                        }
                        if t871 {
                            var inline1423 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1423
                        } else {
                            var t873_rhs uint32 = 31
                            var t873 uint32 = first__8 & t873_rhs
                            var t874_rhs int = 6
                            var t874 uint32 = t873 << t874_rhs
                            var t875_rhs uint32 = 63
                            var t875 uint32 = second__9 & t875_rhs
                            var t876 uint32 = t874 | t875
                            var inline1425 int = 2
                            var inline1426 Option__char = char_from_uint32(t876)
                            switch inline1426.(type) {
                            case Option__char_None:
                                var inline1427 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1427
                            case Option__char_Some:
                                var inline1428 rune = inline1426.(Option__char_Some)._0
                                var inline1430 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1428,
                                    _2: inline1425,
                                }
                                return inline1430
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t883 bool = first__8 < 240
                    if t883 {
                        var t916 int = length__7 - index__6
                        var t917 bool = t916 < 3
                        if t917 {
                            var inline1437 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1437
                        } else {
                            var t885 int = index__6 + 1
                            var t886 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t885)
                            var second__10 uint32 = uint32(uint8(t886))
                            var t887 int = index__6 + 2
                            var t888 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t887)
                            var third__11 uint32 = uint32(uint8(t888))
                            var t914 bool = utf8_invalid_continuation(second__10)
                            var jp909 bool
                            if t914 {
                                jp909 = true
                            } else {
                                var inline1439 bool = third__11 < 128
                                if inline1439 {
                                    jp909 = true
                                } else {
                                    var inline1440 bool = third__11 > 191
                                    jp909 = inline1440
                                }
                            }
                            var jp903 bool
                            if jp909 {
                                jp903 = true
                            } else {
                                var t912 bool = first__8 == 224
                                if t912 {
                                    var t913 bool = second__10 < 160
                                    jp903 = t913
                                } else {
                                    jp903 = false
                                }
                            }
                            var jp892 bool
                            if jp903 {
                                jp892 = true
                            } else {
                                var t906 bool = first__8 == 237
                                if t906 {
                                    var t907 bool = second__10 >= 160
                                    jp892 = t907
                                } else {
                                    jp892 = false
                                }
                            }
                            if jp892 {
                                var inline1442 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1442
                            } else {
                                var t894_rhs uint32 = 15
                                var t894 uint32 = first__8 & t894_rhs
                                var t895_rhs int = 12
                                var t895 uint32 = t894 << t895_rhs
                                var t896_rhs uint32 = 63
                                var t896 uint32 = second__10 & t896_rhs
                                var t897_rhs int = 6
                                var t897 uint32 = t896 << t897_rhs
                                var t898 uint32 = t895 | t897
                                var t899_rhs uint32 = 63
                                var t899 uint32 = third__11 & t899_rhs
                                var t900 uint32 = t898 | t899
                                var inline1444 int = 3
                                var inline1445 Option__char = char_from_uint32(t900)
                                switch inline1445.(type) {
                                case Option__char_None:
                                    var inline1446 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return inline1446
                                case Option__char_Some:
                                    var inline1447 rune = inline1445.(Option__char_Some)._0
                                    var inline1449 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                        _0: true,
                                        _1: inline1447,
                                        _2: inline1444,
                                    }
                                    return inline1449
                                default:
                                    panic("non-exhaustive match")
                                }
                            }
                        }
                    } else {
                        var t921 bool = first__8 < 245
                        if t921 {
                            var t962 int = length__7 - index__6
                            var t963 bool = t962 < 4
                            if t963 {
                                var t964 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t964
                            } else {
                                var t923 int = index__6 + 1
                                var t924 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t923)
                                var second__12 uint32 = uint32(uint8(t924))
                                var t925 int = index__6 + 2
                                var t926 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t925)
                                var third__13 uint32 = uint32(uint8(t926))
                                var t927 int = index__6 + 3
                                var t928 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t927)
                                var fourth__14 uint32 = uint32(uint8(t928))
                                var t960 bool = utf8_invalid_continuation(second__12)
                                var jp958 bool
                                if t960 {
                                    jp958 = true
                                } else {
                                    var t961 bool = utf8_invalid_continuation(third__13)
                                    jp958 = t961
                                }
                                var jp952 bool
                                if jp958 {
                                    jp952 = true
                                } else {
                                    var t959 bool = utf8_invalid_continuation(fourth__14)
                                    jp952 = t959
                                }
                                var jp946 bool
                                if jp952 {
                                    jp946 = true
                                } else {
                                    var t955 bool = first__8 == 240
                                    if t955 {
                                        var t956 bool = second__12 < 144
                                        jp946 = t956
                                    } else {
                                        jp946 = false
                                    }
                                }
                                var jp932 bool
                                if jp946 {
                                    jp932 = true
                                } else {
                                    var t949 bool = first__8 == 244
                                    if t949 {
                                        var t950 bool = second__12 > 143
                                        jp932 = t950
                                    } else {
                                        jp932 = false
                                    }
                                }
                                if jp932 {
                                    var t933 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t933
                                } else {
                                    var t934_rhs uint32 = 7
                                    var t934 uint32 = first__8 & t934_rhs
                                    var t935_rhs int = 18
                                    var t935 uint32 = t934 << t935_rhs
                                    var t936_rhs uint32 = 63
                                    var t936 uint32 = second__12 & t936_rhs
                                    var t937_rhs int = 12
                                    var t937 uint32 = t936 << t937_rhs
                                    var t938 uint32 = t935 | t937
                                    var t939_rhs uint32 = 63
                                    var t939 uint32 = third__13 & t939_rhs
                                    var t940_rhs int = 6
                                    var t940 uint32 = t939 << t940_rhs
                                    var t941 uint32 = t938 | t940
                                    var t942_rhs uint32 = 63
                                    var t942 uint32 = fourth__14 & t942_rhs
                                    var t943 uint32 = t941 | t942
                                    var t944 Tuple3_4bool_4char_3int = utf8_valid_decode(t943, 4)
                                    return t944
                                }
                            }
                        } else {
                            var t965 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t965
                        }
                    }
                }
            }
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func utf8_invalid_decode() Tuple3_4bool_4char_3int {
    var t981 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t981
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t984 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t984
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1485 rune
    var inline1453 bool = utf8_valid_scalar(value__0)
    if inline1453 {
        var inline1454 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1455 rune = inline1454._1
        commute_field1485 = inline1455
        var t990 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1485,
            _2: width__1,
        }
        return t990
    } else {
        var inline1451 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1451
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t995 bool = value__3 < 128
    if t995 {
        return true
    } else {
        var t996 bool = value__3 > 191
        return t996
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1001 bool
    var inline1459 bool = value__32 <= 1114111
    if inline1459 {
        var inline1460 bool = value__32 >= 55296
        var inline1462 bool
        if inline1460 {
            var inline1464 bool = value__32 <= 57343
            inline1462 = inline1464
        } else {
            inline1462 = false
        }
        var inline1463 bool = !inline1462
        t1001 = inline1463
    } else {
        t1001 = false
    }
    if t1001 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1002 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1002
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1007 bool = value__4 <= 1114111
    if t1007 {
        var t1011 bool = value__4 >= 55296
        var jp1009 bool
        if t1011 {
            var t1012 bool = value__4 <= 57343
            jp1009 = t1012
        } else {
            jp1009 = false
        }
        var t1010 bool = !jp1009
        return t1010
    } else {
        return false
    }
}

func main() {
    main0()
}
