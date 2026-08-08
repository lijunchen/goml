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

type Tuple2_11Option__int_11Option__int struct {
    _0 Option__int
    _1 Option__int
}

type Tuple2_14Option__string_14Option__string struct {
    _0 Option__string
    _1 Option__string
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

type Tuple2_27_goml_m_std_p_fs_p_FileType_27_goml_m_std_p_fs_p_FileType struct {
    _0 _goml_m_std_p_fs_p_FileType
    _1 _goml_m_std_p_fs_p_FileType
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
    var inline1021 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t192 = inline1021
    var t193 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t192,
    }
    return t193
}

func _goml_m_std_p_fs_p_read__file(path__113 string) Result__string__string {
    var commute_field1515 _goml_m_std_p_bytes_p_Bytes
    var commute_field1517 string
    var inline1344 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__113)
    var inline1345 bool = inline1344._0
    var inline1346 *_goml_vec_uint8 = inline1344._1
    var inline1347 string = inline1344._2
    if inline1345 {
        var inline1351 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline1346)
        commute_field1515 = inline1351
        var inline1335 *_goml_vec_uint8 = commute_field1515.values
        var inline1336 Tuple2_4bool_6string = string_from_utf8(inline1335)
        var inline1337 bool = inline1336._0
        var inline1338 string = inline1336._1
        if inline1337 {
            var inline1341 Result__string__string = Result__string__string_Ok{
                _0: inline1338,
            }
            return inline1341
        } else {
            var inline1342 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline1342
        }
    } else {
        commute_field1517 = inline1347
        var t683 Result__string__string = Result__string__string_Err{
            _0: commute_field1517,
        }
        return t683
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__122 string, data__123 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t697 *_goml_vec_uint8
    var inline1369 *_goml_vec_uint8 = data__123.values
    t697 = inline1369
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
    var inline1415 string = "goml-std-test.txt"
    var inline1416 string = "std-ok"
    var inline1417 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline1416)
    _goml_m_std_p_fs_p_write__bytes(inline1415, inline1417)
    var t761 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t762 string
    switch t761.(type) {
    case Result__string__string_Ok:
        var inline1409 string = t761.(Result__string__string_Ok)._0
        t762 = inline1409
    case Result__string__string_Err:
        var inline1411 string = t761.(Result__string__string_Err)._0
        var inline1413 string = "err " + inline1411
        t762 = inline1413
    default:
        panic("non-exhaustive match")
    }
    var inline1406 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t762)
    _goml_runtime_std_io_println(inline1406)
    var t763 bool
    var inline1403 string = "goml-std-test.txt"
    var inline1404 bool = _goml_runtime_std_fs_file_exists(inline1403)
    t763 = inline1404
    var t764 string
    var inline1401 string = _goml_runtime_core_bool_to_string(t763)
    t764 = inline1401
    var inline1398 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t764)
    _goml_runtime_std_io_println(inline1398)
    var t765 _goml_m_Result____Vec_l_string_r_____string
    var inline1387 string = "."
    var inline1388 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(inline1387)
    var inline1389 bool = inline1388._0
    var inline1390 *_goml_vec_string = inline1388._1
    var inline1391 string = inline1388._2
    if inline1389 {
        var inline1395 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline1390,
        }
        t765 = inline1395
    } else {
        var inline1396 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline1391,
        }
        t765 = inline1396
    }
    var t766 string
    switch t765.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline1378 *_goml_vec_string = t765.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline1380 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline1378)
        var inline1381 bool = inline1380 > 0
        var inline1382 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline1381)
        t766 = inline1382
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline1383 string = t765.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline1385 string = "err " + inline1383
        t766 = inline1385
    default:
        panic("non-exhaustive match")
    }
    var inline1375 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t766)
    _goml_runtime_std_io_println(inline1375)
    return struct{}{}
}

func string_from_utf8(bytes__24 *_goml_vec_uint8) Tuple2_4bool_6string {
    var mtmp10 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(bytes__24)
    var x12 string = mtmp10._1
    var index__26 int = 0
    Loop_loop809:
    for {
        var t810 int
        var inline1433 int = _goml_runtime_core_string_len(x12)
        t810 = inline1433
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

func _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(self__117 uint32, other__118 uint32) bool {
    var t842 bool = self__117 == other__118
    return t842
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__187 *_goml_vec_string) int {
    var t850 int = vec_len__Vec_6string(self__187)
    return t850
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t853 string = _goml_runtime_core_bool_to_string(self__66)
    return t853
}

func _goml_m_inherent_i_string_i_string_i_byte__len(self__38 string) int {
    var t859 int = _goml_runtime_core_string_len(self__38)
    return t859
}

func string_decode_utf8_at(value__5 string, index__6 int) Tuple3_4bool_4char_3int {
    var length__7 int = _goml_m_inherent_i_string_i_string_i_byte__len(value__5)
    var t978 bool = index__6 < 0
    var jp976 bool
    if t978 {
        jp976 = true
    } else {
        var t979 bool = index__6 >= length__7
        jp976 = t979
    }
    if jp976 {
        var inline1440 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1440
    } else {
        var t863 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, index__6)
        var first__8 uint32 = uint32(uint8(t863))
        var t866 bool = first__8 < 128
        if t866 {
            var inline1442 int = 1
            var inline1443 Option__char = char_from_uint32(first__8)
            switch inline1443.(type) {
            case Option__char_None:
                var inline1444 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                return inline1444
            case Option__char_Some:
                var inline1445 rune = inline1443.(Option__char_Some)._0
                var inline1447 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: true,
                    _1: inline1445,
                    _2: inline1442,
                }
                return inline1447
            default:
                panic("non-exhaustive match")
            }
        } else {
            var t870 bool = first__8 < 194
            if t870 {
                var inline1449 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                    _0: false,
                    _1: 0,
                    _2: 0,
                }
                return inline1449
            } else {
                var t874 bool = first__8 < 224
                if t874 {
                    var t887 int = length__7 - index__6
                    var t888 bool = t887 < 2
                    if t888 {
                        var inline1451 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                            _0: false,
                            _1: 0,
                            _2: 0,
                        }
                        return inline1451
                    } else {
                        var t876 int = index__6 + 1
                        var t877 uint8
                        var inline1465 uint8 = _goml_runtime_core_string_byte_get(value__5, t876)
                        t877 = inline1465
                        var second__9 uint32 = uint32(uint8(t877))
                        var t880 bool
                        var inline1462 bool = second__9 < 128
                        if inline1462 {
                            t880 = true
                        } else {
                            var inline1463 bool = second__9 > 191
                            t880 = inline1463
                        }
                        if t880 {
                            var inline1453 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1453
                        } else {
                            var t882_rhs uint32 = 31
                            var t882 uint32 = first__8 & t882_rhs
                            var t883_rhs int = 6
                            var t883 uint32 = t882 << t883_rhs
                            var t884_rhs uint32 = 63
                            var t884 uint32 = second__9 & t884_rhs
                            var t885 uint32 = t883 | t884
                            var inline1455 int = 2
                            var inline1456 Option__char = char_from_uint32(t885)
                            switch inline1456.(type) {
                            case Option__char_None:
                                var inline1457 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return inline1457
                            case Option__char_Some:
                                var inline1458 rune = inline1456.(Option__char_Some)._0
                                var inline1460 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: true,
                                    _1: inline1458,
                                    _2: inline1455,
                                }
                                return inline1460
                            default:
                                panic("non-exhaustive match")
                            }
                        }
                    }
                } else {
                    var t892 bool = first__8 < 240
                    if t892 {
                        var t925 int = length__7 - index__6
                        var t926 bool = t925 < 3
                        if t926 {
                            var inline1467 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                _0: false,
                                _1: 0,
                                _2: 0,
                            }
                            return inline1467
                        } else {
                            var t894 int = index__6 + 1
                            var t895 uint8
                            var inline1482 uint8 = _goml_runtime_core_string_byte_get(value__5, t894)
                            t895 = inline1482
                            var second__10 uint32 = uint32(uint8(t895))
                            var t896 int = index__6 + 2
                            var t897 uint8
                            var inline1480 uint8 = _goml_runtime_core_string_byte_get(value__5, t896)
                            t897 = inline1480
                            var third__11 uint32 = uint32(uint8(t897))
                            var t923 bool = utf8_invalid_continuation(second__10)
                            var jp918 bool
                            if t923 {
                                jp918 = true
                            } else {
                                var inline1469 bool = third__11 < 128
                                if inline1469 {
                                    jp918 = true
                                } else {
                                    var inline1470 bool = third__11 > 191
                                    jp918 = inline1470
                                }
                            }
                            var jp912 bool
                            if jp918 {
                                jp912 = true
                            } else {
                                var t921 bool
                                var inline1472 uint32 = 224
                                var inline1473 bool = first__8 == inline1472
                                t921 = inline1473
                                if t921 {
                                    var t922 bool = second__10 < 160
                                    jp912 = t922
                                } else {
                                    jp912 = false
                                }
                            }
                            var jp901 bool
                            if jp912 {
                                jp901 = true
                            } else {
                                var t915 bool
                                var inline1475 uint32 = 237
                                var inline1476 bool = first__8 == inline1475
                                t915 = inline1476
                                if t915 {
                                    var t916 bool = second__10 >= 160
                                    jp901 = t916
                                } else {
                                    jp901 = false
                                }
                            }
                            if jp901 {
                                var inline1478 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
                                    _0: false,
                                    _1: 0,
                                    _2: 0,
                                }
                                return inline1478
                            } else {
                                var t903_rhs uint32 = 15
                                var t903 uint32 = first__8 & t903_rhs
                                var t904_rhs int = 12
                                var t904 uint32 = t903 << t904_rhs
                                var t905_rhs uint32 = 63
                                var t905 uint32 = second__10 & t905_rhs
                                var t906_rhs int = 6
                                var t906 uint32 = t905 << t906_rhs
                                var t907 uint32 = t904 | t906
                                var t908_rhs uint32 = 63
                                var t908 uint32 = third__11 & t908_rhs
                                var t909 uint32 = t907 | t908
                                var t910 Tuple3_4bool_4char_3int = utf8_valid_decode(t909, 3)
                                return t910
                            }
                        }
                    } else {
                        var t930 bool = first__8 < 245
                        if t930 {
                            var t971 int = length__7 - index__6
                            var t972 bool = t971 < 4
                            if t972 {
                                var t973 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                return t973
                            } else {
                                var t932 int = index__6 + 1
                                var t933 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t932)
                                var second__12 uint32 = uint32(uint8(t933))
                                var t934 int = index__6 + 2
                                var t935 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t934)
                                var third__13 uint32 = uint32(uint8(t935))
                                var t936 int = index__6 + 3
                                var t937 uint8 = _goml_m_inherent_i_string_i_string_i_byte__get(value__5, t936)
                                var fourth__14 uint32 = uint32(uint8(t937))
                                var t969 bool = utf8_invalid_continuation(second__12)
                                var jp967 bool
                                if t969 {
                                    jp967 = true
                                } else {
                                    var t970 bool = utf8_invalid_continuation(third__13)
                                    jp967 = t970
                                }
                                var jp961 bool
                                if jp967 {
                                    jp961 = true
                                } else {
                                    var t968 bool = utf8_invalid_continuation(fourth__14)
                                    jp961 = t968
                                }
                                var jp955 bool
                                if jp961 {
                                    jp955 = true
                                } else {
                                    var t964 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 240)
                                    if t964 {
                                        var t965 bool = second__12 < 144
                                        jp955 = t965
                                    } else {
                                        jp955 = false
                                    }
                                }
                                var jp941 bool
                                if jp955 {
                                    jp941 = true
                                } else {
                                    var t958 bool = _goml_m_trait__impl_i_PartialEq_i_uint32_i_eq(first__8, 244)
                                    if t958 {
                                        var t959 bool = second__12 > 143
                                        jp941 = t959
                                    } else {
                                        jp941 = false
                                    }
                                }
                                if jp941 {
                                    var t942 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                                    return t942
                                } else {
                                    var t943_rhs uint32 = 7
                                    var t943 uint32 = first__8 & t943_rhs
                                    var t944_rhs int = 18
                                    var t944 uint32 = t943 << t944_rhs
                                    var t945_rhs uint32 = 63
                                    var t945 uint32 = second__12 & t945_rhs
                                    var t946_rhs int = 12
                                    var t946 uint32 = t945 << t946_rhs
                                    var t947 uint32 = t944 | t946
                                    var t948_rhs uint32 = 63
                                    var t948 uint32 = third__13 & t948_rhs
                                    var t949_rhs int = 6
                                    var t949 uint32 = t948 << t949_rhs
                                    var t950 uint32 = t947 | t949
                                    var t951_rhs uint32 = 63
                                    var t951 uint32 = fourth__14 & t951_rhs
                                    var t952 uint32 = t950 | t951
                                    var t953 Tuple3_4bool_4char_3int = utf8_valid_decode(t952, 4)
                                    return t953
                                }
                            }
                        } else {
                            var t974 Tuple3_4bool_4char_3int = utf8_invalid_decode()
                            return t974
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
    var t984 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
        _0: false,
        _1: 0,
        _2: 0,
    }
    return t984
}

func _goml_m_inherent_i_string_i_string_i_byte__get(self__41 string, index__42 int) uint8 {
    var t987 uint8 = _goml_runtime_core_string_byte_get(self__41, index__42)
    return t987
}

func utf8_valid_decode(value__0 uint32, width__1 int) Tuple3_4bool_4char_3int {
    var commute_field1520 rune
    var inline1486 bool = utf8_valid_scalar(value__0)
    if inline1486 {
        var inline1487 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__0)
        var inline1489 rune = inline1487._1
        commute_field1520 = inline1489
        var t993 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: true,
            _1: commute_field1520,
            _2: width__1,
        }
        return t993
    } else {
        var inline1484 Tuple3_4bool_4char_3int = Tuple3_4bool_4char_3int{
            _0: false,
            _1: 0,
            _2: 0,
        }
        return inline1484
    }
}

func utf8_invalid_continuation(value__3 uint32) bool {
    var t998 bool = value__3 < 128
    if t998 {
        return true
    } else {
        var t999 bool = value__3 > 191
        return t999
    }
}

func char_from_uint32(value__32 uint32) Option__char {
    var t1004 bool
    var inline1493 bool = value__32 <= 1114111
    if inline1493 {
        var inline1494 bool = value__32 >= 55296
        var inline1496 bool
        if inline1494 {
            var inline1498 bool = value__32 <= 57343
            inline1496 = inline1498
        } else {
            inline1496 = false
        }
        var inline1497 bool = !inline1496
        t1004 = inline1497
    } else {
        t1004 = false
    }
    if t1004 {
        var mtmp22 Tuple2_4bool_4char = _goml_runtime_core_char_from_uint32(value__32)
        var x24 rune = mtmp22._1
        var t1005 Option__char = Option__char_Some{
            _0: x24,
        }
        return t1005
    } else {
        return Option__char_None{}
    }
}

func utf8_valid_scalar(value__4 uint32) bool {
    var t1010 bool = value__4 <= 1114111
    if t1010 {
        var t1014 bool = value__4 >= 55296
        var jp1012 bool
        if t1014 {
            var t1015 bool = value__4 <= 57343
            jp1012 = t1015
        } else {
            jp1012 = false
        }
        var t1013 bool = !jp1012
        return t1013
    } else {
        return false
    }
}

func main() {
    main0()
}
