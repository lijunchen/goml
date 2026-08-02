package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
    _goml_utf8 "unicode/utf8"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_to_bytes(s string) *_goml_vec_uint8 {
    return &_goml_vec_uint8{
        items: []byte(s),
    }
}

func _goml_runtime_core_string_from_utf8(bytes *_goml_vec_uint8) Tuple2_4bool_6string {
    if !_goml_utf8.Valid(bytes.items) {
        return Tuple2_4bool_6string{
            _0: false,
            _1: "",
        }
    }
    return Tuple2_4bool_6string{
        _0: true,
        _1: string(bytes.items),
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

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_10Vec_5uint8_6string struct {
    _0 bool
    _1 *_goml_vec_uint8
    _2 string
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

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type Option__uint8 interface {
    isOption__uint8()
}

type None struct {}

func (_ None) isOption__uint8() {}

type Some struct {
    _0 uint8
}

func (_ Some) isOption__uint8() {}

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

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(values__1 *_goml_vec_uint8) _goml_m_std_p_bytes_p_Bytes {
    var t172 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    return t172
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var t175 *_goml_vec_uint8
    var inline403 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(value__2)
    t175 = inline403
    var t176 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t175,
    }
    return t176
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var mtmp0 _goml_m_Result____std_p_bytes_p_Bytes____string
    var inline439 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__0)
    var inline440 bool = inline439._0
    var inline441 *_goml_vec_uint8 = inline439._1
    var inline442 string = inline439._2
    if inline440 {
        var inline446 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(inline441)
        var inline447 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: inline446,
        }
        mtmp0 = inline447
    } else {
        var inline448 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: inline442,
        }
        mtmp0 = inline448
    }
    switch mtmp0.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var x1 _goml_m_std_p_bytes_p_Bytes = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var inline430 *_goml_vec_uint8 = x1.values
        var inline431 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(inline430)
        var inline432 bool = inline431._0
        var inline433 string = inline431._1
        if inline432 {
            var inline436 Result__string__string = Result__string__string_Ok{
                _0: inline433,
            }
            return inline436
        } else {
            var inline437 Result__string__string = Result__string__string_Err{
                _0: "invalid UTF-8",
            }
            return inline437
        }
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var x2 string = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var t230 Result__string__string = Result__string__string_Err{
            _0: x2,
        }
        return t230
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t244 *_goml_vec_uint8
    var inline464 *_goml_vec_uint8 = data__10.values
    t244 = inline464
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t244)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    if x8 {
        var t247 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        return t247
    } else {
        var t248 Result__unit__string = Result__unit__string_Err{
            _0: x9,
        }
        return t248
    }
}

func main0() struct{} {
    var inline518 string = "goml-std-test.txt"
    var inline519 string = "std-ok"
    var inline520 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(inline519)
    _goml_m_std_p_fs_p_write__bytes(inline518, inline520)
    var t340 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t341 string
    switch t340.(type) {
    case Result__string__string_Ok:
        var inline512 string = t340.(Result__string__string_Ok)._0
        t341 = inline512
    case Result__string__string_Err:
        var inline514 string = t340.(Result__string__string_Err)._0
        var inline516 string = "err " + inline514
        t341 = inline516
    default:
        panic("non-exhaustive match")
    }
    var inline509 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t341)
    _goml_runtime_std_io_println(inline509)
    var t342 bool
    var inline506 string = "goml-std-test.txt"
    var inline507 bool = _goml_runtime_std_fs_file_exists(inline506)
    t342 = inline507
    var t343 string
    var inline504 string = _goml_runtime_core_bool_to_string(t342)
    t343 = inline504
    var inline501 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t343)
    _goml_runtime_std_io_println(inline501)
    var t344 _goml_m_Result____Vec_l_string_r_____string
    var inline490 string = "."
    var inline491 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(inline490)
    var inline492 bool = inline491._0
    var inline493 *_goml_vec_string = inline491._1
    var inline494 string = inline491._2
    if inline492 {
        var inline498 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: inline493,
        }
        t344 = inline498
    } else {
        var inline499 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: inline494,
        }
        t344 = inline499
    }
    var t345 string
    switch t344.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var inline481 *_goml_vec_string = t344.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var inline483 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(inline481)
        var inline484 bool = inline483 > 0
        var inline485 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline484)
        t345 = inline485
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var inline486 string = t344.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var inline488 string = "err " + inline486
        t345 = inline488
    default:
        panic("non-exhaustive match")
    }
    var inline478 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t345)
    _goml_runtime_std_io_println(inline478)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__137 *_goml_vec_string) int {
    var t386 int = vec_len__Vec_6string(self__137)
    return t386
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t389 string = _goml_runtime_core_bool_to_string(self__37)
    return t389
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
