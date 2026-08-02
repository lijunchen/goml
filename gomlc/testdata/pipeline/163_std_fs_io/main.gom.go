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
    var t175 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__2)
    var t176 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t175,
    }
    return t176
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(self__22 _goml_m_std_p_bytes_p_Bytes) *_goml_vec_uint8 {
    var t217 *_goml_vec_uint8 = self__22.values
    return t217
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var t220 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t220)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    if x7 {
        var t223 Result__string__string = Result__string__string_Ok{
            _0: x8,
        }
        return t223
    } else {
        var t224 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        return t224
    }
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var mtmp0 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(path__0)
    switch mtmp0.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var x1 _goml_m_std_p_bytes_p_Bytes = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var t229 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(x1)
        return t229
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

func _goml_m_std_p_fs_p_write__file(path__3 string, content__4 string) Result__unit__string {
    var t233 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(content__4)
    var t234 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(path__3, t233)
    return t234
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    if x4 {
        var t239 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(x5)
        var t240 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t239,
        }
        return t240
    } else {
        var t241 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: x6,
        }
        return t241
    }
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var t244 *_goml_vec_uint8 = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(data__10)
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

func _goml_m_std_p_fs_p_exists(path__22 string) bool {
    var t269 bool = _goml_runtime_std_fs_file_exists(path__22)
    return t269
}

func _goml_m_std_p_fs_p_read__dir(path__29 string) _goml_m_Result____Vec_l_string_r_____string {
    var mtmp23 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__29)
    var x24 bool = mtmp23._0
    var x25 *_goml_vec_string = mtmp23._1
    var x26 string = mtmp23._2
    if x24 {
        var t286 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: x25,
        }
        return t286
    } else {
        var t287 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: x26,
        }
        return t287
    }
}

func show_read(res__0 Result__string__string) string {
    switch res__0.(type) {
    case Result__string__string_Ok:
        var x155 string = res__0.(Result__string__string_Ok)._0
        return x155
    case Result__string__string_Err:
        var x156 string = res__0.(Result__string__string_Err)._0
        var t330 string = "err " + x156
        return t330
    default:
        panic("non-exhaustive match")
    }
}

func show_dir(res__3 _goml_m_Result____Vec_l_string_r_____string) string {
    switch res__3.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var x157 *_goml_vec_string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var t335 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(x157)
        var t336 bool = t335 > 0
        var t337 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t336)
        return t337
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var x158 string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var t338 string = "err " + x158
        return t338
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    _goml_m_std_p_fs_p_write__file("goml-std-test.txt", "std-ok")
    var t340 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t341 string = show_read(t340)
    _goml_m_std_p_io_p_println____T__string(t341)
    var t342 bool = _goml_m_std_p_fs_p_exists("goml-std-test.txt")
    var t343 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t342)
    _goml_m_std_p_io_p_println____T__string(t343)
    var t344 _goml_m_Result____Vec_l_string_r_____string = _goml_m_std_p_fs_p_read__dir(".")
    var t345 string = show_dir(t344)
    _goml_m_std_p_io_p_println____T__string(t345)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var t354 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    return t354
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__137 *_goml_vec_string) int {
    var t386 int = vec_len__Vec_6string(self__137)
    return t386
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t389 string = _goml_runtime_core_bool_to_string(self__37)
    return t389
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t391 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t391)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
