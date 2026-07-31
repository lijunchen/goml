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
    var retv169 _goml_m_std_p_bytes_p_Bytes
    var t170 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    retv169 = t170
    return retv169
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var retv172 _goml_m_std_p_bytes_p_Bytes
    var t173 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__2)
    var t174 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t173,
    }
    retv172 = t174
    return retv172
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(self__22 _goml_m_std_p_bytes_p_Bytes) *_goml_vec_uint8 {
    var retv214 *_goml_vec_uint8
    var t215 *_goml_vec_uint8 = self__22.values
    retv214 = t215
    return retv214
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var retv217 Result__string__string
    var t218 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t218)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    var value__25 string = x8
    var valid__24 bool = x7
    var jp220 Result__string__string
    if valid__24 {
        var t221 Result__string__string = Result__string__string_Ok{
            _0: value__25,
        }
        jp220 = t221
    } else {
        var t222 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        jp220 = t222
    }
    retv217 = jp220
    return retv217
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var retv224 Result__string__string
    var mtmp0 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(path__0)
    var jp226 Result__string__string
    switch mtmp0.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var x1 _goml_m_std_p_bytes_p_Bytes = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var data__1 _goml_m_std_p_bytes_p_Bytes = x1
        var t227 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(data__1)
        jp226 = t227
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var x2 string = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var error__2 string = x2
        var t228 Result__string__string = Result__string__string_Err{
            _0: error__2,
        }
        jp226 = t228
    default:
        panic("non-exhaustive match")
    }
    retv224 = jp226
    return retv224
}

func _goml_m_std_p_fs_p_write__file(path__3 string, content__4 string) Result__unit__string {
    var retv230 Result__unit__string
    var t231 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(content__4)
    var t232 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(path__3, t231)
    retv230 = t232
    return retv230
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var retv234 _goml_m_Result____std_p_bytes_p_Bytes____string
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    var err__8 string = x6
    var data__7 *_goml_vec_uint8 = x5
    var ok__6 bool = x4
    var jp236 _goml_m_Result____std_p_bytes_p_Bytes____string
    if ok__6 {
        var t237 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(data__7)
        var t238 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t237,
        }
        jp236 = t238
    } else {
        var t239 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: err__8,
        }
        jp236 = t239
    }
    retv234 = jp236
    return retv234
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var retv241 Result__unit__string
    var t242 *_goml_vec_uint8 = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(data__10)
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t242)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    var err__12 string = x9
    var ok__11 bool = x8
    var jp244 Result__unit__string
    if ok__11 {
        var t245 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp244 = t245
    } else {
        var t246 Result__unit__string = Result__unit__string_Err{
            _0: err__12,
        }
        jp244 = t246
    }
    retv241 = jp244
    return retv241
}

func _goml_m_std_p_fs_p_exists(path__22 string) bool {
    var retv266 bool
    var t267 bool = _goml_runtime_std_fs_file_exists(path__22)
    retv266 = t267
    return retv266
}

func _goml_m_std_p_fs_p_read__dir(path__29 string) _goml_m_Result____Vec_l_string_r_____string {
    var retv281 _goml_m_Result____Vec_l_string_r_____string
    var mtmp23 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__29)
    var x24 bool = mtmp23._0
    var x25 *_goml_vec_string = mtmp23._1
    var x26 string = mtmp23._2
    var err__32 string = x26
    var names__31 *_goml_vec_string = x25
    var ok__30 bool = x24
    var jp283 _goml_m_Result____Vec_l_string_r_____string
    if ok__30 {
        var t284 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: names__31,
        }
        jp283 = t284
    } else {
        var t285 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: err__32,
        }
        jp283 = t285
    }
    retv281 = jp283
    return retv281
}

func show_read(res__0 Result__string__string) string {
    var retv325 string
    var jp327 string
    switch res__0.(type) {
    case Result__string__string_Ok:
        var x152 string = res__0.(Result__string__string_Ok)._0
        var value__1 string = x152
        jp327 = value__1
    case Result__string__string_Err:
        var x153 string = res__0.(Result__string__string_Err)._0
        var err__2 string = x153
        var t328 string = "err " + err__2
        jp327 = t328
    default:
        panic("non-exhaustive match")
    }
    retv325 = jp327
    return retv325
}

func show_dir(res__3 _goml_m_Result____Vec_l_string_r_____string) string {
    var retv330 string
    var jp332 string
    switch res__3.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var x154 *_goml_vec_string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var names__4 *_goml_vec_string = x154
        var t333 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(names__4)
        var t334 bool = t333 > 0
        var t335 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t334)
        jp332 = t335
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var x155 string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var err__5 string = x155
        var t336 string = "err " + err__5
        jp332 = t336
    default:
        panic("non-exhaustive match")
    }
    retv330 = jp332
    return retv330
}

func main0() struct{} {
    _goml_m_std_p_fs_p_write__file("goml-std-test.txt", "std-ok")
    var t338 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t339 string = show_read(t338)
    _goml_m_std_p_io_p_println____T__string(t339)
    var t340 bool = _goml_m_std_p_fs_p_exists("goml-std-test.txt")
    var t341 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t340)
    _goml_m_std_p_io_p_println____T__string(t341)
    var t342 _goml_m_Result____Vec_l_string_r_____string = _goml_m_std_p_fs_p_read__dir(".")
    var t343 string = show_dir(t342)
    _goml_m_std_p_io_p_println____T__string(t343)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var retv351 *_goml_vec_uint8
    var t352 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    retv351 = t352
    return retv351
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__137 *_goml_vec_string) int {
    var retv383 int
    var t384 int = vec_len__Vec_6string(self__137)
    retv383 = t384
    return retv383
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv386 string
    var t387 string = _goml_runtime_core_bool_to_string(self__37)
    retv386 = t387
    return retv386
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t389 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t389)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv395 string
    retv395 = self__38
    return retv395
}

func main() {
    main0()
}
