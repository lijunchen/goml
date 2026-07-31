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
    var retv168 _goml_m_std_p_bytes_p_Bytes
    var t169 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    retv168 = t169
    return retv168
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var retv171 _goml_m_std_p_bytes_p_Bytes
    var t172 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__2)
    var t173 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t172,
    }
    retv171 = t173
    return retv171
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(self__22 _goml_m_std_p_bytes_p_Bytes) *_goml_vec_uint8 {
    var retv213 *_goml_vec_uint8
    var t214 *_goml_vec_uint8 = self__22.values
    retv213 = t214
    return retv213
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var retv216 Result__string__string
    var t217 *_goml_vec_uint8 = self__23.values
    var mtmp6 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t217)
    var x7 bool = mtmp6._0
    var x8 string = mtmp6._1
    var value__25 string = x8
    var valid__24 bool = x7
    var jp219 Result__string__string
    if valid__24 {
        var t220 Result__string__string = Result__string__string_Ok{
            _0: value__25,
        }
        jp219 = t220
    } else {
        var t221 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        jp219 = t221
    }
    retv216 = jp219
    return retv216
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var retv223 Result__string__string
    var mtmp0 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(path__0)
    var jp225 Result__string__string
    switch mtmp0.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var x1 _goml_m_std_p_bytes_p_Bytes = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var data__1 _goml_m_std_p_bytes_p_Bytes = x1
        var t226 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(data__1)
        jp225 = t226
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var x2 string = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var error__2 string = x2
        var t227 Result__string__string = Result__string__string_Err{
            _0: error__2,
        }
        jp225 = t227
    default:
        panic("non-exhaustive match")
    }
    retv223 = jp225
    return retv223
}

func _goml_m_std_p_fs_p_write__file(path__3 string, content__4 string) Result__unit__string {
    var retv229 Result__unit__string
    var t230 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(content__4)
    var t231 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(path__3, t230)
    retv229 = t231
    return retv229
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var retv233 _goml_m_Result____std_p_bytes_p_Bytes____string
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    var err__8 string = x6
    var data__7 *_goml_vec_uint8 = x5
    var ok__6 bool = x4
    var jp235 _goml_m_Result____std_p_bytes_p_Bytes____string
    if ok__6 {
        var t236 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(data__7)
        var t237 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t236,
        }
        jp235 = t237
    } else {
        var t238 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: err__8,
        }
        jp235 = t238
    }
    retv233 = jp235
    return retv233
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var retv240 Result__unit__string
    var t241 *_goml_vec_uint8 = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(data__10)
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t241)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    var err__12 string = x9
    var ok__11 bool = x8
    var jp243 Result__unit__string
    if ok__11 {
        var t244 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp243 = t244
    } else {
        var t245 Result__unit__string = Result__unit__string_Err{
            _0: err__12,
        }
        jp243 = t245
    }
    retv240 = jp243
    return retv240
}

func _goml_m_std_p_fs_p_exists(path__22 string) bool {
    var retv265 bool
    var t266 bool = _goml_runtime_std_fs_file_exists(path__22)
    retv265 = t266
    return retv265
}

func _goml_m_std_p_fs_p_read__dir(path__29 string) _goml_m_Result____Vec_l_string_r_____string {
    var retv280 _goml_m_Result____Vec_l_string_r_____string
    var mtmp23 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__29)
    var x24 bool = mtmp23._0
    var x25 *_goml_vec_string = mtmp23._1
    var x26 string = mtmp23._2
    var err__32 string = x26
    var names__31 *_goml_vec_string = x25
    var ok__30 bool = x24
    var jp282 _goml_m_Result____Vec_l_string_r_____string
    if ok__30 {
        var t283 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: names__31,
        }
        jp282 = t283
    } else {
        var t284 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: err__32,
        }
        jp282 = t284
    }
    retv280 = jp282
    return retv280
}

func show_read(res__0 Result__string__string) string {
    var retv324 string
    var jp326 string
    switch res__0.(type) {
    case Result__string__string_Ok:
        var x152 string = res__0.(Result__string__string_Ok)._0
        var value__1 string = x152
        jp326 = value__1
    case Result__string__string_Err:
        var x153 string = res__0.(Result__string__string_Err)._0
        var err__2 string = x153
        var t327 string = "err " + err__2
        jp326 = t327
    default:
        panic("non-exhaustive match")
    }
    retv324 = jp326
    return retv324
}

func show_dir(res__3 _goml_m_Result____Vec_l_string_r_____string) string {
    var retv329 string
    var jp331 string
    switch res__3.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var x154 *_goml_vec_string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var names__4 *_goml_vec_string = x154
        var t332 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(names__4)
        var t333 bool = t332 > 0
        var t334 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t333)
        jp331 = t334
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var x155 string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var err__5 string = x155
        var t335 string = "err " + err__5
        jp331 = t335
    default:
        panic("non-exhaustive match")
    }
    retv329 = jp331
    return retv329
}

func main0() struct{} {
    _goml_m_std_p_fs_p_write__file("goml-std-test.txt", "std-ok")
    var t337 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t338 string = show_read(t337)
    _goml_m_std_p_io_p_println____T__string(t338)
    var t339 bool = _goml_m_std_p_fs_p_exists("goml-std-test.txt")
    var t340 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t339)
    _goml_m_std_p_io_p_println____T__string(t340)
    var t341 _goml_m_Result____Vec_l_string_r_____string = _goml_m_std_p_fs_p_read__dir(".")
    var t342 string = show_dir(t341)
    _goml_m_std_p_io_p_println____T__string(t342)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__24 string) *_goml_vec_uint8 {
    var retv350 *_goml_vec_uint8
    var t351 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__24)
    retv350 = t351
    return retv350
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__137 *_goml_vec_string) int {
    var retv382 int
    var t383 int = vec_len__Vec_6string(self__137)
    retv382 = t383
    return retv382
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv385 string
    var t386 string = _goml_runtime_core_bool_to_string(self__37)
    retv385 = t386
    return retv385
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t388 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t388)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv394 string
    retv394 = self__38
    return retv394
}

func main() {
    main0()
}
