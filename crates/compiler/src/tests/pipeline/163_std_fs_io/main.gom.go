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
    var i int32 = 0
    for {
        if i >= int32(len(entries)) {
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

func vec_len__Vec_6string(vec *_goml_vec_string) int32 {
    return int32(len(vec.items))
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
    var retv78 _goml_m_std_p_bytes_p_Bytes
    var t79 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: values__1,
    }
    retv78 = t79
    return retv78
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(value__2 string) _goml_m_std_p_bytes_p_Bytes {
    var retv81 _goml_m_std_p_bytes_p_Bytes
    var t82 *_goml_vec_uint8 = _goml_m_inherent_i_string_i_string_i_to__bytes(value__2)
    var t83 _goml_m_std_p_bytes_p_Bytes = _goml_m_std_p_bytes_p_Bytes{
        values: t82,
    }
    retv81 = t83
    return retv81
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(self__22 _goml_m_std_p_bytes_p_Bytes) *_goml_vec_uint8 {
    var retv124 *_goml_vec_uint8
    var t125 *_goml_vec_uint8 = self__22.values
    retv124 = t125
    return retv124
}

func _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(self__23 _goml_m_std_p_bytes_p_Bytes) Result__string__string {
    var retv127 Result__string__string
    var t128 *_goml_vec_uint8 = self__23.values
    var mtmp0 Tuple2_4bool_6string = _goml_runtime_core_string_from_utf8(t128)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var value__25 string = x2
    var valid__24 bool = x1
    var jp130 Result__string__string
    if valid__24 {
        var t131 Result__string__string = Result__string__string_Ok{
            _0: value__25,
        }
        jp130 = t131
    } else {
        var t132 Result__string__string = Result__string__string_Err{
            _0: "invalid UTF-8",
        }
        jp130 = t132
    }
    retv127 = jp130
    return retv127
}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var retv134 Result__string__string
    var mtmp0 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_std_p_fs_p_read__bytes(path__0)
    var jp136 Result__string__string
    switch mtmp0.(type) {
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Ok:
        var x1 _goml_m_std_p_bytes_p_Bytes = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Ok)._0
        var data__1 _goml_m_std_p_bytes_p_Bytes = x1
        var t137 Result__string__string = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__string(data__1)
        jp136 = t137
    case _goml_m_Result____std_p_bytes_p_Bytes____string_Err:
        var x2 string = mtmp0.(_goml_m_Result____std_p_bytes_p_Bytes____string_Err)._0
        var error__2 string = x2
        var t138 Result__string__string = Result__string__string_Err{
            _0: error__2,
        }
        jp136 = t138
    default:
        panic("non-exhaustive match")
    }
    retv134 = jp136
    return retv134
}

func _goml_m_std_p_fs_p_write__file(path__3 string, content__4 string) Result__unit__string {
    var retv140 Result__unit__string
    var t141 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__string(content__4)
    var t142 Result__unit__string = _goml_m_std_p_fs_p_write__bytes(path__3, t141)
    retv140 = t142
    return retv140
}

func _goml_m_std_p_fs_p_read__bytes(path__5 string) _goml_m_Result____std_p_bytes_p_Bytes____string {
    var retv144 _goml_m_Result____std_p_bytes_p_Bytes____string
    var mtmp3 Tuple3_4bool_10Vec_5uint8_6string = _goml_runtime_std_fs_read_bytes(path__5)
    var x4 bool = mtmp3._0
    var x5 *_goml_vec_uint8 = mtmp3._1
    var x6 string = mtmp3._2
    var err__8 string = x6
    var data__7 *_goml_vec_uint8 = x5
    var ok__6 bool = x4
    var jp146 _goml_m_Result____std_p_bytes_p_Bytes____string
    if ok__6 {
        var t147 _goml_m_std_p_bytes_p_Bytes = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_from__vec(data__7)
        var t148 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Ok{
            _0: t147,
        }
        jp146 = t148
    } else {
        var t149 _goml_m_Result____std_p_bytes_p_Bytes____string = _goml_m_Result____std_p_bytes_p_Bytes____string_Err{
            _0: err__8,
        }
        jp146 = t149
    }
    retv144 = jp146
    return retv144
}

func _goml_m_std_p_fs_p_write__bytes(path__9 string, data__10 _goml_m_std_p_bytes_p_Bytes) Result__unit__string {
    var retv151 Result__unit__string
    var t152 *_goml_vec_uint8 = _goml_m_inherent_i_std_p_bytes_p_Bytes_i_std_p_bytes_p_Bytes_i_to__vec(data__10)
    var mtmp7 Tuple2_4bool_6string = _goml_runtime_std_fs_write_bytes(path__9, t152)
    var x8 bool = mtmp7._0
    var x9 string = mtmp7._1
    var err__12 string = x9
    var ok__11 bool = x8
    var jp154 Result__unit__string
    if ok__11 {
        var t155 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp154 = t155
    } else {
        var t156 Result__unit__string = Result__unit__string_Err{
            _0: err__12,
        }
        jp154 = t156
    }
    retv151 = jp154
    return retv151
}

func _goml_m_std_p_fs_p_exists(path__16 string) bool {
    var retv164 bool
    var t165 bool = _goml_runtime_std_fs_file_exists(path__16)
    retv164 = t165
    return retv164
}

func _goml_m_std_p_fs_p_read__dir(path__23 string) _goml_m_Result____Vec_l_string_r_____string {
    var retv179 _goml_m_Result____Vec_l_string_r_____string
    var mtmp17 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__23)
    var x18 bool = mtmp17._0
    var x19 *_goml_vec_string = mtmp17._1
    var x20 string = mtmp17._2
    var err__26 string = x20
    var names__25 *_goml_vec_string = x19
    var ok__24 bool = x18
    var jp181 _goml_m_Result____Vec_l_string_r_____string
    if ok__24 {
        var t182 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: names__25,
        }
        jp181 = t182
    } else {
        var t183 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: err__26,
        }
        jp181 = t183
    }
    retv179 = jp181
    return retv179
}

func show_read(res__0 Result__string__string) string {
    var retv206 string
    var jp208 string
    switch res__0.(type) {
    case Result__string__string_Ok:
        var x61 string = res__0.(Result__string__string_Ok)._0
        var value__1 string = x61
        jp208 = value__1
    case Result__string__string_Err:
        var x62 string = res__0.(Result__string__string_Err)._0
        var err__2 string = x62
        var t209 string = "err " + err__2
        jp208 = t209
    default:
        panic("non-exhaustive match")
    }
    retv206 = jp208
    return retv206
}

func show_dir(res__3 _goml_m_Result____Vec_l_string_r_____string) string {
    var retv211 string
    var jp213 string
    switch res__3.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var x63 *_goml_vec_string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var names__4 *_goml_vec_string = x63
        var t214 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(names__4)
        var t215 bool = t214 > 0
        var t216 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t215)
        jp213 = t216
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var x64 string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var err__5 string = x64
        var t217 string = "err " + err__5
        jp213 = t217
    default:
        panic("non-exhaustive match")
    }
    retv211 = jp213
    return retv211
}

func main0() struct{} {
    _goml_m_std_p_fs_p_write__file("goml-std-test.txt", "std-ok")
    var t219 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t220 string = show_read(t219)
    _goml_m_std_p_io_p_println____T__string(t220)
    var t221 bool = _goml_m_std_p_fs_p_exists("goml-std-test.txt")
    var t222 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t221)
    _goml_m_std_p_io_p_println____T__string(t222)
    var t223 _goml_m_Result____Vec_l_string_r_____string = _goml_m_std_p_fs_p_read__dir(".")
    var t224 string = show_dir(t223)
    _goml_m_std_p_io_p_println____T__string(t224)
    return struct{}{}
}

func _goml_m_inherent_i_string_i_string_i_to__bytes(self__23 string) *_goml_vec_uint8 {
    var retv232 *_goml_vec_uint8
    var t233 *_goml_vec_uint8 = _goml_runtime_core_string_to_bytes(self__23)
    retv232 = t233
    return retv232
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__134 *_goml_vec_string) int32 {
    var retv269 int32
    var t270 int32 = vec_len__Vec_6string(self__134)
    retv269 = t270
    return retv269
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv272 string
    var t273 string = _goml_runtime_core_bool_to_string(self__36)
    retv272 = t273
    return retv272
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t275)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv278 string
    retv278 = self__37
    return retv278
}

func main() {
    main0()
}
