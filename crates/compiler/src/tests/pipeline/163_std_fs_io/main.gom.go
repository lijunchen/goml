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

func _goml_runtime_std_fs_read_file(path string) Tuple3_4bool_6string_6string {
    var data []uint8
    var err error
    data, err = _goml_os.ReadFile(path)
    if err != nil {
        return Tuple3_4bool_6string_6string{
            _0: false,
            _1: "",
            _2: err.Error(),
        }
    }
    return Tuple3_4bool_6string_6string{
        _0: true,
        _1: string(data),
        _2: "",
    }
}

func _goml_runtime_std_fs_write_file(path string, content string) Tuple2_4bool_6string {
    var err error = _goml_os.WriteFile(path, []byte(content), 0644)
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

type _goml_vec_string struct {
    items []string
}

func vec_len__Vec_6string(vec *_goml_vec_string) int32 {
    return int32(len(vec.items))
}

type Tuple3_4bool_6string_6string struct {
    _0 bool
    _1 string
    _2 string
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_11Vec_6string_6string struct {
    _0 bool
    _1 *_goml_vec_string
    _2 string
}

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

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var retv67 Result__string__string
    var mtmp0 Tuple3_4bool_6string_6string = _goml_runtime_std_fs_read_file(path__0)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var x3 string = mtmp0._2
    var err__3 string = x3
    var data__2 string = x2
    var ok__1 bool = x1
    var jp69 Result__string__string
    if ok__1 {
        var t70 Result__string__string = Result__string__string_Ok{
            _0: data__2,
        }
        jp69 = t70
    } else {
        var t71 Result__string__string = Result__string__string_Err{
            _0: err__3,
        }
        jp69 = t71
    }
    retv67 = jp69
    return retv67
}

func _goml_m_std_p_fs_p_write__file(path__4 string, content__5 string) Result__unit__string {
    var retv73 Result__unit__string
    var mtmp4 Tuple2_4bool_6string = _goml_runtime_std_fs_write_file(path__4, content__5)
    var x5 bool = mtmp4._0
    var x6 string = mtmp4._1
    var err__7 string = x6
    var ok__6 bool = x5
    var jp75 Result__unit__string
    if ok__6 {
        var t76 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp75 = t76
    } else {
        var t77 Result__unit__string = Result__unit__string_Err{
            _0: err__7,
        }
        jp75 = t77
    }
    retv73 = jp75
    return retv73
}

func _goml_m_std_p_fs_p_exists(path__11 string) bool {
    var retv85 bool
    var t86 bool = _goml_runtime_std_fs_file_exists(path__11)
    retv85 = t86
    return retv85
}

func _goml_m_std_p_fs_p_read__dir(path__12 string) _goml_m_Result____Vec_l_string_r_____string {
    var retv88 _goml_m_Result____Vec_l_string_r_____string
    var mtmp10 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__12)
    var x11 bool = mtmp10._0
    var x12 *_goml_vec_string = mtmp10._1
    var x13 string = mtmp10._2
    var err__15 string = x13
    var names__14 *_goml_vec_string = x12
    var ok__13 bool = x11
    var jp90 _goml_m_Result____Vec_l_string_r_____string
    if ok__13 {
        var t91 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: names__14,
        }
        jp90 = t91
    } else {
        var t92 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: err__15,
        }
        jp90 = t92
    }
    retv88 = jp90
    return retv88
}

func show_read(res__0 Result__string__string) string {
    var retv94 string
    var jp96 string
    switch res__0.(type) {
    case Result__string__string_Ok:
        var x58 string = res__0.(Result__string__string_Ok)._0
        var value__1 string = x58
        jp96 = value__1
    case Result__string__string_Err:
        var x59 string = res__0.(Result__string__string_Err)._0
        var err__2 string = x59
        var t97 string = "err " + err__2
        jp96 = t97
    default:
        panic("non-exhaustive match")
    }
    retv94 = jp96
    return retv94
}

func show_dir(res__3 _goml_m_Result____Vec_l_string_r_____string) string {
    var retv99 string
    var jp101 string
    switch res__3.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var x60 *_goml_vec_string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var names__4 *_goml_vec_string = x60
        var t102 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(names__4)
        var t103 bool = t102 > 0
        var t104 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t103)
        jp101 = t104
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var x61 string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var err__5 string = x61
        var t105 string = "err " + err__5
        jp101 = t105
    default:
        panic("non-exhaustive match")
    }
    retv99 = jp101
    return retv99
}

func main0() struct{} {
    _goml_m_std_p_fs_p_write__file("goml-std-test.txt", "std-ok")
    var t107 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t108 string = show_read(t107)
    _goml_m_std_p_io_p_println____T__string(t108)
    var t109 bool = _goml_m_std_p_fs_p_exists("goml-std-test.txt")
    var t110 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t109)
    _goml_m_std_p_io_p_println____T__string(t110)
    var t111 _goml_m_Result____Vec_l_string_r_____string = _goml_m_std_p_fs_p_read__dir(".")
    var t112 string = show_dir(t111)
    _goml_m_std_p_io_p_println____T__string(t112)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__131 *_goml_vec_string) int32 {
    var retv114 int32
    var t115 int32 = vec_len__Vec_6string(self__131)
    retv114 = t115
    return retv114
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv117 string
    var t118 string = _goml_runtime_core_bool_to_string(self__33)
    retv117 = t118
    return retv117
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t120 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t120)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv123 string
    retv123 = self__34
    return retv123
}

func main() {
    main0()
}
