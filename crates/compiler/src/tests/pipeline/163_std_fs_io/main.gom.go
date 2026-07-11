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
    var retv31 Result__string__string
    var mtmp0 Tuple3_4bool_6string_6string = _goml_runtime_std_fs_read_file(path__0)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var x3 string = mtmp0._2
    var err__3 string = x3
    var data__2 string = x2
    var ok__1 bool = x1
    var jp33 Result__string__string
    if ok__1 {
        var t34 Result__string__string = Result__string__string_Ok{
            _0: data__2,
        }
        jp33 = t34
    } else {
        var t35 Result__string__string = Result__string__string_Err{
            _0: err__3,
        }
        jp33 = t35
    }
    retv31 = jp33
    return retv31
}

func _goml_m_std_p_fs_p_write__file(path__4 string, content__5 string) Result__unit__string {
    var retv37 Result__unit__string
    var mtmp4 Tuple2_4bool_6string = _goml_runtime_std_fs_write_file(path__4, content__5)
    var x5 bool = mtmp4._0
    var x6 string = mtmp4._1
    var err__7 string = x6
    var ok__6 bool = x5
    var jp39 Result__unit__string
    if ok__6 {
        var t40 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp39 = t40
    } else {
        var t41 Result__unit__string = Result__unit__string_Err{
            _0: err__7,
        }
        jp39 = t41
    }
    retv37 = jp39
    return retv37
}

func _goml_m_std_p_fs_p_exists(path__11 string) bool {
    var retv49 bool
    var t50 bool = _goml_runtime_std_fs_file_exists(path__11)
    retv49 = t50
    return retv49
}

func _goml_m_std_p_fs_p_read__dir(path__12 string) _goml_m_Result____Vec_l_string_r_____string {
    var retv52 _goml_m_Result____Vec_l_string_r_____string
    var mtmp10 Tuple3_4bool_11Vec_6string_6string = _goml_runtime_std_fs_read_dir(path__12)
    var x11 bool = mtmp10._0
    var x12 *_goml_vec_string = mtmp10._1
    var x13 string = mtmp10._2
    var err__15 string = x13
    var names__14 *_goml_vec_string = x12
    var ok__13 bool = x11
    var jp54 _goml_m_Result____Vec_l_string_r_____string
    if ok__13 {
        var t55 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: names__14,
        }
        jp54 = t55
    } else {
        var t56 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: err__15,
        }
        jp54 = t56
    }
    retv52 = jp54
    return retv52
}

func show_read(res__0 Result__string__string) string {
    var retv58 string
    var jp60 string
    switch res__0.(type) {
    case Result__string__string_Ok:
        var x22 string = res__0.(Result__string__string_Ok)._0
        var value__1 string = x22
        jp60 = value__1
    case Result__string__string_Err:
        var x23 string = res__0.(Result__string__string_Err)._0
        var err__2 string = x23
        var t61 string = "err " + err__2
        jp60 = t61
    default:
        panic("non-exhaustive match")
    }
    retv58 = jp60
    return retv58
}

func show_dir(res__3 _goml_m_Result____Vec_l_string_r_____string) string {
    var retv63 string
    var jp65 string
    switch res__3.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var x24 *_goml_vec_string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var names__4 *_goml_vec_string = x24
        var t66 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(names__4)
        var t67 bool = t66 > 0
        var t68 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t67)
        jp65 = t68
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var x25 string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var err__5 string = x25
        var t69 string = "err " + err__5
        jp65 = t69
    default:
        panic("non-exhaustive match")
    }
    retv63 = jp65
    return retv63
}

func main0() struct{} {
    _goml_m_std_p_fs_p_write__file("goml-std-test.txt", "std-ok")
    var t71 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t72 string = show_read(t71)
    _goml_m_std_p_io_p_println____T__string(t72)
    var t73 bool = _goml_m_std_p_fs_p_exists("goml-std-test.txt")
    var t74 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t73)
    _goml_m_std_p_io_p_println____T__string(t74)
    var t75 _goml_m_Result____Vec_l_string_r_____string = _goml_m_std_p_fs_p_read__dir(".")
    var t76 string = show_dir(t75)
    _goml_m_std_p_io_p_println____T__string(t76)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__107 *_goml_vec_string) int32 {
    var retv78 int32
    var t79 int32 = vec_len__Vec_6string(self__107)
    retv78 = t79
    return retv78
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv81 string
    var t82 string = _goml_runtime_core_bool_to_string(self__8)
    retv81 = t82
    return retv81
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t84 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t84)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv87 string
    retv87 = self__9
    return retv87
}

func main() {
    main0()
}
