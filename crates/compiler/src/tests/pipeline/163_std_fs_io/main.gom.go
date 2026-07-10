package main

import (
    _goml_fmt "fmt"
    _goml_os "os"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_m_std_p_fs_p_read__file__raw(path string) Tuple3_4bool_6string_6string {
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

func _goml_m_std_p_fs_p_write__file__raw(path string, content string) Tuple2_4bool_6string {
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

func _goml_m_std_p_fs_p_file__exists__raw(path string) bool {
    var err error
    _, err = _goml_os.Stat(path)
    return err == nil
}

func _goml_m_std_p_fs_p_read__dir__raw(path string) Tuple3_4bool_11Vec_6string_6string {
    var entries []_goml_os.DirEntry
    var err error
    entries, err = _goml_os.ReadDir(path)
    if err != nil {
        return Tuple3_4bool_11Vec_6string_6string{
            _0: false,
            _1: nil,
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
        _1: names,
        _2: "",
    }
}

func _goml_m_std_p_io_p_println__raw(value string) struct{} {
    _goml_fmt.Println(value)
    return struct{}{}
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
    _1 []string
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
    _0 []string
}

func (_ _goml_m_Result____Vec_l_string_r_____string_Ok) is_goml_m_Result____Vec_l_string_r_____string() {}

type _goml_m_Result____Vec_l_string_r_____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____Vec_l_string_r_____string_Err) is_goml_m_Result____Vec_l_string_r_____string() {}

func _goml_m_std_p_fs_p_read__file(path__0 string) Result__string__string {
    var retv9 Result__string__string
    var mtmp0 Tuple3_4bool_6string_6string = _goml_m_std_p_fs_p_read__file__raw(path__0)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var x3 string = mtmp0._2
    var err__3 string = x3
    var data__2 string = x2
    var ok__1 bool = x1
    var jp11 Result__string__string
    if ok__1 {
        var t12 Result__string__string = Result__string__string_Ok{
            _0: data__2,
        }
        jp11 = t12
    } else {
        var t13 Result__string__string = Result__string__string_Err{
            _0: err__3,
        }
        jp11 = t13
    }
    retv9 = jp11
    return retv9
}

func _goml_m_std_p_fs_p_write__file(path__4 string, content__5 string) Result__unit__string {
    var retv15 Result__unit__string
    var mtmp4 Tuple2_4bool_6string = _goml_m_std_p_fs_p_write__file__raw(path__4, content__5)
    var x5 bool = mtmp4._0
    var x6 string = mtmp4._1
    var err__7 string = x6
    var ok__6 bool = x5
    var jp17 Result__unit__string
    if ok__6 {
        var t18 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp17 = t18
    } else {
        var t19 Result__unit__string = Result__unit__string_Err{
            _0: err__7,
        }
        jp17 = t19
    }
    retv15 = jp17
    return retv15
}

func _goml_m_std_p_fs_p_exists(path__11 string) bool {
    var retv27 bool
    var t28 bool = _goml_m_std_p_fs_p_file__exists__raw(path__11)
    retv27 = t28
    return retv27
}

func _goml_m_std_p_fs_p_read__dir(path__12 string) _goml_m_Result____Vec_l_string_r_____string {
    var retv30 _goml_m_Result____Vec_l_string_r_____string
    var mtmp10 Tuple3_4bool_11Vec_6string_6string = _goml_m_std_p_fs_p_read__dir__raw(path__12)
    var x11 bool = mtmp10._0
    var x12 []string = mtmp10._1
    var x13 string = mtmp10._2
    var err__15 string = x13
    var names__14 []string = x12
    var ok__13 bool = x11
    var jp32 _goml_m_Result____Vec_l_string_r_____string
    if ok__13 {
        var t33 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Ok{
            _0: names__14,
        }
        jp32 = t33
    } else {
        var t34 _goml_m_Result____Vec_l_string_r_____string = _goml_m_Result____Vec_l_string_r_____string_Err{
            _0: err__15,
        }
        jp32 = t34
    }
    retv30 = jp32
    return retv30
}

func show_read(res__0 Result__string__string) string {
    var retv36 string
    var jp38 string
    switch res__0.(type) {
    case Result__string__string_Ok:
        var x0 string = res__0.(Result__string__string_Ok)._0
        var value__1 string = x0
        jp38 = value__1
    case Result__string__string_Err:
        var x1 string = res__0.(Result__string__string_Err)._0
        var err__2 string = x1
        var t39 string = "err " + err__2
        jp38 = t39
    default:
        panic("non-exhaustive match")
    }
    retv36 = jp38
    return retv36
}

func show_dir(res__3 _goml_m_Result____Vec_l_string_r_____string) string {
    var retv41 string
    var jp43 string
    switch res__3.(type) {
    case _goml_m_Result____Vec_l_string_r_____string_Ok:
        var x2 []string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Ok)._0
        var names__4 []string = x2
        var t44 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(names__4)
        var t45 bool = t44 > 0
        var t46 string = bool_to_string(t45)
        jp43 = t46
    case _goml_m_Result____Vec_l_string_r_____string_Err:
        var x3 string = res__3.(_goml_m_Result____Vec_l_string_r_____string_Err)._0
        var err__5 string = x3
        var t47 string = "err " + err__5
        jp43 = t47
    default:
        panic("non-exhaustive match")
    }
    retv41 = jp43
    return retv41
}

func main0() struct{} {
    _goml_m_std_p_fs_p_write__file("goml-std-test.txt", "std-ok")
    var t49 Result__string__string = _goml_m_std_p_fs_p_read__file("goml-std-test.txt")
    var t50 string = show_read(t49)
    _goml_m_std_p_io_p_println____T__string(t50)
    var t51 bool = _goml_m_std_p_fs_p_exists("goml-std-test.txt")
    var t52 string = bool_to_string(t51)
    _goml_m_std_p_io_p_println____T__string(t52)
    var t53 _goml_m_Result____Vec_l_string_r_____string = _goml_m_std_p_fs_p_read__dir(".")
    var t54 string = show_dir(t53)
    _goml_m_std_p_io_p_println____T__string(t54)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__73 []string) int32 {
    var retv56 int32
    var t57 int32 = int32(len(self__73))
    retv56 = t57
    return retv56
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    _goml_m_std_p_io_p_println__raw(value__1)
    return struct{}{}
}

func main() {
    main0()
}
