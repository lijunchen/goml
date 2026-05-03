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

func _goml_std_x3a__x3a_fs_x3a__x3a_read_x5f_file_x5f_raw(path string) Tuple3_4bool_6string_6string {
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

func _goml_std_x3a__x3a_fs_x3a__x3a_write_x5f_file_x5f_raw(path string, content string) Tuple2_4bool_6string {
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

func _goml_std_x3a__x3a_fs_x3a__x3a_file_x5f_exists_x5f_raw(path string) bool {
    var err error
    _, err = _goml_os.Stat(path)
    return err == nil
}

func _goml_std_x3a__x3a_fs_x3a__x3a_read_x5f_dir_x5f_raw(path string) Tuple3_4bool_11Vec_6string_6string {
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

func _goml_std_x3a__x3a_io_x3a__x3a_println_x5f_raw(value string) struct{} {
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

type _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string interface {
    is_goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string()
}

type _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string_Ok struct {
    _0 []string
}

func (_ _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string_Ok) is_goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string() {}

type _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string_Err struct {
    _0 string
}

func (_ _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string_Err) is_goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string() {}

func _goml_std_x3a__x3a_fs_x3a__x3a_read_x5f_file(path__0 string) Result__string__string {
    var retv12 Result__string__string
    var mtmp0 Tuple3_4bool_6string_6string = _goml_std_x3a__x3a_fs_x3a__x3a_read_x5f_file_x5f_raw(path__0)
    var x1 bool = mtmp0._0
    var x2 string = mtmp0._1
    var x3 string = mtmp0._2
    var err__3 string = x3
    var data__2 string = x2
    var ok__1 bool = x1
    var jp14 Result__string__string
    if ok__1 {
        var t15 Result__string__string = Result__string__string_Ok{
            _0: data__2,
        }
        jp14 = t15
    } else {
        var t16 Result__string__string = Result__string__string_Err{
            _0: err__3,
        }
        jp14 = t16
    }
    retv12 = jp14
    return retv12
}

func _goml_std_x3a__x3a_fs_x3a__x3a_write_x5f_file(path__4 string, content__5 string) Result__unit__string {
    var retv18 Result__unit__string
    var mtmp4 Tuple2_4bool_6string = _goml_std_x3a__x3a_fs_x3a__x3a_write_x5f_file_x5f_raw(path__4, content__5)
    var x5 bool = mtmp4._0
    var x6 string = mtmp4._1
    var err__7 string = x6
    var ok__6 bool = x5
    var jp20 Result__unit__string
    if ok__6 {
        var t21 Result__unit__string = Result__unit__string_Ok{
            _0: struct{}{},
        }
        jp20 = t21
    } else {
        var t22 Result__unit__string = Result__unit__string_Err{
            _0: err__7,
        }
        jp20 = t22
    }
    retv18 = jp20
    return retv18
}

func _goml_std_x3a__x3a_fs_x3a__x3a_exists(path__8 string) bool {
    var retv24 bool
    var t25 bool = _goml_std_x3a__x3a_fs_x3a__x3a_file_x5f_exists_x5f_raw(path__8)
    retv24 = t25
    return retv24
}

func _goml_std_x3a__x3a_fs_x3a__x3a_read_x5f_dir(path__9 string) _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string {
    var retv27 _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string
    var mtmp7 Tuple3_4bool_11Vec_6string_6string = _goml_std_x3a__x3a_fs_x3a__x3a_read_x5f_dir_x5f_raw(path__9)
    var x8 bool = mtmp7._0
    var x9 []string = mtmp7._1
    var x10 string = mtmp7._2
    var err__12 string = x10
    var names__11 []string = x9
    var ok__10 bool = x8
    var jp29 _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string
    if ok__10 {
        var t30 _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string = _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string_Ok{
            _0: names__11,
        }
        jp29 = t30
    } else {
        var t31 _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string = _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string_Err{
            _0: err__12,
        }
        jp29 = t31
    }
    retv27 = jp29
    return retv27
}

func show_read(res__0 Result__string__string) string {
    var retv35 string
    var jp37 string
    switch res__0.(type) {
    case Result__string__string_Ok:
        var x0 string = res__0.(Result__string__string_Ok)._0
        var value__1 string = x0
        jp37 = value__1
    case Result__string__string_Err:
        var x1 string = res__0.(Result__string__string_Err)._0
        var err__2 string = x1
        var t38 string = "err " + err__2
        jp37 = t38
    default:
        panic("non-exhaustive match")
    }
    retv35 = jp37
    return retv35
}

func show_dir(res__3 _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string) string {
    var retv40 string
    var jp42 string
    switch res__3.(type) {
    case _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string_Ok:
        var x2 []string = res__3.(_goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string_Ok)._0
        var names__4 []string = x2
        var t43 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(names__4)
        var t44 bool = t43 > 0
        var t45 string = bool_to_string(t44)
        jp42 = t45
    case _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string_Err:
        var x3 string = res__3.(_goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string_Err)._0
        var err__5 string = x3
        var t46 string = "err " + err__5
        jp42 = t46
    default:
        panic("non-exhaustive match")
    }
    retv40 = jp42
    return retv40
}

func main0() struct{} {
    _goml_std_x3a__x3a_fs_x3a__x3a_write_x5f_file("goml-std-test.txt", "std-ok")
    var t48 Result__string__string = _goml_std_x3a__x3a_fs_x3a__x3a_read_x5f_file("goml-std-test.txt")
    var t49 string = show_read(t48)
    _goml_std_x3a__x3a_io_x3a__x3a_println_x5f__x5f_T_x5f_string(t49)
    var t50 bool = _goml_std_x3a__x3a_fs_x3a__x3a_exists("goml-std-test.txt")
    var t51 string = bool_to_string(t50)
    _goml_std_x3a__x3a_io_x3a__x3a_println_x5f__x5f_T_x5f_string(t51)
    var t52 _goml_Result_x5f__x5f_Vec_x5b_string_x5d__x5f__x5f_string = _goml_std_x3a__x3a_fs_x3a__x3a_read_x5f_dir(".")
    var t53 string = show_dir(t52)
    _goml_std_x3a__x3a_io_x3a__x3a_println_x5f__x5f_T_x5f_string(t53)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(self__73 []string) int32 {
    var retv55 int32
    var t56 int32 = int32(len(self__73))
    retv55 = t56
    return retv55
}

func _goml_std_x3a__x3a_io_x3a__x3a_println_x5f__x5f_T_x5f_string(value__1 string) struct{} {
    _goml_std_x3a__x3a_io_x3a__x3a_println_x5f_raw(value__1)
    return struct{}{}
}

func main() {
    main0()
}
