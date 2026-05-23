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

func _goml_std_x3a__x3a_env_x3a__x3a_args_x5f_raw() []string {
    return _goml_os.Args
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

func _goml_std_x3a__x3a_env_x3a__x3a_args() []string {
    var retv1 []string
    var t2 []string = _goml_std_x3a__x3a_env_x3a__x3a_args_x5f_raw()
    retv1 = t2
    return retv1
}

func main0() struct{} {
    var t27 []string = _goml_std_x3a__x3a_env_x3a__x3a_args()
    var t28 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(t27)
    var t29 bool = t28 > 0
    var t30 string = bool_to_string(t29)
    _goml_std_x3a__x3a_io_x3a__x3a_println_x5f__x5f_T_x5f_string(t30)
    return struct{}{}
}

func _goml_std_x3a__x3a_io_x3a__x3a_println_x5f__x5f_T_x5f_string(value__1 string) struct{} {
    _goml_std_x3a__x3a_io_x3a__x3a_println_x5f_raw(value__1)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(self__73 []string) int32 {
    var retv35 int32
    var t36 int32 = int32(len(self__73))
    retv35 = t36
    return retv35
}

func main() {
    main0()
}
