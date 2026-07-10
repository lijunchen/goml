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

func _goml_std_x3a__x3a_env_x3a__x3a_args() []string {
    var retv1 []string
    var t2 []string = _goml_std_x3a__x3a_env_x3a__x3a_args_x5f_raw()
    retv1 = t2
    return retv1
}

func main0() struct{} {
    var t4 []string = _goml_std_x3a__x3a_env_x3a__x3a_args()
    var t5 int32 = _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(t4)
    var t6 bool = t5 > 0
    var t7 string = bool_to_string(t6)
    _goml_std_x3a__x3a_io_x3a__x3a_println_x5f__x5f_T_x5f_string(t7)
    return struct{}{}
}

func _goml_std_x3a__x3a_io_x3a__x3a_println_x5f__x5f_T_x5f_string(value__1 string) struct{} {
    _goml_std_x3a__x3a_io_x3a__x3a_println_x5f_raw(value__1)
    return struct{}{}
}

func _goml_inherent_x23_Vec_x23_Vec_x5b_T_x5d__x23_len_x5f__x5f_T_x5f_string(self__73 []string) int32 {
    var retv12 int32
    var t13 int32 = int32(len(self__73))
    retv12 = t13
    return retv12
}

func main() {
    main0()
}
