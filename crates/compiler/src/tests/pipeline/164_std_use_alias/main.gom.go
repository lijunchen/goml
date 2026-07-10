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

func _goml_m_std_p_env_p_args__raw() []string {
    return _goml_os.Args
}

func _goml_m_std_p_io_p_println__raw(value string) struct{} {
    _goml_fmt.Println(value)
    return struct{}{}
}

func _goml_m_std_p_env_p_args() []string {
    var retv1 []string
    var t2 []string = _goml_m_std_p_env_p_args__raw()
    retv1 = t2
    return retv1
}

func main0() struct{} {
    var t4 []string = _goml_m_std_p_env_p_args()
    var t5 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t4)
    var t6 bool = t5 > 0
    var t7 string = bool_to_string(t6)
    _goml_m_std_p_io_p_println____T__string(t7)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    _goml_m_std_p_io_p_println__raw(value__1)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__73 []string) int32 {
    var retv12 int32
    var t13 int32 = int32(len(self__73))
    retv12 = t13
    return retv12
}

func main() {
    main0()
}
