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

func _goml_runtime_std_env_args() *_goml_vec_string {
    return &_goml_vec_string{
        items: _goml_os.Args,
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

func _goml_m_std_p_env_p_args() *_goml_vec_string {
    var retv8 *_goml_vec_string
    var t9 *_goml_vec_string = _goml_runtime_std_env_args()
    retv8 = t9
    return retv8
}

func main0() struct{} {
    var t11 *_goml_vec_string = _goml_m_std_p_env_p_args()
    var t12 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t11)
    var t13 bool = t12 > 0
    var t14 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t13)
    _goml_m_std_p_io_p_println____T__string(t14)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t17 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t17)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__84 *_goml_vec_string) int32 {
    var retv20 int32
    var t21 int32 = vec_len__Vec_6string(self__84)
    retv20 = t21
    return retv20
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv23 string
    var t24 string = _goml_runtime_core_bool_to_string(self__8)
    retv23 = t24
    return retv23
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv26 string
    retv26 = self__9
    return retv26
}

func main() {
    main0()
}
