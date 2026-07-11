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
    var retv23 *_goml_vec_string
    var t24 *_goml_vec_string = _goml_runtime_std_env_args()
    retv23 = t24
    return retv23
}

func main0() struct{} {
    var t26 *_goml_vec_string = _goml_m_std_p_env_p_args()
    var t27 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t26)
    var t28 bool = t27 > 0
    var t29 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t28)
    _goml_m_std_p_io_p_println____T__string(t29)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t32 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t32)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__107 *_goml_vec_string) int32 {
    var retv35 int32
    var t36 int32 = vec_len__Vec_6string(self__107)
    retv35 = t36
    return retv35
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv38 string
    var t39 string = _goml_runtime_core_bool_to_string(self__8)
    retv38 = t39
    return retv38
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv41 string
    retv41 = self__9
    return retv41
}

func main() {
    main0()
}
