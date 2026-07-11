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
    var retv5 *_goml_vec_string
    var t6 *_goml_vec_string = _goml_runtime_std_env_args()
    retv5 = t6
    return retv5
}

func main0() struct{} {
    var t8 *_goml_vec_string = _goml_m_std_p_env_p_args()
    var t9 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t8)
    var t10 bool = t9 > 0
    var t11 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t10)
    _goml_m_std_p_io_p_println____T__string(t11)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t14 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t14)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__82 *_goml_vec_string) int32 {
    var retv17 int32
    var t18 int32 = vec_len__Vec_6string(self__82)
    retv17 = t18
    return retv17
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv20 string
    var t21 string = _goml_runtime_core_bool_to_string(self__8)
    retv20 = t21
    return retv20
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv23 string
    retv23 = self__9
    return retv23
}

func main() {
    main0()
}
