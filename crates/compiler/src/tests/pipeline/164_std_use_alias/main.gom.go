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
    var retv59 *_goml_vec_string
    var t60 *_goml_vec_string = _goml_runtime_std_env_args()
    retv59 = t60
    return retv59
}

func main0() struct{} {
    var t62 *_goml_vec_string = _goml_m_std_p_env_p_args()
    var t63 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t62)
    var t64 bool = t63 > 0
    var t65 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t64)
    _goml_m_std_p_io_p_println____T__string(t65)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t68 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t68)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__131 *_goml_vec_string) int32 {
    var retv71 int32
    var t72 int32 = vec_len__Vec_6string(self__131)
    retv71 = t72
    return retv71
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__33 bool) string {
    var retv74 string
    var t75 string = _goml_runtime_core_bool_to_string(self__33)
    retv74 = t75
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv77 string
    retv77 = self__34
    return retv77
}

func main() {
    main0()
}
