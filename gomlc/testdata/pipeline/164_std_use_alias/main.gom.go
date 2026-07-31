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

type _goml_vec_uint8 struct {
    items []uint8
}

type _goml_vec_string struct {
    items []string
}

func vec_len__Vec_6string(vec *_goml_vec_string) int {
    return int(len(vec.items))
}

type Tuple2_4bool_6string struct {
    _0 bool
    _1 string
}

type Tuple3_4bool_6string_6string struct {
    _0 bool
    _1 string
    _2 string
}

type Tuple3_4bool_10Vec_5uint8_6string struct {
    _0 bool
    _1 *_goml_vec_uint8
    _2 string
}

type _goml_m_std_p_bytes_p_Bytes struct {
    values *_goml_vec_uint8
}

type Option__uint8 interface {
    isOption__uint8()
}

type Option__uint8_None struct {}

func (_ Option__uint8_None) isOption__uint8() {}

type Option__uint8_Some struct {
    _0 uint8
}

func (_ Option__uint8_Some) isOption__uint8() {}

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

type Option__string interface {
    isOption__string()
}

type Option__string_None struct {}

func (_ Option__string_None) isOption__string() {}

type Option__string_Some struct {
    _0 string
}

func (_ Option__string_Some) isOption__string() {}

type _goml_m_Result____std_p_bytes_p_Bytes____string interface {
    is_goml_m_Result____std_p_bytes_p_Bytes____string()
}

type _goml_m_Result____std_p_bytes_p_Bytes____string_Ok struct {
    _0 _goml_m_std_p_bytes_p_Bytes
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____string_Ok) is_goml_m_Result____std_p_bytes_p_Bytes____string() {}

type _goml_m_Result____std_p_bytes_p_Bytes____string_Err struct {
    _0 string
}

func (_ _goml_m_Result____std_p_bytes_p_Bytes____string_Err) is_goml_m_Result____std_p_bytes_p_Bytes____string() {}

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

func _goml_m_std_p_env_p_args() *_goml_vec_string {
    var retv215 *_goml_vec_string
    var t216 *_goml_vec_string = _goml_runtime_std_env_args()
    retv215 = t216
    return retv215
}

func main0() struct{} {
    var t267 *_goml_vec_string = _goml_m_std_p_env_p_args()
    var t268 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(t267)
    var t269 bool = t268 > 0
    var t270 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t269)
    _goml_m_std_p_io_p_println____T__string(t270)
    return struct{}{}
}

func _goml_m_std_p_io_p_println____T__string(value__1 string) struct{} {
    var t311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_std_io_println(t311)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__string(self__137 *_goml_vec_string) int {
    var retv314 int
    var t315 int = vec_len__Vec_6string(self__137)
    retv314 = t315
    return retv314
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv317 string
    var t318 string = _goml_runtime_core_bool_to_string(self__37)
    retv317 = t318
    return retv317
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv323 string
    retv323 = self__38
    return retv323
}

func main() {
    main0()
}
