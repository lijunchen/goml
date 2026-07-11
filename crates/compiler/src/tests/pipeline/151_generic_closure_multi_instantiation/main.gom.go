package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type closure_env_make_pairer_T_int32_0 struct {
    x_0 int32
}

type closure_env_make_pairer_T_string_1 struct {
    x_0 string
}

func main0() struct{} {
    var int_pairer__2 func(string) string = make_pairer__T_int32(7)
    var string_pairer__3 func(string) string = make_pairer__T_string("ok")
    var t12 string = int_pairer__2("a")
    println__T_string(t12)
    var t13 string = string_pairer__3("b")
    println__T_string(t13)
    return struct{}{}
}

func make_pairer__T_int32(x__0 int32) func(string) string {
    var retv15 func(string) string
    var t16 closure_env_make_pairer_T_int32_0 = closure_env_make_pairer_T_int32_0{
        x_0: x__0,
    }
    retv15 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h8cae0d704ec184429c6d982f53fd0781_nt32__0_i_apply(t16, p0)
    }
    return retv15
}

func make_pairer__T_string(x__0 string) func(string) string {
    var retv18 func(string) string
    var t19 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: x__0,
    }
    retv18 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(t19, p0)
    }
    return retv18
}

func println__T_string(value__1 string) struct{} {
    var t21 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t21)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv24 string
    var t25 string = _goml_runtime_core_int32_to_string(self__13)
    retv24 = t25
    return retv24
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv27 string
    retv27 = self__9
    return retv27
}

func _goml_m_inherent_i_closure__en_h8cae0d704ec184429c6d982f53fd0781_nt32__0_i_apply(env9 closure_env_make_pairer_T_int32_0, tag__1 string) string {
    var retv29 string
    var x__0 int32 = env9.x_0
    var t30 string = tag__1 + ":"
    var t31 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__0)
    var t32 string = t30 + t31
    retv29 = t32
    return retv29
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env10 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var retv34 string
    var x__0 string = env10.x_0
    var t35 string = tag__1 + ":"
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x__0)
    var t37 string = t35 + t36
    retv34 = t37
    return retv34
}

func main() {
    main0()
}
