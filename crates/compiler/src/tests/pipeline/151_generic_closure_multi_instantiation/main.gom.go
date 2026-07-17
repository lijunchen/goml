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
    var t66 string = int_pairer__2("a")
    println__T_string(t66)
    var t67 string = string_pairer__3("b")
    println__T_string(t67)
    return struct{}{}
}

func make_pairer__T_int32(x__0 int32) func(string) string {
    var retv69 func(string) string
    var t70 closure_env_make_pairer_T_int32_0 = closure_env_make_pairer_T_int32_0{
        x_0: x__0,
    }
    retv69 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h8cae0d704ec184429c6d982f53fd0781_nt32__0_i_apply(t70, p0)
    }
    return retv69
}

func make_pairer__T_string(x__0 string) func(string) string {
    var retv72 func(string) string
    var t73 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: x__0,
    }
    retv72 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(t73, p0)
    }
    return retv72
}

func println__T_string(value__1 string) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t75)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv78 string
    var t79 string = _goml_runtime_core_int32_to_string(self__41)
    retv78 = t79
    return retv78
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv81 string
    retv81 = self__37
    return retv81
}

func _goml_m_inherent_i_closure__en_h8cae0d704ec184429c6d982f53fd0781_nt32__0_i_apply(env63 closure_env_make_pairer_T_int32_0, tag__1 string) string {
    var retv83 string
    var x__0 int32 = env63.x_0
    var t84 string = tag__1 + ":"
    var t85 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__0)
    var t86 string = t84 + t85
    retv83 = t86
    return retv83
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env64 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var retv88 string
    var x__0 string = env64.x_0
    var t89 string = tag__1 + ":"
    var t90 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x__0)
    var t91 string = t89 + t90
    retv88 = t91
    return retv88
}

func main() {
    main0()
}
