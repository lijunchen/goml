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
    var t63 string = int_pairer__2("a")
    println__T_string(t63)
    var t64 string = string_pairer__3("b")
    println__T_string(t64)
    return struct{}{}
}

func make_pairer__T_int32(x__0 int32) func(string) string {
    var retv66 func(string) string
    var t67 closure_env_make_pairer_T_int32_0 = closure_env_make_pairer_T_int32_0{
        x_0: x__0,
    }
    retv66 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h8cae0d704ec184429c6d982f53fd0781_nt32__0_i_apply(t67, p0)
    }
    return retv66
}

func make_pairer__T_string(x__0 string) func(string) string {
    var retv69 func(string) string
    var t70 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: x__0,
    }
    retv69 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(t70, p0)
    }
    return retv69
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__38 int32) string {
    var retv75 string
    var t76 string = _goml_runtime_core_int32_to_string(self__38)
    retv75 = t76
    return retv75
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv78 string
    retv78 = self__34
    return retv78
}

func _goml_m_inherent_i_closure__en_h8cae0d704ec184429c6d982f53fd0781_nt32__0_i_apply(env60 closure_env_make_pairer_T_int32_0, tag__1 string) string {
    var retv80 string
    var x__0 int32 = env60.x_0
    var t81 string = tag__1 + ":"
    var t82 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__0)
    var t83 string = t81 + t82
    retv80 = t83
    return retv80
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env61 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var retv85 string
    var x__0 string = env61.x_0
    var t86 string = tag__1 + ":"
    var t87 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x__0)
    var t88 string = t86 + t87
    retv85 = t88
    return retv85
}

func main() {
    main0()
}
