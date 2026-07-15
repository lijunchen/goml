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
    var t27 string = int_pairer__2("a")
    println__T_string(t27)
    var t28 string = string_pairer__3("b")
    println__T_string(t28)
    return struct{}{}
}

func make_pairer__T_int32(x__0 int32) func(string) string {
    var retv30 func(string) string
    var t31 closure_env_make_pairer_T_int32_0 = closure_env_make_pairer_T_int32_0{
        x_0: x__0,
    }
    retv30 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h8cae0d704ec184429c6d982f53fd0781_nt32__0_i_apply(t31, p0)
    }
    return retv30
}

func make_pairer__T_string(x__0 string) func(string) string {
    var retv33 func(string) string
    var t34 closure_env_make_pairer_T_string_1 = closure_env_make_pairer_T_string_1{
        x_0: x__0,
    }
    retv33 = func(p0 string) string {
        return _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(t34, p0)
    }
    return retv33
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv39 string
    var t40 string = _goml_runtime_core_int32_to_string(self__13)
    retv39 = t40
    return retv39
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv42 string
    retv42 = self__9
    return retv42
}

func _goml_m_inherent_i_closure__en_h8cae0d704ec184429c6d982f53fd0781_nt32__0_i_apply(env24 closure_env_make_pairer_T_int32_0, tag__1 string) string {
    var retv44 string
    var x__0 int32 = env24.x_0
    var t45 string = tag__1 + ":"
    var t46 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x__0)
    var t47 string = t45 + t46
    retv44 = t47
    return retv44
}

func _goml_m_inherent_i_closure__en_h0926de79f2e12e90a6ce0e3536cf7f8d_ring__1_i_apply(env25 closure_env_make_pairer_T_string_1, tag__1 string) string {
    var retv49 string
    var x__0 string = env25.x_0
    var t50 string = tag__1 + ":"
    var t51 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(x__0)
    var t52 string = t50 + t51
    retv49 = t52
    return retv49
}

func main() {
    main0()
}
