package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Box__int struct {
    value int
}

type Box__string struct {
    value string
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {}

type closure_env_main_2 struct {}

func main0() struct{} {
    var first__4 Box__int = Box__int{
        value: 42,
    }
    var t186 closure_env_main_0 = closure_env_main_0{}
    var text__6 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(first__4, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t186, p0)
    })
    var t187 string = text__6.value
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t187)
    _goml_runtime_core_string_println(inline237)
    var second__7 Box__int = Box__int{
        value: 7,
    }
    var t188 closure_env_main_1 = closure_env_main_1{}
    var explicit__9 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(second__7, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t188, p0)
    })
    var t189 string = explicit__9.value
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t189)
    _goml_runtime_core_string_println(inline234)
    var third__10 Box__int = Box__int{
        value: 9,
    }
    var t190 closure_env_main_2 = closure_env_main_2{}
    var static_call__12 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(third__10, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t190, p0)
    })
    var t191 string = static_call__12.value
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t191)
    _goml_runtime_core_string_println(inline231)
    var rendered__13 string
    var inline226 int = 5
    var inline228 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline226)
    var inline229 string = "value:" + inline228
    rendered__13 = inline229
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(rendered__13)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(self__0 Box__int, map_fn__1 func(int) string) Box__string {
    var t198 int = self__0.value
    var t199 string = map_fn__1(t198)
    var t200 Box__string = Box__string{
        value: t199,
    }
    return t200
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t212 string = _goml_runtime_core_int_to_string(self__69)
    return t212
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env182 closure_env_main_0, value__5 int) string {
    var inline243 string = _goml_runtime_core_int_to_string(value__5)
    return inline243
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env183 closure_env_main_1, value__8 int) string {
    var inline245 string = _goml_runtime_core_int_to_string(value__8)
    return inline245
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env184 closure_env_main_2, value__11 int) string {
    var inline247 string = _goml_runtime_core_int_to_string(value__11)
    return inline247
}

func main() {
    main0()
}
