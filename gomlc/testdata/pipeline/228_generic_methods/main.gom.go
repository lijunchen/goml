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
    var t145 closure_env_main_0 = closure_env_main_0{}
    var text__6 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(first__4, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t145, p0)
    })
    var t146 string = text__6.value
    var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t146)
    _goml_runtime_core_string_println(inline196)
    var second__7 Box__int = Box__int{
        value: 7,
    }
    var t147 closure_env_main_1 = closure_env_main_1{}
    var explicit__9 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(second__7, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t147, p0)
    })
    var t148 string = explicit__9.value
    var inline193 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t148)
    _goml_runtime_core_string_println(inline193)
    var third__10 Box__int = Box__int{
        value: 9,
    }
    var t149 closure_env_main_2 = closure_env_main_2{}
    var static_call__12 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(third__10, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t149, p0)
    })
    var t150 string = static_call__12.value
    var inline190 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t150)
    _goml_runtime_core_string_println(inline190)
    var rendered__13 string
    var inline185 int = 5
    var inline187 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline185)
    var inline188 string = "value:" + inline187
    rendered__13 = inline188
    var inline182 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(rendered__13)
    _goml_runtime_core_string_println(inline182)
    return struct{}{}
}

func _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(self__0 Box__int, map_fn__1 func(int) string) Box__string {
    var t157 int = self__0.value
    var t158 string = map_fn__1(t157)
    var t159 Box__string = Box__string{
        value: t158,
    }
    return t159
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t171 string = _goml_runtime_core_int_to_string(self__69)
    return t171
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env141 closure_env_main_0, value__5 int) string {
    var inline202 string = _goml_runtime_core_int_to_string(value__5)
    return inline202
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env142 closure_env_main_1, value__8 int) string {
    var inline204 string = _goml_runtime_core_int_to_string(value__8)
    return inline204
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env143 closure_env_main_2, value__11 int) string {
    var inline206 string = _goml_runtime_core_int_to_string(value__11)
    return inline206
}

func main() {
    main0()
}
