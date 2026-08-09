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
    var t181 closure_env_main_0 = closure_env_main_0{}
    var text__6 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(first__4, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t181, p0)
    })
    var t182 string = text__6.value
    var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
    _goml_runtime_core_string_println(inline231)
    var second__7 Box__int = Box__int{
        value: 7,
    }
    var t183 closure_env_main_1 = closure_env_main_1{}
    var explicit__9 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(second__7, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t183, p0)
    })
    var t184 string = explicit__9.value
    var inline228 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t184)
    _goml_runtime_core_string_println(inline228)
    var third__10 Box__int = Box__int{
        value: 9,
    }
    var t185 closure_env_main_2 = closure_env_main_2{}
    var static_call__12 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(third__10, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t185, p0)
    })
    var t186 string = static_call__12.value
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t186)
    _goml_runtime_core_string_println(inline225)
    var rendered__13 string
    var inline221 int = 5
    var inline222 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline221)
    var inline223 string = "value:" + inline222
    rendered__13 = inline223
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(rendered__13)
    _goml_runtime_core_string_println(inline218)
    return struct{}{}
}

func _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(self__0 Box__int, map_fn__1 func(int) string) Box__string {
    var t193 int = self__0.value
    var t194 string = map_fn__1(t193)
    var t195 Box__string = Box__string{
        value: t194,
    }
    return t195
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t207 string = _goml_runtime_core_int_to_string(self__69)
    return t207
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env177 closure_env_main_0, value__5 int) string {
    var inline237 string = _goml_runtime_core_int_to_string(value__5)
    return inline237
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env178 closure_env_main_1, value__8 int) string {
    var inline239 string = _goml_runtime_core_int_to_string(value__8)
    return inline239
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env179 closure_env_main_2, value__11 int) string {
    var inline241 string = _goml_runtime_core_int_to_string(value__11)
    return inline241
}

func main() {
    main0()
}
