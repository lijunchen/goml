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
    var t161 closure_env_main_0 = closure_env_main_0{}
    var text__6 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(first__4, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t161, p0)
    })
    var t162 string = text__6.value
    println__T_string(t162)
    var second__7 Box__int = Box__int{
        value: 7,
    }
    var t163 closure_env_main_1 = closure_env_main_1{}
    var explicit__9 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(second__7, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t163, p0)
    })
    var t164 string = explicit__9.value
    println__T_string(t164)
    var third__10 Box__int = Box__int{
        value: 9,
    }
    var t165 closure_env_main_2 = closure_env_main_2{}
    var static_call__12 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(third__10, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t165, p0)
    })
    var t166 string = static_call__12.value
    println__T_string(t166)
    var t167 Box__string = Box__string{
        value: "value",
    }
    var rendered__13 string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_render____T__string____U__int(t167, 5)
    println__T_string(rendered__13)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv169 string
    var t170 string = _goml_runtime_core_int_to_string(self__5)
    retv169 = t170
    return retv169
}

func _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(self__0 Box__int, map_fn__1 func(int) string) Box__string {
    var retv172 Box__string
    var t173 int = self__0.value
    var t174 string = map_fn__1(t173)
    var t175 Box__string = Box__string{
        value: t174,
    }
    retv172 = t175
    return retv172
}

func println__T_string(value__1 string) struct{} {
    var t177 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t177)
    return struct{}{}
}

func _goml_m_inherent_i_Box_i_Box_l_T_r__i_render____T__string____U__int(self__2 Box__string, value__3 int) string {
    var retv180 string
    var t181 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__3)
    var t182 string = "value:" + t181
    retv180 = t182
    return retv180
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv184 string
    retv184 = self__38
    return retv184
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv186 string
    var t187 string = _goml_runtime_core_int_to_string(self__40)
    retv186 = t187
    return retv186
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env157 closure_env_main_0, value__5 int) string {
    var retv189 string
    var t190 string = _goml_m_inherent_i_int_i_int_i_to__string(value__5)
    retv189 = t190
    return retv189
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env158 closure_env_main_1, value__8 int) string {
    var retv192 string
    var t193 string = _goml_m_inherent_i_int_i_int_i_to__string(value__8)
    retv192 = t193
    return retv192
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env159 closure_env_main_2, value__11 int) string {
    var retv195 string
    var t196 string = _goml_m_inherent_i_int_i_int_i_to__string(value__11)
    retv195 = t196
    return retv195
}

func main() {
    main0()
}
