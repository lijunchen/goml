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
    var t164 closure_env_main_0 = closure_env_main_0{}
    var text__6 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(first__4, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t164, p0)
    })
    var t165 string = text__6.value
    println__T_string(t165)
    var second__7 Box__int = Box__int{
        value: 7,
    }
    var t166 closure_env_main_1 = closure_env_main_1{}
    var explicit__9 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(second__7, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t166, p0)
    })
    var t167 string = explicit__9.value
    println__T_string(t167)
    var third__10 Box__int = Box__int{
        value: 9,
    }
    var t168 closure_env_main_2 = closure_env_main_2{}
    var static_call__12 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(third__10, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t168, p0)
    })
    var t169 string = static_call__12.value
    println__T_string(t169)
    var t170 Box__string = Box__string{
        value: "value",
    }
    var rendered__13 string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_render____T__string____U__int(t170, 5)
    println__T_string(rendered__13)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t173 string = _goml_runtime_core_int_to_string(self__5)
    return t173
}

func _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(self__0 Box__int, map_fn__1 func(int) string) Box__string {
    var t176 int = self__0.value
    var t177 string = map_fn__1(t176)
    var t178 Box__string = Box__string{
        value: t177,
    }
    return t178
}

func println__T_string(value__1 string) struct{} {
    var t180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t180)
    return struct{}{}
}

func _goml_m_inherent_i_Box_i_Box_l_T_r__i_render____T__string____U__int(self__2 Box__string, value__3 int) string {
    var t184 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__3)
    var t185 string = "value:" + t184
    return t185
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t190 string = _goml_runtime_core_int_to_string(self__40)
    return t190
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env160 closure_env_main_0, value__5 int) string {
    var t193 string = _goml_m_inherent_i_int_i_int_i_to__string(value__5)
    return t193
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env161 closure_env_main_1, value__8 int) string {
    var t196 string = _goml_m_inherent_i_int_i_int_i_to__string(value__8)
    return t196
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env162 closure_env_main_2, value__11 int) string {
    var t199 string = _goml_m_inherent_i_int_i_int_i_to__string(value__11)
    return t199
}

func main() {
    main0()
}
