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
    var t117 closure_env_main_0 = closure_env_main_0{}
    var text__6 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(first__4, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t117, p0)
    })
    var t118 string = text__6.value
    println__T_string(t118)
    var second__7 Box__int = Box__int{
        value: 7,
    }
    var t119 closure_env_main_1 = closure_env_main_1{}
    var explicit__9 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(second__7, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t119, p0)
    })
    var t120 string = explicit__9.value
    println__T_string(t120)
    var third__10 Box__int = Box__int{
        value: 9,
    }
    var t121 closure_env_main_2 = closure_env_main_2{}
    var static_call__12 Box__string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(third__10, func(p0 int) string {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t121, p0)
    })
    var t122 string = static_call__12.value
    println__T_string(t122)
    var t123 Box__string = Box__string{
        value: "value",
    }
    var rendered__13 string = _goml_m_inherent_i_Box_i_Box_l_T_r__i_render____T__string____U__int(t123, 5)
    println__T_string(rendered__13)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv125 string
    var t126 string = _goml_runtime_core_int_to_string(self__5)
    retv125 = t126
    return retv125
}

func _goml_m_inherent_i_Box_i_Box_l_T_r__i_map____T__int____U__string(self__0 Box__int, map_fn__1 func(int) string) Box__string {
    var retv128 Box__string
    var t129 int = self__0.value
    var t130 string = map_fn__1(t129)
    var t131 Box__string = Box__string{
        value: t130,
    }
    retv128 = t131
    return retv128
}

func println__T_string(value__1 string) struct{} {
    var t133 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t133)
    return struct{}{}
}

func _goml_m_inherent_i_Box_i_Box_l_T_r__i_render____T__string____U__int(self__2 Box__string, value__3 int) string {
    var retv136 string
    var t137 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__3)
    var t138 string = "value:" + t137
    retv136 = t138
    return retv136
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv140 string
    retv140 = self__38
    return retv140
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv142 string
    var t143 string = _goml_runtime_core_int_to_string(self__40)
    retv142 = t143
    return retv142
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env113 closure_env_main_0, value__5 int) string {
    var retv145 string
    var t146 string = _goml_m_inherent_i_int_i_int_i_to__string(value__5)
    retv145 = t146
    return retv145
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env114 closure_env_main_1, value__8 int) string {
    var retv148 string
    var t149 string = _goml_m_inherent_i_int_i_int_i_to__string(value__8)
    retv148 = t149
    return retv148
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env115 closure_env_main_2, value__11 int) string {
    var retv151 string
    var t152 string = _goml_m_inherent_i_int_i_int_i_to__string(value__11)
    retv151 = t152
    return retv151
}

func main() {
    main0()
}
