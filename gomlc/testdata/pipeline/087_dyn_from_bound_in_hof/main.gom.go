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

type closure_env_mk_renderer_T_int32_0 struct {
    d_0 dyn__Display
}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_int32_i_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Display_i_int32_i_show(self__0 int32) string {
    var retv111 string
    var t112 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv111 = t112
    return retv111
}

func main0() struct{} {
    var value__3 int32 = 42
    var f__4 func() string = mk_renderer__T_int32(value__3)
    var t114 string = f__4()
    println__T_string(t114)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv116 string
    var t117 string = _goml_runtime_core_int32_to_string(self__6)
    retv116 = t117
    return retv116
}

func mk_renderer__T_int32(x__1 int32) func() string {
    var retv119 func() string
    var d__2 dyn__Display = dyn__Display{
        data: int32(x__1),
        vtable: dyn__Display__vtable__int32(),
    }
    var t120 closure_env_mk_renderer_T_int32_0 = closure_env_mk_renderer_T_int32_0{
        d_0: d__2,
    }
    retv119 = func() string {
        return _goml_m_inherent_i_closure__en_hdbe3b8bac53e729aca2514d7798cc493_nt32__0_i_apply(t120)
    }
    return retv119
}

func println__T_string(value__1 string) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t122)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv125 string
    retv125 = self__38
    return retv125
}

func _goml_m_inherent_i_closure__en_hdbe3b8bac53e729aca2514d7798cc493_nt32__0_i_apply(env109 closure_env_mk_renderer_T_int32_0) string {
    var retv127 string
    var d__2 dyn__Display = env109.d_0
    var t128 string = d__2.vtable.show(d__2.data)
    retv127 = t128
    return retv127
}

func main() {
    main0()
}
