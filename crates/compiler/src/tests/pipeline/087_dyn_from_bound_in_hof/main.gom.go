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
    var retv61 string
    var t62 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv61 = t62
    return retv61
}

func main0() struct{} {
    var f__3 func() string = mk_renderer__T_int32(42)
    var t64 string = f__3()
    println__T_string(t64)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv66 string
    var t67 string = _goml_runtime_core_int32_to_string(self__2)
    retv66 = t67
    return retv66
}

func mk_renderer__T_int32(x__1 int32) func() string {
    var retv69 func() string
    var d__2 dyn__Display = dyn__Display{
        data: int32(x__1),
        vtable: dyn__Display__vtable__int32(),
    }
    var t70 closure_env_mk_renderer_T_int32_0 = closure_env_mk_renderer_T_int32_0{
        d_0: d__2,
    }
    retv69 = func() string {
        return _goml_m_inherent_i_closure__en_hdbe3b8bac53e729aca2514d7798cc493_nt32__0_i_apply(t70)
    }
    return retv69
}

func println__T_string(value__1 string) struct{} {
    var t72 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t72)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv75 string
    retv75 = self__34
    return retv75
}

func _goml_m_inherent_i_closure__en_hdbe3b8bac53e729aca2514d7798cc493_nt32__0_i_apply(env59 closure_env_mk_renderer_T_int32_0) string {
    var retv77 string
    var d__2 dyn__Display = env59.d_0
    var t78 string = d__2.vtable.show(d__2.data)
    retv77 = t78
    return retv77
}

func main() {
    main0()
}
