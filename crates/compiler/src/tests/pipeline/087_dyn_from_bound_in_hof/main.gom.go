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
    var retv25 string
    var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv25 = t26
    return retv25
}

func main0() struct{} {
    var f__3 func() string = mk_renderer__T_int32(42)
    var t28 string = f__3()
    println__T_string(t28)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv30 string
    var t31 string = _goml_runtime_core_int32_to_string(self__2)
    retv30 = t31
    return retv30
}

func mk_renderer__T_int32(x__1 int32) func() string {
    var retv33 func() string
    var d__2 dyn__Display = dyn__Display{
        data: int32(x__1),
        vtable: dyn__Display__vtable__int32(),
    }
    var t34 closure_env_mk_renderer_T_int32_0 = closure_env_mk_renderer_T_int32_0{
        d_0: d__2,
    }
    retv33 = func() string {
        return _goml_m_inherent_i_closure__en_hdbe3b8bac53e729aca2514d7798cc493_nt32__0_i_apply(t34)
    }
    return retv33
}

func println__T_string(value__1 string) struct{} {
    var t36 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t36)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv39 string
    retv39 = self__9
    return retv39
}

func _goml_m_inherent_i_closure__en_hdbe3b8bac53e729aca2514d7798cc493_nt32__0_i_apply(env23 closure_env_mk_renderer_T_int32_0) string {
    var retv41 string
    var d__2 dyn__Display = env23.d_0
    var t42 string = d__2.vtable.show(d__2.data)
    retv41 = t42
    return retv41
}

func main() {
    main0()
}
