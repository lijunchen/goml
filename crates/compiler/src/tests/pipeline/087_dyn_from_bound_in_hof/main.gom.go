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
    var retv64 string
    var t65 string = _goml_m_inherent_i_int32_i_int32_i_to__string(self__0)
    retv64 = t65
    return retv64
}

func main0() struct{} {
    var f__3 func() string = mk_renderer__T_int32(42)
    var t67 string = f__3()
    println__T_string(t67)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv69 string
    var t70 string = _goml_runtime_core_int32_to_string(self__5)
    retv69 = t70
    return retv69
}

func mk_renderer__T_int32(x__1 int32) func() string {
    var retv72 func() string
    var d__2 dyn__Display = dyn__Display{
        data: int32(x__1),
        vtable: dyn__Display__vtable__int32(),
    }
    var t73 closure_env_mk_renderer_T_int32_0 = closure_env_mk_renderer_T_int32_0{
        d_0: d__2,
    }
    retv72 = func() string {
        return _goml_m_inherent_i_closure__en_hdbe3b8bac53e729aca2514d7798cc493_nt32__0_i_apply(t73)
    }
    return retv72
}

func println__T_string(value__1 string) struct{} {
    var t75 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t75)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv78 string
    retv78 = self__37
    return retv78
}

func _goml_m_inherent_i_closure__en_hdbe3b8bac53e729aca2514d7798cc493_nt32__0_i_apply(env62 closure_env_mk_renderer_T_int32_0) string {
    var retv80 string
    var d__2 dyn__Display = env62.d_0
    var t81 string = d__2.vtable.show(d__2.data)
    retv80 = t81
    return retv80
}

func main() {
    main0()
}
