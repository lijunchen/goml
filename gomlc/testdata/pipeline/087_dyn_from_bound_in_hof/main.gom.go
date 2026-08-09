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
    var inline195 string = _goml_runtime_core_int32_to_string(self__0)
    return inline195
}

func main0() struct{} {
    var value__3 int32 = 42
    var f__4 func() string
    var inline200 dyn__Display = dyn__Display{
        data: int32(value__3),
        vtable: dyn__Display__vtable__int32(),
    }
    var inline201 closure_env_mk_renderer_T_int32_0 = closure_env_mk_renderer_T_int32_0{
        d_0: inline200,
    }
    var inline202 func() string = func() string {
        return _goml_m_inherent_i_closure__en_hdbe3b8bac53e729aca2514d7798cc493_nt32__0_i_apply(inline201)
    }
    f__4 = inline202
    var t178 string = f__4()
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
    _goml_runtime_core_string_println(inline197)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__en_hdbe3b8bac53e729aca2514d7798cc493_nt32__0_i_apply(env173 closure_env_mk_renderer_T_int32_0) string {
    var d__2 dyn__Display = env173.d_0
    var t193 string = d__2.vtable.show(d__2.data)
    return t193
}

func main() {
    main0()
}
