package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
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
    return _goml_trait_impl_Display_int32_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_trait_impl_Display_int32_show(self__0 int32) string {
    var t2 string = int32_to_string(self__0)
    return t2
}

func main0() struct{} {
    var f__3 closure_env_mk_renderer_T_int32_0 = mk_renderer__T_int32(42)
    var t3 string = _goml_inherent_closure_env_mk_renderer_T_int32_0_closure_env_mk_renderer_T_int32_0_apply(f__3)
    println__T_string(t3)
    return struct{}{}
}

func mk_renderer__T_int32(x__1 int32) closure_env_mk_renderer_T_int32_0 {
    var d__2 dyn__Display = dyn__Display{
        data: x__1,
        vtable: dyn__Display__vtable__int32(),
    }
    var t4 closure_env_mk_renderer_T_int32_0 = closure_env_mk_renderer_T_int32_0{
        d_0: d__2,
    }
    return t4
}

func println__T_string(value__1 string) struct{} {
    var t5 struct{} = string_println(value__1)
    return t5
}

func _goml_inherent_closure_env_mk_renderer_T_int32_0_closure_env_mk_renderer_T_int32_0_apply(env1 closure_env_mk_renderer_T_int32_0) string {
    var d__2 dyn__Display = env1.d_0
    var t6 string = d__2.vtable.show(d__2.data)
    return t6
}

func main() {
    main0()
}
