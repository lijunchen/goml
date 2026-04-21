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

type GoError = error

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
    var retv3 string
    var t4 string = int32_to_string(self__0)
    retv3 = t4
    return retv3
}

func main0() struct{} {
    var f__3 func() string = mk_renderer__T_int32(42)
    var t6 string = f__3()
    println__T_string(t6)
    return struct{}{}
}

func mk_renderer__T_int32(x__1 int32) func() string {
    var retv8 func() string
    var d__2 dyn__Display = dyn__Display{
        data: int32(x__1),
        vtable: dyn__Display__vtable__int32(),
    }
    var t9 closure_env_mk_renderer_T_int32_0 = closure_env_mk_renderer_T_int32_0{
        d_0: d__2,
    }
    retv8 = func() string {
        return _goml_inherent_closure_env_mk_renderer_T_int32_0_closure_env_mk_renderer_T_int32_0_apply(t9)
    }
    return retv8
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func _goml_inherent_closure_env_mk_renderer_T_int32_0_closure_env_mk_renderer_T_int32_0_apply(env1 closure_env_mk_renderer_T_int32_0) string {
    var retv13 string
    var d__2 dyn__Display = env1.d_0
    var t14 string = d__2.vtable.show(d__2.data)
    retv13 = t14
    return retv13
}

func main() {
    main0()
}
