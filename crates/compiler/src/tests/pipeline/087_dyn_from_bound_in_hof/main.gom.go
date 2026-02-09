package main

import (
    "fmt"
)

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__bool__show(self any) string {
    return _goml_trait_impl_Display_bool_show(self.(bool))
}

func dyn__Display__vtable__bool() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__bool__show,
    }
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
    var ret10 string
    ret10 = int32_to_string(self__0)
    return ret10
}

func _goml_trait_impl_Display_bool_show(self__1 bool) string {
    var ret11 string
    ret11 = bool_to_string(self__1)
    return ret11
}

func main0() struct{} {
    var ret12 struct{}
    var t2 string = render_twice__T_int32(42)
    println__T_string(t2)
    var t3 string = render_twice__T_bool(true)
    println__T_string(t3)
    ret12 = struct{}{}
    return ret12
}

func render_twice__T_int32(x__2 int32) string {
    var ret13 string
    var d__3 dyn__Display = dyn__Display{
        data: x__2,
        vtable: dyn__Display__vtable__int32(),
    }
    var t5 string = d__3.vtable.show(d__3.data)
    var t4 string = t5 + ","
    var t6 string = d__3.vtable.show(d__3.data)
    ret13 = t4 + t6
    return ret13
}

func println__T_string(value__1 string) struct{} {
    var ret14 struct{}
    ret14 = string_println(value__1)
    return ret14
}

func render_twice__T_bool(x__2 bool) string {
    var ret15 string
    var d__3 dyn__Display = dyn__Display{
        data: x__2,
        vtable: dyn__Display__vtable__bool(),
    }
    var t8 string = d__3.vtable.show(d__3.data)
    var t7 string = t8 + ","
    var t9 string = d__3.vtable.show(d__3.data)
    ret15 = t7 + t9
    return ret15
}

func main() {
    main0()
}
