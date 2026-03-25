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

func render(x__1 dyn__Display) string {
    var retv6 string
    var t7 string = x__1.vtable.show(x__1.data)
    retv6 = t7
    return retv6
}

func main0() struct{} {
    var v__2 []dyn__Display = nil
    var t9 dyn__Display = dyn__Display{
        data: int32(10),
        vtable: dyn__Display__vtable__int32(),
    }
    var v__3 []dyn__Display = append(v__2, t9)
    var t10 dyn__Display = dyn__Display{
        data: int32(20),
        vtable: dyn__Display__vtable__int32(),
    }
    var v__4 []dyn__Display = append(v__3, t10)
    var s__5 []dyn__Display = v__4[0:2]
    var t11 dyn__Display = s__5[0]
    var t12 string = render(t11)
    println__T_string(t12)
    var t__6 []dyn__Display = s__5[1:2]
    var t13 dyn__Display = t__6[0]
    var t14 string = render(t13)
    println__T_string(t14)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
