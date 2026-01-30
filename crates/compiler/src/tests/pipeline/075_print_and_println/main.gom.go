package main

import (
    "fmt"
)

func unit_to_string(x struct{}) string {
    return "()"
}

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

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_int32(reference *ref_int32_x) int32 {
    return reference.value
}

type S struct {
    value int32
}

type dyn__Show_vtable struct {
    show func(any) string
}

type dyn__Show struct {
    data any
    vtable *dyn__Show_vtable
}

func dyn__Show__wrap__S__show(self any) string {
    return _goml_trait_impl_Show_S_show(self.(S))
}

func dyn__Show__vtable__S() *dyn__Show_vtable {
    return &dyn__Show_vtable{
        show: dyn__Show__wrap__S__show,
    }
}

func _goml_trait_impl_Show_S_show(self__0 S) string {
    var ret25 string
    var t13 int32 = self__0.value
    var t12 string = int32_to_string(t13)
    var t11 string = "S(" + t12
    ret25 = t11 + ")"
    return ret25
}

func main0() struct{} {
    var ret26 struct{}
    var t14 string = int32_to_string(1)
    string_println(t14)
    var t15 string = bool_to_string(true)
    string_println(t15)
    string_println("hi")
    var t16 string = unit_to_string(struct{}{})
    string_println(t16)
    var t17 string = int32_to_string(2)
    string_println(t17)
    var t18 string = int32_to_string(2)
    string_println(t18)
    var s__1 S = S{
        value: 9,
    }
    var t19 string = _goml_trait_impl_Show_S_show(s__1)
    string_println(t19)
    var d__2 dyn__Show = dyn__Show{
        data: s__1,
        vtable: dyn__Show__vtable__S(),
    }
    var t20 string = d__2.vtable.show(d__2.data)
    string_println(t20)
    var r__3 *ref_int32_x = ref__Ref_int32(5)
    var t24 int32 = ref_get__Ref_int32(r__3)
    var t23 string = int32_to_string(t24)
    var t22 string = "ref(" + t23
    var t21 string = t22 + ")"
    string_println(t21)
    string_print("no-newline")
    string_println("!")
    ret26 = struct{}{}
    return ret26
}

func main() {
    main0()
}
