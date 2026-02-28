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

type dyn__ToString_vtable struct {
    to_string func(any) string
}

type dyn__ToString struct {
    data any
    vtable *dyn__ToString_vtable
}

func dyn__ToString__wrap__S__to_string(self any) string {
    return _goml_trait_impl_ToString_S_to_string(self.(S))
}

func dyn__ToString__vtable__S() *dyn__ToString_vtable {
    return &dyn__ToString_vtable{
        to_string: dyn__ToString__wrap__S__to_string,
    }
}

func _goml_trait_impl_ToString_S_to_string(self__0 S) string {
    var t11 int32 = self__0.value
    var t12 string = int32_to_string(t11)
    var t13 string = "S(" + t12
    var t14 string = t13 + ")"
    return t14
}

func main0() struct{} {
    println__T_int32(1)
    println__T_bool(true)
    println__T_string("hi")
    println__T_unit(struct{}{})
    var t15 string = int32_to_string(2)
    println__T_string(t15)
    var t16 string = int32_to_string(2)
    println__T_string(t16)
    var s__1 S = S{
        value: 9,
    }
    println__T_S(s__1)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    println__T_dynToString(d__2)
    var r__3 *ref_int32_x = ref__Ref_int32(5)
    _goml_println__T_Ref_x5b_int32_x5d_(r__3)
    print__T_string("no-newline")
    println__T_string("!")
    return struct{}{}
}

func println__T_int32(value__1 int32) struct{} {
    var t17 string = int32_to_string(value__1)
    var t18 struct{} = string_println(t17)
    return t18
}

func println__T_bool(value__1 bool) struct{} {
    var t19 string = bool_to_string(value__1)
    var t20 struct{} = string_println(t19)
    return t20
}

func println__T_string(value__1 string) struct{} {
    var t21 struct{} = string_println(value__1)
    return t21
}

func println__T_unit(value__1 struct{}) struct{} {
    var t22 string = unit_to_string(value__1)
    var t23 struct{} = string_println(t22)
    return t23
}

func println__T_S(value__1 S) struct{} {
    var t24 string = _goml_trait_impl_ToString_S_to_string(value__1)
    var t25 struct{} = string_println(t24)
    return t25
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t26 string = value__1.vtable.to_string(value__1.data)
    var t27 struct{} = string_println(t26)
    return t27
}

func _goml_println__T_Ref_x5b_int32_x5d_(value__1 *ref_int32_x) struct{} {
    var t28 int32 = ref_get__Ref_int32(value__1)
    var t29 string = int32_to_string(t28)
    var t30 string = "ref(" + t29
    var t31 string = t30 + ")"
    var t32 struct{} = string_println(t31)
    return t32
}

func print__T_string(value__0 string) struct{} {
    var t33 struct{} = string_print(value__0)
    return t33
}

func main() {
    main0()
}
