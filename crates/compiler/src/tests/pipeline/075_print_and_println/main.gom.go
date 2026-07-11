package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_unit_to_string(x struct{}) string {
    return "()"
}

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_int32_x struct {
    value int32
}

func ref__Ref_5int32(value int32) *ref_int32_x {
    return &ref_int32_x{
        value: value,
    }
}

func ref_get__Ref_5int32(reference *ref_int32_x) int32 {
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
    return _goml_m_trait__impl_i_ToString_i_S_i_to__string(self.(S))
}

func dyn__ToString__vtable__S() *dyn__ToString_vtable {
    return &dyn__ToString_vtable{
        to_string: dyn__ToString__wrap__S__to_string,
    }
}

func _goml_m_trait__impl_i_ToString_i_S_i_to__string(self__0 S) string {
    var retv19 string
    var t20 int32 = self__0.value
    var t21 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t20)
    var t22 string = "S(" + t21
    var t23 string = t22 + ")"
    retv19 = t23
    return retv19
}

func main0() struct{} {
    println__T_int32(1)
    println__T_bool(true)
    println__T_string("hi")
    println__T_unit(struct{}{})
    var t25 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(2)
    println__T_string(t25)
    var t26 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
    println__T_string(t26)
    var s__1 S = S{
        value: 9,
    }
    println__T_S(s__1)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    println__T_dynToString(d__2)
    var r__3 *ref_int32_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(5)
    _goml_m_println____T__Ref_l_int32_r_(r__3)
    print__T_string("no-newline")
    println__T_string("!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv28 string
    var t29 string = _goml_runtime_core_int32_to_string(self__2)
    retv28 = t29
    return retv28
}

func println__T_int32(value__1 int32) struct{} {
    var t31 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t31)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t34 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t34)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t37 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t37)
    return struct{}{}
}

func println__T_unit(value__1 struct{}) struct{} {
    var t40 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__1)
    _goml_runtime_core_string_println(t40)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv43 string
    var t44 string = _goml_runtime_core_int32_to_string(self__13)
    retv43 = t44
    return retv43
}

func println__T_S(value__1 S) struct{} {
    var t46 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(value__1)
    _goml_runtime_core_string_println(t46)
    return struct{}{}
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t49 string = value__1.vtable.to_string(value__1.data)
    _goml_runtime_core_string_println(t49)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__114 int32) *ref_int32_x {
    var retv52 *ref_int32_x
    var t53 *ref_int32_x = ref__Ref_5int32(value__114)
    retv52 = t53
    return retv52
}

func _goml_m_println____T__Ref_l_int32_r_(value__1 *ref_int32_x) struct{} {
    var t55 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int32_r__i_to__string(value__1)
    _goml_runtime_core_string_println(t55)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t58 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t58)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv61 string
    var t62 string = _goml_runtime_core_bool_to_string(self__8)
    retv61 = t62
    return retv61
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv64 string
    retv64 = self__9
    return retv64
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__7 struct{}) string {
    var retv66 string
    var t67 string = _goml_runtime_core_unit_to_string(self__7)
    retv66 = t67
    return retv66
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int32_r__i_to__string(self__69 *ref_int32_x) string {
    var retv69 string
    var v__70 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__69)
    var t70 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(v__70)
    var t71 string = "ref(" + t70
    var t72 string = t71 + ")"
    retv69 = t72
    return retv69
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__115 *ref_int32_x) int32 {
    var retv74 int32
    var t75 int32 = ref_get__Ref_5int32(self__115)
    retv74 = t75
    return retv74
}

func main() {
    main0()
}
