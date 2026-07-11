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
    var retv34 string
    var t35 int32 = self__0.value
    var t36 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t35)
    var t37 string = "S(" + t36
    var t38 string = t37 + ")"
    retv34 = t38
    return retv34
}

func main0() struct{} {
    println__T_int32(1)
    println__T_bool(true)
    println__T_string("hi")
    println__T_unit(struct{}{})
    var t40 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(2)
    println__T_string(t40)
    var t41 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
    println__T_string(t41)
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
    var retv43 string
    var t44 string = _goml_runtime_core_int32_to_string(self__2)
    retv43 = t44
    return retv43
}

func println__T_int32(value__1 int32) struct{} {
    var t46 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t46)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t49 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t49)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t52 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t52)
    return struct{}{}
}

func println__T_unit(value__1 struct{}) struct{} {
    var t55 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__1)
    _goml_runtime_core_string_println(t55)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv58 string
    var t59 string = _goml_runtime_core_int32_to_string(self__13)
    retv58 = t59
    return retv58
}

func println__T_S(value__1 S) struct{} {
    var t61 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(value__1)
    _goml_runtime_core_string_println(t61)
    return struct{}{}
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t64 string = value__1.vtable.to_string(value__1.data)
    _goml_runtime_core_string_println(t64)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__137 int32) *ref_int32_x {
    var retv67 *ref_int32_x
    var t68 *ref_int32_x = ref__Ref_5int32(value__137)
    retv67 = t68
    return retv67
}

func _goml_m_println____T__Ref_l_int32_r_(value__1 *ref_int32_x) struct{} {
    var t70 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int32_r__i_to__string(value__1)
    _goml_runtime_core_string_println(t70)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t73 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t73)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv76 string
    var t77 string = _goml_runtime_core_bool_to_string(self__8)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv79 string
    retv79 = self__9
    return retv79
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__7 struct{}) string {
    var retv81 string
    var t82 string = _goml_runtime_core_unit_to_string(self__7)
    retv81 = t82
    return retv81
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int32_r__i_to__string(self__69 *ref_int32_x) string {
    var retv84 string
    var v__70 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__69)
    var t85 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(v__70)
    var t86 string = "ref(" + t85
    var t87 string = t86 + ")"
    retv84 = t87
    return retv84
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__138 *ref_int32_x) int32 {
    var retv89 int32
    var t90 int32 = ref_get__Ref_5int32(self__138)
    retv89 = t90
    return retv89
}

func main() {
    main0()
}
