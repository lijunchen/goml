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
    var retv16 string
    var t17 int32 = self__0.value
    var t18 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t17)
    var t19 string = "S(" + t18
    var t20 string = t19 + ")"
    retv16 = t20
    return retv16
}

func main0() struct{} {
    println__T_int32(1)
    println__T_bool(true)
    println__T_string("hi")
    println__T_unit(struct{}{})
    var t22 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(2)
    println__T_string(t22)
    var t23 string = _goml_m_inherent_i_int32_i_int32_i_to__string(2)
    println__T_string(t23)
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
    var retv25 string
    var t26 string = _goml_runtime_core_int32_to_string(self__2)
    retv25 = t26
    return retv25
}

func println__T_int32(value__1 int32) struct{} {
    var t28 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t28)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t31 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t31)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t34 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t34)
    return struct{}{}
}

func println__T_unit(value__1 struct{}) struct{} {
    var t37 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__1)
    _goml_runtime_core_string_println(t37)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__13 int32) string {
    var retv40 string
    var t41 string = _goml_runtime_core_int32_to_string(self__13)
    retv40 = t41
    return retv40
}

func println__T_S(value__1 S) struct{} {
    var t43 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(value__1)
    _goml_runtime_core_string_println(t43)
    return struct{}{}
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t46 string = value__1.vtable.to_string(value__1.data)
    _goml_runtime_core_string_println(t46)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int32(value__102 int32) *ref_int32_x {
    var retv49 *ref_int32_x
    var t50 *ref_int32_x = ref__Ref_5int32(value__102)
    retv49 = t50
    return retv49
}

func _goml_m_println____T__Ref_l_int32_r_(value__1 *ref_int32_x) struct{} {
    var t52 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int32_r__i_to__string(value__1)
    _goml_runtime_core_string_println(t52)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t55 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t55)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__8 bool) string {
    var retv58 string
    var t59 string = _goml_runtime_core_bool_to_string(self__8)
    retv58 = t59
    return retv58
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv61 string
    retv61 = self__9
    return retv61
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__7 struct{}) string {
    var retv63 string
    var t64 string = _goml_runtime_core_unit_to_string(self__7)
    retv63 = t64
    return retv63
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int32_r__i_to__string(self__69 *ref_int32_x) string {
    var retv66 string
    var v__70 int32 = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__69)
    var t67 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(v__70)
    var t68 string = "ref(" + t67
    var t69 string = t68 + ")"
    retv66 = t69
    return retv66
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int32(self__103 *ref_int32_x) int32 {
    var retv71 int32
    var t72 int32 = ref_get__Ref_5int32(self__103)
    retv71 = t72
    return retv71
}

func main() {
    main0()
}
