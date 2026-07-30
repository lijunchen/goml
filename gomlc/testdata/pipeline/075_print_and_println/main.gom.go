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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
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

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
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
    var retv120 string
    var t121 int32 = self__0.value
    var t122 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t121)
    var t123 string = "S(" + t122
    var t124 string = t123 + ")"
    retv120 = t124
    return retv120
}

func main0() struct{} {
    println__T_int(1)
    println__T_bool(true)
    println__T_string("hi")
    println__T_unit(struct{}{})
    var t126 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(2)
    println__T_string(t126)
    var t127 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
    println__T_string(t127)
    var s__1 S = S{
        value: 9,
    }
    println__T_S(s__1)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    println__T_dynToString(d__2)
    var r__3 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(5)
    _goml_m_println____T__Ref_l_int_r_(r__3)
    print__T_string("no-newline")
    println__T_string("!")
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv129 string
    var t130 string = _goml_runtime_core_int32_to_string(self__6)
    retv129 = t130
    return retv129
}

func println__T_int(value__1 int) struct{} {
    var t132 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t132)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t135 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t135)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t138 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t138)
    return struct{}{}
}

func println__T_unit(value__1 struct{}) struct{} {
    var t141 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__1)
    _goml_runtime_core_string_println(t141)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv144 string
    var t145 string = _goml_runtime_core_int_to_string(self__40)
    retv144 = t145
    return retv144
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv147 string
    var t148 string = _goml_runtime_core_int_to_string(self__5)
    retv147 = t148
    return retv147
}

func println__T_S(value__1 S) struct{} {
    var t150 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(value__1)
    _goml_runtime_core_string_println(t150)
    return struct{}{}
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t153 string = value__1.vtable.to_string(value__1.data)
    _goml_runtime_core_string_println(t153)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv156 *ref_int_x
    var t157 *ref_int_x = ref__Ref_3int(value__207)
    retv156 = t157
    return retv156
}

func _goml_m_println____T__Ref_l_int_r_(value__1 *ref_int_x) struct{} {
    var t159 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(value__1)
    _goml_runtime_core_string_println(t159)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t162 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t162)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv165 string
    var t166 string = _goml_runtime_core_bool_to_string(self__37)
    retv165 = t166
    return retv165
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv168 string
    retv168 = self__38
    return retv168
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv170 string
    var t171 string = _goml_runtime_core_unit_to_string(self__36)
    retv170 = t171
    return retv170
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__99 *ref_int_x) string {
    var retv173 string
    var v__100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__99)
    var t174 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(v__100)
    var t175 string = "ref(" + t174
    var t176 string = t175 + ")"
    retv173 = t176
    return retv173
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv178 int
    var t179 int = ref_get__Ref_3int(self__208)
    retv178 = t179
    return retv178
}

func main() {
    main0()
}
