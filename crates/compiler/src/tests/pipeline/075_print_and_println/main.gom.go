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
    var retv76 string
    var t77 int32 = self__0.value
    var t78 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t77)
    var t79 string = "S(" + t78
    var t80 string = t79 + ")"
    retv76 = t80
    return retv76
}

func main0() struct{} {
    println__T_int(1)
    println__T_bool(true)
    println__T_string("hi")
    println__T_unit(struct{}{})
    var t82 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(2)
    println__T_string(t82)
    var t83 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
    println__T_string(t83)
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
    var retv85 string
    var t86 string = _goml_runtime_core_int32_to_string(self__6)
    retv85 = t86
    return retv85
}

func println__T_int(value__1 int) struct{} {
    var t88 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t88)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t91 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t91)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t94 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t94)
    return struct{}{}
}

func println__T_unit(value__1 struct{}) struct{} {
    var t97 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__1)
    _goml_runtime_core_string_println(t97)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv100 string
    var t101 string = _goml_runtime_core_int_to_string(self__40)
    retv100 = t101
    return retv100
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv103 string
    var t104 string = _goml_runtime_core_int_to_string(self__5)
    retv103 = t104
    return retv103
}

func println__T_S(value__1 S) struct{} {
    var t106 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(value__1)
    _goml_runtime_core_string_println(t106)
    return struct{}{}
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t109 string = value__1.vtable.to_string(value__1.data)
    _goml_runtime_core_string_println(t109)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__209 int) *ref_int_x {
    var retv112 *ref_int_x
    var t113 *ref_int_x = ref__Ref_3int(value__209)
    retv112 = t113
    return retv112
}

func _goml_m_println____T__Ref_l_int_r_(value__1 *ref_int_x) struct{} {
    var t115 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(value__1)
    _goml_runtime_core_string_println(t115)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t118 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t118)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv121 string
    var t122 string = _goml_runtime_core_bool_to_string(self__37)
    retv121 = t122
    return retv121
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv124 string
    retv124 = self__38
    return retv124
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv126 string
    var t127 string = _goml_runtime_core_unit_to_string(self__36)
    retv126 = t127
    return retv126
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__99 *ref_int_x) string {
    var retv129 string
    var v__100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__99)
    var t130 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(v__100)
    var t131 string = "ref(" + t130
    var t132 string = t131 + ")"
    retv129 = t132
    return retv129
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__210 *ref_int_x) int {
    var retv134 int
    var t135 int = ref_get__Ref_3int(self__210)
    retv134 = t135
    return retv134
}

func main() {
    main0()
}
