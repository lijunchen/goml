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
    var retv80 string
    var t81 int32 = self__0.value
    var t82 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t81)
    var t83 string = "S(" + t82
    var t84 string = t83 + ")"
    retv80 = t84
    return retv80
}

func main0() struct{} {
    println__T_int(1)
    println__T_bool(true)
    println__T_string("hi")
    println__T_unit(struct{}{})
    var t86 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(2)
    println__T_string(t86)
    var t87 string = _goml_m_inherent_i_int_i_int_i_to__string(2)
    println__T_string(t87)
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
    var retv89 string
    var t90 string = _goml_runtime_core_int32_to_string(self__6)
    retv89 = t90
    return retv89
}

func println__T_int(value__1 int) struct{} {
    var t92 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t92)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t95 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t95)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t98 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t98)
    return struct{}{}
}

func println__T_unit(value__1 struct{}) struct{} {
    var t101 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(value__1)
    _goml_runtime_core_string_println(t101)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv104 string
    var t105 string = _goml_runtime_core_int_to_string(self__40)
    retv104 = t105
    return retv104
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv107 string
    var t108 string = _goml_runtime_core_int_to_string(self__5)
    retv107 = t108
    return retv107
}

func println__T_S(value__1 S) struct{} {
    var t110 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(value__1)
    _goml_runtime_core_string_println(t110)
    return struct{}{}
}

func println__T_dynToString(value__1 dyn__ToString) struct{} {
    var t113 string = value__1.vtable.to_string(value__1.data)
    _goml_runtime_core_string_println(t113)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv116 *ref_int_x
    var t117 *ref_int_x = ref__Ref_3int(value__207)
    retv116 = t117
    return retv116
}

func _goml_m_println____T__Ref_l_int_r_(value__1 *ref_int_x) struct{} {
    var t119 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(value__1)
    _goml_runtime_core_string_println(t119)
    return struct{}{}
}

func print__T_string(value__0 string) struct{} {
    var t122 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__0)
    _goml_runtime_core_string_print(t122)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv125 string
    var t126 string = _goml_runtime_core_bool_to_string(self__37)
    retv125 = t126
    return retv125
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv128 string
    retv128 = self__38
    return retv128
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__36 struct{}) string {
    var retv130 string
    var t131 string = _goml_runtime_core_unit_to_string(self__36)
    retv130 = t131
    return retv130
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__99 *ref_int_x) string {
    var retv133 string
    var v__100 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__99)
    var t134 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(v__100)
    var t135 string = "ref(" + t134
    var t136 string = t135 + ")"
    retv133 = t136
    return retv133
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv138 int
    var t139 int = ref_get__Ref_3int(self__208)
    retv138 = t139
    return retv138
}

func main() {
    main0()
}
