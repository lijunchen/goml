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

type Ordering int32

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
    var t421 int32 = self__0.value
    var t422 string
    var inline487 string = _goml_runtime_core_int32_to_string(t421)
    t422 = inline487
    var t423 string = "S(" + t422
    var t424 string = t423 + ")"
    return t424
}

func main0() struct{} {
    var inline533 int = 1
    var inline534 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline533)
    _goml_runtime_core_string_println(inline534)
    var inline529 bool = true
    var inline530 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline529)
    _goml_runtime_core_string_println(inline530)
    var inline525 string = "hi"
    var inline526 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline525)
    _goml_runtime_core_string_println(inline526)
    var inline521 struct{} = struct{}{}
    var inline522 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline521)
    _goml_runtime_core_string_println(inline522)
    var t426 string
    var inline518 int = 2
    var inline519 string = _goml_runtime_core_int_to_string(inline518)
    t426 = inline519
    var inline515 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t426)
    _goml_runtime_core_string_println(inline515)
    var t427 string
    var inline512 int = 2
    var inline513 string = _goml_runtime_core_int_to_string(inline512)
    t427 = inline513
    var inline509 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline509)
    var s__1 S = S{
        value: 9,
    }
    var inline506 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    _goml_runtime_core_string_println(inline506)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    var inline503 string = d__2.vtable.to_string(d__2.data)
    _goml_runtime_core_string_println(inline503)
    var r__3 *ref_int_x
    var inline500 int = 5
    var inline501 *ref_int_x = ref__Ref_3int(inline500)
    r__3 = inline501
    var inline497 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(r__3)
    _goml_runtime_core_string_println(inline497)
    var inline493 string = "no-newline"
    var inline494 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline493)
    _goml_runtime_core_string_print(inline494)
    var inline489 string = "!"
    var inline490 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline489)
    _goml_runtime_core_string_println(inline490)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t445 string = _goml_runtime_core_int_to_string(self__151)
    return t445
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t466 string = _goml_runtime_core_bool_to_string(self__148)
    return t466
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__147 struct{}) string {
    var t471 string = _goml_runtime_core_unit_to_string(self__147)
    return t471
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__250 *ref_int_x) string {
    var v__251 int
    var inline557 int = ref_get__Ref_3int(self__250)
    v__251 = inline557
    var t474 string
    var inline555 string = _goml_runtime_core_int_to_string(v__251)
    t474 = inline555
    var t475 string = "ref(" + t474
    var t476 string = t475 + ")"
    return t476
}

func main() {
    main0()
}
