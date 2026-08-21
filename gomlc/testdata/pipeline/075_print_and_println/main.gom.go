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
    var t424 int32 = self__0.value
    var t425 string
    var inline490 string = _goml_runtime_core_int32_to_string(t424)
    t425 = inline490
    var t426 string = "S(" + t425
    var t427 string = t426 + ")"
    return t427
}

func main0() struct{} {
    var inline536 int = 1
    var inline537 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline536)
    _goml_runtime_core_string_println(inline537)
    var inline532 bool = true
    var inline533 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline532)
    _goml_runtime_core_string_println(inline533)
    var inline528 string = "hi"
    var inline529 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline528)
    _goml_runtime_core_string_println(inline529)
    var inline524 struct{} = struct{}{}
    var inline525 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline524)
    _goml_runtime_core_string_println(inline525)
    var t429 string
    var inline521 int = 2
    var inline522 string = _goml_runtime_core_int_to_string(inline521)
    t429 = inline522
    var inline518 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t429)
    _goml_runtime_core_string_println(inline518)
    var t430 string
    var inline515 int = 2
    var inline516 string = _goml_runtime_core_int_to_string(inline515)
    t430 = inline516
    var inline512 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t430)
    _goml_runtime_core_string_println(inline512)
    var s__1 S = S{
        value: 9,
    }
    var inline509 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    _goml_runtime_core_string_println(inline509)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    var inline506 string = d__2.vtable.to_string(d__2.data)
    _goml_runtime_core_string_println(inline506)
    var r__3 *ref_int_x
    var inline503 int = 5
    var inline504 *ref_int_x = ref__Ref_3int(inline503)
    r__3 = inline504
    var inline500 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(r__3)
    _goml_runtime_core_string_println(inline500)
    var inline496 string = "no-newline"
    var inline497 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline496)
    _goml_runtime_core_string_print(inline497)
    var inline492 string = "!"
    var inline493 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline492)
    _goml_runtime_core_string_println(inline493)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t448 string = _goml_runtime_core_int_to_string(self__151)
    return t448
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t469 string = _goml_runtime_core_bool_to_string(self__148)
    return t469
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__147 struct{}) string {
    var t474 string = _goml_runtime_core_unit_to_string(self__147)
    return t474
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__250 *ref_int_x) string {
    var v__251 int
    var inline560 int = ref_get__Ref_3int(self__250)
    v__251 = inline560
    var t477 string
    var inline558 string = _goml_runtime_core_int_to_string(v__251)
    t477 = inline558
    var t478 string = "ref(" + t477
    var t479 string = t478 + ")"
    return t479
}

func main() {
    main0()
}
