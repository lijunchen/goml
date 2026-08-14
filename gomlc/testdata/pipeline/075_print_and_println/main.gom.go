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
    var t200 int32 = self__0.value
    var t201 string
    var inline266 string = _goml_runtime_core_int32_to_string(t200)
    t201 = inline266
    var t202 string = "S(" + t201
    var t203 string = t202 + ")"
    return t203
}

func main0() struct{} {
    var inline312 int = 1
    var inline313 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline312)
    _goml_runtime_core_string_println(inline313)
    var inline308 bool = true
    var inline309 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline308)
    _goml_runtime_core_string_println(inline309)
    var inline304 string = "hi"
    var inline305 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline304)
    _goml_runtime_core_string_println(inline305)
    var inline300 struct{} = struct{}{}
    var inline301 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline300)
    _goml_runtime_core_string_println(inline301)
    var t205 string
    var inline297 int = 2
    var inline298 string = _goml_runtime_core_int_to_string(inline297)
    t205 = inline298
    var inline294 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
    _goml_runtime_core_string_println(inline294)
    var t206 string
    var inline291 int = 2
    var inline292 string = _goml_runtime_core_int_to_string(inline291)
    t206 = inline292
    var inline288 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline288)
    var s__1 S = S{
        value: 9,
    }
    var inline285 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    _goml_runtime_core_string_println(inline285)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    var inline282 string = d__2.vtable.to_string(d__2.data)
    _goml_runtime_core_string_println(inline282)
    var r__3 *ref_int_x
    var inline279 int = 5
    var inline280 *ref_int_x = ref__Ref_3int(inline279)
    r__3 = inline280
    var inline276 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(r__3)
    _goml_runtime_core_string_println(inline276)
    var inline272 string = "no-newline"
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline272)
    _goml_runtime_core_string_print(inline273)
    var inline268 string = "!"
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline268)
    _goml_runtime_core_string_println(inline269)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t224 string = _goml_runtime_core_int_to_string(self__67)
    return t224
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t245 string = _goml_runtime_core_bool_to_string(self__64)
    return t245
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__63 struct{}) string {
    var t250 string = _goml_runtime_core_unit_to_string(self__63)
    return t250
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__166 *ref_int_x) string {
    var v__167 int
    var inline336 int = ref_get__Ref_3int(self__166)
    v__167 = inline336
    var t253 string
    var inline334 string = _goml_runtime_core_int_to_string(v__167)
    t253 = inline334
    var t254 string = "ref(" + t253
    var t255 string = t254 + ")"
    return t255
}

func main() {
    main0()
}
