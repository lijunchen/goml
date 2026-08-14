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
    var t195 int32 = self__0.value
    var t196 string
    var inline261 string = _goml_runtime_core_int32_to_string(t195)
    t196 = inline261
    var t197 string = "S(" + t196
    var t198 string = t197 + ")"
    return t198
}

func main0() struct{} {
    var inline307 int = 1
    var inline308 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(inline307)
    _goml_runtime_core_string_println(inline308)
    var inline303 bool = true
    var inline304 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(inline303)
    _goml_runtime_core_string_println(inline304)
    var inline299 string = "hi"
    var inline300 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline299)
    _goml_runtime_core_string_println(inline300)
    var inline295 struct{} = struct{}{}
    var inline296 string = _goml_m_trait__impl_i_ToString_i_unit_i_to__string(inline295)
    _goml_runtime_core_string_println(inline296)
    var t200 string
    var inline292 int = 2
    var inline293 string = _goml_runtime_core_int_to_string(inline292)
    t200 = inline293
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
    _goml_runtime_core_string_println(inline289)
    var t201 string
    var inline286 int = 2
    var inline287 string = _goml_runtime_core_int_to_string(inline286)
    t201 = inline287
    var inline283 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline283)
    var s__1 S = S{
        value: 9,
    }
    var inline280 string = _goml_m_trait__impl_i_ToString_i_S_i_to__string(s__1)
    _goml_runtime_core_string_println(inline280)
    var d__2 dyn__ToString = dyn__ToString{
        data: s__1,
        vtable: dyn__ToString__vtable__S(),
    }
    var inline277 string = d__2.vtable.to_string(d__2.data)
    _goml_runtime_core_string_println(inline277)
    var r__3 *ref_int_x
    var inline274 int = 5
    var inline275 *ref_int_x = ref__Ref_3int(inline274)
    r__3 = inline275
    var inline271 string = _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(r__3)
    _goml_runtime_core_string_println(inline271)
    var inline267 string = "no-newline"
    var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline267)
    _goml_runtime_core_string_print(inline268)
    var inline263 string = "!"
    var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline263)
    _goml_runtime_core_string_println(inline264)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t219 string = _goml_runtime_core_int_to_string(self__67)
    return t219
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t240 string = _goml_runtime_core_bool_to_string(self__64)
    return t240
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_unit_i_to__string(self__63 struct{}) string {
    var t245 string = _goml_runtime_core_unit_to_string(self__63)
    return t245
}

func _goml_m_trait__impl_i_ToString_i_Ref_l_int_r__i_to__string(self__166 *ref_int_x) string {
    var v__167 int
    var inline331 int = ref_get__Ref_3int(self__166)
    v__167 = inline331
    var t248 string
    var inline329 string = _goml_runtime_core_int_to_string(v__167)
    t248 = inline329
    var t249 string = "ref(" + t248
    var t250 string = t249 + ")"
    return t250
}

func main() {
    main0()
}
