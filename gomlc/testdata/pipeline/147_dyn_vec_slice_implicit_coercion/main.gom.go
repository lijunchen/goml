package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_Dyn_Display struct {
    items []dyn__Display
}

func vec_new__Vec_11Dyn_Display() *_goml_vec_Dyn_Display {
    return &_goml_vec_Dyn_Display{
        items: nil,
    }
}

func vec_push__Vec_11Dyn_Display(vec *_goml_vec_Dyn_Display, elem dyn__Display) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func dyn__Display__wrap__int32__show(self any) string {
    return _goml_m_trait__impl_i_Display_i_int32_i_show(self.(int32))
}

func dyn__Display__vtable__int32() *dyn__Display_vtable {
    return &dyn__Display_vtable{
        show: dyn__Display__wrap__int32__show,
    }
}

func _goml_m_trait__impl_i_Display_i_int32_i_show(self__0 int32) string {
    var inline224 string = _goml_runtime_core_int32_to_string(self__0)
    return inline224
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display
    var inline248 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__2 = inline248
    var first__3 int32 = 10
    var second__4 int32 = 20
    var t198 dyn__Display = dyn__Display{
        data: int32(first__3),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t198)
    var t199 dyn__Display = dyn__Display{
        data: int32(second__4),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t199)
    var s__5 []dyn__Display
    var inline240 int = 0
    var inline241 int = 2
    var inline242 []dyn__Display = v__2.items[inline240:inline241]
    s__5 = inline242
    var t200 dyn__Display = s__5[0]
    var t201 string
    var inline238 string = t200.vtable.show(t200.data)
    t201 = inline238
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline235)
    var t__6 []dyn__Display
    var inline231 int = 1
    var inline232 int = 2
    var inline233 []dyn__Display = s__5[inline231:inline232]
    t__6 = inline233
    var t202 dyn__Display = t__6[0]
    var t203 string
    var inline229 string = t202.vtable.show(t202.data)
    t203 = inline229
    var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t203)
    _goml_runtime_core_string_println(inline226)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
