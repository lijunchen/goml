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
    var inline219 string = _goml_runtime_core_int32_to_string(self__0)
    return inline219
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display
    var inline243 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__2 = inline243
    var first__3 int32 = 10
    var second__4 int32 = 20
    var t193 dyn__Display = dyn__Display{
        data: int32(first__3),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t193)
    var t194 dyn__Display = dyn__Display{
        data: int32(second__4),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t194)
    var s__5 []dyn__Display
    var inline235 int = 0
    var inline236 int = 2
    var inline237 []dyn__Display = v__2.items[inline235:inline236]
    s__5 = inline237
    var t195 dyn__Display = s__5[0]
    var t196 string
    var inline233 string = t195.vtable.show(t195.data)
    t196 = inline233
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
    _goml_runtime_core_string_println(inline230)
    var t__6 []dyn__Display
    var inline226 int = 1
    var inline227 int = 2
    var inline228 []dyn__Display = s__5[inline226:inline227]
    t__6 = inline228
    var t197 dyn__Display = t__6[0]
    var t198 string
    var inline224 string = t197.vtable.show(t197.data)
    t198 = inline224
    var inline221 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline221)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
