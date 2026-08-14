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

type Ordering int32

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
    var inline445 string = _goml_runtime_core_int32_to_string(self__0)
    return inline445
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display
    var inline469 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__2 = inline469
    var first__3 int32 = 10
    var second__4 int32 = 20
    var t419 dyn__Display = dyn__Display{
        data: int32(first__3),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t419)
    var t420 dyn__Display = dyn__Display{
        data: int32(second__4),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t420)
    var s__5 []dyn__Display
    var inline461 int = 0
    var inline462 int = 2
    var inline463 []dyn__Display = v__2.items[inline461:inline462]
    s__5 = inline463
    var t421 dyn__Display = s__5[0]
    var t422 string
    var inline459 string = t421.vtable.show(t421.data)
    t422 = inline459
    var inline456 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t422)
    _goml_runtime_core_string_println(inline456)
    var t__6 []dyn__Display
    var inline452 int = 1
    var inline453 int = 2
    var inline454 []dyn__Display = s__5[inline452:inline453]
    t__6 = inline454
    var t423 dyn__Display = t__6[0]
    var t424 string
    var inline450 string = t423.vtable.show(t423.data)
    t424 = inline450
    var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t424)
    _goml_runtime_core_string_println(inline447)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
