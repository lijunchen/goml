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
    var inline448 string = _goml_runtime_core_int32_to_string(self__0)
    return inline448
}

func main0() struct{} {
    var v__2 *_goml_vec_Dyn_Display
    var inline472 *_goml_vec_Dyn_Display = vec_new__Vec_11Dyn_Display()
    v__2 = inline472
    var first__3 int32 = 10
    var second__4 int32 = 20
    var t422 dyn__Display = dyn__Display{
        data: int32(first__3),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t422)
    var t423 dyn__Display = dyn__Display{
        data: int32(second__4),
        vtable: dyn__Display__vtable__int32(),
    }
    vec_push__Vec_11Dyn_Display(v__2, t423)
    var s__5 []dyn__Display
    var inline464 int = 0
    var inline465 int = 2
    var inline466 []dyn__Display = v__2.items[inline464:inline465]
    s__5 = inline466
    var t424 dyn__Display = s__5[0]
    var t425 string
    var inline462 string = t424.vtable.show(t424.data)
    t425 = inline462
    var inline459 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t425)
    _goml_runtime_core_string_println(inline459)
    var t__6 []dyn__Display
    var inline455 int = 1
    var inline456 int = 2
    var inline457 []dyn__Display = s__5[inline455:inline456]
    t__6 = inline457
    var t426 dyn__Display = t__6[0]
    var t427 string
    var inline453 string = t426.vtable.show(t426.data)
    t427 = inline453
    var inline450 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t427)
    _goml_runtime_core_string_println(inline450)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
