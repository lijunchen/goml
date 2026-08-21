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

type Ordering int32

type Boxed interface {
    isBoxed()
}

type One struct {
    _0 dyn__Display
}

func (_ One) isBoxed() {}

type Pair struct {
    _0 dyn__Display
    _1 dyn__Display
}

func (_ Pair) isBoxed() {}

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
    var one__5 int32 = 42
    var left__6 int32 = 7
    var right__7 int32 = 9
    var t429 dyn__Display = dyn__Display{
        data: int32(one__5),
        vtable: dyn__Display__vtable__int32(),
    }
    var t431 string
    var inline467 string = t429.vtable.show(t429.data)
    t431 = inline467
    var inline462 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t431)
    _goml_runtime_core_string_println(inline462)
    var t432 dyn__Display = dyn__Display{
        data: int32(left__6),
        vtable: dyn__Display__vtable__int32(),
    }
    var t433 dyn__Display = dyn__Display{
        data: int32(right__7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t435 string
    var inline457 string = t432.vtable.show(t432.data)
    var inline458 string = inline457 + "-"
    var inline459 string = t433.vtable.show(t433.data)
    var inline460 string = inline458 + inline459
    t435 = inline460
    var inline447 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
    _goml_runtime_core_string_println(inline447)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
