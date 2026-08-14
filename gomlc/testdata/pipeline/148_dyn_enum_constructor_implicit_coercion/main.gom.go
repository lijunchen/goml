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
    var inline221 string = _goml_runtime_core_int32_to_string(self__0)
    return inline221
}

func main0() struct{} {
    var one__5 int32 = 42
    var left__6 int32 = 7
    var right__7 int32 = 9
    var t205 dyn__Display = dyn__Display{
        data: int32(one__5),
        vtable: dyn__Display__vtable__int32(),
    }
    var t207 string
    var inline243 string = t205.vtable.show(t205.data)
    t207 = inline243
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
    _goml_runtime_core_string_println(inline238)
    var t208 dyn__Display = dyn__Display{
        data: int32(left__6),
        vtable: dyn__Display__vtable__int32(),
    }
    var t209 dyn__Display = dyn__Display{
        data: int32(right__7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t211 string
    var inline233 string = t208.vtable.show(t208.data)
    var inline234 string = inline233 + "-"
    var inline235 string = t209.vtable.show(t209.data)
    var inline236 string = inline234 + inline235
    t211 = inline236
    var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
    _goml_runtime_core_string_println(inline223)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
