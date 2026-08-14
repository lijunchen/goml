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
    var inline216 string = _goml_runtime_core_int32_to_string(self__0)
    return inline216
}

func main0() struct{} {
    var one__5 int32 = 42
    var left__6 int32 = 7
    var right__7 int32 = 9
    var t200 dyn__Display = dyn__Display{
        data: int32(one__5),
        vtable: dyn__Display__vtable__int32(),
    }
    var t202 string
    var inline238 string = t200.vtable.show(t200.data)
    t202 = inline238
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t202)
    _goml_runtime_core_string_println(inline233)
    var t203 dyn__Display = dyn__Display{
        data: int32(left__6),
        vtable: dyn__Display__vtable__int32(),
    }
    var t204 dyn__Display = dyn__Display{
        data: int32(right__7),
        vtable: dyn__Display__vtable__int32(),
    }
    var t206 string
    var inline228 string = t203.vtable.show(t203.data)
    var inline229 string = inline228 + "-"
    var inline230 string = t204.vtable.show(t204.data)
    var inline231 string = inline229 + inline230
    t206 = inline231
    var inline218 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline218)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
