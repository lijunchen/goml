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

type dyn__Display_vtable struct {
    show func(any) string
}

type dyn__Display struct {
    data any
    vtable *dyn__Display_vtable
}

func main0() struct{} {
    var t418 int32
    t418 = 9
    var t419 string
    var inline435 string = _goml_runtime_core_int32_to_string(t418)
    t419 = inline435
    var inline432 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t419)
    _goml_runtime_core_string_println(inline432)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
