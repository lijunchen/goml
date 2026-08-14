package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
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

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

func find(wanted__0 int) Option__int {
    var current__1 *ref_int_x
    var inline234 int = 0
    var inline235 *ref_int_x = ref__Ref_3int(inline234)
    current__1 = inline235
    for {
        var value__2 int
        var inline232 int = ref_get__Ref_3int(current__1)
        value__2 = inline232
        var t197 bool = value__2 >= 5
        if t197 {
            return None{}
        } else {
            var t199 bool = value__2 == wanted__0
            if t199 {
                var t200 Option__int = Some{
                    _0: value__2,
                }
                return t200
            } else {
                var t201 int = value__2 + 1
                ref_set__Ref_3int(current__1, t201)
                continue
            }
        }
    }
}

func main0() struct{} {
    var t207 bool
    var inline257 int = 3
    var inline258 Option__int = find(inline257)
    switch inline258.(type) {
    case None:
        t207 = false
    case Some:
        t207 = true
    default:
        panic("non-exhaustive match")
    }
    var inline254 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t207)
    _goml_runtime_core_string_println(inline254)
    var t208 bool
    var inline251 int = 8
    var inline252 Option__int = find(inline251)
    switch inline252.(type) {
    case None:
        t208 = false
    case Some:
        t208 = true
    default:
        panic("non-exhaustive match")
    }
    var inline248 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t208)
    _goml_runtime_core_string_println(inline248)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t222 string = _goml_runtime_core_bool_to_string(self__64)
    return t222
}

func main() {
    main0()
}
