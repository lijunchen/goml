package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type T interface {
    isT()
}

type A struct {}

func (_ A) isT() {}

type B struct {
    _0 bool
    _1 struct{}
}

func (_ B) isT() {}

func main0() struct{} {
    var x182 bool = true
    switch x182 {
    case true:
        var t190 string
        var inline206 int = 2
        var inline207 string = _goml_runtime_core_int_to_string(inline206)
        t190 = inline207
        var inline203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t190)
        _goml_runtime_core_string_println(inline203)
        return struct{}{}
    case false:
        var t192 string
        var inline212 int = 3
        var inline213 string = _goml_runtime_core_int_to_string(inline212)
        t192 = inline213
        var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t192)
        _goml_runtime_core_string_println(inline209)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
