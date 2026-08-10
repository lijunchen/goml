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
    var x172 bool = true
    switch x172 {
    case true:
        var t180 string
        var inline196 int = 2
        var inline197 string = _goml_runtime_core_int_to_string(inline196)
        t180 = inline197
        var inline193 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t180)
        _goml_runtime_core_string_println(inline193)
        return struct{}{}
    case false:
        var t182 string
        var inline202 int = 3
        var inline203 string = _goml_runtime_core_int_to_string(inline202)
        t182 = inline203
        var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t182)
        _goml_runtime_core_string_println(inline199)
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
