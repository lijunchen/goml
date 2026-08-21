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

type Ordering int32

func main0() struct{} {
    var x__0 int = 1
    var t414 int = x__0 + 1
    x__0 = t414
    var t415 string
    var inline428 string = _goml_runtime_core_int_to_string(x__0)
    t415 = inline428
    var inline425 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t415)
    _goml_runtime_core_string_println(inline425)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
