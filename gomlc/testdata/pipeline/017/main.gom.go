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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_3int_3int struct {
    _0 int
    _1 int
}

func main0() struct{} {
    var x__1 int = 123
    var t161 string
    var inline188 string = _goml_runtime_core_int_to_string(x__1)
    t161 = inline188
    var inline185 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t161)
    _goml_runtime_core_string_println(inline185)
    var x__2 bool = true
    var t162 string
    var inline183 string = _goml_runtime_core_bool_to_string(x__2)
    t162 = inline183
    var inline180 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t162)
    _goml_runtime_core_string_println(inline180)
    var t163 string
    t163 = "(?, ?)"
    var inline176 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t163)
    _goml_runtime_core_string_println(inline176)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
