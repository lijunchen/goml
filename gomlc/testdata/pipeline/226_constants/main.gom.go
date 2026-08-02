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

func _goml_runtime_core_uint8_to_string(x uint8) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

const (
    base int = 16
    answer int = base * 2 + 10
    mask uint8 = 240
    greeting string = "constant" + " value"
    enabled bool = true
)

func main0() struct{} {
    var t159 string
    var inline177 string = _goml_runtime_core_int_to_string(answer)
    t159 = inline177
    _goml_runtime_core_string_println(t159)
    var t160 string
    var inline175 string = _goml_runtime_core_uint8_to_string(mask)
    t160 = inline175
    _goml_runtime_core_string_println(t160)
    _goml_runtime_core_string_println(greeting)
    var t161 string
    var inline173 string = _goml_runtime_core_bool_to_string(enabled)
    t161 = inline173
    _goml_runtime_core_string_println(t161)
    return struct{}{}
}

func main() {
    main0()
}
