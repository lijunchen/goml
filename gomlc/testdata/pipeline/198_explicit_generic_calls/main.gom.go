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

func main0() struct{} {
    var t157 string
    var inline173 string = "direct"
    t157 = inline173
    _goml_runtime_core_string_println(t157)
    var t158 int32
    var inline171 int32 = 42
    t158 = inline171
    var t159 string
    var inline169 string = _goml_runtime_core_int32_to_string(t158)
    t159 = inline169
    _goml_runtime_core_string_println(t159)
    return struct{}{}
}

func main() {
    main0()
}
