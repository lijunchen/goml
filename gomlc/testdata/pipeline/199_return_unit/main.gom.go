package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func main0() struct{} {
    _goml_runtime_core_string_println("continued")
    return struct{}{}
}

func main() {
    main0()
}
