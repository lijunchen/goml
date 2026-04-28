package main

import (
    _goml_fmt "fmt"
)

func unit_to_string(x struct{}) string {
    return "()"
}

func bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    _goml_fmt.Print(s)
    return struct{}{}
}

type GoError = error

func main0() struct{} {
    print__T_unit(struct{}{})
    print__T_bool(true)
    print__T_bool(false)
    print__T_int32(123)
    return struct{}{}
}

func print__T_unit(value__0 struct{}) struct{} {
    var t6 string = unit_to_string(value__0)
    string_print(t6)
    return struct{}{}
}

func print__T_bool(value__0 bool) struct{} {
    var t9 string = bool_to_string(value__0)
    string_print(t9)
    return struct{}{}
}

func print__T_int32(value__0 int32) struct{} {
    var t12 string = int32_to_string(value__0)
    string_print(t12)
    return struct{}{}
}

func main() {
    main0()
}
