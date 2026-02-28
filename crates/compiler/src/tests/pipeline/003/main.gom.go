package main

import (
    "fmt"
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
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func main0() struct{} {
    print__T_unit(struct{}{})
    print__T_bool(true)
    print__T_bool(false)
    print__T_int32(123)
    return struct{}{}
}

func print__T_unit(value__0 struct{}) struct{} {
    var t4 string = unit_to_string(value__0)
    var t5 struct{} = string_print(t4)
    return t5
}

func print__T_bool(value__0 bool) struct{} {
    var t6 string = bool_to_string(value__0)
    var t7 struct{} = string_print(t6)
    return t7
}

func print__T_int32(value__0 int32) struct{} {
    var t8 string = int32_to_string(value__0)
    var t9 struct{} = string_print(t8)
    return t9
}

func main() {
    main0()
}
