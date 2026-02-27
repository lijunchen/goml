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
    var ret7 struct{}
    print__T_unit(struct{}{})
    print__T_bool(true)
    print__T_bool(false)
    print__T_int32(123)
    ret7 = struct{}{}
    return ret7
}

func print__T_unit(value__0 struct{}) struct{} {
    var ret8 struct{}
    var t4 string = unit_to_string(value__0)
    ret8 = string_print(t4)
    return ret8
}

func print__T_bool(value__0 bool) struct{} {
    var ret9 struct{}
    var t5 string = bool_to_string(value__0)
    ret9 = string_print(t5)
    return ret9
}

func print__T_int32(value__0 int32) struct{} {
    var ret10 struct{}
    var t6 string = int32_to_string(value__0)
    ret10 = string_print(t6)
    return ret10
}

func main() {
    main0()
}
