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
    var ret18 struct{}
    print__T_unit(struct{}{})
    print__T_bool(true)
    print__T_bool(false)
    print__T_int32(123)
    ret18 = struct{}{}
    return ret18
}

func print__T_unit(value__0 struct{}) struct{} {
    var ret19 struct{}
    var t15 string = unit_to_string(value__0)
    ret19 = string_print(t15)
    return ret19
}

func print__T_bool(value__0 bool) struct{} {
    var ret20 struct{}
    var t16 string = bool_to_string(value__0)
    ret20 = string_print(t16)
    return ret20
}

func print__T_int32(value__0 int32) struct{} {
    var ret21 struct{}
    var t17 string = int32_to_string(value__0)
    ret21 = string_print(t17)
    return ret21
}

func main() {
    main0()
}
