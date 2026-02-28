package main

import (
    "fmt"
)

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

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

func _goml_trait_impl_Arith_int32_add(self__0 int32, other__1 int32) int32 {
    var t4 int32
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t4 = self__0 + other__1
            return t4
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Arith_int32_less(self__2 int32, other__3 int32) bool {
    var t5 bool
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t5 = self__2 < other__3
            return t5
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Output_int32_output(self__4 int32) struct{} {
    var t6 string
    var t7 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t6 = int32_to_string(self__4)
            t7 = string_println(t6)
            return t7
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Output_bool_output(self__5 bool) struct{} {
    var t8 string
    var t9 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t8 = bool_to_string(self__5)
            t9 = string_println(t8)
            return t9
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var a__7 int32
    var b__8 int32
    var c__9 int32
    var mtmp0 struct{}
    var a__10 int32
    var b__11 int32
    var c__12 bool
    var mtmp1 struct{}
    var mtmp2 string
    var mtmp3 bool
    _ = mtmp0
    _ = mtmp1
    _ = mtmp2
    _ = mtmp3
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            a__7 = id__T_int32(1)
            b__8 = id__T_int32(2)
            c__9 = _goml_trait_impl_Arith_int32_add(a__7, b__8)
            _goml_trait_impl_Output_int32_output(c__9)
            a__10 = id__T_int32(3)
            b__11 = id__T_int32(4)
            c__12 = _goml_trait_impl_Arith_int32_less(a__10, b__11)
            _goml_trait_impl_Output_bool_output(c__12)
            id__T_string("abc")
            id__T_bool(true)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func id__T_int32(x__6 int32) int32 {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return x__6
        default:
            panic("invalid pc")
        }
    }
}

func id__T_string(x__6 string) string {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return x__6
        default:
            panic("invalid pc")
        }
    }
}

func id__T_bool(x__6 bool) bool {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return x__6
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
