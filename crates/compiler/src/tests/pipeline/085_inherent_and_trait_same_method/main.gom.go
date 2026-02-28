package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Boxed struct {
    value int32
}

func _goml_inherent_Boxed_Boxed_format(self__0 Boxed) string {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return "inherent"
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_Render_Boxed_format(self__1 Boxed) string {
    var t2 int32
    var t3 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t2 = self__1.value
            t3 = int32_to_string(t2)
            return t3
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var t4 Boxed
    var t5 string
    var _wild0 struct{}
    var t6 Boxed
    var t7 string
    var _wild1 struct{}
    _ = _wild0
    _ = _wild1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t4 = Boxed{
                value: 9,
            }
            t5 = _goml_inherent_Boxed_Boxed_format(t4)
            println__T_string(t5)
            t6 = Boxed{
                value: 9,
            }
            t7 = _goml_trait_impl_Render_Boxed_format(t6)
            println__T_string(t7)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t8 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t8 = string_println(value__1)
            return t8
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
