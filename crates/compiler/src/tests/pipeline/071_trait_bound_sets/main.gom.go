package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type S struct {}

func _goml_trait_impl_A_S_foo(self__0 S) string {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return "A"
        default:
            panic("invalid pc")
        }
    }
}

func _goml_trait_impl_C_S_bar(self__2 S) string {
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            return "C"
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var s__5 S
    var t2 string
    var mtmp0 struct{}
    var t3 string
    var mtmp1 struct{}
    _ = mtmp0
    _ = mtmp1
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            s__5 = S{}
            t2 = pick_a__T_S(s__5)
            println__T_string(t2)
            t3 = bar_it__T_S(s__5)
            println__T_string(t3)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func pick_a__T_S(x__3 S) string {
    var t4 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t4 = _goml_trait_impl_A_S_foo(x__3)
            return t4
        default:
            panic("invalid pc")
        }
    }
}

func println__T_string(value__1 string) struct{} {
    var t5 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t5 = string_println(value__1)
            return t5
        default:
            panic("invalid pc")
        }
    }
}

func bar_it__T_S(x__4 S) string {
    var t6 string
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t6 = _goml_trait_impl_C_S_bar(x__4)
            return t6
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
