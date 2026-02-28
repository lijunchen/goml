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

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

type Tuple2_bool_bool struct {
    _0 bool
    _1 bool
}

func main0() struct{} {
    var a__0 Tuple2_bool_bool
    var x0 bool
    var x1 bool
    var mtmp2 struct{}
    var c__2 Tuple2_bool_bool
    var x3 bool
    var x4 bool
    var d__3 bool
    var t7 struct{}
    var b__1 bool
    var t8 struct{}
    _ = mtmp2
    _ = t7
    _ = t8
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            a__0 = Tuple2_bool_bool{
                _0: true,
                _1: false,
            }
            x0 = a__0._0
            x1 = a__0._1
            switch x0 {
            case true:
                pc = 5
            case false:
                pc = 6
            default:
                panic("non-exhaustive match")
            }
        case 1:
            c__2 = Tuple2_bool_bool{
                _0: true,
                _1: true,
            }
            x3 = c__2._0
            x4 = c__2._1
            switch x3 {
            case true:
                pc = 3
            case false:
                pc = 4
            default:
                panic("non-exhaustive match")
            }
        case 2:
            return struct{}{}
        case 3:
            d__3 = x4
            print__T_bool(d__3)
            pc = 2
        case 4:
            pc = 2
        case 5:
            b__1 = x1
            print__T_bool(b__1)
            pc = 1
        case 6:
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func print__T_bool(value__0 bool) struct{} {
    var t9 string
    var t10 struct{}
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t9 = bool_to_string(value__0)
            t10 = string_print(t9)
            return t10
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
