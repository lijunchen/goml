package main

import (
    _goml_fmt "fmt"
)

func unit_to_string(x struct{}) string {
    return "()"
}

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Tuple2_4bool_4bool struct {
    _0 bool
    _1 bool
}

type GoError = error

func main0() struct{} {
    var a__0 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: false,
    }
    var x0 bool = a__0._0
    var x1 bool = a__0._1
    var jp9 Tuple2_4bool_4bool
    switch x1 {
    case true:
        var jp24 Tuple2_4bool_4bool
        switch x0 {
        case true:
            var t25 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: false,
            }
            jp24 = t25
        case false:
            var t26 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: false,
            }
            jp24 = t26
        default:
            panic("non-exhaustive match")
        }
        jp9 = jp24
    case false:
        var jp28 Tuple2_4bool_4bool
        switch x0 {
        case true:
            var t29 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: false,
                _1: true,
            }
            jp28 = t29
        case false:
            var t30 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
                _0: true,
                _1: true,
            }
            jp28 = t30
        default:
            panic("non-exhaustive match")
        }
        jp9 = jp28
    default:
        panic("non-exhaustive match")
    }
    var b__1 Tuple2_4bool_4bool = jp9
    var x3 bool = b__1._1
    var w__2 bool = x3
    var b_1__3 bool = w__2
    var mtmp4 Tuple2_4bool_4bool = Tuple2_4bool_4bool{
        _0: true,
        _1: b_1__3,
    }
    var x5 bool = mtmp4._0
    var x6 bool = mtmp4._1
    switch x6 {
    case true:
        switch x5 {
        case true:
            var t14 string = int32_to_string(3)
            println__T_string(t14)
        case false:
            var t16 string = int32_to_string(1)
            println__T_string(t16)
        default:
            panic("non-exhaustive match")
        }
    case false:
        switch x5 {
        case true:
            var t19 string = int32_to_string(2)
            println__T_string(t19)
        case false:
            var t21 string = int32_to_string(0)
            println__T_string(t21)
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
    var c__4 struct{} = struct{}{}
    var t11 string = unit_to_string(c__4)
    println__T_string(t11)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
