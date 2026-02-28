package main

import (
    "fmt"
)

func unit_to_string(x struct{}) string {
    return "()"
}

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
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
    var jp8 Tuple2_bool_bool
    var b__1 Tuple2_bool_bool
    var x2 bool
    var x3 bool
    var w__2 bool
    var b_1__3 bool
    var mtmp4 Tuple2_bool_bool
    var x5 bool
    var x6 bool
    var c__4 struct{}
    var t10 string
    var t11 struct{}
    var t13 string
    var t14 struct{}
    var t15 string
    var t16 struct{}
    var t18 string
    var t19 struct{}
    var t20 string
    var t21 struct{}
    var jp23 Tuple2_bool_bool
    var t24 Tuple2_bool_bool
    var t25 Tuple2_bool_bool
    var jp27 Tuple2_bool_bool
    var t28 Tuple2_bool_bool
    var t29 Tuple2_bool_bool
    _ = x2
    _ = t14
    _ = t16
    _ = t19
    _ = t21
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
            switch x1 {
            case true:
                pc = 11
            case false:
                pc = 15
            default:
                panic("non-exhaustive match")
            }
        case 1:
            b__1 = jp8
            x3 = b__1._1
            w__2 = x3
            b_1__3 = w__2
            mtmp4 = Tuple2_bool_bool{
                _0: true,
                _1: b_1__3,
            }
            x5 = mtmp4._0
            x6 = mtmp4._1
            switch x6 {
            case true:
                pc = 3
            case false:
                pc = 7
            default:
                panic("non-exhaustive match")
            }
        case 2:
            c__4 = struct{}{}
            t10 = unit_to_string(c__4)
            t11 = string_print(t10)
            return t11
        case 3:
            switch x5 {
            case true:
                pc = 5
            case false:
                pc = 6
            default:
                panic("non-exhaustive match")
            }
        case 4:
            pc = 2
        case 5:
            t13 = int32_to_string(3)
            string_print(t13)
            pc = 4
        case 6:
            t15 = int32_to_string(1)
            string_print(t15)
            pc = 4
        case 7:
            switch x5 {
            case true:
                pc = 9
            case false:
                pc = 10
            default:
                panic("non-exhaustive match")
            }
        case 8:
            pc = 2
        case 9:
            t18 = int32_to_string(2)
            string_print(t18)
            pc = 8
        case 10:
            t20 = int32_to_string(0)
            string_print(t20)
            pc = 8
        case 11:
            switch x0 {
            case true:
                pc = 13
            case false:
                pc = 14
            default:
                panic("non-exhaustive match")
            }
        case 12:
            jp8 = jp23
            pc = 1
        case 13:
            t24 = Tuple2_bool_bool{
                _0: false,
                _1: false,
            }
            jp23 = t24
            pc = 12
        case 14:
            t25 = Tuple2_bool_bool{
                _0: true,
                _1: false,
            }
            jp23 = t25
            pc = 12
        case 15:
            switch x0 {
            case true:
                pc = 17
            case false:
                pc = 18
            default:
                panic("non-exhaustive match")
            }
        case 16:
            jp8 = jp27
            pc = 1
        case 17:
            t28 = Tuple2_bool_bool{
                _0: false,
                _1: true,
            }
            jp27 = t28
            pc = 16
        case 18:
            t29 = Tuple2_bool_bool{
                _0: true,
                _1: true,
            }
            jp27 = t29
            pc = 16
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
