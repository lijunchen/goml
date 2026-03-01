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
    var a__0 Tuple2_bool_bool = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    var x0 bool = a__0._0
    var x1 bool = a__0._1
    var jp9 Tuple2_bool_bool
    switch x1 {
    case true:
        var jp24 Tuple2_bool_bool
        switch x0 {
        case true:
            var t25 Tuple2_bool_bool = Tuple2_bool_bool{
                _0: false,
                _1: false,
            }
            jp24 = t25
        case false:
            var t26 Tuple2_bool_bool = Tuple2_bool_bool{
                _0: true,
                _1: false,
            }
            jp24 = t26
        default:
            panic("non-exhaustive match")
        }
        jp9 = jp24
        var b__1 Tuple2_bool_bool = jp9
        var x3 bool = b__1._1
        var w__2 bool = x3
        var b_1__3 bool = w__2
        var mtmp4 Tuple2_bool_bool = Tuple2_bool_bool{
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
                string_print(t14)
            case false:
                var t16 string = int32_to_string(1)
                string_print(t16)
            default:
                panic("non-exhaustive match")
            }
            var c__4 struct{} = struct{}{}
            var t11 string = unit_to_string(c__4)
            string_print(t11)
            return struct{}{}
        case false:
            switch x5 {
            case true:
                var t19 string = int32_to_string(2)
                string_print(t19)
            case false:
                var t21 string = int32_to_string(0)
                string_print(t21)
            default:
                panic("non-exhaustive match")
            }
            var c__4 struct{} = struct{}{}
            var t11 string = unit_to_string(c__4)
            string_print(t11)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    case false:
        var jp28 Tuple2_bool_bool
        switch x0 {
        case true:
            var t29 Tuple2_bool_bool = Tuple2_bool_bool{
                _0: false,
                _1: true,
            }
            jp28 = t29
        case false:
            var t30 Tuple2_bool_bool = Tuple2_bool_bool{
                _0: true,
                _1: true,
            }
            jp28 = t30
        default:
            panic("non-exhaustive match")
        }
        jp9 = jp28
        var b__1 Tuple2_bool_bool = jp9
        var x3 bool = b__1._1
        var w__2 bool = x3
        var b_1__3 bool = w__2
        var mtmp4 Tuple2_bool_bool = Tuple2_bool_bool{
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
                string_print(t14)
            case false:
                var t16 string = int32_to_string(1)
                string_print(t16)
            default:
                panic("non-exhaustive match")
            }
            var c__4 struct{} = struct{}{}
            var t11 string = unit_to_string(c__4)
            string_print(t11)
            return struct{}{}
        case false:
            switch x5 {
            case true:
                var t19 string = int32_to_string(2)
                string_print(t19)
            case false:
                var t21 string = int32_to_string(0)
                string_print(t21)
            default:
                panic("non-exhaustive match")
            }
            var c__4 struct{} = struct{}{}
            var t11 string = unit_to_string(c__4)
            string_print(t11)
            return struct{}{}
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
