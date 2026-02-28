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
    var jp8 Tuple2_bool_bool
    switch x1 {
    case true:
        var jp23 Tuple2_bool_bool
        switch x0 {
        case true:
            var t24 Tuple2_bool_bool = Tuple2_bool_bool{
                _0: false,
                _1: false,
            }
            jp23 = t24
        case false:
            var t25 Tuple2_bool_bool = Tuple2_bool_bool{
                _0: true,
                _1: false,
            }
            jp23 = t25
        default:
            panic("non-exhaustive match")
        }
        jp8 = jp23
        var b__1 Tuple2_bool_bool = jp8
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
                var t13 string = int32_to_string(3)
                string_print(t13)
            case false:
                var t15 string = int32_to_string(1)
                string_print(t15)
            default:
                panic("non-exhaustive match")
            }
            var c__4 struct{} = struct{}{}
            var t10 string = unit_to_string(c__4)
            var t11 struct{} = string_print(t10)
            return t11
        case false:
            switch x5 {
            case true:
                var t18 string = int32_to_string(2)
                string_print(t18)
            case false:
                var t20 string = int32_to_string(0)
                string_print(t20)
            default:
                panic("non-exhaustive match")
            }
            var c__4 struct{} = struct{}{}
            var t10 string = unit_to_string(c__4)
            var t11 struct{} = string_print(t10)
            return t11
        default:
            panic("non-exhaustive match")
        }
    case false:
        var jp27 Tuple2_bool_bool
        switch x0 {
        case true:
            var t28 Tuple2_bool_bool = Tuple2_bool_bool{
                _0: false,
                _1: true,
            }
            jp27 = t28
        case false:
            var t29 Tuple2_bool_bool = Tuple2_bool_bool{
                _0: true,
                _1: true,
            }
            jp27 = t29
        default:
            panic("non-exhaustive match")
        }
        jp8 = jp27
        var b__1 Tuple2_bool_bool = jp8
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
                var t13 string = int32_to_string(3)
                string_print(t13)
            case false:
                var t15 string = int32_to_string(1)
                string_print(t15)
            default:
                panic("non-exhaustive match")
            }
            var c__4 struct{} = struct{}{}
            var t10 string = unit_to_string(c__4)
            var t11 struct{} = string_print(t10)
            return t11
        case false:
            switch x5 {
            case true:
                var t18 string = int32_to_string(2)
                string_print(t18)
            case false:
                var t20 string = int32_to_string(0)
                string_print(t20)
            default:
                panic("non-exhaustive match")
            }
            var c__4 struct{} = struct{}{}
            var t10 string = unit_to_string(c__4)
            var t11 struct{} = string_print(t10)
            return t11
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
