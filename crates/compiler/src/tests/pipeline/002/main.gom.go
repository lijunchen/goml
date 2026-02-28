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
    var t15 string
    var t18 string
    var t20 string
    var jp23 Tuple2_bool_bool
    var t24 Tuple2_bool_bool
    var t25 Tuple2_bool_bool
    var jp27 Tuple2_bool_bool
    var t28 Tuple2_bool_bool
    var t29 Tuple2_bool_bool
    a__0 = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    x0 = a__0._0
    x1 = a__0._1
    switch x1 {
    case true:
        goto b11
    case false:
        goto b15
    default:
        panic("non-exhaustive match")
    }
    b1:
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
        goto b3
    case false:
        goto b7
    default:
        panic("non-exhaustive match")
    }
    b2:
    c__4 = struct{}{}
    t10 = unit_to_string(c__4)
    t11 = string_print(t10)
    return t11
    b3:
    switch x5 {
    case true:
        goto b5
    case false:
        goto b6
    default:
        panic("non-exhaustive match")
    }
    b4:
    goto b2
    b5:
    t13 = int32_to_string(3)
    string_print(t13)
    goto b4
    b6:
    t15 = int32_to_string(1)
    string_print(t15)
    goto b4
    b7:
    switch x5 {
    case true:
        goto b9
    case false:
        goto b10
    default:
        panic("non-exhaustive match")
    }
    b8:
    goto b2
    b9:
    t18 = int32_to_string(2)
    string_print(t18)
    goto b8
    b10:
    t20 = int32_to_string(0)
    string_print(t20)
    goto b8
    b11:
    switch x0 {
    case true:
        goto b13
    case false:
        goto b14
    default:
        panic("non-exhaustive match")
    }
    b12:
    jp8 = jp23
    goto b1
    b13:
    t24 = Tuple2_bool_bool{
        _0: false,
        _1: false,
    }
    jp23 = t24
    goto b12
    b14:
    t25 = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    jp23 = t25
    goto b12
    b15:
    switch x0 {
    case true:
        goto b17
    case false:
        goto b18
    default:
        panic("non-exhaustive match")
    }
    b16:
    jp8 = jp27
    goto b1
    b17:
    t28 = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    jp27 = t28
    goto b16
    b18:
    t29 = Tuple2_bool_bool{
        _0: true,
        _1: true,
    }
    jp27 = t29
    goto b16
}

func main() {
    main0()
}
