package main

import (
    "fmt"
)

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type Tuple2_bool_bool struct {
    _0 bool
    _1 bool
}

func test_nested_match(x__0 Tuple2_bool_bool, y__1 Tuple2_bool_bool) struct{} {
    var x0 bool
    var x1 bool
    var x2 bool
    var x3 bool
    var x4 bool
    var x5 bool
    var x6 bool
    var x7 bool
    x0 = x__0._0
    x1 = x__0._1
    switch x1 {
    case true:
        goto b2
    case false:
        goto b9
    default:
        panic("non-exhaustive match")
    }
    b1:
    return struct{}{}
    b2:
    x2 = y__1._0
    x3 = y__1._1
    switch x3 {
    case true:
        goto b4
    case false:
        goto b8
    default:
        panic("non-exhaustive match")
    }
    b3:
    goto b1
    b4:
    switch x2 {
    case true:
        goto b6
    case false:
        goto b7
    default:
        panic("non-exhaustive match")
    }
    b5:
    goto b3
    b6:
    string_println("case4")
    goto b5
    b7:
    string_println("case3")
    goto b5
    b8:
    string_println("case4")
    goto b3
    b9:
    switch x0 {
    case true:
        goto b11
    case false:
        goto b18
    default:
        panic("non-exhaustive match")
    }
    b10:
    goto b1
    b11:
    x4 = y__1._0
    x5 = y__1._1
    switch x5 {
    case true:
        goto b13
    case false:
        goto b17
    default:
        panic("non-exhaustive match")
    }
    b12:
    goto b10
    b13:
    switch x4 {
    case true:
        goto b15
    case false:
        goto b16
    default:
        panic("non-exhaustive match")
    }
    b14:
    goto b12
    b15:
    string_println("case2")
    goto b14
    b16:
    string_println("case1")
    goto b14
    b17:
    string_println("case2")
    goto b12
    b18:
    x6 = y__1._0
    x7 = y__1._1
    switch x7 {
    case true:
        goto b20
    case false:
        goto b24
    default:
        panic("non-exhaustive match")
    }
    b19:
    goto b10
    b20:
    switch x6 {
    case true:
        goto b22
    case false:
        goto b23
    default:
        panic("non-exhaustive match")
    }
    b21:
    goto b19
    b22:
    string_println("case4")
    goto b21
    b23:
    string_println("case3")
    goto b21
    b24:
    string_println("case4")
    goto b19
}

func main0() struct{} {
    var t29 Tuple2_bool_bool
    var t30 Tuple2_bool_bool
    var t31 Tuple2_bool_bool
    var t32 Tuple2_bool_bool
    var t33 Tuple2_bool_bool
    var t34 Tuple2_bool_bool
    var t35 Tuple2_bool_bool
    var t36 Tuple2_bool_bool
    t29 = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    t30 = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t29, t30)
    t31 = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    t32 = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t31, t32)
    t33 = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    t34 = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    test_nested_match(t33, t34)
    t35 = Tuple2_bool_bool{
        _0: false,
        _1: true,
    }
    t36 = Tuple2_bool_bool{
        _0: true,
        _1: false,
    }
    test_nested_match(t35, t36)
    return struct{}{}
}

func main() {
    main0()
}
