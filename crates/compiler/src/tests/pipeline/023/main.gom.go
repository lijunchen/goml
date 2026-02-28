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

type Tuple2_int32_string struct {
    _0 int32
    _1 string
}

type Mixed interface {
    isMixed()
}

type OnlyInt struct {
    _0 int32
}

func (_ OnlyInt) isMixed() {}

type OnlyStr struct {
    _0 string
}

func (_ OnlyStr) isMixed() {}

type Both struct {
    _0 int32
    _1 string
}

func (_ Both) isMixed() {}

func match_mixed_pair(pair__0 Tuple2_int32_string) int32 {
    var x0 int32
    var x1 string
    var jp19 int32
    var jp21 int32
    var jp23 int32
    var jp25 int32
    x0 = pair__0._0
    x1 = pair__0._1
    switch x1 {
    case "zero":
        goto b2
    case "one":
        goto b6
    default:
        goto b11
    }
    b1:
    return jp19
    b2:
    switch x0 {
    case 0:
        goto b4
    default:
        goto b5
    }
    b3:
    jp19 = jp21
    goto b1
    b4:
    jp21 = 1
    goto b3
    b5:
    jp21 = 4
    goto b3
    b6:
    switch x0 {
    case 0:
        goto b8
    case 1:
        goto b9
    default:
        goto b10
    }
    b7:
    jp19 = jp23
    goto b1
    b8:
    jp23 = 2
    goto b7
    b9:
    jp23 = 3
    goto b7
    b10:
    jp23 = 5
    goto b7
    b11:
    switch x0 {
    case 0:
        goto b13
    default:
        goto b14
    }
    b12:
    jp19 = jp25
    goto b1
    b13:
    jp25 = 2
    goto b12
    b14:
    jp25 = 5
    goto b12
}

func match_mixed_enum(value__1 Mixed) int32 {
    var jp27 int32
    var x2 int32
    var jp29 int32
    var x3 string
    var jp31 int32
    var x4 int32
    var x5 string
    var jp33 int32
    var jp35 int32
    var jp37 int32
    switch value__1.(type) {
    case OnlyInt:
        goto b2
    case OnlyStr:
        goto b6
    case Both:
        goto b10
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp27
    b2:
    x2 = value__1.(OnlyInt)._0
    switch x2 {
    case 0:
        goto b4
    default:
        goto b5
    }
    b3:
    jp27 = jp29
    goto b1
    b4:
    jp29 = 6
    goto b3
    b5:
    jp29 = 7
    goto b3
    b6:
    x3 = value__1.(OnlyStr)._0
    switch x3 {
    case "zero":
        goto b8
    default:
        goto b9
    }
    b7:
    jp27 = jp31
    goto b1
    b8:
    jp31 = 8
    goto b7
    b9:
    jp31 = 9
    goto b7
    b10:
    x4 = value__1.(Both)._0
    x5 = value__1.(Both)._1
    switch x5 {
    case "zero":
        goto b12
    default:
        goto b16
    }
    b11:
    jp27 = jp33
    goto b1
    b12:
    switch x4 {
    case 0:
        goto b14
    default:
        goto b15
    }
    b13:
    jp33 = jp35
    goto b11
    b14:
    jp35 = 10
    goto b13
    b15:
    jp35 = 12
    goto b13
    b16:
    switch x4 {
    case 0:
        goto b18
    default:
        goto b19
    }
    b17:
    jp33 = jp37
    goto b11
    b18:
    jp37 = 11
    goto b17
    b19:
    jp37 = 13
    goto b17
}

func main0() struct{} {
    var t38 Tuple2_int32_string
    var t39 int32
    var t40 string
    var t41 Tuple2_int32_string
    var t42 int32
    var t43 string
    var t44 Tuple2_int32_string
    var t45 int32
    var t46 string
    var t47 Tuple2_int32_string
    var t48 int32
    var t49 string
    var t50 Tuple2_int32_string
    var t51 int32
    var t52 string
    var t53 Mixed
    var t54 int32
    var t55 string
    var t56 Mixed
    var t57 int32
    var t58 string
    var t59 Mixed
    var t60 int32
    var t61 string
    var t62 Mixed
    var t63 int32
    var t64 string
    var t65 Mixed
    var t66 int32
    var t67 string
    var t68 Mixed
    var t69 int32
    var t70 string
    var t71 Mixed
    var t72 int32
    var t73 string
    var t74 Mixed
    var t75 int32
    var t76 string
    var t77 struct{}
    t38 = Tuple2_int32_string{
        _0: 0,
        _1: "zero",
    }
    t39 = match_mixed_pair(t38)
    t40 = int32_to_string(t39)
    string_println(t40)
    t41 = Tuple2_int32_string{
        _0: 0,
        _1: "other",
    }
    t42 = match_mixed_pair(t41)
    t43 = int32_to_string(t42)
    string_println(t43)
    t44 = Tuple2_int32_string{
        _0: 1,
        _1: "one",
    }
    t45 = match_mixed_pair(t44)
    t46 = int32_to_string(t45)
    string_println(t46)
    t47 = Tuple2_int32_string{
        _0: 2,
        _1: "zero",
    }
    t48 = match_mixed_pair(t47)
    t49 = int32_to_string(t48)
    string_println(t49)
    t50 = Tuple2_int32_string{
        _0: 2,
        _1: "two",
    }
    t51 = match_mixed_pair(t50)
    t52 = int32_to_string(t51)
    string_println(t52)
    t53 = OnlyInt{
        _0: 0,
    }
    t54 = match_mixed_enum(t53)
    t55 = int32_to_string(t54)
    string_println(t55)
    t56 = OnlyInt{
        _0: 5,
    }
    t57 = match_mixed_enum(t56)
    t58 = int32_to_string(t57)
    string_println(t58)
    t59 = OnlyStr{
        _0: "zero",
    }
    t60 = match_mixed_enum(t59)
    t61 = int32_to_string(t60)
    string_println(t61)
    t62 = OnlyStr{
        _0: "hello",
    }
    t63 = match_mixed_enum(t62)
    t64 = int32_to_string(t63)
    string_println(t64)
    t65 = Both{
        _0: 0,
        _1: "zero",
    }
    t66 = match_mixed_enum(t65)
    t67 = int32_to_string(t66)
    string_println(t67)
    t68 = Both{
        _0: 0,
        _1: "hello",
    }
    t69 = match_mixed_enum(t68)
    t70 = int32_to_string(t69)
    string_println(t70)
    t71 = Both{
        _0: 2,
        _1: "zero",
    }
    t72 = match_mixed_enum(t71)
    t73 = int32_to_string(t72)
    string_println(t73)
    t74 = Both{
        _0: 3,
        _1: "three",
    }
    t75 = match_mixed_enum(t74)
    t76 = int32_to_string(t75)
    t77 = string_println(t76)
    return t77
}

func main() {
    main0()
}
