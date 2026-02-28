package main

import (
    "fmt"
)

func int32_to_string(x int32) string {
    return fmt.Sprintf("%d", x)
}

func string_print(s string) struct{} {
    fmt.Print(s)
    return struct{}{}
}

func string_println(s string) struct{} {
    fmt.Println(s)
    return struct{}{}
}

type IntList interface {
    isIntList()
}

type Nil struct {}

func (_ Nil) isIntList() {}

type Cons struct {
    _0 int32
    _1 IntList
}

func (_ Cons) isIntList() {}

func print_int_list(xs__0 IntList) struct{} {
    var x0 int32
    var x1 IntList
    var xs__2 IntList
    var x__1 int32
    var t27 string
    switch xs__0.(type) {
    case Nil:
        goto b2
    case Cons:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return struct{}{}
    b2:
    string_print("Nil")
    goto b1
    b3:
    x0 = xs__0.(Cons)._0
    x1 = xs__0.(Cons)._1
    xs__2 = x1
    x__1 = x0
    string_print("Cons")
    string_print("(")
    t27 = int32_to_string(x__1)
    string_print(t27)
    string_print(", ")
    print_int_list(xs__2)
    string_print(")")
    goto b1
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var jp29 IntList
    var x8 int32
    var x9 IntList
    var tail__6 IntList
    var head__5 int32
    var t30 IntList
    var t31 IntList
    switch xs__3.(type) {
    case Nil:
        goto b2
    case Cons:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp29
    b2:
    jp29 = acc__4
    goto b1
    b3:
    x8 = xs__3.(Cons)._0
    x9 = xs__3.(Cons)._1
    tail__6 = x9
    head__5 = x8
    t30 = Cons{
        _0: head__5,
        _1: acc__4,
    }
    t31 = int_list_rev_aux(tail__6, t30)
    jp29 = t31
    goto b1
}

func int_list_rev(xs__7 IntList) IntList {
    var t32 IntList
    t32 = int_list_rev_aux(xs__7, Nil{})
    return t32
}

func int_list_length(xs__8 IntList) int32 {
    var jp34 int32
    var x11 IntList
    var xs__9 IntList
    var t35 int32
    var t36 int32
    switch xs__8.(type) {
    case Nil:
        goto b2
    case Cons:
        goto b3
    default:
        panic("non-exhaustive match")
    }
    b1:
    return jp34
    b2:
    jp34 = 0
    goto b1
    b3:
    x11 = xs__8.(Cons)._1
    xs__9 = x11
    t35 = int_list_length(xs__9)
    t36 = 1 + t35
    jp34 = t36
    goto b1
}

func print_int_list_length(xs__10 IntList) struct{} {
    var t37 int32
    var t38 string
    string_print("Length: ")
    t37 = int_list_length(xs__10)
    t38 = int32_to_string(t37)
    string_println(t38)
    return struct{}{}
}

func main0() struct{} {
    var x__11 IntList
    var x__12 IntList
    var t39 IntList
    var t40 IntList
    var x__13 IntList
    var y__14 IntList
    x__11 = Nil{}
    print_int_list(x__11)
    string_println("")
    print_int_list_length(x__11)
    x__12 = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    string_println("")
    print_int_list_length(x__12)
    t39 = Cons{
        _0: 3,
        _1: Nil{},
    }
    t40 = Cons{
        _0: 2,
        _1: t39,
    }
    x__13 = Cons{
        _0: 1,
        _1: t40,
    }
    print_int_list(x__13)
    string_println("")
    print_int_list_length(x__13)
    y__14 = int_list_rev(x__13)
    print_int_list(y__14)
    string_println("")
    return struct{}{}
}

func main() {
    main0()
}
