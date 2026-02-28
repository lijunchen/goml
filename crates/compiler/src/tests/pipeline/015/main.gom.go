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
    switch xs__0.(type) {
    case Nil:
        string_print("Nil")
    case Cons:
        var x0 int32 = xs__0.(Cons)._0
        var x1 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x1
        var x__1 int32 = x0
        string_print("Cons")
        string_print("(")
        var t27 string = int32_to_string(x__1)
        string_print(t27)
        string_print(", ")
        print_int_list(xs__2)
        string_print(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var jp29 IntList
    switch xs__3.(type) {
    case Nil:
        jp29 = acc__4
    case Cons:
        var x8 int32 = xs__3.(Cons)._0
        var x9 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x9
        var head__5 int32 = x8
        var t30 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t31 IntList = int_list_rev_aux(tail__6, t30)
        jp29 = t31
    default:
        panic("non-exhaustive match")
    }
    return jp29
}

func int_list_rev(xs__7 IntList) IntList {
    var t32 IntList = int_list_rev_aux(xs__7, Nil{})
    return t32
}

func int_list_length(xs__8 IntList) int32 {
    var jp34 int32
    switch xs__8.(type) {
    case Nil:
        jp34 = 0
    case Cons:
        var x11 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x11
        var t35 int32 = int_list_length(xs__9)
        var t36 int32 = 1 + t35
        jp34 = t36
    default:
        panic("non-exhaustive match")
    }
    return jp34
}

func print_int_list_length(xs__10 IntList) struct{} {
    string_print("Length: ")
    var t37 int32 = int_list_length(xs__10)
    var t38 string = int32_to_string(t37)
    string_println(t38)
    return struct{}{}
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    string_println("")
    print_int_list_length(x__11)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    string_println("")
    print_int_list_length(x__12)
    var t39 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t40 IntList = Cons{
        _0: 2,
        _1: t39,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t40,
    }
    print_int_list(x__13)
    string_println("")
    print_int_list_length(x__13)
    var y__14 IntList = int_list_rev(x__13)
    print_int_list(y__14)
    string_println("")
    return struct{}{}
}

func main() {
    main0()
}
