package main

import (
    _goml_fmt "fmt"
)

func int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func string_println(s string) struct{} {
    _goml_fmt.Println(s)
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

type GoError = error

func print_int_list(xs__0 IntList) struct{} {
    switch xs__0.(type) {
    case Nil:
        println__T_string("Nil")
    case Cons:
        var x0 int32 = xs__0.(Cons)._0
        var x1 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x1
        var x__1 int32 = x0
        println__T_string("Cons")
        println__T_string("(")
        var t28 string = int32_to_string(x__1)
        println__T_string(t28)
        println__T_string(", ")
        print_int_list(xs__2)
        println__T_string(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var retv30 IntList
    var jp32 IntList
    switch xs__3.(type) {
    case Nil:
        jp32 = acc__4
    case Cons:
        var x8 int32 = xs__3.(Cons)._0
        var x9 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x9
        var head__5 int32 = x8
        var t33 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t34 IntList = int_list_rev_aux(tail__6, t33)
        jp32 = t34
    default:
        panic("non-exhaustive match")
    }
    retv30 = jp32
    return retv30
}

func int_list_rev(xs__7 IntList) IntList {
    var retv36 IntList
    var t37 IntList = int_list_rev_aux(xs__7, Nil{})
    retv36 = t37
    return retv36
}

func int_list_length(xs__8 IntList) int32 {
    var retv39 int32
    var jp41 int32
    switch xs__8.(type) {
    case Nil:
        jp41 = 0
    case Cons:
        var x11 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x11
        var t42 int32 = int_list_length(xs__9)
        var t43 int32 = 1 + t42
        jp41 = t43
    default:
        panic("non-exhaustive match")
    }
    retv39 = jp41
    return retv39
}

func print_int_list_length(xs__10 IntList) struct{} {
    println__T_string("Length: ")
    var t45 int32 = int_list_length(xs__10)
    var t46 string = int32_to_string(t45)
    println__T_string(t46)
    return struct{}{}
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    println__T_string("")
    print_int_list_length(x__11)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    println__T_string("")
    print_int_list_length(x__12)
    var t48 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t49 IntList = Cons{
        _0: 2,
        _1: t48,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t49,
    }
    print_int_list(x__13)
    println__T_string("")
    print_int_list_length(x__13)
    var y__14 IntList = int_list_rev(x__13)
    print_int_list(y__14)
    println__T_string("")
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    string_println(value__1)
    return struct{}{}
}

func main() {
    main0()
}
