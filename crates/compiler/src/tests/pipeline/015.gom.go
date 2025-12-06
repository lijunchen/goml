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
    var ret35 struct{}
    switch xs__0 := xs__0.(type) {
    case Nil:
        ret35 = string_print("Nil")
    case Cons:
        var x0 int32 = xs__0._0
        var x1 IntList = xs__0._1
        var xs__2 IntList = x1
        var x__1 int32 = x0
        string_print("Cons")
        string_print("(")
        var t25 string = int32_to_string(x__1)
        string_print(t25)
        string_print(", ")
        print_int_list(xs__2)
        string_print(")")
        ret35 = struct{}{}
    }
    return ret35
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var ret36 IntList
    switch xs__3 := xs__3.(type) {
    case Nil:
        ret36 = acc__4
    case Cons:
        var x8 int32 = xs__3._0
        var x9 IntList = xs__3._1
        var tail__6 IntList = x9
        var head__5 int32 = x8
        var t26 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        ret36 = int_list_rev_aux(tail__6, t26)
    }
    return ret36
}

func int_list_rev(xs__7 IntList) IntList {
    var ret37 IntList
    var t27 IntList = Nil{}
    ret37 = int_list_rev_aux(xs__7, t27)
    return ret37
}

func int_list_length(xs__8 IntList) int32 {
    var ret38 int32
    switch xs__8 := xs__8.(type) {
    case Nil:
        ret38 = 0
    case Cons:
        var x11 IntList = xs__8._1
        var xs__9 IntList = x11
        var t28 int32 = int_list_length(xs__9)
        ret38 = 1 + t28
    }
    return ret38
}

func print_int_list_length(xs__10 IntList) struct{} {
    var ret39 struct{}
    string_print("Length: ")
    var t30 int32 = int_list_length(xs__10)
    var t29 string = int32_to_string(t30)
    string_println(t29)
    ret39 = struct{}{}
    return ret39
}

func main0() struct{} {
    var ret40 struct{}
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    string_println("")
    print_int_list_length(x__11)
    var t31 IntList = Nil{}
    var x__12 IntList = Cons{
        _0: 1,
        _1: t31,
    }
    print_int_list(x__12)
    string_println("")
    print_int_list_length(x__12)
    var t34 IntList = Nil{}
    var t33 IntList = Cons{
        _0: 3,
        _1: t34,
    }
    var t32 IntList = Cons{
        _0: 2,
        _1: t33,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t32,
    }
    print_int_list(x__13)
    string_println("")
    print_int_list_length(x__13)
    var y__14 IntList = int_list_rev(x__13)
    print_int_list(y__14)
    string_println("")
    ret40 = struct{}{}
    return ret40
}

func main() {
    main0()
}
