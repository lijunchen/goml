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
    var ret2129 struct{}
    switch xs__0 := xs__0.(type) {
    case Nil:
        ret2129 = string_print("Nil")
    case Cons:
        var x0 int32 = xs__0._0
        var x1 IntList = xs__0._1
        var xs__2 IntList = x1
        var x__1 int32 = x0
        string_print("Cons")
        string_print("(")
        var t2119 string = int32_to_string(x__1)
        string_print(t2119)
        string_print(", ")
        print_int_list(xs__2)
        string_print(")")
        ret2129 = struct{}{}
    }
    return ret2129
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var ret2130 IntList
    switch xs__3 := xs__3.(type) {
    case Nil:
        ret2130 = acc__4
    case Cons:
        var x65 int32 = xs__3._0
        var x66 IntList = xs__3._1
        var tail__6 IntList = x66
        var head__5 int32 = x65
        var t2120 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        ret2130 = int_list_rev_aux(tail__6, t2120)
    }
    return ret2130
}

func int_list_rev(xs__7 IntList) IntList {
    var ret2131 IntList
    var t2121 IntList = Nil{}
    ret2131 = int_list_rev_aux(xs__7, t2121)
    return ret2131
}

func int_list_length(xs__8 IntList) int32 {
    var ret2132 int32
    switch xs__8 := xs__8.(type) {
    case Nil:
        ret2132 = 0
    case Cons:
        var x68 IntList = xs__8._1
        var xs__9 IntList = x68
        var t2122 int32 = int_list_length(xs__9)
        ret2132 = 1 + t2122
    }
    return ret2132
}

func print_int_list_length(xs__10 IntList) struct{} {
    var ret2133 struct{}
    string_print("Length: ")
    var t2124 int32 = int_list_length(xs__10)
    var t2123 string = int32_to_string(t2124)
    string_println(t2123)
    ret2133 = struct{}{}
    return ret2133
}

func main0() struct{} {
    var ret2134 struct{}
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    string_println("")
    print_int_list_length(x__11)
    var t2125 IntList = Nil{}
    var x__12 IntList = Cons{
        _0: 1,
        _1: t2125,
    }
    print_int_list(x__12)
    string_println("")
    print_int_list_length(x__12)
    var t2128 IntList = Nil{}
    var t2127 IntList = Cons{
        _0: 3,
        _1: t2128,
    }
    var t2126 IntList = Cons{
        _0: 2,
        _1: t2127,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t2126,
    }
    print_int_list(x__13)
    string_println("")
    print_int_list_length(x__13)
    var y__14 IntList = int_list_rev(x__13)
    print_int_list(y__14)
    string_println("")
    ret2134 = struct{}{}
    return ret2134
}

func main() {
    main0()
}
