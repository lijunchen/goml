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
    var t26 struct{}
    var x0 int32
    var x1 IntList
    var xs__2 IntList
    var x__1 int32
    var mtmp2 struct{}
    var mtmp3 struct{}
    var t27 string
    var mtmp4 struct{}
    var mtmp5 struct{}
    var mtmp6 struct{}
    var mtmp7 struct{}
    _ = t26
    _ = mtmp2
    _ = mtmp3
    _ = mtmp4
    _ = mtmp5
    _ = mtmp6
    _ = mtmp7
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch xs__0.(type) {
            case Nil:
                pc = 2
            case Cons:
                pc = 3
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return struct{}{}
        case 2:
            string_print("Nil")
            pc = 1
        case 3:
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
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var jp29 IntList
    var x8 int32
    var x9 IntList
    var tail__6 IntList
    var head__5 int32
    var t30 IntList
    var t31 IntList
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch xs__3.(type) {
            case Nil:
                pc = 2
            case Cons:
                pc = 3
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp29
        case 2:
            jp29 = acc__4
            pc = 1
        case 3:
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
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func int_list_rev(xs__7 IntList) IntList {
    var t32 IntList
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            t32 = int_list_rev_aux(xs__7, Nil{})
            return t32
        default:
            panic("invalid pc")
        }
    }
}

func int_list_length(xs__8 IntList) int32 {
    var jp34 int32
    var x10 int32
    var x11 IntList
    var xs__9 IntList
    var t35 int32
    var t36 int32
    _ = x10
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            switch xs__8.(type) {
            case Nil:
                pc = 2
            case Cons:
                pc = 3
            default:
                panic("non-exhaustive match")
            }
        case 1:
            return jp34
        case 2:
            jp34 = 0
            pc = 1
        case 3:
            x11 = xs__8.(Cons)._1
            xs__9 = x11
            t35 = int_list_length(xs__9)
            t36 = 1 + t35
            jp34 = t36
            pc = 1
        default:
            panic("invalid pc")
        }
    }
}

func print_int_list_length(xs__10 IntList) struct{} {
    var mtmp12 struct{}
    var t37 int32
    var t38 string
    var mtmp13 struct{}
    _ = mtmp12
    _ = mtmp13
    var pc int32 = 0
    for {
        switch pc {
        case 0:
            string_print("Length: ")
            t37 = int_list_length(xs__10)
            t38 = int32_to_string(t37)
            string_println(t38)
            return struct{}{}
        default:
            panic("invalid pc")
        }
    }
}

func main0() struct{} {
    var x__11 IntList
    var mtmp14 struct{}
    var mtmp15 struct{}
    var mtmp16 struct{}
    var x__12 IntList
    var mtmp17 struct{}
    var mtmp18 struct{}
    var mtmp19 struct{}
    var t39 IntList
    var t40 IntList
    var x__13 IntList
    var mtmp20 struct{}
    var mtmp21 struct{}
    var mtmp22 struct{}
    var y__14 IntList
    var mtmp23 struct{}
    var mtmp24 struct{}
    _ = mtmp14
    _ = mtmp15
    _ = mtmp16
    _ = mtmp17
    _ = mtmp18
    _ = mtmp19
    _ = mtmp20
    _ = mtmp21
    _ = mtmp22
    _ = mtmp23
    _ = mtmp24
    var pc int32 = 0
    for {
        switch pc {
        case 0:
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
        default:
            panic("invalid pc")
        }
    }
}

func main() {
    main0()
}
