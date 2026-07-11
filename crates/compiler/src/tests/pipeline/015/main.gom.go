package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
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

func print_int_list(xs__0 IntList) struct{} {
    switch xs__0.(type) {
    case Nil:
        println__T_string("Nil")
    case Cons:
        var x22 int32 = xs__0.(Cons)._0
        var x23 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x23
        var x__1 int32 = x22
        println__T_string("Cons")
        println__T_string("(")
        var t50 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
        println__T_string(t50)
        println__T_string(", ")
        print_int_list(xs__2)
        println__T_string(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var retv52 IntList
    var jp54 IntList
    switch xs__3.(type) {
    case Nil:
        jp54 = acc__4
    case Cons:
        var x30 int32 = xs__3.(Cons)._0
        var x31 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x31
        var head__5 int32 = x30
        var t55 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t56 IntList = int_list_rev_aux(tail__6, t55)
        jp54 = t56
    default:
        panic("non-exhaustive match")
    }
    retv52 = jp54
    return retv52
}

func int_list_rev(xs__7 IntList) IntList {
    var retv58 IntList
    var t59 IntList = int_list_rev_aux(xs__7, Nil{})
    retv58 = t59
    return retv58
}

func int_list_length(xs__8 IntList) int32 {
    var retv61 int32
    var jp63 int32
    switch xs__8.(type) {
    case Nil:
        jp63 = 0
    case Cons:
        var x33 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x33
        var t64 int32 = int_list_length(xs__9)
        var t65 int32 = 1 + t64
        jp63 = t65
    default:
        panic("non-exhaustive match")
    }
    retv61 = jp63
    return retv61
}

func print_int_list_length(xs__10 IntList) struct{} {
    println__T_string("Length: ")
    var t67 int32 = int_list_length(xs__10)
    var t68 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t67)
    println__T_string(t68)
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
    var t70 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t71 IntList = Cons{
        _0: 2,
        _1: t70,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t71,
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
    var t73 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t73)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv76 string
    var t77 string = _goml_runtime_core_int32_to_string(self__2)
    retv76 = t77
    return retv76
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv79 string
    retv79 = self__9
    return retv79
}

func main() {
    main0()
}
