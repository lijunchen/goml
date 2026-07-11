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
        var x4 int32 = xs__0.(Cons)._0
        var x5 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x5
        var x__1 int32 = x4
        println__T_string("Cons")
        println__T_string("(")
        var t32 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
        println__T_string(t32)
        println__T_string(", ")
        print_int_list(xs__2)
        println__T_string(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var retv34 IntList
    var jp36 IntList
    switch xs__3.(type) {
    case Nil:
        jp36 = acc__4
    case Cons:
        var x12 int32 = xs__3.(Cons)._0
        var x13 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x13
        var head__5 int32 = x12
        var t37 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t38 IntList = int_list_rev_aux(tail__6, t37)
        jp36 = t38
    default:
        panic("non-exhaustive match")
    }
    retv34 = jp36
    return retv34
}

func int_list_rev(xs__7 IntList) IntList {
    var retv40 IntList
    var t41 IntList = int_list_rev_aux(xs__7, Nil{})
    retv40 = t41
    return retv40
}

func int_list_length(xs__8 IntList) int32 {
    var retv43 int32
    var jp45 int32
    switch xs__8.(type) {
    case Nil:
        jp45 = 0
    case Cons:
        var x15 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x15
        var t46 int32 = int_list_length(xs__9)
        var t47 int32 = 1 + t46
        jp45 = t47
    default:
        panic("non-exhaustive match")
    }
    retv43 = jp45
    return retv43
}

func print_int_list_length(xs__10 IntList) struct{} {
    println__T_string("Length: ")
    var t49 int32 = int_list_length(xs__10)
    var t50 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t49)
    println__T_string(t50)
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
    var t52 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t53 IntList = Cons{
        _0: 2,
        _1: t52,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t53,
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
    var t55 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t55)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv58 string
    var t59 string = _goml_runtime_core_int32_to_string(self__2)
    retv58 = t59
    return retv58
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv61 string
    retv61 = self__9
    return retv61
}

func main() {
    main0()
}
