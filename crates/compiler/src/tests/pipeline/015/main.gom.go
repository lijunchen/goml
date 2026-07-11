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
        var x7 int32 = xs__0.(Cons)._0
        var x8 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x8
        var x__1 int32 = x7
        println__T_string("Cons")
        println__T_string("(")
        var t35 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
        println__T_string(t35)
        println__T_string(", ")
        print_int_list(xs__2)
        println__T_string(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var retv37 IntList
    var jp39 IntList
    switch xs__3.(type) {
    case Nil:
        jp39 = acc__4
    case Cons:
        var x15 int32 = xs__3.(Cons)._0
        var x16 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x16
        var head__5 int32 = x15
        var t40 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t41 IntList = int_list_rev_aux(tail__6, t40)
        jp39 = t41
    default:
        panic("non-exhaustive match")
    }
    retv37 = jp39
    return retv37
}

func int_list_rev(xs__7 IntList) IntList {
    var retv43 IntList
    var t44 IntList = int_list_rev_aux(xs__7, Nil{})
    retv43 = t44
    return retv43
}

func int_list_length(xs__8 IntList) int32 {
    var retv46 int32
    var jp48 int32
    switch xs__8.(type) {
    case Nil:
        jp48 = 0
    case Cons:
        var x18 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x18
        var t49 int32 = int_list_length(xs__9)
        var t50 int32 = 1 + t49
        jp48 = t50
    default:
        panic("non-exhaustive match")
    }
    retv46 = jp48
    return retv46
}

func print_int_list_length(xs__10 IntList) struct{} {
    println__T_string("Length: ")
    var t52 int32 = int_list_length(xs__10)
    var t53 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t52)
    println__T_string(t53)
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
    var t55 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t56 IntList = Cons{
        _0: 2,
        _1: t55,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t56,
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
    var t58 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t58)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv61 string
    var t62 string = _goml_runtime_core_int32_to_string(self__2)
    retv61 = t62
    return retv61
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__9 string) string {
    var retv64 string
    retv64 = self__9
    return retv64
}

func main() {
    main0()
}
