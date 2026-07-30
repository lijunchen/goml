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
        var x68 int32 = xs__0.(Cons)._0
        var x69 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x69
        var x__1 int32 = x68
        println__T_string("Cons")
        println__T_string("(")
        var t96 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
        println__T_string(t96)
        println__T_string(", ")
        print_int_list(xs__2)
        println__T_string(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var retv98 IntList
    var jp100 IntList
    switch xs__3.(type) {
    case Nil:
        jp100 = acc__4
    case Cons:
        var x76 int32 = xs__3.(Cons)._0
        var x77 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x77
        var head__5 int32 = x76
        var t101 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t102 IntList = int_list_rev_aux(tail__6, t101)
        jp100 = t102
    default:
        panic("non-exhaustive match")
    }
    retv98 = jp100
    return retv98
}

func int_list_rev(xs__7 IntList) IntList {
    var retv104 IntList
    var t105 IntList = int_list_rev_aux(xs__7, Nil{})
    retv104 = t105
    return retv104
}

func int_list_length(xs__8 IntList) int32 {
    var retv107 int32
    var jp109 int32
    switch xs__8.(type) {
    case Nil:
        jp109 = 0
    case Cons:
        var x79 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x79
        var t110 int32 = int_list_length(xs__9)
        var t111 int32 = 1 + t110
        jp109 = t111
    default:
        panic("non-exhaustive match")
    }
    retv107 = jp109
    return retv107
}

func print_int_list_length(xs__10 IntList) struct{} {
    println__T_string("Length: ")
    var t113 int32 = int_list_length(xs__10)
    var t114 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t113)
    println__T_string(t114)
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
    var t116 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t117 IntList = Cons{
        _0: 2,
        _1: t116,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t117,
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
    var t119 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t119)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv122 string
    var t123 string = _goml_runtime_core_int32_to_string(self__6)
    retv122 = t123
    return retv122
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv125 string
    retv125 = self__38
    return retv125
}

func main() {
    main0()
}
