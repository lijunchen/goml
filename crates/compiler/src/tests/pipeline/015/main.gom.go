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
        var x61 int32 = xs__0.(Cons)._0
        var x62 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x62
        var x__1 int32 = x61
        println__T_string("Cons")
        println__T_string("(")
        var t89 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
        println__T_string(t89)
        println__T_string(", ")
        print_int_list(xs__2)
        println__T_string(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var retv91 IntList
    var jp93 IntList
    switch xs__3.(type) {
    case Nil:
        jp93 = acc__4
    case Cons:
        var x69 int32 = xs__3.(Cons)._0
        var x70 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x70
        var head__5 int32 = x69
        var t94 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t95 IntList = int_list_rev_aux(tail__6, t94)
        jp93 = t95
    default:
        panic("non-exhaustive match")
    }
    retv91 = jp93
    return retv91
}

func int_list_rev(xs__7 IntList) IntList {
    var retv97 IntList
    var t98 IntList = int_list_rev_aux(xs__7, Nil{})
    retv97 = t98
    return retv97
}

func int_list_length(xs__8 IntList) int32 {
    var retv100 int32
    var jp102 int32
    switch xs__8.(type) {
    case Nil:
        jp102 = 0
    case Cons:
        var x72 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x72
        var t103 int32 = int_list_length(xs__9)
        var t104 int32 = 1 + t103
        jp102 = t104
    default:
        panic("non-exhaustive match")
    }
    retv100 = jp102
    return retv100
}

func print_int_list_length(xs__10 IntList) struct{} {
    println__T_string("Length: ")
    var t106 int32 = int_list_length(xs__10)
    var t107 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t106)
    println__T_string(t107)
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
    var t109 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t110 IntList = Cons{
        _0: 2,
        _1: t109,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t110,
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
    var t112 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t112)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv115 string
    var t116 string = _goml_runtime_core_int32_to_string(self__5)
    retv115 = t116
    return retv115
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv118 string
    retv118 = self__37
    return retv118
}

func main() {
    main0()
}
