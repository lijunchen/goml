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
        var x58 int32 = xs__0.(Cons)._0
        var x59 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x59
        var x__1 int32 = x58
        println__T_string("Cons")
        println__T_string("(")
        var t86 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
        println__T_string(t86)
        println__T_string(", ")
        print_int_list(xs__2)
        println__T_string(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var retv88 IntList
    var jp90 IntList
    switch xs__3.(type) {
    case Nil:
        jp90 = acc__4
    case Cons:
        var x66 int32 = xs__3.(Cons)._0
        var x67 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x67
        var head__5 int32 = x66
        var t91 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t92 IntList = int_list_rev_aux(tail__6, t91)
        jp90 = t92
    default:
        panic("non-exhaustive match")
    }
    retv88 = jp90
    return retv88
}

func int_list_rev(xs__7 IntList) IntList {
    var retv94 IntList
    var t95 IntList = int_list_rev_aux(xs__7, Nil{})
    retv94 = t95
    return retv94
}

func int_list_length(xs__8 IntList) int32 {
    var retv97 int32
    var jp99 int32
    switch xs__8.(type) {
    case Nil:
        jp99 = 0
    case Cons:
        var x69 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x69
        var t100 int32 = int_list_length(xs__9)
        var t101 int32 = 1 + t100
        jp99 = t101
    default:
        panic("non-exhaustive match")
    }
    retv97 = jp99
    return retv97
}

func print_int_list_length(xs__10 IntList) struct{} {
    println__T_string("Length: ")
    var t103 int32 = int_list_length(xs__10)
    var t104 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t103)
    println__T_string(t104)
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
    var t106 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t107 IntList = Cons{
        _0: 2,
        _1: t106,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t107,
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
    var t109 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t109)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__2 int32) string {
    var retv112 string
    var t113 string = _goml_runtime_core_int32_to_string(self__2)
    retv112 = t113
    return retv112
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv115 string
    retv115 = self__34
    return retv115
}

func main() {
    main0()
}
