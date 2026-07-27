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
        var x64 int32 = xs__0.(Cons)._0
        var x65 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x65
        var x__1 int32 = x64
        println__T_string("Cons")
        println__T_string("(")
        var t92 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
        println__T_string(t92)
        println__T_string(", ")
        print_int_list(xs__2)
        println__T_string(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var retv94 IntList
    var jp96 IntList
    switch xs__3.(type) {
    case Nil:
        jp96 = acc__4
    case Cons:
        var x72 int32 = xs__3.(Cons)._0
        var x73 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x73
        var head__5 int32 = x72
        var t97 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t98 IntList = int_list_rev_aux(tail__6, t97)
        jp96 = t98
    default:
        panic("non-exhaustive match")
    }
    retv94 = jp96
    return retv94
}

func int_list_rev(xs__7 IntList) IntList {
    var retv100 IntList
    var t101 IntList = int_list_rev_aux(xs__7, Nil{})
    retv100 = t101
    return retv100
}

func int_list_length(xs__8 IntList) int32 {
    var retv103 int32
    var jp105 int32
    switch xs__8.(type) {
    case Nil:
        jp105 = 0
    case Cons:
        var x75 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x75
        var t106 int32 = int_list_length(xs__9)
        var t107 int32 = 1 + t106
        jp105 = t107
    default:
        panic("non-exhaustive match")
    }
    retv103 = jp105
    return retv103
}

func print_int_list_length(xs__10 IntList) struct{} {
    println__T_string("Length: ")
    var t109 int32 = int_list_length(xs__10)
    var t110 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t109)
    println__T_string(t110)
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
    var t112 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t113 IntList = Cons{
        _0: 2,
        _1: t112,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t113,
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
    var t115 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t115)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv118 string
    var t119 string = _goml_runtime_core_int32_to_string(self__6)
    retv118 = t119
    return retv118
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv121 string
    retv121 = self__38
    return retv121
}

func main() {
    main0()
}
