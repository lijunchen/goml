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
        var x108 int32 = xs__0.(Cons)._0
        var x109 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x109
        var x__1 int32 = x108
        println__T_string("Cons")
        println__T_string("(")
        var t136 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
        println__T_string(t136)
        println__T_string(", ")
        print_int_list(xs__2)
        println__T_string(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var retv138 IntList
    var jp140 IntList
    switch xs__3.(type) {
    case Nil:
        jp140 = acc__4
    case Cons:
        var x116 int32 = xs__3.(Cons)._0
        var x117 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x117
        var head__5 int32 = x116
        var t141 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t142 IntList = int_list_rev_aux(tail__6, t141)
        jp140 = t142
    default:
        panic("non-exhaustive match")
    }
    retv138 = jp140
    return retv138
}

func int_list_rev(xs__7 IntList) IntList {
    var retv144 IntList
    var t145 IntList = int_list_rev_aux(xs__7, Nil{})
    retv144 = t145
    return retv144
}

func int_list_length(xs__8 IntList) int32 {
    var retv147 int32
    var jp149 int32
    switch xs__8.(type) {
    case Nil:
        jp149 = 0
    case Cons:
        var x119 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x119
        var t150 int32 = int_list_length(xs__9)
        var t151 int32 = 1 + t150
        jp149 = t151
    default:
        panic("non-exhaustive match")
    }
    retv147 = jp149
    return retv147
}

func print_int_list_length(xs__10 IntList) struct{} {
    println__T_string("Length: ")
    var t153 int32 = int_list_length(xs__10)
    var t154 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t153)
    println__T_string(t154)
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
    var t156 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t157 IntList = Cons{
        _0: 2,
        _1: t156,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t157,
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
    var t159 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t159)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv162 string
    var t163 string = _goml_runtime_core_int32_to_string(self__6)
    retv162 = t163
    return retv162
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv165 string
    retv165 = self__38
    return retv165
}

func main() {
    main0()
}
