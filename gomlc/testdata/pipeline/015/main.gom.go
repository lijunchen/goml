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
        var x152 int32 = xs__0.(Cons)._0
        var x153 IntList = xs__0.(Cons)._1
        var xs__2 IntList = x153
        var x__1 int32 = x152
        println__T_string("Cons")
        println__T_string("(")
        var t180 string = _goml_m_inherent_i_int32_i_int32_i_to__string(x__1)
        println__T_string(t180)
        println__T_string(", ")
        print_int_list(xs__2)
        println__T_string(")")
    default:
        panic("non-exhaustive match")
    }
    return struct{}{}
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    var retv182 IntList
    var jp184 IntList
    switch xs__3.(type) {
    case Nil:
        jp184 = acc__4
    case Cons:
        var x160 int32 = xs__3.(Cons)._0
        var x161 IntList = xs__3.(Cons)._1
        var tail__6 IntList = x161
        var head__5 int32 = x160
        var t185 IntList = Cons{
            _0: head__5,
            _1: acc__4,
        }
        var t186 IntList = int_list_rev_aux(tail__6, t185)
        jp184 = t186
    default:
        panic("non-exhaustive match")
    }
    retv182 = jp184
    return retv182
}

func int_list_rev(xs__7 IntList) IntList {
    var retv188 IntList
    var t189 IntList = int_list_rev_aux(xs__7, Nil{})
    retv188 = t189
    return retv188
}

func int_list_length(xs__8 IntList) int32 {
    var retv191 int32
    var jp193 int32
    switch xs__8.(type) {
    case Nil:
        jp193 = 0
    case Cons:
        var x163 IntList = xs__8.(Cons)._1
        var xs__9 IntList = x163
        var t194 int32 = int_list_length(xs__9)
        var t195 int32 = 1 + t194
        jp193 = t195
    default:
        panic("non-exhaustive match")
    }
    retv191 = jp193
    return retv191
}

func print_int_list_length(xs__10 IntList) struct{} {
    println__T_string("Length: ")
    var t197 int32 = int_list_length(xs__10)
    var t198 string = _goml_m_inherent_i_int32_i_int32_i_to__string(t197)
    println__T_string(t198)
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
    var t200 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t201 IntList = Cons{
        _0: 2,
        _1: t200,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t201,
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
    var t203 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t203)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv206 string
    var t207 string = _goml_runtime_core_int32_to_string(self__6)
    retv206 = t207
    return retv206
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv209 string
    retv209 = self__38
    return retv209
}

func main() {
    main0()
}
