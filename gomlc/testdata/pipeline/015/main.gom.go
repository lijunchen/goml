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
        var inline195 string = "Nil"
        var inline196 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline195)
        _goml_runtime_core_string_println(inline196)
        return struct{}{}
    case Cons:
        var x136 int32 = xs__0.(Cons)._0
        var x137 IntList = xs__0.(Cons)._1
        var inline216 string = "Cons"
        var inline217 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline216)
        _goml_runtime_core_string_println(inline217)
        var inline212 string = "("
        var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline212)
        _goml_runtime_core_string_println(inline213)
        var t164 string
        var inline210 string = _goml_runtime_core_int32_to_string(x136)
        t164 = inline210
        var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t164)
        _goml_runtime_core_string_println(inline207)
        var inline203 string = ", "
        var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline203)
        _goml_runtime_core_string_println(inline204)
        print_int_list(x137)
        var inline199 string = ")"
        var inline200 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline199)
        _goml_runtime_core_string_println(inline200)
        return struct{}{}
    default:
        panic("non-exhaustive match")
    }
}

func int_list_rev_aux(xs__3 IntList, acc__4 IntList) IntList {
    switch xs__3.(type) {
    case Nil:
        return acc__4
    case Cons:
        var x144 int32 = xs__3.(Cons)._0
        var x145 IntList = xs__3.(Cons)._1
        var t169 IntList = Cons{
            _0: x144,
            _1: acc__4,
        }
        var t170 IntList = int_list_rev_aux(x145, t169)
        return t170
    default:
        panic("non-exhaustive match")
    }
}

func int_list_length(xs__8 IntList) int32 {
    switch xs__8.(type) {
    case Nil:
        return 0
    case Cons:
        var x147 IntList = xs__8.(Cons)._1
        var t178 int32 = int_list_length(x147)
        var t179 int32 = 1 + t178
        return t179
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    var inline258 string = ""
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline258)
    _goml_runtime_core_string_println(inline259)
    println__T_string("Length: ")
    var inline254 int32 = int_list_length(x__11)
    var inline255 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline254)
    println__T_string(inline255)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    var inline249 string = ""
    var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline249)
    _goml_runtime_core_string_println(inline250)
    println__T_string("Length: ")
    var inline245 int32 = int_list_length(x__12)
    var inline246 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline245)
    println__T_string(inline246)
    var t184 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t185 IntList = Cons{
        _0: 2,
        _1: t184,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t185,
    }
    print_int_list(x__13)
    var inline240 string = ""
    var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline240)
    _goml_runtime_core_string_println(inline241)
    println__T_string("Length: ")
    var inline236 int32 = int_list_length(x__13)
    var inline237 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline236)
    println__T_string(inline237)
    var y__14 IntList
    var inline233 IntList = int_list_rev_aux(x__13, Nil{})
    y__14 = inline233
    print_int_list(y__14)
    var inline229 string = ""
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline229)
    _goml_runtime_core_string_println(inline230)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t187 string
    t187 = value__31
    _goml_runtime_core_string_println(t187)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t191 string = _goml_runtime_core_int32_to_string(self__35)
    return t191
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
