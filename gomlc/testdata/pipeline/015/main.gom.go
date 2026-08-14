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
        var inline241 string = "Nil"
        var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline241)
        _goml_runtime_core_string_println(inline242)
        return struct{}{}
    case Cons:
        var x182 int32 = xs__0.(Cons)._0
        var x183 IntList = xs__0.(Cons)._1
        var inline262 string = "Cons"
        var inline263 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline262)
        _goml_runtime_core_string_println(inline263)
        var inline258 string = "("
        var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline258)
        _goml_runtime_core_string_println(inline259)
        var t210 string
        var inline256 string = _goml_runtime_core_int32_to_string(x182)
        t210 = inline256
        var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t210)
        _goml_runtime_core_string_println(inline253)
        var inline249 string = ", "
        var inline250 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline249)
        _goml_runtime_core_string_println(inline250)
        print_int_list(x183)
        var inline245 string = ")"
        var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline245)
        _goml_runtime_core_string_println(inline246)
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
        var x190 int32 = xs__3.(Cons)._0
        var x191 IntList = xs__3.(Cons)._1
        var t215 IntList = Cons{
            _0: x190,
            _1: acc__4,
        }
        var t216 IntList = int_list_rev_aux(x191, t215)
        return t216
    default:
        panic("non-exhaustive match")
    }
}

func int_list_length(xs__8 IntList) int32 {
    switch xs__8.(type) {
    case Nil:
        return 0
    case Cons:
        var x193 IntList = xs__8.(Cons)._1
        var t224 int32 = int_list_length(x193)
        var t225 int32 = 1 + t224
        return t225
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    var inline304 string = ""
    var inline305 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline304)
    _goml_runtime_core_string_println(inline305)
    println__T_string("Length: ")
    var inline300 int32 = int_list_length(x__11)
    var inline301 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline300)
    println__T_string(inline301)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    var inline295 string = ""
    var inline296 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline295)
    _goml_runtime_core_string_println(inline296)
    println__T_string("Length: ")
    var inline291 int32 = int_list_length(x__12)
    var inline292 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline291)
    println__T_string(inline292)
    var t230 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t231 IntList = Cons{
        _0: 2,
        _1: t230,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t231,
    }
    print_int_list(x__13)
    var inline286 string = ""
    var inline287 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline286)
    _goml_runtime_core_string_println(inline287)
    println__T_string("Length: ")
    var inline282 int32 = int_list_length(x__13)
    var inline283 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline282)
    println__T_string(inline283)
    var y__14 IntList
    var inline279 IntList = int_list_rev_aux(x__13, Nil{})
    y__14 = inline279
    print_int_list(y__14)
    var inline275 string = ""
    var inline276 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline275)
    _goml_runtime_core_string_println(inline276)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t233 string
    t233 = value__1
    _goml_runtime_core_string_println(t233)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t237 string = _goml_runtime_core_int32_to_string(self__33)
    return t237
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
