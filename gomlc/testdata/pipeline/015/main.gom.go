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
        var inline246 string = "Nil"
        var inline247 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline246)
        _goml_runtime_core_string_println(inline247)
        return struct{}{}
    case Cons:
        var x187 int32 = xs__0.(Cons)._0
        var x188 IntList = xs__0.(Cons)._1
        var inline267 string = "Cons"
        var inline268 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline267)
        _goml_runtime_core_string_println(inline268)
        var inline263 string = "("
        var inline264 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline263)
        _goml_runtime_core_string_println(inline264)
        var t215 string
        var inline261 string = _goml_runtime_core_int32_to_string(x187)
        t215 = inline261
        var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t215)
        _goml_runtime_core_string_println(inline258)
        var inline254 string = ", "
        var inline255 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline254)
        _goml_runtime_core_string_println(inline255)
        print_int_list(x188)
        var inline250 string = ")"
        var inline251 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline250)
        _goml_runtime_core_string_println(inline251)
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
        var x195 int32 = xs__3.(Cons)._0
        var x196 IntList = xs__3.(Cons)._1
        var t220 IntList = Cons{
            _0: x195,
            _1: acc__4,
        }
        var t221 IntList = int_list_rev_aux(x196, t220)
        return t221
    default:
        panic("non-exhaustive match")
    }
}

func int_list_length(xs__8 IntList) int32 {
    switch xs__8.(type) {
    case Nil:
        return 0
    case Cons:
        var x198 IntList = xs__8.(Cons)._1
        var t229 int32 = int_list_length(x198)
        var t230 int32 = 1 + t229
        return t230
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    var inline309 string = ""
    var inline310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline309)
    _goml_runtime_core_string_println(inline310)
    println__T_string("Length: ")
    var inline305 int32 = int_list_length(x__11)
    var inline306 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline305)
    println__T_string(inline306)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    var inline300 string = ""
    var inline301 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline300)
    _goml_runtime_core_string_println(inline301)
    println__T_string("Length: ")
    var inline296 int32 = int_list_length(x__12)
    var inline297 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline296)
    println__T_string(inline297)
    var t235 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t236 IntList = Cons{
        _0: 2,
        _1: t235,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t236,
    }
    print_int_list(x__13)
    var inline291 string = ""
    var inline292 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline291)
    _goml_runtime_core_string_println(inline292)
    println__T_string("Length: ")
    var inline287 int32 = int_list_length(x__13)
    var inline288 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline287)
    println__T_string(inline288)
    var y__14 IntList
    var inline284 IntList = int_list_rev_aux(x__13, Nil{})
    y__14 = inline284
    print_int_list(y__14)
    var inline280 string = ""
    var inline281 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline280)
    _goml_runtime_core_string_println(inline281)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t238 string
    t238 = value__1
    _goml_runtime_core_string_println(t238)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t242 string = _goml_runtime_core_int32_to_string(self__33)
    return t242
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
