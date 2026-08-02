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
        var inline214 string = "Nil"
        var inline215 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline214)
        _goml_runtime_core_string_println(inline215)
        return struct{}{}
    case Cons:
        var x155 int32 = xs__0.(Cons)._0
        var x156 IntList = xs__0.(Cons)._1
        var inline235 string = "Cons"
        var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline235)
        _goml_runtime_core_string_println(inline236)
        var inline231 string = "("
        var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline231)
        _goml_runtime_core_string_println(inline232)
        var t183 string
        var inline229 string = _goml_runtime_core_int32_to_string(x155)
        t183 = inline229
        var inline226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
        _goml_runtime_core_string_println(inline226)
        var inline222 string = ", "
        var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline222)
        _goml_runtime_core_string_println(inline223)
        print_int_list(x156)
        var inline218 string = ")"
        var inline219 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline218)
        _goml_runtime_core_string_println(inline219)
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
        var x163 int32 = xs__3.(Cons)._0
        var x164 IntList = xs__3.(Cons)._1
        var t188 IntList = Cons{
            _0: x163,
            _1: acc__4,
        }
        var t189 IntList = int_list_rev_aux(x164, t188)
        return t189
    default:
        panic("non-exhaustive match")
    }
}

func int_list_length(xs__8 IntList) int32 {
    switch xs__8.(type) {
    case Nil:
        return 0
    case Cons:
        var x166 IntList = xs__8.(Cons)._1
        var t197 int32 = int_list_length(x166)
        var t198 int32 = 1 + t197
        return t198
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    var inline277 string = ""
    var inline278 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline277)
    _goml_runtime_core_string_println(inline278)
    println__T_string("Length: ")
    var inline273 int32 = int_list_length(x__11)
    var inline274 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline273)
    println__T_string(inline274)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    var inline268 string = ""
    var inline269 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline268)
    _goml_runtime_core_string_println(inline269)
    println__T_string("Length: ")
    var inline264 int32 = int_list_length(x__12)
    var inline265 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline264)
    println__T_string(inline265)
    var t203 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t204 IntList = Cons{
        _0: 2,
        _1: t203,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t204,
    }
    print_int_list(x__13)
    var inline259 string = ""
    var inline260 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline259)
    _goml_runtime_core_string_println(inline260)
    println__T_string("Length: ")
    var inline255 int32 = int_list_length(x__13)
    var inline256 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline255)
    println__T_string(inline256)
    var y__14 IntList
    var inline252 IntList = int_list_rev_aux(x__13, Nil{})
    y__14 = inline252
    print_int_list(y__14)
    var inline248 string = ""
    var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline248)
    _goml_runtime_core_string_println(inline249)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t206 string
    t206 = value__1
    _goml_runtime_core_string_println(t206)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t210 string = _goml_runtime_core_int32_to_string(self__6)
    return t210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
