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
        var inline231 string = "Nil"
        var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline231)
        _goml_runtime_core_string_println(inline232)
        return struct{}{}
    case Cons:
        var x172 int32 = xs__0.(Cons)._0
        var x173 IntList = xs__0.(Cons)._1
        var inline252 string = "Cons"
        var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline252)
        _goml_runtime_core_string_println(inline253)
        var inline248 string = "("
        var inline249 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline248)
        _goml_runtime_core_string_println(inline249)
        var t200 string
        var inline246 string = _goml_runtime_core_int32_to_string(x172)
        t200 = inline246
        var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
        _goml_runtime_core_string_println(inline243)
        var inline239 string = ", "
        var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline239)
        _goml_runtime_core_string_println(inline240)
        print_int_list(x173)
        var inline235 string = ")"
        var inline236 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline235)
        _goml_runtime_core_string_println(inline236)
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
        var x180 int32 = xs__3.(Cons)._0
        var x181 IntList = xs__3.(Cons)._1
        var t205 IntList = Cons{
            _0: x180,
            _1: acc__4,
        }
        var t206 IntList = int_list_rev_aux(x181, t205)
        return t206
    default:
        panic("non-exhaustive match")
    }
}

func int_list_length(xs__8 IntList) int32 {
    switch xs__8.(type) {
    case Nil:
        return 0
    case Cons:
        var x183 IntList = xs__8.(Cons)._1
        var t214 int32 = int_list_length(x183)
        var t215 int32 = 1 + t214
        return t215
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    var inline294 string = ""
    var inline295 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline294)
    _goml_runtime_core_string_println(inline295)
    println__T_string("Length: ")
    var inline290 int32 = int_list_length(x__11)
    var inline291 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline290)
    println__T_string(inline291)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    var inline285 string = ""
    var inline286 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline285)
    _goml_runtime_core_string_println(inline286)
    println__T_string("Length: ")
    var inline281 int32 = int_list_length(x__12)
    var inline282 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline281)
    println__T_string(inline282)
    var t220 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t221 IntList = Cons{
        _0: 2,
        _1: t220,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t221,
    }
    print_int_list(x__13)
    var inline276 string = ""
    var inline277 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline276)
    _goml_runtime_core_string_println(inline277)
    println__T_string("Length: ")
    var inline272 int32 = int_list_length(x__13)
    var inline273 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline272)
    println__T_string(inline273)
    var y__14 IntList
    var inline269 IntList = int_list_rev_aux(x__13, Nil{})
    y__14 = inline269
    print_int_list(y__14)
    var inline265 string = ""
    var inline266 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline265)
    _goml_runtime_core_string_println(inline266)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t223 string
    t223 = value__31
    _goml_runtime_core_string_println(t223)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t227 string = _goml_runtime_core_int32_to_string(self__35)
    return t227
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
