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
        var inline236 string = "Nil"
        var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline236)
        _goml_runtime_core_string_println(inline237)
        return struct{}{}
    case Cons:
        var x177 int32 = xs__0.(Cons)._0
        var x178 IntList = xs__0.(Cons)._1
        var inline257 string = "Cons"
        var inline258 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline257)
        _goml_runtime_core_string_println(inline258)
        var inline253 string = "("
        var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline253)
        _goml_runtime_core_string_println(inline254)
        var t205 string
        var inline251 string = _goml_runtime_core_int32_to_string(x177)
        t205 = inline251
        var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t205)
        _goml_runtime_core_string_println(inline248)
        var inline244 string = ", "
        var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline244)
        _goml_runtime_core_string_println(inline245)
        print_int_list(x178)
        var inline240 string = ")"
        var inline241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline240)
        _goml_runtime_core_string_println(inline241)
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
        var x185 int32 = xs__3.(Cons)._0
        var x186 IntList = xs__3.(Cons)._1
        var t210 IntList = Cons{
            _0: x185,
            _1: acc__4,
        }
        var t211 IntList = int_list_rev_aux(x186, t210)
        return t211
    default:
        panic("non-exhaustive match")
    }
}

func int_list_length(xs__8 IntList) int32 {
    switch xs__8.(type) {
    case Nil:
        return 0
    case Cons:
        var x188 IntList = xs__8.(Cons)._1
        var t219 int32 = int_list_length(x188)
        var t220 int32 = 1 + t219
        return t220
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    var inline299 string = ""
    var inline300 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline299)
    _goml_runtime_core_string_println(inline300)
    println__T_string("Length: ")
    var inline295 int32 = int_list_length(x__11)
    var inline296 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline295)
    println__T_string(inline296)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    var inline290 string = ""
    var inline291 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline290)
    _goml_runtime_core_string_println(inline291)
    println__T_string("Length: ")
    var inline286 int32 = int_list_length(x__12)
    var inline287 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline286)
    println__T_string(inline287)
    var t225 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t226 IntList = Cons{
        _0: 2,
        _1: t225,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t226,
    }
    print_int_list(x__13)
    var inline281 string = ""
    var inline282 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline281)
    _goml_runtime_core_string_println(inline282)
    println__T_string("Length: ")
    var inline277 int32 = int_list_length(x__13)
    var inline278 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline277)
    println__T_string(inline278)
    var y__14 IntList
    var inline274 IntList = int_list_rev_aux(x__13, Nil{})
    y__14 = inline274
    print_int_list(y__14)
    var inline270 string = ""
    var inline271 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline270)
    _goml_runtime_core_string_println(inline271)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t228 string
    t228 = value__31
    _goml_runtime_core_string_println(t228)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t232 string = _goml_runtime_core_int32_to_string(self__35)
    return t232
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
