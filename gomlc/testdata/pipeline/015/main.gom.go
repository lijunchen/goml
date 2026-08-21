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

type Ordering int32

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
        var inline470 string = "Nil"
        var inline471 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline470)
        _goml_runtime_core_string_println(inline471)
        return struct{}{}
    case Cons:
        var x411 int32 = xs__0.(Cons)._0
        var x412 IntList = xs__0.(Cons)._1
        var inline491 string = "Cons"
        var inline492 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline491)
        _goml_runtime_core_string_println(inline492)
        var inline487 string = "("
        var inline488 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline487)
        _goml_runtime_core_string_println(inline488)
        var t439 string
        var inline485 string = _goml_runtime_core_int32_to_string(x411)
        t439 = inline485
        var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t439)
        _goml_runtime_core_string_println(inline482)
        var inline478 string = ", "
        var inline479 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline478)
        _goml_runtime_core_string_println(inline479)
        print_int_list(x412)
        var inline474 string = ")"
        var inline475 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline474)
        _goml_runtime_core_string_println(inline475)
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
        var x419 int32 = xs__3.(Cons)._0
        var x420 IntList = xs__3.(Cons)._1
        var t444 IntList = Cons{
            _0: x419,
            _1: acc__4,
        }
        var t445 IntList = int_list_rev_aux(x420, t444)
        return t445
    default:
        panic("non-exhaustive match")
    }
}

func int_list_length(xs__8 IntList) int32 {
    switch xs__8.(type) {
    case Nil:
        return 0
    case Cons:
        var x422 IntList = xs__8.(Cons)._1
        var t453 int32 = int_list_length(x422)
        var t454 int32 = 1 + t453
        return t454
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    var inline533 string = ""
    var inline534 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline533)
    _goml_runtime_core_string_println(inline534)
    println__T_string("Length: ")
    var inline529 int32 = int_list_length(x__11)
    var inline530 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline529)
    println__T_string(inline530)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    var inline524 string = ""
    var inline525 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline524)
    _goml_runtime_core_string_println(inline525)
    println__T_string("Length: ")
    var inline520 int32 = int_list_length(x__12)
    var inline521 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline520)
    println__T_string(inline521)
    var t459 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t460 IntList = Cons{
        _0: 2,
        _1: t459,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t460,
    }
    print_int_list(x__13)
    var inline515 string = ""
    var inline516 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline515)
    _goml_runtime_core_string_println(inline516)
    println__T_string("Length: ")
    var inline511 int32 = int_list_length(x__13)
    var inline512 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline511)
    println__T_string(inline512)
    var y__14 IntList
    var inline508 IntList = int_list_rev_aux(x__13, Nil{})
    y__14 = inline508
    print_int_list(y__14)
    var inline504 string = ""
    var inline505 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline504)
    _goml_runtime_core_string_println(inline505)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t462 string
    t462 = value__1
    _goml_runtime_core_string_println(t462)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t466 string = _goml_runtime_core_int32_to_string(self__33)
    return t466
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
