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
        var inline467 string = "Nil"
        var inline468 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline467)
        _goml_runtime_core_string_println(inline468)
        return struct{}{}
    case Cons:
        var x408 int32 = xs__0.(Cons)._0
        var x409 IntList = xs__0.(Cons)._1
        var inline488 string = "Cons"
        var inline489 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline488)
        _goml_runtime_core_string_println(inline489)
        var inline484 string = "("
        var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline484)
        _goml_runtime_core_string_println(inline485)
        var t436 string
        var inline482 string = _goml_runtime_core_int32_to_string(x408)
        t436 = inline482
        var inline479 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
        _goml_runtime_core_string_println(inline479)
        var inline475 string = ", "
        var inline476 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline475)
        _goml_runtime_core_string_println(inline476)
        print_int_list(x409)
        var inline471 string = ")"
        var inline472 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline471)
        _goml_runtime_core_string_println(inline472)
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
        var x416 int32 = xs__3.(Cons)._0
        var x417 IntList = xs__3.(Cons)._1
        var t441 IntList = Cons{
            _0: x416,
            _1: acc__4,
        }
        var t442 IntList = int_list_rev_aux(x417, t441)
        return t442
    default:
        panic("non-exhaustive match")
    }
}

func int_list_length(xs__8 IntList) int32 {
    switch xs__8.(type) {
    case Nil:
        return 0
    case Cons:
        var x419 IntList = xs__8.(Cons)._1
        var t450 int32 = int_list_length(x419)
        var t451 int32 = 1 + t450
        return t451
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__11 IntList = Nil{}
    print_int_list(x__11)
    var inline530 string = ""
    var inline531 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline530)
    _goml_runtime_core_string_println(inline531)
    println__T_string("Length: ")
    var inline526 int32 = int_list_length(x__11)
    var inline527 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline526)
    println__T_string(inline527)
    var x__12 IntList = Cons{
        _0: 1,
        _1: Nil{},
    }
    print_int_list(x__12)
    var inline521 string = ""
    var inline522 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline521)
    _goml_runtime_core_string_println(inline522)
    println__T_string("Length: ")
    var inline517 int32 = int_list_length(x__12)
    var inline518 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline517)
    println__T_string(inline518)
    var t456 IntList = Cons{
        _0: 3,
        _1: Nil{},
    }
    var t457 IntList = Cons{
        _0: 2,
        _1: t456,
    }
    var x__13 IntList = Cons{
        _0: 1,
        _1: t457,
    }
    print_int_list(x__13)
    var inline512 string = ""
    var inline513 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline512)
    _goml_runtime_core_string_println(inline513)
    println__T_string("Length: ")
    var inline508 int32 = int_list_length(x__13)
    var inline509 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline508)
    println__T_string(inline509)
    var y__14 IntList
    var inline505 IntList = int_list_rev_aux(x__13, Nil{})
    y__14 = inline505
    print_int_list(y__14)
    var inline501 string = ""
    var inline502 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline501)
    _goml_runtime_core_string_println(inline502)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t459 string
    t459 = value__1
    _goml_runtime_core_string_println(t459)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__33 int32) string {
    var t463 string = _goml_runtime_core_int32_to_string(self__33)
    return t463
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
