package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Ordering int32

type Key struct {
    _tag int32
    _v1_0 int32
    _v1_1 int32
}

type Message__string interface {
    isMessage__string()
}

type Quit struct {}

func (_ Quit) isMessage__string() {}

type Write struct {
    _0 string
}

func (_ Write) isMessage__string() {}

type Move struct {
    _0 int32
    _1 int32
    _2 string
}

func (_ Move) isMessage__string() {}

func _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(self__10 Key, other__11 Key) bool {
    switch other__11._tag {
    case 0:
        switch self__10._tag {
        case 0:
            return true
        default:
            return false
        }
    case 1:
        var x418 int32 = other__11._v1_0
        var x419 int32 = other__11._v1_1
        switch self__10._tag {
        case 1:
            var x422 int32 = self__10._v1_0
            var x423 int32 = self__10._v1_1
            var jp475 bool
            var inline527 bool = x422 == x418
            jp475 = inline527
            if jp475 {
                var inline529 bool = x423 == x419
                return inline529
            } else {
                return false
            }
        default:
            return false
        }
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var x__22 int32 = 3
    var t488 int32
    var inline562 int32 = 4
    var inline565 int32 = x__22 + inline562
    t488 = inline565
    var inline558 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t488)
    _goml_runtime_core_string_println(inline558)
    var t489 string
    var inline555 string = "north"
    t489 = inline555
    var inline550 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t489)
    _goml_runtime_core_string_println(inline550)
    var t491 string
    var inline537 int32 = 1
    var inline538 int32 = 2
    var inline541 string = "Key::Point { " + "x: "
    var inline542 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline537)
    var inline543 string = inline541 + inline542
    var inline544 string = inline543 + ", "
    var inline545 string = inline544 + "y: "
    var inline546 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline538)
    var inline547 string = inline545 + inline546
    var inline548 string = inline547 + " }"
    t491 = inline548
    var inline534 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t491)
    _goml_runtime_core_string_println(inline534)
    var t492 Key = Key{
        _tag: 1,
        _v1_0: 1,
        _v1_1: 2,
    }
    var t493 Key = Key{
        _tag: 1,
        _v1_0: 1,
        _v1_1: 2,
    }
    var t494 bool = _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(t492, t493)
    var inline531 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t494)
    _goml_runtime_core_string_println(inline531)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t497 string = _goml_runtime_core_int32_to_string(self__154)
    return t497
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t517 string = _goml_runtime_core_bool_to_string(self__148)
    return t517
}

func main() {
    main0()
}
