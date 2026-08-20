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
        var x415 int32 = other__11._v1_0
        var x416 int32 = other__11._v1_1
        switch self__10._tag {
        case 1:
            var x419 int32 = self__10._v1_0
            var x420 int32 = self__10._v1_1
            var jp472 bool
            var inline524 bool = x419 == x415
            jp472 = inline524
            if jp472 {
                var inline526 bool = x420 == x416
                return inline526
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
    var t485 int32
    var inline559 int32 = 4
    var inline562 int32 = x__22 + inline559
    t485 = inline562
    var inline555 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t485)
    _goml_runtime_core_string_println(inline555)
    var t486 string
    var inline552 string = "north"
    t486 = inline552
    var inline547 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t486)
    _goml_runtime_core_string_println(inline547)
    var t488 string
    var inline534 int32 = 1
    var inline535 int32 = 2
    var inline538 string = "Key::Point { " + "x: "
    var inline539 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline534)
    var inline540 string = inline538 + inline539
    var inline541 string = inline540 + ", "
    var inline542 string = inline541 + "y: "
    var inline543 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline535)
    var inline544 string = inline542 + inline543
    var inline545 string = inline544 + " }"
    t488 = inline545
    var inline531 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t488)
    _goml_runtime_core_string_println(inline531)
    var t489 Key = Key{
        _tag: 1,
        _v1_0: 1,
        _v1_1: 2,
    }
    var t490 Key = Key{
        _tag: 1,
        _v1_0: 1,
        _v1_1: 2,
    }
    var t491 bool = _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(t489, t490)
    var inline528 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t491)
    _goml_runtime_core_string_println(inline528)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t494 string = _goml_runtime_core_int32_to_string(self__154)
    return t494
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t514 string = _goml_runtime_core_bool_to_string(self__148)
    return t514
}

func main() {
    main0()
}
