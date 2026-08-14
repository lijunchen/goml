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

type Point struct {
    x int32
    y int32
}

type Ordering int32

type Message interface {
    isMessage()
}

type Quit struct {}

func (_ Quit) isMessage() {}

type Move struct {
    _0 int32
    _1 int32
}

func (_ Move) isMessage() {}

type Write struct {
    _0 string
}

func (_ Write) isMessage() {}

func _goml_m_trait__impl_i_ToString_i_Message_i_to__string(self__3 Message) string {
    switch self__3.(type) {
    case Quit:
        return "Message::Quit"
    case Move:
        var x411 int32 = self__3.(Move)._0
        var x412 int32 = self__3.(Move)._1
        var t432 string
        var inline458 string = _goml_runtime_core_int32_to_string(x411)
        t432 = inline458
        var t433 string = "Message::Move(" + t432
        var t434 string = t433 + ", "
        var t435 string
        var inline456 string = _goml_runtime_core_int32_to_string(x412)
        t435 = inline456
        var t436 string = t434 + t435
        var t437 string = t436 + ")"
        return t437
    case Write:
        var x413 string = self__3.(Write)._0
        var t438 string = "Message::Write(" + x413
        var t439 string = t438 + ")"
        return t439
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var summary__8 string
    var inline473 int32 = 4
    var inline474 int32 = 7
    var inline477 string = "Point { " + "x: "
    var inline478 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline473)
    var inline479 string = inline477 + inline478
    var inline480 string = inline479 + ", "
    var inline481 string = inline480 + "y: "
    var inline482 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline474)
    var inline483 string = inline481 + inline482
    var inline484 string = inline483 + " }"
    summary__8 = inline484
    var t441 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t441)
    var t442 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t442)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    var inline469 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(summary__8)
    _goml_runtime_core_string_println(inline469)
    var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(mv__9)
    _goml_runtime_core_string_println(inline466)
    var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__10)
    _goml_runtime_core_string_println(inline463)
    var inline460 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(exit__11)
    _goml_runtime_core_string_println(inline460)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t445 string = _goml_runtime_core_int32_to_string(self__154)
    return t445
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
