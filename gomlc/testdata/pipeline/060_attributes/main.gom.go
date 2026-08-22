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
        var x414 int32 = self__3.(Move)._0
        var x415 int32 = self__3.(Move)._1
        var t435 string
        var inline461 string = _goml_runtime_core_int32_to_string(x414)
        t435 = inline461
        var t436 string = "Message::Move(" + t435
        var t437 string = t436 + ", "
        var t438 string
        var inline459 string = _goml_runtime_core_int32_to_string(x415)
        t438 = inline459
        var t439 string = t437 + t438
        var t440 string = t439 + ")"
        return t440
    case Write:
        var x416 string = self__3.(Write)._0
        var t441 string = "Message::Write(" + x416
        var t442 string = t441 + ")"
        return t442
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var summary__8 string
    var inline476 int32 = 4
    var inline477 int32 = 7
    var inline480 string = "Point { " + "x: "
    var inline481 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(inline476)
    var inline482 string = inline480 + inline481
    var inline483 string = inline482 + ", "
    var inline484 string = inline483 + "y: "
    var inline485 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(inline477)
    var inline486 string = inline484 + inline485
    var inline487 string = inline486 + " }"
    summary__8 = inline487
    var t444 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t444)
    var t445 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t445)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    var inline472 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(summary__8)
    _goml_runtime_core_string_println(inline472)
    var inline469 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(mv__9)
    _goml_runtime_core_string_println(inline469)
    var inline466 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__10)
    _goml_runtime_core_string_println(inline466)
    var inline463 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(exit__11)
    _goml_runtime_core_string_println(inline463)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t448 string = _goml_runtime_core_int32_to_string(self__154)
    return t448
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
