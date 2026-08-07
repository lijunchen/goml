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
        var x175 int32 = self__3.(Move)._0
        var x176 int32 = self__3.(Move)._1
        var t196 string
        var inline222 string = _goml_runtime_core_int32_to_string(x175)
        t196 = inline222
        var t197 string = "Message::Move(" + t196
        var t198 string = t197 + ", "
        var t199 string
        var inline220 string = _goml_runtime_core_int32_to_string(x176)
        t199 = inline220
        var t200 string = t198 + t199
        var t201 string = t200 + ")"
        return t201
    case Write:
        var x177 string = self__3.(Write)._0
        var t202 string = "Message::Write(" + x177
        var t203 string = t202 + ")"
        return t203
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var summary__8 string
    var inline237 int32 = 4
    var inline238 int32 = 7
    var inline241 string = "Point { " + "x: "
    var inline242 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline237)
    var inline243 string = inline241 + inline242
    var inline244 string = inline243 + ", "
    var inline245 string = inline244 + "y: "
    var inline246 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline238)
    var inline247 string = inline245 + inline246
    var inline248 string = inline247 + " }"
    summary__8 = inline248
    var t205 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t205)
    var t206 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t206)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    var inline233 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(summary__8)
    _goml_runtime_core_string_println(inline233)
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(mv__9)
    _goml_runtime_core_string_println(inline230)
    var inline227 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__10)
    _goml_runtime_core_string_println(inline227)
    var inline224 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(exit__11)
    _goml_runtime_core_string_println(inline224)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t209 string = _goml_runtime_core_int32_to_string(self__72)
    return t209
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
