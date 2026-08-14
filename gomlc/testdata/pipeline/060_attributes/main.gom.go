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
        var x190 int32 = self__3.(Move)._0
        var x191 int32 = self__3.(Move)._1
        var t211 string
        var inline237 string = _goml_runtime_core_int32_to_string(x190)
        t211 = inline237
        var t212 string = "Message::Move(" + t211
        var t213 string = t212 + ", "
        var t214 string
        var inline235 string = _goml_runtime_core_int32_to_string(x191)
        t214 = inline235
        var t215 string = t213 + t214
        var t216 string = t215 + ")"
        return t216
    case Write:
        var x192 string = self__3.(Write)._0
        var t217 string = "Message::Write(" + x192
        var t218 string = t217 + ")"
        return t218
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var summary__8 string
    var inline252 int32 = 4
    var inline253 int32 = 7
    var inline256 string = "Point { " + "x: "
    var inline257 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline252)
    var inline258 string = inline256 + inline257
    var inline259 string = inline258 + ", "
    var inline260 string = inline259 + "y: "
    var inline261 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline253)
    var inline262 string = inline260 + inline261
    var inline263 string = inline262 + " }"
    summary__8 = inline263
    var t220 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t220)
    var t221 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t221)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(summary__8)
    _goml_runtime_core_string_println(inline248)
    var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(mv__9)
    _goml_runtime_core_string_println(inline245)
    var inline242 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__10)
    _goml_runtime_core_string_println(inline242)
    var inline239 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(exit__11)
    _goml_runtime_core_string_println(inline239)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t224 string = _goml_runtime_core_int32_to_string(self__70)
    return t224
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
