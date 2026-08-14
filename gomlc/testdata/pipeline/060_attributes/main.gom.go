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
        var x185 int32 = self__3.(Move)._0
        var x186 int32 = self__3.(Move)._1
        var t206 string
        var inline232 string = _goml_runtime_core_int32_to_string(x185)
        t206 = inline232
        var t207 string = "Message::Move(" + t206
        var t208 string = t207 + ", "
        var t209 string
        var inline230 string = _goml_runtime_core_int32_to_string(x186)
        t209 = inline230
        var t210 string = t208 + t209
        var t211 string = t210 + ")"
        return t211
    case Write:
        var x187 string = self__3.(Write)._0
        var t212 string = "Message::Write(" + x187
        var t213 string = t212 + ")"
        return t213
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var summary__8 string
    var inline247 int32 = 4
    var inline248 int32 = 7
    var inline251 string = "Point { " + "x: "
    var inline252 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline247)
    var inline253 string = inline251 + inline252
    var inline254 string = inline253 + ", "
    var inline255 string = inline254 + "y: "
    var inline256 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline248)
    var inline257 string = inline255 + inline256
    var inline258 string = inline257 + " }"
    summary__8 = inline258
    var t215 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t215)
    var t216 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t216)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(summary__8)
    _goml_runtime_core_string_println(inline243)
    var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(mv__9)
    _goml_runtime_core_string_println(inline240)
    var inline237 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__10)
    _goml_runtime_core_string_println(inline237)
    var inline234 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(exit__11)
    _goml_runtime_core_string_println(inline234)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t219 string = _goml_runtime_core_int32_to_string(self__70)
    return t219
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
