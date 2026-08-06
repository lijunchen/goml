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
        var x139 int32 = self__3.(Move)._0
        var x140 int32 = self__3.(Move)._1
        var t160 string
        var inline186 string = _goml_runtime_core_int32_to_string(x139)
        t160 = inline186
        var t161 string = "Message::Move(" + t160
        var t162 string = t161 + ", "
        var t163 string
        var inline184 string = _goml_runtime_core_int32_to_string(x140)
        t163 = inline184
        var t164 string = t162 + t163
        var t165 string = t164 + ")"
        return t165
    case Write:
        var x141 string = self__3.(Write)._0
        var t166 string = "Message::Write(" + x141
        var t167 string = t166 + ")"
        return t167
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var summary__8 string
    var inline201 int32 = 4
    var inline202 int32 = 7
    var inline205 string = "Point { " + "x: "
    var inline206 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline201)
    var inline207 string = inline205 + inline206
    var inline208 string = inline207 + ", "
    var inline209 string = inline208 + "y: "
    var inline210 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline202)
    var inline211 string = inline209 + inline210
    var inline212 string = inline211 + " }"
    summary__8 = inline212
    var t169 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t169)
    var t170 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t170)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    var inline197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(summary__8)
    _goml_runtime_core_string_println(inline197)
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(mv__9)
    _goml_runtime_core_string_println(inline194)
    var inline191 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__10)
    _goml_runtime_core_string_println(inline191)
    var inline188 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(exit__11)
    _goml_runtime_core_string_println(inline188)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t173 string = _goml_runtime_core_int32_to_string(self__72)
    return t173
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
