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
        var x158 int32 = self__3.(Move)._0
        var x159 int32 = self__3.(Move)._1
        var t179 string
        var inline205 string = _goml_runtime_core_int32_to_string(x158)
        t179 = inline205
        var t180 string = "Message::Move(" + t179
        var t181 string = t180 + ", "
        var t182 string
        var inline203 string = _goml_runtime_core_int32_to_string(x159)
        t182 = inline203
        var t183 string = t181 + t182
        var t184 string = t183 + ")"
        return t184
    case Write:
        var x160 string = self__3.(Write)._0
        var t185 string = "Message::Write(" + x160
        var t186 string = t185 + ")"
        return t186
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var summary__8 string
    var inline220 int32 = 4
    var inline221 int32 = 7
    var inline224 string = "Point { " + "x: "
    var inline225 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline220)
    var inline226 string = inline224 + inline225
    var inline227 string = inline226 + ", "
    var inline228 string = inline227 + "y: "
    var inline229 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline221)
    var inline230 string = inline228 + inline229
    var inline231 string = inline230 + " }"
    summary__8 = inline231
    var t188 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t188)
    var t189 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t189)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    var inline216 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(summary__8)
    _goml_runtime_core_string_println(inline216)
    var inline213 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(mv__9)
    _goml_runtime_core_string_println(inline213)
    var inline210 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__10)
    _goml_runtime_core_string_println(inline210)
    var inline207 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(exit__11)
    _goml_runtime_core_string_println(inline207)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t192 string = _goml_runtime_core_int32_to_string(self__6)
    return t192
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
