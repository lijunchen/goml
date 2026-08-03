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
        var x180 int32 = self__3.(Move)._0
        var x181 int32 = self__3.(Move)._1
        var t201 string
        var inline227 string = _goml_runtime_core_int32_to_string(x180)
        t201 = inline227
        var t202 string = "Message::Move(" + t201
        var t203 string = t202 + ", "
        var t204 string
        var inline225 string = _goml_runtime_core_int32_to_string(x181)
        t204 = inline225
        var t205 string = t203 + t204
        var t206 string = t205 + ")"
        return t206
    case Write:
        var x182 string = self__3.(Write)._0
        var t207 string = "Message::Write(" + x182
        var t208 string = t207 + ")"
        return t208
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var summary__8 string
    var inline242 int32 = 4
    var inline243 int32 = 7
    var inline246 string = "Point { " + "x: "
    var inline247 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline242)
    var inline248 string = inline246 + inline247
    var inline249 string = inline248 + ", "
    var inline250 string = inline249 + "y: "
    var inline251 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline243)
    var inline252 string = inline250 + inline251
    var inline253 string = inline252 + " }"
    summary__8 = inline253
    var t210 Message = Move{
        _0: 1,
        _1: 2,
    }
    var mv__9 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t210)
    var t211 Message = Write{
        _0: "done",
    }
    var text__10 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(t211)
    var exit__11 string = _goml_m_trait__impl_i_ToString_i_Message_i_to__string(Quit{})
    var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(summary__8)
    _goml_runtime_core_string_println(inline238)
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(mv__9)
    _goml_runtime_core_string_println(inline235)
    var inline232 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(text__10)
    _goml_runtime_core_string_println(inline232)
    var inline229 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(exit__11)
    _goml_runtime_core_string_println(inline229)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t214 string = _goml_runtime_core_int32_to_string(self__35)
    return t214
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
