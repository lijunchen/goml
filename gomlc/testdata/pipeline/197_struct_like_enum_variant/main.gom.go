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

type Key interface {
    isKey()
}

type Empty struct {}

func (_ Empty) isKey() {}

type Point struct {
    _0 int32
    _1 int32
}

func (_ Point) isKey() {}

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
    switch other__11.(type) {
    case Empty:
        switch self__10.(type) {
        case Empty:
            return true
        default:
            return false
        }
    case Point:
        var x189 int32 = other__11.(Point)._0
        var x190 int32 = other__11.(Point)._1
        switch self__10.(type) {
        case Point:
            var x193 int32 = self__10.(Point)._0
            var x194 int32 = self__10.(Point)._1
            var jp246 bool
            var inline298 bool = x193 == x189
            jp246 = inline298
            if jp246 {
                var inline300 bool = x194 == x190
                return inline300
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
    var t259 int32
    var inline333 int32 = 4
    var inline336 int32 = x__22 + inline333
    t259 = inline336
    var inline329 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t259)
    _goml_runtime_core_string_println(inline329)
    var t260 string
    var inline326 string = "north"
    t260 = inline326
    var inline321 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t260)
    _goml_runtime_core_string_println(inline321)
    var t262 string
    var inline308 int32 = 1
    var inline309 int32 = 2
    var inline312 string = "Key::Point { " + "x: "
    var inline313 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline308)
    var inline314 string = inline312 + inline313
    var inline315 string = inline314 + ", "
    var inline316 string = inline315 + "y: "
    var inline317 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline309)
    var inline318 string = inline316 + inline317
    var inline319 string = inline318 + " }"
    t262 = inline319
    var inline305 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t262)
    _goml_runtime_core_string_println(inline305)
    var t263 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t264 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t265 bool = _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(t263, t264)
    var inline302 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t265)
    _goml_runtime_core_string_println(inline302)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t268 string = _goml_runtime_core_int32_to_string(self__70)
    return t268
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t288 string = _goml_runtime_core_bool_to_string(self__64)
    return t288
}

func main() {
    main0()
}
