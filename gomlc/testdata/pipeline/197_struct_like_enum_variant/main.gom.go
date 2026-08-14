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
        var x194 int32 = other__11.(Point)._0
        var x195 int32 = other__11.(Point)._1
        switch self__10.(type) {
        case Point:
            var x198 int32 = self__10.(Point)._0
            var x199 int32 = self__10.(Point)._1
            var jp251 bool
            var inline303 bool = x198 == x194
            jp251 = inline303
            if jp251 {
                var inline305 bool = x199 == x195
                return inline305
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
    var t264 int32
    var inline338 int32 = 4
    var inline341 int32 = x__22 + inline338
    t264 = inline341
    var inline334 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t264)
    _goml_runtime_core_string_println(inline334)
    var t265 string
    var inline331 string = "north"
    t265 = inline331
    var inline326 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t265)
    _goml_runtime_core_string_println(inline326)
    var t267 string
    var inline313 int32 = 1
    var inline314 int32 = 2
    var inline317 string = "Key::Point { " + "x: "
    var inline318 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline313)
    var inline319 string = inline317 + inline318
    var inline320 string = inline319 + ", "
    var inline321 string = inline320 + "y: "
    var inline322 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline314)
    var inline323 string = inline321 + inline322
    var inline324 string = inline323 + " }"
    t267 = inline324
    var inline310 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t267)
    _goml_runtime_core_string_println(inline310)
    var t268 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t269 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t270 bool = _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(t268, t269)
    var inline307 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t270)
    _goml_runtime_core_string_println(inline307)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t273 string = _goml_runtime_core_int32_to_string(self__70)
    return t273
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t293 string = _goml_runtime_core_bool_to_string(self__64)
    return t293
}

func main() {
    main0()
}
