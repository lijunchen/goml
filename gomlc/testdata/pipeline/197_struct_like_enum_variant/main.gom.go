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
        var x179 int32 = other__11.(Point)._0
        var x180 int32 = other__11.(Point)._1
        switch self__10.(type) {
        case Point:
            var x183 int32 = self__10.(Point)._0
            var x184 int32 = self__10.(Point)._1
            var jp236 bool
            var inline288 bool = x183 == x179
            jp236 = inline288
            if jp236 {
                var inline290 bool = x184 == x180
                return inline290
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
    var t249 int32
    var inline323 int32 = 4
    var inline326 int32 = x__22 + inline323
    t249 = inline326
    var inline319 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t249)
    _goml_runtime_core_string_println(inline319)
    var t250 string
    var inline316 string = "north"
    t250 = inline316
    var inline311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t250)
    _goml_runtime_core_string_println(inline311)
    var t252 string
    var inline298 int32 = 1
    var inline299 int32 = 2
    var inline302 string = "Key::Point { " + "x: "
    var inline303 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline298)
    var inline304 string = inline302 + inline303
    var inline305 string = inline304 + ", "
    var inline306 string = inline305 + "y: "
    var inline307 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline299)
    var inline308 string = inline306 + inline307
    var inline309 string = inline308 + " }"
    t252 = inline309
    var inline295 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t252)
    _goml_runtime_core_string_println(inline295)
    var t253 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t254 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t255 bool = _goml_m_trait__impl_i_PartialEq_i_Key_i_eq(t253, t254)
    var inline292 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t255)
    _goml_runtime_core_string_println(inline292)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t258 string = _goml_runtime_core_int32_to_string(self__70)
    return t258
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t278 string = _goml_runtime_core_bool_to_string(self__64)
    return t278
}

func main() {
    main0()
}
