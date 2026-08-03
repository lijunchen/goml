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

type Tuple2_3Key_3Key struct {
    _0 Key
    _1 Key
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

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__3 Key, other__4 Key) bool {
    switch other__4.(type) {
    case Empty:
        switch self__3.(type) {
        case Empty:
            return true
        default:
            return false
        }
    case Point:
        var x141 int32 = other__4.(Point)._0
        var x142 int32 = other__4.(Point)._1
        switch self__3.(type) {
        case Point:
            var x145 int32 = self__3.(Point)._0
            var x146 int32 = self__3.(Point)._1
            var jp184 bool
            var inline243 bool = x145 == x141
            jp184 = inline243
            if jp184 {
                var inline245 bool = x146 == x142
                return inline245
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
    var t205 int32
    var inline285 int32 = 4
    var inline289 int32 = x__22 + inline285
    t205 = inline289
    var inline280 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t205)
    _goml_runtime_core_string_println(inline280)
    var t206 string
    var inline277 string = "north"
    t206 = inline277
    var inline270 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
    _goml_runtime_core_string_println(inline270)
    var t208 string
    var inline257 int32 = 1
    var inline258 int32 = 2
    var inline261 string = "Key::Point { " + "x: "
    var inline262 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline257)
    var inline263 string = inline261 + inline262
    var inline264 string = inline263 + ", "
    var inline265 string = inline264 + "y: "
    var inline266 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline258)
    var inline267 string = inline265 + inline266
    var inline268 string = inline267 + " }"
    t208 = inline268
    var inline254 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t208)
    _goml_runtime_core_string_println(inline254)
    var t209 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t210 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t211 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(t209, t210)
    var inline251 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t211)
    _goml_runtime_core_string_println(inline251)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t214 string = _goml_runtime_core_int32_to_string(self__35)
    return t214
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t232 string = _goml_runtime_core_int32_to_string(self__72)
    return t232
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t237 string = _goml_runtime_core_bool_to_string(self__66)
    return t237
}

func main() {
    main0()
}
