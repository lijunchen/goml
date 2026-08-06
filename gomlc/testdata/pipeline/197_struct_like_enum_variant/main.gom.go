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

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__10 Key, other__11 Key) bool {
    switch other__11.(type) {
    case Empty:
        switch self__10.(type) {
        case Empty:
            return true
        default:
            return false
        }
    case Point:
        var x143 int32 = other__11.(Point)._0
        var x144 int32 = other__11.(Point)._1
        switch self__10.(type) {
        case Point:
            var x147 int32 = self__10.(Point)._0
            var x148 int32 = self__10.(Point)._1
            var jp200 bool
            var inline252 bool = x147 == x143
            jp200 = inline252
            if jp200 {
                var inline254 bool = x148 == x144
                return inline254
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
    var t213 int32
    var inline290 int32 = 4
    var inline294 int32 = x__22 + inline290
    t213 = inline294
    var inline285 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
    _goml_runtime_core_string_println(inline285)
    var t214 string
    var inline282 string = "north"
    t214 = inline282
    var inline275 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t214)
    _goml_runtime_core_string_println(inline275)
    var t216 string
    var inline262 int32 = 1
    var inline263 int32 = 2
    var inline266 string = "Key::Point { " + "x: "
    var inline267 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline262)
    var inline268 string = inline266 + inline267
    var inline269 string = inline268 + ", "
    var inline270 string = inline269 + "y: "
    var inline271 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(inline263)
    var inline272 string = inline270 + inline271
    var inline273 string = inline272 + " }"
    t216 = inline273
    var inline259 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t216)
    _goml_runtime_core_string_println(inline259)
    var t217 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t218 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t219 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(t217, t218)
    var inline256 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t219)
    _goml_runtime_core_string_println(inline256)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t222 string = _goml_runtime_core_int32_to_string(self__72)
    return t222
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t242 string = _goml_runtime_core_bool_to_string(self__66)
    return t242
}

func main() {
    main0()
}
