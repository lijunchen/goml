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
        var x160 int32 = other__4.(Point)._0
        var x161 int32 = other__4.(Point)._1
        switch self__3.(type) {
        case Point:
            var x164 int32 = self__3.(Point)._0
            var x165 int32 = self__3.(Point)._1
            var jp203 bool
            var inline262 bool = x164 == x160
            jp203 = inline262
            if jp203 {
                var inline264 bool = x165 == x161
                return inline264
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
    var t224 int32
    var inline304 int32 = 4
    var inline308 int32 = x__22 + inline304
    t224 = inline308
    var inline299 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t224)
    _goml_runtime_core_string_println(inline299)
    var t225 string
    var inline296 string = "north"
    t225 = inline296
    var inline289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t225)
    _goml_runtime_core_string_println(inline289)
    var t227 string
    var inline276 int32 = 1
    var inline277 int32 = 2
    var inline280 string = "Key::Point { " + "x: "
    var inline281 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline276)
    var inline282 string = inline280 + inline281
    var inline283 string = inline282 + ", "
    var inline284 string = inline283 + "y: "
    var inline285 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline277)
    var inline286 string = inline284 + inline285
    var inline287 string = inline286 + " }"
    t227 = inline287
    var inline273 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t227)
    _goml_runtime_core_string_println(inline273)
    var t228 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t229 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t230 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(t228, t229)
    var inline270 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t230)
    _goml_runtime_core_string_println(inline270)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var t233 string = _goml_runtime_core_int32_to_string(self__6)
    return t233
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t251 string = _goml_runtime_core_int32_to_string(self__43)
    return t251
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t256 string = _goml_runtime_core_bool_to_string(self__37)
    return t256
}

func main() {
    main0()
}
