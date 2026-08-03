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
        var x182 int32 = other__4.(Point)._0
        var x183 int32 = other__4.(Point)._1
        switch self__3.(type) {
        case Point:
            var x186 int32 = self__3.(Point)._0
            var x187 int32 = self__3.(Point)._1
            var jp225 bool
            var inline284 bool = x186 == x182
            jp225 = inline284
            if jp225 {
                var inline286 bool = x187 == x183
                return inline286
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
    var t246 int32
    var inline326 int32 = 4
    var inline330 int32 = x__22 + inline326
    t246 = inline330
    var inline321 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t246)
    _goml_runtime_core_string_println(inline321)
    var t247 string
    var inline318 string = "north"
    t247 = inline318
    var inline311 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t247)
    _goml_runtime_core_string_println(inline311)
    var t249 string
    var inline298 int32 = 1
    var inline299 int32 = 2
    var inline302 string = "Key::Point { " + "x: "
    var inline303 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline298)
    var inline304 string = inline302 + inline303
    var inline305 string = inline304 + ", "
    var inline306 string = inline305 + "y: "
    var inline307 string = _goml_m_inherent_i_int32_i_int32_i_to__string(inline299)
    var inline308 string = inline306 + inline307
    var inline309 string = inline308 + " }"
    t249 = inline309
    var inline295 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t249)
    _goml_runtime_core_string_println(inline295)
    var t250 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t251 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t252 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(t250, t251)
    var inline292 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t252)
    _goml_runtime_core_string_println(inline292)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__35 int32) string {
    var t255 string = _goml_runtime_core_int32_to_string(self__35)
    return t255
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t273 string = _goml_runtime_core_int32_to_string(self__72)
    return t273
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t278 string = _goml_runtime_core_bool_to_string(self__66)
    return t278
}

func main() {
    main0()
}
