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

func _goml_m_trait__impl_i_ToString_i_Key_i_to__string(self__0 Key) string {
    var retv90 string
    var jp92 string
    switch self__0.(type) {
    case Empty:
        jp92 = "Key::Empty"
    case Point:
        var x64 int32 = self__0.(Point)._0
        var x65 int32 = self__0.(Point)._1
        var __field1__2 int32 = x65
        var __field0__1 int32 = x64
        var t93 string = "Key::Point { " + "x: "
        var t94 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__1)
        var t95 string = t93 + t94
        var t96 string = t95 + ", "
        var t97 string = t96 + "y: "
        var t98 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__2)
        var t99 string = t97 + t98
        var t100 string = t99 + " }"
        jp92 = t100
    default:
        panic("non-exhaustive match")
    }
    retv90 = jp92
    return retv90
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__3 Key, other__4 Key) bool {
    var retv102 bool
    var mtmp66 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__3,
        _1: other__4,
    }
    var x67 Key = mtmp66._0
    var x68 Key = mtmp66._1
    var jp104 bool
    switch x68.(type) {
    case Empty:
        var jp106 bool
        switch x67.(type) {
        case Empty:
            jp106 = true
        default:
            jp106 = false
        }
        jp104 = jp106
    case Point:
        var x69 int32 = x68.(Point)._0
        var x70 int32 = x68.(Point)._1
        var jp108 bool
        switch x67.(type) {
        case Point:
            var x73 int32 = x67.(Point)._0
            var x74 int32 = x67.(Point)._1
            var __l1_1__6 int32 = x74
            var __l1_0__5 int32 = x73
            var __r1_1__8 int32 = x70
            var __r1_0__7 int32 = x69
            var jp112 bool
            if true {
                var t114 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__5, __r1_0__7)
                jp112 = t114
            } else {
                jp112 = false
            }
            var jp110 bool
            if jp112 {
                var t113 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_1__6, __r1_1__8)
                jp110 = t113
            } else {
                jp110 = false
            }
            jp108 = jp110
        default:
            jp108 = false
        }
        jp104 = jp108
    default:
        panic("non-exhaustive match")
    }
    retv102 = jp104
    return retv102
}

func score(value__16 Message__string) int32 {
    var retv124 int32
    var jp126 int32
    switch value__16.(type) {
    case Quit:
        jp126 = 0
    case Write:
        jp126 = 1
    case Move:
        var x78 int32 = value__16.(Move)._0
        var x79 int32 = value__16.(Move)._1
        var vertical__18 int32 = x79
        var x__17 int32 = x78
        var t127 int32 = x__17 + vertical__18
        jp126 = t127
    default:
        panic("non-exhaustive match")
    }
    retv124 = jp126
    return retv124
}

func label(value__19 Message__string) string {
    var retv129 string
    var jp131 string
    switch value__19.(type) {
    case Quit:
        jp131 = "quit"
    case Write:
        var x81 string = value__19.(Write)._0
        var text__21 string = x81
        jp131 = text__21
    case Move:
        var x84 string = value__19.(Move)._2
        var label__20 string = x84
        jp131 = label__20
    default:
        panic("non-exhaustive match")
    }
    retv129 = jp131
    return retv129
}

func main0() struct{} {
    var x__22 int32 = 3
    var direction__23 string = "north"
    var first__24 Message__string = Move{
        _0: x__22,
        _1: 4,
        _2: direction__23,
    }
    var second__25 Message__string = Move{
        _0: 3,
        _1: 4,
        _2: "north",
    }
    var t133 int32 = score(first__24)
    println__T_int32(t133)
    var t134 string = label(second__25)
    println__T_string(t134)
    var t135 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t136 string = _goml_m_trait__impl_i_ToString_i_Key_i_to__string(t135)
    println__T_string(t136)
    var t137 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t138 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t139 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(t137, t138)
    println__T_bool(t139)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv141 string
    var t142 string = _goml_runtime_core_int32_to_string(self__6)
    retv141 = t142
    return retv141
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv144 bool
    var t145 bool = self__65 == other__66
    retv144 = t145
    return retv144
}

func println__T_int32(value__1 int32) struct{} {
    var t150 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t150)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t153 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t153)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t156 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t156)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv159 string
    var t160 string = _goml_runtime_core_int32_to_string(self__43)
    retv159 = t160
    return retv159
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv162 string
    retv162 = self__38
    return retv162
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv164 string
    var t165 string = _goml_runtime_core_bool_to_string(self__37)
    retv164 = t165
    return retv164
}

func main() {
    main0()
}
