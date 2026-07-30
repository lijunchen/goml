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
    var retv94 string
    var jp96 string
    switch self__0.(type) {
    case Empty:
        jp96 = "Key::Empty"
    case Point:
        var x68 int32 = self__0.(Point)._0
        var x69 int32 = self__0.(Point)._1
        var __field1__2 int32 = x69
        var __field0__1 int32 = x68
        var t97 string = "Key::Point { " + "x: "
        var t98 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__1)
        var t99 string = t97 + t98
        var t100 string = t99 + ", "
        var t101 string = t100 + "y: "
        var t102 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__2)
        var t103 string = t101 + t102
        var t104 string = t103 + " }"
        jp96 = t104
    default:
        panic("non-exhaustive match")
    }
    retv94 = jp96
    return retv94
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__3 Key, other__4 Key) bool {
    var retv106 bool
    var mtmp70 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__3,
        _1: other__4,
    }
    var x71 Key = mtmp70._0
    var x72 Key = mtmp70._1
    var jp108 bool
    switch x72.(type) {
    case Empty:
        var jp110 bool
        switch x71.(type) {
        case Empty:
            jp110 = true
        default:
            jp110 = false
        }
        jp108 = jp110
    case Point:
        var x73 int32 = x72.(Point)._0
        var x74 int32 = x72.(Point)._1
        var jp112 bool
        switch x71.(type) {
        case Point:
            var x77 int32 = x71.(Point)._0
            var x78 int32 = x71.(Point)._1
            var __l1_1__6 int32 = x78
            var __l1_0__5 int32 = x77
            var __r1_1__8 int32 = x74
            var __r1_0__7 int32 = x73
            var jp116 bool
            if true {
                var t118 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__5, __r1_0__7)
                jp116 = t118
            } else {
                jp116 = false
            }
            var jp114 bool
            if jp116 {
                var t117 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_1__6, __r1_1__8)
                jp114 = t117
            } else {
                jp114 = false
            }
            jp112 = jp114
        default:
            jp112 = false
        }
        jp108 = jp112
    default:
        panic("non-exhaustive match")
    }
    retv106 = jp108
    return retv106
}

func score(value__16 Message__string) int32 {
    var retv128 int32
    var jp130 int32
    switch value__16.(type) {
    case Quit:
        jp130 = 0
    case Write:
        jp130 = 1
    case Move:
        var x82 int32 = value__16.(Move)._0
        var x83 int32 = value__16.(Move)._1
        var vertical__18 int32 = x83
        var x__17 int32 = x82
        var t131 int32 = x__17 + vertical__18
        jp130 = t131
    default:
        panic("non-exhaustive match")
    }
    retv128 = jp130
    return retv128
}

func label(value__19 Message__string) string {
    var retv133 string
    var jp135 string
    switch value__19.(type) {
    case Quit:
        jp135 = "quit"
    case Write:
        var x85 string = value__19.(Write)._0
        var text__21 string = x85
        jp135 = text__21
    case Move:
        var x88 string = value__19.(Move)._2
        var label__20 string = x88
        jp135 = label__20
    default:
        panic("non-exhaustive match")
    }
    retv133 = jp135
    return retv133
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
    var t137 int32 = score(first__24)
    println__T_int32(t137)
    var t138 string = label(second__25)
    println__T_string(t138)
    var t139 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t140 string = _goml_m_trait__impl_i_ToString_i_Key_i_to__string(t139)
    println__T_string(t140)
    var t141 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t142 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t143 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(t141, t142)
    println__T_bool(t143)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv145 string
    var t146 string = _goml_runtime_core_int32_to_string(self__6)
    retv145 = t146
    return retv145
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv148 bool
    var t149 bool = self__65 == other__66
    retv148 = t149
    return retv148
}

func println__T_int32(value__1 int32) struct{} {
    var t154 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t154)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t157 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t157)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t160 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t160)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv163 string
    var t164 string = _goml_runtime_core_int32_to_string(self__43)
    retv163 = t164
    return retv163
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv166 string
    retv166 = self__38
    return retv166
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv168 string
    var t169 string = _goml_runtime_core_bool_to_string(self__37)
    retv168 = t169
    return retv168
}

func main() {
    main0()
}
