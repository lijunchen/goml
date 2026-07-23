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
    var retv87 string
    var jp89 string
    switch self__0.(type) {
    case Empty:
        jp89 = "Key::Empty"
    case Point:
        var x61 int32 = self__0.(Point)._0
        var x62 int32 = self__0.(Point)._1
        var __field1__2 int32 = x62
        var __field0__1 int32 = x61
        var t90 string = "Key::Point { " + "x: "
        var t91 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__1)
        var t92 string = t90 + t91
        var t93 string = t92 + ", "
        var t94 string = t93 + "y: "
        var t95 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__2)
        var t96 string = t94 + t95
        var t97 string = t96 + " }"
        jp89 = t97
    default:
        panic("non-exhaustive match")
    }
    retv87 = jp89
    return retv87
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__3 Key, other__4 Key) bool {
    var retv99 bool
    var mtmp63 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__3,
        _1: other__4,
    }
    var x64 Key = mtmp63._0
    var x65 Key = mtmp63._1
    var jp101 bool
    switch x65.(type) {
    case Empty:
        var jp103 bool
        switch x64.(type) {
        case Empty:
            jp103 = true
        default:
            jp103 = false
        }
        jp101 = jp103
    case Point:
        var x66 int32 = x65.(Point)._0
        var x67 int32 = x65.(Point)._1
        var jp105 bool
        switch x64.(type) {
        case Point:
            var x70 int32 = x64.(Point)._0
            var x71 int32 = x64.(Point)._1
            var __l1_1__6 int32 = x71
            var __l1_0__5 int32 = x70
            var __r1_1__8 int32 = x67
            var __r1_0__7 int32 = x66
            var jp109 bool
            if true {
                var t111 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__5, __r1_0__7)
                jp109 = t111
            } else {
                jp109 = false
            }
            var jp107 bool
            if jp109 {
                var t110 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_1__6, __r1_1__8)
                jp107 = t110
            } else {
                jp107 = false
            }
            jp105 = jp107
        default:
            jp105 = false
        }
        jp101 = jp105
    default:
        panic("non-exhaustive match")
    }
    retv99 = jp101
    return retv99
}

func score(value__16 Message__string) int32 {
    var retv121 int32
    var jp123 int32
    switch value__16.(type) {
    case Quit:
        jp123 = 0
    case Write:
        jp123 = 1
    case Move:
        var x75 int32 = value__16.(Move)._0
        var x76 int32 = value__16.(Move)._1
        var vertical__18 int32 = x76
        var x__17 int32 = x75
        var t124 int32 = x__17 + vertical__18
        jp123 = t124
    default:
        panic("non-exhaustive match")
    }
    retv121 = jp123
    return retv121
}

func label(value__19 Message__string) string {
    var retv126 string
    var jp128 string
    switch value__19.(type) {
    case Quit:
        jp128 = "quit"
    case Write:
        var x78 string = value__19.(Write)._0
        var text__21 string = x78
        jp128 = text__21
    case Move:
        var x81 string = value__19.(Move)._2
        var label__20 string = x81
        jp128 = label__20
    default:
        panic("non-exhaustive match")
    }
    retv126 = jp128
    return retv126
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
    var t130 int32 = score(first__24)
    println__T_int32(t130)
    var t131 string = label(second__25)
    println__T_string(t131)
    var t132 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t133 string = _goml_m_trait__impl_i_ToString_i_Key_i_to__string(t132)
    println__T_string(t133)
    var t134 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t135 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t136 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(t134, t135)
    println__T_bool(t136)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__5 int32) string {
    var retv138 string
    var t139 string = _goml_runtime_core_int32_to_string(self__5)
    retv138 = t139
    return retv138
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__61 int32, other__62 int32) bool {
    var retv141 bool
    var t142 bool = self__61 == other__62
    retv141 = t142
    return retv141
}

func println__T_int32(value__1 int32) struct{} {
    var t147 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t147)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t150 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t150)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t153 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t153)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv156 string
    var t157 string = _goml_runtime_core_int32_to_string(self__41)
    retv156 = t157
    return retv156
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv159 string
    retv159 = self__37
    return retv159
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__36 bool) string {
    var retv161 string
    var t162 string = _goml_runtime_core_bool_to_string(self__36)
    retv161 = t162
    return retv161
}

func main() {
    main0()
}
