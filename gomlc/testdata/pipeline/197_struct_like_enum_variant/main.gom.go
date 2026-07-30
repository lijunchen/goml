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
    var retv134 string
    var jp136 string
    switch self__0.(type) {
    case Empty:
        jp136 = "Key::Empty"
    case Point:
        var x108 int32 = self__0.(Point)._0
        var x109 int32 = self__0.(Point)._1
        var __field1__2 int32 = x109
        var __field0__1 int32 = x108
        var t137 string = "Key::Point { " + "x: "
        var t138 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__1)
        var t139 string = t137 + t138
        var t140 string = t139 + ", "
        var t141 string = t140 + "y: "
        var t142 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__2)
        var t143 string = t141 + t142
        var t144 string = t143 + " }"
        jp136 = t144
    default:
        panic("non-exhaustive match")
    }
    retv134 = jp136
    return retv134
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__3 Key, other__4 Key) bool {
    var retv146 bool
    var mtmp110 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__3,
        _1: other__4,
    }
    var x111 Key = mtmp110._0
    var x112 Key = mtmp110._1
    var jp148 bool
    switch x112.(type) {
    case Empty:
        var jp150 bool
        switch x111.(type) {
        case Empty:
            jp150 = true
        default:
            jp150 = false
        }
        jp148 = jp150
    case Point:
        var x113 int32 = x112.(Point)._0
        var x114 int32 = x112.(Point)._1
        var jp152 bool
        switch x111.(type) {
        case Point:
            var x117 int32 = x111.(Point)._0
            var x118 int32 = x111.(Point)._1
            var __l1_1__6 int32 = x118
            var __l1_0__5 int32 = x117
            var __r1_1__8 int32 = x114
            var __r1_0__7 int32 = x113
            var jp156 bool
            if true {
                var t158 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__5, __r1_0__7)
                jp156 = t158
            } else {
                jp156 = false
            }
            var jp154 bool
            if jp156 {
                var t157 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_1__6, __r1_1__8)
                jp154 = t157
            } else {
                jp154 = false
            }
            jp152 = jp154
        default:
            jp152 = false
        }
        jp148 = jp152
    default:
        panic("non-exhaustive match")
    }
    retv146 = jp148
    return retv146
}

func score(value__16 Message__string) int32 {
    var retv168 int32
    var jp170 int32
    switch value__16.(type) {
    case Quit:
        jp170 = 0
    case Write:
        jp170 = 1
    case Move:
        var x122 int32 = value__16.(Move)._0
        var x123 int32 = value__16.(Move)._1
        var vertical__18 int32 = x123
        var x__17 int32 = x122
        var t171 int32 = x__17 + vertical__18
        jp170 = t171
    default:
        panic("non-exhaustive match")
    }
    retv168 = jp170
    return retv168
}

func label(value__19 Message__string) string {
    var retv173 string
    var jp175 string
    switch value__19.(type) {
    case Quit:
        jp175 = "quit"
    case Write:
        var x125 string = value__19.(Write)._0
        var text__21 string = x125
        jp175 = text__21
    case Move:
        var x128 string = value__19.(Move)._2
        var label__20 string = x128
        jp175 = label__20
    default:
        panic("non-exhaustive match")
    }
    retv173 = jp175
    return retv173
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
    var t177 int32 = score(first__24)
    println__T_int32(t177)
    var t178 string = label(second__25)
    println__T_string(t178)
    var t179 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t180 string = _goml_m_trait__impl_i_ToString_i_Key_i_to__string(t179)
    println__T_string(t180)
    var t181 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t182 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t183 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(t181, t182)
    println__T_bool(t183)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv185 string
    var t186 string = _goml_runtime_core_int32_to_string(self__6)
    retv185 = t186
    return retv185
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv188 bool
    var t189 bool = self__65 == other__66
    retv188 = t189
    return retv188
}

func println__T_int32(value__1 int32) struct{} {
    var t194 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t194)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t197 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t197)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t200 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t200)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv203 string
    var t204 string = _goml_runtime_core_int32_to_string(self__43)
    retv203 = t204
    return retv203
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv206 string
    retv206 = self__38
    return retv206
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv208 string
    var t209 string = _goml_runtime_core_bool_to_string(self__37)
    retv208 = t209
    return retv208
}

func main() {
    main0()
}
