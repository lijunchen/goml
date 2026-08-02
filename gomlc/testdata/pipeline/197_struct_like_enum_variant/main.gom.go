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
    var retv181 string
    var jp183 string
    switch self__0.(type) {
    case Empty:
        jp183 = "Key::Empty"
    case Point:
        var x155 int32 = self__0.(Point)._0
        var x156 int32 = self__0.(Point)._1
        var __field1__2 int32 = x156
        var __field0__1 int32 = x155
        var t184 string = "Key::Point { " + "x: "
        var t185 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__1)
        var t186 string = t184 + t185
        var t187 string = t186 + ", "
        var t188 string = t187 + "y: "
        var t189 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__2)
        var t190 string = t188 + t189
        var t191 string = t190 + " }"
        jp183 = t191
    default:
        panic("non-exhaustive match")
    }
    retv181 = jp183
    return retv181
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__3 Key, other__4 Key) bool {
    var retv193 bool
    var mtmp157 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__3,
        _1: other__4,
    }
    var x158 Key = mtmp157._0
    var x159 Key = mtmp157._1
    var jp195 bool
    switch x159.(type) {
    case Empty:
        var jp197 bool
        switch x158.(type) {
        case Empty:
            jp197 = true
        default:
            jp197 = false
        }
        jp195 = jp197
    case Point:
        var x160 int32 = x159.(Point)._0
        var x161 int32 = x159.(Point)._1
        var jp199 bool
        switch x158.(type) {
        case Point:
            var x164 int32 = x158.(Point)._0
            var x165 int32 = x158.(Point)._1
            var __l1_1__6 int32 = x165
            var __l1_0__5 int32 = x164
            var __r1_1__8 int32 = x161
            var __r1_0__7 int32 = x160
            var jp203 bool
            if true {
                var t205 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__5, __r1_0__7)
                jp203 = t205
            } else {
                jp203 = false
            }
            var jp201 bool
            if jp203 {
                var t204 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_1__6, __r1_1__8)
                jp201 = t204
            } else {
                jp201 = false
            }
            jp199 = jp201
        default:
            jp199 = false
        }
        jp195 = jp199
    default:
        panic("non-exhaustive match")
    }
    retv193 = jp195
    return retv193
}

func score(value__16 Message__string) int32 {
    var retv215 int32
    var jp217 int32
    switch value__16.(type) {
    case Quit:
        jp217 = 0
    case Write:
        jp217 = 1
    case Move:
        var x169 int32 = value__16.(Move)._0
        var x170 int32 = value__16.(Move)._1
        var vertical__18 int32 = x170
        var x__17 int32 = x169
        var t218 int32 = x__17 + vertical__18
        jp217 = t218
    default:
        panic("non-exhaustive match")
    }
    retv215 = jp217
    return retv215
}

func label(value__19 Message__string) string {
    var retv220 string
    var jp222 string
    switch value__19.(type) {
    case Quit:
        jp222 = "quit"
    case Write:
        var x172 string = value__19.(Write)._0
        var text__21 string = x172
        jp222 = text__21
    case Move:
        var x175 string = value__19.(Move)._2
        var label__20 string = x175
        jp222 = label__20
    default:
        panic("non-exhaustive match")
    }
    retv220 = jp222
    return retv220
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
    var t224 int32 = score(first__24)
    println__T_int32(t224)
    var t225 string = label(second__25)
    println__T_string(t225)
    var t226 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t227 string = _goml_m_trait__impl_i_ToString_i_Key_i_to__string(t226)
    println__T_string(t227)
    var t228 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t229 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t230 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(t228, t229)
    println__T_bool(t230)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv232 string
    var t233 string = _goml_runtime_core_int32_to_string(self__6)
    retv232 = t233
    return retv232
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv235 bool
    var t236 bool = self__65 == other__66
    retv235 = t236
    return retv235
}

func println__T_int32(value__1 int32) struct{} {
    var t241 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t241)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t244 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t244)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t247 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t247)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv250 string
    var t251 string = _goml_runtime_core_int32_to_string(self__43)
    retv250 = t251
    return retv250
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv253 string
    retv253 = self__38
    return retv253
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv255 string
    var t256 string = _goml_runtime_core_bool_to_string(self__37)
    retv255 = t256
    return retv255
}

func main() {
    main0()
}
