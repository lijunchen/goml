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
    var retv178 string
    var jp180 string
    switch self__0.(type) {
    case Empty:
        jp180 = "Key::Empty"
    case Point:
        var x152 int32 = self__0.(Point)._0
        var x153 int32 = self__0.(Point)._1
        var __field1__2 int32 = x153
        var __field0__1 int32 = x152
        var t181 string = "Key::Point { " + "x: "
        var t182 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field0__1)
        var t183 string = t181 + t182
        var t184 string = t183 + ", "
        var t185 string = t184 + "y: "
        var t186 string = _goml_m_inherent_i_int32_i_int32_i_to__string(__field1__2)
        var t187 string = t185 + t186
        var t188 string = t187 + " }"
        jp180 = t188
    default:
        panic("non-exhaustive match")
    }
    retv178 = jp180
    return retv178
}

func _goml_m_trait__impl_i_Eq_i_Key_i_eq(self__3 Key, other__4 Key) bool {
    var retv190 bool
    var mtmp154 Tuple2_3Key_3Key = Tuple2_3Key_3Key{
        _0: self__3,
        _1: other__4,
    }
    var x155 Key = mtmp154._0
    var x156 Key = mtmp154._1
    var jp192 bool
    switch x156.(type) {
    case Empty:
        var jp194 bool
        switch x155.(type) {
        case Empty:
            jp194 = true
        default:
            jp194 = false
        }
        jp192 = jp194
    case Point:
        var x157 int32 = x156.(Point)._0
        var x158 int32 = x156.(Point)._1
        var jp196 bool
        switch x155.(type) {
        case Point:
            var x161 int32 = x155.(Point)._0
            var x162 int32 = x155.(Point)._1
            var __l1_1__6 int32 = x162
            var __l1_0__5 int32 = x161
            var __r1_1__8 int32 = x158
            var __r1_0__7 int32 = x157
            var jp200 bool
            if true {
                var t202 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_0__5, __r1_0__7)
                jp200 = t202
            } else {
                jp200 = false
            }
            var jp198 bool
            if jp200 {
                var t201 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(__l1_1__6, __r1_1__8)
                jp198 = t201
            } else {
                jp198 = false
            }
            jp196 = jp198
        default:
            jp196 = false
        }
        jp192 = jp196
    default:
        panic("non-exhaustive match")
    }
    retv190 = jp192
    return retv190
}

func score(value__16 Message__string) int32 {
    var retv212 int32
    var jp214 int32
    switch value__16.(type) {
    case Quit:
        jp214 = 0
    case Write:
        jp214 = 1
    case Move:
        var x166 int32 = value__16.(Move)._0
        var x167 int32 = value__16.(Move)._1
        var vertical__18 int32 = x167
        var x__17 int32 = x166
        var t215 int32 = x__17 + vertical__18
        jp214 = t215
    default:
        panic("non-exhaustive match")
    }
    retv212 = jp214
    return retv212
}

func label(value__19 Message__string) string {
    var retv217 string
    var jp219 string
    switch value__19.(type) {
    case Quit:
        jp219 = "quit"
    case Write:
        var x169 string = value__19.(Write)._0
        var text__21 string = x169
        jp219 = text__21
    case Move:
        var x172 string = value__19.(Move)._2
        var label__20 string = x172
        jp219 = label__20
    default:
        panic("non-exhaustive match")
    }
    retv217 = jp219
    return retv217
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
    var t221 int32 = score(first__24)
    println__T_int32(t221)
    var t222 string = label(second__25)
    println__T_string(t222)
    var t223 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t224 string = _goml_m_trait__impl_i_ToString_i_Key_i_to__string(t223)
    println__T_string(t224)
    var t225 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t226 Key = Point{
        _0: 1,
        _1: 2,
    }
    var t227 bool = _goml_m_trait__impl_i_Eq_i_Key_i_eq(t225, t226)
    println__T_bool(t227)
    return struct{}{}
}

func _goml_m_inherent_i_int32_i_int32_i_to__string(self__6 int32) string {
    var retv229 string
    var t230 string = _goml_runtime_core_int32_to_string(self__6)
    retv229 = t230
    return retv229
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv232 bool
    var t233 bool = self__65 == other__66
    retv232 = t233
    return retv232
}

func println__T_int32(value__1 int32) struct{} {
    var t238 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t238)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t241 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t241)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t244 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t244)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv247 string
    var t248 string = _goml_runtime_core_int32_to_string(self__43)
    retv247 = t248
    return retv247
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv250 string
    retv250 = self__38
    return retv250
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv252 string
    var t253 string = _goml_runtime_core_bool_to_string(self__37)
    retv252 = t253
    return retv252
}

func main() {
    main0()
}
