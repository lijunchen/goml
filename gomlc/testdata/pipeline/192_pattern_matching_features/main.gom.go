package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_intrinsic_missing(s string) struct{} {
    println("missing: " + s)
    panic("")
    return struct{}{}
}

func array_get__Array_4_3int(arr [4]int, index int) int {
    return arr[index]
}

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

type ref_Maybe_x struct {
    value Maybe
}

func ref__Ref_5Maybe(value Maybe) *ref_Maybe_x {
    return &ref_Maybe_x{
        value: value,
    }
}

func ref_get__Ref_5Maybe(reference *ref_Maybe_x) Maybe {
    return reference.value
}

func ref_set__Ref_5Maybe(reference *ref_Maybe_x, value Maybe) struct{} {
    reference.value = value
    return struct{}{}
}

func missing__int32(s string) int32 {
    _goml_intrinsic_missing(s)
    var ret int32
    return ret
}

func missing__string(s string) string {
    _goml_intrinsic_missing(s)
    var ret string
    return ret
}

type Pair struct {
    left int32
    right int32
}

type Maybe interface {
    isMaybe()
}

type None struct {}

func (_ None) isMaybe() {}

type Some struct {
    _0 int32
}

func (_ Some) isMaybe() {}

type Either interface {
    isEither()
}

type Left struct {
    _0 int32
}

func (_ Left) isEither() {}

type Right struct {
    _0 int32
}

func (_ Right) isEither() {}

func unwrap_either(value__0 Either) int32 {
    var retv104 int32
    var match68 Either = value__0
    var whole__1 Either = match68
    var jp106 int32
    switch whole__1.(type) {
    case Left:
        var shared__2 int32 = whole__1.(Left)._0
        var jp108 int32
        switch whole__1.(type) {
        case Left:
            jp108 = 0
        case Right:
            jp108 = 1
        default:
            panic("non-exhaustive match")
        }
        var t109 int32 = shared__2 + jp108
        jp106 = t109
    default:
        var jp111 int32
        switch whole__1.(type) {
        case Right:
            var shared__2 int32 = whole__1.(Right)._0
            var jp113 int32
            switch whole__1.(type) {
            case Left:
                jp113 = 0
            case Right:
                jp113 = 1
            default:
                panic("non-exhaustive match")
            }
            var t114 int32 = shared__2 + jp113
            jp111 = t114
        default:
            var t115 int32 = missing__int32("")
            jp111 = t115
        }
        jp106 = jp111
    }
    retv104 = jp106
    return retv104
}

func char_group(value__3 rune) string {
    var retv117 string
    var match73 rune = value__3
    var t120 bool = match73 >= 97
    var jp119 string
    if t120 {
        var t123 bool = match73 <= 99
        var jp122 string
        if t123 {
            jp122 = "abc"
        } else {
            jp122 = "other"
        }
        jp119 = jp122
    } else {
        jp119 = "other"
    }
    retv117 = jp119
    return retv117
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var retv125 string
    var jp127 string
    switch value__4.(type) {
    case Some:
        var x74 int32 = value__4.(Some)._0
        var n__7 int32 = x74
        var match75 int32 = n__7
        var t168 bool = match75 == 0
        var jp167 string
        if t168 {
            jp167 = "small"
        } else {
            var t171 bool = match75 == 1
            var jp170 string
            if t171 {
                jp170 = "small"
            } else {
                var t174 bool = match75 >= 2
                var jp173 string
                if t174 {
                    var t177 bool = match75 <= 4
                    var jp176 string
                    if t177 {
                        jp176 = "middle"
                    } else {
                        var x__8 int32 = match75
                        var t180 bool = x__8 > 10
                        var jp179 string
                        if t180 {
                            jp179 = "large"
                        } else {
                            jp179 = "other"
                        }
                        jp176 = jp179
                    }
                    jp173 = jp176
                } else {
                    var x__8 int32 = match75
                    var t183 bool = x__8 > 10
                    var jp182 string
                    if t183 {
                        jp182 = "large"
                    } else {
                        jp182 = "other"
                    }
                    jp173 = jp182
                }
                jp170 = jp173
            }
            jp167 = jp170
        }
        jp127 = jp167
    default:
        jp127 = "none"
    }
    var from_if__9 string = jp127
    var match76 *_goml_vec_int32 = numbers__5
    var t144 int = vec_len__Vec_5int32(match76)
    var t145 bool = t144 == 0
    var jp129 string
    if t145 {
        jp129 = "empty"
    } else {
        var t148 int = vec_len__Vec_5int32(match76)
        var t149 bool = t148 >= 1
        var jp147 string
        if t149 {
            var first__10 int32 = vec_get__Vec_5int32(match76, 0)
            var t150 int = vec_len__Vec_5int32(match76)
            var tail__11 []int32 = match76.items[1:t150]
            var t153 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(tail__11)
            var t154 int32 = int32(int(t153))
            var t155 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(first__10, t154)
            var jp152 string
            if t155 {
                jp152 = "balanced"
            } else {
                var t158 int = vec_len__Vec_5int32(match76)
                var t159 bool = t158 >= 1
                var jp157 string
                if t159 {
                    jp157 = "nonempty"
                } else {
                    var t160 string = missing__string("")
                    jp157 = t160
                }
                jp152 = jp157
            }
            jp147 = jp152
        } else {
            var t163 int = vec_len__Vec_5int32(match76)
            var t164 bool = t163 >= 1
            var jp162 string
            if t164 {
                jp162 = "nonempty"
            } else {
                var t165 string = missing__string("")
                jp162 = t165
            }
            jp147 = jp162
        }
        jp129 = jp147
    }
    var from_vec__12 string = jp129
    var match77 []int32 = view__6
    var t136 int = len(match77)
    var t137 bool = t136 >= 2
    var jp131 string
    if t137 {
        var first__13 int32 = match77[0]
        var t138 int = len(match77)
        var t139 int = t138 - 1
        var t140 int = t139 + 0
        var last__14 int32 = match77[t140]
        var t143 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(first__13, last__14)
        var jp142 string
        if t143 {
            jp142 = "same ends"
        } else {
            jp142 = "different ends"
        }
        jp131 = jp142
    } else {
        jp131 = "different ends"
    }
    var from_slice__15 string = jp131
    var t132 string = from_if__9 + "/"
    var t133 string = t132 + from_vec__12
    var t134 string = t133 + "/"
    var t135 string = t134 + from_slice__15
    retv125 = t135
    return retv125
}

func main0() struct{} {
    var pair__16 Pair = Pair{
        left: 3,
        right: 9,
    }
    var mtmp78 Pair = pair__16
    var x79 int32 = mtmp78.left
    var left__17 int32 = x79
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var mtmp81 [4]int = values__18
    var first__19 int = array_get__Array_4_3int(mtmp81, 0)
    var last__21 int = array_get__Array_4_3int(mtmp81, 3)
    var t185 int = array_get__Array_4_3int(mtmp81, 1)
    var t186 int = array_get__Array_4_3int(mtmp81, 2)
    var middle__20 [2]int = [2]int{t185, t186}
    println__T_int32(left__17)
    var t187 int = array_get__Array_2_3int(middle__20, 0)
    var t188 int = first__19 + t187
    var t189 int = t188 + last__21
    println__T_int(t189)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t190 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(numbers__22)
    var view__23 []int32 = numbers__22.items[0:t190]
    var t191 Maybe = Some{
        _0: 3,
    }
    var t192 string = describe(t191, numbers__22, view__23)
    println__T_string(t192)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t193 string = describe(None{}, empty__24, empty_view__25)
    println__T_string(t193)
    var t194 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(t194)
    Loop_loop208:
    for {
        if true {
            var mtmp88 Maybe = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(state__26)
            switch mtmp88.(type) {
            case Some:
                var x89 int32 = mtmp88.(Some)._0
                var n__27 int32 = x89
                println__T_int32(n__27)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(state__26, None{})
                continue
            default:
                break Loop_loop208
            }
        } else {
            break Loop_loop208
        }
    }
    var mtmp93 Maybe = Some{
        _0: 6,
    }
    switch mtmp93.(type) {
    case Some:
        var x94 int32 = mtmp93.(Some)._0
        var n__28 int32 = x94
        println__T_int32(n__28)
    default:
    }
    var match97 Maybe = Some{
        _0: 5,
    }
    var whole__29 Maybe = match97
    var jp198 int32
    switch whole__29.(type) {
    case Some:
        var value__30 int32 = whole__29.(Some)._0
        var jp203 int32
        switch whole__29.(type) {
        case None:
            jp203 = 0
        case Some:
            var x98 int32 = whole__29.(Some)._0
            var inner__31 int32 = x98
            jp203 = inner__31
        default:
            panic("non-exhaustive match")
        }
        var t204 int32 = value__30 + jp203
        jp198 = t204
    default:
        var jp206 int32
        switch match97.(type) {
        case None:
            jp206 = 0
        default:
            var t207 int32 = missing__int32("")
            jp206 = t207
        }
        jp198 = jp206
    }
    var aliased__32 int32 = jp198
    println__T_int32(aliased__32)
    var t199 Either = Right{
        _0: 11,
    }
    var t200 int32 = unwrap_either(t199)
    println__T_int32(t200)
    var t201 string = char_group(98)
    println__T_string(t201)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__186 []int32) int {
    var retv211 int
    var t212 int = len(self__186)
    retv211 = t212
    return retv211
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv214 bool
    var t215 bool = self__65 == other__66
    retv214 = t215
    return retv214
}

func println__T_int32(value__1 int32) struct{} {
    var t217 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t217)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t220 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t220)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__137 *_goml_vec_int32) int {
    var retv223 int
    var t224 int = vec_len__Vec_5int32(self__137)
    retv223 = t224
    return retv223
}

func println__T_string(value__1 string) struct{} {
    var t226 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t226)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(value__207 Maybe) *ref_Maybe_x {
    var retv229 *ref_Maybe_x
    var t230 *ref_Maybe_x = ref__Ref_5Maybe(value__207)
    retv229 = t230
    return retv229
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(self__208 *ref_Maybe_x) Maybe {
    var retv232 Maybe
    var t233 Maybe = ref_get__Ref_5Maybe(self__208)
    retv232 = t233
    return retv232
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(self__209 *ref_Maybe_x, value__210 Maybe) struct{} {
    ref_set__Ref_5Maybe(self__209, value__210)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv237 string
    var t238 string = _goml_runtime_core_int32_to_string(self__43)
    retv237 = t238
    return retv237
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv240 string
    var t241 string = _goml_runtime_core_int_to_string(self__40)
    retv240 = t241
    return retv240
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv243 string
    retv243 = self__38
    return retv243
}

func main() {
    main0()
}
