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
    var retv100 int32
    var match64 Either = value__0
    var whole__1 Either = match64
    var jp102 int32
    switch whole__1.(type) {
    case Left:
        var shared__2 int32 = whole__1.(Left)._0
        var jp104 int32
        switch whole__1.(type) {
        case Left:
            jp104 = 0
        case Right:
            jp104 = 1
        default:
            panic("non-exhaustive match")
        }
        var t105 int32 = shared__2 + jp104
        jp102 = t105
    default:
        var jp107 int32
        switch whole__1.(type) {
        case Right:
            var shared__2 int32 = whole__1.(Right)._0
            var jp109 int32
            switch whole__1.(type) {
            case Left:
                jp109 = 0
            case Right:
                jp109 = 1
            default:
                panic("non-exhaustive match")
            }
            var t110 int32 = shared__2 + jp109
            jp107 = t110
        default:
            var t111 int32 = missing__int32("")
            jp107 = t111
        }
        jp102 = jp107
    }
    retv100 = jp102
    return retv100
}

func char_group(value__3 rune) string {
    var retv113 string
    var match69 rune = value__3
    var t116 bool = match69 >= 97
    var jp115 string
    if t116 {
        var t119 bool = match69 <= 99
        var jp118 string
        if t119 {
            jp118 = "abc"
        } else {
            jp118 = "other"
        }
        jp115 = jp118
    } else {
        jp115 = "other"
    }
    retv113 = jp115
    return retv113
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var retv121 string
    var jp123 string
    switch value__4.(type) {
    case Some:
        var x70 int32 = value__4.(Some)._0
        var n__7 int32 = x70
        var match71 int32 = n__7
        var t164 bool = match71 == 0
        var jp163 string
        if t164 {
            jp163 = "small"
        } else {
            var t167 bool = match71 == 1
            var jp166 string
            if t167 {
                jp166 = "small"
            } else {
                var t170 bool = match71 >= 2
                var jp169 string
                if t170 {
                    var t173 bool = match71 <= 4
                    var jp172 string
                    if t173 {
                        jp172 = "middle"
                    } else {
                        var x__8 int32 = match71
                        var t176 bool = x__8 > 10
                        var jp175 string
                        if t176 {
                            jp175 = "large"
                        } else {
                            jp175 = "other"
                        }
                        jp172 = jp175
                    }
                    jp169 = jp172
                } else {
                    var x__8 int32 = match71
                    var t179 bool = x__8 > 10
                    var jp178 string
                    if t179 {
                        jp178 = "large"
                    } else {
                        jp178 = "other"
                    }
                    jp169 = jp178
                }
                jp166 = jp169
            }
            jp163 = jp166
        }
        jp123 = jp163
    default:
        jp123 = "none"
    }
    var from_if__9 string = jp123
    var match72 *_goml_vec_int32 = numbers__5
    var t140 int = vec_len__Vec_5int32(match72)
    var t141 bool = t140 == 0
    var jp125 string
    if t141 {
        jp125 = "empty"
    } else {
        var t144 int = vec_len__Vec_5int32(match72)
        var t145 bool = t144 >= 1
        var jp143 string
        if t145 {
            var first__10 int32 = vec_get__Vec_5int32(match72, 0)
            var t146 int = vec_len__Vec_5int32(match72)
            var tail__11 []int32 = match72.items[1:t146]
            var t149 int = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(tail__11)
            var t150 int32 = int32(int(t149))
            var t151 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(first__10, t150)
            var jp148 string
            if t151 {
                jp148 = "balanced"
            } else {
                var t154 int = vec_len__Vec_5int32(match72)
                var t155 bool = t154 >= 1
                var jp153 string
                if t155 {
                    jp153 = "nonempty"
                } else {
                    var t156 string = missing__string("")
                    jp153 = t156
                }
                jp148 = jp153
            }
            jp143 = jp148
        } else {
            var t159 int = vec_len__Vec_5int32(match72)
            var t160 bool = t159 >= 1
            var jp158 string
            if t160 {
                jp158 = "nonempty"
            } else {
                var t161 string = missing__string("")
                jp158 = t161
            }
            jp143 = jp158
        }
        jp125 = jp143
    }
    var from_vec__12 string = jp125
    var match73 []int32 = view__6
    var t132 int = len(match73)
    var t133 bool = t132 >= 2
    var jp127 string
    if t133 {
        var first__13 int32 = match73[0]
        var t134 int = len(match73)
        var t135 int = t134 - 1
        var t136 int = t135 + 0
        var last__14 int32 = match73[t136]
        var t139 bool = _goml_m_trait__impl_i_Eq_i_int32_i_eq(first__13, last__14)
        var jp138 string
        if t139 {
            jp138 = "same ends"
        } else {
            jp138 = "different ends"
        }
        jp127 = jp138
    } else {
        jp127 = "different ends"
    }
    var from_slice__15 string = jp127
    var t128 string = from_if__9 + "/"
    var t129 string = t128 + from_vec__12
    var t130 string = t129 + "/"
    var t131 string = t130 + from_slice__15
    retv121 = t131
    return retv121
}

func main0() struct{} {
    var pair__16 Pair = Pair{
        left: 3,
        right: 9,
    }
    var mtmp74 Pair = pair__16
    var x75 int32 = mtmp74.left
    var left__17 int32 = x75
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var mtmp77 [4]int = values__18
    var first__19 int = array_get__Array_4_3int(mtmp77, 0)
    var last__21 int = array_get__Array_4_3int(mtmp77, 3)
    var t181 int = array_get__Array_4_3int(mtmp77, 1)
    var t182 int = array_get__Array_4_3int(mtmp77, 2)
    var middle__20 [2]int = [2]int{t181, t182}
    println__T_int32(left__17)
    var t183 int = array_get__Array_2_3int(middle__20, 0)
    var t184 int = first__19 + t183
    var t185 int = t184 + last__21
    println__T_int(t185)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t186 int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(numbers__22)
    var view__23 []int32 = numbers__22.items[0:t186]
    var t187 Maybe = Some{
        _0: 3,
    }
    var t188 string = describe(t187, numbers__22, view__23)
    println__T_string(t188)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t189 string = describe(None{}, empty__24, empty_view__25)
    println__T_string(t189)
    var t190 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(t190)
    Loop_loop204:
    for {
        if true {
            var mtmp84 Maybe = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(state__26)
            switch mtmp84.(type) {
            case Some:
                var x85 int32 = mtmp84.(Some)._0
                var n__27 int32 = x85
                println__T_int32(n__27)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(state__26, None{})
                continue
            default:
                break Loop_loop204
            }
        } else {
            break Loop_loop204
        }
    }
    var mtmp89 Maybe = Some{
        _0: 6,
    }
    switch mtmp89.(type) {
    case Some:
        var x90 int32 = mtmp89.(Some)._0
        var n__28 int32 = x90
        println__T_int32(n__28)
    default:
    }
    var match93 Maybe = Some{
        _0: 5,
    }
    var whole__29 Maybe = match93
    var jp194 int32
    switch whole__29.(type) {
    case Some:
        var value__30 int32 = whole__29.(Some)._0
        var jp199 int32
        switch whole__29.(type) {
        case None:
            jp199 = 0
        case Some:
            var x94 int32 = whole__29.(Some)._0
            var inner__31 int32 = x94
            jp199 = inner__31
        default:
            panic("non-exhaustive match")
        }
        var t200 int32 = value__30 + jp199
        jp194 = t200
    default:
        var jp202 int32
        switch match93.(type) {
        case None:
            jp202 = 0
        default:
            var t203 int32 = missing__int32("")
            jp202 = t203
        }
        jp194 = jp202
    }
    var aliased__32 int32 = jp194
    println__T_int32(aliased__32)
    var t195 Either = Right{
        _0: 11,
    }
    var t196 int32 = unwrap_either(t195)
    println__T_int32(t196)
    var t197 string = char_group(98)
    println__T_string(t197)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__188 []int32) int {
    var retv207 int
    var t208 int = len(self__188)
    retv207 = t208
    return retv207
}

func _goml_m_trait__impl_i_Eq_i_int32_i_eq(self__65 int32, other__66 int32) bool {
    var retv210 bool
    var t211 bool = self__65 == other__66
    retv210 = t211
    return retv210
}

func println__T_int32(value__1 int32) struct{} {
    var t213 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t213)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t216 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t216)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__139 *_goml_vec_int32) int {
    var retv219 int
    var t220 int = vec_len__Vec_5int32(self__139)
    retv219 = t220
    return retv219
}

func println__T_string(value__1 string) struct{} {
    var t222 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t222)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(value__209 Maybe) *ref_Maybe_x {
    var retv225 *ref_Maybe_x
    var t226 *ref_Maybe_x = ref__Ref_5Maybe(value__209)
    retv225 = t226
    return retv225
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(self__210 *ref_Maybe_x) Maybe {
    var retv228 Maybe
    var t229 Maybe = ref_get__Ref_5Maybe(self__210)
    retv228 = t229
    return retv228
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(self__211 *ref_Maybe_x, value__212 Maybe) struct{} {
    ref_set__Ref_5Maybe(self__211, value__212)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var retv233 string
    var t234 string = _goml_runtime_core_int32_to_string(self__43)
    retv233 = t234
    return retv233
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv236 string
    var t237 string = _goml_runtime_core_int_to_string(self__40)
    retv236 = t237
    return retv236
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv239 string
    retv239 = self__38
    return retv239
}

func main() {
    main0()
}
