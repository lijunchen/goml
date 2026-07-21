package main

import (
    _goml_fmt "fmt"
)

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

func array_get__Array_4_5int32(arr [4]int32, index int32) int32 {
    return arr[index]
}

func array_get__Array_2_5int32(arr [2]int32, index int32) int32 {
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

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int32) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int32 {
    return int32(len(vec.items))
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
    var retv97 int32
    var match61 Either = value__0
    var whole__1 Either = match61
    var jp99 int32
    switch whole__1.(type) {
    case Left:
        var shared__2 int32 = whole__1.(Left)._0
        var jp101 int32
        switch whole__1.(type) {
        case Left:
            jp101 = 0
        case Right:
            jp101 = 1
        default:
            panic("non-exhaustive match")
        }
        var t102 int32 = shared__2 + jp101
        jp99 = t102
    default:
        var jp104 int32
        switch whole__1.(type) {
        case Right:
            var shared__2 int32 = whole__1.(Right)._0
            var jp106 int32
            switch whole__1.(type) {
            case Left:
                jp106 = 0
            case Right:
                jp106 = 1
            default:
                panic("non-exhaustive match")
            }
            var t107 int32 = shared__2 + jp106
            jp104 = t107
        default:
            var t108 int32 = missing__int32("")
            jp104 = t108
        }
        jp99 = jp104
    }
    retv97 = jp99
    return retv97
}

func char_group(value__3 rune) string {
    var retv110 string
    var match66 rune = value__3
    var t113 bool = match66 >= 97
    var jp112 string
    if t113 {
        var t116 bool = match66 <= 99
        var jp115 string
        if t116 {
            jp115 = "abc"
        } else {
            jp115 = "other"
        }
        jp112 = jp115
    } else {
        jp112 = "other"
    }
    retv110 = jp112
    return retv110
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var retv118 string
    var jp120 string
    switch value__4.(type) {
    case Some:
        var x67 int32 = value__4.(Some)._0
        var n__7 int32 = x67
        var match68 int32 = n__7
        var t160 bool = match68 == 0
        var jp159 string
        if t160 {
            jp159 = "small"
        } else {
            var t163 bool = match68 == 1
            var jp162 string
            if t163 {
                jp162 = "small"
            } else {
                var t166 bool = match68 >= 2
                var jp165 string
                if t166 {
                    var t169 bool = match68 <= 4
                    var jp168 string
                    if t169 {
                        jp168 = "middle"
                    } else {
                        var x__8 int32 = match68
                        var t172 bool = x__8 > 10
                        var jp171 string
                        if t172 {
                            jp171 = "large"
                        } else {
                            jp171 = "other"
                        }
                        jp168 = jp171
                    }
                    jp165 = jp168
                } else {
                    var x__8 int32 = match68
                    var t175 bool = x__8 > 10
                    var jp174 string
                    if t175 {
                        jp174 = "large"
                    } else {
                        jp174 = "other"
                    }
                    jp165 = jp174
                }
                jp162 = jp165
            }
            jp159 = jp162
        }
        jp120 = jp159
    default:
        jp120 = "none"
    }
    var from_if__9 string = jp120
    var match69 *_goml_vec_int32 = numbers__5
    var t137 int32 = vec_len__Vec_5int32(match69)
    var t138 bool = t137 == 0
    var jp122 string
    if t138 {
        jp122 = "empty"
    } else {
        var t141 int32 = vec_len__Vec_5int32(match69)
        var t142 bool = t141 >= 1
        var jp140 string
        if t142 {
            var first__10 int32 = vec_get__Vec_5int32(match69, 0)
            var t143 int32 = vec_len__Vec_5int32(match69)
            var tail__11 []int32 = match69.items[1:t143]
            var t146 int32 = _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(tail__11)
            var t147 bool = first__10 == t146
            var jp145 string
            if t147 {
                jp145 = "balanced"
            } else {
                var t150 int32 = vec_len__Vec_5int32(match69)
                var t151 bool = t150 >= 1
                var jp149 string
                if t151 {
                    jp149 = "nonempty"
                } else {
                    var t152 string = missing__string("")
                    jp149 = t152
                }
                jp145 = jp149
            }
            jp140 = jp145
        } else {
            var t155 int32 = vec_len__Vec_5int32(match69)
            var t156 bool = t155 >= 1
            var jp154 string
            if t156 {
                jp154 = "nonempty"
            } else {
                var t157 string = missing__string("")
                jp154 = t157
            }
            jp140 = jp154
        }
        jp122 = jp140
    }
    var from_vec__12 string = jp122
    var match70 []int32 = view__6
    var t129 int32 = int32(len(match70))
    var t130 bool = t129 >= 2
    var jp124 string
    if t130 {
        var first__13 int32 = match70[0]
        var t131 int32 = int32(len(match70))
        var t132 int32 = t131 - 1
        var t133 int32 = t132 + 0
        var last__14 int32 = match70[t133]
        var t136 bool = first__13 == last__14
        var jp135 string
        if t136 {
            jp135 = "same ends"
        } else {
            jp135 = "different ends"
        }
        jp124 = jp135
    } else {
        jp124 = "different ends"
    }
    var from_slice__15 string = jp124
    var t125 string = from_if__9 + "/"
    var t126 string = t125 + from_vec__12
    var t127 string = t126 + "/"
    var t128 string = t127 + from_slice__15
    retv118 = t128
    return retv118
}

func main0() struct{} {
    var pair__16 Pair = Pair{
        left: 3,
        right: 9,
    }
    var mtmp71 Pair = pair__16
    var x72 int32 = mtmp71.left
    var left__17 int32 = x72
    var values__18 [4]int32 = [4]int32{1, 2, 3, 1}
    var mtmp74 [4]int32 = values__18
    var first__19 int32 = array_get__Array_4_5int32(mtmp74, 0)
    var last__21 int32 = array_get__Array_4_5int32(mtmp74, 3)
    var t177 int32 = array_get__Array_4_5int32(mtmp74, 1)
    var t178 int32 = array_get__Array_4_5int32(mtmp74, 2)
    var middle__20 [2]int32 = [2]int32{t177, t178}
    println__T_int32(left__17)
    var t179 int32 = array_get__Array_2_5int32(middle__20, 0)
    var t180 int32 = first__19 + t179
    var t181 int32 = t180 + last__21
    println__T_int32(t181)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t182 int32 = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(numbers__22)
    var view__23 []int32 = numbers__22.items[0:t182]
    var t183 Maybe = Some{
        _0: 3,
    }
    var t184 string = describe(t183, numbers__22, view__23)
    println__T_string(t184)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t185 string = describe(None{}, empty__24, empty_view__25)
    println__T_string(t185)
    var t186 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(t186)
    Loop_loop200:
    for {
        if true {
            var mtmp81 Maybe = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(state__26)
            switch mtmp81.(type) {
            case Some:
                var x82 int32 = mtmp81.(Some)._0
                var n__27 int32 = x82
                println__T_int32(n__27)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(state__26, None{})
                continue
            default:
                break Loop_loop200
            }
        } else {
            break Loop_loop200
        }
    }
    var mtmp86 Maybe = Some{
        _0: 6,
    }
    switch mtmp86.(type) {
    case Some:
        var x87 int32 = mtmp86.(Some)._0
        var n__28 int32 = x87
        println__T_int32(n__28)
    default:
    }
    var match90 Maybe = Some{
        _0: 5,
    }
    var whole__29 Maybe = match90
    var jp190 int32
    switch whole__29.(type) {
    case Some:
        var value__30 int32 = whole__29.(Some)._0
        var jp195 int32
        switch whole__29.(type) {
        case None:
            jp195 = 0
        case Some:
            var x91 int32 = whole__29.(Some)._0
            var inner__31 int32 = x91
            jp195 = inner__31
        default:
            panic("non-exhaustive match")
        }
        var t196 int32 = value__30 + jp195
        jp190 = t196
    default:
        var jp198 int32
        switch match90.(type) {
        case None:
            jp198 = 0
        default:
            var t199 int32 = missing__int32("")
            jp198 = t199
        }
        jp190 = jp198
    }
    var aliased__32 int32 = jp190
    println__T_int32(aliased__32)
    var t191 Either = Right{
        _0: 11,
    }
    var t192 int32 = unwrap_either(t191)
    println__T_int32(t192)
    var t193 string = char_group(98)
    println__T_string(t193)
    return struct{}{}
}

func _goml_m_inherent_i_Slice_i_Slice_l_T_r__i_len____T__int32(self__183 []int32) int32 {
    var retv203 int32
    var t204 int32 = int32(len(self__183))
    retv203 = t204
    return retv203
}

func println__T_int32(value__1 int32) struct{} {
    var t206 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(value__1)
    _goml_runtime_core_string_println(t206)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_len____T__int32(self__134 *_goml_vec_int32) int32 {
    var retv209 int32
    var t210 int32 = vec_len__Vec_5int32(self__134)
    retv209 = t210
    return retv209
}

func println__T_string(value__1 string) struct{} {
    var t212 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t212)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Maybe(value__204 Maybe) *ref_Maybe_x {
    var retv215 *ref_Maybe_x
    var t216 *ref_Maybe_x = ref__Ref_5Maybe(value__204)
    retv215 = t216
    return retv215
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Maybe(self__205 *ref_Maybe_x) Maybe {
    var retv218 Maybe
    var t219 Maybe = ref_get__Ref_5Maybe(self__205)
    retv218 = t219
    return retv218
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Maybe(self__206 *ref_Maybe_x, value__207 Maybe) struct{} {
    ref_set__Ref_5Maybe(self__206, value__207)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__41 int32) string {
    var retv223 string
    var t224 string = _goml_runtime_core_int32_to_string(self__41)
    retv223 = t224
    return retv223
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv226 string
    retv226 = self__37
    return retv226
}

func main() {
    main0()
}
