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

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type _goml_vec_Boxed__int struct {
    items []Boxed__int
}

func vec_new__Vec_10Boxed__int() *_goml_vec_Boxed__int {
    return &_goml_vec_Boxed__int{
        items: nil,
    }
}

func vec_push__Vec_10Boxed__int(vec *_goml_vec_Boxed__int, elem Boxed__int) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_10Boxed__int(vec *_goml_vec_Boxed__int, index int) Boxed__int {
    return vec.items[index]
}

func vec_len__Vec_10Boxed__int(vec *_goml_vec_Boxed__int) int {
    return int(len(vec.items))
}

type ref_Option__int_x struct {
    value Option__int
}

func ref__Ref_11Option__int(value Option__int) *ref_Option__int_x {
    return &ref_Option__int_x{
        value: value,
    }
}

func ref_get__Ref_11Option__int(reference *ref_Option__int_x) Option__int {
    return reference.value
}

func ref_set__Ref_11Option__int(reference *ref_Option__int_x, value Option__int) struct{} {
    reference.value = value
    return struct{}{}
}

type ref_int_x struct {
    value int
}

func ref__Ref_3int(value int) *ref_int_x {
    return &ref_int_x{
        value: value,
    }
}

func ref_get__Ref_3int(reference *ref_int_x) int {
    return reference.value
}

func ref_set__Ref_3int(reference *ref_int_x, value int) struct{} {
    reference.value = value
    return struct{}{}
}

type Second interface {
    isSecond()
}

type Second_Shared struct {
    _0 int
}

func (_ Second_Shared) isSecond() {}

type First__int interface {
    isFirst__int()
}

type First__int_Shared struct {
    _0 int
}

func (_ First__int_Shared) isFirst__int() {}

type Idle struct {}

func (_ Idle) isFirst__int() {}

type Data struct {
    _0 int
    _1 string
}

func (_ Data) isFirst__int() {}

type Result__int__string interface {
    isResult__int__string()
}

type Ok struct {
    _0 int
}

func (_ Ok) isResult__int__string() {}

type Err struct {
    _0 string
}

func (_ Err) isResult__int__string() {}

type Option__Result__int__string interface {
    isOption__Result__int__string()
}

type Option__Result__int__string_None struct {}

func (_ Option__Result__int__string_None) isOption__Result__int__string() {}

type Option__Result__int__string_Some struct {
    _0 Result__int__string
}

func (_ Option__Result__int__string_Some) isOption__Result__int__string() {}

type Option__int interface {
    isOption__int()
}

type Option__int_None struct {}

func (_ Option__int_None) isOption__int() {}

type Option__int_Some struct {
    _0 int
}

func (_ Option__int_Some) isOption__int() {}

type Boxed__int interface {
    isBoxed__int()
}

type Value struct {
    _0 int
}

func (_ Value) isBoxed__int() {}

func classify(value__0 First__int) string {
    var retv148 string
    var jp150 string
    switch value__0.(type) {
    case First__int_Shared:
        var x108 int = value__0.(First__int_Shared)._0
        var number__1 int = x108
        var t151 string = _goml_m_inherent_i_int_i_int_i_to__string(number__1)
        var t152 string = "shared:" + t151
        jp150 = t152
    case Idle:
        jp150 = "idle"
    case Data:
        var x109 int = value__0.(Data)._0
        var x110 string = value__0.(Data)._1
        var label__3 string = x110
        var value__2 int = x109
        var t153 string = label__3 + ":"
        var t154 string = _goml_m_inherent_i_int_i_int_i_to__string(value__2)
        var t155 string = t153 + t154
        jp150 = t155
    default:
        panic("non-exhaustive match")
    }
    retv148 = jp150
    return retv148
}

func nested(value__4 Option__Result__int__string) string {
    var retv157 string
    var jp159 string
    switch value__4.(type) {
    case Option__Result__int__string_None:
        jp159 = "none"
    case Option__Result__int__string_Some:
        var x111 Result__int__string = value__4.(Option__Result__int__string_Some)._0
        var jp161 string
        switch x111.(type) {
        case Ok:
            var x112 int = x111.(Ok)._0
            var number__5 int = x112
            var t162 string = _goml_m_inherent_i_int_i_int_i_to__string(number__5)
            var t163 string = "ok:" + t162
            jp161 = t163
        case Err:
            var x113 string = x111.(Err)._0
            var message__6 string = x113
            var t164 string = "err:" + message__6
            jp161 = t164
        default:
            panic("non-exhaustive match")
        }
        jp159 = jp161
    default:
        panic("non-exhaustive match")
    }
    retv157 = jp159
    return retv157
}

func optional_number(value__7 Option__int) int {
    var retv166 int
    var jp168 int
    switch value__7.(type) {
    case Option__int_Some:
        var x114 int = value__7.(Option__int_Some)._0
        var number__8 int = x114
        jp168 = number__8
    default:
        jp168 = 0
    }
    retv166 = jp168
    return retv166
}

func is_non_shared(value__9 First__int) bool {
    var retv170 bool
    var jp172 bool
    switch value__9.(type) {
    case First__int_Shared:
        jp172 = false
    case Idle:
        jp172 = true
    case Data:
        jp172 = true
    default:
        panic("non-exhaustive match")
    }
    retv170 = jp172
    return retv170
}

func take_once(value__10 Option__int) int {
    var retv174 int
    var current__11 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__10)
    var result__12 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop177:
    for {
        if true {
            var mtmp118 Option__int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_int_r_(current__11)
            switch mtmp118.(type) {
            case Option__int_Some:
                var x119 int = mtmp118.(Option__int_Some)._0
                var number__13 int = x119
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__12, number__13)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_int_r_(current__11, Option__int_None{})
                continue
            default:
                break Loop_loop177
            }
        } else {
            break Loop_loop177
        }
    }
    var t176 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__12)
    retv174 = t176
    return retv174
}

func unbox(value__14 Boxed__int) int {
    var retv180 int
    var mtmp123 Boxed__int = value__14
    var jp182 int
    switch mtmp123.(type) {
    case Value:
        var x124 int = mtmp123.(Value)._0
        var number__15 int = x124
        jp182 = number__15
    default:
        panic("non-exhaustive match")
    }
    retv180 = jp182
    return retv180
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var retv184 int
    var result__17 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var for_source125 *_goml_vec_Boxed__int = values__16
    var for_limit126 int = vec_len__Vec_10Boxed__int(for_source125)
    var for_index127 int = 0
    Loop_loop187:
    for {
        var t188 bool = for_index127 < for_limit126
        if t188 {
            var for_item128 Boxed__int = vec_get__Vec_10Boxed__int(for_source125, for_index127)
            var t189 int = for_index127 + 1
            for_index127 = t189
            switch for_item128.(type) {
            case Value:
                var x130 int = for_item128.(Value)._0
                var number__18 int = x130
                var t191 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__17)
                var t192 int = t191 + number__18
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__17, t192)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop187
        }
    }
    var t186 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__17)
    retv184 = t186
    return retv184
}

func main0() struct{} {
    var vec_literal__1450 *_goml_vec_Boxed__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_()
    var t194 Boxed__int = Value{
        _0: 19,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t194)
    var t195 Boxed__int = Value{
        _0: 23,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t195)
    var boxed__19 *_goml_vec_Boxed__int = vec_literal__1450
    var t196 First__int = First__int_Shared{
        _0: 7,
    }
    var t197 string = classify(t196)
    println__T_string(t197)
    var t198 string = classify(Idle{})
    println__T_string(t198)
    var t199 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t200 string = classify(t199)
    println__T_string(t200)
    var t201 Result__int__string = Ok{
        _0: 11,
    }
    var t202 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t201,
    }
    var t203 string = nested(t202)
    println__T_string(t203)
    var t204 Result__int__string = Err{
        _0: "bad",
    }
    var t205 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t204,
    }
    var t206 string = nested(t205)
    println__T_string(t206)
    var t207 string = nested(Option__Result__int__string_None{})
    println__T_string(t207)
    var t208 Option__int = Option__int_Some{
        _0: 13,
    }
    var t209 int = optional_number(t208)
    println__T_int(t209)
    var t210 int = optional_number(Option__int_None{})
    println__T_int(t210)
    var t211 bool = is_non_shared(Idle{})
    println__T_bool(t211)
    var t212 First__int = First__int_Shared{
        _0: 1,
    }
    var t213 bool = is_non_shared(t212)
    println__T_bool(t213)
    var t214 Option__int = Option__int_Some{
        _0: 15,
    }
    var t215 int = take_once(t214)
    println__T_int(t215)
    var t216 Boxed__int = Value{
        _0: 17,
    }
    var t217 int = unbox(t216)
    println__T_int(t217)
    var t218 int = sum_boxed(boxed__19)
    println__T_int(t218)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv221 string
    var t222 string = _goml_runtime_core_int_to_string(self__5)
    retv221 = t222
    return retv221
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__207 Option__int) *ref_Option__int_x {
    var retv224 *ref_Option__int_x
    var t225 *ref_Option__int_x = ref__Ref_11Option__int(value__207)
    retv224 = t225
    return retv224
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv227 *ref_int_x
    var t228 *ref_int_x = ref__Ref_3int(value__207)
    retv227 = t228
    return retv227
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_int_r_(self__208 *ref_Option__int_x) Option__int {
    var retv230 Option__int
    var t231 Option__int = ref_get__Ref_11Option__int(self__208)
    retv230 = t231
    return retv230
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(self__209 *ref_int_x, value__210 int) struct{} {
    ref_set__Ref_3int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_int_r_(self__209 *ref_Option__int_x, value__210 Option__int) struct{} {
    ref_set__Ref_11Option__int(self__209, value__210)
    return struct{}{}
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(self__208 *ref_int_x) int {
    var retv237 int
    var t238 int = ref_get__Ref_3int(self__208)
    retv237 = t238
    return retv237
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_() *_goml_vec_Boxed__int {
    var retv240 *_goml_vec_Boxed__int
    var t241 *_goml_vec_Boxed__int = vec_new__Vec_10Boxed__int()
    retv240 = t241
    return retv240
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(self__126 *_goml_vec_Boxed__int, elem__127 Boxed__int) struct{} {
    vec_push__Vec_10Boxed__int(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t245)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t248 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t248)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t251 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t251)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv254 string
    retv254 = self__38
    return retv254
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv256 string
    var t257 string = _goml_runtime_core_int_to_string(self__40)
    retv256 = t257
    return retv256
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv259 string
    var t260 string = _goml_runtime_core_bool_to_string(self__37)
    retv259 = t260
    return retv259
}

func main() {
    main0()
}
