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
    switch value__0.(type) {
    case First__int_Shared:
        var x155 int = value__0.(First__int_Shared)._0
        var t198 string
        var inline315 string = _goml_runtime_core_int_to_string(x155)
        t198 = inline315
        var t199 string = "shared:" + t198
        return t199
    case Idle:
        return "idle"
    case Data:
        var x156 int = value__0.(Data)._0
        var x157 string = value__0.(Data)._1
        var t200 string = x157 + ":"
        var t201 string
        var inline317 string = _goml_runtime_core_int_to_string(x156)
        t201 = inline317
        var t202 string = t200 + t201
        return t202
    default:
        panic("non-exhaustive match")
    }
}

func nested(value__4 Option__Result__int__string) string {
    switch value__4.(type) {
    case Option__Result__int__string_None:
        return "none"
    case Option__Result__int__string_Some:
        var x158 Result__int__string = value__4.(Option__Result__int__string_Some)._0
        switch x158.(type) {
        case Ok:
            var x159 int = x158.(Ok)._0
            var t209 string
            var inline319 string = _goml_runtime_core_int_to_string(x159)
            t209 = inline319
            var t210 string = "ok:" + t209
            return t210
        case Err:
            var x160 string = x158.(Err)._0
            var t211 string = "err:" + x160
            return t211
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func optional_number(value__7 Option__int) int {
    switch value__7.(type) {
    case Option__int_Some:
        var x161 int = value__7.(Option__int_Some)._0
        return x161
    default:
        return 0
    }
}

func take_once(value__10 Option__int) int {
    var current__11 *ref_Option__int_x
    var inline332 *ref_Option__int_x = ref__Ref_11Option__int(value__10)
    current__11 = inline332
    var result__12 *ref_int_x
    var inline329 int = 0
    var inline330 *ref_int_x = ref__Ref_3int(inline329)
    result__12 = inline330
    Loop_loop224:
    for {
        var mtmp165 Option__int
        var inline325 Option__int = ref_get__Ref_11Option__int(current__11)
        mtmp165 = inline325
        switch mtmp165.(type) {
        case Option__int_Some:
            var x166 int = mtmp165.(Option__int_Some)._0
            ref_set__Ref_3int(result__12, x166)
            ref_set__Ref_11Option__int(current__11, Option__int_None{})
            continue
        default:
            break Loop_loop224
        }
    }
    var inline327 int = ref_get__Ref_3int(result__12)
    return inline327
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var result__17 *ref_int_x
    var inline340 int = 0
    var inline341 *ref_int_x = ref__Ref_3int(inline340)
    result__17 = inline341
    var for_limit173 int = vec_len__Vec_10Boxed__int(values__16)
    var for_index174 int = 0
    Loop_loop234:
    for {
        var t235 bool = for_index174 < for_limit173
        if t235 {
            var for_item175 Boxed__int = vec_get__Vec_10Boxed__int(values__16, for_index174)
            var t236 int = for_index174 + 1
            for_index174 = t236
            switch for_item175.(type) {
            case Value:
                var x177 int = for_item175.(Value)._0
                var t238 int
                var inline336 int = ref_get__Ref_3int(result__17)
                t238 = inline336
                var t239 int = t238 + x177
                ref_set__Ref_3int(result__17, t239)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop234
        }
    }
    var inline338 int = ref_get__Ref_3int(result__17)
    return inline338
}

func main0() struct{} {
    var vec_literal__1450 *_goml_vec_Boxed__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_()
    var t241 Boxed__int = Value{
        _0: 19,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t241)
    var t242 Boxed__int = Value{
        _0: 23,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t242)
    var t243 First__int = First__int_Shared{
        _0: 7,
    }
    var t244 string = classify(t243)
    println__T_string(t244)
    var t245 string = classify(Idle{})
    println__T_string(t245)
    var t246 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t247 string = classify(t246)
    println__T_string(t247)
    var t248 Result__int__string = Ok{
        _0: 11,
    }
    var t249 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t248,
    }
    var t250 string = nested(t249)
    println__T_string(t250)
    var t251 Result__int__string = Err{
        _0: "bad",
    }
    var t252 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t251,
    }
    var t253 string = nested(t252)
    println__T_string(t253)
    var t254 string = nested(Option__Result__int__string_None{})
    var inline379 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t254)
    _goml_runtime_core_string_println(inline379)
    var t255 Option__int = Option__int_Some{
        _0: 13,
    }
    var t256 int = optional_number(t255)
    var inline376 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t256)
    _goml_runtime_core_string_println(inline376)
    var t257 int
    t257 = 0
    var inline370 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t257)
    _goml_runtime_core_string_println(inline370)
    var t258 bool
    t258 = true
    var inline363 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t258)
    _goml_runtime_core_string_println(inline363)
    var t260 bool
    t260 = false
    var inline356 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t260)
    _goml_runtime_core_string_println(inline356)
    var t261 Option__int = Option__int_Some{
        _0: 15,
    }
    var t262 int = take_once(t261)
    var inline353 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t262)
    _goml_runtime_core_string_println(inline353)
    var t264 int
    var inline350 int = 17
    t264 = inline350
    var inline346 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t264)
    _goml_runtime_core_string_println(inline346)
    var t265 int = sum_boxed(vec_literal__1450)
    var inline343 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t265)
    _goml_runtime_core_string_println(inline343)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_() *_goml_vec_Boxed__int {
    var t288 *_goml_vec_Boxed__int = vec_new__Vec_10Boxed__int()
    return t288
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(self__126 *_goml_vec_Boxed__int, elem__127 Boxed__int) struct{} {
    vec_push__Vec_10Boxed__int(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t292 string
    t292 = value__1
    _goml_runtime_core_string_println(t292)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t304 string = _goml_runtime_core_int_to_string(self__40)
    return t304
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var t307 string = _goml_runtime_core_bool_to_string(self__37)
    return t307
}

func main() {
    main0()
}
