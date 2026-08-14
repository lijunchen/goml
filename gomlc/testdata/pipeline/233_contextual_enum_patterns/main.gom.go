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
        var x187 int = value__0.(First__int_Shared)._0
        var t230 string
        var inline362 string = _goml_runtime_core_int_to_string(x187)
        t230 = inline362
        var t231 string = "shared:" + t230
        return t231
    case Idle:
        return "idle"
    case Data:
        var x188 int = value__0.(Data)._0
        var x189 string = value__0.(Data)._1
        var t232 string = x189 + ":"
        var t233 string
        var inline364 string = _goml_runtime_core_int_to_string(x188)
        t233 = inline364
        var t234 string = t232 + t233
        return t234
    default:
        panic("non-exhaustive match")
    }
}

func nested(value__4 Option__Result__int__string) string {
    switch value__4.(type) {
    case Option__Result__int__string_None:
        return "none"
    case Option__Result__int__string_Some:
        var x190 Result__int__string = value__4.(Option__Result__int__string_Some)._0
        switch x190.(type) {
        case Ok:
            var x191 int = x190.(Ok)._0
            var t241 string
            var inline366 string = _goml_runtime_core_int_to_string(x191)
            t241 = inline366
            var t242 string = "ok:" + t241
            return t242
        case Err:
            var x192 string = x190.(Err)._0
            var t243 string = "err:" + x192
            return t243
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func take_once(value__10 Option__int) int {
    var current__11 *ref_Option__int_x
    var inline379 *ref_Option__int_x = ref__Ref_11Option__int(value__10)
    current__11 = inline379
    var result__12 *ref_int_x
    var inline376 int = 0
    var inline377 *ref_int_x = ref__Ref_3int(inline376)
    result__12 = inline377
    Loop_loop256:
    for {
        var mtmp197 Option__int
        var inline372 Option__int = ref_get__Ref_11Option__int(current__11)
        mtmp197 = inline372
        switch mtmp197.(type) {
        case Option__int_Some:
            var x198 int = mtmp197.(Option__int_Some)._0
            ref_set__Ref_3int(result__12, x198)
            ref_set__Ref_11Option__int(current__11, Option__int_None{})
            continue
        default:
            break Loop_loop256
        }
    }
    var inline374 int = ref_get__Ref_3int(result__12)
    return inline374
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var result__17 *ref_int_x
    var inline387 int = 0
    var inline388 *ref_int_x = ref__Ref_3int(inline387)
    result__17 = inline388
    var for_limit205 int = vec_len__Vec_10Boxed__int(values__16)
    var for_index206 int = 0
    Loop_loop266:
    for {
        var t267 bool = for_index206 < for_limit205
        if t267 {
            var for_item207 Boxed__int = vec_get__Vec_10Boxed__int(values__16, for_index206)
            var t268 int = for_index206 + 1
            for_index206 = t268
            switch for_item207.(type) {
            case Value:
                var x209 int = for_item207.(Value)._0
                var t270 int
                var inline383 int = ref_get__Ref_3int(result__17)
                t270 = inline383
                var t271 int = t270 + x209
                ref_set__Ref_3int(result__17, t271)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop266
        }
    }
    var inline385 int = ref_get__Ref_3int(result__17)
    return inline385
}

func main0() struct{} {
    var vec_literal__1450 *_goml_vec_Boxed__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_()
    var t273 Boxed__int = Value{
        _0: 19,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t273)
    var t274 Boxed__int = Value{
        _0: 23,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t274)
    var t275 First__int = First__int_Shared{
        _0: 7,
    }
    var t276 string = classify(t275)
    println__T_string(t276)
    var t277 string = classify(Idle{})
    println__T_string(t277)
    var t278 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t279 string = classify(t278)
    println__T_string(t279)
    var t280 Result__int__string = Ok{
        _0: 11,
    }
    var t281 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t280,
    }
    var t282 string = nested(t281)
    println__T_string(t282)
    var t283 Result__int__string = Err{
        _0: "bad",
    }
    var t284 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t283,
    }
    var t285 string = nested(t284)
    println__T_string(t285)
    var t286 string = nested(Option__Result__int__string_None{})
    var inline423 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t286)
    _goml_runtime_core_string_println(inline423)
    var t288 int
    var inline420 int = 13
    t288 = inline420
    var inline417 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t288)
    _goml_runtime_core_string_println(inline417)
    var t289 int
    t289 = 0
    var inline411 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t289)
    _goml_runtime_core_string_println(inline411)
    var t290 bool
    t290 = true
    var inline407 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t290)
    _goml_runtime_core_string_println(inline407)
    var t292 bool
    t292 = false
    var inline403 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t292)
    _goml_runtime_core_string_println(inline403)
    var t293 Option__int = Option__int_Some{
        _0: 15,
    }
    var t294 int = take_once(t293)
    var inline400 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t294)
    _goml_runtime_core_string_println(inline400)
    var t296 int
    var inline397 int = 17
    t296 = inline397
    var inline393 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t296)
    _goml_runtime_core_string_println(inline393)
    var t297 int = sum_boxed(vec_literal__1450)
    var inline390 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t297)
    _goml_runtime_core_string_println(inline390)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_() *_goml_vec_Boxed__int {
    var t320 *_goml_vec_Boxed__int = vec_new__Vec_10Boxed__int()
    return t320
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(self__174 *_goml_vec_Boxed__int, elem__175 Boxed__int) struct{} {
    vec_push__Vec_10Boxed__int(self__174, elem__175)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t324 string
    t324 = value__1
    _goml_runtime_core_string_println(t324)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t336 string = _goml_runtime_core_int_to_string(self__67)
    return t336
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t339 string = _goml_runtime_core_bool_to_string(self__64)
    return t339
}

func main() {
    main0()
}
