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
        var x172 int = value__0.(First__int_Shared)._0
        var t215 string
        var inline347 string = _goml_runtime_core_int_to_string(x172)
        t215 = inline347
        var t216 string = "shared:" + t215
        return t216
    case Idle:
        return "idle"
    case Data:
        var x173 int = value__0.(Data)._0
        var x174 string = value__0.(Data)._1
        var t217 string = x174 + ":"
        var t218 string
        var inline349 string = _goml_runtime_core_int_to_string(x173)
        t218 = inline349
        var t219 string = t217 + t218
        return t219
    default:
        panic("non-exhaustive match")
    }
}

func nested(value__4 Option__Result__int__string) string {
    switch value__4.(type) {
    case Option__Result__int__string_None:
        return "none"
    case Option__Result__int__string_Some:
        var x175 Result__int__string = value__4.(Option__Result__int__string_Some)._0
        switch x175.(type) {
        case Ok:
            var x176 int = x175.(Ok)._0
            var t226 string
            var inline351 string = _goml_runtime_core_int_to_string(x176)
            t226 = inline351
            var t227 string = "ok:" + t226
            return t227
        case Err:
            var x177 string = x175.(Err)._0
            var t228 string = "err:" + x177
            return t228
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func take_once(value__10 Option__int) int {
    var current__11 *ref_Option__int_x
    var inline364 *ref_Option__int_x = ref__Ref_11Option__int(value__10)
    current__11 = inline364
    var result__12 *ref_int_x
    var inline361 int = 0
    var inline362 *ref_int_x = ref__Ref_3int(inline361)
    result__12 = inline362
    Loop_loop241:
    for {
        var mtmp182 Option__int
        var inline357 Option__int = ref_get__Ref_11Option__int(current__11)
        mtmp182 = inline357
        switch mtmp182.(type) {
        case Option__int_Some:
            var x183 int = mtmp182.(Option__int_Some)._0
            ref_set__Ref_3int(result__12, x183)
            ref_set__Ref_11Option__int(current__11, Option__int_None{})
            continue
        default:
            break Loop_loop241
        }
    }
    var inline359 int = ref_get__Ref_3int(result__12)
    return inline359
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var result__17 *ref_int_x
    var inline372 int = 0
    var inline373 *ref_int_x = ref__Ref_3int(inline372)
    result__17 = inline373
    var for_limit190 int = vec_len__Vec_10Boxed__int(values__16)
    var for_index191 int = 0
    Loop_loop251:
    for {
        var t252 bool = for_index191 < for_limit190
        if t252 {
            var for_item192 Boxed__int = vec_get__Vec_10Boxed__int(values__16, for_index191)
            var t253 int = for_index191 + 1
            for_index191 = t253
            switch for_item192.(type) {
            case Value:
                var x194 int = for_item192.(Value)._0
                var t255 int
                var inline368 int = ref_get__Ref_3int(result__17)
                t255 = inline368
                var t256 int = t255 + x194
                ref_set__Ref_3int(result__17, t256)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop251
        }
    }
    var inline370 int = ref_get__Ref_3int(result__17)
    return inline370
}

func main0() struct{} {
    var vec_literal__1450 *_goml_vec_Boxed__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_()
    var t258 Boxed__int = Value{
        _0: 19,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t258)
    var t259 Boxed__int = Value{
        _0: 23,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t259)
    var t260 First__int = First__int_Shared{
        _0: 7,
    }
    var t261 string = classify(t260)
    println__T_string(t261)
    var t262 string = classify(Idle{})
    println__T_string(t262)
    var t263 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t264 string = classify(t263)
    println__T_string(t264)
    var t265 Result__int__string = Ok{
        _0: 11,
    }
    var t266 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t265,
    }
    var t267 string = nested(t266)
    println__T_string(t267)
    var t268 Result__int__string = Err{
        _0: "bad",
    }
    var t269 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t268,
    }
    var t270 string = nested(t269)
    println__T_string(t270)
    var t271 string = nested(Option__Result__int__string_None{})
    var inline408 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t271)
    _goml_runtime_core_string_println(inline408)
    var t273 int
    var inline405 int = 13
    t273 = inline405
    var inline402 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t273)
    _goml_runtime_core_string_println(inline402)
    var t274 int
    t274 = 0
    var inline396 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t274)
    _goml_runtime_core_string_println(inline396)
    var t275 bool
    t275 = true
    var inline392 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t275)
    _goml_runtime_core_string_println(inline392)
    var t277 bool
    t277 = false
    var inline388 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t277)
    _goml_runtime_core_string_println(inline388)
    var t278 Option__int = Option__int_Some{
        _0: 15,
    }
    var t279 int = take_once(t278)
    var inline385 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t279)
    _goml_runtime_core_string_println(inline385)
    var t281 int
    var inline382 int = 17
    t281 = inline382
    var inline378 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t281)
    _goml_runtime_core_string_println(inline378)
    var t282 int = sum_boxed(vec_literal__1450)
    var inline375 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t282)
    _goml_runtime_core_string_println(inline375)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_() *_goml_vec_Boxed__int {
    var t305 *_goml_vec_Boxed__int = vec_new__Vec_10Boxed__int()
    return t305
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(self__174 *_goml_vec_Boxed__int, elem__175 Boxed__int) struct{} {
    vec_push__Vec_10Boxed__int(self__174, elem__175)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t309 string
    t309 = value__1
    _goml_runtime_core_string_println(t309)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t321 string = _goml_runtime_core_int_to_string(self__67)
    return t321
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t324 string = _goml_runtime_core_bool_to_string(self__64)
    return t324
}

func main() {
    main0()
}
