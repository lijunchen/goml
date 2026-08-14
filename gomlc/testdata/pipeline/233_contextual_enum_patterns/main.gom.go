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
        var x182 int = value__0.(First__int_Shared)._0
        var t225 string
        var inline357 string = _goml_runtime_core_int_to_string(x182)
        t225 = inline357
        var t226 string = "shared:" + t225
        return t226
    case Idle:
        return "idle"
    case Data:
        var x183 int = value__0.(Data)._0
        var x184 string = value__0.(Data)._1
        var t227 string = x184 + ":"
        var t228 string
        var inline359 string = _goml_runtime_core_int_to_string(x183)
        t228 = inline359
        var t229 string = t227 + t228
        return t229
    default:
        panic("non-exhaustive match")
    }
}

func nested(value__4 Option__Result__int__string) string {
    switch value__4.(type) {
    case Option__Result__int__string_None:
        return "none"
    case Option__Result__int__string_Some:
        var x185 Result__int__string = value__4.(Option__Result__int__string_Some)._0
        switch x185.(type) {
        case Ok:
            var x186 int = x185.(Ok)._0
            var t236 string
            var inline361 string = _goml_runtime_core_int_to_string(x186)
            t236 = inline361
            var t237 string = "ok:" + t236
            return t237
        case Err:
            var x187 string = x185.(Err)._0
            var t238 string = "err:" + x187
            return t238
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func take_once(value__10 Option__int) int {
    var current__11 *ref_Option__int_x
    var inline374 *ref_Option__int_x = ref__Ref_11Option__int(value__10)
    current__11 = inline374
    var result__12 *ref_int_x
    var inline371 int = 0
    var inline372 *ref_int_x = ref__Ref_3int(inline371)
    result__12 = inline372
    Loop_loop251:
    for {
        var mtmp192 Option__int
        var inline367 Option__int = ref_get__Ref_11Option__int(current__11)
        mtmp192 = inline367
        switch mtmp192.(type) {
        case Option__int_Some:
            var x193 int = mtmp192.(Option__int_Some)._0
            ref_set__Ref_3int(result__12, x193)
            ref_set__Ref_11Option__int(current__11, Option__int_None{})
            continue
        default:
            break Loop_loop251
        }
    }
    var inline369 int = ref_get__Ref_3int(result__12)
    return inline369
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var result__17 *ref_int_x
    var inline382 int = 0
    var inline383 *ref_int_x = ref__Ref_3int(inline382)
    result__17 = inline383
    var for_limit200 int = vec_len__Vec_10Boxed__int(values__16)
    var for_index201 int = 0
    Loop_loop261:
    for {
        var t262 bool = for_index201 < for_limit200
        if t262 {
            var for_item202 Boxed__int = vec_get__Vec_10Boxed__int(values__16, for_index201)
            var t263 int = for_index201 + 1
            for_index201 = t263
            switch for_item202.(type) {
            case Value:
                var x204 int = for_item202.(Value)._0
                var t265 int
                var inline378 int = ref_get__Ref_3int(result__17)
                t265 = inline378
                var t266 int = t265 + x204
                ref_set__Ref_3int(result__17, t266)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop261
        }
    }
    var inline380 int = ref_get__Ref_3int(result__17)
    return inline380
}

func main0() struct{} {
    var vec_literal__1450 *_goml_vec_Boxed__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_()
    var t268 Boxed__int = Value{
        _0: 19,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t268)
    var t269 Boxed__int = Value{
        _0: 23,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t269)
    var t270 First__int = First__int_Shared{
        _0: 7,
    }
    var t271 string = classify(t270)
    println__T_string(t271)
    var t272 string = classify(Idle{})
    println__T_string(t272)
    var t273 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t274 string = classify(t273)
    println__T_string(t274)
    var t275 Result__int__string = Ok{
        _0: 11,
    }
    var t276 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t275,
    }
    var t277 string = nested(t276)
    println__T_string(t277)
    var t278 Result__int__string = Err{
        _0: "bad",
    }
    var t279 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t278,
    }
    var t280 string = nested(t279)
    println__T_string(t280)
    var t281 string = nested(Option__Result__int__string_None{})
    var inline418 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t281)
    _goml_runtime_core_string_println(inline418)
    var t283 int
    var inline415 int = 13
    t283 = inline415
    var inline412 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t283)
    _goml_runtime_core_string_println(inline412)
    var t284 int
    t284 = 0
    var inline406 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t284)
    _goml_runtime_core_string_println(inline406)
    var t285 bool
    t285 = true
    var inline402 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t285)
    _goml_runtime_core_string_println(inline402)
    var t287 bool
    t287 = false
    var inline398 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t287)
    _goml_runtime_core_string_println(inline398)
    var t288 Option__int = Option__int_Some{
        _0: 15,
    }
    var t289 int = take_once(t288)
    var inline395 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t289)
    _goml_runtime_core_string_println(inline395)
    var t291 int
    var inline392 int = 17
    t291 = inline392
    var inline388 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t291)
    _goml_runtime_core_string_println(inline388)
    var t292 int = sum_boxed(vec_literal__1450)
    var inline385 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t292)
    _goml_runtime_core_string_println(inline385)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_() *_goml_vec_Boxed__int {
    var t315 *_goml_vec_Boxed__int = vec_new__Vec_10Boxed__int()
    return t315
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(self__174 *_goml_vec_Boxed__int, elem__175 Boxed__int) struct{} {
    vec_push__Vec_10Boxed__int(self__174, elem__175)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t319 string
    t319 = value__1
    _goml_runtime_core_string_println(t319)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t331 string = _goml_runtime_core_int_to_string(self__67)
    return t331
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__64 bool) string {
    var t334 string = _goml_runtime_core_bool_to_string(self__64)
    return t334
}

func main() {
    main0()
}
