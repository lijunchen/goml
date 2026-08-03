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
        var x177 int = value__0.(First__int_Shared)._0
        var t220 string
        var inline337 string = _goml_runtime_core_int_to_string(x177)
        t220 = inline337
        var t221 string = "shared:" + t220
        return t221
    case Idle:
        return "idle"
    case Data:
        var x178 int = value__0.(Data)._0
        var x179 string = value__0.(Data)._1
        var t222 string = x179 + ":"
        var t223 string
        var inline339 string = _goml_runtime_core_int_to_string(x178)
        t223 = inline339
        var t224 string = t222 + t223
        return t224
    default:
        panic("non-exhaustive match")
    }
}

func nested(value__4 Option__Result__int__string) string {
    switch value__4.(type) {
    case Option__Result__int__string_None:
        return "none"
    case Option__Result__int__string_Some:
        var x180 Result__int__string = value__4.(Option__Result__int__string_Some)._0
        switch x180.(type) {
        case Ok:
            var x181 int = x180.(Ok)._0
            var t231 string
            var inline341 string = _goml_runtime_core_int_to_string(x181)
            t231 = inline341
            var t232 string = "ok:" + t231
            return t232
        case Err:
            var x182 string = x180.(Err)._0
            var t233 string = "err:" + x182
            return t233
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
        var x183 int = value__7.(Option__int_Some)._0
        return x183
    default:
        return 0
    }
}

func take_once(value__10 Option__int) int {
    var current__11 *ref_Option__int_x
    var inline354 *ref_Option__int_x = ref__Ref_11Option__int(value__10)
    current__11 = inline354
    var result__12 *ref_int_x
    var inline351 int = 0
    var inline352 *ref_int_x = ref__Ref_3int(inline351)
    result__12 = inline352
    Loop_loop246:
    for {
        var mtmp187 Option__int
        var inline347 Option__int = ref_get__Ref_11Option__int(current__11)
        mtmp187 = inline347
        switch mtmp187.(type) {
        case Option__int_Some:
            var x188 int = mtmp187.(Option__int_Some)._0
            ref_set__Ref_3int(result__12, x188)
            ref_set__Ref_11Option__int(current__11, Option__int_None{})
            continue
        default:
            break Loop_loop246
        }
    }
    var inline349 int = ref_get__Ref_3int(result__12)
    return inline349
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var result__17 *ref_int_x
    var inline362 int = 0
    var inline363 *ref_int_x = ref__Ref_3int(inline362)
    result__17 = inline363
    var for_limit195 int = vec_len__Vec_10Boxed__int(values__16)
    var for_index196 int = 0
    Loop_loop256:
    for {
        var t257 bool = for_index196 < for_limit195
        if t257 {
            var for_item197 Boxed__int = vec_get__Vec_10Boxed__int(values__16, for_index196)
            var t258 int = for_index196 + 1
            for_index196 = t258
            switch for_item197.(type) {
            case Value:
                var x199 int = for_item197.(Value)._0
                var t260 int
                var inline358 int = ref_get__Ref_3int(result__17)
                t260 = inline358
                var t261 int = t260 + x199
                ref_set__Ref_3int(result__17, t261)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop256
        }
    }
    var inline360 int = ref_get__Ref_3int(result__17)
    return inline360
}

func main0() struct{} {
    var vec_literal__1450 *_goml_vec_Boxed__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_()
    var t263 Boxed__int = Value{
        _0: 19,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t263)
    var t264 Boxed__int = Value{
        _0: 23,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t264)
    var t265 First__int = First__int_Shared{
        _0: 7,
    }
    var t266 string = classify(t265)
    println__T_string(t266)
    var t267 string = classify(Idle{})
    println__T_string(t267)
    var t268 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t269 string = classify(t268)
    println__T_string(t269)
    var t270 Result__int__string = Ok{
        _0: 11,
    }
    var t271 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t270,
    }
    var t272 string = nested(t271)
    println__T_string(t272)
    var t273 Result__int__string = Err{
        _0: "bad",
    }
    var t274 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t273,
    }
    var t275 string = nested(t274)
    println__T_string(t275)
    var t276 string = nested(Option__Result__int__string_None{})
    var inline401 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t276)
    _goml_runtime_core_string_println(inline401)
    var t277 Option__int = Option__int_Some{
        _0: 13,
    }
    var t278 int = optional_number(t277)
    var inline398 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t278)
    _goml_runtime_core_string_println(inline398)
    var t279 int
    t279 = 0
    var inline392 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t279)
    _goml_runtime_core_string_println(inline392)
    var t280 bool
    t280 = true
    var inline385 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t280)
    _goml_runtime_core_string_println(inline385)
    var t282 bool
    t282 = false
    var inline378 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t282)
    _goml_runtime_core_string_println(inline378)
    var t283 Option__int = Option__int_Some{
        _0: 15,
    }
    var t284 int = take_once(t283)
    var inline375 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t284)
    _goml_runtime_core_string_println(inline375)
    var t286 int
    var inline372 int = 17
    t286 = inline372
    var inline368 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t286)
    _goml_runtime_core_string_println(inline368)
    var t287 int = sum_boxed(vec_literal__1450)
    var inline365 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t287)
    _goml_runtime_core_string_println(inline365)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_() *_goml_vec_Boxed__int {
    var t310 *_goml_vec_Boxed__int = vec_new__Vec_10Boxed__int()
    return t310
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(self__155 *_goml_vec_Boxed__int, elem__156 Boxed__int) struct{} {
    vec_push__Vec_10Boxed__int(self__155, elem__156)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t314 string
    t314 = value__31
    _goml_runtime_core_string_println(t314)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t326 string = _goml_runtime_core_int_to_string(self__69)
    return t326
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t329 string = _goml_runtime_core_bool_to_string(self__66)
    return t329
}

func main() {
    main0()
}
