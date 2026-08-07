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
        var x136 int = value__0.(First__int_Shared)._0
        var t179 string
        var inline296 string = _goml_runtime_core_int_to_string(x136)
        t179 = inline296
        var t180 string = "shared:" + t179
        return t180
    case Idle:
        return "idle"
    case Data:
        var x137 int = value__0.(Data)._0
        var x138 string = value__0.(Data)._1
        var t181 string = x138 + ":"
        var t182 string
        var inline298 string = _goml_runtime_core_int_to_string(x137)
        t182 = inline298
        var t183 string = t181 + t182
        return t183
    default:
        panic("non-exhaustive match")
    }
}

func nested(value__4 Option__Result__int__string) string {
    switch value__4.(type) {
    case Option__Result__int__string_None:
        return "none"
    case Option__Result__int__string_Some:
        var x139 Result__int__string = value__4.(Option__Result__int__string_Some)._0
        switch x139.(type) {
        case Ok:
            var x140 int = x139.(Ok)._0
            var t190 string
            var inline300 string = _goml_runtime_core_int_to_string(x140)
            t190 = inline300
            var t191 string = "ok:" + t190
            return t191
        case Err:
            var x141 string = x139.(Err)._0
            var t192 string = "err:" + x141
            return t192
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
        var x142 int = value__7.(Option__int_Some)._0
        return x142
    default:
        return 0
    }
}

func take_once(value__10 Option__int) int {
    var current__11 *ref_Option__int_x
    var inline313 *ref_Option__int_x = ref__Ref_11Option__int(value__10)
    current__11 = inline313
    var result__12 *ref_int_x
    var inline310 int = 0
    var inline311 *ref_int_x = ref__Ref_3int(inline310)
    result__12 = inline311
    Loop_loop205:
    for {
        var mtmp146 Option__int
        var inline306 Option__int = ref_get__Ref_11Option__int(current__11)
        mtmp146 = inline306
        switch mtmp146.(type) {
        case Option__int_Some:
            var x147 int = mtmp146.(Option__int_Some)._0
            ref_set__Ref_3int(result__12, x147)
            ref_set__Ref_11Option__int(current__11, Option__int_None{})
            continue
        default:
            break Loop_loop205
        }
    }
    var inline308 int = ref_get__Ref_3int(result__12)
    return inline308
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var result__17 *ref_int_x
    var inline321 int = 0
    var inline322 *ref_int_x = ref__Ref_3int(inline321)
    result__17 = inline322
    var for_limit154 int = vec_len__Vec_10Boxed__int(values__16)
    var for_index155 int = 0
    Loop_loop215:
    for {
        var t216 bool = for_index155 < for_limit154
        if t216 {
            var for_item156 Boxed__int = vec_get__Vec_10Boxed__int(values__16, for_index155)
            var t217 int = for_index155 + 1
            for_index155 = t217
            switch for_item156.(type) {
            case Value:
                var x158 int = for_item156.(Value)._0
                var t219 int
                var inline317 int = ref_get__Ref_3int(result__17)
                t219 = inline317
                var t220 int = t219 + x158
                ref_set__Ref_3int(result__17, t220)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop215
        }
    }
    var inline319 int = ref_get__Ref_3int(result__17)
    return inline319
}

func main0() struct{} {
    var vec_literal__1450 *_goml_vec_Boxed__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_()
    var t222 Boxed__int = Value{
        _0: 19,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t222)
    var t223 Boxed__int = Value{
        _0: 23,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t223)
    var t224 First__int = First__int_Shared{
        _0: 7,
    }
    var t225 string = classify(t224)
    println__T_string(t225)
    var t226 string = classify(Idle{})
    println__T_string(t226)
    var t227 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t228 string = classify(t227)
    println__T_string(t228)
    var t229 Result__int__string = Ok{
        _0: 11,
    }
    var t230 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t229,
    }
    var t231 string = nested(t230)
    println__T_string(t231)
    var t232 Result__int__string = Err{
        _0: "bad",
    }
    var t233 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t232,
    }
    var t234 string = nested(t233)
    println__T_string(t234)
    var t235 string = nested(Option__Result__int__string_None{})
    var inline360 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t235)
    _goml_runtime_core_string_println(inline360)
    var t236 Option__int = Option__int_Some{
        _0: 13,
    }
    var t237 int = optional_number(t236)
    var inline357 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t237)
    _goml_runtime_core_string_println(inline357)
    var t238 int
    t238 = 0
    var inline351 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t238)
    _goml_runtime_core_string_println(inline351)
    var t239 bool
    t239 = true
    var inline344 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t239)
    _goml_runtime_core_string_println(inline344)
    var t241 bool
    t241 = false
    var inline337 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t241)
    _goml_runtime_core_string_println(inline337)
    var t242 Option__int = Option__int_Some{
        _0: 15,
    }
    var t243 int = take_once(t242)
    var inline334 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t243)
    _goml_runtime_core_string_println(inline334)
    var t245 int
    var inline331 int = 17
    t245 = inline331
    var inline327 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t245)
    _goml_runtime_core_string_println(inline327)
    var t246 int = sum_boxed(vec_literal__1450)
    var inline324 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t246)
    _goml_runtime_core_string_println(inline324)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_() *_goml_vec_Boxed__int {
    var t269 *_goml_vec_Boxed__int = vec_new__Vec_10Boxed__int()
    return t269
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(self__151 *_goml_vec_Boxed__int, elem__152 Boxed__int) struct{} {
    vec_push__Vec_10Boxed__int(self__151, elem__152)
    return struct{}{}
}

func println__T_string(value__31 string) struct{} {
    var t273 string
    t273 = value__31
    _goml_runtime_core_string_println(t273)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t285 string = _goml_runtime_core_int_to_string(self__69)
    return t285
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t288 string = _goml_runtime_core_bool_to_string(self__66)
    return t288
}

func main() {
    main0()
}
