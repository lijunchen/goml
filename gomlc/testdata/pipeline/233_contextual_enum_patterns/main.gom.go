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
    var retv192 string
    var jp194 string
    switch value__0.(type) {
    case First__int_Shared:
        var x152 int = value__0.(First__int_Shared)._0
        var number__1 int = x152
        var t195 string = _goml_m_inherent_i_int_i_int_i_to__string(number__1)
        var t196 string = "shared:" + t195
        jp194 = t196
    case Idle:
        jp194 = "idle"
    case Data:
        var x153 int = value__0.(Data)._0
        var x154 string = value__0.(Data)._1
        var label__3 string = x154
        var value__2 int = x153
        var t197 string = label__3 + ":"
        var t198 string = _goml_m_inherent_i_int_i_int_i_to__string(value__2)
        var t199 string = t197 + t198
        jp194 = t199
    default:
        panic("non-exhaustive match")
    }
    retv192 = jp194
    return retv192
}

func nested(value__4 Option__Result__int__string) string {
    var retv201 string
    var jp203 string
    switch value__4.(type) {
    case Option__Result__int__string_None:
        jp203 = "none"
    case Option__Result__int__string_Some:
        var x155 Result__int__string = value__4.(Option__Result__int__string_Some)._0
        var jp205 string
        switch x155.(type) {
        case Ok:
            var x156 int = x155.(Ok)._0
            var number__5 int = x156
            var t206 string = _goml_m_inherent_i_int_i_int_i_to__string(number__5)
            var t207 string = "ok:" + t206
            jp205 = t207
        case Err:
            var x157 string = x155.(Err)._0
            var message__6 string = x157
            var t208 string = "err:" + message__6
            jp205 = t208
        default:
            panic("non-exhaustive match")
        }
        jp203 = jp205
    default:
        panic("non-exhaustive match")
    }
    retv201 = jp203
    return retv201
}

func optional_number(value__7 Option__int) int {
    var retv210 int
    var jp212 int
    switch value__7.(type) {
    case Option__int_Some:
        var x158 int = value__7.(Option__int_Some)._0
        var number__8 int = x158
        jp212 = number__8
    default:
        jp212 = 0
    }
    retv210 = jp212
    return retv210
}

func is_non_shared(value__9 First__int) bool {
    var retv214 bool
    var jp216 bool
    switch value__9.(type) {
    case First__int_Shared:
        jp216 = false
    case Idle:
        jp216 = true
    case Data:
        jp216 = true
    default:
        panic("non-exhaustive match")
    }
    retv214 = jp216
    return retv214
}

func take_once(value__10 Option__int) int {
    var retv218 int
    var current__11 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__10)
    var result__12 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop221:
    for {
        if true {
            var mtmp162 Option__int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_int_r_(current__11)
            switch mtmp162.(type) {
            case Option__int_Some:
                var x163 int = mtmp162.(Option__int_Some)._0
                var number__13 int = x163
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__12, number__13)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_int_r_(current__11, Option__int_None{})
                continue
            default:
                break Loop_loop221
            }
        } else {
            break Loop_loop221
        }
    }
    var t220 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__12)
    retv218 = t220
    return retv218
}

func unbox(value__14 Boxed__int) int {
    var retv224 int
    var mtmp167 Boxed__int = value__14
    var jp226 int
    switch mtmp167.(type) {
    case Value:
        var x168 int = mtmp167.(Value)._0
        var number__15 int = x168
        jp226 = number__15
    default:
        panic("non-exhaustive match")
    }
    retv224 = jp226
    return retv224
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var retv228 int
    var result__17 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var for_source169 *_goml_vec_Boxed__int = values__16
    var for_limit170 int = vec_len__Vec_10Boxed__int(for_source169)
    var for_index171 int = 0
    Loop_loop231:
    for {
        var t232 bool = for_index171 < for_limit170
        if t232 {
            var for_item172 Boxed__int = vec_get__Vec_10Boxed__int(for_source169, for_index171)
            var t233 int = for_index171 + 1
            for_index171 = t233
            switch for_item172.(type) {
            case Value:
                var x174 int = for_item172.(Value)._0
                var number__18 int = x174
                var t235 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__17)
                var t236 int = t235 + number__18
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__17, t236)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop231
        }
    }
    var t230 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__17)
    retv228 = t230
    return retv228
}

func main0() struct{} {
    var vec_literal__1450 *_goml_vec_Boxed__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_()
    var t238 Boxed__int = Value{
        _0: 19,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t238)
    var t239 Boxed__int = Value{
        _0: 23,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t239)
    var boxed__19 *_goml_vec_Boxed__int = vec_literal__1450
    var t240 First__int = First__int_Shared{
        _0: 7,
    }
    var t241 string = classify(t240)
    println__T_string(t241)
    var t242 string = classify(Idle{})
    println__T_string(t242)
    var t243 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t244 string = classify(t243)
    println__T_string(t244)
    var t245 Result__int__string = Ok{
        _0: 11,
    }
    var t246 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t245,
    }
    var t247 string = nested(t246)
    println__T_string(t247)
    var t248 Result__int__string = Err{
        _0: "bad",
    }
    var t249 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t248,
    }
    var t250 string = nested(t249)
    println__T_string(t250)
    var t251 string = nested(Option__Result__int__string_None{})
    println__T_string(t251)
    var t252 Option__int = Option__int_Some{
        _0: 13,
    }
    var t253 int = optional_number(t252)
    println__T_int(t253)
    var t254 int = optional_number(Option__int_None{})
    println__T_int(t254)
    var t255 bool = is_non_shared(Idle{})
    println__T_bool(t255)
    var t256 First__int = First__int_Shared{
        _0: 1,
    }
    var t257 bool = is_non_shared(t256)
    println__T_bool(t257)
    var t258 Option__int = Option__int_Some{
        _0: 15,
    }
    var t259 int = take_once(t258)
    println__T_int(t259)
    var t260 Boxed__int = Value{
        _0: 17,
    }
    var t261 int = unbox(t260)
    println__T_int(t261)
    var t262 int = sum_boxed(boxed__19)
    println__T_int(t262)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv265 string
    var t266 string = _goml_runtime_core_int_to_string(self__5)
    retv265 = t266
    return retv265
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__207 Option__int) *ref_Option__int_x {
    var retv268 *ref_Option__int_x
    var t269 *ref_Option__int_x = ref__Ref_11Option__int(value__207)
    retv268 = t269
    return retv268
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv271 *ref_int_x
    var t272 *ref_int_x = ref__Ref_3int(value__207)
    retv271 = t272
    return retv271
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_int_r_(self__208 *ref_Option__int_x) Option__int {
    var retv274 Option__int
    var t275 Option__int = ref_get__Ref_11Option__int(self__208)
    retv274 = t275
    return retv274
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
    var retv281 int
    var t282 int = ref_get__Ref_3int(self__208)
    retv281 = t282
    return retv281
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_() *_goml_vec_Boxed__int {
    var retv284 *_goml_vec_Boxed__int
    var t285 *_goml_vec_Boxed__int = vec_new__Vec_10Boxed__int()
    retv284 = t285
    return retv284
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(self__126 *_goml_vec_Boxed__int, elem__127 Boxed__int) struct{} {
    vec_push__Vec_10Boxed__int(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t289 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t289)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t292 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t292)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t295 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t295)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv298 string
    retv298 = self__38
    return retv298
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv300 string
    var t301 string = _goml_runtime_core_int_to_string(self__40)
    retv300 = t301
    return retv300
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv303 string
    var t304 string = _goml_runtime_core_bool_to_string(self__37)
    retv303 = t304
    return retv303
}

func main() {
    main0()
}
