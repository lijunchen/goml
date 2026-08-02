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
    var retv195 string
    var jp197 string
    switch value__0.(type) {
    case First__int_Shared:
        var x155 int = value__0.(First__int_Shared)._0
        var number__1 int = x155
        var t198 string = _goml_m_inherent_i_int_i_int_i_to__string(number__1)
        var t199 string = "shared:" + t198
        jp197 = t199
    case Idle:
        jp197 = "idle"
    case Data:
        var x156 int = value__0.(Data)._0
        var x157 string = value__0.(Data)._1
        var label__3 string = x157
        var value__2 int = x156
        var t200 string = label__3 + ":"
        var t201 string = _goml_m_inherent_i_int_i_int_i_to__string(value__2)
        var t202 string = t200 + t201
        jp197 = t202
    default:
        panic("non-exhaustive match")
    }
    retv195 = jp197
    return retv195
}

func nested(value__4 Option__Result__int__string) string {
    var retv204 string
    var jp206 string
    switch value__4.(type) {
    case Option__Result__int__string_None:
        jp206 = "none"
    case Option__Result__int__string_Some:
        var x158 Result__int__string = value__4.(Option__Result__int__string_Some)._0
        var jp208 string
        switch x158.(type) {
        case Ok:
            var x159 int = x158.(Ok)._0
            var number__5 int = x159
            var t209 string = _goml_m_inherent_i_int_i_int_i_to__string(number__5)
            var t210 string = "ok:" + t209
            jp208 = t210
        case Err:
            var x160 string = x158.(Err)._0
            var message__6 string = x160
            var t211 string = "err:" + message__6
            jp208 = t211
        default:
            panic("non-exhaustive match")
        }
        jp206 = jp208
    default:
        panic("non-exhaustive match")
    }
    retv204 = jp206
    return retv204
}

func optional_number(value__7 Option__int) int {
    var retv213 int
    var jp215 int
    switch value__7.(type) {
    case Option__int_Some:
        var x161 int = value__7.(Option__int_Some)._0
        var number__8 int = x161
        jp215 = number__8
    default:
        jp215 = 0
    }
    retv213 = jp215
    return retv213
}

func is_non_shared(value__9 First__int) bool {
    var retv217 bool
    var jp219 bool
    switch value__9.(type) {
    case First__int_Shared:
        jp219 = false
    case Idle:
        jp219 = true
    case Data:
        jp219 = true
    default:
        panic("non-exhaustive match")
    }
    retv217 = jp219
    return retv217
}

func take_once(value__10 Option__int) int {
    var retv221 int
    var current__11 *ref_Option__int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__10)
    var result__12 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    Loop_loop224:
    for {
        if true {
            var mtmp165 Option__int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_int_r_(current__11)
            switch mtmp165.(type) {
            case Option__int_Some:
                var x166 int = mtmp165.(Option__int_Some)._0
                var number__13 int = x166
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__12, number__13)
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__Option_l_int_r_(current__11, Option__int_None{})
                continue
            default:
                break Loop_loop224
            }
        } else {
            break Loop_loop224
        }
    }
    var t223 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__12)
    retv221 = t223
    return retv221
}

func unbox(value__14 Boxed__int) int {
    var retv227 int
    var mtmp170 Boxed__int = value__14
    var jp229 int
    switch mtmp170.(type) {
    case Value:
        var x171 int = mtmp170.(Value)._0
        var number__15 int = x171
        jp229 = number__15
    default:
        panic("non-exhaustive match")
    }
    retv227 = jp229
    return retv227
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var retv231 int
    var result__17 *ref_int_x = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(0)
    var for_source172 *_goml_vec_Boxed__int = values__16
    var for_limit173 int = vec_len__Vec_10Boxed__int(for_source172)
    var for_index174 int = 0
    Loop_loop234:
    for {
        var t235 bool = for_index174 < for_limit173
        if t235 {
            var for_item175 Boxed__int = vec_get__Vec_10Boxed__int(for_source172, for_index174)
            var t236 int = for_index174 + 1
            for_index174 = t236
            switch for_item175.(type) {
            case Value:
                var x177 int = for_item175.(Value)._0
                var number__18 int = x177
                var t238 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__17)
                var t239 int = t238 + number__18
                _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_set____T__int(result__17, t239)
            default:
                panic("non-exhaustive match")
            }
            continue
        } else {
            break Loop_loop234
        }
    }
    var t233 int = _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__int(result__17)
    retv231 = t233
    return retv231
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
    var boxed__19 *_goml_vec_Boxed__int = vec_literal__1450
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
    println__T_string(t254)
    var t255 Option__int = Option__int_Some{
        _0: 13,
    }
    var t256 int = optional_number(t255)
    println__T_int(t256)
    var t257 int = optional_number(Option__int_None{})
    println__T_int(t257)
    var t258 bool = is_non_shared(Idle{})
    println__T_bool(t258)
    var t259 First__int = First__int_Shared{
        _0: 1,
    }
    var t260 bool = is_non_shared(t259)
    println__T_bool(t260)
    var t261 Option__int = Option__int_Some{
        _0: 15,
    }
    var t262 int = take_once(t261)
    println__T_int(t262)
    var t263 Boxed__int = Value{
        _0: 17,
    }
    var t264 int = unbox(t263)
    println__T_int(t264)
    var t265 int = sum_boxed(boxed__19)
    println__T_int(t265)
    return struct{}{}
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var retv268 string
    var t269 string = _goml_runtime_core_int_to_string(self__5)
    retv268 = t269
    return retv268
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__Option_l_int_r_(value__207 Option__int) *ref_Option__int_x {
    var retv271 *ref_Option__int_x
    var t272 *ref_Option__int_x = ref__Ref_11Option__int(value__207)
    retv271 = t272
    return retv271
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_new____T__int(value__207 int) *ref_int_x {
    var retv274 *ref_int_x
    var t275 *ref_int_x = ref__Ref_3int(value__207)
    retv274 = t275
    return retv274
}

func _goml_m_inherent_i_Ref_i_Ref_l_T_r__i_get____T__Option_l_int_r_(self__208 *ref_Option__int_x) Option__int {
    var retv277 Option__int
    var t278 Option__int = ref_get__Ref_11Option__int(self__208)
    retv277 = t278
    return retv277
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
    var retv284 int
    var t285 int = ref_get__Ref_3int(self__208)
    retv284 = t285
    return retv284
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_() *_goml_vec_Boxed__int {
    var retv287 *_goml_vec_Boxed__int
    var t288 *_goml_vec_Boxed__int = vec_new__Vec_10Boxed__int()
    retv287 = t288
    return retv287
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(self__126 *_goml_vec_Boxed__int, elem__127 Boxed__int) struct{} {
    vec_push__Vec_10Boxed__int(self__126, elem__127)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t292 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t292)
    return struct{}{}
}

func println__T_int(value__1 int) struct{} {
    var t295 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(value__1)
    _goml_runtime_core_string_println(t295)
    return struct{}{}
}

func println__T_bool(value__1 bool) struct{} {
    var t298 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(value__1)
    _goml_runtime_core_string_println(t298)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv301 string
    retv301 = self__38
    return retv301
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var retv303 string
    var t304 string = _goml_runtime_core_int_to_string(self__40)
    retv303 = t304
    return retv303
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__37 bool) string {
    var retv306 string
    var t307 string = _goml_runtime_core_bool_to_string(self__37)
    retv306 = t307
    return retv306
}

func main() {
    main0()
}
