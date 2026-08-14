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

type Ordering int32

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
        var x408 int = value__0.(First__int_Shared)._0
        var t451 string
        var inline583 string = _goml_runtime_core_int_to_string(x408)
        t451 = inline583
        var t452 string = "shared:" + t451
        return t452
    case Idle:
        return "idle"
    case Data:
        var x409 int = value__0.(Data)._0
        var x410 string = value__0.(Data)._1
        var t453 string = x410 + ":"
        var t454 string
        var inline585 string = _goml_runtime_core_int_to_string(x409)
        t454 = inline585
        var t455 string = t453 + t454
        return t455
    default:
        panic("non-exhaustive match")
    }
}

func nested(value__4 Option__Result__int__string) string {
    switch value__4.(type) {
    case Option__Result__int__string_None:
        return "none"
    case Option__Result__int__string_Some:
        var x411 Result__int__string = value__4.(Option__Result__int__string_Some)._0
        switch x411.(type) {
        case Ok:
            var x412 int = x411.(Ok)._0
            var t462 string
            var inline587 string = _goml_runtime_core_int_to_string(x412)
            t462 = inline587
            var t463 string = "ok:" + t462
            return t463
        case Err:
            var x413 string = x411.(Err)._0
            var t464 string = "err:" + x413
            return t464
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func take_once(value__10 Option__int) int {
    var current__11 *ref_Option__int_x
    var inline600 *ref_Option__int_x = ref__Ref_11Option__int(value__10)
    current__11 = inline600
    var result__12 *ref_int_x
    var inline597 int = 0
    var inline598 *ref_int_x = ref__Ref_3int(inline597)
    result__12 = inline598
    Loop_loop477:
    for {
        var mtmp418 Option__int
        var inline593 Option__int = ref_get__Ref_11Option__int(current__11)
        mtmp418 = inline593
        switch mtmp418.(type) {
        case Option__int_Some:
            var x419 int = mtmp418.(Option__int_Some)._0
            ref_set__Ref_3int(result__12, x419)
            ref_set__Ref_11Option__int(current__11, Option__int_None{})
            continue
        default:
            break Loop_loop477
        }
    }
    var inline595 int = ref_get__Ref_3int(result__12)
    return inline595
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var result__17 *ref_int_x
    var inline608 int = 0
    var inline609 *ref_int_x = ref__Ref_3int(inline608)
    result__17 = inline609
    var for_limit426 int = vec_len__Vec_10Boxed__int(values__16)
    var for_index427 int = 0
    Loop_loop487:
    for {
        var t488 bool = for_index427 < for_limit426
        if t488 {
            var for_item428 Boxed__int = vec_get__Vec_10Boxed__int(values__16, for_index427)
            var t489 int = for_index427 + 1
            for_index427 = t489
            switch for_item428.(type) {
            case Value:
                var x430 int = for_item428.(Value)._0
                var t491 int
                var inline604 int = ref_get__Ref_3int(result__17)
                t491 = inline604
                var t492 int = t491 + x430
                ref_set__Ref_3int(result__17, t492)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop487
        }
    }
    var inline606 int = ref_get__Ref_3int(result__17)
    return inline606
}

func main0() struct{} {
    var vec_literal__1450 *_goml_vec_Boxed__int = _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_()
    var t494 Boxed__int = Value{
        _0: 19,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t494)
    var t495 Boxed__int = Value{
        _0: 23,
    }
    _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(vec_literal__1450, t495)
    var t496 First__int = First__int_Shared{
        _0: 7,
    }
    var t497 string = classify(t496)
    println__T_string(t497)
    var t498 string = classify(Idle{})
    println__T_string(t498)
    var t499 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t500 string = classify(t499)
    println__T_string(t500)
    var t501 Result__int__string = Ok{
        _0: 11,
    }
    var t502 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t501,
    }
    var t503 string = nested(t502)
    println__T_string(t503)
    var t504 Result__int__string = Err{
        _0: "bad",
    }
    var t505 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t504,
    }
    var t506 string = nested(t505)
    println__T_string(t506)
    var t507 string = nested(Option__Result__int__string_None{})
    var inline644 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t507)
    _goml_runtime_core_string_println(inline644)
    var t509 int
    var inline641 int = 13
    t509 = inline641
    var inline638 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t509)
    _goml_runtime_core_string_println(inline638)
    var t510 int
    t510 = 0
    var inline632 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t510)
    _goml_runtime_core_string_println(inline632)
    var t511 bool
    t511 = true
    var inline628 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t511)
    _goml_runtime_core_string_println(inline628)
    var t513 bool
    t513 = false
    var inline624 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t513)
    _goml_runtime_core_string_println(inline624)
    var t514 Option__int = Option__int_Some{
        _0: 15,
    }
    var t515 int = take_once(t514)
    var inline621 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t515)
    _goml_runtime_core_string_println(inline621)
    var t517 int
    var inline618 int = 17
    t517 = inline618
    var inline614 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t517)
    _goml_runtime_core_string_println(inline614)
    var t518 int = sum_boxed(vec_literal__1450)
    var inline611 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t518)
    _goml_runtime_core_string_println(inline611)
    return struct{}{}
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_new____T__Boxed_l_int_r_() *_goml_vec_Boxed__int {
    var t541 *_goml_vec_Boxed__int = vec_new__Vec_10Boxed__int()
    return t541
}

func _goml_m_inherent_i_Vec_i_Vec_l_T_r__i_push____T__Boxed_l_int_r_(self__258 *_goml_vec_Boxed__int, elem__259 Boxed__int) struct{} {
    vec_push__Vec_10Boxed__int(self__258, elem__259)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t545 string
    t545 = value__1
    _goml_runtime_core_string_println(t545)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t557 string = _goml_runtime_core_int_to_string(self__151)
    return t557
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t560 string = _goml_runtime_core_bool_to_string(self__148)
    return t560
}

func main() {
    main0()
}
