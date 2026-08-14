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
        var t449 string
        var inline577 string = _goml_runtime_core_int_to_string(x408)
        t449 = inline577
        var t450 string = "shared:" + t449
        return t450
    case Idle:
        return "idle"
    case Data:
        var x409 int = value__0.(Data)._0
        var x410 string = value__0.(Data)._1
        var t451 string = x410 + ":"
        var t452 string
        var inline579 string = _goml_runtime_core_int_to_string(x409)
        t452 = inline579
        var t453 string = t451 + t452
        return t453
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
            var t460 string
            var inline581 string = _goml_runtime_core_int_to_string(x412)
            t460 = inline581
            var t461 string = "ok:" + t460
            return t461
        case Err:
            var x413 string = x411.(Err)._0
            var t462 string = "err:" + x413
            return t462
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func take_once(value__10 Option__int) int {
    var current__11 *ref_Option__int_x
    var inline594 *ref_Option__int_x = ref__Ref_11Option__int(value__10)
    current__11 = inline594
    var result__12 *ref_int_x
    var inline591 int = 0
    var inline592 *ref_int_x = ref__Ref_3int(inline591)
    result__12 = inline592
    Loop_loop475:
    for {
        var mtmp418 Option__int
        var inline587 Option__int = ref_get__Ref_11Option__int(current__11)
        mtmp418 = inline587
        switch mtmp418.(type) {
        case Option__int_Some:
            var x419 int = mtmp418.(Option__int_Some)._0
            ref_set__Ref_3int(result__12, x419)
            ref_set__Ref_11Option__int(current__11, Option__int_None{})
            continue
        default:
            break Loop_loop475
        }
    }
    var inline589 int = ref_get__Ref_3int(result__12)
    return inline589
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var result__17 *ref_int_x
    var inline602 int = 0
    var inline603 *ref_int_x = ref__Ref_3int(inline602)
    result__17 = inline603
    var for_limit426 int = vec_len__Vec_10Boxed__int(values__16)
    var for_index427 int = 0
    Loop_loop485:
    for {
        var t486 bool = for_index427 < for_limit426
        if t486 {
            var for_item428 Boxed__int = vec_get__Vec_10Boxed__int(values__16, for_index427)
            var t487 int = for_index427 + 1
            for_index427 = t487
            switch for_item428.(type) {
            case Value:
                var x430 int = for_item428.(Value)._0
                var t489 int
                var inline598 int = ref_get__Ref_3int(result__17)
                t489 = inline598
                var t490 int = t489 + x430
                ref_set__Ref_3int(result__17, t490)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop485
        }
    }
    var inline600 int = ref_get__Ref_3int(result__17)
    return inline600
}

func main0() struct{} {
    var t492 Boxed__int = Value{
        _0: 19,
    }
    var t493 Boxed__int = Value{
        _0: 23,
    }
    var t494 [2]Boxed__int = [2]Boxed__int{t492, t493}
    var boxed__19 *_goml_vec_Boxed__int = func(values [2]Boxed__int) *_goml_vec_Boxed__int {
        return &_goml_vec_Boxed__int{
            items: values[0:len(values)],
        }
    }(t494)
    var t495 First__int = First__int_Shared{
        _0: 7,
    }
    var t496 string = classify(t495)
    println__T_string(t496)
    var t497 string = classify(Idle{})
    println__T_string(t497)
    var t498 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t499 string = classify(t498)
    println__T_string(t499)
    var t500 Result__int__string = Ok{
        _0: 11,
    }
    var t501 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t500,
    }
    var t502 string = nested(t501)
    println__T_string(t502)
    var t503 Result__int__string = Err{
        _0: "bad",
    }
    var t504 Option__Result__int__string = Option__Result__int__string_Some{
        _0: t503,
    }
    var t505 string = nested(t504)
    println__T_string(t505)
    var t506 string = nested(Option__Result__int__string_None{})
    var inline638 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t506)
    _goml_runtime_core_string_println(inline638)
    var t508 int
    var inline635 int = 13
    t508 = inline635
    var inline632 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t508)
    _goml_runtime_core_string_println(inline632)
    var t509 int
    t509 = 0
    var inline626 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t509)
    _goml_runtime_core_string_println(inline626)
    var t510 bool
    t510 = true
    var inline622 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t510)
    _goml_runtime_core_string_println(inline622)
    var t512 bool
    t512 = false
    var inline618 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t512)
    _goml_runtime_core_string_println(inline618)
    var t513 Option__int = Option__int_Some{
        _0: 15,
    }
    var t514 int = take_once(t513)
    var inline615 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t514)
    _goml_runtime_core_string_println(inline615)
    var t516 int
    var inline612 int = 17
    t516 = inline612
    var inline608 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t516)
    _goml_runtime_core_string_println(inline608)
    var t517 int = sum_boxed(boxed__19)
    var inline605 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t517)
    _goml_runtime_core_string_println(inline605)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t539 string
    t539 = value__1
    _goml_runtime_core_string_println(t539)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t551 string = _goml_runtime_core_int_to_string(self__151)
    return t551
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t554 string = _goml_runtime_core_bool_to_string(self__148)
    return t554
}

func main() {
    main0()
}
