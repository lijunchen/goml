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

type Second struct {
    _tag int32
    _v0_0 int
}

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

type Result__int__string struct {
    _tag int32
    _v0_0 int
    _v1_0 string
}

type Option__Result__int__string struct {
    _tag int32
    _v1_0 Result__int__string
}

type Option__int struct {
    _tag int32
    _v1_0 int
}

type Boxed__int struct {
    _tag int32
    _v0_0 int
}

func classify(value__0 First__int) string {
    switch value__0.(type) {
    case First__int_Shared:
        var x411 int = value__0.(First__int_Shared)._0
        var t452 string
        var inline580 string = _goml_runtime_core_int_to_string(x411)
        t452 = inline580
        var t453 string = "shared:" + t452
        return t453
    case Idle:
        return "idle"
    case Data:
        var x412 int = value__0.(Data)._0
        var x413 string = value__0.(Data)._1
        var t454 string = x413 + ":"
        var t455 string
        var inline582 string = _goml_runtime_core_int_to_string(x412)
        t455 = inline582
        var t456 string = t454 + t455
        return t456
    default:
        panic("non-exhaustive match")
    }
}

func nested(value__4 Option__Result__int__string) string {
    switch value__4._tag {
    case 0:
        return "none"
    case 1:
        var x414 Result__int__string = value__4._v1_0
        switch x414._tag {
        case 0:
            var x415 int = x414._v0_0
            var t463 string
            var inline584 string = _goml_runtime_core_int_to_string(x415)
            t463 = inline584
            var t464 string = "ok:" + t463
            return t464
        case 1:
            var x416 string = x414._v1_0
            var t465 string = "err:" + x416
            return t465
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func take_once(value__10 Option__int) int {
    var current__11 *ref_Option__int_x
    var inline597 *ref_Option__int_x = ref__Ref_11Option__int(value__10)
    current__11 = inline597
    var result__12 *ref_int_x
    var inline594 int = 0
    var inline595 *ref_int_x = ref__Ref_3int(inline594)
    result__12 = inline595
    Loop_loop478:
    for {
        var mtmp421 Option__int
        var inline590 Option__int = ref_get__Ref_11Option__int(current__11)
        mtmp421 = inline590
        switch mtmp421._tag {
        case 1:
            var x422 int = mtmp421._v1_0
            ref_set__Ref_3int(result__12, x422)
            ref_set__Ref_11Option__int(current__11, Option__int{
                _tag: 0,
            })
            continue
        default:
            break Loop_loop478
        }
    }
    var inline592 int = ref_get__Ref_3int(result__12)
    return inline592
}

func sum_boxed(values__16 *_goml_vec_Boxed__int) int {
    var result__17 *ref_int_x
    var inline605 int = 0
    var inline606 *ref_int_x = ref__Ref_3int(inline605)
    result__17 = inline606
    var for_limit429 int = vec_len__Vec_10Boxed__int(values__16)
    var for_index430 int = 0
    Loop_loop488:
    for {
        var t489 bool = for_index430 < for_limit429
        if t489 {
            var for_item431 Boxed__int = vec_get__Vec_10Boxed__int(values__16, for_index430)
            var t490 int = for_index430 + 1
            for_index430 = t490
            switch for_item431._tag {
            case 0:
                var x433 int = for_item431._v0_0
                var t492 int
                var inline601 int = ref_get__Ref_3int(result__17)
                t492 = inline601
                var t493 int = t492 + x433
                ref_set__Ref_3int(result__17, t493)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop488
        }
    }
    var inline603 int = ref_get__Ref_3int(result__17)
    return inline603
}

func main0() struct{} {
    var t495 Boxed__int = Boxed__int{
        _tag: 0,
        _v0_0: 19,
    }
    var t496 Boxed__int = Boxed__int{
        _tag: 0,
        _v0_0: 23,
    }
    var t497 [2]Boxed__int = [2]Boxed__int{t495, t496}
    var boxed__19 *_goml_vec_Boxed__int = func(values [2]Boxed__int) *_goml_vec_Boxed__int {
        var storage struct {
            vector _goml_vec_Boxed__int
            values [2]Boxed__int
        }
        storage.values = values
        storage.vector.items = storage.values[0:len(storage.values)]
        return &storage.vector
    }(t497)
    var t498 First__int = First__int_Shared{
        _0: 7,
    }
    var t499 string = classify(t498)
    println__T_string(t499)
    var t500 string = classify(Idle{})
    println__T_string(t500)
    var t501 First__int = Data{
        _0: 9,
        _1: "data",
    }
    var t502 string = classify(t501)
    println__T_string(t502)
    var t503 Result__int__string = Result__int__string{
        _tag: 0,
        _v0_0: 11,
    }
    var t504 Option__Result__int__string = Option__Result__int__string{
        _tag: 1,
        _v1_0: t503,
    }
    var t505 string = nested(t504)
    println__T_string(t505)
    var t506 Result__int__string = Result__int__string{
        _tag: 1,
        _v1_0: "bad",
    }
    var t507 Option__Result__int__string = Option__Result__int__string{
        _tag: 1,
        _v1_0: t506,
    }
    var t508 string = nested(t507)
    println__T_string(t508)
    var t509 string = nested(Option__Result__int__string{
        _tag: 0,
    })
    var inline641 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t509)
    _goml_runtime_core_string_println(inline641)
    var t511 int
    var inline638 int = 13
    t511 = inline638
    var inline635 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t511)
    _goml_runtime_core_string_println(inline635)
    var t512 int
    t512 = 0
    var inline629 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t512)
    _goml_runtime_core_string_println(inline629)
    var t513 bool
    t513 = true
    var inline625 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t513)
    _goml_runtime_core_string_println(inline625)
    var t515 bool
    t515 = false
    var inline621 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t515)
    _goml_runtime_core_string_println(inline621)
    var t516 Option__int = Option__int{
        _tag: 1,
        _v1_0: 15,
    }
    var t517 int = take_once(t516)
    var inline618 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t517)
    _goml_runtime_core_string_println(inline618)
    var t519 int
    var inline615 int = 17
    t519 = inline615
    var inline611 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t519)
    _goml_runtime_core_string_println(inline611)
    var t520 int = sum_boxed(boxed__19)
    var inline608 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t520)
    _goml_runtime_core_string_println(inline608)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t542 string
    t542 = value__1
    _goml_runtime_core_string_println(t542)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t554 string = _goml_runtime_core_int_to_string(self__151)
    return t554
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t557 string = _goml_runtime_core_bool_to_string(self__148)
    return t557
}

func main() {
    main0()
}
