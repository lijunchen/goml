package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_int_to_string(x int) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_int32_to_string(x int32) string {
    return _goml_fmt.Sprintf("%d", x)
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func _goml_intrinsic_missing(s string) struct{} {
    println("missing: " + s)
    panic("")
    return struct{}{}
}

func array_get__Array_4_3int(arr [4]int, index int) int {
    return arr[index]
}

func array_get__Array_2_3int(arr [2]int, index int) int {
    return arr[index]
}

type _goml_vec_int32 struct {
    items []int32
}

func vec_new__Vec_5int32() *_goml_vec_int32 {
    return &_goml_vec_int32{
        items: nil,
    }
}

func vec_push__Vec_5int32(vec *_goml_vec_int32, elem int32) struct{} {
    vec.items = append(vec.items, elem)
    return struct{}{}
}

func vec_get__Vec_5int32(vec *_goml_vec_int32, index int) int32 {
    return vec.items[index]
}

func vec_len__Vec_5int32(vec *_goml_vec_int32) int {
    return int(len(vec.items))
}

type ref_Maybe_x struct {
    value Maybe
}

func ref__Ref_5Maybe(value Maybe) *ref_Maybe_x {
    return &ref_Maybe_x{
        value: value,
    }
}

func ref_get__Ref_5Maybe(reference *ref_Maybe_x) Maybe {
    return reference.value
}

func ref_set__Ref_5Maybe(reference *ref_Maybe_x, value Maybe) struct{} {
    reference.value = value
    return struct{}{}
}

func missing__int32(s string) int32 {
    _goml_intrinsic_missing(s)
    var ret int32
    return ret
}

func missing__string(s string) string {
    _goml_intrinsic_missing(s)
    var ret string
    return ret
}

type Pair struct {
    left int32
    right int32
}

type Ordering int32

type Maybe interface {
    isMaybe()
}

type None struct {}

func (_ None) isMaybe() {}

type Some struct {
    _0 int32
}

func (_ Some) isMaybe() {}

type Either interface {
    isEither()
}

type Left struct {
    _0 int32
}

func (_ Left) isEither() {}

type Right struct {
    _0 int32
}

func (_ Right) isEither() {}

func unwrap_either(value__0 Either) int32 {
    switch value__0.(type) {
    case Left:
        var shared__2 int32 = value__0.(Left)._0
        var jp448 int32
        switch value__0.(type) {
        case Left:
            jp448 = 0
        case Right:
            jp448 = 1
        default:
            panic("non-exhaustive match")
        }
        var t449 int32 = shared__2 + jp448
        return t449
    default:
        switch value__0.(type) {
        case Right:
            var shared__2 int32 = value__0.(Right)._0
            var jp453 int32
            switch value__0.(type) {
            case Left:
                jp453 = 0
            case Right:
                jp453 = 1
            default:
                panic("non-exhaustive match")
            }
            var t454 int32 = shared__2 + jp453
            return t454
        default:
            var t455 int32 = missing__int32("")
            return t455
        }
    }
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var jp467 string
    switch value__4.(type) {
    case Some:
        var x414 int32 = value__4.(Some)._0
        var t508 bool = x414 == 0
        if t508 {
            jp467 = "small"
        } else {
            var t511 bool = x414 == 1
            if t511 {
                jp467 = "small"
            } else {
                var t514 bool = x414 >= 2
                if t514 {
                    var t517 bool = x414 <= 4
                    if t517 {
                        jp467 = "middle"
                    } else {
                        var t520 bool = x414 > 10
                        if t520 {
                            jp467 = "large"
                        } else {
                            jp467 = "other"
                        }
                    }
                } else {
                    var t523 bool = x414 > 10
                    if t523 {
                        jp467 = "large"
                    } else {
                        jp467 = "other"
                    }
                }
            }
        }
    default:
        jp467 = "none"
    }
    var t484 int = vec_len__Vec_5int32(numbers__5)
    var t485 bool = t484 == 0
    var jp469 string
    if t485 {
        jp469 = "empty"
    } else {
        var t488 int = vec_len__Vec_5int32(numbers__5)
        var t489 bool = t488 >= 1
        if t489 {
            var first__10 int32 = vec_get__Vec_5int32(numbers__5, 0)
            var t490 int = vec_len__Vec_5int32(numbers__5)
            var tail__11 []int32 = numbers__5.items[1:t490]
            var t493 int
            var inline582 int = len(tail__11)
            t493 = inline582
            var t494 int32 = int32(int(t493))
            var t495 bool = first__10 == t494
            if t495 {
                jp469 = "balanced"
            } else {
                var t498 int = vec_len__Vec_5int32(numbers__5)
                var t499 bool = t498 >= 1
                if t499 {
                    jp469 = "nonempty"
                } else {
                    var t500 string = missing__string("")
                    jp469 = t500
                }
            }
        } else {
            var t503 int = vec_len__Vec_5int32(numbers__5)
            var t504 bool = t503 >= 1
            if t504 {
                jp469 = "nonempty"
            } else {
                var t505 string = missing__string("")
                jp469 = t505
            }
        }
    }
    var t476 int = len(view__6)
    var t477 bool = t476 >= 2
    var jp471 string
    if t477 {
        var first__13 int32 = view__6[0]
        var t478 int = len(view__6)
        var t479 int = t478 - 1
        var t480 int = t479 + 0
        var last__14 int32 = view__6[t480]
        var t483 bool = first__13 == last__14
        if t483 {
            jp471 = "same ends"
        } else {
            jp471 = "different ends"
        }
    } else {
        jp471 = "different ends"
    }
    var t472 string = jp467 + "/"
    var t473 string = t472 + jp469
    var t474 string = t473 + "/"
    var t475 string = t474 + jp471
    return t475
}

func main0() struct{} {
    var x419 int32 = 3
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var first__19 int = array_get__Array_4_3int(values__18, 0)
    var last__21 int = array_get__Array_4_3int(values__18, 3)
    var t525 int = array_get__Array_4_3int(values__18, 1)
    var t526 int = array_get__Array_4_3int(values__18, 2)
    var middle__20 [2]int = [2]int{t525, t526}
    var inline621 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x419)
    _goml_runtime_core_string_println(inline621)
    var t527 int = array_get__Array_2_3int(middle__20, 0)
    var t528 int = first__19 + t527
    var t529 int = t528 + last__21
    var inline618 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(t529)
    _goml_runtime_core_string_println(inline618)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t530 int
    var inline616 int = vec_len__Vec_5int32(numbers__22)
    t530 = inline616
    var view__23 []int32 = numbers__22.items[0:t530]
    var t531 Maybe = Some{
        _0: 3,
    }
    var t532 string = describe(t531, numbers__22, view__23)
    var inline613 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t532)
    _goml_runtime_core_string_println(inline613)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t533 string = describe(None{}, empty__24, empty_view__25)
    var inline610 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t533)
    _goml_runtime_core_string_println(inline610)
    var t534 Maybe = Some{
        _0: 7,
    }
    var state__26 *ref_Maybe_x
    var inline608 *ref_Maybe_x = ref__Ref_5Maybe(t534)
    state__26 = inline608
    Loop_loop548:
    for {
        var mtmp428 Maybe
        var inline589 Maybe = ref_get__Ref_5Maybe(state__26)
        mtmp428 = inline589
        switch mtmp428.(type) {
        case Some:
            var x429 int32 = mtmp428.(Some)._0
            var inline586 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x429)
            _goml_runtime_core_string_println(inline586)
            ref_set__Ref_5Maybe(state__26, None{})
            continue
        default:
            break Loop_loop548
        }
    }
    var x434 int32 = 6
    var inline591 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(x434)
    _goml_runtime_core_string_println(inline591)
    var jp538 int32
    var value__30 int32 = 5
    var jp543 int32
    var x438 int32 = 5
    jp543 = x438
    var t544 int32 = value__30 + jp543
    jp538 = t544
    var inline605 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(jp538)
    _goml_runtime_core_string_println(inline605)
    var t539 Either = Right{
        _0: 11,
    }
    var t540 int32 = unwrap_either(t539)
    var inline602 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t540)
    _goml_runtime_core_string_println(inline602)
    var t541 string
    var inline597 rune = 98
    var inline599 bool = inline597 >= 97
    if inline599 {
        var inline600 bool = inline597 <= 99
        if inline600 {
            t541 = "abc"
        } else {
            t541 = "other"
        }
    } else {
        t541 = "other"
    }
    var inline594 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t541)
    _goml_runtime_core_string_println(inline594)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t575 string = _goml_runtime_core_int32_to_string(self__154)
    return t575
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t578 string = _goml_runtime_core_int_to_string(self__151)
    return t578
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
