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

type Maybe struct {
    _tag int32
    _v1_0 int32
}

type Either struct {
    _tag int32
    _v0_0 int32
    _v1_0 int32
}

func unwrap_either(value__0 Either) int32 {
    switch value__0._tag {
    case 0:
        var shared__2 int32 = value__0._v0_0
        var jp451 int32
        switch value__0._tag {
        case 0:
            jp451 = 0
        case 1:
            jp451 = 1
        default:
            panic("non-exhaustive match")
        }
        var t452 int32 = shared__2 + jp451
        return t452
    default:
        switch value__0._tag {
        case 1:
            var shared__2 int32 = value__0._v1_0
            var jp456 int32
            switch value__0._tag {
            case 0:
                jp456 = 0
            case 1:
                jp456 = 1
            default:
                panic("non-exhaustive match")
            }
            var t457 int32 = shared__2 + jp456
            return t457
        default:
            var t458 int32 = missing__int32("")
            return t458
        }
    }
}

func describe(value__4 Maybe, numbers__5 *_goml_vec_int32, view__6 []int32) string {
    var jp470 string
    switch value__4._tag {
    case 1:
        var x417 int32 = value__4._v1_0
        var t511 bool = x417 == 0
        if t511 {
            jp470 = "small"
        } else {
            var t514 bool = x417 == 1
            if t514 {
                jp470 = "small"
            } else {
                var t517 bool = x417 >= 2
                if t517 {
                    var t520 bool = x417 <= 4
                    if t520 {
                        jp470 = "middle"
                    } else {
                        var t523 bool = x417 > 10
                        if t523 {
                            jp470 = "large"
                        } else {
                            jp470 = "other"
                        }
                    }
                } else {
                    var t526 bool = x417 > 10
                    if t526 {
                        jp470 = "large"
                    } else {
                        jp470 = "other"
                    }
                }
            }
        }
    default:
        jp470 = "none"
    }
    var t487 int = vec_len__Vec_5int32(numbers__5)
    var t488 bool = t487 == 0
    var jp472 string
    if t488 {
        jp472 = "empty"
    } else {
        var t491 int = vec_len__Vec_5int32(numbers__5)
        var t492 bool = t491 >= 1
        if t492 {
            var first__10 int32 = vec_get__Vec_5int32(numbers__5, 0)
            var t493 int = vec_len__Vec_5int32(numbers__5)
            var tail__11 []int32 = numbers__5.items[1:t493]
            var t496 int
            var inline585 int = len(tail__11)
            t496 = inline585
            var t497 int32 = int32(int(t496))
            var t498 bool = first__10 == t497
            if t498 {
                jp472 = "balanced"
            } else {
                var t501 int = vec_len__Vec_5int32(numbers__5)
                var t502 bool = t501 >= 1
                if t502 {
                    jp472 = "nonempty"
                } else {
                    var t503 string = missing__string("")
                    jp472 = t503
                }
            }
        } else {
            var t506 int = vec_len__Vec_5int32(numbers__5)
            var t507 bool = t506 >= 1
            if t507 {
                jp472 = "nonempty"
            } else {
                var t508 string = missing__string("")
                jp472 = t508
            }
        }
    }
    var t479 int = len(view__6)
    var t480 bool = t479 >= 2
    var jp474 string
    if t480 {
        var first__13 int32 = view__6[0]
        var t481 int = len(view__6)
        var t482 int = t481 - 1
        var t483 int = t482 + 0
        var last__14 int32 = view__6[t483]
        var t486 bool = first__13 == last__14
        if t486 {
            jp474 = "same ends"
        } else {
            jp474 = "different ends"
        }
    } else {
        jp474 = "different ends"
    }
    var t475 string = jp470 + "/"
    var t476 string = t475 + jp472
    var t477 string = t476 + "/"
    var t478 string = t477 + jp474
    return t478
}

func main0() struct{} {
    var x422 int32 = 3
    var values__18 [4]int = [4]int{1, 2, 3, 1}
    var first__19 int = array_get__Array_4_3int(values__18, 0)
    var last__21 int = array_get__Array_4_3int(values__18, 3)
    var t528 int = array_get__Array_4_3int(values__18, 1)
    var t529 int = array_get__Array_4_3int(values__18, 2)
    var middle__20 [2]int = [2]int{t528, t529}
    var inline624 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x422)
    _goml_runtime_core_string_println(inline624)
    var t530 int = array_get__Array_2_3int(middle__20, 0)
    var t531 int = first__19 + t530
    var t532 int = t531 + last__21
    var inline621 string = _goml_m_trait__impl_i_ToString_i_isize_i_to__string(t532)
    _goml_runtime_core_string_println(inline621)
    var numbers__22 *_goml_vec_int32 = vec_new__Vec_5int32()
    vec_push__Vec_5int32(numbers__22, 1)
    vec_push__Vec_5int32(numbers__22, 8)
    var t533 int
    var inline619 int = vec_len__Vec_5int32(numbers__22)
    t533 = inline619
    var view__23 []int32 = numbers__22.items[0:t533]
    var t534 Maybe = Maybe{
        _tag: 1,
        _v1_0: 3,
    }
    var t535 string = describe(t534, numbers__22, view__23)
    var inline616 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t535)
    _goml_runtime_core_string_println(inline616)
    var empty__24 *_goml_vec_int32 = vec_new__Vec_5int32()
    var empty_view__25 []int32 = empty__24.items[0:0]
    var t536 string = describe(Maybe{
        _tag: 0,
    }, empty__24, empty_view__25)
    var inline613 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t536)
    _goml_runtime_core_string_println(inline613)
    var t537 Maybe = Maybe{
        _tag: 1,
        _v1_0: 7,
    }
    var state__26 *ref_Maybe_x
    var inline611 *ref_Maybe_x = ref__Ref_5Maybe(t537)
    state__26 = inline611
    Loop_loop551:
    for {
        var mtmp431 Maybe
        var inline592 Maybe = ref_get__Ref_5Maybe(state__26)
        mtmp431 = inline592
        switch mtmp431._tag {
        case 1:
            var x432 int32 = mtmp431._v1_0
            var inline589 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x432)
            _goml_runtime_core_string_println(inline589)
            ref_set__Ref_5Maybe(state__26, Maybe{
                _tag: 0,
            })
            continue
        default:
            break Loop_loop551
        }
    }
    var x437 int32 = 6
    var inline594 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(x437)
    _goml_runtime_core_string_println(inline594)
    var jp541 int32
    var value__30 int32 = 5
    var jp546 int32
    var x441 int32 = 5
    jp546 = x441
    var t547 int32 = value__30 + jp546
    jp541 = t547
    var inline608 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(jp541)
    _goml_runtime_core_string_println(inline608)
    var t542 Either = Either{
        _tag: 1,
        _v1_0: 11,
    }
    var t543 int32 = unwrap_either(t542)
    var inline605 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t543)
    _goml_runtime_core_string_println(inline605)
    var t544 string
    var inline600 rune = 98
    var inline602 bool = inline600 >= 97
    if inline602 {
        var inline603 bool = inline600 <= 99
        if inline603 {
            t544 = "abc"
        } else {
            t544 = "other"
        }
    } else {
        t544 = "other"
    }
    var inline597 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t544)
    _goml_runtime_core_string_println(inline597)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t578 string = _goml_runtime_core_int32_to_string(self__154)
    return t578
}

func _goml_m_trait__impl_i_ToString_i_isize_i_to__string(self__151 int) string {
    var t581 string = _goml_runtime_core_int_to_string(self__151)
    return t581
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
