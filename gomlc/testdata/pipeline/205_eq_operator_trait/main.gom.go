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

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func array_get__Array_2_15AlwaysDifferent(arr [2]AlwaysDifferent, index int) AlwaysDifferent {
    return arr[index]
}

type AlwaysDifferent struct {
    value int32
}

type Ordering int32

func main0() struct{} {
    var t419 bool
    t419 = false
    var inline469 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t419)
    _goml_runtime_core_string_println(inline469)
    var t420 bool
    t420 = false
    var t421 bool = !t420
    var inline465 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t421)
    _goml_runtime_core_string_println(inline465)
    var t440 bool
    t440 = false
    var jp425 bool
    if t440 {
        var t441 int = 2
        var t442 int = 2
        var inline454 bool = t441 == t442
        jp425 = inline454
    } else {
        jp425 = false
    }
    var inline461 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp425)
    _goml_runtime_core_string_println(inline461)
    var t426 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t427 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t426, t427}
    var t428 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t429 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t428, t429}
    array_get__Array_2_15AlwaysDifferent(left_array__6, 0)
    array_get__Array_2_15AlwaysDifferent(right_array__7, 0)
    var t434 bool
    t434 = false
    var jp431 bool
    if t434 {
        array_get__Array_2_15AlwaysDifferent(left_array__6, 1)
        array_get__Array_2_15AlwaysDifferent(right_array__7, 1)
        jp431 = false
    } else {
        jp431 = false
    }
    var inline457 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp431)
    _goml_runtime_core_string_println(inline457)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t452 string = _goml_runtime_core_bool_to_string(self__148)
    return t452
}

func main() {
    main0()
}
