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
    var t422 bool
    t422 = false
    var inline472 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t422)
    _goml_runtime_core_string_println(inline472)
    var t423 bool
    t423 = false
    var t424 bool = !t423
    var inline468 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t424)
    _goml_runtime_core_string_println(inline468)
    var t443 bool
    t443 = false
    var jp428 bool
    if t443 {
        var t444 int = 2
        var t445 int = 2
        var inline457 bool = t444 == t445
        jp428 = inline457
    } else {
        jp428 = false
    }
    var inline464 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp428)
    _goml_runtime_core_string_println(inline464)
    var t429 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t430 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t429, t430}
    var t431 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t432 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t431, t432}
    array_get__Array_2_15AlwaysDifferent(left_array__6, 0)
    array_get__Array_2_15AlwaysDifferent(right_array__7, 0)
    var t437 bool
    t437 = false
    var jp434 bool
    if t437 {
        array_get__Array_2_15AlwaysDifferent(left_array__6, 1)
        array_get__Array_2_15AlwaysDifferent(right_array__7, 1)
        jp434 = false
    } else {
        jp434 = false
    }
    var inline460 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp434)
    _goml_runtime_core_string_println(inline460)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__148 bool) string {
    var t455 string = _goml_runtime_core_bool_to_string(self__148)
    return t455
}

func main() {
    main0()
}
