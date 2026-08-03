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

type Tuple2_15AlwaysDifferent_3int struct {
    _0 AlwaysDifferent
    _1 int
}

type AlwaysDifferent struct {
    value int32
}

func main0() struct{} {
    var t188 bool
    t188 = false
    var inline238 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t188)
    _goml_runtime_core_string_println(inline238)
    var t189 bool
    t189 = false
    var t190 bool = !t189
    var inline234 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t190)
    _goml_runtime_core_string_println(inline234)
    var t209 bool
    t209 = false
    var jp194 bool
    if t209 {
        var t210 int = 2
        var t211 int = 2
        var inline223 bool = t210 == t211
        jp194 = inline223
    } else {
        jp194 = false
    }
    var inline230 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp194)
    _goml_runtime_core_string_println(inline230)
    var t195 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t196 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t195, t196}
    var t197 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t198 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t197, t198}
    array_get__Array_2_15AlwaysDifferent(left_array__6, 0)
    array_get__Array_2_15AlwaysDifferent(right_array__7, 0)
    var t203 bool
    t203 = false
    var jp200 bool
    if t203 {
        array_get__Array_2_15AlwaysDifferent(left_array__6, 1)
        array_get__Array_2_15AlwaysDifferent(right_array__7, 1)
        jp200 = false
    } else {
        jp200 = false
    }
    var inline226 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp200)
    _goml_runtime_core_string_println(inline226)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__66 bool) string {
    var t221 string = _goml_runtime_core_bool_to_string(self__66)
    return t221
}

func main() {
    main0()
}
