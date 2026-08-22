package main

import (
    _goml_os "os"
)

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

func array_get__Array_2_15AlwaysDifferent(arr [2]AlwaysDifferent, index int) AlwaysDifferent {
    return arr[index]
}

type _goml_vec_uint32 struct {
    items []uint32
}

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
}

type AlwaysDifferent struct {
    value int32
}

type Ordering int32

func main0() struct{} {
    var t807 bool
    t807 = false
    var inline857 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t807)
    _goml_runtime_core_string_println(inline857)
    var t808 bool
    t808 = false
    var t809 bool = !t808
    var inline853 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t809)
    _goml_runtime_core_string_println(inline853)
    var t828 bool
    t828 = false
    var jp813 bool
    if t828 {
        var t829 int = 2
        var t830 int = 2
        var inline842 bool = t829 == t830
        jp813 = inline842
    } else {
        jp813 = false
    }
    var inline849 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp813)
    _goml_runtime_core_string_println(inline849)
    var t814 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t815 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var left_array__6 [2]AlwaysDifferent = [2]AlwaysDifferent{t814, t815}
    var t816 AlwaysDifferent = AlwaysDifferent{
        value: 1,
    }
    var t817 AlwaysDifferent = AlwaysDifferent{
        value: 2,
    }
    var right_array__7 [2]AlwaysDifferent = [2]AlwaysDifferent{t816, t817}
    array_get__Array_2_15AlwaysDifferent(left_array__6, 0)
    array_get__Array_2_15AlwaysDifferent(right_array__7, 0)
    var t822 bool
    t822 = false
    var jp819 bool
    if t822 {
        array_get__Array_2_15AlwaysDifferent(left_array__6, 1)
        array_get__Array_2_15AlwaysDifferent(right_array__7, 1)
        jp819 = false
    } else {
        jp819 = false
    }
    var inline845 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(jp819)
    _goml_runtime_core_string_println(inline845)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t840 string = _goml_runtime_core_bool_to_string(self__401)
    return t840
}

func main() {
    main0()
}
