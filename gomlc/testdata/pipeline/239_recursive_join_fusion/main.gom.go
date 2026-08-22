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

type _goml_vec_uint32 struct {
    items []uint32
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

type Ordering int32

type Option__isize struct {
    _tag int32
    _v1_0 int
}

func find(wanted__0 int) Option__isize {
    var current__1 *ref_int_x
    var inline848 int = 0
    var inline849 *ref_int_x = ref__Ref_3int(inline848)
    current__1 = inline849
    for {
        var value__2 int
        var inline846 int = ref_get__Ref_3int(current__1)
        value__2 = inline846
        var t811 bool = value__2 >= 5
        if t811 {
            return Option__isize{
                _tag: 0,
            }
        } else {
            var t813 bool = value__2 == wanted__0
            if t813 {
                var t814 Option__isize = Option__isize{
                    _tag: 1,
                    _v1_0: value__2,
                }
                return t814
            } else {
                var t815 int = value__2 + 1
                ref_set__Ref_3int(current__1, t815)
                continue
            }
        }
    }
}

func main0() struct{} {
    var t821 bool
    var inline871 int = 3
    var inline872 Option__isize = find(inline871)
    switch inline872._tag {
    case 0:
        t821 = false
    case 1:
        t821 = true
    default:
        panic("non-exhaustive match")
    }
    var inline868 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t821)
    _goml_runtime_core_string_println(inline868)
    var t822 bool
    var inline865 int = 8
    var inline866 Option__isize = find(inline865)
    switch inline866._tag {
    case 0:
        t822 = false
    case 1:
        t822 = true
    default:
        panic("non-exhaustive match")
    }
    var inline862 string = _goml_m_trait__impl_i_ToString_i_bool_i_to__string(t822)
    _goml_runtime_core_string_println(inline862)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_bool_i_to__string(self__401 bool) string {
    var t836 string = _goml_runtime_core_bool_to_string(self__401)
    return t836
}

func main() {
    main0()
}
