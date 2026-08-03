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

func continue_branch() struct{} {
    var count__2 *ref_int_x
    var inline209 int = 0
    var inline210 *ref_int_x = ref__Ref_3int(inline209)
    count__2 = inline210
    Loop_loop149:
    for {
        var t150 int
        var inline207 int = ref_get__Ref_3int(count__2)
        t150 = inline207
        var t151 bool = t150 < 2
        if t151 {
            var t152 int
            var inline205 int = ref_get__Ref_3int(count__2)
            t152 = inline205
            var t153 int = t152 + 1
            ref_set__Ref_3int(count__2, t153)
            var t157 int
            var inline201 int = ref_get__Ref_3int(count__2)
            t157 = inline201
            var t158 bool
            var inline198 int = 1
            var inline199 bool = t157 == inline198
            t158 = inline199
            var jp155 int
            if t158 {
                continue
            } else {
                jp155 = 7
                var inline195 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp155)
                _goml_runtime_core_string_println(inline195)
                continue
            }
        } else {
            break Loop_loop149
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    var jp163 int
    if stop__4 {
        return struct{}{}
    } else {
        jp163 = 9
        var inline212 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp163)
        _goml_runtime_core_string_println(inline212)
        return struct{}{}
    }
}

func main0() struct{} {
    var t165 int32
    var inline227 bool = false
    var inline229 int32
    if inline227 {
        t165 = 10
        var inline224 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t165)
        _goml_runtime_core_string_println(inline224)
        var t166 int32
        var inline218 bool = true
        var inline220 int32
        if inline218 {
            t166 = 10
            var inline215 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t166)
            _goml_runtime_core_string_println(inline215)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline220 = 20
            var inline222 int32 = inline220 + 1
            t166 = inline222
            var inline215 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t166)
            _goml_runtime_core_string_println(inline215)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    } else {
        inline229 = 20
        var inline231 int32 = inline229 + 1
        t165 = inline231
        var inline224 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t165)
        _goml_runtime_core_string_println(inline224)
        var t166 int32
        var inline218 bool = true
        var inline220 int32
        if inline218 {
            t166 = 10
            var inline215 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t166)
            _goml_runtime_core_string_println(inline215)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline220 = 20
            var inline222 int32 = inline220 + 1
            t166 = inline222
            var inline215 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t166)
            _goml_runtime_core_string_println(inline215)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t187 string = _goml_runtime_core_int_to_string(self__69)
    return t187
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t190 string = _goml_runtime_core_int32_to_string(self__72)
    return t190
}

func main() {
    main0()
}
