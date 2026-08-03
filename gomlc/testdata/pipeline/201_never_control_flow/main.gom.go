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
    var inline250 int = 0
    var inline251 *ref_int_x = ref__Ref_3int(inline250)
    count__2 = inline251
    Loop_loop190:
    for {
        var t191 int
        var inline248 int = ref_get__Ref_3int(count__2)
        t191 = inline248
        var t192 bool = t191 < 2
        if t192 {
            var t193 int
            var inline246 int = ref_get__Ref_3int(count__2)
            t193 = inline246
            var t194 int = t193 + 1
            ref_set__Ref_3int(count__2, t194)
            var t198 int
            var inline242 int = ref_get__Ref_3int(count__2)
            t198 = inline242
            var t199 bool
            var inline239 int = 1
            var inline240 bool = t198 == inline239
            t199 = inline240
            var jp196 int
            if t199 {
                continue
            } else {
                jp196 = 7
                var inline236 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp196)
                _goml_runtime_core_string_println(inline236)
                continue
            }
        } else {
            break Loop_loop190
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    var jp204 int
    if stop__4 {
        return struct{}{}
    } else {
        jp204 = 9
        var inline253 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp204)
        _goml_runtime_core_string_println(inline253)
        return struct{}{}
    }
}

func main0() struct{} {
    var t206 int32
    var inline268 bool = false
    var inline270 int32
    if inline268 {
        t206 = 10
        var inline265 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t206)
        _goml_runtime_core_string_println(inline265)
        var t207 int32
        var inline259 bool = true
        var inline261 int32
        if inline259 {
            t207 = 10
            var inline256 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t207)
            _goml_runtime_core_string_println(inline256)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline261 = 20
            var inline263 int32 = inline261 + 1
            t207 = inline263
            var inline256 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t207)
            _goml_runtime_core_string_println(inline256)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    } else {
        inline270 = 20
        var inline272 int32 = inline270 + 1
        t206 = inline272
        var inline265 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t206)
        _goml_runtime_core_string_println(inline265)
        var t207 int32
        var inline259 bool = true
        var inline261 int32
        if inline259 {
            t207 = 10
            var inline256 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t207)
            _goml_runtime_core_string_println(inline256)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline261 = 20
            var inline263 int32 = inline261 + 1
            t207 = inline263
            var inline256 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t207)
            _goml_runtime_core_string_println(inline256)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t228 string = _goml_runtime_core_int_to_string(self__69)
    return t228
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t231 string = _goml_runtime_core_int32_to_string(self__72)
    return t231
}

func main() {
    main0()
}
