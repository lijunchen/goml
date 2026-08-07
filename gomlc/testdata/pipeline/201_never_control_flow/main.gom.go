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
    var inline245 int = 0
    var inline246 *ref_int_x = ref__Ref_3int(inline245)
    count__2 = inline246
    Loop_loop185:
    for {
        var t186 int
        var inline243 int = ref_get__Ref_3int(count__2)
        t186 = inline243
        var t187 bool = t186 < 2
        if t187 {
            var t188 int
            var inline241 int = ref_get__Ref_3int(count__2)
            t188 = inline241
            var t189 int = t188 + 1
            ref_set__Ref_3int(count__2, t189)
            var t193 int
            var inline237 int = ref_get__Ref_3int(count__2)
            t193 = inline237
            var t194 bool
            var inline234 int = 1
            var inline235 bool = t193 == inline234
            t194 = inline235
            var jp191 int
            if t194 {
                continue
            } else {
                jp191 = 7
                var inline231 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp191)
                _goml_runtime_core_string_println(inline231)
                continue
            }
        } else {
            break Loop_loop185
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    var jp199 int
    if stop__4 {
        return struct{}{}
    } else {
        jp199 = 9
        var inline248 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp199)
        _goml_runtime_core_string_println(inline248)
        return struct{}{}
    }
}

func main0() struct{} {
    var t201 int32
    var inline263 bool = false
    var inline265 int32
    if inline263 {
        t201 = 10
        var inline260 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t201)
        _goml_runtime_core_string_println(inline260)
        var t202 int32
        var inline254 bool = true
        var inline256 int32
        if inline254 {
            t202 = 10
            var inline251 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
            _goml_runtime_core_string_println(inline251)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline256 = 20
            var inline258 int32 = inline256 + 1
            t202 = inline258
            var inline251 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
            _goml_runtime_core_string_println(inline251)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    } else {
        inline265 = 20
        var inline267 int32 = inline265 + 1
        t201 = inline267
        var inline260 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t201)
        _goml_runtime_core_string_println(inline260)
        var t202 int32
        var inline254 bool = true
        var inline256 int32
        if inline254 {
            t202 = 10
            var inline251 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
            _goml_runtime_core_string_println(inline251)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline256 = 20
            var inline258 int32 = inline256 + 1
            t202 = inline258
            var inline251 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
            _goml_runtime_core_string_println(inline251)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__69 int) string {
    var t223 string = _goml_runtime_core_int_to_string(self__69)
    return t223
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t226 string = _goml_runtime_core_int32_to_string(self__72)
    return t226
}

func main() {
    main0()
}
