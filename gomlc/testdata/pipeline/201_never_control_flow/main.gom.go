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
    var inline255 int = 0
    var inline256 *ref_int_x = ref__Ref_3int(inline255)
    count__2 = inline256
    Loop_loop195:
    for {
        var t196 int
        var inline253 int = ref_get__Ref_3int(count__2)
        t196 = inline253
        var t197 bool = t196 < 2
        if t197 {
            var t198 int
            var inline251 int = ref_get__Ref_3int(count__2)
            t198 = inline251
            var t199 int = t198 + 1
            ref_set__Ref_3int(count__2, t199)
            var t203 int
            var inline247 int = ref_get__Ref_3int(count__2)
            t203 = inline247
            var t204 bool
            var inline244 int = 1
            var inline245 bool = t203 == inline244
            t204 = inline245
            var jp201 int
            if t204 {
                continue
            } else {
                jp201 = 7
                var inline241 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp201)
                _goml_runtime_core_string_println(inline241)
                continue
            }
        } else {
            break Loop_loop195
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    var jp209 int
    if stop__4 {
        return struct{}{}
    } else {
        jp209 = 9
        var inline258 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp209)
        _goml_runtime_core_string_println(inline258)
        return struct{}{}
    }
}

func main0() struct{} {
    var t211 int32
    var inline273 bool = false
    var inline275 int32
    if inline273 {
        t211 = 10
        var inline270 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t211)
        _goml_runtime_core_string_println(inline270)
        var t212 int32
        var inline264 bool = true
        var inline266 int32
        if inline264 {
            t212 = 10
            var inline261 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t212)
            _goml_runtime_core_string_println(inline261)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline266 = 20
            var inline268 int32 = inline266 + 1
            t212 = inline268
            var inline261 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t212)
            _goml_runtime_core_string_println(inline261)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    } else {
        inline275 = 20
        var inline277 int32 = inline275 + 1
        t211 = inline277
        var inline270 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t211)
        _goml_runtime_core_string_println(inline270)
        var t212 int32
        var inline264 bool = true
        var inline266 int32
        if inline264 {
            t212 = 10
            var inline261 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t212)
            _goml_runtime_core_string_println(inline261)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline266 = 20
            var inline268 int32 = inline266 + 1
            t212 = inline268
            var inline261 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t212)
            _goml_runtime_core_string_println(inline261)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t233 string = _goml_runtime_core_int_to_string(self__67)
    return t233
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t236 string = _goml_runtime_core_int32_to_string(self__70)
    return t236
}

func main() {
    main0()
}
