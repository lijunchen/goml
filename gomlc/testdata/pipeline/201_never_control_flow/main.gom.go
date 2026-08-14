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
    var inline260 int = 0
    var inline261 *ref_int_x = ref__Ref_3int(inline260)
    count__2 = inline261
    Loop_loop200:
    for {
        var t201 int
        var inline258 int = ref_get__Ref_3int(count__2)
        t201 = inline258
        var t202 bool = t201 < 2
        if t202 {
            var t203 int
            var inline256 int = ref_get__Ref_3int(count__2)
            t203 = inline256
            var t204 int = t203 + 1
            ref_set__Ref_3int(count__2, t204)
            var t208 int
            var inline252 int = ref_get__Ref_3int(count__2)
            t208 = inline252
            var t209 bool
            var inline249 int = 1
            var inline250 bool = t208 == inline249
            t209 = inline250
            var jp206 int
            if t209 {
                continue
            } else {
                jp206 = 7
                var inline246 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp206)
                _goml_runtime_core_string_println(inline246)
                continue
            }
        } else {
            break Loop_loop200
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    var jp214 int
    if stop__4 {
        return struct{}{}
    } else {
        jp214 = 9
        var inline263 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp214)
        _goml_runtime_core_string_println(inline263)
        return struct{}{}
    }
}

func main0() struct{} {
    var t216 int32
    var inline278 bool = false
    var inline280 int32
    if inline278 {
        t216 = 10
        var inline275 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t216)
        _goml_runtime_core_string_println(inline275)
        var t217 int32
        var inline269 bool = true
        var inline271 int32
        if inline269 {
            t217 = 10
            var inline266 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t217)
            _goml_runtime_core_string_println(inline266)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline271 = 20
            var inline273 int32 = inline271 + 1
            t217 = inline273
            var inline266 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t217)
            _goml_runtime_core_string_println(inline266)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    } else {
        inline280 = 20
        var inline282 int32 = inline280 + 1
        t216 = inline282
        var inline275 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t216)
        _goml_runtime_core_string_println(inline275)
        var t217 int32
        var inline269 bool = true
        var inline271 int32
        if inline269 {
            t217 = 10
            var inline266 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t217)
            _goml_runtime_core_string_println(inline266)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline271 = 20
            var inline273 int32 = inline271 + 1
            t217 = inline273
            var inline266 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t217)
            _goml_runtime_core_string_println(inline266)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__67 int) string {
    var t238 string = _goml_runtime_core_int_to_string(self__67)
    return t238
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t241 string = _goml_runtime_core_int32_to_string(self__70)
    return t241
}

func main() {
    main0()
}
