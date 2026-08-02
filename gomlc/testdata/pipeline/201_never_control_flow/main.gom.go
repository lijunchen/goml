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
    var inline228 int = 0
    var inline229 *ref_int_x = ref__Ref_3int(inline228)
    count__2 = inline229
    Loop_loop168:
    for {
        var t169 int
        var inline226 int = ref_get__Ref_3int(count__2)
        t169 = inline226
        var t170 bool = t169 < 2
        if t170 {
            var t171 int
            var inline224 int = ref_get__Ref_3int(count__2)
            t171 = inline224
            var t172 int = t171 + 1
            ref_set__Ref_3int(count__2, t172)
            var t176 int
            var inline220 int = ref_get__Ref_3int(count__2)
            t176 = inline220
            var t177 bool
            var inline217 int = 1
            var inline218 bool = t176 == inline217
            t177 = inline218
            var jp174 int
            if t177 {
                continue
            } else {
                jp174 = 7
                var inline214 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp174)
                _goml_runtime_core_string_println(inline214)
                continue
            }
        } else {
            break Loop_loop168
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    var jp182 int
    if stop__4 {
        return struct{}{}
    } else {
        jp182 = 9
        var inline231 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp182)
        _goml_runtime_core_string_println(inline231)
        return struct{}{}
    }
}

func main0() struct{} {
    var t184 int32
    var inline246 bool = false
    var inline248 int32
    if inline246 {
        t184 = 10
        var inline243 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t184)
        _goml_runtime_core_string_println(inline243)
        var t185 int32
        var inline237 bool = true
        var inline239 int32
        if inline237 {
            t185 = 10
            var inline234 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t185)
            _goml_runtime_core_string_println(inline234)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline239 = 20
            var inline241 int32 = inline239 + 1
            t185 = inline241
            var inline234 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t185)
            _goml_runtime_core_string_println(inline234)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    } else {
        inline248 = 20
        var inline250 int32 = inline248 + 1
        t184 = inline250
        var inline243 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t184)
        _goml_runtime_core_string_println(inline243)
        var t185 int32
        var inline237 bool = true
        var inline239 int32
        if inline237 {
            t185 = 10
            var inline234 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t185)
            _goml_runtime_core_string_println(inline234)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline239 = 20
            var inline241 int32 = inline239 + 1
            t185 = inline241
            var inline234 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t185)
            _goml_runtime_core_string_println(inline234)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__40 int) string {
    var t206 string = _goml_runtime_core_int_to_string(self__40)
    return t206
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t209 string = _goml_runtime_core_int32_to_string(self__43)
    return t209
}

func main() {
    main0()
}
