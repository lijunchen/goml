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

type closure_env_f_0 struct {}

func add_after_match(flag__0 bool) int32 {
    var jp165 int32
    switch flag__0 {
    case true:
        return 5
    case false:
        jp165 = 7
        var t166 int32 = jp165 + 1
        return t166
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t176 int32 = add_after_match(false)
    var inline248 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t176)
    _goml_runtime_core_string_println(inline248)
    var t177 int32
    var inline242 bool = true
    var inline244 int32
    switch inline242 {
    case true:
        t177 = 5
        var inline239 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t177)
        _goml_runtime_core_string_println(inline239)
        var t178 string
        var inline234 bool = false
        var inline236 int
        switch inline234 {
        case true:
            t178 = "early"
            var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
            _goml_runtime_core_string_println(inline231)
            var t179 string
            var inline226 bool = true
            var inline228 int
            switch inline226 {
            case true:
                t179 = "early"
                var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
                _goml_runtime_core_string_println(inline223)
                var t180 int32
                var inline219 bool = false
                var inline220 closure_env_f_0 = closure_env_f_0{}
                var inline221 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline220, inline219)
                t180 = inline221
                var inline216 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t180)
                _goml_runtime_core_string_println(inline216)
                var t181 int32
                var inline212 bool = true
                var inline213 closure_env_f_0 = closure_env_f_0{}
                var inline214 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline213, inline212)
                t181 = inline214
                var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t181)
                _goml_runtime_core_string_println(inline209)
                return struct{}{}
            case false:
                inline228 = 7
                var inline229 string = _goml_m_inherent_i_int_i_int_i_to__string(inline228)
                t179 = inline229
                var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
                _goml_runtime_core_string_println(inline223)
                var t180 int32
                var inline219 bool = false
                var inline220 closure_env_f_0 = closure_env_f_0{}
                var inline221 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline220, inline219)
                t180 = inline221
                var inline216 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t180)
                _goml_runtime_core_string_println(inline216)
                var t181 int32
                var inline212 bool = true
                var inline213 closure_env_f_0 = closure_env_f_0{}
                var inline214 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline213, inline212)
                t181 = inline214
                var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t181)
                _goml_runtime_core_string_println(inline209)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline236 = 7
            var inline237 string = _goml_m_inherent_i_int_i_int_i_to__string(inline236)
            t178 = inline237
            var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
            _goml_runtime_core_string_println(inline231)
            var t179 string
            var inline226 bool = true
            var inline228 int
            switch inline226 {
            case true:
                t179 = "early"
                var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
                _goml_runtime_core_string_println(inline223)
                var t180 int32
                var inline219 bool = false
                var inline220 closure_env_f_0 = closure_env_f_0{}
                var inline221 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline220, inline219)
                t180 = inline221
                var inline216 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t180)
                _goml_runtime_core_string_println(inline216)
                var t181 int32
                var inline212 bool = true
                var inline213 closure_env_f_0 = closure_env_f_0{}
                var inline214 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline213, inline212)
                t181 = inline214
                var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t181)
                _goml_runtime_core_string_println(inline209)
                return struct{}{}
            case false:
                inline228 = 7
                var inline229 string = _goml_m_inherent_i_int_i_int_i_to__string(inline228)
                t179 = inline229
                var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
                _goml_runtime_core_string_println(inline223)
                var t180 int32
                var inline219 bool = false
                var inline220 closure_env_f_0 = closure_env_f_0{}
                var inline221 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline220, inline219)
                t180 = inline221
                var inline216 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t180)
                _goml_runtime_core_string_println(inline216)
                var t181 int32
                var inline212 bool = true
                var inline213 closure_env_f_0 = closure_env_f_0{}
                var inline214 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline213, inline212)
                t181 = inline214
                var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t181)
                _goml_runtime_core_string_println(inline209)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case false:
        inline244 = 7
        var inline246 int32 = inline244 + 1
        t177 = inline246
        var inline239 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t177)
        _goml_runtime_core_string_println(inline239)
        var t178 string
        var inline234 bool = false
        var inline236 int
        switch inline234 {
        case true:
            t178 = "early"
            var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
            _goml_runtime_core_string_println(inline231)
            var t179 string
            var inline226 bool = true
            var inline228 int
            switch inline226 {
            case true:
                t179 = "early"
                var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
                _goml_runtime_core_string_println(inline223)
                var t180 int32
                var inline219 bool = false
                var inline220 closure_env_f_0 = closure_env_f_0{}
                var inline221 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline220, inline219)
                t180 = inline221
                var inline216 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t180)
                _goml_runtime_core_string_println(inline216)
                var t181 int32
                var inline212 bool = true
                var inline213 closure_env_f_0 = closure_env_f_0{}
                var inline214 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline213, inline212)
                t181 = inline214
                var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t181)
                _goml_runtime_core_string_println(inline209)
                return struct{}{}
            case false:
                inline228 = 7
                var inline229 string = _goml_m_inherent_i_int_i_int_i_to__string(inline228)
                t179 = inline229
                var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
                _goml_runtime_core_string_println(inline223)
                var t180 int32
                var inline219 bool = false
                var inline220 closure_env_f_0 = closure_env_f_0{}
                var inline221 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline220, inline219)
                t180 = inline221
                var inline216 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t180)
                _goml_runtime_core_string_println(inline216)
                var t181 int32
                var inline212 bool = true
                var inline213 closure_env_f_0 = closure_env_f_0{}
                var inline214 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline213, inline212)
                t181 = inline214
                var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t181)
                _goml_runtime_core_string_println(inline209)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline236 = 7
            var inline237 string = _goml_m_inherent_i_int_i_int_i_to__string(inline236)
            t178 = inline237
            var inline231 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t178)
            _goml_runtime_core_string_println(inline231)
            var t179 string
            var inline226 bool = true
            var inline228 int
            switch inline226 {
            case true:
                t179 = "early"
                var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
                _goml_runtime_core_string_println(inline223)
                var t180 int32
                var inline219 bool = false
                var inline220 closure_env_f_0 = closure_env_f_0{}
                var inline221 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline220, inline219)
                t180 = inline221
                var inline216 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t180)
                _goml_runtime_core_string_println(inline216)
                var t181 int32
                var inline212 bool = true
                var inline213 closure_env_f_0 = closure_env_f_0{}
                var inline214 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline213, inline212)
                t181 = inline214
                var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t181)
                _goml_runtime_core_string_println(inline209)
                return struct{}{}
            case false:
                inline228 = 7
                var inline229 string = _goml_m_inherent_i_int_i_int_i_to__string(inline228)
                t179 = inline229
                var inline223 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t179)
                _goml_runtime_core_string_println(inline223)
                var t180 int32
                var inline219 bool = false
                var inline220 closure_env_f_0 = closure_env_f_0{}
                var inline221 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline220, inline219)
                t180 = inline221
                var inline216 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t180)
                _goml_runtime_core_string_println(inline216)
                var t181 int32
                var inline212 bool = true
                var inline213 closure_env_f_0 = closure_env_f_0{}
                var inline214 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline213, inline212)
                t181 = inline214
                var inline209 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t181)
                _goml_runtime_core_string_println(inline209)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    default:
        panic("non-exhaustive match")
    }
}

func _goml_m_inherent_i_int_i_int_i_to__string(self__5 int) string {
    var t184 string = _goml_runtime_core_int_to_string(self__5)
    return t184
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__43 int32) string {
    var t193 string = _goml_runtime_core_int32_to_string(self__43)
    return t193
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env161 closure_env_f_0, inner__4 bool) int32 {
    var jp199 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp199 = 4
        var t200 int32 = jp199 + 3
        return t200
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
