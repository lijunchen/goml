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
    var jp182 int32
    switch flag__0 {
    case true:
        return 5
    case false:
        jp182 = 7
        var t183 int32 = jp182 + 1
        return t183
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t193 int32 = add_after_match(false)
    var inline265 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t193)
    _goml_runtime_core_string_println(inline265)
    var t194 int32
    var inline259 bool = true
    var inline261 int32
    switch inline259 {
    case true:
        t194 = 5
        var inline256 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t194)
        _goml_runtime_core_string_println(inline256)
        var t195 string
        var inline251 bool = false
        var inline253 int
        switch inline251 {
        case true:
            t195 = "early"
            var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
            _goml_runtime_core_string_println(inline248)
            var t196 string
            var inline243 bool = true
            var inline245 int
            switch inline243 {
            case true:
                t196 = "early"
                var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
                _goml_runtime_core_string_println(inline240)
                var t197 int32
                var inline236 bool = false
                var inline237 closure_env_f_0 = closure_env_f_0{}
                var inline238 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline237, inline236)
                t197 = inline238
                var inline233 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t197)
                _goml_runtime_core_string_println(inline233)
                var t198 int32
                var inline229 bool = true
                var inline230 closure_env_f_0 = closure_env_f_0{}
                var inline231 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline230, inline229)
                t198 = inline231
                var inline226 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline226)
                return struct{}{}
            case false:
                inline245 = 7
                var inline246 string = _goml_m_inherent_i_int_i_int_i_to__string(inline245)
                t196 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
                _goml_runtime_core_string_println(inline240)
                var t197 int32
                var inline236 bool = false
                var inline237 closure_env_f_0 = closure_env_f_0{}
                var inline238 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline237, inline236)
                t197 = inline238
                var inline233 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t197)
                _goml_runtime_core_string_println(inline233)
                var t198 int32
                var inline229 bool = true
                var inline230 closure_env_f_0 = closure_env_f_0{}
                var inline231 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline230, inline229)
                t198 = inline231
                var inline226 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline226)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline253 = 7
            var inline254 string = _goml_m_inherent_i_int_i_int_i_to__string(inline253)
            t195 = inline254
            var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
            _goml_runtime_core_string_println(inline248)
            var t196 string
            var inline243 bool = true
            var inline245 int
            switch inline243 {
            case true:
                t196 = "early"
                var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
                _goml_runtime_core_string_println(inline240)
                var t197 int32
                var inline236 bool = false
                var inline237 closure_env_f_0 = closure_env_f_0{}
                var inline238 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline237, inline236)
                t197 = inline238
                var inline233 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t197)
                _goml_runtime_core_string_println(inline233)
                var t198 int32
                var inline229 bool = true
                var inline230 closure_env_f_0 = closure_env_f_0{}
                var inline231 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline230, inline229)
                t198 = inline231
                var inline226 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline226)
                return struct{}{}
            case false:
                inline245 = 7
                var inline246 string = _goml_m_inherent_i_int_i_int_i_to__string(inline245)
                t196 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
                _goml_runtime_core_string_println(inline240)
                var t197 int32
                var inline236 bool = false
                var inline237 closure_env_f_0 = closure_env_f_0{}
                var inline238 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline237, inline236)
                t197 = inline238
                var inline233 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t197)
                _goml_runtime_core_string_println(inline233)
                var t198 int32
                var inline229 bool = true
                var inline230 closure_env_f_0 = closure_env_f_0{}
                var inline231 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline230, inline229)
                t198 = inline231
                var inline226 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline226)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case false:
        inline261 = 7
        var inline263 int32 = inline261 + 1
        t194 = inline263
        var inline256 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t194)
        _goml_runtime_core_string_println(inline256)
        var t195 string
        var inline251 bool = false
        var inline253 int
        switch inline251 {
        case true:
            t195 = "early"
            var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
            _goml_runtime_core_string_println(inline248)
            var t196 string
            var inline243 bool = true
            var inline245 int
            switch inline243 {
            case true:
                t196 = "early"
                var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
                _goml_runtime_core_string_println(inline240)
                var t197 int32
                var inline236 bool = false
                var inline237 closure_env_f_0 = closure_env_f_0{}
                var inline238 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline237, inline236)
                t197 = inline238
                var inline233 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t197)
                _goml_runtime_core_string_println(inline233)
                var t198 int32
                var inline229 bool = true
                var inline230 closure_env_f_0 = closure_env_f_0{}
                var inline231 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline230, inline229)
                t198 = inline231
                var inline226 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline226)
                return struct{}{}
            case false:
                inline245 = 7
                var inline246 string = _goml_m_inherent_i_int_i_int_i_to__string(inline245)
                t196 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
                _goml_runtime_core_string_println(inline240)
                var t197 int32
                var inline236 bool = false
                var inline237 closure_env_f_0 = closure_env_f_0{}
                var inline238 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline237, inline236)
                t197 = inline238
                var inline233 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t197)
                _goml_runtime_core_string_println(inline233)
                var t198 int32
                var inline229 bool = true
                var inline230 closure_env_f_0 = closure_env_f_0{}
                var inline231 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline230, inline229)
                t198 = inline231
                var inline226 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline226)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline253 = 7
            var inline254 string = _goml_m_inherent_i_int_i_int_i_to__string(inline253)
            t195 = inline254
            var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t195)
            _goml_runtime_core_string_println(inline248)
            var t196 string
            var inline243 bool = true
            var inline245 int
            switch inline243 {
            case true:
                t196 = "early"
                var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
                _goml_runtime_core_string_println(inline240)
                var t197 int32
                var inline236 bool = false
                var inline237 closure_env_f_0 = closure_env_f_0{}
                var inline238 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline237, inline236)
                t197 = inline238
                var inline233 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t197)
                _goml_runtime_core_string_println(inline233)
                var t198 int32
                var inline229 bool = true
                var inline230 closure_env_f_0 = closure_env_f_0{}
                var inline231 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline230, inline229)
                t198 = inline231
                var inline226 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline226)
                return struct{}{}
            case false:
                inline245 = 7
                var inline246 string = _goml_m_inherent_i_int_i_int_i_to__string(inline245)
                t196 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
                _goml_runtime_core_string_println(inline240)
                var t197 int32
                var inline236 bool = false
                var inline237 closure_env_f_0 = closure_env_f_0{}
                var inline238 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline237, inline236)
                t197 = inline238
                var inline233 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t197)
                _goml_runtime_core_string_println(inline233)
                var t198 int32
                var inline229 bool = true
                var inline230 closure_env_f_0 = closure_env_f_0{}
                var inline231 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline230, inline229)
                t198 = inline231
                var inline226 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline226)
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

func _goml_m_inherent_i_int_i_int_i_to__string(self__34 int) string {
    var t201 string = _goml_runtime_core_int_to_string(self__34)
    return t201
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t210 string = _goml_runtime_core_int32_to_string(self__72)
    return t210
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env178 closure_env_f_0, inner__4 bool) int32 {
    var jp216 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp216 = 4
        var t217 int32 = jp216 + 3
        return t217
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
