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
    var jp187 int32
    switch flag__0 {
    case true:
        return 5
    case false:
        jp187 = 7
        var t188 int32 = jp187 + 1
        return t188
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t198 int32 = add_after_match(false)
    var inline270 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
    _goml_runtime_core_string_println(inline270)
    var t199 int32
    var inline264 bool = true
    var inline266 int32
    switch inline264 {
    case true:
        t199 = 5
        var inline261 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
        _goml_runtime_core_string_println(inline261)
        var t200 string
        var inline256 bool = false
        var inline258 int
        switch inline256 {
        case true:
            t200 = "early"
            var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
            _goml_runtime_core_string_println(inline253)
            var t201 string
            var inline248 bool = true
            var inline250 int
            switch inline248 {
            case true:
                t201 = "early"
                var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
                _goml_runtime_core_string_println(inline245)
                var t202 int32
                var inline241 bool = false
                var inline242 closure_env_f_0 = closure_env_f_0{}
                var inline243 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline242, inline241)
                t202 = inline243
                var inline238 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
                _goml_runtime_core_string_println(inline238)
                var t203 int32
                var inline234 bool = true
                var inline235 closure_env_f_0 = closure_env_f_0{}
                var inline236 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline235, inline234)
                t203 = inline236
                var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
                _goml_runtime_core_string_println(inline231)
                return struct{}{}
            case false:
                inline250 = 7
                var inline251 string = _goml_m_inherent_i_int_i_int_i_to__string(inline250)
                t201 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
                _goml_runtime_core_string_println(inline245)
                var t202 int32
                var inline241 bool = false
                var inline242 closure_env_f_0 = closure_env_f_0{}
                var inline243 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline242, inline241)
                t202 = inline243
                var inline238 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
                _goml_runtime_core_string_println(inline238)
                var t203 int32
                var inline234 bool = true
                var inline235 closure_env_f_0 = closure_env_f_0{}
                var inline236 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline235, inline234)
                t203 = inline236
                var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
                _goml_runtime_core_string_println(inline231)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline258 = 7
            var inline259 string = _goml_m_inherent_i_int_i_int_i_to__string(inline258)
            t200 = inline259
            var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
            _goml_runtime_core_string_println(inline253)
            var t201 string
            var inline248 bool = true
            var inline250 int
            switch inline248 {
            case true:
                t201 = "early"
                var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
                _goml_runtime_core_string_println(inline245)
                var t202 int32
                var inline241 bool = false
                var inline242 closure_env_f_0 = closure_env_f_0{}
                var inline243 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline242, inline241)
                t202 = inline243
                var inline238 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
                _goml_runtime_core_string_println(inline238)
                var t203 int32
                var inline234 bool = true
                var inline235 closure_env_f_0 = closure_env_f_0{}
                var inline236 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline235, inline234)
                t203 = inline236
                var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
                _goml_runtime_core_string_println(inline231)
                return struct{}{}
            case false:
                inline250 = 7
                var inline251 string = _goml_m_inherent_i_int_i_int_i_to__string(inline250)
                t201 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
                _goml_runtime_core_string_println(inline245)
                var t202 int32
                var inline241 bool = false
                var inline242 closure_env_f_0 = closure_env_f_0{}
                var inline243 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline242, inline241)
                t202 = inline243
                var inline238 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
                _goml_runtime_core_string_println(inline238)
                var t203 int32
                var inline234 bool = true
                var inline235 closure_env_f_0 = closure_env_f_0{}
                var inline236 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline235, inline234)
                t203 = inline236
                var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
                _goml_runtime_core_string_println(inline231)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case false:
        inline266 = 7
        var inline268 int32 = inline266 + 1
        t199 = inline268
        var inline261 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
        _goml_runtime_core_string_println(inline261)
        var t200 string
        var inline256 bool = false
        var inline258 int
        switch inline256 {
        case true:
            t200 = "early"
            var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
            _goml_runtime_core_string_println(inline253)
            var t201 string
            var inline248 bool = true
            var inline250 int
            switch inline248 {
            case true:
                t201 = "early"
                var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
                _goml_runtime_core_string_println(inline245)
                var t202 int32
                var inline241 bool = false
                var inline242 closure_env_f_0 = closure_env_f_0{}
                var inline243 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline242, inline241)
                t202 = inline243
                var inline238 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
                _goml_runtime_core_string_println(inline238)
                var t203 int32
                var inline234 bool = true
                var inline235 closure_env_f_0 = closure_env_f_0{}
                var inline236 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline235, inline234)
                t203 = inline236
                var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
                _goml_runtime_core_string_println(inline231)
                return struct{}{}
            case false:
                inline250 = 7
                var inline251 string = _goml_m_inherent_i_int_i_int_i_to__string(inline250)
                t201 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
                _goml_runtime_core_string_println(inline245)
                var t202 int32
                var inline241 bool = false
                var inline242 closure_env_f_0 = closure_env_f_0{}
                var inline243 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline242, inline241)
                t202 = inline243
                var inline238 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
                _goml_runtime_core_string_println(inline238)
                var t203 int32
                var inline234 bool = true
                var inline235 closure_env_f_0 = closure_env_f_0{}
                var inline236 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline235, inline234)
                t203 = inline236
                var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
                _goml_runtime_core_string_println(inline231)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline258 = 7
            var inline259 string = _goml_m_inherent_i_int_i_int_i_to__string(inline258)
            t200 = inline259
            var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t200)
            _goml_runtime_core_string_println(inline253)
            var t201 string
            var inline248 bool = true
            var inline250 int
            switch inline248 {
            case true:
                t201 = "early"
                var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
                _goml_runtime_core_string_println(inline245)
                var t202 int32
                var inline241 bool = false
                var inline242 closure_env_f_0 = closure_env_f_0{}
                var inline243 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline242, inline241)
                t202 = inline243
                var inline238 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
                _goml_runtime_core_string_println(inline238)
                var t203 int32
                var inline234 bool = true
                var inline235 closure_env_f_0 = closure_env_f_0{}
                var inline236 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline235, inline234)
                t203 = inline236
                var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
                _goml_runtime_core_string_println(inline231)
                return struct{}{}
            case false:
                inline250 = 7
                var inline251 string = _goml_m_inherent_i_int_i_int_i_to__string(inline250)
                t201 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
                _goml_runtime_core_string_println(inline245)
                var t202 int32
                var inline241 bool = false
                var inline242 closure_env_f_0 = closure_env_f_0{}
                var inline243 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline242, inline241)
                t202 = inline243
                var inline238 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t202)
                _goml_runtime_core_string_println(inline238)
                var t203 int32
                var inline234 bool = true
                var inline235 closure_env_f_0 = closure_env_f_0{}
                var inline236 int32 = _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline235, inline234)
                t203 = inline236
                var inline231 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t203)
                _goml_runtime_core_string_println(inline231)
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
    var t206 string = _goml_runtime_core_int_to_string(self__34)
    return t206
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t215 string = _goml_runtime_core_int32_to_string(self__72)
    return t215
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env183 closure_env_f_0, inner__4 bool) int32 {
    var jp221 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp221 = 4
        var t222 int32 = jp221 + 3
        return t222
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
