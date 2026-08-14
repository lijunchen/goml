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
    var jp197 int32
    switch flag__0 {
    case true:
        return 5
    case false:
        jp197 = 7
        var t198 int32 = jp197 + 1
        return t198
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t209 int32 = add_after_match(false)
    var inline278 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
    _goml_runtime_core_string_println(inline278)
    var t210 int32
    var inline272 bool = true
    var inline274 int32
    switch inline272 {
    case true:
        t210 = 5
        var inline269 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t210)
        _goml_runtime_core_string_println(inline269)
        var t211 string
        var inline264 bool = false
        var inline266 int
        switch inline264 {
        case true:
            t211 = "early"
            var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline261)
            var t212 string
            var inline256 bool = true
            var inline258 int
            switch inline256 {
            case true:
                t212 = "early"
                var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
                _goml_runtime_core_string_println(inline253)
                var t213 int32
                var inline248 bool = false
                var inline249 closure_env_f_0 = closure_env_f_0{}
                var inline250 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline249, p0)
                }
                var inline251 int32 = inline250(inline248)
                t213 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
                _goml_runtime_core_string_println(inline245)
                var t214 int32
                var inline240 bool = true
                var inline241 closure_env_f_0 = closure_env_f_0{}
                var inline242 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline241, p0)
                }
                var inline243 int32 = inline242(inline240)
                t214 = inline243
                var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t214)
                _goml_runtime_core_string_println(inline237)
                return struct{}{}
            case false:
                inline258 = 7
                var inline259 string = _goml_m_inherent_i_int_i_int_i_to__string(inline258)
                t212 = inline259
                var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
                _goml_runtime_core_string_println(inline253)
                var t213 int32
                var inline248 bool = false
                var inline249 closure_env_f_0 = closure_env_f_0{}
                var inline250 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline249, p0)
                }
                var inline251 int32 = inline250(inline248)
                t213 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
                _goml_runtime_core_string_println(inline245)
                var t214 int32
                var inline240 bool = true
                var inline241 closure_env_f_0 = closure_env_f_0{}
                var inline242 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline241, p0)
                }
                var inline243 int32 = inline242(inline240)
                t214 = inline243
                var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t214)
                _goml_runtime_core_string_println(inline237)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline266 = 7
            var inline267 string = _goml_m_inherent_i_int_i_int_i_to__string(inline266)
            t211 = inline267
            var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline261)
            var t212 string
            var inline256 bool = true
            var inline258 int
            switch inline256 {
            case true:
                t212 = "early"
                var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
                _goml_runtime_core_string_println(inline253)
                var t213 int32
                var inline248 bool = false
                var inline249 closure_env_f_0 = closure_env_f_0{}
                var inline250 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline249, p0)
                }
                var inline251 int32 = inline250(inline248)
                t213 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
                _goml_runtime_core_string_println(inline245)
                var t214 int32
                var inline240 bool = true
                var inline241 closure_env_f_0 = closure_env_f_0{}
                var inline242 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline241, p0)
                }
                var inline243 int32 = inline242(inline240)
                t214 = inline243
                var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t214)
                _goml_runtime_core_string_println(inline237)
                return struct{}{}
            case false:
                inline258 = 7
                var inline259 string = _goml_m_inherent_i_int_i_int_i_to__string(inline258)
                t212 = inline259
                var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
                _goml_runtime_core_string_println(inline253)
                var t213 int32
                var inline248 bool = false
                var inline249 closure_env_f_0 = closure_env_f_0{}
                var inline250 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline249, p0)
                }
                var inline251 int32 = inline250(inline248)
                t213 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
                _goml_runtime_core_string_println(inline245)
                var t214 int32
                var inline240 bool = true
                var inline241 closure_env_f_0 = closure_env_f_0{}
                var inline242 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline241, p0)
                }
                var inline243 int32 = inline242(inline240)
                t214 = inline243
                var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t214)
                _goml_runtime_core_string_println(inline237)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case false:
        inline274 = 7
        var inline276 int32 = inline274 + 1
        t210 = inline276
        var inline269 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t210)
        _goml_runtime_core_string_println(inline269)
        var t211 string
        var inline264 bool = false
        var inline266 int
        switch inline264 {
        case true:
            t211 = "early"
            var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline261)
            var t212 string
            var inline256 bool = true
            var inline258 int
            switch inline256 {
            case true:
                t212 = "early"
                var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
                _goml_runtime_core_string_println(inline253)
                var t213 int32
                var inline248 bool = false
                var inline249 closure_env_f_0 = closure_env_f_0{}
                var inline250 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline249, p0)
                }
                var inline251 int32 = inline250(inline248)
                t213 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
                _goml_runtime_core_string_println(inline245)
                var t214 int32
                var inline240 bool = true
                var inline241 closure_env_f_0 = closure_env_f_0{}
                var inline242 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline241, p0)
                }
                var inline243 int32 = inline242(inline240)
                t214 = inline243
                var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t214)
                _goml_runtime_core_string_println(inline237)
                return struct{}{}
            case false:
                inline258 = 7
                var inline259 string = _goml_m_inherent_i_int_i_int_i_to__string(inline258)
                t212 = inline259
                var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
                _goml_runtime_core_string_println(inline253)
                var t213 int32
                var inline248 bool = false
                var inline249 closure_env_f_0 = closure_env_f_0{}
                var inline250 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline249, p0)
                }
                var inline251 int32 = inline250(inline248)
                t213 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
                _goml_runtime_core_string_println(inline245)
                var t214 int32
                var inline240 bool = true
                var inline241 closure_env_f_0 = closure_env_f_0{}
                var inline242 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline241, p0)
                }
                var inline243 int32 = inline242(inline240)
                t214 = inline243
                var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t214)
                _goml_runtime_core_string_println(inline237)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline266 = 7
            var inline267 string = _goml_m_inherent_i_int_i_int_i_to__string(inline266)
            t211 = inline267
            var inline261 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t211)
            _goml_runtime_core_string_println(inline261)
            var t212 string
            var inline256 bool = true
            var inline258 int
            switch inline256 {
            case true:
                t212 = "early"
                var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
                _goml_runtime_core_string_println(inline253)
                var t213 int32
                var inline248 bool = false
                var inline249 closure_env_f_0 = closure_env_f_0{}
                var inline250 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline249, p0)
                }
                var inline251 int32 = inline250(inline248)
                t213 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
                _goml_runtime_core_string_println(inline245)
                var t214 int32
                var inline240 bool = true
                var inline241 closure_env_f_0 = closure_env_f_0{}
                var inline242 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline241, p0)
                }
                var inline243 int32 = inline242(inline240)
                t214 = inline243
                var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t214)
                _goml_runtime_core_string_println(inline237)
                return struct{}{}
            case false:
                inline258 = 7
                var inline259 string = _goml_m_inherent_i_int_i_int_i_to__string(inline258)
                t212 = inline259
                var inline253 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t212)
                _goml_runtime_core_string_println(inline253)
                var t213 int32
                var inline248 bool = false
                var inline249 closure_env_f_0 = closure_env_f_0{}
                var inline250 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline249, p0)
                }
                var inline251 int32 = inline250(inline248)
                t213 = inline251
                var inline245 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t213)
                _goml_runtime_core_string_println(inline245)
                var t214 int32
                var inline240 bool = true
                var inline241 closure_env_f_0 = closure_env_f_0{}
                var inline242 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline241, p0)
                }
                var inline243 int32 = inline242(inline240)
                t214 = inline243
                var inline237 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t214)
                _goml_runtime_core_string_println(inline237)
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

func _goml_m_inherent_i_int_i_int_i_to__string(self__32 int) string {
    var t217 string = _goml_runtime_core_int_to_string(self__32)
    return t217
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t226 string = _goml_runtime_core_int32_to_string(self__70)
    return t226
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env193 closure_env_f_0, inner__4 bool) int32 {
    var jp232 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp232 = 4
        var t233 int32 = jp232 + 3
        return t233
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
