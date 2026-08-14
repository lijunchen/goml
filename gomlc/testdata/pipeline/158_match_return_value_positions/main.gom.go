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
    var jp192 int32
    switch flag__0 {
    case true:
        return 5
    case false:
        jp192 = 7
        var t193 int32 = jp192 + 1
        return t193
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t204 int32 = add_after_match(false)
    var inline273 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t204)
    _goml_runtime_core_string_println(inline273)
    var t205 int32
    var inline267 bool = true
    var inline269 int32
    switch inline267 {
    case true:
        t205 = 5
        var inline264 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t205)
        _goml_runtime_core_string_println(inline264)
        var t206 string
        var inline259 bool = false
        var inline261 int
        switch inline259 {
        case true:
            t206 = "early"
            var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline256)
            var t207 string
            var inline251 bool = true
            var inline253 int
            switch inline251 {
            case true:
                t207 = "early"
                var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
                _goml_runtime_core_string_println(inline248)
                var t208 int32
                var inline243 bool = false
                var inline244 closure_env_f_0 = closure_env_f_0{}
                var inline245 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline244, p0)
                }
                var inline246 int32 = inline245(inline243)
                t208 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
                _goml_runtime_core_string_println(inline240)
                var t209 int32
                var inline235 bool = true
                var inline236 closure_env_f_0 = closure_env_f_0{}
                var inline237 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline236, p0)
                }
                var inline238 int32 = inline237(inline235)
                t209 = inline238
                var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
                _goml_runtime_core_string_println(inline232)
                return struct{}{}
            case false:
                inline253 = 7
                var inline254 string = _goml_m_inherent_i_int_i_int_i_to__string(inline253)
                t207 = inline254
                var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
                _goml_runtime_core_string_println(inline248)
                var t208 int32
                var inline243 bool = false
                var inline244 closure_env_f_0 = closure_env_f_0{}
                var inline245 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline244, p0)
                }
                var inline246 int32 = inline245(inline243)
                t208 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
                _goml_runtime_core_string_println(inline240)
                var t209 int32
                var inline235 bool = true
                var inline236 closure_env_f_0 = closure_env_f_0{}
                var inline237 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline236, p0)
                }
                var inline238 int32 = inline237(inline235)
                t209 = inline238
                var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
                _goml_runtime_core_string_println(inline232)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline261 = 7
            var inline262 string = _goml_m_inherent_i_int_i_int_i_to__string(inline261)
            t206 = inline262
            var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline256)
            var t207 string
            var inline251 bool = true
            var inline253 int
            switch inline251 {
            case true:
                t207 = "early"
                var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
                _goml_runtime_core_string_println(inline248)
                var t208 int32
                var inline243 bool = false
                var inline244 closure_env_f_0 = closure_env_f_0{}
                var inline245 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline244, p0)
                }
                var inline246 int32 = inline245(inline243)
                t208 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
                _goml_runtime_core_string_println(inline240)
                var t209 int32
                var inline235 bool = true
                var inline236 closure_env_f_0 = closure_env_f_0{}
                var inline237 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline236, p0)
                }
                var inline238 int32 = inline237(inline235)
                t209 = inline238
                var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
                _goml_runtime_core_string_println(inline232)
                return struct{}{}
            case false:
                inline253 = 7
                var inline254 string = _goml_m_inherent_i_int_i_int_i_to__string(inline253)
                t207 = inline254
                var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
                _goml_runtime_core_string_println(inline248)
                var t208 int32
                var inline243 bool = false
                var inline244 closure_env_f_0 = closure_env_f_0{}
                var inline245 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline244, p0)
                }
                var inline246 int32 = inline245(inline243)
                t208 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
                _goml_runtime_core_string_println(inline240)
                var t209 int32
                var inline235 bool = true
                var inline236 closure_env_f_0 = closure_env_f_0{}
                var inline237 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline236, p0)
                }
                var inline238 int32 = inline237(inline235)
                t209 = inline238
                var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
                _goml_runtime_core_string_println(inline232)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case false:
        inline269 = 7
        var inline271 int32 = inline269 + 1
        t205 = inline271
        var inline264 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t205)
        _goml_runtime_core_string_println(inline264)
        var t206 string
        var inline259 bool = false
        var inline261 int
        switch inline259 {
        case true:
            t206 = "early"
            var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline256)
            var t207 string
            var inline251 bool = true
            var inline253 int
            switch inline251 {
            case true:
                t207 = "early"
                var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
                _goml_runtime_core_string_println(inline248)
                var t208 int32
                var inline243 bool = false
                var inline244 closure_env_f_0 = closure_env_f_0{}
                var inline245 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline244, p0)
                }
                var inline246 int32 = inline245(inline243)
                t208 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
                _goml_runtime_core_string_println(inline240)
                var t209 int32
                var inline235 bool = true
                var inline236 closure_env_f_0 = closure_env_f_0{}
                var inline237 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline236, p0)
                }
                var inline238 int32 = inline237(inline235)
                t209 = inline238
                var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
                _goml_runtime_core_string_println(inline232)
                return struct{}{}
            case false:
                inline253 = 7
                var inline254 string = _goml_m_inherent_i_int_i_int_i_to__string(inline253)
                t207 = inline254
                var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
                _goml_runtime_core_string_println(inline248)
                var t208 int32
                var inline243 bool = false
                var inline244 closure_env_f_0 = closure_env_f_0{}
                var inline245 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline244, p0)
                }
                var inline246 int32 = inline245(inline243)
                t208 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
                _goml_runtime_core_string_println(inline240)
                var t209 int32
                var inline235 bool = true
                var inline236 closure_env_f_0 = closure_env_f_0{}
                var inline237 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline236, p0)
                }
                var inline238 int32 = inline237(inline235)
                t209 = inline238
                var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
                _goml_runtime_core_string_println(inline232)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline261 = 7
            var inline262 string = _goml_m_inherent_i_int_i_int_i_to__string(inline261)
            t206 = inline262
            var inline256 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t206)
            _goml_runtime_core_string_println(inline256)
            var t207 string
            var inline251 bool = true
            var inline253 int
            switch inline251 {
            case true:
                t207 = "early"
                var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
                _goml_runtime_core_string_println(inline248)
                var t208 int32
                var inline243 bool = false
                var inline244 closure_env_f_0 = closure_env_f_0{}
                var inline245 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline244, p0)
                }
                var inline246 int32 = inline245(inline243)
                t208 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
                _goml_runtime_core_string_println(inline240)
                var t209 int32
                var inline235 bool = true
                var inline236 closure_env_f_0 = closure_env_f_0{}
                var inline237 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline236, p0)
                }
                var inline238 int32 = inline237(inline235)
                t209 = inline238
                var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
                _goml_runtime_core_string_println(inline232)
                return struct{}{}
            case false:
                inline253 = 7
                var inline254 string = _goml_m_inherent_i_int_i_int_i_to__string(inline253)
                t207 = inline254
                var inline248 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t207)
                _goml_runtime_core_string_println(inline248)
                var t208 int32
                var inline243 bool = false
                var inline244 closure_env_f_0 = closure_env_f_0{}
                var inline245 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline244, p0)
                }
                var inline246 int32 = inline245(inline243)
                t208 = inline246
                var inline240 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t208)
                _goml_runtime_core_string_println(inline240)
                var t209 int32
                var inline235 bool = true
                var inline236 closure_env_f_0 = closure_env_f_0{}
                var inline237 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline236, p0)
                }
                var inline238 int32 = inline237(inline235)
                t209 = inline238
                var inline232 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t209)
                _goml_runtime_core_string_println(inline232)
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
    var t212 string = _goml_runtime_core_int_to_string(self__32)
    return t212
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__70 int32) string {
    var t221 string = _goml_runtime_core_int32_to_string(self__70)
    return t221
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env188 closure_env_f_0, inner__4 bool) int32 {
    var jp227 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp227 = 4
        var t228 int32 = jp227 + 3
        return t228
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
