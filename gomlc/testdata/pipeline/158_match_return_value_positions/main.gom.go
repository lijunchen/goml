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
    var t194 int32 = add_after_match(false)
    var inline263 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t194)
    _goml_runtime_core_string_println(inline263)
    var t195 int32
    var inline257 bool = true
    var inline259 int32
    switch inline257 {
    case true:
        t195 = 5
        var inline254 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t195)
        _goml_runtime_core_string_println(inline254)
        var t196 string
        var inline249 bool = false
        var inline251 int
        switch inline249 {
        case true:
            t196 = "early"
            var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
            _goml_runtime_core_string_println(inline246)
            var t197 string
            var inline241 bool = true
            var inline243 int
            switch inline241 {
            case true:
                t197 = "early"
                var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
                _goml_runtime_core_string_println(inline238)
                var t198 int32
                var inline233 bool = false
                var inline234 closure_env_f_0 = closure_env_f_0{}
                var inline235 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline234, p0)
                }
                var inline236 int32 = inline235(inline233)
                t198 = inline236
                var inline230 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline230)
                var t199 int32
                var inline225 bool = true
                var inline226 closure_env_f_0 = closure_env_f_0{}
                var inline227 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline226, p0)
                }
                var inline228 int32 = inline227(inline225)
                t199 = inline228
                var inline222 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
                _goml_runtime_core_string_println(inline222)
                return struct{}{}
            case false:
                inline243 = 7
                var inline244 string = _goml_m_inherent_i_int_i_int_i_to__string(inline243)
                t197 = inline244
                var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
                _goml_runtime_core_string_println(inline238)
                var t198 int32
                var inline233 bool = false
                var inline234 closure_env_f_0 = closure_env_f_0{}
                var inline235 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline234, p0)
                }
                var inline236 int32 = inline235(inline233)
                t198 = inline236
                var inline230 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline230)
                var t199 int32
                var inline225 bool = true
                var inline226 closure_env_f_0 = closure_env_f_0{}
                var inline227 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline226, p0)
                }
                var inline228 int32 = inline227(inline225)
                t199 = inline228
                var inline222 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
                _goml_runtime_core_string_println(inline222)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline251 = 7
            var inline252 string = _goml_m_inherent_i_int_i_int_i_to__string(inline251)
            t196 = inline252
            var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
            _goml_runtime_core_string_println(inline246)
            var t197 string
            var inline241 bool = true
            var inline243 int
            switch inline241 {
            case true:
                t197 = "early"
                var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
                _goml_runtime_core_string_println(inline238)
                var t198 int32
                var inline233 bool = false
                var inline234 closure_env_f_0 = closure_env_f_0{}
                var inline235 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline234, p0)
                }
                var inline236 int32 = inline235(inline233)
                t198 = inline236
                var inline230 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline230)
                var t199 int32
                var inline225 bool = true
                var inline226 closure_env_f_0 = closure_env_f_0{}
                var inline227 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline226, p0)
                }
                var inline228 int32 = inline227(inline225)
                t199 = inline228
                var inline222 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
                _goml_runtime_core_string_println(inline222)
                return struct{}{}
            case false:
                inline243 = 7
                var inline244 string = _goml_m_inherent_i_int_i_int_i_to__string(inline243)
                t197 = inline244
                var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
                _goml_runtime_core_string_println(inline238)
                var t198 int32
                var inline233 bool = false
                var inline234 closure_env_f_0 = closure_env_f_0{}
                var inline235 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline234, p0)
                }
                var inline236 int32 = inline235(inline233)
                t198 = inline236
                var inline230 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline230)
                var t199 int32
                var inline225 bool = true
                var inline226 closure_env_f_0 = closure_env_f_0{}
                var inline227 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline226, p0)
                }
                var inline228 int32 = inline227(inline225)
                t199 = inline228
                var inline222 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
                _goml_runtime_core_string_println(inline222)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case false:
        inline259 = 7
        var inline261 int32 = inline259 + 1
        t195 = inline261
        var inline254 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t195)
        _goml_runtime_core_string_println(inline254)
        var t196 string
        var inline249 bool = false
        var inline251 int
        switch inline249 {
        case true:
            t196 = "early"
            var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
            _goml_runtime_core_string_println(inline246)
            var t197 string
            var inline241 bool = true
            var inline243 int
            switch inline241 {
            case true:
                t197 = "early"
                var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
                _goml_runtime_core_string_println(inline238)
                var t198 int32
                var inline233 bool = false
                var inline234 closure_env_f_0 = closure_env_f_0{}
                var inline235 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline234, p0)
                }
                var inline236 int32 = inline235(inline233)
                t198 = inline236
                var inline230 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline230)
                var t199 int32
                var inline225 bool = true
                var inline226 closure_env_f_0 = closure_env_f_0{}
                var inline227 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline226, p0)
                }
                var inline228 int32 = inline227(inline225)
                t199 = inline228
                var inline222 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
                _goml_runtime_core_string_println(inline222)
                return struct{}{}
            case false:
                inline243 = 7
                var inline244 string = _goml_m_inherent_i_int_i_int_i_to__string(inline243)
                t197 = inline244
                var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
                _goml_runtime_core_string_println(inline238)
                var t198 int32
                var inline233 bool = false
                var inline234 closure_env_f_0 = closure_env_f_0{}
                var inline235 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline234, p0)
                }
                var inline236 int32 = inline235(inline233)
                t198 = inline236
                var inline230 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline230)
                var t199 int32
                var inline225 bool = true
                var inline226 closure_env_f_0 = closure_env_f_0{}
                var inline227 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline226, p0)
                }
                var inline228 int32 = inline227(inline225)
                t199 = inline228
                var inline222 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
                _goml_runtime_core_string_println(inline222)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline251 = 7
            var inline252 string = _goml_m_inherent_i_int_i_int_i_to__string(inline251)
            t196 = inline252
            var inline246 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t196)
            _goml_runtime_core_string_println(inline246)
            var t197 string
            var inline241 bool = true
            var inline243 int
            switch inline241 {
            case true:
                t197 = "early"
                var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
                _goml_runtime_core_string_println(inline238)
                var t198 int32
                var inline233 bool = false
                var inline234 closure_env_f_0 = closure_env_f_0{}
                var inline235 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline234, p0)
                }
                var inline236 int32 = inline235(inline233)
                t198 = inline236
                var inline230 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline230)
                var t199 int32
                var inline225 bool = true
                var inline226 closure_env_f_0 = closure_env_f_0{}
                var inline227 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline226, p0)
                }
                var inline228 int32 = inline227(inline225)
                t199 = inline228
                var inline222 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
                _goml_runtime_core_string_println(inline222)
                return struct{}{}
            case false:
                inline243 = 7
                var inline244 string = _goml_m_inherent_i_int_i_int_i_to__string(inline243)
                t197 = inline244
                var inline238 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t197)
                _goml_runtime_core_string_println(inline238)
                var t198 int32
                var inline233 bool = false
                var inline234 closure_env_f_0 = closure_env_f_0{}
                var inline235 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline234, p0)
                }
                var inline236 int32 = inline235(inline233)
                t198 = inline236
                var inline230 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t198)
                _goml_runtime_core_string_println(inline230)
                var t199 int32
                var inline225 bool = true
                var inline226 closure_env_f_0 = closure_env_f_0{}
                var inline227 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline226, p0)
                }
                var inline228 int32 = inline227(inline225)
                t199 = inline228
                var inline222 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t199)
                _goml_runtime_core_string_println(inline222)
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
    var t202 string = _goml_runtime_core_int_to_string(self__34)
    return t202
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__72 int32) string {
    var t211 string = _goml_runtime_core_int32_to_string(self__72)
    return t211
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env178 closure_env_f_0, inner__4 bool) int32 {
    var jp217 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp217 = 4
        var t218 int32 = jp217 + 3
        return t218
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
