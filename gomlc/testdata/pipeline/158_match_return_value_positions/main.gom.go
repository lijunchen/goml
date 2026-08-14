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

type Ordering int32

func add_after_match(flag__0 bool) int32 {
    var jp418 int32
    switch flag__0 {
    case true:
        return 5
    case false:
        jp418 = 7
        var t419 int32 = jp418 + 1
        return t419
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t430 int32 = add_after_match(false)
    var inline499 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t430)
    _goml_runtime_core_string_println(inline499)
    var t431 int32
    var inline493 bool = true
    var inline495 int32
    switch inline493 {
    case true:
        t431 = 5
        var inline490 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t431)
        _goml_runtime_core_string_println(inline490)
        var t432 string
        var inline485 bool = false
        var inline487 int
        switch inline485 {
        case true:
            t432 = "early"
            var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
            _goml_runtime_core_string_println(inline482)
            var t433 string
            var inline477 bool = true
            var inline479 int
            switch inline477 {
            case true:
                t433 = "early"
                var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
                _goml_runtime_core_string_println(inline474)
                var t434 int32
                var inline469 bool = false
                var inline470 closure_env_f_0 = closure_env_f_0{}
                var inline471 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline470, p0)
                }
                var inline472 int32 = inline471(inline469)
                t434 = inline472
                var inline466 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t434)
                _goml_runtime_core_string_println(inline466)
                var t435 int32
                var inline461 bool = true
                var inline462 closure_env_f_0 = closure_env_f_0{}
                var inline463 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline462, p0)
                }
                var inline464 int32 = inline463(inline461)
                t435 = inline464
                var inline458 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t435)
                _goml_runtime_core_string_println(inline458)
                return struct{}{}
            case false:
                inline479 = 7
                var inline480 string = _goml_m_inherent_i_int_i_int_i_to__string(inline479)
                t433 = inline480
                var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
                _goml_runtime_core_string_println(inline474)
                var t434 int32
                var inline469 bool = false
                var inline470 closure_env_f_0 = closure_env_f_0{}
                var inline471 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline470, p0)
                }
                var inline472 int32 = inline471(inline469)
                t434 = inline472
                var inline466 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t434)
                _goml_runtime_core_string_println(inline466)
                var t435 int32
                var inline461 bool = true
                var inline462 closure_env_f_0 = closure_env_f_0{}
                var inline463 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline462, p0)
                }
                var inline464 int32 = inline463(inline461)
                t435 = inline464
                var inline458 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t435)
                _goml_runtime_core_string_println(inline458)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline487 = 7
            var inline488 string = _goml_m_inherent_i_int_i_int_i_to__string(inline487)
            t432 = inline488
            var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
            _goml_runtime_core_string_println(inline482)
            var t433 string
            var inline477 bool = true
            var inline479 int
            switch inline477 {
            case true:
                t433 = "early"
                var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
                _goml_runtime_core_string_println(inline474)
                var t434 int32
                var inline469 bool = false
                var inline470 closure_env_f_0 = closure_env_f_0{}
                var inline471 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline470, p0)
                }
                var inline472 int32 = inline471(inline469)
                t434 = inline472
                var inline466 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t434)
                _goml_runtime_core_string_println(inline466)
                var t435 int32
                var inline461 bool = true
                var inline462 closure_env_f_0 = closure_env_f_0{}
                var inline463 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline462, p0)
                }
                var inline464 int32 = inline463(inline461)
                t435 = inline464
                var inline458 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t435)
                _goml_runtime_core_string_println(inline458)
                return struct{}{}
            case false:
                inline479 = 7
                var inline480 string = _goml_m_inherent_i_int_i_int_i_to__string(inline479)
                t433 = inline480
                var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
                _goml_runtime_core_string_println(inline474)
                var t434 int32
                var inline469 bool = false
                var inline470 closure_env_f_0 = closure_env_f_0{}
                var inline471 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline470, p0)
                }
                var inline472 int32 = inline471(inline469)
                t434 = inline472
                var inline466 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t434)
                _goml_runtime_core_string_println(inline466)
                var t435 int32
                var inline461 bool = true
                var inline462 closure_env_f_0 = closure_env_f_0{}
                var inline463 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline462, p0)
                }
                var inline464 int32 = inline463(inline461)
                t435 = inline464
                var inline458 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t435)
                _goml_runtime_core_string_println(inline458)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case false:
        inline495 = 7
        var inline497 int32 = inline495 + 1
        t431 = inline497
        var inline490 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t431)
        _goml_runtime_core_string_println(inline490)
        var t432 string
        var inline485 bool = false
        var inline487 int
        switch inline485 {
        case true:
            t432 = "early"
            var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
            _goml_runtime_core_string_println(inline482)
            var t433 string
            var inline477 bool = true
            var inline479 int
            switch inline477 {
            case true:
                t433 = "early"
                var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
                _goml_runtime_core_string_println(inline474)
                var t434 int32
                var inline469 bool = false
                var inline470 closure_env_f_0 = closure_env_f_0{}
                var inline471 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline470, p0)
                }
                var inline472 int32 = inline471(inline469)
                t434 = inline472
                var inline466 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t434)
                _goml_runtime_core_string_println(inline466)
                var t435 int32
                var inline461 bool = true
                var inline462 closure_env_f_0 = closure_env_f_0{}
                var inline463 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline462, p0)
                }
                var inline464 int32 = inline463(inline461)
                t435 = inline464
                var inline458 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t435)
                _goml_runtime_core_string_println(inline458)
                return struct{}{}
            case false:
                inline479 = 7
                var inline480 string = _goml_m_inherent_i_int_i_int_i_to__string(inline479)
                t433 = inline480
                var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
                _goml_runtime_core_string_println(inline474)
                var t434 int32
                var inline469 bool = false
                var inline470 closure_env_f_0 = closure_env_f_0{}
                var inline471 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline470, p0)
                }
                var inline472 int32 = inline471(inline469)
                t434 = inline472
                var inline466 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t434)
                _goml_runtime_core_string_println(inline466)
                var t435 int32
                var inline461 bool = true
                var inline462 closure_env_f_0 = closure_env_f_0{}
                var inline463 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline462, p0)
                }
                var inline464 int32 = inline463(inline461)
                t435 = inline464
                var inline458 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t435)
                _goml_runtime_core_string_println(inline458)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline487 = 7
            var inline488 string = _goml_m_inherent_i_int_i_int_i_to__string(inline487)
            t432 = inline488
            var inline482 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t432)
            _goml_runtime_core_string_println(inline482)
            var t433 string
            var inline477 bool = true
            var inline479 int
            switch inline477 {
            case true:
                t433 = "early"
                var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
                _goml_runtime_core_string_println(inline474)
                var t434 int32
                var inline469 bool = false
                var inline470 closure_env_f_0 = closure_env_f_0{}
                var inline471 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline470, p0)
                }
                var inline472 int32 = inline471(inline469)
                t434 = inline472
                var inline466 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t434)
                _goml_runtime_core_string_println(inline466)
                var t435 int32
                var inline461 bool = true
                var inline462 closure_env_f_0 = closure_env_f_0{}
                var inline463 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline462, p0)
                }
                var inline464 int32 = inline463(inline461)
                t435 = inline464
                var inline458 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t435)
                _goml_runtime_core_string_println(inline458)
                return struct{}{}
            case false:
                inline479 = 7
                var inline480 string = _goml_m_inherent_i_int_i_int_i_to__string(inline479)
                t433 = inline480
                var inline474 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t433)
                _goml_runtime_core_string_println(inline474)
                var t434 int32
                var inline469 bool = false
                var inline470 closure_env_f_0 = closure_env_f_0{}
                var inline471 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline470, p0)
                }
                var inline472 int32 = inline471(inline469)
                t434 = inline472
                var inline466 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t434)
                _goml_runtime_core_string_println(inline466)
                var t435 int32
                var inline461 bool = true
                var inline462 closure_env_f_0 = closure_env_f_0{}
                var inline463 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline462, p0)
                }
                var inline464 int32 = inline463(inline461)
                t435 = inline464
                var inline458 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t435)
                _goml_runtime_core_string_println(inline458)
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
    var t438 string = _goml_runtime_core_int_to_string(self__32)
    return t438
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t447 string = _goml_runtime_core_int32_to_string(self__154)
    return t447
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env414 closure_env_f_0, inner__4 bool) int32 {
    var jp453 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp453 = 4
        var t454 int32 = jp453 + 3
        return t454
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
