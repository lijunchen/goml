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
    var jp421 int32
    switch flag__0 {
    case true:
        return 5
    case false:
        jp421 = 7
        var t422 int32 = jp421 + 1
        return t422
    default:
        panic("non-exhaustive match")
    }
}

func main0() struct{} {
    var t433 int32 = add_after_match(false)
    var inline502 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t433)
    _goml_runtime_core_string_println(inline502)
    var t434 int32
    var inline496 bool = true
    var inline498 int32
    switch inline496 {
    case true:
        t434 = 5
        var inline493 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t434)
        _goml_runtime_core_string_println(inline493)
        var t435 string
        var inline488 bool = false
        var inline490 int
        switch inline488 {
        case true:
            t435 = "early"
            var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
            _goml_runtime_core_string_println(inline485)
            var t436 string
            var inline480 bool = true
            var inline482 int
            switch inline480 {
            case true:
                t436 = "early"
                var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
                _goml_runtime_core_string_println(inline477)
                var t437 int32
                var inline472 bool = false
                var inline473 closure_env_f_0 = closure_env_f_0{}
                var inline474 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline473, p0)
                }
                var inline475 int32 = inline474(inline472)
                t437 = inline475
                var inline469 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t437)
                _goml_runtime_core_string_println(inline469)
                var t438 int32
                var inline464 bool = true
                var inline465 closure_env_f_0 = closure_env_f_0{}
                var inline466 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline465, p0)
                }
                var inline467 int32 = inline466(inline464)
                t438 = inline467
                var inline461 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t438)
                _goml_runtime_core_string_println(inline461)
                return struct{}{}
            case false:
                inline482 = 7
                var inline483 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline482)
                t436 = inline483
                var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
                _goml_runtime_core_string_println(inline477)
                var t437 int32
                var inline472 bool = false
                var inline473 closure_env_f_0 = closure_env_f_0{}
                var inline474 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline473, p0)
                }
                var inline475 int32 = inline474(inline472)
                t437 = inline475
                var inline469 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t437)
                _goml_runtime_core_string_println(inline469)
                var t438 int32
                var inline464 bool = true
                var inline465 closure_env_f_0 = closure_env_f_0{}
                var inline466 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline465, p0)
                }
                var inline467 int32 = inline466(inline464)
                t438 = inline467
                var inline461 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t438)
                _goml_runtime_core_string_println(inline461)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline490 = 7
            var inline491 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline490)
            t435 = inline491
            var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
            _goml_runtime_core_string_println(inline485)
            var t436 string
            var inline480 bool = true
            var inline482 int
            switch inline480 {
            case true:
                t436 = "early"
                var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
                _goml_runtime_core_string_println(inline477)
                var t437 int32
                var inline472 bool = false
                var inline473 closure_env_f_0 = closure_env_f_0{}
                var inline474 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline473, p0)
                }
                var inline475 int32 = inline474(inline472)
                t437 = inline475
                var inline469 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t437)
                _goml_runtime_core_string_println(inline469)
                var t438 int32
                var inline464 bool = true
                var inline465 closure_env_f_0 = closure_env_f_0{}
                var inline466 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline465, p0)
                }
                var inline467 int32 = inline466(inline464)
                t438 = inline467
                var inline461 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t438)
                _goml_runtime_core_string_println(inline461)
                return struct{}{}
            case false:
                inline482 = 7
                var inline483 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline482)
                t436 = inline483
                var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
                _goml_runtime_core_string_println(inline477)
                var t437 int32
                var inline472 bool = false
                var inline473 closure_env_f_0 = closure_env_f_0{}
                var inline474 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline473, p0)
                }
                var inline475 int32 = inline474(inline472)
                t437 = inline475
                var inline469 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t437)
                _goml_runtime_core_string_println(inline469)
                var t438 int32
                var inline464 bool = true
                var inline465 closure_env_f_0 = closure_env_f_0{}
                var inline466 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline465, p0)
                }
                var inline467 int32 = inline466(inline464)
                t438 = inline467
                var inline461 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t438)
                _goml_runtime_core_string_println(inline461)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        default:
            panic("non-exhaustive match")
        }
    case false:
        inline498 = 7
        var inline500 int32 = inline498 + 1
        t434 = inline500
        var inline493 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t434)
        _goml_runtime_core_string_println(inline493)
        var t435 string
        var inline488 bool = false
        var inline490 int
        switch inline488 {
        case true:
            t435 = "early"
            var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
            _goml_runtime_core_string_println(inline485)
            var t436 string
            var inline480 bool = true
            var inline482 int
            switch inline480 {
            case true:
                t436 = "early"
                var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
                _goml_runtime_core_string_println(inline477)
                var t437 int32
                var inline472 bool = false
                var inline473 closure_env_f_0 = closure_env_f_0{}
                var inline474 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline473, p0)
                }
                var inline475 int32 = inline474(inline472)
                t437 = inline475
                var inline469 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t437)
                _goml_runtime_core_string_println(inline469)
                var t438 int32
                var inline464 bool = true
                var inline465 closure_env_f_0 = closure_env_f_0{}
                var inline466 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline465, p0)
                }
                var inline467 int32 = inline466(inline464)
                t438 = inline467
                var inline461 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t438)
                _goml_runtime_core_string_println(inline461)
                return struct{}{}
            case false:
                inline482 = 7
                var inline483 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline482)
                t436 = inline483
                var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
                _goml_runtime_core_string_println(inline477)
                var t437 int32
                var inline472 bool = false
                var inline473 closure_env_f_0 = closure_env_f_0{}
                var inline474 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline473, p0)
                }
                var inline475 int32 = inline474(inline472)
                t437 = inline475
                var inline469 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t437)
                _goml_runtime_core_string_println(inline469)
                var t438 int32
                var inline464 bool = true
                var inline465 closure_env_f_0 = closure_env_f_0{}
                var inline466 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline465, p0)
                }
                var inline467 int32 = inline466(inline464)
                t438 = inline467
                var inline461 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t438)
                _goml_runtime_core_string_println(inline461)
                return struct{}{}
            default:
                panic("non-exhaustive match")
            }
        case false:
            inline490 = 7
            var inline491 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline490)
            t435 = inline491
            var inline485 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t435)
            _goml_runtime_core_string_println(inline485)
            var t436 string
            var inline480 bool = true
            var inline482 int
            switch inline480 {
            case true:
                t436 = "early"
                var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
                _goml_runtime_core_string_println(inline477)
                var t437 int32
                var inline472 bool = false
                var inline473 closure_env_f_0 = closure_env_f_0{}
                var inline474 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline473, p0)
                }
                var inline475 int32 = inline474(inline472)
                t437 = inline475
                var inline469 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t437)
                _goml_runtime_core_string_println(inline469)
                var t438 int32
                var inline464 bool = true
                var inline465 closure_env_f_0 = closure_env_f_0{}
                var inline466 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline465, p0)
                }
                var inline467 int32 = inline466(inline464)
                t438 = inline467
                var inline461 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t438)
                _goml_runtime_core_string_println(inline461)
                return struct{}{}
            case false:
                inline482 = 7
                var inline483 string = _goml_m_inherent_i_isize_i_isize_i_to__string(inline482)
                t436 = inline483
                var inline477 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t436)
                _goml_runtime_core_string_println(inline477)
                var t437 int32
                var inline472 bool = false
                var inline473 closure_env_f_0 = closure_env_f_0{}
                var inline474 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline473, p0)
                }
                var inline475 int32 = inline474(inline472)
                t437 = inline475
                var inline469 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t437)
                _goml_runtime_core_string_println(inline469)
                var t438 int32
                var inline464 bool = true
                var inline465 closure_env_f_0 = closure_env_f_0{}
                var inline466 func(bool) int32 = func(p0 bool) int32 {
                    return _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(inline465, p0)
                }
                var inline467 int32 = inline466(inline464)
                t438 = inline467
                var inline461 string = _goml_m_trait__impl_i_ToString_i_i32_i_to__string(t438)
                _goml_runtime_core_string_println(inline461)
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

func _goml_m_inherent_i_isize_i_isize_i_to__string(self__32 int) string {
    var t441 string = _goml_runtime_core_int_to_string(self__32)
    return t441
}

func _goml_m_trait__impl_i_ToString_i_i32_i_to__string(self__154 int32) string {
    var t450 string = _goml_runtime_core_int32_to_string(self__154)
    return t450
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func _goml_m_inherent_i_closure__env__f__0_i_closure__env__f__0_i_apply(env417 closure_env_f_0, inner__4 bool) int32 {
    var jp456 int32
    switch inner__4 {
    case true:
        return 2
    case false:
        jp456 = 4
        var t457 int32 = jp456 + 3
        return t457
    default:
        panic("non-exhaustive match")
    }
}

func main() {
    main0()
}
