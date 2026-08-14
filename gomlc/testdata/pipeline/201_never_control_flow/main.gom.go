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

type Ordering int32

func continue_branch() struct{} {
    var count__2 *ref_int_x
    var inline481 int = 0
    var inline482 *ref_int_x = ref__Ref_3int(inline481)
    count__2 = inline482
    Loop_loop421:
    for {
        var t422 int
        var inline479 int = ref_get__Ref_3int(count__2)
        t422 = inline479
        var t423 bool = t422 < 2
        if t423 {
            var t424 int
            var inline477 int = ref_get__Ref_3int(count__2)
            t424 = inline477
            var t425 int = t424 + 1
            ref_set__Ref_3int(count__2, t425)
            var t429 int
            var inline473 int = ref_get__Ref_3int(count__2)
            t429 = inline473
            var t430 bool
            var inline470 int = 1
            var inline471 bool = t429 == inline470
            t430 = inline471
            var jp427 int
            if t430 {
                continue
            } else {
                jp427 = 7
                var inline467 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp427)
                _goml_runtime_core_string_println(inline467)
                continue
            }
        } else {
            break Loop_loop421
        }
    }
    return struct{}{}
}

func break_branch(stop__4 bool) struct{} {
    var jp435 int
    if stop__4 {
        return struct{}{}
    } else {
        jp435 = 9
        var inline484 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp435)
        _goml_runtime_core_string_println(inline484)
        return struct{}{}
    }
}

func main0() struct{} {
    var t437 int32
    var inline499 bool = false
    var inline501 int32
    if inline499 {
        t437 = 10
        var inline496 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t437)
        _goml_runtime_core_string_println(inline496)
        var t438 int32
        var inline490 bool = true
        var inline492 int32
        if inline490 {
            t438 = 10
            var inline487 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t438)
            _goml_runtime_core_string_println(inline487)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline492 = 20
            var inline494 int32 = inline492 + 1
            t438 = inline494
            var inline487 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t438)
            _goml_runtime_core_string_println(inline487)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    } else {
        inline501 = 20
        var inline503 int32 = inline501 + 1
        t437 = inline503
        var inline496 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t437)
        _goml_runtime_core_string_println(inline496)
        var t438 int32
        var inline490 bool = true
        var inline492 int32
        if inline490 {
            t438 = 10
            var inline487 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t438)
            _goml_runtime_core_string_println(inline487)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline492 = 20
            var inline494 int32 = inline492 + 1
            t438 = inline494
            var inline487 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t438)
            _goml_runtime_core_string_println(inline487)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t459 string = _goml_runtime_core_int_to_string(self__151)
    return t459
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t462 string = _goml_runtime_core_int32_to_string(self__154)
    return t462
}

func main() {
    main0()
}
