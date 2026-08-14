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
    var inline478 int = 0
    var inline479 *ref_int_x = ref__Ref_3int(inline478)
    count__2 = inline479
    Loop_loop421:
    for {
        var t422 int
        var inline476 int = ref_get__Ref_3int(count__2)
        t422 = inline476
        var t423 bool = t422 < 2
        if t423 {
            var t424 int
            var inline474 int = ref_get__Ref_3int(count__2)
            t424 = inline474
            var t425 int = t424 + 1
            ref_set__Ref_3int(count__2, t425)
            var t429 int
            var inline470 int = ref_get__Ref_3int(count__2)
            t429 = inline470
            var t430 bool = t429 == 1
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
        var inline481 string = _goml_m_trait__impl_i_ToString_i_int_i_to__string(jp435)
        _goml_runtime_core_string_println(inline481)
        return struct{}{}
    }
}

func main0() struct{} {
    var t437 int32
    var inline496 bool = false
    var inline498 int32
    if inline496 {
        t437 = 10
        var inline493 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t437)
        _goml_runtime_core_string_println(inline493)
        var t438 int32
        var inline487 bool = true
        var inline489 int32
        if inline487 {
            t438 = 10
            var inline484 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t438)
            _goml_runtime_core_string_println(inline484)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline489 = 20
            var inline491 int32 = inline489 + 1
            t438 = inline491
            var inline484 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t438)
            _goml_runtime_core_string_println(inline484)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    } else {
        inline498 = 20
        var inline500 int32 = inline498 + 1
        t437 = inline500
        var inline493 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t437)
        _goml_runtime_core_string_println(inline493)
        var t438 int32
        var inline487 bool = true
        var inline489 int32
        if inline487 {
            t438 = 10
            var inline484 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t438)
            _goml_runtime_core_string_println(inline484)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        } else {
            inline489 = 20
            var inline491 int32 = inline489 + 1
            t438 = inline491
            var inline484 string = _goml_m_trait__impl_i_ToString_i_int32_i_to__string(t438)
            _goml_runtime_core_string_println(inline484)
            continue_branch()
            break_branch(false)
            break_branch(true)
            return struct{}{}
        }
    }
}

func _goml_m_trait__impl_i_ToString_i_int_i_to__string(self__151 int) string {
    var t456 string = _goml_runtime_core_int_to_string(self__151)
    return t456
}

func _goml_m_trait__impl_i_ToString_i_int32_i_to__string(self__154 int32) string {
    var t459 string = _goml_runtime_core_int32_to_string(self__154)
    return t459
}

func main() {
    main0()
}
