package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type ref_bool_x struct {
    value bool
}

func ref__Ref_4bool(value bool) *ref_bool_x {
    return &ref_bool_x{
        value: value,
    }
}

func ref_get__Ref_4bool(reference *ref_bool_x) bool {
    return reference.value
}

func ref_set__Ref_4bool(reference *ref_bool_x, value bool) struct{} {
    reference.value = value
    return struct{}{}
}

type Ordering int32

type Event int32

const (
    Open Event = 0
    Close Event = 1
    Advance Event = 2
    Error Event = 3
)

func main0() struct{} {
    var running__0 *ref_bool_x
    var inline464 bool = true
    var inline465 *ref_bool_x = ref__Ref_4bool(inline464)
    running__0 = inline465
    Loop_loop414:
    for {
        var t415 bool
        var inline458 bool = ref_get__Ref_4bool(running__0)
        t415 = inline458
        if t415 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x
                var inline447 bool = false
                var inline448 *ref_bool_x = ref__Ref_4bool(inline447)
                scanning__2 = inline448
                Loop_loop422:
                for {
                    var t423 bool
                    var inline445 bool = ref_get__Ref_4bool(scanning__2)
                    t423 = inline445
                    if t423 {
                        continue
                    } else {
                        break Loop_loop422
                    }
                }
                var scanning__3 *ref_bool_x
                var inline455 bool = false
                var inline456 *ref_bool_x = ref__Ref_4bool(inline455)
                scanning__3 = inline456
                Loop_loop419:
                for {
                    var t420 bool
                    var inline450 bool = ref_get__Ref_4bool(scanning__3)
                    t420 = inline450
                    if t420 {
                        continue
                    } else {
                        break Loop_loop419
                    }
                }
                var inline452 bool = false
                ref_set__Ref_4bool(running__0, inline452)
                continue
            case Close:
                var scanning__3 *ref_bool_x
                var inline455 bool = false
                var inline456 *ref_bool_x = ref__Ref_4bool(inline455)
                scanning__3 = inline456
                Loop_loop419__2:
                for {
                    var t420 bool
                    var inline450 bool = ref_get__Ref_4bool(scanning__3)
                    t420 = inline450
                    if t420 {
                        continue
                    } else {
                        break Loop_loop419__2
                    }
                }
                var inline452 bool = false
                ref_set__Ref_4bool(running__0, inline452)
                continue
            case Advance:
                var scanning__3 *ref_bool_x
                var inline455 bool = false
                var inline456 *ref_bool_x = ref__Ref_4bool(inline455)
                scanning__3 = inline456
                Loop_loop419__3:
                for {
                    var t420 bool
                    var inline450 bool = ref_get__Ref_4bool(scanning__3)
                    t420 = inline450
                    if t420 {
                        continue
                    } else {
                        break Loop_loop419__3
                    }
                }
                var inline452 bool = false
                ref_set__Ref_4bool(running__0, inline452)
                continue
            case Error:
                var scanning__3 *ref_bool_x
                var inline455 bool = false
                var inline456 *ref_bool_x = ref__Ref_4bool(inline455)
                scanning__3 = inline456
                Loop_loop419__4:
                for {
                    var t420 bool
                    var inline450 bool = ref_get__Ref_4bool(scanning__3)
                    t420 = inline450
                    if t420 {
                        continue
                    } else {
                        break Loop_loop419__4
                    }
                }
                var inline452 bool = false
                ref_set__Ref_4bool(running__0, inline452)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop414
        }
    }
    var inline460 string = "ok"
    var inline461 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline460)
    _goml_runtime_core_string_println(inline461)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
