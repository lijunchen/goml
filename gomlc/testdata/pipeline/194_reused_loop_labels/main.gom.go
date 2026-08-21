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
    var inline467 bool = true
    var inline468 *ref_bool_x = ref__Ref_4bool(inline467)
    running__0 = inline468
    Loop_loop417:
    for {
        var t418 bool
        var inline461 bool = ref_get__Ref_4bool(running__0)
        t418 = inline461
        if t418 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x
                var inline450 bool = false
                var inline451 *ref_bool_x = ref__Ref_4bool(inline450)
                scanning__2 = inline451
                Loop_loop425:
                for {
                    var t426 bool
                    var inline448 bool = ref_get__Ref_4bool(scanning__2)
                    t426 = inline448
                    if t426 {
                        continue
                    } else {
                        break Loop_loop425
                    }
                }
                var scanning__3 *ref_bool_x
                var inline458 bool = false
                var inline459 *ref_bool_x = ref__Ref_4bool(inline458)
                scanning__3 = inline459
                Loop_loop422:
                for {
                    var t423 bool
                    var inline453 bool = ref_get__Ref_4bool(scanning__3)
                    t423 = inline453
                    if t423 {
                        continue
                    } else {
                        break Loop_loop422
                    }
                }
                var inline455 bool = false
                ref_set__Ref_4bool(running__0, inline455)
                continue
            case Close:
                var scanning__3 *ref_bool_x
                var inline458 bool = false
                var inline459 *ref_bool_x = ref__Ref_4bool(inline458)
                scanning__3 = inline459
                Loop_loop422__2:
                for {
                    var t423 bool
                    var inline453 bool = ref_get__Ref_4bool(scanning__3)
                    t423 = inline453
                    if t423 {
                        continue
                    } else {
                        break Loop_loop422__2
                    }
                }
                var inline455 bool = false
                ref_set__Ref_4bool(running__0, inline455)
                continue
            case Advance:
                var scanning__3 *ref_bool_x
                var inline458 bool = false
                var inline459 *ref_bool_x = ref__Ref_4bool(inline458)
                scanning__3 = inline459
                Loop_loop422__3:
                for {
                    var t423 bool
                    var inline453 bool = ref_get__Ref_4bool(scanning__3)
                    t423 = inline453
                    if t423 {
                        continue
                    } else {
                        break Loop_loop422__3
                    }
                }
                var inline455 bool = false
                ref_set__Ref_4bool(running__0, inline455)
                continue
            case Error:
                var scanning__3 *ref_bool_x
                var inline458 bool = false
                var inline459 *ref_bool_x = ref__Ref_4bool(inline458)
                scanning__3 = inline459
                Loop_loop422__4:
                for {
                    var t423 bool
                    var inline453 bool = ref_get__Ref_4bool(scanning__3)
                    t423 = inline453
                    if t423 {
                        continue
                    } else {
                        break Loop_loop422__4
                    }
                }
                var inline455 bool = false
                ref_set__Ref_4bool(running__0, inline455)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop417
        }
    }
    var inline463 string = "ok"
    var inline464 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline463)
    _goml_runtime_core_string_println(inline464)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__149 string) string {
    return self__149
}

func main() {
    main0()
}
