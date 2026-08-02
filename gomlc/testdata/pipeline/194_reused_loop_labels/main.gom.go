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

type Event int32

const (
    Open Event = 0
    Close Event = 1
    Advance Event = 2
    Error Event = 3
)

func main0() struct{} {
    var running__0 *ref_bool_x
    var inline202 bool = true
    var inline203 *ref_bool_x = ref__Ref_4bool(inline202)
    running__0 = inline203
    Loop_loop161:
    for {
        var t162 bool
        var inline200 bool = ref_get__Ref_4bool(running__0)
        t162 = inline200
        if t162 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x
                var inline189 bool = false
                var inline190 *ref_bool_x = ref__Ref_4bool(inline189)
                scanning__2 = inline190
                Loop_loop169:
                for {
                    var t170 bool
                    var inline187 bool = ref_get__Ref_4bool(scanning__2)
                    t170 = inline187
                    if t170 {
                        continue
                    } else {
                        break Loop_loop169
                    }
                }
                var scanning__3 *ref_bool_x
                var inline197 bool = false
                var inline198 *ref_bool_x = ref__Ref_4bool(inline197)
                scanning__3 = inline198
                Loop_loop166:
                for {
                    var t167 bool
                    var inline192 bool = ref_get__Ref_4bool(scanning__3)
                    t167 = inline192
                    if t167 {
                        continue
                    } else {
                        break Loop_loop166
                    }
                }
                var inline194 bool = false
                ref_set__Ref_4bool(running__0, inline194)
                continue
            case Close:
                var scanning__3 *ref_bool_x
                var inline197 bool = false
                var inline198 *ref_bool_x = ref__Ref_4bool(inline197)
                scanning__3 = inline198
                Loop_loop166__2:
                for {
                    var t167 bool
                    var inline192 bool = ref_get__Ref_4bool(scanning__3)
                    t167 = inline192
                    if t167 {
                        continue
                    } else {
                        break Loop_loop166__2
                    }
                }
                var inline194 bool = false
                ref_set__Ref_4bool(running__0, inline194)
                continue
            case Advance:
                var scanning__3 *ref_bool_x
                var inline197 bool = false
                var inline198 *ref_bool_x = ref__Ref_4bool(inline197)
                scanning__3 = inline198
                Loop_loop166__3:
                for {
                    var t167 bool
                    var inline192 bool = ref_get__Ref_4bool(scanning__3)
                    t167 = inline192
                    if t167 {
                        continue
                    } else {
                        break Loop_loop166__3
                    }
                }
                var inline194 bool = false
                ref_set__Ref_4bool(running__0, inline194)
                continue
            case Error:
                var scanning__3 *ref_bool_x
                var inline197 bool = false
                var inline198 *ref_bool_x = ref__Ref_4bool(inline197)
                scanning__3 = inline198
                Loop_loop166__4:
                for {
                    var t167 bool
                    var inline192 bool = ref_get__Ref_4bool(scanning__3)
                    t167 = inline192
                    if t167 {
                        continue
                    } else {
                        break Loop_loop166__4
                    }
                }
                var inline194 bool = false
                ref_set__Ref_4bool(running__0, inline194)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop161
        }
    }
    _goml_runtime_core_string_println("ok")
    return struct{}{}
}

func main() {
    main0()
}
