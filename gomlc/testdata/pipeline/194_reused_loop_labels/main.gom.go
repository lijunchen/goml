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
    var inline228 bool = true
    var inline229 *ref_bool_x = ref__Ref_4bool(inline228)
    running__0 = inline229
    Loop_loop178:
    for {
        var t179 bool
        var inline222 bool = ref_get__Ref_4bool(running__0)
        t179 = inline222
        if t179 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x
                var inline211 bool = false
                var inline212 *ref_bool_x = ref__Ref_4bool(inline211)
                scanning__2 = inline212
                Loop_loop186:
                for {
                    var t187 bool
                    var inline209 bool = ref_get__Ref_4bool(scanning__2)
                    t187 = inline209
                    if t187 {
                        continue
                    } else {
                        break Loop_loop186
                    }
                }
                var scanning__3 *ref_bool_x
                var inline219 bool = false
                var inline220 *ref_bool_x = ref__Ref_4bool(inline219)
                scanning__3 = inline220
                Loop_loop183:
                for {
                    var t184 bool
                    var inline214 bool = ref_get__Ref_4bool(scanning__3)
                    t184 = inline214
                    if t184 {
                        continue
                    } else {
                        break Loop_loop183
                    }
                }
                var inline216 bool = false
                ref_set__Ref_4bool(running__0, inline216)
                continue
            case Close:
                var scanning__3 *ref_bool_x
                var inline219 bool = false
                var inline220 *ref_bool_x = ref__Ref_4bool(inline219)
                scanning__3 = inline220
                Loop_loop183__2:
                for {
                    var t184 bool
                    var inline214 bool = ref_get__Ref_4bool(scanning__3)
                    t184 = inline214
                    if t184 {
                        continue
                    } else {
                        break Loop_loop183__2
                    }
                }
                var inline216 bool = false
                ref_set__Ref_4bool(running__0, inline216)
                continue
            case Advance:
                var scanning__3 *ref_bool_x
                var inline219 bool = false
                var inline220 *ref_bool_x = ref__Ref_4bool(inline219)
                scanning__3 = inline220
                Loop_loop183__3:
                for {
                    var t184 bool
                    var inline214 bool = ref_get__Ref_4bool(scanning__3)
                    t184 = inline214
                    if t184 {
                        continue
                    } else {
                        break Loop_loop183__3
                    }
                }
                var inline216 bool = false
                ref_set__Ref_4bool(running__0, inline216)
                continue
            case Error:
                var scanning__3 *ref_bool_x
                var inline219 bool = false
                var inline220 *ref_bool_x = ref__Ref_4bool(inline219)
                scanning__3 = inline220
                Loop_loop183__4:
                for {
                    var t184 bool
                    var inline214 bool = ref_get__Ref_4bool(scanning__3)
                    t184 = inline214
                    if t184 {
                        continue
                    } else {
                        break Loop_loop183__4
                    }
                }
                var inline216 bool = false
                ref_set__Ref_4bool(running__0, inline216)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop178
        }
    }
    var inline224 string = "ok"
    var inline225 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline224)
    _goml_runtime_core_string_println(inline225)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
