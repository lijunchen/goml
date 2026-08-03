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
    var inline233 bool = true
    var inline234 *ref_bool_x = ref__Ref_4bool(inline233)
    running__0 = inline234
    Loop_loop183:
    for {
        var t184 bool
        var inline227 bool = ref_get__Ref_4bool(running__0)
        t184 = inline227
        if t184 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x
                var inline216 bool = false
                var inline217 *ref_bool_x = ref__Ref_4bool(inline216)
                scanning__2 = inline217
                Loop_loop191:
                for {
                    var t192 bool
                    var inline214 bool = ref_get__Ref_4bool(scanning__2)
                    t192 = inline214
                    if t192 {
                        continue
                    } else {
                        break Loop_loop191
                    }
                }
                var scanning__3 *ref_bool_x
                var inline224 bool = false
                var inline225 *ref_bool_x = ref__Ref_4bool(inline224)
                scanning__3 = inline225
                Loop_loop188:
                for {
                    var t189 bool
                    var inline219 bool = ref_get__Ref_4bool(scanning__3)
                    t189 = inline219
                    if t189 {
                        continue
                    } else {
                        break Loop_loop188
                    }
                }
                var inline221 bool = false
                ref_set__Ref_4bool(running__0, inline221)
                continue
            case Close:
                var scanning__3 *ref_bool_x
                var inline224 bool = false
                var inline225 *ref_bool_x = ref__Ref_4bool(inline224)
                scanning__3 = inline225
                Loop_loop188__2:
                for {
                    var t189 bool
                    var inline219 bool = ref_get__Ref_4bool(scanning__3)
                    t189 = inline219
                    if t189 {
                        continue
                    } else {
                        break Loop_loop188__2
                    }
                }
                var inline221 bool = false
                ref_set__Ref_4bool(running__0, inline221)
                continue
            case Advance:
                var scanning__3 *ref_bool_x
                var inline224 bool = false
                var inline225 *ref_bool_x = ref__Ref_4bool(inline224)
                scanning__3 = inline225
                Loop_loop188__3:
                for {
                    var t189 bool
                    var inline219 bool = ref_get__Ref_4bool(scanning__3)
                    t189 = inline219
                    if t189 {
                        continue
                    } else {
                        break Loop_loop188__3
                    }
                }
                var inline221 bool = false
                ref_set__Ref_4bool(running__0, inline221)
                continue
            case Error:
                var scanning__3 *ref_bool_x
                var inline224 bool = false
                var inline225 *ref_bool_x = ref__Ref_4bool(inline224)
                scanning__3 = inline225
                Loop_loop188__4:
                for {
                    var t189 bool
                    var inline219 bool = ref_get__Ref_4bool(scanning__3)
                    t189 = inline219
                    if t189 {
                        continue
                    } else {
                        break Loop_loop188__4
                    }
                }
                var inline221 bool = false
                ref_set__Ref_4bool(running__0, inline221)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop183
        }
    }
    var inline229 string = "ok"
    var inline230 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline229)
    _goml_runtime_core_string_println(inline230)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
