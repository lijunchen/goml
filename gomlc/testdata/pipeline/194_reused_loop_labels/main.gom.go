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
    var inline238 bool = true
    var inline239 *ref_bool_x = ref__Ref_4bool(inline238)
    running__0 = inline239
    Loop_loop188:
    for {
        var t189 bool
        var inline232 bool = ref_get__Ref_4bool(running__0)
        t189 = inline232
        if t189 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x
                var inline221 bool = false
                var inline222 *ref_bool_x = ref__Ref_4bool(inline221)
                scanning__2 = inline222
                Loop_loop196:
                for {
                    var t197 bool
                    var inline219 bool = ref_get__Ref_4bool(scanning__2)
                    t197 = inline219
                    if t197 {
                        continue
                    } else {
                        break Loop_loop196
                    }
                }
                var scanning__3 *ref_bool_x
                var inline229 bool = false
                var inline230 *ref_bool_x = ref__Ref_4bool(inline229)
                scanning__3 = inline230
                Loop_loop193:
                for {
                    var t194 bool
                    var inline224 bool = ref_get__Ref_4bool(scanning__3)
                    t194 = inline224
                    if t194 {
                        continue
                    } else {
                        break Loop_loop193
                    }
                }
                var inline226 bool = false
                ref_set__Ref_4bool(running__0, inline226)
                continue
            case Close:
                var scanning__3 *ref_bool_x
                var inline229 bool = false
                var inline230 *ref_bool_x = ref__Ref_4bool(inline229)
                scanning__3 = inline230
                Loop_loop193__2:
                for {
                    var t194 bool
                    var inline224 bool = ref_get__Ref_4bool(scanning__3)
                    t194 = inline224
                    if t194 {
                        continue
                    } else {
                        break Loop_loop193__2
                    }
                }
                var inline226 bool = false
                ref_set__Ref_4bool(running__0, inline226)
                continue
            case Advance:
                var scanning__3 *ref_bool_x
                var inline229 bool = false
                var inline230 *ref_bool_x = ref__Ref_4bool(inline229)
                scanning__3 = inline230
                Loop_loop193__3:
                for {
                    var t194 bool
                    var inline224 bool = ref_get__Ref_4bool(scanning__3)
                    t194 = inline224
                    if t194 {
                        continue
                    } else {
                        break Loop_loop193__3
                    }
                }
                var inline226 bool = false
                ref_set__Ref_4bool(running__0, inline226)
                continue
            case Error:
                var scanning__3 *ref_bool_x
                var inline229 bool = false
                var inline230 *ref_bool_x = ref__Ref_4bool(inline229)
                scanning__3 = inline230
                Loop_loop193__4:
                for {
                    var t194 bool
                    var inline224 bool = ref_get__Ref_4bool(scanning__3)
                    t194 = inline224
                    if t194 {
                        continue
                    } else {
                        break Loop_loop193__4
                    }
                }
                var inline226 bool = false
                ref_set__Ref_4bool(running__0, inline226)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop188
        }
    }
    var inline234 string = "ok"
    var inline235 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline234)
    _goml_runtime_core_string_println(inline235)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
