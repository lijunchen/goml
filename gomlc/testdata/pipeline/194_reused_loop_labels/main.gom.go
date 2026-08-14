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
    var inline243 bool = true
    var inline244 *ref_bool_x = ref__Ref_4bool(inline243)
    running__0 = inline244
    Loop_loop193:
    for {
        var t194 bool
        var inline237 bool = ref_get__Ref_4bool(running__0)
        t194 = inline237
        if t194 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x
                var inline226 bool = false
                var inline227 *ref_bool_x = ref__Ref_4bool(inline226)
                scanning__2 = inline227
                Loop_loop201:
                for {
                    var t202 bool
                    var inline224 bool = ref_get__Ref_4bool(scanning__2)
                    t202 = inline224
                    if t202 {
                        continue
                    } else {
                        break Loop_loop201
                    }
                }
                var scanning__3 *ref_bool_x
                var inline234 bool = false
                var inline235 *ref_bool_x = ref__Ref_4bool(inline234)
                scanning__3 = inline235
                Loop_loop198:
                for {
                    var t199 bool
                    var inline229 bool = ref_get__Ref_4bool(scanning__3)
                    t199 = inline229
                    if t199 {
                        continue
                    } else {
                        break Loop_loop198
                    }
                }
                var inline231 bool = false
                ref_set__Ref_4bool(running__0, inline231)
                continue
            case Close:
                var scanning__3 *ref_bool_x
                var inline234 bool = false
                var inline235 *ref_bool_x = ref__Ref_4bool(inline234)
                scanning__3 = inline235
                Loop_loop198__2:
                for {
                    var t199 bool
                    var inline229 bool = ref_get__Ref_4bool(scanning__3)
                    t199 = inline229
                    if t199 {
                        continue
                    } else {
                        break Loop_loop198__2
                    }
                }
                var inline231 bool = false
                ref_set__Ref_4bool(running__0, inline231)
                continue
            case Advance:
                var scanning__3 *ref_bool_x
                var inline234 bool = false
                var inline235 *ref_bool_x = ref__Ref_4bool(inline234)
                scanning__3 = inline235
                Loop_loop198__3:
                for {
                    var t199 bool
                    var inline229 bool = ref_get__Ref_4bool(scanning__3)
                    t199 = inline229
                    if t199 {
                        continue
                    } else {
                        break Loop_loop198__3
                    }
                }
                var inline231 bool = false
                ref_set__Ref_4bool(running__0, inline231)
                continue
            case Error:
                var scanning__3 *ref_bool_x
                var inline234 bool = false
                var inline235 *ref_bool_x = ref__Ref_4bool(inline234)
                scanning__3 = inline235
                Loop_loop198__4:
                for {
                    var t199 bool
                    var inline229 bool = ref_get__Ref_4bool(scanning__3)
                    t199 = inline229
                    if t199 {
                        continue
                    } else {
                        break Loop_loop198__4
                    }
                }
                var inline231 bool = false
                ref_set__Ref_4bool(running__0, inline231)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop193
        }
    }
    var inline239 string = "ok"
    var inline240 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline239)
    _goml_runtime_core_string_println(inline240)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
