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
    var inline211 bool = true
    var inline212 *ref_bool_x = ref__Ref_4bool(inline211)
    running__0 = inline212
    Loop_loop161:
    for {
        var t162 bool
        var inline205 bool = ref_get__Ref_4bool(running__0)
        t162 = inline205
        if t162 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x
                var inline194 bool = false
                var inline195 *ref_bool_x = ref__Ref_4bool(inline194)
                scanning__2 = inline195
                Loop_loop169:
                for {
                    var t170 bool
                    var inline192 bool = ref_get__Ref_4bool(scanning__2)
                    t170 = inline192
                    if t170 {
                        continue
                    } else {
                        break Loop_loop169
                    }
                }
                var scanning__3 *ref_bool_x
                var inline202 bool = false
                var inline203 *ref_bool_x = ref__Ref_4bool(inline202)
                scanning__3 = inline203
                Loop_loop166:
                for {
                    var t167 bool
                    var inline197 bool = ref_get__Ref_4bool(scanning__3)
                    t167 = inline197
                    if t167 {
                        continue
                    } else {
                        break Loop_loop166
                    }
                }
                var inline199 bool = false
                ref_set__Ref_4bool(running__0, inline199)
                continue
            case Close:
                var scanning__3 *ref_bool_x
                var inline202 bool = false
                var inline203 *ref_bool_x = ref__Ref_4bool(inline202)
                scanning__3 = inline203
                Loop_loop166__2:
                for {
                    var t167 bool
                    var inline197 bool = ref_get__Ref_4bool(scanning__3)
                    t167 = inline197
                    if t167 {
                        continue
                    } else {
                        break Loop_loop166__2
                    }
                }
                var inline199 bool = false
                ref_set__Ref_4bool(running__0, inline199)
                continue
            case Advance:
                var scanning__3 *ref_bool_x
                var inline202 bool = false
                var inline203 *ref_bool_x = ref__Ref_4bool(inline202)
                scanning__3 = inline203
                Loop_loop166__3:
                for {
                    var t167 bool
                    var inline197 bool = ref_get__Ref_4bool(scanning__3)
                    t167 = inline197
                    if t167 {
                        continue
                    } else {
                        break Loop_loop166__3
                    }
                }
                var inline199 bool = false
                ref_set__Ref_4bool(running__0, inline199)
                continue
            case Error:
                var scanning__3 *ref_bool_x
                var inline202 bool = false
                var inline203 *ref_bool_x = ref__Ref_4bool(inline202)
                scanning__3 = inline203
                Loop_loop166__4:
                for {
                    var t167 bool
                    var inline197 bool = ref_get__Ref_4bool(scanning__3)
                    t167 = inline197
                    if t167 {
                        continue
                    } else {
                        break Loop_loop166__4
                    }
                }
                var inline199 bool = false
                ref_set__Ref_4bool(running__0, inline199)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop161
        }
    }
    var inline207 string = "ok"
    var inline208 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline207)
    _goml_runtime_core_string_println(inline208)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
