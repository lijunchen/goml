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
    var inline192 bool = true
    var inline193 *ref_bool_x = ref__Ref_4bool(inline192)
    running__0 = inline193
    Loop_loop142:
    for {
        var t143 bool
        var inline186 bool = ref_get__Ref_4bool(running__0)
        t143 = inline186
        if t143 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x
                var inline175 bool = false
                var inline176 *ref_bool_x = ref__Ref_4bool(inline175)
                scanning__2 = inline176
                Loop_loop150:
                for {
                    var t151 bool
                    var inline173 bool = ref_get__Ref_4bool(scanning__2)
                    t151 = inline173
                    if t151 {
                        continue
                    } else {
                        break Loop_loop150
                    }
                }
                var scanning__3 *ref_bool_x
                var inline183 bool = false
                var inline184 *ref_bool_x = ref__Ref_4bool(inline183)
                scanning__3 = inline184
                Loop_loop147:
                for {
                    var t148 bool
                    var inline178 bool = ref_get__Ref_4bool(scanning__3)
                    t148 = inline178
                    if t148 {
                        continue
                    } else {
                        break Loop_loop147
                    }
                }
                var inline180 bool = false
                ref_set__Ref_4bool(running__0, inline180)
                continue
            case Close:
                var scanning__3 *ref_bool_x
                var inline183 bool = false
                var inline184 *ref_bool_x = ref__Ref_4bool(inline183)
                scanning__3 = inline184
                Loop_loop147__2:
                for {
                    var t148 bool
                    var inline178 bool = ref_get__Ref_4bool(scanning__3)
                    t148 = inline178
                    if t148 {
                        continue
                    } else {
                        break Loop_loop147__2
                    }
                }
                var inline180 bool = false
                ref_set__Ref_4bool(running__0, inline180)
                continue
            case Advance:
                var scanning__3 *ref_bool_x
                var inline183 bool = false
                var inline184 *ref_bool_x = ref__Ref_4bool(inline183)
                scanning__3 = inline184
                Loop_loop147__3:
                for {
                    var t148 bool
                    var inline178 bool = ref_get__Ref_4bool(scanning__3)
                    t148 = inline178
                    if t148 {
                        continue
                    } else {
                        break Loop_loop147__3
                    }
                }
                var inline180 bool = false
                ref_set__Ref_4bool(running__0, inline180)
                continue
            case Error:
                var scanning__3 *ref_bool_x
                var inline183 bool = false
                var inline184 *ref_bool_x = ref__Ref_4bool(inline183)
                scanning__3 = inline184
                Loop_loop147__4:
                for {
                    var t148 bool
                    var inline178 bool = ref_get__Ref_4bool(scanning__3)
                    t148 = inline178
                    if t148 {
                        continue
                    } else {
                        break Loop_loop147__4
                    }
                }
                var inline180 bool = false
                ref_set__Ref_4bool(running__0, inline180)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop142
        }
    }
    var inline188 string = "ok"
    var inline189 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline188)
    _goml_runtime_core_string_println(inline189)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
