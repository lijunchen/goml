package main

import (
    _goml_os "os"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_os.Stdout.WriteString(s + "\n")
    return struct{}{}
}

type _goml_vec_uint32 struct {
    items []uint32
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

type FloatNatural struct {
    words *_goml_vec_uint32
}

type ParsedFloat struct {
    valid bool
    negative bool
    special int
    numerator FloatNatural
    decimal_exponent int
    binary_exponent int
    hexadecimal bool
    significant_digits int
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
    var inline852 bool = true
    var inline853 *ref_bool_x = ref__Ref_4bool(inline852)
    running__0 = inline853
    Loop_loop802:
    for {
        var t803 bool
        var inline846 bool = ref_get__Ref_4bool(running__0)
        t803 = inline846
        if t803 {
            var event__1 Event = Open
            switch event__1 {
            case Open:
                var scanning__2 *ref_bool_x
                var inline835 bool = false
                var inline836 *ref_bool_x = ref__Ref_4bool(inline835)
                scanning__2 = inline836
                Loop_loop810:
                for {
                    var t811 bool
                    var inline833 bool = ref_get__Ref_4bool(scanning__2)
                    t811 = inline833
                    if t811 {
                        continue
                    } else {
                        break Loop_loop810
                    }
                }
                var scanning__3 *ref_bool_x
                var inline843 bool = false
                var inline844 *ref_bool_x = ref__Ref_4bool(inline843)
                scanning__3 = inline844
                Loop_loop807:
                for {
                    var t808 bool
                    var inline838 bool = ref_get__Ref_4bool(scanning__3)
                    t808 = inline838
                    if t808 {
                        continue
                    } else {
                        break Loop_loop807
                    }
                }
                var inline840 bool = false
                ref_set__Ref_4bool(running__0, inline840)
                continue
            case Close:
                var scanning__3 *ref_bool_x
                var inline843 bool = false
                var inline844 *ref_bool_x = ref__Ref_4bool(inline843)
                scanning__3 = inline844
                Loop_loop807__2:
                for {
                    var t808 bool
                    var inline838 bool = ref_get__Ref_4bool(scanning__3)
                    t808 = inline838
                    if t808 {
                        continue
                    } else {
                        break Loop_loop807__2
                    }
                }
                var inline840 bool = false
                ref_set__Ref_4bool(running__0, inline840)
                continue
            case Advance:
                var scanning__3 *ref_bool_x
                var inline843 bool = false
                var inline844 *ref_bool_x = ref__Ref_4bool(inline843)
                scanning__3 = inline844
                Loop_loop807__3:
                for {
                    var t808 bool
                    var inline838 bool = ref_get__Ref_4bool(scanning__3)
                    t808 = inline838
                    if t808 {
                        continue
                    } else {
                        break Loop_loop807__3
                    }
                }
                var inline840 bool = false
                ref_set__Ref_4bool(running__0, inline840)
                continue
            case Error:
                var scanning__3 *ref_bool_x
                var inline843 bool = false
                var inline844 *ref_bool_x = ref__Ref_4bool(inline843)
                scanning__3 = inline844
                Loop_loop807__4:
                for {
                    var t808 bool
                    var inline838 bool = ref_get__Ref_4bool(scanning__3)
                    t808 = inline838
                    if t808 {
                        continue
                    } else {
                        break Loop_loop807__4
                    }
                }
                var inline840 bool = false
                ref_set__Ref_4bool(running__0, inline840)
                continue
            default:
                panic("non-exhaustive match")
            }
        } else {
            break Loop_loop802
        }
    }
    var inline848 string = "ok"
    var inline849 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(inline848)
    _goml_runtime_core_string_println(inline849)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
