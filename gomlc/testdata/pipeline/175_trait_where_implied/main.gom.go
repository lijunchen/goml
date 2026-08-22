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

type Worker struct {
    name string
}

type Station struct {
    worker Worker
}

type Ordering int32

func _goml_m_trait__impl_i_Ready_i_Worker_i_state(self__0 Worker) string {
    var t799 string = self__0.name
    var t800 string = t799 + ":ready"
    return t800
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var t803 Worker = self__1.worker
    return t803
}

func main0() struct{} {
    var t805 Worker = Worker{
        name: "build",
    }
    var t806 Station = Station{
        worker: t805,
    }
    var t807 string
    var inline821 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(t806)
    var inline822 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(inline821)
    t807 = inline822
    var inline818 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t807)
    _goml_runtime_core_string_println(inline818)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__402 string) string {
    return self__402
}

func main() {
    main0()
}
