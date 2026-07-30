package main

import (
    _goml_fmt "fmt"
)

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

type Worker struct {
    name string
}

type Station struct {
    worker Worker
}

func _goml_m_trait__impl_i_Ready_i_Worker_i_state(self__0 Worker) string {
    var retv110 string
    var t111 string = self__0.name
    var t112 string = t111 + ":ready"
    retv110 = t112
    return retv110
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var retv114 Worker
    var t115 Worker = self__1.worker
    retv114 = t115
    return retv114
}

func main0() struct{} {
    var t117 Worker = Worker{
        name: "build",
    }
    var t118 Station = Station{
        worker: t117,
    }
    var t119 string = describe__S_Station__T_Worker(t118)
    println__T_string(t119)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t121 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t121)
    return struct{}{}
}

func describe__S_Station__T_Worker(service__2 Station) string {
    var retv124 string
    var t125 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(service__2)
    var t126 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(t125)
    retv124 = t126
    return retv124
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv128 string
    retv128 = self__38
    return retv128
}

func main() {
    main0()
}
