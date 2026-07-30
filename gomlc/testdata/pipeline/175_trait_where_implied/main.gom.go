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
    var retv70 string
    var t71 string = self__0.name
    var t72 string = t71 + ":ready"
    retv70 = t72
    return retv70
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var retv74 Worker
    var t75 Worker = self__1.worker
    retv74 = t75
    return retv74
}

func main0() struct{} {
    var t77 Worker = Worker{
        name: "build",
    }
    var t78 Station = Station{
        worker: t77,
    }
    var t79 string = describe__S_Station__T_Worker(t78)
    println__T_string(t79)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t81 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t81)
    return struct{}{}
}

func describe__S_Station__T_Worker(service__2 Station) string {
    var retv84 string
    var t85 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(service__2)
    var t86 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(t85)
    retv84 = t86
    return retv84
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv88 string
    retv88 = self__38
    return retv88
}

func main() {
    main0()
}
