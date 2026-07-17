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
    var retv63 string
    var t64 string = self__0.name
    var t65 string = t64 + ":ready"
    retv63 = t65
    return retv63
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var retv67 Worker
    var t68 Worker = self__1.worker
    retv67 = t68
    return retv67
}

func main0() struct{} {
    var t70 Worker = Worker{
        name: "build",
    }
    var t71 Station = Station{
        worker: t70,
    }
    var t72 string = describe__S_Station__T_Worker(t71)
    println__T_string(t72)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t74 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t74)
    return struct{}{}
}

func describe__S_Station__T_Worker(service__2 Station) string {
    var retv77 string
    var t78 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(service__2)
    var t79 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(t78)
    retv77 = t79
    return retv77
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__37 string) string {
    var retv81 string
    retv81 = self__37
    return retv81
}

func main() {
    main0()
}
