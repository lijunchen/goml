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
    var retv66 string
    var t67 string = self__0.name
    var t68 string = t67 + ":ready"
    retv66 = t68
    return retv66
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var retv70 Worker
    var t71 Worker = self__1.worker
    retv70 = t71
    return retv70
}

func main0() struct{} {
    var t73 Worker = Worker{
        name: "build",
    }
    var t74 Station = Station{
        worker: t73,
    }
    var t75 string = describe__S_Station__T_Worker(t74)
    println__T_string(t75)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t77 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t77)
    return struct{}{}
}

func describe__S_Station__T_Worker(service__2 Station) string {
    var retv80 string
    var t81 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(service__2)
    var t82 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(t81)
    retv80 = t82
    return retv80
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    var retv84 string
    retv84 = self__38
    return retv84
}

func main() {
    main0()
}
