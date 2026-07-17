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
    var retv60 string
    var t61 string = self__0.name
    var t62 string = t61 + ":ready"
    retv60 = t62
    return retv60
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var retv64 Worker
    var t65 Worker = self__1.worker
    retv64 = t65
    return retv64
}

func main0() struct{} {
    var t67 Worker = Worker{
        name: "build",
    }
    var t68 Station = Station{
        worker: t67,
    }
    var t69 string = describe__S_Station__T_Worker(t68)
    println__T_string(t69)
    return struct{}{}
}

func println__T_string(value__1 string) struct{} {
    var t71 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(value__1)
    _goml_runtime_core_string_println(t71)
    return struct{}{}
}

func describe__S_Station__T_Worker(service__2 Station) string {
    var retv74 string
    var t75 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(service__2)
    var t76 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(t75)
    retv74 = t76
    return retv74
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__34 string) string {
    var retv78 string
    retv78 = self__34
    return retv78
}

func main() {
    main0()
}
