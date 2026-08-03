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
    var t139 string = self__0.name
    var t140 string = t139 + ":ready"
    return t140
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var t143 Worker = self__1.worker
    return t143
}

func main0() struct{} {
    var t145 Worker = Worker{
        name: "build",
    }
    var t146 Station = Station{
        worker: t145,
    }
    var t147 string
    var inline161 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(t146)
    var inline162 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(inline161)
    t147 = inline162
    var inline158 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t147)
    _goml_runtime_core_string_println(inline158)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
