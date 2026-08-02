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
    var t158 string = self__0.name
    var t159 string = t158 + ":ready"
    return t159
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var t162 Worker = self__1.worker
    return t162
}

func main0() struct{} {
    var t164 Worker = Worker{
        name: "build",
    }
    var t165 Station = Station{
        worker: t164,
    }
    var t166 string
    var inline180 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(t165)
    var inline181 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(inline180)
    t166 = inline181
    var inline177 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t166)
    _goml_runtime_core_string_println(inline177)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__38 string) string {
    return self__38
}

func main() {
    main0()
}
