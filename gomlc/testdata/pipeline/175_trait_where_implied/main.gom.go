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
    var t190 string = self__0.name
    var t191 string = t190 + ":ready"
    return t191
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var t194 Worker = self__1.worker
    return t194
}

func main0() struct{} {
    var t196 Worker = Worker{
        name: "build",
    }
    var t197 Station = Station{
        worker: t196,
    }
    var t198 string
    var inline212 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(t197)
    var inline213 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(inline212)
    t198 = inline213
    var inline209 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t198)
    _goml_runtime_core_string_println(inline209)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
