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
    var t185 string = self__0.name
    var t186 string = t185 + ":ready"
    return t186
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var t189 Worker = self__1.worker
    return t189
}

func main0() struct{} {
    var t191 Worker = Worker{
        name: "build",
    }
    var t192 Station = Station{
        worker: t191,
    }
    var t193 string
    var inline207 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(t192)
    var inline208 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(inline207)
    t193 = inline208
    var inline204 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t193)
    _goml_runtime_core_string_println(inline204)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
