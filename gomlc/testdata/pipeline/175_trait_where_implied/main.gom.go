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
    var t180 string = self__0.name
    var t181 string = t180 + ":ready"
    return t181
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var t184 Worker = self__1.worker
    return t184
}

func main0() struct{} {
    var t186 Worker = Worker{
        name: "build",
    }
    var t187 Station = Station{
        worker: t186,
    }
    var t188 string
    var inline202 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(t187)
    var inline203 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(inline202)
    t188 = inline203
    var inline199 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t188)
    _goml_runtime_core_string_println(inline199)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__67 string) string {
    return self__67
}

func main() {
    main0()
}
