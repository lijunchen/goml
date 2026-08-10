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
    var t175 string = self__0.name
    var t176 string = t175 + ":ready"
    return t176
}

func _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(self__1 Station) Worker {
    var t179 Worker = self__1.worker
    return t179
}

func main0() struct{} {
    var t181 Worker = Worker{
        name: "build",
    }
    var t182 Station = Station{
        worker: t181,
    }
    var t183 string
    var inline197 Worker = _goml_m_trait__impl_i_Service_i__l_Worker_r__x40_Station_i_get(t182)
    var inline198 string = _goml_m_trait__impl_i_Ready_i_Worker_i_state(inline197)
    t183 = inline198
    var inline194 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t183)
    _goml_runtime_core_string_println(inline194)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func main() {
    main0()
}
