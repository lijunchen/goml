package main

import (
    _goml_fmt "fmt"
    _goml_sync "sync"
    _goml_runtime_pkg "runtime"
)

func _goml_once_cell_goroutine_id() uint64 {
    var buffer []uint8 = make([]uint8, 64)
    var length int = _goml_runtime_pkg.Stack(buffer, false)
    var index int = 10
    var result uint64 = 0
    for {
        if index >= length {
            break
        }
        if buffer[index] < 48 || buffer[index] > 57 {
            break
        }
        result = result * 10 + uint64(buffer[index] - 48)
        index = index + 1
    }
    return result
}

func _goml_runtime_core_bool_to_string(x bool) string {
    if x {
        return "true"
    } else {
        return "false"
    }
}

func _goml_runtime_core_string_println(s string) struct{} {
    _goml_fmt.Println(s)
    return struct{}{}
}

func once_cell_new__OnceCell__int() *OnceCell__int {
    var cell *OnceCell__int = &OnceCell__int{}
    cell.cond = _goml_sync.NewCond(&cell.mutex)
    return cell
}

func once_cell_get_or_init__OnceCell__int(cell *OnceCell__int, init func() int) int {
    var goroutine uint64 = _goml_once_cell_goroutine_id()
    cell.mutex.Lock()
    for {
        if cell.state == 2 {
            cell.mutex.Unlock()
            return cell.value
        }
        if cell.state == 1 {
            if cell.owner == goroutine {
                cell.mutex.Unlock()
                panic("recursive OnceCell initialization: " + cell.name)
            }
            cell.cond.Wait()
            continue
        }
        cell.state = 1
        cell.owner = goroutine
        cell.mutex.Unlock()
        var initialized int = init()
        cell.mutex.Lock()
        cell.value = initialized
        cell.state = 2
        cell.owner = 0
        cell.cond.Broadcast()
        cell.mutex.Unlock()
        return initialized
    }
}

type Tuple2_3int_4bool struct {
    _0 int
    _1 bool
}

type closure_env_main_0 struct {}

type closure_env_main_1 struct {
    results_0 chan int
}

type closure_env_main_2 struct {}

type closure_env_main_3 struct {
    results_0 chan int
}

type OnceCell__int struct {
    mutex _goml_sync.Mutex
    cond *_goml_sync.Cond
    state int
    owner uint64
    value int
    name string
}

type Option__int interface {
    isOption__int()
}

type None struct {}

func (_ None) isOption__int() {}

type Some struct {
    _0 int
}

func (_ Some) isOption__int() {}

var VALUE *OnceCell__int = func() *OnceCell__int {
    var cell *OnceCell__int = once_cell_new__OnceCell__int()
    cell.name = "VALUE"
    return cell
}()

func main0() struct{} {
    var results__0 chan int
    var inline270 int = 2
    var inline271 chan int = func(p0 int) chan int {
        return make(chan int, p0)
    }(inline270)
    results__0 = inline271
    var t194 closure_env_main_1 = closure_env_main_1{
        results_0: results__0,
    }
    var t195 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(t194)
    }
    go t195()
    var t196 closure_env_main_3 = closure_env_main_3{
        results_0: results__0,
    }
    var t197 func() struct{} = func() struct{} {
        return _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(t196)
    }
    go t197()
    var t198 Option__int
    var inline263 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(results__0)
    var inline264 int = inline263._0
    var inline265 bool = inline263._1
    if inline265 {
        var inline268 Option__int = Some{
            _0: inline264,
        }
        t198 = inline268
    } else {
        t198 = None{}
    }
    var first__1 int
    var inline259 int = 0
    switch t198.(type) {
    case None:
        first__1 = inline259
    case Some:
        var inline260 int = t198.(Some)._0
        first__1 = inline260
    default:
        panic("non-exhaustive match")
    }
    var t199 Option__int
    var inline252 Tuple2_3int_4bool = func(p0 chan int) Tuple2_3int_4bool {
        var value int
        var ok bool
        value, ok = <-p0
        return Tuple2_3int_4bool{
            _0: value,
            _1: ok,
        }
    }(results__0)
    var inline253 int = inline252._0
    var inline254 bool = inline252._1
    if inline254 {
        var inline257 Option__int = Some{
            _0: inline253,
        }
        t199 = inline257
    } else {
        t199 = None{}
    }
    var second__2 int
    var inline248 int = 0
    switch t199.(type) {
    case None:
        second__2 = inline248
    case Some:
        var inline249 int = t199.(Some)._0
        second__2 = inline249
    default:
        panic("non-exhaustive match")
    }
    var t200 bool = first__1 == second__2
    var t201 string
    var inline246 string = _goml_runtime_core_bool_to_string(t200)
    t201 = inline246
    var inline243 string = _goml_m_trait__impl_i_ToString_i_string_i_to__string(t201)
    _goml_runtime_core_string_println(inline243)
    return struct{}{}
}

func _goml_m_trait__impl_i_ToString_i_string_i_to__string(self__65 string) string {
    return self__65
}

func _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(env189 closure_env_main_0) int {
    return 41
}

func _goml_m_inherent_i_closure__env__main__1_i_closure__env__main__1_i_apply(env190 closure_env_main_1) struct{} {
    var results__0 chan int = env190.results_0
    var t231 closure_env_main_0 = closure_env_main_0{}
    var t232 func() int = func() int {
        return _goml_m_inherent_i_closure__env__main__0_i_closure__env__main__0_i_apply(t231)
    }
    var t233 int
    var inline276 int = once_cell_get_or_init__OnceCell__int(VALUE, t232)
    t233 = inline276
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(results__0, t233)
    return struct{}{}
}

func _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(env191 closure_env_main_2) int {
    return 42
}

func _goml_m_inherent_i_closure__env__main__3_i_closure__env__main__3_i_apply(env192 closure_env_main_3) struct{} {
    var results__0 chan int = env192.results_0
    var t238 closure_env_main_2 = closure_env_main_2{}
    var t239 func() int = func() int {
        return _goml_m_inherent_i_closure__env__main__2_i_closure__env__main__2_i_apply(t238)
    }
    var t240 int
    var inline280 int = once_cell_get_or_init__OnceCell__int(VALUE, t239)
    t240 = inline280
    func(p0 chan int, p1 int) struct{} {
        p0 <- p1
        return struct{}{}
    }(results__0, t240)
    return struct{}{}
}

func main() {
    main0()
}
