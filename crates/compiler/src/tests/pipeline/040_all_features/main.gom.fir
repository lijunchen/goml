struct Tracker {
    label: string,
    count: Ref[int32],
    toggled: Ref[bool],
}

enum Record[T] {
    Value(T),
    Pair(T, T),
    Empty
}

enum Maybe[T] {
    Some(T),
    None
}

trait Describe {
  fn describe(Self) -> string;
}

impl Describe for Tracker {
  fn describe(local/0: Tracker) -> string {
    let Tracker { label: local/1, count: local/2, toggled: local/3 } = self/0;
    let local/4 = ref_get(count/2);
    let local/5 = ref_get(toggled/3);
    let local/6 = "Tracker(" + label/1;
    let local/7 = with_label/6 + ", count: ";
    let local/8 = with_count_label/7 + int32_to_string(current/4);
    let local/9 = with_count/8 + ", toggled: ";
    with_flag_label/9 + bool_to_string(flag/5) + ")";
  }
}

impl Describe for Record[int32] {
  fn describe(local/10: Record[int32]) -> string {
    match self/10 {
        ctor/0(local/11) => "Value(" + int32_to_string(value/11) + ")",
        ctor/1(local/12, local/13) => {
              let local/14 = "Pair(" + int32_to_string(before/12);
              prefix/14 + ", " + int32_to_string(after/13) + ")";
          },
        ctor/2 => "Empty",
    };
  }
}

impl Describe for Record[string] {
  fn describe(local/15: Record[string]) -> string {
    match self/15 {
        ctor/0(local/16) => "Value(" + text/16 + ")",
        ctor/1(local/17, local/18) => {
              let local/19 = "Pair(" + before/17;
              prefix/19 + ", " + after/18 + ")";
          },
        ctor/2 => "Empty",
    };
  }
}

fn choose(local/20: bool, local/21: T, local/22: T) -> T {
  if flag/20 {
      {
          when_true/21;
      }
  } else {
      {
          when_false/22;
      }
  };
}

fn map_maybe(local/23: Maybe[T], local/24: (T) -> U) -> Maybe[U] {
  match value/23 {
      ctor/3(local/25) => ctor/3(f/24(inner/25)),
      ctor/4 => ctor/4,
  };
}

fn format_total(local/26: int32) -> string {
  "total: " + int32_to_string(total/26);
}

fn increment(local/27: int32) -> int32 {
  value/27 + 1;
}

fn triple(local/28: int32) -> int32 {
  value/28 * 3;
}

fn pair_join(local/29: (string, string)) -> string {
  let (local/30, local/31) = parts/29;
  left/30 + " -> " + right/31;
}

fn run_transforms(local/32: int32, local/33: [(int32) -> int32; 2]) -> [int32; 2] {
  let local/34 = array_get(transforms/33, 0);
  let local/35 = array_get(transforms/33, 1);
  let local/36 = first/34(value/32);
  let local/37 = second/35(first_result/36);
  array_set([first_result/36, value/32], 1, second_result/37);
}

fn gather(local/38: Record[int32]) -> Maybe[int32] {
  match record/38 {
      ctor/0(local/39) => ctor/3(value/39),
      ctor/1(_, local/40) => ctor/3(after/40),
      ctor/2 => ctor/4,
  };
}

fn build_counter(local/41: string, local/42: int32) -> (Tracker, () -> Record[int32], (int32) -> Record[int32], () -> Record[string]) {
  let local/43 = ref(start/42);
  let local/44 = ref(false);
  let local/45 = Tracker {
      label: label/41,
      count: count/43,
      toggled: toggled/44
  };
  let local/46 = || {
      ctor/0(ref_get(count/43));
  };
  let local/49 = |local/47: int32| {
      let local/48 = ref_get(count/43);
      let _ = ref_set(count/43, before/48 + delta/47);
      ctor/1(before/48, ref_get(count/43));
  };
  let local/52 = || {
      let local/50 = ref_get(toggled/44);
      let _ = ref_set(toggled/44, !before/50);
      let local/51 = ref_get(toggled/44);
      ctor/1(bool_to_string(before/50), bool_to_string(after/51));
  };
  (tracker/45, snapshot/46, bump/49, flip/52);
}

fn main() {
  let (local/53, local/54, local/55, local/56) = def/15("goml", 2);
  let local/57 = Describe::describe(tracker/53);
  let local/58 = snapshot/54();
  let local/59 = bump/55(5);
  let local/60 = flip/56();
  let local/61 = def/14(first_record/58);
  let local/62 = def/14(bumped_record/59);
  let local/63 = def/7(true, maybe_second/62, maybe_first/61);
  let local/64 = def/8(chosen/63, def/9);
  let local/65 = [def/10, def/11];
  let local/66 = def/13(4, transforms/65);
  let local/67 = array_get(results/66, 0);
  let local/68 = array_get(results/66, 1);
  let local/69 = first_result/67 < second_result/68 && true;
  let local/70 = Describe::describe(first_record/58);
  let local/71 = Describe::describe(bumped_record/59);
  let local/72 = Describe::describe(flipped_record/60);
  let local/74 = match stringified/64 {
      ctor/3(local/73) => "Snapshot: " + text/73,
      ctor/4 => "Snapshot: none",
  };
  let local/75 = def/12((int32_to_string(first_result/67), int32_to_string(second_result/68)));
  let local/76 = bool_to_string(order_check/69);
  let _ = string_println(tracker_info/57);
  let _ = string_println(first_text/70);
  let _ = string_println(bumped_text/71);
  let _ = string_println(flipped_text/72);
  let _ = string_println(summary/74);
  let _ = string_println(pair_text/75);
  let _ = string_println(bool_text/76);
  ();
}

