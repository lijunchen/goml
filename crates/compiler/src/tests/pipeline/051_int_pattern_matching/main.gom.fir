struct PairData {
    head: int32,
    tail: int64,
}

fn is_special8(local/0: int8) -> bool {
  match value/0 {
      5i8 => true,
      7i8 => true,
      _ => false,
  };
}

fn is_special16(local/1: int16) -> bool {
  match value/1 {
      1024i16 => true,
      2048i16 => true,
      _ => false,
  };
}

fn is_special32(local/2: int32) -> bool {
  match value/2 {
      4096i32 => true,
      8192i32 => true,
      _ => false,
  };
}

fn is_special64(local/3: int64) -> bool {
  match value/3 {
      16384i64 => true,
      32768i64 => true,
      _ => false,
  };
}

fn match_tuple(local/4: (int8, int16)) -> bool {
  match values/4 {
      (1i8, 2i16) => true,
      _ => false,
  };
}

fn match_struct(local/5: PairData) -> bool {
  match pair/5 {
      PairData { head: 100i32, tail: 200i64 } => true,
      PairData { head: _, tail: 300i64 } => true,
      _ => false,
  };
}

fn report(local/6: string, local/7: bool) -> string {
  label/6 + bool_to_string(value/7);
}

fn main() {
  let local/8: int8 = 1i8;
  let local/9: int16 = 2i16;
  let local/10 = def/5((tuple_first/8, tuple_second/9));
  let local/11 = def/5((3i8, 4i16));
  let local/12 = def/6(PairData {
        head: 100i32,
        tail: 200i64
    });
  let local/13 = def/6(PairData {
        head: 10i32,
        tail: 300i64
    });
  let local/14 = def/7("int8=", def/1(5i8));
  let local/15 = def/7(",int16=", def/2(1024i16));
  let local/16 = def/7(",int32=", def/3(8192i32));
  let local/17 = def/7(",int64_a=", def/4(16384i64));
  let local/18 = def/7(",int64_b=", def/4(32768i64));
  let local/19 = def/7(",tuple_hit=", tuple_result_hit/10);
  let local/20 = def/7(",tuple_miss=", tuple_result_miss/11);
  let local/21 = def/7(",struct_first=", pair_first/12);
  let local/22 = def/7(",struct_second=", pair_second/13);
  let local/23 = part1/14 + part2/15 + part3/16 + part4/17 + part5/18 + part6/19 + part7/20 + part8/21 + part9/22;
  let _ = string_println(message/23);
  ();
}

