fn child(local/0: Ref[int32]) -> unit {
  ref_set(signal/0, 1);
}

fn main() -> unit {
  let local/1 = ref(0);
  go || {
      def/0(signal/1);
  };
  while ref_get(signal/1) < 1 {
      {
          ();
      }
  };
  string_println("main");
  ();
}

