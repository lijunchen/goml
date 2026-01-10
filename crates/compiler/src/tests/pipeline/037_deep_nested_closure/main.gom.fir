fn main() {
  let local/0 = 10;
  let local/11 = |local/1: int32| {
      let local/2 = 20;
      let local/10 = |local/3: int32| {
          let local/4 = 30;
          let local/9 = |local/5: int32| {
              let local/6 = 40;
              let local/8 = |local/7: int32| {
                  a/0 + b/2 + c/4 + d/6 + x/1 + y/3 + z/5 + w/7;
              };
              f4/8(4);
          };
          f3/9(3);
      };
      f2/10(2);
  };
  let local/12 = f1/11(1);
  string_println(int32_to_string(result/12));
}

