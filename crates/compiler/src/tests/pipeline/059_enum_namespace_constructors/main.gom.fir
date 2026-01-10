enum Color {
    Red,
    Green
}

enum Signal {
    Red,
    Yellow
}

fn color_is_red(local/0: Color) -> bool {
  match color/0 {
      ctor/0 => true,
      ctor/1 => false,
  };
}

fn toggle_signal(local/1: Signal) -> Signal {
  match signal/1 {
      ctor/2 => ctor/3,
      ctor/3 => ctor/2,
  };
}

fn main() -> Signal {
  let local/2 = ctor/0;
  let _ = def/2(current/2);
  def/3(ctor/2);
}

