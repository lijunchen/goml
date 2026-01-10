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
      ctor(def/0::v0) => true,
      ctor(def/0::v1) => false,
  };
}

fn toggle_signal(local/1: Signal) -> Signal {
  match signal/1 {
      ctor(def/1::v0) => ctor(def/1::v1),
      ctor(def/1::v1) => ctor(def/1::v0),
  };
}

fn main() -> Signal {
  let local/2 = ctor(def/0::v0);
  let _ = def/2(current/2);
  def/3(ctor(def/1::v0));
}

