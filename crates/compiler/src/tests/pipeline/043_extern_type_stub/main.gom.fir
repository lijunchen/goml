extern type Time

extern type Duration

extern "go" "time" unix(secs: int32, nanos: int32) -> Time

extern "go" "time" duration(nanos: int32) -> Duration

extern "go" "fmt" "Sprintf" format_time(format: string, value: Time) -> string

extern "go" "fmt" "Sprintf" format_duration(format: string, value: Duration) -> string

fn describe_epoch() -> string {
  let local/0 = def/2(946684800, 500000000);
  def/4("epoch snapshot => %v", epoch/0);
}

fn describe_planned_launch() -> string {
  let local/1 = def/2(1709294730, 250000000);
  def/4("planned launch => %v", launch/1);
}

fn describe_duration() -> string {
  let local/2 = def/3(1500000000);
  def/5("parsed duration => %v", parsed/2);
}

fn main() -> unit {
  string_println(def/6());
  string_println(def/7());
  string_println(def/8());
  ();
}

