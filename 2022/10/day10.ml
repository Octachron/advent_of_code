[@@@warning "-unused-value-declaration"]
type instruction =
  | Add of int
  | Noop

let parse_instr = function
  | "noop" -> Noop
  | s -> Scanf.sscanf s "addx %d" (fun d -> Add d)

type cpu = {r1: int}
let cpu_start = { r1 = 1 }
let pp_cpu ppf cpu = Format.fprintf ppf "{r1=%d}" cpu.r1

let execute cycle cpu = function
  | Noop -> cycle + 1, cpu
  | Add x -> cycle + 2, { r1 = cpu.r1 + x }

module Signal_history = Map.Make(Int)
let pp_history ppf h =
  let pp_entry cycle cpu =
    Format.fprintf ppf "%d %a@," cycle pp_cpu cpu
  in
  Format.fprintf ppf "@[<v>";
  Signal_history.iter pp_entry h;
  Format.fprintf ppf "@]"


type state = { cycle:int; cpu:cpu; history: cpu Signal_history.t }

let event state instr =
  let cycle, cpu = execute state.cycle state.cpu instr in
  let history = Signal_history.add cycle cpu state.history in
  {cycle; cpu; history}

let (.!()) h p =
  match Signal_history.find_last (fun cycle -> cycle <= p) h with
  | _, cpu -> cpu
  | exception Not_found -> Helper.Pp.failf "cycle %d not found@." p

let strength c st =
  c * st.history.!(c).r1

let width = 40
let h = 6
let rec draw_screen cycle ppf history =
  if cycle >= h * width then () else
  let pos = history.!(cycle).r1 in
  let col = cycle mod width in
  let char = if abs(col - pos) <= 1 then "█" else " " in
  Format.fprintf ppf "%s" char;
  if ( (cycle+1) mod width = 0) then
    Format.fprintf ppf "@ ";
  draw_screen (cycle + 1) ppf history

let draw_screen ppf history = Format.fprintf ppf "@[<v>%a@]" (draw_screen 0) history

let () =
  let state = { cpu = cpu_start; history = Signal_history.singleton 0 cpu_start; cycle = 0 } in
  let f state x = event state (parse_instr x) in
  let state = Helper.Input.fold f state "10/data/input" in
  let points = List.init 6 (fun n -> 20 + 40 *n - 1) in
  let score = List.fold_left (fun s c -> s + strength c state) 0 points in
  Format.printf "strength=%d@." score;
  Format.printf "%a" draw_screen state.history
