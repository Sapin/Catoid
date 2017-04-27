
let () =
	if Array.length Sys.argv > 1 then
	let prog = Vm.load Sys.argv.(1) in
	let _ = Vm.run Vm.env_empty prog in
	()

