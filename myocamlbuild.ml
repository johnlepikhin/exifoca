open Ocamlbuild_plugin

let files = [
	"META";
	"exifoca.cmxa";
	"exifoca.cma";
	"exifoca.mli";
	"exifoca.a";
	"exifoca.cmi";
]

let rule_ocamlfind l _ _ = Cmd (S((A"ocamlfind") :: l))

let installer_rules ~files ~name =
	let deps = List.map (fun f -> f) files in
	let files = List.map (fun f -> A f) files in
	rule ("Install " ^ name) ~prod:"install" ~deps (rule_ocamlfind (A"install" :: A name :: files));
	rule ("Uninstall " ^ name) ~prod:"uninstall" ~deps:[] (rule_ocamlfind [A"remove"; A name]);
	rule ("Reinstall" ^ name) ~prod:"reinstall" ~deps:["uninstall"; "install"] (fun _ _ -> Cmd (S[A"/bin/true"]))

let _ =
	dispatch
		begin
			function
				| After_rules ->
					installer_rules ~files ~name:"exifoca"
				| _ ->
					()
		end
