
module Orientation = struct
	type t =
		| Horizontal
		| Mirror_horizontal
		| Rotate_180
		| Mirror_vertical
		| Mirror_horizontal_and_rotate_270_CW
		| Rotate_90_CW
		| Mirror_horizontal_and_rotate_90_CW
		| Rotate_270_CW

	let of_int = function
		| 1 -> Horizontal
		| 2 -> Mirror_horizontal
		| 3 -> Rotate_180
		| 4 -> Mirror_vertical
		| 5 -> Mirror_horizontal_and_rotate_270_CW
		| 6 -> Rotate_90_CW
		| 7 -> Mirror_horizontal_and_rotate_90_CW
		| 8 -> Rotate_270_CW
		| _ -> raise (failwith "Invalid Orientation")
end

module ResolutionUnit = struct
	type t =
		| Not_specified
		| Inches
		| Cm

	let of_int = function
		| 1 -> Not_specified
		| 2 -> Inches
		| 3 -> Cm
		| _ -> raise (failwith "Invalid ResolutionUnit")
end

module YCbCrPositioning = struct
	type t =
		| Centered
		| Co_sited

	let of_int = function
		| 1 -> Centered
		| 2 -> Co_sited
		| _ -> raise (failwith "Invalid YCbCrPositioning")
end

type t =
	| ImageDescription of string
	| Make of string
	| Model of string
	| Orientation of Orientation.t
	| XResolution of float
	| YResolution of float
	| ResolutionUnit of ResolutionUnit.t
	| Software of string
	| ModifyDate of string
	| YCbCrPositioning of YCbCrPositioning.t
	| ExifOffset of int
	| Padding of string
	| ThumbnailOffset of int
	| ThumbnailLength of int
	| Compression of int
	| GPSVersionID of int
	| GPSLatitudeRef of string
	| GPSLatitude of float
	| GPSLongitudeRef of string
	| GPSLongitude of float
	| GPSAltitudeRef of int
	| GPSAltitude of float

(*****************************************************************************************)

let error s pos =
	raise (failwith (Printf.sprintf "Error: %s at pos %i" s pos))

type byteorder =
	| Motorolla
	| Intel

module DFormat = struct
	type t =
		| UnsignedByte
		| AsciiString
		| UnsignedShort
		| UnsignedLong
		| UnsignedRational
		| SignedByte
		| Undefined
		| SignedShort
		| SignedLong
		| SignedRational
		| SingleFloat
		| DoubleFloat

	let of_int = function
		| 1 -> UnsignedByte
		| 2 -> AsciiString
		| 3 -> UnsignedShort
		| 4 -> UnsignedLong
		| 5 -> UnsignedRational
		| 6 -> SignedByte
		| 7 -> Undefined
		| 8 -> SignedShort
		| 9 -> SignedLong
		| 10 -> SignedRational
		| 11 -> SingleFloat
		| 12 -> DoubleFloat
		| e -> raise (failwith "Unknown data format")

	let name = function
		| UnsignedByte -> "UnsignedByte"
		| AsciiString -> "AsciiString"
		| UnsignedShort -> "UnsignedShort"
		| UnsignedLong -> "UnsignedLong"
		| UnsignedRational -> "UnsignedRational"
		| SignedByte -> "SignedByte"
		| Undefined -> "Undefined"
		| SignedShort -> "SignedShort"
		| SignedLong -> "SignedLong"
		| SignedRational -> "SignedRational"
		| SingleFloat -> "SingleFloat"
		| DoubleFloat -> "DoubleFloat"

	let length = function
		| UnsignedByte -> 1
		| AsciiString -> 1
		| UnsignedShort -> 2
		| UnsignedLong -> 4
		| UnsignedRational -> 8
		| SignedByte -> 1
		| Undefined -> 1
		| SignedShort -> 2
		| SignedLong -> 4
		| SignedRational -> 8
		| SingleFloat -> 4
		| DoubleFloat -> 8
end


let int8 s pos byte_order =
	let (lsl) c o = Int64.shift_left (Int64.of_int (Char.code c)) o in
	let (++) = Int64.add in
	match byte_order with
		| Motorolla ->
			(s.[pos] lsl 56)
			++ (s.[pos+1] lsl 48)
			++ (s.[pos+2] lsl 40)
			++ (s.[pos+3] lsl 32)
			++ (s.[pos+4] lsl 24)
			++ (s.[pos+5] lsl 16)
			++ (s.[pos+6] lsl 8)
			++ (Int64.of_int (Char.code s.[pos+7]))
		| Intel ->
			(s.[pos+7] lsl 56)
			++ (s.[pos+6] lsl 48)
			++ (s.[pos+5] lsl 40)
			++ (s.[pos+4] lsl 32)
			++ (s.[pos+3] lsl 24)
			++ (s.[pos+2] lsl 16)
			++ (s.[pos+1] lsl 8)
			++ (Int64.of_int (Char.code s.[pos]))

let int4 s pos = function
	| Motorolla ->
		((Char.code s.[pos]) lsl 24)
		+ ((Char.code s.[pos+1]) lsl 16)
		+ ((Char.code s.[pos+2]) lsl 8)
		+ (Char.code s.[pos+3])
	| Intel ->
		((Char.code s.[pos+3]) lsl 24)
		+ ((Char.code s.[pos+2]) lsl 16)
		+ ((Char.code s.[pos+1]) lsl 8)
		+ (Char.code s.[pos])

let int2 s pos = function
	| Motorolla ->
		((Char.code s.[pos]) lsl 8)
		+ (Char.code s.[pos+1])
	| Intel ->
		+ ((Char.code s.[pos+1]) lsl 8)
		+ (Char.code s.[pos])

let rational s pos byte_order =
	let num = int4 s pos byte_order in
	let denum = int4 s (pos+4) byte_order in
	(float_of_int num) /. (float_of_int denum)

let rec parse_main_ifd_entry ~save_f ~s ~pos ~baseoffset ~components ~byte_order = function
	| 0x010e ->
		let v = String.sub s pos (components-1) in
		save_f (ImageDescription v)
	| 0x010f ->
		let v = String.sub s pos (components-1) in
		save_f (Make v)
	| 0x0110 ->
		let v = String.sub s pos (components-1) in
		save_f (Model v)
	| 0x0112 ->
		let v = int2 s pos byte_order in
		save_f (Orientation (Orientation.of_int v))
	| 0x011a ->
		let v = rational s pos byte_order in
		save_f (XResolution v)
	| 0x011b ->
		let v = rational s pos byte_order in
		save_f (YResolution v)
	| 0x0128 ->
		let v = int2 s pos byte_order in
		save_f (ResolutionUnit (ResolutionUnit.of_int v))
	| 0x0131 ->
		let v = String.sub s pos (components-1) in
		save_f (Software v)
	| 0x0132 ->
		let v = String.sub s pos (components-1) in
		save_f (ModifyDate v)
	| 0x0213 ->
		let v = int2 s pos byte_order in
		save_f (YCbCrPositioning (YCbCrPositioning.of_int v))
	| 0x8769 ->
		let v = int4 s pos byte_order in
		save_f (ExifOffset v)
	| 0xea1c ->
		let v = String.sub s pos (components-1) in
		save_f (Padding v)
	| 0x0201 ->
		let v = int4 s pos byte_order in
		save_f (ThumbnailOffset  v)
	| 0x0202 ->
		let v = int4 s pos byte_order in
		save_f (ThumbnailLength  v)
	| 0x0103 ->
		let v = int2 s pos byte_order in
		save_f (Compression v)
	| 0x8825 ->
		ifd_of_string ~save_f ~s ~baseoffset ~parser_f:parse_gps_ifd_entry ~pos byte_order;
	| _ ->
		(* TODO *)
		()

and parse_gps_ifd_entry ~save_f ~s ~pos ~baseoffset ~components ~byte_order = function
	| 0x0000 ->
		let v = int2 s pos byte_order in
		save_f (GPSVersionID v)
	| 0x0001 ->
		let v = String.sub s pos (components-1) in
		save_f (GPSLatitudeRef v)
	| 0x0002 ->
		let v1 = rational s pos byte_order in
		let v2 = rational s (pos+8) byte_order in
		let v3 = rational s (pos+16) byte_order in
		let v = v1 +. (v2 /. 60.) +. (v3 /. 3600.) in
		save_f (GPSLatitude v)
	| 0x0003 ->
		let v = String.sub s pos (components-1) in
		save_f (GPSLongitudeRef v)
	| 0x0004 ->
		let v1 = rational s pos byte_order in
		let v2 = rational s (pos+8) byte_order in
		let v3 = rational s (pos+16) byte_order in
		let v = v1 +. (v2 /. 60.) +. (v3 /. 3600.) in
		save_f (GPSLongitude v)
	| 0x0005 ->
		let v = int4 s pos byte_order in
		save_f (GPSAltitudeRef v)
	| 0x0006 ->
		let v = rational s pos byte_order in
		save_f (GPSAltitude v)
	| _ ->
		(* TODO *)
		()

and read_ifd_entry ~save_f ~s ~baseoffset ~parser_f ~pos byte_order =
	let tag = int2 s pos byte_order in
	let dformat = int2 s (pos+2) byte_order in
	let dformat = DFormat.of_int dformat in
	let components = int4 s (pos+4) byte_order in
	let total_length = components * (DFormat.length dformat) in
	let offset = int4 s (pos+8) byte_order in
	let pos =
		if total_length <= 4 then
			pos+8
		else
			baseoffset + offset
	in
	match tag with
		| 0x8825 -> ifd_of_string ~save_f ~s ~baseoffset ~parser_f:parse_gps_ifd_entry ~pos:(baseoffset + offset) byte_order
		| tag -> parser_f ~save_f ~s ~pos ~baseoffset ~components ~byte_order tag

and ifd_of_string ~save_f ~s ~baseoffset ~parser_f ~pos byte_order =
	let entry_count = int2 s pos byte_order in
	let rec loop n pos =
		if n = entry_count then
			()
		else
		begin
			read_ifd_entry ~save_f ~s ~baseoffset ~parser_f ~pos byte_order;
			loop (n+1) (pos+12)
		end
	in
	loop 0 (pos+2);
	let next_ifd_offset = int4 s (pos+2+entry_count*12) byte_order in
	if next_ifd_offset > 0 then
		ifd_of_string ~save_f ~s ~baseoffset ~parser_f ~pos:(next_ifd_offset+baseoffset) byte_order
	else
		()

let all_ifd_of_string ~tags ~s ~baseoffset ~pos byte_order =
	let save_f v =
		tags := v :: !tags
	in
	ifd_of_string ~save_f ~s ~baseoffset ~parser_f:parse_main_ifd_entry ~pos byte_order

let exif ~tags ~s pos =
	if (String.sub s pos 6) = "Exif\000\000" then
	begin
		let pos = pos+6 in
		let baseoffset = pos in
		let byte_order =
			match s.[pos], s.[pos+1] with
				| 'M', 'M' -> Motorolla
				| 'I', 'I' -> Intel
				| _ -> error "invalid byte order" pos
		in
		let ifd_offset = int4 s (pos+4) byte_order in
		all_ifd_of_string ~tags ~s ~baseoffset ~pos:(pos+ifd_offset) byte_order
	end
	else
		()

let of_string s =
	let tags = ref [] in
	let rec loop pos =
		if s.[pos] <> '\xff' then
			error "invalid marker" pos
		else ();
		let get_size () =
			((Char.code s.[pos+2]) lsl 8) + (Char.code s.[pos+3])
		in
		match s.[pos+1] with
			| '\xda' (* begin data *)
			| '\xd9' (* end of image *)
				-> ()
			| '\xe1' ->
				let size = get_size () in
				exif ~tags ~s (pos+4);
				loop (pos+size+2)
			| c ->
				let size = get_size () in
				loop (pos+size+2)
	in
	loop 2;
	!tags

let of_file s =
	let ch = open_in s in
	let r = Buffer.create 100000 in
	let s = String.create 100000 in
	let rec loop () =
		let rb = input ch s 0 100000 in
		if rb = 0 then
			()
		else
		begin
			Buffer.add_substring r s 0 rb;
			loop ()
		end
	in
	loop ();
	close_in ch;
	of_string (Buffer.contents r)
