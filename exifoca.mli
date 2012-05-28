module Orientation : sig
	type t =
   	| Horizontal
		| Mirror_horizontal
		| Rotate_180
		| Mirror_vertical
		| Mirror_horizontal_and_rotate_270_CW
		| Rotate_90_CW
		| Mirror_horizontal_and_rotate_90_CW
		| Rotate_270_CW
  end

module ResolutionUnit : sig
	type t =
		| Not_specified
		| Inches
		| Cm
end

module YCbCrPositioning : sig
	type t =
		| Centered
		| Co_sited
end

(* All unknown tags are silently skipped *)
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

(* Accepts content of JPEG file as an argument *)
val of_string : string -> t list

(* Accepts path to JPEG file as an argument *)
val of_file : string -> t list
