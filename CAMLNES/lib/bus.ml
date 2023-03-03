open Ppu_constants;;

let bus = Array.make 65536 0;;

let read addr =
  (*(if addr = _OAMDATA then failwith "OAMDATA is not supported"
  );*)
  
  bus.(addr);;

let oamdma msb =
  let bus_start_addr = msb lsl 8 in
  let offset = read _OAMADDR in
  for i = 0 to 255 do
    Ppumem.oam_write ((i + offset) mod 256) (read (bus_start_addr + i));
  done;;

let write addr byte =
  assert (0 <= byte && byte <= 255);
  bus.(addr) <- byte;

  (
    (*if addr = _OAMDATA then failwith "OAMDATA is not supported"*)
    if addr = _OAMDMA then oamdma byte
  );
;;
