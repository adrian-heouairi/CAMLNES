open Ppu_constants

let bus = Array.make 65536 0

let read addr =
  (*(if addr = _OAMDATA then failwith "OAMDATA is not supported"
    );*)
  bus.(addr)

let do_OAMDMA msb =
  let bus_start_addr = msb lsl 8 in
  let offset = read _OAMADDR in
  for i = 0 to 255 do
    Ppumem._OAM_write ((i + offset) mod 256) (read (bus_start_addr + i))
  done

let write addr byte =
  assert (0 <= byte && byte <= 255);
  bus.(addr) <- byte;

  if
    (*if addr = _OAMDATA then failwith "OAMDATA is not supported"*)
    addr = _OAMDMA
  then do_OAMDMA byte
