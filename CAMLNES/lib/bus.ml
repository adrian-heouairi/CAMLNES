let bus = Array.make 65536 0;;

let read addr = bus.(addr);;

let write addr byte =
  assert (0 <= byte && byte <= 255);
  bus.(addr) <- byte;;

let load_PRG_bank addr _PRG_bank = (* Bank is necessarily of size 16384 *)
  for i = addr to addr + 16383 do
    write i _PRG_bank.(i - addr)
  done;;
