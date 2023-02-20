open CAMLNES;;
(*open Tsdl;;*)

Init.init Sys.argv.(1);;

(* Here loop that executes CPU and PPU: *)

(*début_de_frame = Unix.timestamp
set cpu.nmi if nmis are activated
exécuter le cpu pour approximativement une frame (en général il se bloquera dans une boucle)
render image
sleep autant qu'il faut jusqu'à la fin de la durée d'une frame selon $début_de_frame
recommencer
obtenir plusieurs longueurs comme durée d'un cycle, d'une frame etc*)