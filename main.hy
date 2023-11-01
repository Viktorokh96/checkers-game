; (import pygame)
; (.init pygame)
; (.init pygame.mixer)
; (setv screen (.set_mode pygame.display [360 480]))
; (input)

(import asyncio [run])
(import functools [partial])
(import ai [probabilistic-monster-ai])
(import game [run-game-loop wait-cli-human-movement-choice])

(run (run-game-loop 
    :configure-movement-src {
		; "white" wait-cli-human-movement-choice
		; "white" (partial probabilistic-monster-ai :res-depth 4)
		"white" (partial probabilistic-monster-ai :res-depth 3)
		"black" (partial probabilistic-monster-ai :res-depth 3)
	}
))
