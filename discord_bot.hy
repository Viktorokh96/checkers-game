(import discord)
(import discord.ext [commands])
(import game [checkers-game is-human-turn? do-turn])
(import movement [abs-route-from-rel-route])
(import utils [assoc])
(import view [draw-board])

(let [
    config {
        "token" "MTAxOTY1MjcyMTczNDg0NDQ0Ng.GyLSB2.5zuMx-2p0A2cM1d4FVChLTA_pRyj6L49Ob85CQ"
        "prefix" "!"
    }
    intents (discord.Intents.default)
]
    (setv game-sessions (dict))
    (setv intents.message_content True)
    (setv bot (.Bot commands :command_prefix (:prefix config) :intents intents))

    (defn gs [[g-s {}] [game None] [wm None]]
        {
            "game" (or game (:game g-s None))
            "wm" (or wm (:wm g-s None))
        }
    )

    (defn create-game-session [[user-team "white"]]
        (gs
            :game (checkers-game :human-team user-team)
            :wm {}
        )
    )

    (defn/a ask-human-about-turn [game-session ctx]
        (let [
            game (:game game-session)
            board (:board game)
            current-turn (:turn game)
            available-movements (get (:available-movements board) current-turn)
            movements-d (dfor [idx m] (enumerate available-movements :start 1) [idx m])
        ]
            (global game-sessions)
            (setv game-session (get game-sessions ctx.author))

            (setv game-sessions 
                (| game-sessions {
                    ctx.author (gs game-session :wm movements-d)
                }))

            (when available-movements
                (setv movements  
                    (str.join "\n" 
                        (lfor [idx m] (.items movements-d) 
                            (+ 
                                (str idx) 
                                " " 
                                (str.join 
                                    "-" 
                                    (abs-route-from-rel-route (:relroute m) (:team m)))
                                " "
                                (str.join
                                    "+"
                                    (abs-route-from-rel-route (:swallowed-ch m) (:team m)))
                            ))) 
                        )
                (await (ctx.reply (str.format "Выбери ход:\n{}" movements)))
            )
        )
    )

    (defn/a [bot.event] on-message [ctx]
        (global game-sessions)
        (when (!= ctx.author bot.user)
            (when (.startswith (.lower ctx.content) "давай игру")
                (await (ctx.reply "Один момент..."))
                (setv game-session (create-game-session))
                (setv game (:game game-session))
                (setv board (:board game))
                (await (ctx.reply "Готово! Вы играете за белых."
                    :file (.File discord (draw-board board))
                ))
                (setv game-sessions (| game-sessions {ctx.author game-session}))
                (when (is-human-turn? game)
                    (await (ask-human-about-turn game-session ctx))
                )
            )

            (when (and (str.isnumeric ctx.content) (in ctx.author game-sessions))
                (setv choosed-num (int ctx.content))
                (setv game-session (get game-sessions ctx.author))
                (setv game (:game game-session))
                (when (and (in choosed-num (:wm game-session)) (is-human-turn? game))
                    (setv choosed-movement (get (:wm game-session) choosed-num))
                    (setv updated-game (do-turn (do-turn game choosed-movement)))
                    (setv updated-gs (gs game-session updated-game))
                    (setv game-sessions (| game-sessions {
                        ctx.author updated-gs
                    }))

                    (setv board (:board updated-game))
                    (await (ctx.reply "Ход сделан!. Смотрите что получилось:"
                        :file (.File discord (draw-board board))
                    ))
                    (await (ask-human-about-turn updated-gs ctx))
                )
            )
        )
    )

    (.run bot (:token config))
)