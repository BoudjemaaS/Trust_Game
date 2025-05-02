breed [players playername] ; type de joueurs
breed [labels nametag]  ; étiquettes

; https://ncase.me/trust/


; afficher la liste des joueurs à être éliminé avant le evolve

; custom-initial-moves-list en input
; ajouter autant de joueurs custom différents, avec un bouton "créer" avec les paramètres setup

labels-own [
  player-id
]
players-own [
  coins
  id-placement
  move-played
  strategy
  last-move-opponent
  betrayed-by

  opponent-memories  ; structure: [[opponent-id [move1 move2 ...]] [opponent-id [move1 move2 ...]] ...]

  move-count
  rounds-played
  matched?
  coins-gained
  consecutive-betrayed

  betrayed-copykitten
  detective-always-lying-against

  ; custom
  custom-initial-moves-list-local
  custom-repeat-initial-moves-local
  custom-betray-after-local
  custom-consecutive-betray-after-local
  custom-truth-percent-local
  custom-play-randomly-local
  custom-play-opponent-last-move-local

]

globals [
  human-decision
  round-number
  elimination-round
  next-elimination-round
  players-to-eliminate
  players-to-replicate
  current-match-pair  ; Paire actuelle de joueurs en match
  remaining-matches  ; Liste des matchs restants dans le tour
  paused?
  next-id-placement  ; Compteur global pour les nouvelles positions
  number_rounds
  worst-player-ids
  worst-player-strat
  best-player-strat
  conv-custom-initial-moves-list
]

; ------------------------------------------- CONTROLE DE L'INTERFACE -------------------------------------------

to setup
  clear-all
  clear-output
  reset-ticks

  resize-world -24 24 -24 24

  set human-decision 1
  set round-number 1
  set current-match-pair []
  set remaining-matches []
  set next-id-placement 0  ; Initialiser le compteur à 0

  ; initialisation dans l'interface
  set elimination-round round_av_evolution
  set next-elimination-round round_av_evolution
  set players-to-eliminate nb_worst_player
  set players-to-replicate nb_best_player

  set conv-custom-initial-moves-list read-from-string custom-initial-moves-list


  set number_rounds 1

  create-all-players
  setup-labels
  ask patches [ set pcolor black ]

  ; Initialiser les matchs pour le premier tour
  prepare-all-matches
end

to go

  ; Si aucun match en cours, commencer un nouveau tour
  if empty? remaining-matches [
    start-new-round
  ]

  play-next-match

end


to one-round-only

  repeat length remaining-matches [
    play-next-match
  ]

  ;start-new-round

end


to-report format-matches [match-list]
  let result ""
  foreach match-list [
    [current-match] ->
    let player1 first current-match
    let player2 last current-match
    let str1 word [strategy] of player1 [id-placement] of player1
    let str2 word [strategy] of player2 [id-placement] of player2
    set result (word result str1 " vs " str2 " | ")  ; \n pour un saut de ligne
  ]
  ifelse (result = "") [ report "Aucun match restant" ] [ report result ]
end

; ------------------------------------------- PLAYER -------------------------------------------

to create-all-players
  ask players [ die ]
  set next-id-placement 0  ; Réinitialisation du compteur

  let radius 15
  let total-players (human-count + bot-count + copycat-count + cheater-count +
    cooperator-count + grudger-count + detective-count +
    simpleton-count + copykitten-count + custom-count)
  let angle-step ifelse-value (total-players > 0) [ 360 / total-players ] [ 0 ]
  let current-angle 0

  ; Liste des types dans l'ordre souhaité
  let player-types [
    ["human" 7.9] ["bot" yellow] ["copycat" blue] ["cheater" red]
    ["cooperator" green] ["grudger" 115] ["detective" 75]
    ["simpleton" 53] ["copykitten" cyan] ["custom" pink]
  ]
  let counts (list human-count bot-count copycat-count cheater-count cooperator-count grudger-count detective-count simpleton-count copykitten-count custom-count)

  ; Création séquentielle triée
  foreach player-types [
    [type-info] ->
    let strategy-play first type-info
    let couleur last type-info
    let n first counts
    set counts but-first counts

    create-players n [
      setup-player strategy-play couleur current-angle radius
      set current-angle current-angle + angle-step
      set id-placement next-id-placement
      set next-id-placement next-id-placement + 1

    ]
  ]
end

to create-custom-player



end

to setup-player [strategie couleur angle radius]
  set shape "person"
  set size 3
  let x (radius * cos angle)
  let y (radius * sin angle)
  setxy x y
  facexy 0 0
  set strategy strategie
  set move-played 2
  set color couleur
  set coins 0

  set betrayed-by []
  set last-move-opponent 1
  set opponent-memories []
  set move-count 0
  set rounds-played 0

  set betrayed-copykitten []
  set detective-always-lying-against []
end


; ------------------------------------------- LABEL -------------------------------------------

to setup-labels
  ask players [
    hatch-labels 1 [
      set player-id [who] of myself
      set shape "line half"
      set size 3

      update-label-position
    ]
  ]
end


to set-names [ target ]
  ; Trouver le dernier adversaire joué
  let last-opponent-id ""
  let last-opponent-strategy ""
  let last-opponent-position ""

  ; Parcourir les mémoires pour trouver le dernier adversaire
  foreach [opponent-memories] of target [
    [record] ->
    let moves last record
    if not empty? moves [
      set last-opponent-id first record
      let opponent-player one-of players with [who = last-opponent-id]
      if opponent-player != nobody [
        set last-opponent-strategy [strategy] of opponent-player
        set last-opponent-position [id-placement] of opponent-player
      ]
    ]
  ]

  if [move-played] of target = 1 [
    set label (word [strategy] of target "\n" [coins] of target " coins \n(Vérité)")
  ]
  if [move-played] of target = 0 [
    set label (word [strategy] of target "\n" [coins] of target " coins \n(Mensonge)")
  ]
  if [move-played] of target != 1 and [move-played] of target != 0 [
    set label (word [strategy] of target "\n" [coins] of target " coins")
  ]
end

to-report get-player-history [id-target]
  let player one-of players with [who = id-target]
  if player = nobody [ report "Joueur non trouvé" ]

  let history ""
  let mem [opponent-memories] of player

  foreach mem [
    [record] ->
    let opponent-id first record
    let moves last record
    let opponent one-of players with [who = opponent-id]

    if opponent != nobody [
      set history (word history
                  "vs " [strategy] of opponent " " [id-placement] of opponent " : ")

      foreach moves [
        move ->
        set history (word history ifelse-value (move = 1) ["1"] ["0"])
      ]

      set history (word history "\n")
    ]
  ]

  if history = "" [ set history "Aucun historique de match" ]

  report (word "Historique de " [strategy] of player " " [id-placement] of player ":\n" history)
end

to update-label-position

  ask labels [
    let target one-of players with [who = [player-id] of myself]
    if target != nobody [
      ; Utilise la position du joueur comme référence
      let player-x [xcor] of target
      let player-y [ycor] of target

      ; Calcule la position du label (décalage fixe par rapport au joueur)
      let label-distance -4  ; Distance en patches entre joueur et label
      let angle-towards-center atan player-y player-x ; Angle vers le centre

      ; Positionne le label dans la direction opposée au centre
      setxy player-x + (label-distance * cos (angle-towards-center + 180))
      player-y + (label-distance * sin (angle-towards-center + 180))

      ; Mise à jour du texte et couleur
      set-names target
      set label-color [color] of target + 2
    ]
  ]

end

to update-labels
  ask labels [
    let target one-of players with [who = [player-id] of myself]
    if target != nobody [

      set-names target
    ]
  ]
end

to add-move-to-memory [player opponent-id move]
  ; Trouve ou crée une entrée mémoire pour cet adversaire
  let mem-record [opponent-memories] of player
  let found false
  let new-mem []

  ; Parcourir les enregistrements existants
  foreach mem-record [
    [record] ->
    if first record = opponent-id [
      ; Mettre à jour l'enregistrement existant
      set found true
      set new-mem lput (list opponent-id (lput move last record)) new-mem
    ]
    if first record != opponent-id [
      ; Garder les autres enregistrements inchangés
      set new-mem lput record new-mem
    ]
  ]

  ; Si aucun enregistrement trouvé pour cet adversaire, en créer un nouveau
  if not found [
    set new-mem lput (list opponent-id (list move)) new-mem
  ]

  let opponent-player one-of players with [who = opponent-id]
  if opponent-player != nobody [
    ;show (word "Match: " [strategy] of player " (ID:" [who] of player ") vs "
     ; [strategy] of opponent-player " (ID:" [who] of opponent-player ") - Memory: " new-mem)
  ]

  ;show "--------"
  ask player [ set opponent-memories new-mem ]
end


to-report get-moves-against [player opponent-id]
  ; Retourne la liste des coups contre un adversaire spécifique
  foreach [opponent-memories] of player [
    [record] ->
    if first record = opponent-id [
      report last record  ; Retourne la liste des coups
    ]
  ]
  report []  ; Retourne liste vide si pas trouvé
end

; ------------------------------------------- EVOLUTION DE LA POPULATION -------------------------------------------

to evolve-population
  let all-players sort-on [coins] players
  if empty? all-players [ print "Attention: liste de joueurs vide!" stop ]
  set worst-player-ids map [p -> [who] of p] sublist all-players 0 nb_worst_player
  set worst-player-strat map [p -> [strategy] of p] sublist all-players 0 nb_worst_player

  let best-player-ids map [p -> [who] of p] reverse sublist all-players (length all-players - nb_best_player) length all-players
  set best-player-strat map [p -> [strategy] of p] reverse sublist all-players (length all-players - nb_best_player) length all-players
  ; supprimer les étiquettes des joueurs à éliminer
  ask labels [
    if member? player-id worst-player-ids [ die ]
  ]

  ; puis éliminer les joueurs
  ask players with [member? who worst-player-ids] [ die ]

  ; reproduire les meilleurs
  ask players with [member? who best-player-ids] [
    hatch 1 [
      set coins 0
      set rounds-played 0
      set opponent-memories []  ; Réinitialise la mémoire
      set move-count 0
      set betrayed-by []
      set id-placement next-id-placement
      set next-id-placement next-id-placement + 1

      set betrayed-copykitten []
      set detective-always-lying-against []

      hatch-labels 1 [
        set player-id [who] of myself
        set shape "line half"
        set size 3
      ]
    ]
  ]

  reposition-all-players

  ask players [
    set coins 0
    set rounds-played 0
    set move-count 0
    set betrayed-by []
    set matched? false

    set betrayed-copykitten []
    set detective-always-lying-against []
  ]
  update-label-position
end

to reposition-all-players
  let radius 15
  let total-players count players
  let angle-step ifelse-value (total-players > 0) [ 360 / total-players ] [ 0 ]

  ; Trier les joueurs par id-placement pour une disposition cohérente
  let sorted-players sort-on [id-placement] players

  let i 0
  foreach sorted-players [
    player ->
    ask player [
      let angle angle-step * i
      let x radius * cos angle
      let y radius * sin angle
      setxy x y
      facexy 0 0  ; S'assurer que tous les joueurs font face au centre
      set i i + 1
    ]
  ]
end

to-report apply-error [intended-decision is-human?]
  ; Si c'est un coup humain, on retourne toujours la décision telle quelle
  if is-human? [ report intended-decision ]

  ; Sinon, on applique le taux d'erreur
  ifelse (random-float 1.0 < error-rate / 100) [
    report 1 - intended-decision
  ] [
    report intended-decision
  ]
end


; ------------------------------------------- COINS -------------------------------------------

to add-coins [target number]
  ask target [
    set coins coins + number
  ]
  ask labels with [player-id = [who] of target] [
    let target-player one-of players with [who = [player-id] of myself]
    set-names target-player
  ]
end

; --------------------------------------------------------------------------------------

; ------------------------------------------- MATCHMAKING -------------------------------------------


; Commence un nouveau tour
to start-new-round
  ifelse fast-round

  [set round-number round-number + round_av_evolution
  ask players [ set rounds-played rounds-played + round_av_evolution]
  ]

  [set round-number round-number + 1
   ask players [ set rounds-played rounds-played + 1 ]
  ]




  ; Vérifier si c'est le moment d'éliminer et reproduire
  ifelse fast-round [
    evolve-population
  ]
  [
    if round-number >= next-elimination-round [
      evolve-population
    set next-elimination-round next-elimination-round + round_av_evolution
  ]
  ]

  ; Préparer les matchs pour le nouveau tour
  prepare-all-matches
  update-label-position
  tick
end

; Joue le prochain match dans la file d'attente
to play-next-match
  if not empty? remaining-matches [
    set current-match-pair first remaining-matches
    set remaining-matches but-first remaining-matches

    let player1 first current-match-pair
    let player2 last current-match-pair

    ; Marquer les joueurs comme ayant joué
    ask player1 [ set matched? true]
    ask player2 [ set matched? true]

    ; Jouer le match
    play-match player1 player2
  ]
end
; Nouvelle procédure pour passer à l'étape suivante (un seul match)
to next-match-step
  ; Si aucun match en cours
  if empty? remaining-matches [
    ; Vérifier si c'est le moment d'éliminer et reproduire
    if round-number >= next-elimination-round [
      evolve-population
      set next-elimination-round next-elimination-round + round_av_evolution
    ]

    ; Commencer un nouveau tour
    start-new-round
    play-next-match
    stop  ; On stop pour ne pas jouer un match immédiatement après
  ]

  ; Jouer le prochain match
  play-next-match
end

; Prépare tous les matchs pour le tour actuel
to prepare-all-matches
  set remaining-matches []

  ; Trier les joueurs par id-placement pour des matchs cohérents
  let bots sort-on [id-placement] players

  ; Matchs entre bots - chaque bot joue contre tous les autres avec un id-placement supérieur
  foreach (range length bots) [
    i ->
    let current-bot item i bots
    foreach (range (i + 1) length bots) [
      j ->
      let opponent item j bots
      set remaining-matches lput (list current-bot opponent) remaining-matches
    ]
  ]

  ask links [ die ]
  ask players [ set matched? false ]
end



to play-match [player1 player2]
  ; Animation visuelle
  ask player1 [ create-link-with player2 [ set color gray set thickness 0.1 ] ]

  ask player1 [ set size 4 ]
  ask player2 [ set size 4 ]
  display
  wait speed-game

  ifelse fast-round [set number_rounds round_av_evolution] [set number_rounds 1]

  repeat number_rounds [
    ; Logique du match
      let decision1 get-decision player1 player2
      let decision2 get-decision player2 player1

      ; Mise à jour des états
      ask player1 [ set last-move-opponent decision2 ]
      ask player2 [ set last-move-opponent decision1 ]

      ; Application des résultats
      apply-results player1 decision1 player2 decision2
    ]

  ; Nettoyage
  ask player1 [ set size 3 ]
  ask player2 [ set size 3 ]

  ; Réinitialiser la paire actuelle
  set current-match-pair []

  update-labels
end


to apply-results [player1 decision1 player2 decision2]
  let id1 [who] of player1
  let id2 [who] of player2

  let previous-coins1 [coins] of player1
  let previous-coins2 [coins] of player2

  ; Joueur 1 coopère
  if (decision1 = 1) [
    if (decision2 = 1) [  ; Mutual cooperation
      add-coins player1 coins-verite-verite
      add-coins player2 coins-verite-verite
      ask player1 [ set move-played 1]
      ask player2 [ set move-played 1]
    ]
    if (decision2 = 0) [  ; Player2 betrayed
      add-coins player1 coins-verite-mensonge
      add-coins player2 coins-mensonge-verite
      ask player1 [ set move-played 1]
      ask player2 [ set move-played 0]
      ask player1 [ set betrayed-by lput id2 betrayed-by]
    ]
  ]

  ; Joueur 1 trahit
  if (decision1 = 0) [
    if (decision2 = 1) [  ; Player1 betrayed
      add-coins player1 coins-mensonge-verite
      add-coins player2 coins-verite-mensonge
      ask player1 [ set move-played 0]
      ask player2 [ set move-played 1]
      ask player2 [ set betrayed-by lput id1 betrayed-by]
    ]
    if (decision2 = 0) [  ; Mutual defection
      add-coins player1 coins-mensonge-mensonge
      add-coins player2 coins-mensonge-mensonge
      ask player1 [ set move-played 0]
      ask player2 [ set move-played 0]
      ask player2 [ set betrayed-by lput id1 betrayed-by]
      ask player1 [ set betrayed-by lput id2 betrayed-by]
    ]
  ]

  add-move-to-memory player1 id2 decision1
  add-move-to-memory player2 id1 decision2

  ask player1 [ set coins-gained (coins - previous-coins1) ]
  ask player2 [ set coins-gained (coins - previous-coins2) ]

end

to-report get-decision [player opponent]
  let opponent-id [who] of opponent
  let opponent-last-move [last-move-opponent] of opponent
  let intended-decision 1  ; Par défaut coopère
  let is-human? ([strategy] of player = "human")
  let my-moves get-moves-against player opponent-id
  let their-moves get-moves-against opponent [who] of player

  if (is-human?) [ set intended-decision human-decision ]
  if ([strategy] of player = "bot") [
    let value-rdm random 101
    set intended-decision ifelse-value (value-rdm < bot-truth-percent) [ 1 ] [ 0 ]
  ]
  if ([strategy] of player = "copycat") [ set intended-decision ifelse-value (empty? their-moves) [ 1 ] [ last their-moves ] ]
  if ([strategy] of player = "cheater") [ set intended-decision 0 ]
  if ([strategy] of player = "cooperator") [ set intended-decision 1 ]

  ; Grudger ne trahit que ceux qui l'ont trahi
  if ([strategy] of player = "grudger") [
    set intended-decision ifelse-value (member? opponent-id [betrayed-by] of player) [ 0 ] [ 1 ]
  ]

  if ([strategy] of player = "detective") [

    let move-counter length my-moves

    ; Phase 1: Séquence test fixe (C-D-C-C)
    if move-counter = 0 [ set intended-decision 1 ]  ; 1er coup: Coopère
    if move-counter = 1 [ set intended-decision 0 ]  ; 2e coup: Trahit (test)
    if move-counter = 2 [ set intended-decision 1 ]  ; 3e coup: Coopère
    if move-counter = 3 [ set intended-decision 1 ]  ; 4e coup: Coopère

    ; Phase 2: Après les 4 coups tests, déterminer la stratégie
    if move-counter = 4 [  ; Seulement au 4ème coup on évalue
      ask player [
        ; Vérifie si l'adversaire a toujours coopéré (1,1,1,1)
        let always-cooperated? (length their-moves = 4 and empty? filter [m -> m = 0] their-moves)

        if always-cooperated? [
          ; Ajoute à la liste des joueurs contre qui on doit toujours mentir
          set detective-always-lying-against lput opponent-id detective-always-lying-against
        ]
      ]
    ]

    if move-counter >= 4 [
      ask player [
        ifelse (member? opponent-id detective-always-lying-against) [
          set intended-decision 0  ; Toujours trahir les joueurs trop coopératifs
        ] [
          ; Comportement normal: copier le dernier coup de l'adversaire
          set intended-decision ifelse-value (empty? their-moves)
            [ 1 ]
            [ last their-moves ]
        ]
      ]
    ]
  ]


    if ([strategy] of player = "simpleton") [
    ifelse (empty? their-moves)
      [ report 1 ]  ; Premier coup: coopère
      [
        ; Récupère mon dernier coup et celui de l'adversaire
        let my-last last my-moves
        let their-last last their-moves

        ; Si les deux ont coopéré ou triché, répète mon dernier coup
        ; Si les résultats diffèrent, change de stratégie
        ifelse (their-last != 0)
          [ report 1 ]       ; Même résultat → répète
          [ report (1 - my-last) ] ; Résultats différents → change
      ]
  ]

  ; Copykitten nécessite deux trahisons consécutives
  if ([strategy] of player = "copykitten") [
    if number-betrayed-consecutive their-moves >= 2 [
      report 0
    ]
  ]

  if ([strategy] of player = "custom") [
    let opp-id [who] of opponent
    let opp-moves get-moves-against player opp-id
    let rounds length opp-moves
    let lenght-initial-moves length conv-custom-initial-moves-list

    if custom-play-opponent-last-move = true [
      report ifelse-value (empty? their-moves)
            [ 1 ]
      [ last their-moves ]
    ]

    if number-betrayed-consecutive their-moves >= custom-consecutive-betray-after and custom-consecutive-betray-after > 0 [
      report 0
    ]

    if number-betrayed their-moves >= custom-betray-after and custom-betray-after > 0 [
      report 0
    ]

    if custom-play-randomly = true [
      let value-rdm random 101
      report ifelse-value (value-rdm < custom-truth-percent) [ 1 ] [ 0 ]
    ]

    if custom-repeat-initial-moves = true [
      let move-index (rounds mod length conv-custom-initial-moves-list)
      let move-to-play item move-index conv-custom-initial-moves-list
      report move-to-play
    ]


    ; Phase initiale: jouer la séquence prédéfinie
    if rounds < lenght-initial-moves [
      let move-to-play item rounds conv-custom-initial-moves-list
      report move-to-play
    ]

    ;aucune décision à prendre
    set intended-decision 1
  ]

  ; Appliquer l'erreur aléatoire
  report apply-error intended-decision is-human?
end


; ------------------------------------------- STRATEGIE -------------------------------------------


to-report number-betrayed [their-moves]
  ; Retourne le nombre de fois où l'adversaire a trahi (joué 0)
  report length filter [m -> m = 0] their-moves
end

to-report number-betrayed-consecutive [their-moves]
  let max-consecutive 0
  let current-streak 0

  foreach their-moves [
    move ->
    ifelse (move = 0) [
      ; Si c'est une trahison, augmenter le compteur courant
      set current-streak (current-streak + 1)
      ; Mettre à jour le maximum si nécessaire
      if (current-streak > max-consecutive) [
        set max-consecutive current-streak
      ]
    ] [
      ; Si ce n'est pas une trahison, réinitialiser le compteur
      set current-streak 0
    ]
  ]

  report max-consecutive
end
@#$#@#$#@
GRAPHICS-WINDOW
725
75
1370
721
-1
-1
13.0
1
10
1
1
1
0
0
0
1
-24
24
-24
24
0
0
1
ticks
30.0

SLIDER
516
141
688
174
human-count
human-count
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
516
181
688
214
bot-count
bot-count
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
517
221
689
254
copycat-count
copycat-count
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
517
260
689
293
cheater-count
cheater-count
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
517
300
689
333
cooperator-count
cooperator-count
0
100
1.0
1
1
NIL
HORIZONTAL

BUTTON
759
725
822
758
NIL
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
517
340
689
373
grudger-count
grudger-count
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
517
381
689
414
detective-count
detective-count
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
517
462
689
495
copykitten-count
copykitten-count
0
100
2.0
1
1
NIL
HORIZONTAL

SLIDER
1439
361
1611
394
round_av_evolution
round_av_evolution
0
100
4.0
1
1
NIL
HORIZONTAL

SLIDER
1440
436
1614
469
nb_worst_player
nb_worst_player
0
10
1.0
1
1
NIL
HORIZONTAL

BUTTON
847
748
957
781
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1084
748
1170
781
Pas à pas
next-match-step
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
880
33
1055
66
speed-game
speed-game
0.00
1
0.0
0.001
1
NIL
HORIZONTAL

BUTTON
904
793
1008
826
Humain vérité
set human-decision 1\nnext-match-step
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
1016
793
1125
826
Humain mentir
set human-decision 0\nnext-match-step
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
1067
33
1239
66
error-rate
error-rate
0
100
0.0
1
1
NIL
HORIZONTAL

SLIDER
1440
399
1612
432
nb_best_player
nb_best_player
0
10
0.0
1
1
NIL
HORIZONTAL

MONITOR
803
80
860
125
Round:
round-number
17
1
11

SLIDER
155
140
327
173
custom-count
custom-count
0
25
1.0
1
1
NIL
HORIZONTAL

SLIDER
65
282
237
315
custom-betray-after
custom-betray-after
0
50
0.0
1
1
NIL
HORIZONTAL

INPUTBOX
1612
237
1662
297
coins-verite-verite
2.0
1
0
Number

INPUTBOX
1541
236
1591
296
coins-verite-mensonge
-1.0
1
0
Number

INPUTBOX
1541
167
1591
227
coins-mensonge-mensonge
0.0
1
0
Number

INPUTBOX
1611
167
1661
227
coins-mensonge-verite
3.0
1
0
Number

MONITOR
950
80
1038
125
Total joueurs:
count players
17
1
11

SWITCH
65
318
291
351
custom-play-opponent-last-move
custom-play-opponent-last-move
1
1
-1000

SLIDER
242
282
461
315
custom-consecutive-betray-after
custom-consecutive-betray-after
0
100
0.0
1
1
NIL
HORIZONTAL

SWITCH
238
227
439
260
custom-repeat-initial-moves
custom-repeat-initial-moves
1
1
-1000

BUTTON
760
766
823
799
reset
set human-count 0\nset bot-count 0\nset copycat-count 0\nset cheater-count 0\nset cooperator-count 0\nset grudger-count 0\nset detective-count 0\nset simpleton-count 0\nset copykitten-count 0\nset custom-count 0\n\nsetup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
516
423
689
456
simpleton-count
simpleton-count
0
100
1.0
1
1
NIL
HORIZONTAL

SLIDER
540
507
668
540
bot-truth-percent
bot-truth-percent
0
100
50.0
1
1
NIL
HORIZONTAL

MONITOR
859
80
953
125
Matchs restants
length remaining-matches
17
1
11

BUTTON
964
748
1076
781
Tour complet
next-match-step\none-round-only\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SWITCH
1187
748
1295
781
fast-round
fast-round
1
1
-1000

SWITCH
63
183
234
216
custom-play-randomly
custom-play-randomly
0
1
-1000

SLIDER
238
183
410
216
custom-truth-percent
custom-truth-percent
0
100
100.0
1
1
NIL
HORIZONTAL

MONITOR
1038
80
1181
125
Worst players
worst-player-strat
17
1
11

TEXTBOX
1555
300
1701
326
Gains du joueur
15
0.0
1

TEXTBOX
1575
99
1639
118
Opposant
15
0.0
1

TEXTBOX
1404
215
1449
234
Joueur
15
0.0
1

TEXTBOX
1542
144
1596
162
Mensonge
11
0.0
1

TEXTBOX
1481
190
1533
208
Mensonge
11
0.0
1

TEXTBOX
1498
255
1529
273
Vérité
11
0.0
1

TEXTBOX
1623
146
1655
164
Vérité
11
0.0
1

TEXTBOX
531
102
681
121
Séléction des joueurs
15
0.0
1

TEXTBOX
188
105
292
124
Joueur Custom
15
0.0
1

TEXTBOX
1471
328
1621
347
Evolution du jeu 
15
0.0
1

INPUTBOX
64
219
234
279
custom-initial-moves-list
[0 1 0 1]
1
0
String (reporter)

MONITOR
1180
80
1292
125
NIL
best-player-strat
17
1
11

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

(suggested things for the user to notice while running the model)

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.4.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
