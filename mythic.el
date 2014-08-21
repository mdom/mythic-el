(require 'cl-seq)

(define-derived-mode mythic-mode text-mode "Mythic" "Mode to play mythic rpg")

(defvar mythic-mode-map nil
  "Keys for mythic mode")

(unless mythic-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c5" (lambda () (interactive) (odds-question 'average)))
    (define-key map "\C-c\C-c" (lambda () (interactive) (odds-question 'average)))
    (setq mythic-mode-map map)))

(setq mythic-fate-chart
      '((50  25  10  5  5  0  0  -20  -20  -40  -40  -55  -65)
	(75  50  25  15  10  5  5  0  0  -20  -20  -35  -45)
	(90  75  50  35  25  15  10  5  5  0  0  -15  -25)
	(95  85  65  50  45  25  15  10  5  5  5  -5  -15)
        (100  90  75  55  50  35  20  15  10  5  5  0  -10)
	(105  95  85  75  65  50  35  25  15  10  10  5  -5)
	(110  95  90  85  80  65  50  45  25  20  15  5  0)
	(115  100  95  90  85  75  55  50  35  25 20  10  5)
	(120  105  95  95  90  85  75  65  50  45  35  15  5)
	(125  115  100  95  95  90  80  75  55  50  45  20  10)
	(130  125  110  95  95  90  85  80  65  55  50  25  10)
	(150  145  130  100  100  95  95  90  85  80  75  50  25)
	(170  165  150  120  120  100  100  95  95  90  90  75  50)))

(setq mythic-ranks
      '(miniscule2 
	miniscule 
	weak 
	low
	below-average
	average
	above-average
	high exceptional
	incredible
	awesome
	superhuman
	superhuman2))

(defmacro mythic-threshold (throw &rest clauses) 
  (declare (indent 1))
  (let (result)
    (dolist (clause clauses result)
      (setq result (cons `((<= ,throw ,(car clause)) ,(cadr clause)) result)))
    `(cond ,@(nreverse result))))

(defun mythic-get-odds (acting difficulty)
  (let* ((odds (nth (position difficulty mythic-ranks) (nth (position acting mythic-ranks) mythic-fate-chart)))
	(throw (mythic-d100))
	(lower (floor odds 5))
	(upper (- 100 (floor (- 99 odds) 5)))
	(answer 
	 (mythic-threshold throw
	   (lower 'exceptional-yes)
	   (odds 'yes)
	   ((1- upper) 'no)
	   (100   'exceptional-no))))    
    (list answer throw odds lower (if (>= upper 100) 0 upper))))

(defun mythic-event-happend? (odds)
  (= (% odds 11) 0))

(defun mythic-d100 ()
  (1+ (random 99)))

(defun mythic-what-event? ()
  (mythic-threshold (mythic-d100)
    (7 "Remote event")
    (28 "NPC action")
    (35 "Introduce a new NPC")
    (45 "Move towards a thread")
    (52 "Move away from a thread")
    (55 "Close a thread")
    (67 "PC negative")
    (75 "PC positive")
    (83 "Ambiguous event")  
    (92 "NPC negative")
    (100 "NPC positive")))

(setq mythic-chaos-level 5)

(defun mythic-chaos-level-rank (mythic-chaos-level)
  (mythic-threshold mythic-chaos-level
    (1 'high)
    (3 'above-average)
    (6 'average)
    (8 'below-average)
    (10 'low)))

(defun mythic-odds-question (acting)
  (interactive)
  (mythic-get-odds acting (mythic-chaos-level-rank mythic-chaos-level)))

;(mythic-get-odds 'superhuman2 'average)
;(mythic-odds-question 'above-average)

;focusArray[0] = new Array("5/Standard game.  Regular fate chart rules."  "1/7/Remote event"  "8/28/NPC action"  "29/35/Introduce a new NPC"  "36/45/Move towards a thread"  "46/52/Move away from a thread"  "53/55/Close a thread"  "56/67/PC negative"  "68/75/PC positive"  "76/83/Ambiguous event"  "84/92/NPC negative"  "93/100/NPC positive")

; focusArray[1] = new Array("4/Horror: the game starts with the Chaos Factor set to 4.  Chaos can only increase not decrease.  When random events are generated results of 1 to 3 within the Chaos Factor are altered scenes.  Any higher numbers will be interrupts."  "1/10/Horror - PC"  "11/23/Horror - NPC"  "24/30/Remote Event"  "31/49/NPC action"  "50/52/Introduce an NPC"  "53/55/Move toward a thread"  "56/62/Move away from a thread"  "63/72/PC Negative"  "73/75/PC positive"  "76/82/Ambiguous event"  "83/97/NPC negative"  "98/100/NPC positive");

; focusArray[2] = new Array("5/Action adventure: Double rolls on the Fate chart always result in a random event whether they fall within the Chaos Facotr range or not.  Chaos cannot fall below 5.  Any scene which would normally lower the Chaos below 5 leaves it unchanged."  "1/16/Action!"  "17/24/Remote event"  "25/44/NPC action"  "45/52/Introduce an NPC"  "53/56/Move toward a thread"  "57/64/Move away from a thread"  "65/76/PC negative"  "77/80/PC positive"  "81/84/Ambiguous event"  "85/96/NPC negative"  "97/100/NPC positive")

; focusArray[3] = new Array("5/Mystery: the Chaos Factor cannot fall below 3.  Any scene which would normally lower the Chaos below 3 leaves it unchanged."  "1/8/Remote event"  "9/20/NPC action"  "21/32/Introduce an NPC"  "33/52/Move toward a thread"  "53/64/Move away from a thread"  "65/72/PC negative"  "73/80/PC positive"  "81/88/Ambiguous event"  "89/96/NPC negative"  "97/100/NPC positive")

; focusArray[4] = new Array("5/Social game: this game uses standard Chaos rules."  "1/12/Drop a bomb!"  "13/24/Remote event"  "25/36/NPC action"  "37/44/Introduce an NPC"  "45/56/Move toward a thread"  "57/60/Move away from a thread"  "61/64/Close a thread"  "65/72/PC negative"  "73/80/PC positive"  "81/92/Ambiguous event"  "93/96/NPC negative"  "7/100/NPC positive")

; focusArray[5] = new Array("5/Personal game: this game uses standard Chaos rules."  "1/7/Remote event"  "8/24/NPC action"  "25/28/PC NPC action"  "29/35/Introduce an NPC"  "36/42/Move toward a thread"  "43/45/Move toward a PC thread"  "46/50/Move away from a thread"  "51/52/Move away from a PC thread"  "53/54/Close thread"  "55/55/Close PC thread"  "56/67/PC negative"  "68/75/PC positive"  "76/83/Ambiguous event"  "84/90/NPC negative"  "91/92/PC NPC negative"  "93/99/NPC positive"  "100/100/PC NPC positive")

; focusArray[6] = new Array("5/Epic game: the Chaos Factor cannot fall below 3.  Any scene which would normally lower the Chaos below 3 leaves it unchanged."  "1/12/Thread escalates"  "13/16/Remote event"  "17/30/NPC action"  "31/42/Introduce an NPC"  "43/46/Move toward a thread"  "47/58/Move away from a thread"  "59/72/PC negative"  "73/80/PC positive"  "81/84/Ambiguous event"  "85/92/NPC negative"  "93/100/NPC positive")

(setq Actions '(
		"Attainment"  "Starting"  "Neglect"  "Fight"  "Recruit"  "Triumph"  "Violate"  "Oppose"  "Malice"  "Communicate"  "Persecute"  "Increase"  "Decrease"  "Abandon"  "Gratify"  "Inquire"  "Antagonise"  "Move"  "Waste"  "Truce"  
		"Release"  "Befriend"  "Judge"  "Desert"  "Dominate"  "Procrastinate"  "Praise"  "Separate"  "Take"  "Break"  "Heal"  "Delay"  "Stop"  "Lie"  "Return"  "Imitate"  "Struggle"  "Inform"  "Bestow"  "Postpone"  
		"Expose"  "Haggle"  "Imprison"  "Release"  "Celebrate"  "Develop"  "Travel"  "Block"  "Harm"  "Debase"  "Overindulge"  "Adjourn"  "Adversity"  "Kill"  "Disrupt"  "Usurp"  "Create"  "Betray"  "Agree"  "Abuse"  
		"Oppress"  "Inspect"  "Ambush"  "Spy"  "Attach"  "Carry"  "Open"  "Carelessness"  "Ruin"  "Extravagance"  "Trick"  "Arrive"  "Propose"  "Divide"  "Refuse"  "Mistrust"  "Deceive"  "Cruelty"  "Intolerance"  "Trust"  
		"Excitement"  "Activity"  "Assist"  "Care"  "Negligence"  "Passion"  "Work hard"  "Control"  "Attract"  "Failure"  "Pursue"  "Vengeance"  "Proceedings"  "Dispute"  "Punish"  "Guide"  "Transform"  "Overthrow"  "Oppress"  "Change"
		))

(setq Subjects '(
		 "Goals"  "Dreams"    "Environment"  "Outside"
		 "Inside" "Realities" "Allies"        "Enemies"
		 "Evil"   "Good"      "Emotions"      "Opposition"
                 "War"  "Peace"  "The innocent"  "Love"  "The spiritual"  "The intellectual"  "New ideas"  "Joy"  
		 "Messages"  "Energy"  "Balance"  "Tension"  "Friendship"  "The physical"  "A project"  "Pleasures"  "Pain"  "Possessions"  "Benefits"  "Plans"  "Lies"  "Expectations"  "Legal matters"  "Bureaucracy"  "Business"  "A plan"  "News"  "Exterior factors" 
		 "Advice"  "A plot"  "Competition"  "Prison"  "Illness"  "Food"  "Attention"  "Success"  "Failure"  "Travel"  "Jealously"  "Dispute"  "Home"  "Investment"  "Suffering"  "Wishes"  "Tactics"  "Stalemate"  "Randomness"  "Misfortune"  
		 "Death"  "Disruption"  "Power"  "A burden"  "Intrigues"  "Fears"  "Ambush"  "Rumour"  "Wounds"  "Extravagance"  "A representative"  "Adversities"  "Opulance"  "Liberty"  "Military"  "The mundane"  "Trials"  "Masses"  "Vehicle"  "Art"  
		 "Victory"  "Dispute"  "Riches"  "Status quo"  "Technology"  "Hope"  "Magic"  "Illusions"  "Portals"  "Danger"  "Weapons"  "Animals"  "Weather"  "Elements"  "Nature"  "The public"  "Leadership"  "Fame"  "Anger"  "Information"
		 ))

(defun random-event ()
  (concat 
   (nth (random (1- (length Actions))) Actions)
   "/"
   (nth (random (1- (length Subjects))) Subjects)))

;(random-event)

(provide 'mythic)
