(require 'cl)

;; seed random
(random 't)

(define-derived-mode mythic-mode text-mode "Mythic" "Mode to play mythic rpg"
  (make-local-variable 'mythic-chaos-level))

(defvar mythic-mode-map nil
  "Keys for mythic mode")

(unless mythic-mode-map
  (let ((map (make-sparse-keymap))
	(ranks (remove nil (mapcar 'cadr mythic-ranks))))
    (dotimes (i (length ranks))
      (let ((acting (nth i ranks)))
	(mythic-kbd map (concat "\C-c\C-co" (mythic-number-to-key i)) 'mythic-odds-question acting)
	(dotimes (j (length ranks))
	  (let ((difficulty (mythic-rank-translate (nth j ranks)))
		(acting (mythic-rank-translate acting)))
	    (mythic-kbd map (concat "\C-c\C-cr" (mythic-number-to-key i) (mythic-number-to-key j)) 'mythic-resisted-question acting difficulty)))))
    (define-key map (kbd "C-c C-o") 'mythic-odds-question)
    (define-key map (kbd "C-c C-r") 'mythic-resisted-question)
    (define-key map (kbd "C-c C-d") 'mythic-dice)
    (define-key map (kbd "C-c C-n") 'mythic-next-scene)
    (define-key map (kbd "C-c C-s n") 'mythic-show-next-scene)
    (define-key map (kbd "C-c C-s p") 'mythic-show-prev-scene)
    (setq mythic-mode-map map)))

(defun mythic-show-next-scene ()
  (interactive)
  (narrow-to-page 1)
  (goto-char (point-min)))

(defun mythic-show-prev-scene ()
  (interactive)
  (narrow-to-page -1)
  (goto-char (point-min)))

(defun mythic-ask-grade (rank)
  (if (string-match "\\(miniscule\\|superhuman\\)" rank)
      (let ((grade (read-number (format "Grade for %s: " rank))))
	(cond ((= grade 1) rank)
	      ((> grade 1) (concat rank (number-to-string grade)))
	      ((error "Grade of %s must be greater than 0" rank))))
    rank))

(defmacro mythic-kbd (map key func &rest ranks)
  (let ((rank (mapcar 'eval ranks)))
    `(define-key ,map
       ,key
       (lambda (extreme-rank)
	 (interactive "p")
	 (let ((ranks (quote ,rank)))
	   (apply ,func
		  (if (> extreme-rank 1)
		      (mapcar 'mythic-ask-grade ranks)
		    ranks)))))))

(defun mythic-number-to-key (number)
  (if (= number 10)
      "+"
    (if (= number 9)
	"0"
      (number-to-string (1+ number)))))

(defconst mythic-fate-chart
      '(( 50  25   10   5  5   0    0 -20 -20 -40 -40 -55 -65)
	( 75  50   25  15  10  5    5   0   0 -20 -20 -35 -45)
	( 90  75   50  35  25  15  10   5   5   0   0 -15 -25)
	( 95  85   65  50  45  25  15  10   5   5   5  -5 -15)
	(100  90   75  55  50  35  20  15  10   5   5   0 -10)
	(105  95   85  75  65  50  35  25  15  10  10   5  -5)
	(110  95   90  85  80  65  50  45  25  20  15   5   0)
	(115 100   95  90  85  75  55  50  35  25  20  10   5)
	(120 105   95  95  90  85  75  65  50  45  35  15   5)
	(125 115  100  95  95  90  80  75  55  50  45  20  10)
	(130 125  110  95  95  90  85  80  65  55  50  25  10)
	(150 145  130 100 100  95  95  90  85  80  75  50  25)
	(170 165  150 120 120 100 100  95  95  90  90  75  50))
      "This two dimensional list cross references the odds of the acting rank versus the difficulty rank."
)

(defconst mythic-ranks
  '(("miniscule2")
    ("miniscule" "impossible")
    ("weak" "no way")
    ("low" "very unlikely")
    ("below-average" "unlikely")
    ("average" "50/50")
    ("above-average" "somewhat likely")
    ("high" "likely")
    ("exceptional" "very likely")
    ("incredible" "near sure thing")
    ("awesome" "a sure thing")
    ("superhuman" "has to be")
    ("superhuman2")))

(defun mythic-rank-translate (rank)
  (let ((list (find rank mythic-ranks :test 'member)))
    (car (remove rank list))))

(defmacro mythic-threshold (throw &rest clauses)
  (declare (indent 1))
  (let (result)
    (dolist (clause clauses result)
      (setq result (cons `((<= ,throw ,(car clause)) ,(cadr clause)) result)))
    `(cond ,@(nreverse result))))

(defun mythic-rank-pos (difficulty)
  (position difficulty mythic-ranks :test 'member))

(defun mythic-extreme-rank-modifier (rank)
  "Returns the odds modifier for a rank below miniscule2 or above superhuman2."
  (if (string-match "\\(miniscule\\|superhuman\\)\\([0-9]+\\)" rank)
      (let ((rank (match-string 1 rank))
	    (grade (- (string-to-number (match-string 2 rank)) 2)))
	(if (> grade 0)
	    (* (if (string= rank "miniscule") -1 1) grade 20)
	  0))
    0))

(defun mythic-truncate-rank (rank)
  "Truncates a rank below miniscule2 to miniscule2 and ranks above superhuman2 to superhuman2. Return rank unchanged if its a member of the rank list."
  (if (string-match "\\(miniscule\\|superhuman\\)[0-9]+" rank)
      (concat (match-string 1 rank) "2")
    rank))

(defun mythic-get-odds (acting difficulty)
  (+ (mythic-extreme-rank-modifier acting)
     (- (mythic-extreme-rank-modifier difficulty))
     (nth (mythic-rank-pos (mythic-truncate-rank difficulty))
	  (nth (mythic-rank-pos (mythic-truncate-rank acting))
	       mythic-fate-chart))))

(defun mythic-ask-question (acting difficulty)
  (let* ((odds (mythic-get-odds acting difficulty))
	 (throw (mythic-d100))
	 (lower (floor odds 5))
	 (upper (- 100 (floor (- 99 odds) 5)))
	 (answer
	  (mythic-threshold throw
	    (lower 'exceptional-yes)
	    (odds 'yes)
	    ((1- upper) 'no)
	    (100 'exceptional-no))))
    (list
     (list 'answer answer)
     (list 'throw throw)
     (list 'odds odds)
     (list 'lower lower)
     (list 'upper  (if (>= upper 100) 0 upper))
     (list 'event (mythic-get-event throw)))))

(defun mythic-get-event (answer)
  (when (mythic-event-happend-p answer)
      (mythic-random-event)))

(defun mythic-event-happend-p (odds)
  (and (<= (/ odds 11) mythic-chaos-level)
       (= (% odds 11) 0)))

(defun mythic-get (key alist)
  (cadr (assoc key alist)))

(defun mythic-d100 ()
  "Returns a random value between 1 and 100."
  (1+ (random 99)))

(defun mythic-event-focus ()
  "Returns the focus of a random event."
  (let ((focus
	 (mythic-threshold (mythic-d100)
	   (7 "Remote event")
	   (28 '("NPC action" mythic-select-npc))
	   (35 "Introduce a new NPC")
	   (45 '("Move towards a thread" mythic-select-thread))
	   (52 '("Move away from a thread" mythic-select-thread))
	   (55 '("Close a thread" mythic-select-thread))
	   (67 "PC negative")
	   (75 "PC positive")
	   (83 "Ambiguous event")
	   (92 '("NPC negative" mythic-select-npc))
	   (100 '("NPC positive" mythic-select-npc)))))
    (if (and (listp focus) (eq major-mode 'mythic-mode))
	(format "%s (%s)" (car focus) (or (funcall (cadr focus)) "unknown"))
      focus)))

(defun mythic-select-thread ()
  (mythic-random-element (mythic-get-list "Threads")))

(defun mythic-select-npc ()
  (mythic-random-element (mythic-get-list "NPCs")))

(defvar mythic-chaos-level 5)

(defun mythic-format-answer (odds)
  (let ((message (format "Answer: %s" (mythic-get 'answer odds)))
	(event (mythic-get 'event odds)))
    (if event
	(message "%s -- Event: %s" message event)
      (message message))))

(defun mythic-chaos-level-rank (mythic-chaos-level)
  "Convert the numeric chaos level to its string representation."
  (cadr (nth (- 10 mythic-chaos-level) mythic-ranks)))

(defun mythic-complete-rank (prompt type)
  (let ((collection (if (eq type 'odds)
			(remove nil (mapcar 'cadr mythic-ranks))
		      (mapcar 'car mythic-ranks))))
    (let ((rank (completing-read prompt collection)))
      (if (or
	   (member rank collection)
	   (and (eq type 'resisted)
		(string-match "\\(superhuman\\|miniscule\\)[0-9]+" rank)))
	  rank
	(error "Unknown rank: %s" rank)))))

(defun mythic-odds-question (acting)
  (interactive (list (mythic-complete-rank "Acting rank: " 'odds)))
  (mythic-format-answer
   (mythic-ask-question acting (mythic-chaos-level-rank mythic-chaos-level))))

(defun mythic-resisted-question (acting difficulty)
  (interactive (list
		(mythic-complete-rank "Acting rank: " 'resisted)
		(mythic-complete-rank "Resisted rank: " 'resisted)))
  (mythic-format-answer
   (mythic-ask-question acting difficulty)))

(defconst mythic-event-actions '(
		"Attainment" "Starting"        "Neglect"     "Fight"
		"Recruit"    "Triumph"         "Violate"     "Oppose"
		"Malice"     "Communicate"     "Persecute"   "Increase"
		"Decrease"   "Abandon"         "Gratify"     "Inquire"
		"Antagonise" "Move"            "Waste"       "Truce"
		"Release"    "Befriend"        "Judge"       "Desert"
		"Dominate"    "Procrastinate"  "Praise"      "Separate"
		"Take"        "Break"          "Heal"        "Delay"
		"Stop"        "Lie"            "Return"      "Imitate"
		"Struggle"    "Inform"         "Bestow"      "Postpone"
		"Expose"      "Haggle"         "Imprison"    "Release"
		"Celebrate"   "Develop"        "Travel"      "Block"
		"Harm"        "Debase"         "Overindulge" "Adjourn"
		"Adversity"   "Kill"           "Disrupt"     "Usurp"
		"Create"      "Betray"         "Agree"       "Abuse"
		"Oppress"     "Inspect"        "Ambush"      "Spy"
		"Attach"      "Carry"          "Open"        "Carelessness"
		"Ruin"        "Extravagance"   "Trick"       "Arrive"
		"Propose"     "Divide"         "Refuse"      "Mistrust"
		"Deceive"     "Cruelty"        "Intolerance" "Trust"
		"Excitement"  "Activity"       "Assist"      "Care"
		"Negligence"  "Passion"        "Work hard"   "Control"
		"Attract"     "Failure"        "Pursue"      "Vengeance"
		"Proceedings" "Dispute"        "Punish"      "Guide"
		"Transform"   "Overthrow"      "Oppress"     "Change"
		)
  "List of actions for random events.")

(defconst mythic-event-subjects '(
		 "Goals"          "Dreams"            "Environment"       "Outside"
		 "Inside"         "Realities"         "Allies"            "Enemies"
		 "Evil"           "Good"              "Emotions"          "Opposition"
		 "War"            "Peace"             "The innocent"      "Love"
		 "The spiritual"  "The intellectual"  "New ideas"         "Joy"
		 "Messages"       "Energy"            "Balance"           "Tension"
		 "Friendship"     "The physical"      "A project"         "Pleasures"
		 "Pain"	          "Possessions"       "Benefits"          "Plans"
		 "Expectations"   "Legal matters"     "Bureaucracy"       "Lies"
		 "Business"       "A plan"            "News"              "Exterior factors"
		 "Advice"         "A plot"            "Competition"       "Prison"
		 "Illness"        "Food"              "Attention"         "Success"
		 "Failure"        "Travel"            "Jealously"         "Dispute"
		 "Home"           "Investment"        "Suffering"         "Wishes"
		 "Tactics"        "Stalemate"         "Randomness"        "Misfortune"
		 "Death"          "Disruption"        "Power"             "A burden"
		 "Intrigues"      "Fears"             "Ambush"            "Rumour"
		 "Wounds"         "Extravagance"      "A representative"  "Adversities"
		 "Opulance"       "Liberty"           "Military"          "The mundane"
		 "Trials"         "Masses"            "Vehicle"           "Art"
		 "Victory"        "Dispute"           "Riches"            "Status quo"
		 "Technology"     "Hope"              "Magic"             "Illusions"
		 "Portals"        "Danger"            "Weapons"           "Animals"
		 "Weather"        "Elements"          "Nature"            "The public"
		 "Leadership"     "Fame"              "Anger"             "Information"
		 )
  "List of subjects for random events.")

(defun mythic-random-element (list)
  (nth (random (1- (length list))) list))

(defun mythic-random-event ()
  (concat
   (mythic-event-focus)
   " - "
   (mythic-random-element mythic-event-actions)
   "/"
   (mythic-random-element mythic-event-subjects)))

(defun mythic-dice (dice-spec)
  "Roll dice according to dice-spec. Possible values are for example for dice-spec are d20, 4d20 or 2d20+4."
  (interactive "sDice: ")
  (if (string-match "\\s-*\\([[:digit:]]+\\)*\\s-*d\\([[:digit:]]+\\)\\s-*\\([+-][[:digit:]]+\\)*" dice-spec)
      (let ((number (string-to-int (or (match-string 1 dice-spec) "1")))
	    (sides (string-to-int (match-string 2 dice-spec)))
	    (modifier (string-to-int (or (match-string 3 dice-spec) "0")))
	    (result 0))
	(dotimes (i number result)
	  (setq result (+ result (1+ (random sides)))))
	(setq result (+ result modifier))
	(if (interactive-p)
	    (message "Result of %s: %s" dice-spec result)
	  result))
    (when (interactive-p)
      (message "Invalid dice %s" dice-spec))))

(defun mythic-next-scene ()
  "Set the next scene by updating the chaos level, prompting for the scene setup and modifiying all lists."
  (interactive)
  (if (y-or-n-p "Increase chaos factor? ")
      (incf mythic-chaos-level)
    (decf mythic-chaos-level))
  (let ((setup (read-string "Scene setup: "))
	(roll (mythic-dice "d10")))
    (if (<= roll mythic-chaos-level)
	(if (oddp roll)
	    (setq setup (read-string "Altered scene. New scene setup: "))
	  (setq setup (read-string
		       (concat "Interrupt scene: "
			       (mythic-random-event)
			       ". New scene setup: ")))))
    (insert (format "\n\nScene setup: %s (Chaos: %d)\n\n"
		    setup mythic-chaos-level))))
(defun mythic-get-list (list)
  (save-excursion
    (save-restriction
      (goto-char (point-min))
      (when (search-forward (concat "List: " list) nil t)
	(narrow-to-page)
	(let ((elts))
	  (while (re-search-forward "^[*]\\s-*\\(.*\\)" nil t)
	    (push (match-string 1) elts))
	  elts)))))


(provide 'mythic)

;; focusArray[1] = new Array("4/Horror: the game starts with the Chaos Factor set to 4.  Chaos can only increase not decrease.  When random events are generated results of 1 to 3 within the Chaos Factor are altered scenes.  Any higher numbers will be interrupts."  "1/10/Horror - PC"  "11/23/Horror - NPC"  "24/30/Remote Event"  "31/49/NPC action"  "50/52/Introduce an NPC"  "53/55/Move toward a thread"  "56/62/Move away from a thread"  "63/72/PC Negative"  "73/75/PC positive"  "76/82/Ambiguous event"  "83/97/NPC negative"  "98/100/NPC positive");

;; focusArray[2] = new Array("5/Action adventure: Double rolls on the Fate chart always result in a random event whether they fall within the Chaos Facotr range or not.  Chaos cannot fall below 5.  Any scene which would normally lower the Chaos below 5 leaves it unchanged."  "1/16/Action!"  "17/24/Remote event"  "25/44/NPC action"  "45/52/Introduce an NPC"  "53/56/Move toward a thread"  "57/64/Move away from a thread"  "65/76/PC negative"  "77/80/PC positive"  "81/84/Ambiguous event"  "85/96/NPC negative"  "97/100/NPC positive")

;; focusArray[3] = new Array("5/Mystery: the Chaos Factor cannot fall below 3.  Any scene which would normally lower the Chaos below 3 leaves it unchanged."  "1/8/Remote event"  "9/20/NPC action"  "21/32/Introduce an NPC"  "33/52/Move toward a thread"  "53/64/Move away from a thread"  "65/72/PC negative"  "73/80/PC positive"  "81/88/Ambiguous event"  "89/96/NPC negative"  "97/100/NPC positive")

;; focusArray[4] = new Array("5/Social game: this game uses standard Chaos rules."  "1/12/Drop a bomb!"  "13/24/Remote event"  "25/36/NPC action"  "37/44/Introduce an NPC"  "45/56/Move toward a thread"  "57/60/Move away from a thread"  "61/64/Close a thread"  "65/72/PC negative"  "73/80/PC positive"  "81/92/Ambiguous event"  "93/96/NPC negative"  "7/100/NPC positive")

;; focusArray[5] = new Array("5/Personal game: this game uses standard Chaos rules."  "1/7/Remote event"  "8/24/NPC action"  "25/28/PC NPC action"  "29/35/Introduce an NPC"  "36/42/Move toward a thread"  "43/45/Move toward a PC thread"  "46/50/Move away from a thread"  "51/52/Move away from a PC thread"  "53/54/Close thread"  "55/55/Close PC thread"  "56/67/PC negative"  "68/75/PC positive"  "76/83/Ambiguous event"  "84/90/NPC negative"  "91/92/PC NPC negative"  "93/99/NPC positive"  "100/100/PC NPC positive")

;; focusArray[6] = new Array("5/Epic game: the Chaos Factor cannot fall below 3.  Any scene which would normally lower the Chaos below 3 leaves it unchanged."  "1/12/Thread escalates"  "13/16/Remote event"  "17/30/NPC action"  "31/42/Introduce an NPC"  "43/46/Move toward a thread"  "47/58/Move away from a thread"  "59/72/PC negative"  "73/80/PC positive"  "81/84/Ambiguous event"  "85/92/NPC negative"  "93/100/NPC positive")
