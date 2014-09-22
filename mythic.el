;;; mythic.el --- major mode to play the mythic role playing game

;; Author: Mario Domgörgen <mario@domgoergen.com>
;; Created: Fri Aug 15 16:14:39 2014 +0200
;; Keywords: mythic rpg dice

;; Copyright (2014) Mario Domgörgen

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.

;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.

;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301 USA


;;; Commentary:
;; 

;;; Code:

(eval-when-compile (require 'cl))

;;; seed random
(random 't)

(define-derived-mode mythic-mode text-mode "Mythic" "Mode to play mythic rpg"
  (make-local-variable 'mythic-chaos-level))

(defvar mythic-mode-map nil
  "Keys for mythic mode.")

(unless mythic-mode-map
  (let ((map (make-sparse-keymap)))
    (let (i)
      (dotimes (i 10)
	(define-key map (concat "\C-c" (number-to-string i)) 'mythic-call-register)))
    (define-key map (kbd "C-c C-o") 'mythic-odds-question)
    (define-key map (kbd "C-c C-c") 'mythic-odds-question)
    (define-key map (kbd "C-c C-r") 'mythic-resisted-question)
    (define-key map (kbd "C-c C-g") 'mythic-display-register)
    (define-key map (kbd "C-c C-d") 'mythic-dice)
    (define-key map (kbd "C-c C-l") 'mythic-display-log)
    (define-key map (kbd "C-c C-t a") 'mythic-add-thread)
    (define-key map (kbd "C-c C-t d") 'mythic-delete-thread)
    (define-key map (kbd "C-c C-t e") 'mythic-edit-threads)
    (define-key map (kbd "C-c C-p a") 'mythic-add-npc)
    (define-key map (kbd "C-c C-p d") 'mythic-delete-npc)
    (define-key map (kbd "C-c C-p e") 'mythic-edit-npcs)
    (define-key map (kbd "C-c C-a") 'mythic-add-scene)
    (define-key map (kbd "M-n") 'mythic-show-next-scene)
    (define-key map (kbd "M-p") 'mythic-show-prev-scene)
    (setq mythic-mode-map map)))

(defun mythic-show-next-scene ()
  "Display next scene."
  (interactive)
  (mythic-show-scene (lambda () (re-search-forward "\nScene: " nil t))))

(defun mythic-show-prev-scene ()
  "Display previous scene."
  (interactive)
  (mythic-show-scene (lambda ()  (re-search-backward "\\(\n\\|\\`\\)Scene: " nil t))))

(defun mythic-show-scene (finder)
  "Display scene found by FINDER.
FINDER is a function that moves point to the beginning of a scene."
  (let ((opoint (point)))
    (goto-char (point-min))
    (widen)
    (if (funcall finder)
	(progn
	  (narrow-to-page)
	  (goto-char (point-min)))
      (narrow-to-page)
      (goto-char opoint))))

(defun mythic-read-grade (rank)
  "Read a number from the minibuffer to specify the grade of an extreme RANK.
If RANK is the string miniscule2 or superhuman2 query the user for a
grade.  The baserank is then added to the entered grade.  For example,
if RANK is miniscule2 and the users enters 4, miniscule4 is
returned.  All other RANKs are returned unchanged."
  (if (string-match "\\(miniscule\\|superhuman\\)2" rank)
      (let* ((baserank (match-string 1 rank))
	     (grade (read-number (format "Grade for %s - must be 2 or higher: " baserank) 2)))
	(if (>= grade 2)
	    (concat baserank (number-to-string grade))
	  (error "Grade of %s must be 2 or higher" baserank)))
    rank))

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
  '((("miniscule2"    . ?M))
    (("miniscule"     . ?m) ("impossible"      . ?i))
    (("weak"          . ?k) ("no way"          . ?w))
    (("low"           . ?l) ("very unlikely"   . ?v))
    (("below-average" . ?b) ("unlikely"        . ?b))
    (("average"       . ?a) ("50/50"           . ?f))
    (("above-average" . ?o) ("somewhat likely" . ?s))
    (("high"          . ?h) ("likely"           .?k))
    (("exceptional"   . ?e) ("very likely"      .?l))
    (("incredible"    . ?i) ("near sure thing"  .?n))
    (("awesome"       . ?w) ("a sure thing"     .?a))
    (("superhuman"    . ?s) ("has to be"        .?h))
    (("superhuman2"   . ?S))))

(defun mythic-ranks-resisted ()
  "Return list of rank names for resisted questions."
  (mapcar 'caar mythic-ranks))

(defun mythic-ranks-odds ()
  "Return list of rank names for odds questions."
  (remove nil (mapcar 'caadr mythic-ranks)))

(defun mythic-ranks-simple ()
  "Return the rank table without any additional information like shortcuts etc."
  (mapcar (lambda (x) (mapcar 'car x )) mythic-ranks))

(defun mythic-rank-translate (rank)
  "Translate between RANK name for resisted and odds questions."
  (let ((list (find rank (mythic-ranks-simple) :test 'member)))
    (car (remove rank list))))

(defun mythic-rank-pos (difficulty)
  "Return DIFFICULTY as position in rank table."
  (position difficulty (mythic-ranks-simple) :test 'member))

(defun mythic-extreme-rank-modifier (rank)
  "Return the odds modifier for a RANK below miniscule2 or above superhuman2."
  (if (string-match "\\(miniscule\\|superhuman\\)\\([0-9]+\\)" rank)
      (let ((rank (match-string 1 rank))
	    (grade (- (string-to-number (match-string 2 rank)) 2)))
	(if (> grade 0)
	    (* (if (string= rank "miniscule") -1 1) grade 20)
	  0))
    0))

(defun mythic-truncate-rank (rank)
  "Truncates a RANK below miniscule2 to miniscule2 and ranks above superhuman2 to superhuman2. Return rank unchanged if its a member of the rank list."
  (if (string-match "\\(miniscule\\|superhuman\\)[0-9]+" rank)
      (concat (match-string 1 rank) "2")
    rank))

(defun mythic-chaos-level-rank (level)
  "Convert the numeric chaos LEVEL to its string representation."
  (cadr (nth (- 10 level) (mythic-ranks-simple))))

(defun mythic-read-rank (prompt type)
  "Prompt user for a rank.
PROMPT ist a string to prompt with.  TYPE selects which rank type to
use and can either of the symbols odd or resisted."
  (save-window-excursion
    (let ((collection (if (eq type 'resisted)
			  (mapcar 'car mythic-ranks)
			(remove nil (mapcar 'cadr mythic-ranks))))
	  (temp-buffer-show-hook '(fit-window-to-buffer)))
      (with-output-to-temp-buffer "*Mythic Choice*"
	(dolist (elt collection)
	  (princ (format "%c %s\n" (cdr elt) (car elt)))))
      (let ((rank (car (find (mythic-read-char prompt (mapcar 'cdr collection)) collection :test (lambda (elt list) (= (cdr list) elt))))))
	(mythic-read-grade rank)))))

(defun mythic-read-char (prompt chars)
  "Read character from minibuffer, prompting with string PROMPT.
CHARS is a list of possible characters."
  (let ((char))
    (while (progn
	     (setq char (read-char prompt))
	     (not (member char chars))))
    char))

(defmacro mythic-threshold (throw &rest clauses)
  (declare (indent 1))
  (let (result)
    (dolist (clause clauses result)
      (setq result (cons `((<= ,throw ,(car clause)) ,(cadr clause)) result)))
    `(cond ,@(nreverse result))))

(defun mythic-get-odds (acting difficulty)
  "Return the probabilty that ACTING versus DIFFICULTY succeedes."
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
    `((answer ,answer)
      (throw ,throw)
      (odds ,odds)
      (lower ,lower)
      (upper  ,(if (>= upper 100) 0 upper))
      (event ,(mythic-get-event throw)))))

(defun mythic-get-event (throw)
  "Return a random event if THROW meets the conditions.
See mythic-event-happend-p for an explanation when this happens."
  (when (mythic-event-happend-p throw)
      (mythic-random-event)))

(defun mythic-event-happend-p (throw)
  "Return true if THROW leads to an random event.
This happens if THROW consists of the same digit twice and this digit
is equal or lower than the chaos factor."
  (and (<= (/ throw 11) mythic-chaos-level)
       (= (% throw 11) 0)))

(defun mythic-get (alist &rest keys)
  "Return the the values of an alist specified by key.
If just one key is specified, return the value. For multiple keys,
return the the values as list."
  (let ((result
	 (mapcar (lambda (key) (cadr (assoc key alist))) keys)))
    (if (= 1 (length keys))
	(car result)
      result)))

(defun mythic-d100 ()
  "Return a random value between 1 and 100."
  (1+ (random 99)))

(defun mythic-event-focus ()
  "Return the focus of a random event."
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
  "Return a random thread."
  (mythic-random-element (mythic-get-list "Threads")))

(defun mythic-select-npc ()
  "Return a random npc."
  (mythic-random-element (mythic-get-list "NPCs")))

(defvar mythic-chaos-level 5
  "The chaos level associated with the buffer.
All odds questions are checked against this value.")

(defun mythic-format-answer (answer)
  "Display a short form of ANSWER in the minibuffer.
Also appends a record to the buffer *Mythic Log*."
  (let ((message (format "Answer: %s" (mythic-get answer 'answer)))
	(event (mythic-get answer 'event)))
    (if event
	(message "%s -- Event: %s" message event)
      (message message))
    (with-current-buffer (get-buffer-create "*Mythic Log*")
      (view-mode)
      (let (buffer-read-only)
	(goto-char (point-max))
	(insert (apply 'format "Odds: %d/%d/%d Throw: %d\n"
		       (mythic-get answer 'lower 'odds 'upper 'throw)))))))

(defun mythic-display-log ()
  "Display question log in other window."
  (interactive)
  (let ((buffer (get-buffer-create "*Mythic Log*")))
    (with-current-buffer buffer
      (view-mode)
      (display-buffer buffer))))

(defvar mythic-register (make-list 10 'nil))

(defun mythic-set-register (register &rest args)
  "Set REGISTER to ARGS.
ARGS should be a callable list."
  (let ((pos (mythic-numkey-to-pos (+ 48 (prefix-numeric-value register)))))
    (if (and pos (>= pos 0) (< pos 10))
	(setf (nth pos mythic-register) args))))

(defun mythic-numkey-to-pos (numkey)
  "Translate numeric key NUMKEY to its number value."
  (cond ((and (> numkey 48) (< numkey 58))
	 (- numkey 49))
	((= numkey 48)
	 9)
	((error "Registers must be between 0 and 9."))))

(defun mythic-pos-to-register (pos)
  "Translate postition POS in a list to its register number."
  (if (= pos 9)
      0
    (1+ pos)))

(defun mythic-display-register ()
  "Display a list of all registers in other buffer."
  (interactive)
  (let ((temp-buffer-show-hook '(fit-window-to-buffer)))
    (with-output-to-temp-buffer "*Mythic Register*"
      (dotimes (i (length mythic-register))
	(let ((register (mythic-pos-to-register i))
	      (element (nth i mythic-register)))
	  (if (null element)
	      (princ (format "%d. unused register\n" register))
	  (princ (format "%d. %S\n" register element))))))))

(defun mythic-call-register ()
  "Call a saved register function.
The register to call is determined by the last input character."
  (interactive)
  (let ((list (nth (mythic-numkey-to-pos last-input-char) mythic-register)))
    (if (and (listp list) (not (null list)))
	(apply (car list) (cdr list))
      (error "Register is empty."))))

(defun mythic-odds-question (acting)
  "Ask a odds question by crossreferencing ACTING against the current chaos level on the fate chart."
  (interactive (list (mythic-read-rank "Acting rank: " 'odds)))
  (mythic-set-register current-prefix-arg 'mythic-odds-question acting)
  (mythic-format-answer
   (mythic-ask-question acting (mythic-chaos-level-rank mythic-chaos-level))))

(defun mythic-resisted-question (acting difficulty)
  "Ask a resisted questiont by crossreferencing ACTING and DIFFICULTY on the fate chart."
  (interactive (list
		(mythic-read-rank "Acting rank: " 'resisted)
		(mythic-read-rank "Resisted rank: " 'resisted)))
  (mythic-set-register current-prefix-arg 'mythic-resisted-question acting difficulty)
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
  "Return a random element of LIST."
  (nth (random (1- (length list))) list))

(defun mythic-random-event ()
  "Return a random event by combining a focus with an action and a subject."
  (concat
   (mythic-event-focus)
   " - "
   (mythic-random-element mythic-event-actions)
   "/"
   (mythic-random-element mythic-event-subjects)))

(defvar mythic-dice-history nil
  "History list for mythic-dice.")

(eval-after-load "savehist"
  '(add-to-list 'savehist-additional-variables 'mythic-dice-history))

(defun mythic-dice (dice-spec)
  "Roll dice according to DICE-SPEC.
Possible values are for example for dice-spec are d20, 4d20 or 2d20+4."
  (interactive (list (read-from-minibuffer "Dice: " nil nil nil 'mythic-dice-history)))
  (if (string-match "\\s-*\\([[:digit:]]+\\)*\\s-*d\\([[:digit:]]+\\)?\\s-*\\([+-][[:digit:]]+\\)*" dice-spec)
      (let ((number (string-to-int (or (match-string 1 dice-spec) "1")))
	    (sides (string-to-int (or (match-string 2 dice-spec) "6")))
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

(defun mythic-increase-chaos-factor ()
  "Increase mythic-chaos-level by one to an upper limit of ten."
  (unless (= mythic-chaos-level 10)
    (incf mythic-chaos-level)))

(defun mythic-decrease-chaos-factor ()
  "Decrease mythic-chaos-level by one to a lower limit of one."
  (unless (= mythic-chaos-level 1)
    (decf mythic-chaos-level)))

(defun mythic-add-scene ()
  "Set the next scene by updating the chaos level, prompting for the scene setup and modifiying all lists."
  (interactive)
  (if (y-or-n-p "Increase chaos factor? ")
      (mythic-increase-chaos-factor)
    (mythic-decrease-chaos-factor))
  (let ((setup (read-string "Scene setup: "))
	(roll (mythic-dice "d10")))
    (if (<= roll mythic-chaos-level)
	(if (oddp roll)
	    (setq setup (read-string "Altered scene. New scene setup: "))
	  (setq setup (read-string
		       (concat "Interrupt scene: "
			       (mythic-random-event)
			       ". New scene setup: ")))))
    (widen)
    (goto-char (point-min))
    (while (re-search-forward "^Scene: " nil t))
    (beginning-of-line)
    (when (looking-at "^Scene: ")
      (search-forward ""))
    (insert (format "\nScene setup: %s (Chaos: %d)\n\n\n\n"
		    setup mythic-chaos-level))
    (goto-char (- (point) 3))
    (narrow-to-page)))

(defun mythic-get-list (list)
  "Return element of LIST.
LISTS must be on its own page and start with the string List:
LIST. Elements are specified by a line starting with wildcard
characters."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward (concat "List: " list) nil t)
	(narrow-to-page)
	(let ((elts))
	  (while (re-search-forward "^[*]\\s-*\\(.*\\)" nil t)
	    (push (match-string 1) elts))
	  elts)))))

(defun mythic-delete-list-element (list elt)
  "Delete list element specified by LIST and ELT.
See mythic-get-list for an description of lists."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (search-forward (concat "List: " list) nil t)
	(narrow-to-page)
	(let ((kill-whole-line t))
	  (when (re-search-forward (concat "^[*]\\s-*" elt) nil t)
	    (move-beginning-of-line nil)
	    (kill-line)))))))

(defun mythic-delete-thread (thread)
  "Close a open THREAD."
  (interactive (list (completing-read "Thread: " (mythic-get-list "Threads"))))
  (mythic-delete-list-element "Threads" thread))

(defun mythic-delete-npc (npc)
  "Remove a NPC from the NPC list."
  (interactive (list (completing-read "NPC: " (mythic-get-list "NPCs"))))
  (mythic-delete-list-element "NPCs" npc))

(defun mythic-add-list-element (list elt)
  "Add list element specified by LIST and ELT.
See mythic-get-list for an description of lists."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (if (search-forward (concat "List: " list) nil t)
	  (progn
	    (narrow-to-page)
	    (while (re-search-forward "^[*]\\s-*\\(.*\\)" nil t)
	      nil)
	    (insert "\n* " elt))
	(goto-char (point-max))
	(insert "\n" ? "\nList: " list "\n\n* " elt)))))

(defun mythic-add-thread (thread)
  "Add a new THREAD."
  (interactive "sThread: ")
  (mythic-add-list-element "Threads" thread))

(defun mythic-add-npc (npc)
  "Add a new NPC."
  (interactive "sNPC: ")
  (mythic-add-list-element "NPCs" npc))

(defun mythic (file)
  "Visit a mythic FILE or create a new one if none already exists."
  (interactive "FFind adventure file: ")
  (find-file file)
  (if (not (file-exists-p (buffer-file-name)))
      (let ((prompt "Scene: "))
	(when (y-or-n-p "Random adventure setup? ")
	  (setq prompt (format "Scene setup (%s): " (mythic-random-event))))
	(insert (format "Scene: %s (Chaos: %d)\n\n\n"
			(read-string prompt) mythic-chaos-level))
	(goto-char (- (point) 3)))
    (while (re-search-forward "^Scene: ")))
  (narrow-to-page)
  (mythic-mode))

(defun mythic-edit-list (list)
  "Open list in a new buffer and select it.
See mythic-get-list for an description of lists."
  (let* ((buffer-name (format "*%s - %s*" (buffer-name) list))
	 (buffer (or (get-buffer buffer-name) (make-indirect-buffer (current-buffer) buffer-name t))))
    (with-current-buffer buffer
      (widen)
      (goto-char (point-min))
      (when (search-forward (concat "List: " list) nil t)
	(narrow-to-page)
	(pop-to-buffer buffer)))))

(defun mythic-edit-threads ()
  "Edit thread list in other window."
  (interactive)
  (mythic-edit-list "Threads"))

(defun mythic-edit-npcs ()
  "Edit npc list in other window."
  (interactive)
  (mythic-edit-list "NPCs"))

;; focusArray[1] = new Array("4/Horror: the game starts with the Chaos Factor set to 4.  Chaos can only increase not decrease.  When random events are generated results of 1 to 3 within the Chaos Factor are altered scenes.  Any higher numbers will be interrupts."  "1/10/Horror - PC"  "11/23/Horror - NPC"  "24/30/Remote Event"  "31/49/NPC action"  "50/52/Introduce an NPC"  "53/55/Move toward a thread"  "56/62/Move away from a thread"  "63/72/PC Negative"  "73/75/PC positive"  "76/82/Ambiguous event"  "83/97/NPC negative"  "98/100/NPC positive");

;; focusArray[2] = new Array("5/Action adventure: Double rolls on the Fate chart always result in a random event whether they fall within the Chaos Facotr range or not.  Chaos cannot fall below 5.  Any scene which would normally lower the Chaos below 5 leaves it unchanged."  "1/16/Action!"  "17/24/Remote event"  "25/44/NPC action"  "45/52/Introduce an NPC"  "53/56/Move toward a thread"  "57/64/Move away from a thread"  "65/76/PC negative"  "77/80/PC positive"  "81/84/Ambiguous event"  "85/96/NPC negative"  "97/100/NPC positive")

;; focusArray[3] = new Array("5/Mystery: the Chaos Factor cannot fall below 3.  Any scene which would normally lower the Chaos below 3 leaves it unchanged."  "1/8/Remote event"  "9/20/NPC action"  "21/32/Introduce an NPC"  "33/52/Move toward a thread"  "53/64/Move away from a thread"  "65/72/PC negative"  "73/80/PC positive"  "81/88/Ambiguous event"  "89/96/NPC negative"  "97/100/NPC positive")

;; focusArray[4] = new Array("5/Social game: this game uses standard Chaos rules."  "1/12/Drop a bomb!"  "13/24/Remote event"  "25/36/NPC action"  "37/44/Introduce an NPC"  "45/56/Move toward a thread"  "57/60/Move away from a thread"  "61/64/Close a thread"  "65/72/PC negative"  "73/80/PC positive"  "81/92/Ambiguous event"  "93/96/NPC negative"  "7/100/NPC positive")

;; focusArray[5] = new Array("5/Personal game: this game uses standard Chaos rules."  "1/7/Remote event"  "8/24/NPC action"  "25/28/PC NPC action"  "29/35/Introduce an NPC"  "36/42/Move toward a thread"  "43/45/Move toward a PC thread"  "46/50/Move away from a thread"  "51/52/Move away from a PC thread"  "53/54/Close thread"  "55/55/Close PC thread"  "56/67/PC negative"  "68/75/PC positive"  "76/83/Ambiguous event"  "84/90/NPC negative"  "91/92/PC NPC negative"  "93/99/NPC positive"  "100/100/PC NPC positive")

;; focusArray[6] = new Array("5/Epic game: the Chaos Factor cannot fall below 3.  Any scene which would normally lower the Chaos below 3 leaves it unchanged."  "1/12/Thread escalates"  "13/16/Remote event"  "17/30/NPC action"  "31/42/Introduce an NPC"  "43/46/Move toward a thread"  "47/58/Move away from a thread"  "59/72/PC negative"  "73/80/PC positive"  "81/84/Ambiguous event"  "85/92/NPC negative"  "93/100/NPC positive")

(provide 'mythic)

;;; mythic.el ends here
