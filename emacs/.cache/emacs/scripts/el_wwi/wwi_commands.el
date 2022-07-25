	;; Course Code	Notes	Assignments	Student List
;; 1	VFX Theory - II	FMAV 2101	Add/Edit	Add/Edit	View
;; 2	VFX Theory - III	FMAV 2102	Add/Edit	Add/Edit	View
;; 3	VFX Practical - II	FMAV 2103	Add/Edit	Add/Edit	View
;; 4	Night Shoot Project - AV	FMAV 2104	Add/Edit	Add/Edit	View
;; 5	Filmmaking Basics I	FMBG 1101	Add/Edit	Add/Edit	View
;; 6	Basic Compositing	FMBV 2107	Add/Edit	Add/Edit	View
;; 7	Introduction to Houdini	ANB3 2109



;; (require 'org)
(setq wwi-subject-alist '(("VFX Theory - II"         "FMAV 2101")
			  ("VFX Theory - III"        "FMAV 2102")
			  ("VFX Practical - II"      "FMAV 2103")
			  ("Night Shoot Project"     "FMAV 2104")
			  ("Filmmaking Basics I"     "FMBG 1101")
			  ("Basic Compositing"       "FMBV 2107")
			  ("Introduction to Houdini" "ANB3 2109")
			  ("Dynamics in Houdini"     "ANB3 2110")
			  ) )

(defun wwi-commands ()
  "A wwi related addon"
  (interactive)
  (let (curret-wwi-selected (completing-read "My Prompt: " '("VFX Theory - II"
							     "VFX Theory - III"
							     "VFX Practical - II"
							     "Night Shoot Project"
							     "Filmmaking Basics I"
							     "Basic Compositing"
							     "Introduction to Houdini"
							     "Dynamics in Houdini"
							     ) nil nil)))
  (message current-wwi-selected)
  )

(provide 'wwi-commands)
