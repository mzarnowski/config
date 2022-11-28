(require 'cl-lib)

;; workflow capture-thought:
;;  open new buffer. Buffer triggering the workflow is still visible
;; at any time, the thought can be discarded
;; a thought can be committed to the archive

;; workflow edit-thought:
;;  open a buffer with the selected thought. Buffer triggering the workflow is still visible
;; a change can be disacrded, diffed, committed or replaced in-situ
;; on commit, the new version is inserted with a backlink to the original. Old note has a link to the new version
;; on in-situ replacement, the note content is replaced. Useful for correcting the typos.

;; workflow refine-thought:
;;  show a buffer with the selected thought. Open another buffer, which allows the user to find a note to update with this thought. New notes can also be created. This note-buffer is our focus, we jump into the note creation workflow.
;;; note that it might as well be a note-related flow: note-dervice, where we select a thought and note to modify
;;; would "notes" be still a good name then?



