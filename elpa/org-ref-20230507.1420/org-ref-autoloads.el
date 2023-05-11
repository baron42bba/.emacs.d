;;; org-ref-autoloads.el --- automatically extracted autoloads  -*- lexical-binding: t -*-
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "contrib" "contrib.el" (0 0 0 0))
;;; Generated autoloads from contrib.el

(register-definition-prefixes "contrib" '("org-ref-"))

;;;***

;;;### (autoloads nil "doi-utils" "doi-utils.el" (0 0 0 0))
;;; Generated autoloads from doi-utils.el

(autoload 'doi-utils-async-download-pdf "doi-utils" "\
Download the PDF for bibtex entry at point asynchronously.
It is not fully async, only the download is. Fully async is
harder because you need to run `doi-utils-get-pdf-url' async
too. " t nil)

(autoload 'doi-utils-get-bibtex-entry-pdf "doi-utils" "\
Download pdf for entry at point if the pdf does not already exist locally.
The entry must have a doi. The pdf will be saved, by the name
%s.pdf where %s is the bibtex label. Files will not be
overwritten. The pdf will be checked to make sure it is a pdf,
and not some html failure page. You must have permission to
access the pdf. We open the pdf at the end if
`doi-utils-open-pdf-after-download' is non-nil.

With one prefix ARG, directly get the pdf from a file (through
`read-file-name') instead of looking up a DOI. With a double
prefix ARG, directly get the pdf from an open buffer (through
`read-buffer-to-switch') instead. These two alternative methods
work even if the entry has no DOI, and the pdf file is not
checked.

\(fn &optional ARG)" t nil)

(autoload 'doi-utils-add-bibtex-entry-from-doi "doi-utils" "\
Add DOI entry to end of a file in the current directory.
Pick the file ending with .bib or in .  If you have an active region that
starts like a DOI, that will be the initial prompt.  If no region
is selected and the first entry of the ‘kill-ring’ starts like a
DOI, then that is the initial prompt.  Otherwise, you have to type
or paste in a DOI.
Argument BIBFILE the bibliography to use.

\(fn DOI &optional BIBFILE)" t nil)

(autoload 'doi-utils-doi-to-org-bibtex "doi-utils" "\
Convert a DOI to an ‘org-bibtex’ form and insert it at point.

\(fn DOI)" t nil)

(autoload 'bibtex-set-field "doi-utils" "\
Set FIELD to VALUE in bibtex file.  create field if it does not exist.
Optional argument NODELIM see `bibtex-make-field'.

\(fn FIELD VALUE &optional NODELIM)" t nil)

(autoload 'doi-utils-update-bibtex-entry-from-doi "doi-utils" "\
Update fields in a bibtex entry from the DOI.
Every field will be updated, so previous change will be lost.

\(fn DOI)" t nil)

(autoload 'doi-utils-update-field "doi-utils" "\
Update the field at point in the bibtex entry.
Data is retrieved from the doi in the entry." t nil)

(autoload 'doi-utils-wos "doi-utils" "\
Open Web of Science entry for DOI.

\(fn DOI)" t nil)

(autoload 'doi-utils-wos-citing "doi-utils" "\
Open Web of Science citing articles entry for DOI.
May be empty if none are found.

\(fn DOI)" t nil)

(autoload 'doi-utils-wos-related "doi-utils" "\
Open Web of Science related articles page for DOI.

\(fn DOI)" t nil)

(autoload 'doi-utils-ads "doi-utils" "\
Open ADS entry for DOI

\(fn DOI)" t nil)

(autoload 'doi-utils-open "doi-utils" "\
Open DOI in browser.

\(fn DOI)" t nil)

(autoload 'doi-utils-open-bibtex "doi-utils" "\
Search through variable `bibtex-completion-bibliography' for DOI.

\(fn DOI)" t nil)

(autoload 'doi-utils-crossref "doi-utils" "\
Search DOI in CrossRef.

\(fn DOI)" t nil)

(autoload 'doi-utils-google-scholar "doi-utils" "\
Google scholar the DOI.

\(fn DOI)" t nil)

(autoload 'doi-utils-pubmed "doi-utils" "\
Search Pubmed for the DOI.

\(fn DOI)" t nil)

(autoload 'doi-utils-crossref-citation-query "doi-utils" "\
Query Crossref with the title of the bibtex entry at point.
Get a list of possible matches. Choose one with completion." t nil)

(autoload 'doi-utils-debug "doi-utils" "\
Generate an org-buffer showing data about DOI.

\(fn DOI)" t nil)

(autoload 'doi-utils-add-entry-from-crossref-query "doi-utils" "\
Search Crossref with QUERY and use completion to select an entry to add to BIBTEX-FILE.

\(fn QUERY BIBTEX-FILE)" t nil)

(register-definition-prefixes "doi-utils" '("*doi-utils-" "agu-pdf-url" "aip-pdf-url" "aps-pdf-url" "asme-biomechanical-pdf-url" "chemistry-europe-pdf-url" "copernicus-" "crossref-add-bibtex-entry" "doi-" "ecs" "frontiers-pdf-url" "generic-full-pdf-url" "ieee" "iop-pdf-url" "jneurosci-pdf-url" "jstor-pdf-url" "linkinghub-elsevier-pdf-url" "nature-pdf-url" "osa-pdf-url" "pnas-pdf-url" "rsc-pdf-url" "sage-pdf-url" "science-" "siam-pdf-url" "springer-" "tandfonline-pdf-url" "wiley-pdf-url-2"))

;;;***

;;;### (autoloads nil "nist-webbook" "nist-webbook.el" (0 0 0 0))
;;; Generated autoloads from nist-webbook.el

(autoload 'nist-webbook-formula "nist-webbook" "\
Search NIST webbook for FORMULA.

\(fn FORMULA)" t nil)

(autoload 'nist-webbook-name "nist-webbook" "\
Search NIST webbook for NAME.

\(fn NAME)" t nil)

;;;***

;;;### (autoloads nil "openalex" "openalex.el" (0 0 0 0))
;;; Generated autoloads from openalex.el

(register-definition-prefixes "openalex" '("oa-" "org-ref-citation-hydra"))

;;;***

;;;### (autoloads nil "org-ref-arxiv" "org-ref-arxiv.el" (0 0 0 0))
;;; Generated autoloads from org-ref-arxiv.el

(autoload 'arxiv-add-bibtex-entry "org-ref-arxiv" "\
Add bibtex entry for ARXIV-NUMBER to BIBFILE.

\(fn ARXIV-NUMBER BIBFILE)" t nil)

(autoload 'arxiv-get-pdf "org-ref-arxiv" "\
Retrieve a pdf for ARXIV-NUMBER and save it to PDF.

\(fn ARXIV-NUMBER PDF)" t nil)

(autoload 'arxiv-get-pdf-add-bibtex-entry "org-ref-arxiv" "\
Add bibtex entry for ARXIV-NUMBER to BIBFILE.
Remove troublesome chars from the bibtex key, retrieve a pdf
for ARXIV-NUMBER and save it to PDFDIR with the same name of the
key.

\(fn ARXIV-NUMBER BIBFILE PDFDIR)" t nil)

(register-definition-prefixes "org-ref-arxiv" '("arxiv-"))

;;;***

;;;### (autoloads nil "org-ref-bibliography-links" "org-ref-bibliography-links.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-ref-bibliography-links.el

(register-definition-prefixes "org-ref-bibliography-links" '("org-ref-"))

;;;***

;;;### (autoloads nil "org-ref-bibtex" "org-ref-bibtex.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-ref-bibtex.el

(autoload 'org-ref-bibtex-generate-longtitles "org-ref-bibtex" "\
Generate longtitles.bib which are @string definitions.
The full journal names are in `org-ref-bibtex-journal-abbreviations'." t nil)

(autoload 'org-ref-bibtex-generate-shorttitles "org-ref-bibtex" "\
Generate shorttitles.bib which are @string definitions.
The abbreviated journal names in `org-ref-bibtex-journal-abbreviations'." t nil)

(autoload 'org-ref-stringify-journal-name "org-ref-bibtex" "\
Replace journal name in a bibtex entry with a string.
The strings are defined in
`org-ref-bibtex-journal-abbreviations'.  The optional arguments KEY,
START and END allow you to use this with `bibtex-map-entries'

\(fn &optional KEY START END)" t nil)

(autoload 'org-ref-set-journal-string "org-ref-bibtex" "\
Set a bibtex journal name to the string that represents FULL-JOURNAL-NAME.
This is defined in `org-ref-bibtex-journal-abbreviations'.

\(fn FULL-JOURNAL-NAME)" t nil)

(autoload 'org-ref-replace-nonascii "org-ref-bibtex" "\
Replace non-ascii characters with LaTeX representations in a
bibtex entry." t nil)

(autoload 'org-ref-title-case "org-ref-bibtex" "\
Convert a bibtex entry title and booktitle to title-case.
Convert only if the entry type is a member of the list
`org-ref-title-case-types'. The arguments KEY, START and END are
optional, and are only there so you can use this function with
`bibtex-map-entries' to change all the title entries in articles and
books.

\(fn &optional KEY START END)" t nil)

(autoload 'org-ref-title-case-article "org-ref-bibtex" "\
Convert a bibtex entry article or book title to title-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles and books.

\(fn &optional KEY START END)" t nil)

(autoload 'org-ref-sentence-case-article "org-ref-bibtex" "\
Convert a bibtex entry article title to sentence-case.
The arguments KEY, START and END are optional, and are only there
so you can use this function with `bibtex-map-entries' to change
all the title entries in articles.

\(fn &optional KEY START END)" t nil)

(autoload 'org-ref-bibtex-next-entry "org-ref-bibtex" "\
Jump to the beginning of the next bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries
forward.  Negative numbers do nothing.

\(fn &optional N)" t nil)

(autoload 'org-ref-bibtex-previous-entry "org-ref-bibtex" "\
Jump to beginning of the previous bibtex entry.
N is a prefix argument.  If it is numeric, jump that many entries back.

\(fn &optional N)" t nil)

(autoload 'org-ref-bibtex-visible-entry "org-ref-bibtex" "\
Jump to visible entry." t nil)

(autoload 'org-ref-bibtex-visible-field "org-ref-bibtex" "\
Jump to visible field." t nil)

(autoload 'org-ref-bibtex-format-url-if-doi "org-ref-bibtex" "\
Hook function to format url to follow the current DOI conventions." t nil)

(autoload 'org-ref-bibtex-wos "org-ref-bibtex" "\
Open bibtex entry in Web Of Science if there is a DOI." t nil)

(autoload 'org-ref-bibtex-wos-citing "org-ref-bibtex" "\
Open citing articles for bibtex entry in Web Of Science if
there is a DOI." t nil)

(autoload 'org-ref-bibtex-wos-related "org-ref-bibtex" "\
Open related articles for bibtex entry in Web Of Science if
there is a DOI." t nil)

(autoload 'org-ref-bibtex-crossref "org-ref-bibtex" "\
Open the bibtex entry in Crossref by its doi." t nil)

(autoload 'org-ref-bibtex-google-scholar "org-ref-bibtex" "\
Open the bibtex entry at point in google-scholar by its doi." t nil)

(autoload 'org-ref-bibtex-pubmed "org-ref-bibtex" "\
Open the bibtex entry at point in Pubmed by its doi." t nil)

(autoload 'org-ref-bibtex-pdf "org-ref-bibtex" "\
Open the pdf for the bibtex entry at point.
Thin wrapper to get `org-ref-bibtex' to open pdf, because it
calls functions with a DOI argument.

\(fn &optional _)" t nil)

(autoload 'org-ref-bibtex-assoc-pdf-with-entry "org-ref-bibtex" "\
Prompt for pdf associated with entry at point and rename it.
Check whether a pdf already exists in `bibtex-completion-library' with the
name '[bibtexkey].pdf'. If the file does not exist, rename it to
'[bibtexkey].pdf' using
`org-ref-bibtex-assoc-pdf-with-entry-move-function' and place it in
a directory. Optional PREFIX argument toggles between
`rename-file' and `copy-file'.

\(fn &optional PREFIX)" t nil)

(autoload 'org-ref-email-bibtex-entry "org-ref-bibtex" "\
Email current bibtex entry at point and pdf if it exists." t nil)

(autoload 'org-ref-set-bibtex-keywords "org-ref-bibtex" "\
Add KEYWORDS to a bibtex entry.
If KEYWORDS is a list, it is converted to a comma-separated
string.  The KEYWORDS are added to the beginning of the
field.  Otherwise KEYWORDS should be a string of comma-separate
keywords.  Optional argument ARG prefix arg to replace keywords.

\(fn KEYWORDS &optional ARG)" t nil)

(autoload 'org-ref-extract-bibtex-blocks "org-ref-bibtex" "\
Extract all bibtex blocks in buffer to BIBFILE.
If BIBFILE exists, append, unless you use a prefix arg (C-u), which
will clobber the file.

\(fn BIBFILE)" t nil)

(autoload 'org-ref-open-bibtex-pdf "org-ref-bibtex" "\
Open pdf for a bibtex entry, if it exists." t nil)

(autoload 'org-ref-open-bibtex-notes "org-ref-bibtex" "\
From a bibtex entry, open the notes if they exist." t nil)

(autoload 'org-ref-open-in-browser "org-ref-bibtex" "\
Open the bibtex entry at point in a browser using the url field or doi field." t nil)

(autoload 'org-ref-build-full-bibliography "org-ref-bibtex" "\
Build pdf of all bibtex entries, and open it." t nil)

(autoload 'org-ref-sort-bibtex-entry "org-ref-bibtex" "\
Sort fields of entry in standard order." t nil)

(autoload 'org-ref-downcase-bibtex-entry "org-ref-bibtex" "\
Downcase the entry type and fields." t nil)

(autoload 'org-ref-clean-bibtex-entry "org-ref-bibtex" "\
Clean and replace the key in a bibtex entry.
See functions in `org-ref-clean-bibtex-entry-hook'." t nil)

(register-definition-prefixes "org-ref-bibtex" '("orcb-" "org-ref-"))

;;;***

;;;### (autoloads nil "org-ref-citation-links" "org-ref-citation-links.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-ref-citation-links.el

(autoload 'org-ref-delete-citation-at-point "org-ref-citation-links" "\
Delete the citation or reference at point." t nil)

(autoload 'org-ref-replace-citation-at-point "org-ref-citation-links" "\
Replace the citation at point." t nil)

(autoload 'org-ref-edit-pre-post-notes "org-ref-citation-links" "\
Edit the pre/post notes at point.

if you are not on a key, or with optional prefix
arg COMMON, edit the common prefixes instead.

\(fn &optional COMMON)" t nil)

(autoload 'org-ref-change-cite-type "org-ref-citation-links" "\
Change the cite type of citation link at point." t nil)

(autoload 'org-ref-sort-citation-link "org-ref-citation-links" "\
Replace link at point with sorted link by year." t nil)

(autoload 'org-ref-next-key "org-ref-citation-links" "\
Move cursor to the next cite key when on a cite link.
Otherwise run `right-word'. If the cursor moves off the link,
move to the beginning of the next cite link after this one." t nil)

(autoload 'org-ref-previous-key "org-ref-citation-links" "\
Move cursor to the previous cite key when on a cite link.
Otherwise run `left-word'. If the cursor moves off the link,
move to the beginning of the previous cite link after this one." t nil)

(autoload 'org-ref-jump-to-visible-key "org-ref-citation-links" "\
Jump to a visible key with avy." t nil)

(autoload 'org-ref-insert-cite-link "org-ref-citation-links" "\
Insert a cite link with completion.
Optional prefix arg SET-TYPE to choose the cite type.

\(fn &optional SET-TYPE)" t nil)

(register-definition-prefixes "org-ref-citation-links" '("org-ref-"))

;;;***

;;;### (autoloads nil "org-ref-compat" "org-ref-compat.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-ref-compat.el

(register-definition-prefixes "org-ref-compat" '("org-ref-to-org-cite"))

;;;***

;;;### (autoloads nil "org-ref-core" "org-ref-core.el" (0 0 0 0))
;;; Generated autoloads from org-ref-core.el

(autoload 'org-ref-insert-link "org-ref-core" "\
Insert an org-ref link.
If no prefix ARG insert a cite.
If one prefix ARG insert a ref.
If two prefix ARGs insert a label.

This is a generic function. Specific backends might
provide their own version.

\(fn ARG)" t nil)

(autoload 'org-ref-help "org-ref-core" "\
Open the `org-ref' manual." t nil)

(register-definition-prefixes "org-ref-core" '("org-ref-"))

;;;***

;;;### (autoloads nil "org-ref-export" "org-ref-export.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-ref-export.el

(register-definition-prefixes "org-ref-export" '("org-ref"))

;;;***

;;;### (autoloads nil "org-ref-glossary" "org-ref-glossary.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-ref-glossary.el

(autoload 'org-ref-add-glossary-entry "org-ref-glossary" "\
Insert a new glossary entry.
LABEL is how you refer to it with links.
NAME is the name of the entry to be defined.
DESCRIPTION is the definition of the entry.
Entry gets added after the last #+latex_header line.

This is not a preferred way to add entries. It is preferred to
manually add them to the glossary table.

\(fn LABEL NAME DESCRIPTION)" t nil)

(autoload 'org-ref-add-acronym-entry "org-ref-glossary" "\
Add an acronym entry with LABEL.
  ABBRV is the abbreviated form.
  FULL is the expanded acronym.

This is not the preferred way to add acronyms, you should add
them manually to the acronyms table.

\(fn LABEL ABBRV FULL)" t nil)

(register-definition-prefixes "org-ref-glossary" '("or-" "org-ref-"))

;;;***

;;;### (autoloads nil "org-ref-helm" "org-ref-helm.el" (0 0 0 0))
;;; Generated autoloads from org-ref-helm.el

(autoload 'org-ref-cite-insert-helm "org-ref-helm" "\
Search BibTeX entries.

With a prefix ARG, the cache is invalidated and the bibliography
reread.

If LOCAL-BIB is non-nil, display that the BibTeX entries are read
from the local bibliography.  This is set internally by
`helm-bibtex-with-local-bibliography'.

If INPUT is non-nil and a string, that value is going to be used
as a predefined search term.  Can be used to define functions for
frequent searches (e.g. your own publications).

\(fn &optional ARG LOCAL-BIB INPUT)" t nil)

(register-definition-prefixes "org-ref-helm" '("org-ref-helm-source-"))

;;;***

;;;### (autoloads nil "org-ref-isbn" "org-ref-isbn.el" (0 0 0 0))
;;; Generated autoloads from org-ref-isbn.el

(autoload 'org-ref-isbn-clean-bibtex-entry "org-ref-isbn" "\
Clean a bibtex entry inserted via `isbn-to-bibtex'.
See functions in `org-ref-isbn-clean-bibtex-entry-hook'." t nil)

(autoload 'isbn-to-bibtex-lead "org-ref-isbn" "\
Search lead.to for ISBN bibtex entry.
You have to copy the entry if it is on the page to your bibtex
file.

\(fn ISBN)" t nil)

(autoload 'isbn-to-bibtex "org-ref-isbn" "\
Get bibtex entry for ISBN and insert it into BIBFILE.
Nothing happens if an entry with the generated key already exists
in the file. Data comes from www.ebook.de.

\(fn ISBN BIBFILE)" t nil)

(register-definition-prefixes "org-ref-isbn" '("isbn-to-bibtex-open-library" "org-ref-isbn-" "oricb-"))

;;;***

;;;### (autoloads nil "org-ref-ivy" "org-ref-ivy.el" (0 0 0 0))
;;; Generated autoloads from org-ref-ivy.el

(register-definition-prefixes "org-ref-ivy" '("org-ref-"))

;;;***

;;;### (autoloads nil "org-ref-label-link" "org-ref-label-link.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-ref-label-link.el

(autoload 'org-ref-insert-label-link "org-ref-label-link" "\
Insert a new label with completion.
The completion helps ensure you use a unique label." t nil)

;;;***

;;;### (autoloads nil "org-ref-latex" "org-ref-latex.el" (0 0 0 0))
;;; Generated autoloads from org-ref-latex.el

(register-definition-prefixes "org-ref-latex" '("org-ref-"))

;;;***

;;;### (autoloads nil "org-ref-misc-links" "org-ref-misc-links.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-ref-misc-links.el

(autoload 'org-ref-list-of-figures "org-ref-misc-links" "\
Generate buffer with list of figures in them.
ARG does nothing.
Ignore figures in COMMENTED sections.

\(fn &optional ARG)" t nil)

(autoload 'org-ref-list-of-tables "org-ref-misc-links" "\
Generate a buffer with a list of tables.
ARG does nothing.

\(fn &optional ARG)" t nil)

(autoload 'org-ref-index "org-ref-misc-links" "\
Open an *index* buffer with links to index entries.
PATH is required for the org-link, but it does nothing here.

\(fn &optional PATH)" t nil)

(register-definition-prefixes "org-ref-misc-links" '("org-ref-idxproc"))

;;;***

;;;### (autoloads nil "org-ref-natbib-bbl-citeproc" "org-ref-natbib-bbl-citeproc.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-ref-natbib-bbl-citeproc.el

(register-definition-prefixes "org-ref-natbib-bbl-citeproc" '("org-"))

;;;***

;;;### (autoloads nil "org-ref-pdf" "org-ref-pdf.el" (0 0 0 0))
;;; Generated autoloads from org-ref-pdf.el

(autoload 'org-ref-pdf-to-bibtex "org-ref-pdf" "\
Add pdf of current buffer to bib file and save pdf. The pdf
should be open in Emacs using the `pdf-tools' package." t nil)

(autoload 'org-ref-pdf-debug-pdf "org-ref-pdf" "\
Try to debug getting a doi from a pdf.
Opens a buffer with the pdf converted to text, and `occur' on the
variable `org-ref-pdf-doi-regex'.

\(fn PDF-FILE)" t nil)

(autoload 'org-ref-pdf-crossref-lookup "org-ref-pdf" "\
Lookup highlighted text in PDFView in CrossRef." t nil)

(register-definition-prefixes "org-ref-pdf" '("org-ref-" "pdftotext-executable"))

;;;***

;;;### (autoloads nil "org-ref-pubmed" "org-ref-pubmed.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-ref-pubmed.el

(autoload 'pubmed-insert-bibtex-from-pmid "org-ref-pubmed" "\
Insert a bibtex entry at point derived from PMID.
You must clean the entry after insertion.

\(fn PMID)" t nil)

(autoload 'pubmed "org-ref-pubmed" "\
Open http://www.ncbi.nlm.nih.gov/pubmed in a browser." t nil)

(autoload 'pubmed-advanced "org-ref-pubmed" "\
Open http://www.ncbi.nlm.nih.gov/pubmed/advanced in a browser." t nil)

(autoload 'pubmed-simple-search "org-ref-pubmed" "\
Open QUERY in Pubmed in a browser.

\(fn QUERY)" t nil)

(autoload 'pubmed-clinical "org-ref-pubmed" "\
Open http://www.ncbi.nlm.nih.gov/pubmed/clinical in a browser." t nil)

(autoload 'pubmed-clinical-search "org-ref-pubmed" "\
Open QUERY in pubmed-clinical.

\(fn QUERY)" t nil)

(register-definition-prefixes "org-ref-pubmed" '("pubmed-"))

;;;***

;;;### (autoloads nil "org-ref-ref-links" "org-ref-ref-links.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-ref-ref-links.el

(register-definition-prefixes "org-ref-ref-links" '("org-ref-"))

;;;***

;;;### (autoloads nil "org-ref-refproc" "org-ref-refproc.el" (0 0
;;;;;;  0 0))
;;; Generated autoloads from org-ref-refproc.el

(register-definition-prefixes "org-ref-refproc" '("org-ref-"))

;;;***

;;;### (autoloads nil "org-ref-scifinder" "org-ref-scifinder.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-ref-scifinder.el

(autoload 'scifinder "org-ref-scifinder" "\
Open https://scifinder.cas.org/scifinder/view/scifinder/scifinderExplore.jsf in a browser." t nil)

;;;***

;;;### (autoloads nil "org-ref-scopus" "org-ref-scopus.el" (0 0 0
;;;;;;  0))
;;; Generated autoloads from org-ref-scopus.el

(autoload 'scopus-related-by-keyword-url "org-ref-scopus" "\
Return a Scopus url to articles related by keyword for DOI.

\(fn DOI)" t nil)

(autoload 'scopus-related-by-author-url "org-ref-scopus" "\
Return a Scopus url to articles related by author for DOI.

\(fn DOI)" t nil)

(autoload 'scopus-related-by-references-url "org-ref-scopus" "\
Return a Scopus url to articles related by references for DOI.

\(fn DOI)" t nil)

(autoload 'scopus-open-eid "org-ref-scopus" "\
Open article with EID in browser.

\(fn EID)" t nil)

(autoload 'scopus-basic-search "org-ref-scopus" "\
Open QUERY as a basic title-abstract-keyword search at scopus.com.

\(fn QUERY)" t nil)

(autoload 'scopus-advanced-search "org-ref-scopus" "\
Open QUERY as an advanced search at scopus.com.

\(fn QUERY)" t nil)

(register-definition-prefixes "org-ref-scopus" '("*hydra-eid*" "*scopus-api-key*" "scopus"))

;;;***

;;;### (autoloads nil "org-ref-url-utils" "org-ref-url-utils.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-ref-url-utils.el

(autoload 'org-ref-url-debug-url "org-ref-url-utils" "\
Open a buffer to URL with all doi patterns highlighted.

\(fn URL)" t nil)

(autoload 'org-ref-url-html-to-bibtex "org-ref-url-utils" "\
Convert URL to a bibtex or biblatex entry in BIBFILE.
If URL is the first in the kill ring, use it. Otherwise, prompt for
one in the minibuffer.

\(fn BIBFILE &optional URL)" t nil)

(register-definition-prefixes "org-ref-url-utils" '("org-ref-"))

;;;***

;;;### (autoloads nil "org-ref-utils" "org-ref-utils.el" (0 0 0 0))
;;; Generated autoloads from org-ref-utils.el

(autoload 'org-ref-version "org-ref-utils" "\
Provide a version string for org-ref.
Copies the string to the clipboard." t nil)

(autoload 'org-ref-debug "org-ref-utils" "\
Print some debug information to a buffer." t nil)

(autoload 'org-ref-open-pdf-at-point "org-ref-utils" "\
Open the pdf for bibtex key under point if it exists." t nil)

(autoload 'org-ref-add-pdf-at-point "org-ref-utils" "\
Add the pdf for bibtex key under point if it exists.

Similar to org-ref-bibtex-assoc-pdf-with-entry prompt for pdf
associated with bibtex key at point and rename it.  Check whether a
pdf already exists in `bibtex-completion-library' with the name
'[bibtexkey].pdf'. If the file does not exist, rename it to
'[bibtexkey].pdf' using
`org-ref-bibtex-assoc-pdf-with-entry-move-function' and place it
in a directory. Optional PREFIX argument toggles between
`rename-file' and `copy-file'.

\(fn &optional PREFIX)" t nil)

(autoload 'org-ref-open-url-at-point "org-ref-utils" "\
Open the url for bibtex key under point." t nil)

(autoload 'org-ref-open-notes-at-point "org-ref-utils" "\
Open the notes for bibtex key under point in a cite link in a buffer.
Can also be called with THEKEY in a program.

\(fn &optional THEKEY)" t nil)

(autoload 'org-ref-open-citation-at-point "org-ref-utils" "\
Open bibtex file to key at point." t nil)

(autoload 'org-ref-copy-entry-as-summary "org-ref-utils" "\
Copy the bibtex entry for the citation at point as a summary." t nil)

(autoload 'org-ref-ads-at-point "org-ref-utils" "\
Open the doi in ADS for bibtex key under point." t nil)

(autoload 'org-ref-wos-at-point "org-ref-utils" "\
Open the doi in wos for bibtex key under point." t nil)

(autoload 'org-ref-wos-citing-at-point "org-ref-utils" "\
Open the doi in wos citing articles for bibtex key under point." t nil)

(autoload 'org-ref-wos-related-at-point "org-ref-utils" "\
Open the doi in wos related articles for bibtex key under point." t nil)

(autoload 'org-ref-google-scholar-at-point "org-ref-utils" "\
Search google scholar for bibtex key under point using the title." t nil)

(autoload 'org-ref-biblio-at-point "org-ref-utils" "\
Do a biblio search for bibtex key under point using the title." t nil)

(autoload 'org-ref-pubmed-at-point "org-ref-utils" "\
Open the doi in pubmed for bibtex key under point." t nil)

(autoload 'org-ref-crossref-at-point "org-ref-utils" "\
Open the doi in crossref for bibtex key under point." t nil)

(autoload 'org-ref-email-at-point "org-ref-utils" "\
Email the citation(s) at point." t nil)

(autoload 'org-ref-find-non-ascii-characters "org-ref-utils" "\
Find non-ascii characters in the buffer.  Useful for cleaning up bibtex files." t nil)

(autoload 'org-ref-extract-bibtex-to-file "org-ref-utils" "\
Extract all bibtex entries for citations buffer to BIBFILE.
If BIBFILE exists, append, unless you use a prefix arg (C-u),
which will CLOBBER the file.

\(fn BIBFILE &optional CLOBBER)" t nil)

(autoload 'org-ref-extract-bibtex-entries "org-ref-utils" "\
Extract the bibtex entries in the current buffer into a bibtex src block." t nil)

(register-definition-prefixes "org-ref-utils" '("ords" "org-ref"))

;;;***

;;;### (autoloads nil "org-ref-worldcat" "org-ref-worldcat.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-ref-worldcat.el

(register-definition-prefixes "org-ref-worldcat" '("worldcat-query-all"))

;;;***

;;;### (autoloads nil "org-ref-wos" "org-ref-wos.el" (0 0 0 0))
;;; Generated autoloads from org-ref-wos.el

(autoload 'wos-search "org-ref-wos" "\
Open the word at point or selection in Web of Science as a topic query." t nil)

(autoload 'wos "org-ref-wos" "\
Open Web of Science search page in a browser." t nil)

(register-definition-prefixes "org-ref-wos" '("*wos-" "wos-"))

;;;***

;;;### (autoloads nil "x2bib" "x2bib.el" (0 0 0 0))
;;; Generated autoloads from x2bib.el

(autoload 'ris2bib "x2bib" "\
Convert RISFILE to bibtex and insert at point.
Without a prefix arg, stderr is diverted.
If VERBOSE is non-nil show command output.
If the region is active, assume it is a ris entry
and convert it to bib format in place.

\(fn RISFILE &optional VERBOSE)" t nil)

(autoload 'medxml2bib "x2bib" "\
Convert MEDFILE (in Pubmed xml) to bibtex and insert at point.
Without a prefix arg, stderr is diverted.
Display output if VERBOSE is non-nil.

\(fn MEDFILE &optional VERBOSE)" t nil)

(autoload 'clean-entries "x2bib" "\
Map over bibtex entries and clean them." t nil)

;;;***

;;;### (autoloads nil nil ("org-ref-pgk.el" "org-ref-pkg.el" "org-ref-sci-id.el"
;;;;;;  "org-ref.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-ref-autoloads.el ends here
