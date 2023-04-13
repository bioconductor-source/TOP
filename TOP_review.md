@Harry25R - Thank you for submitting to _Bioconductor_. Please see the initial review of
the package below. The required changes must be made while the suggested
changes do not have to be (though we strongly encourage them). Comment back
here with updates that have been made and when the package is ready for a
re-review.
Keep in mind that the deadline to accept new packages into the Bioconductor 3.17
release is 4/19. Any packages accepted after this date will be included in the
next release.

## DESCRIPTION

- [x] SUGGESTION: Consider adding the maintainer's ORCID iD in 'Authors@R' with
  'comment=c(ORCID-"...")'.
Added Harry's ORCID ID.
- [x] REQUIRED: 'LazyData:' in the 'DESCRIPTION' should be set to false or 
removed.
LazyData is set to false.
- [x] REQUIRED: The 'Data:' field should be removed as this isn't usual practice
  for Bioconductor packages.
Removed Data.
- [x] REQUIRED: The following packages should be added to 'Suggests:' since they
  are called/used in the vignette.
	'Biobase', 'curatedOvarianData', 'ggbeeswarm', 'ggsci', 'survminer',
        'tidyverse'
Added suggestions to the Description file.

## NAMESPACE

- [ ] REQUIRED: The package 'statmod' is listed as a Depends but is not imported
  from in the NAMESPACE file. This should be added.
- [x] SUGGESTION: Consider adding `importFrom("stats", "coef", "offset", "sd")`.
Added importFrom("stats", "coef", "offset", "sd").
- [ ] REQUIRED: The '%>%' is has both internal and export listed in its
  documentation. I would not export this function.

## NEWS

- [ ] SUGGESTION: Consider adding a NEWS file, so your package news will be 
included in Bioconductor release announcements.

## Man pages

- [ ] SUGGESTION: We encourage having a package man page with an overview of the
  package and links to the main functions. Users should be able to have a
relevant page display with `?TOP`.
- [ ] REQURIED: There should be runnable examples to man pages that document
  exported objects. The man page for calculateCCA does not have a runnable
example.
- [ ] REQUIRED: The use of donttest and dontrun is discouraged and generally not
  allowed; exceptions can be made with proper justification. If this option is
used it is preferable to use donttest instead of dontrun. donttest requires
valid R code while dontrun does not.

## Unit tests

- [ ] SUGGESTION: Consider adding unit tests. We strongly encourage them. See
  https://contributions.bioconductor.org/tests.html.

## R code

- [ ] REQUIRED: Undefined global functions or variables:
        . Feature Features Organ Pathways coef coef_abs freq from lambda.min n
        name offset score sd to value variable
- [ ] REQUIRED: Avoid `sapply()`; use `vapply()` instead.
- [ ] REQUIRED: Avoid `1:...`; use `seq_len()` or `seq_along()` if possible.
- [ ] REQUIRED: Avoid 'cat' and 'print' outside of 'show' methods.
- [ ] REQUIRED: Avoid using '=' for assignment and use '<-' instead.
- [ ] REQUIRED: Avoid the use of 'paste' in condition signals.
- [ ] REQUIRED: Avoid 'suppressWarnings'/'Messages' if possible (found 1 times)
- [ ] SUGGESTION: For formating reasons, consider shorter lines. There are 110 
lines that are > 80 characters long.
- [ ] SUGGESTION: For formating reasons, consider multiples of 4 spaces for line
 indents. There are 111 lines that are not.

Best,
Kayla
