FILES = examples/small.vl abstract-eval.scm code-generator.scm macro.scm eval.scm primitives.scm syntax.scm data.scm env.scm analysis.scm abstract-values.scm nomenclature.scm load.scm

workbook: $(FILES)
	enscript -M a4 -fCourier-Bold12 -o workbook-a4.ps --file-align=2 README  --color --highlight $(FILES)
	enscript -M letter -fCourier-Bold12 -o workbook-letter.ps --file-align=2 README  --color --highlight $(FILES)

.PHONY: workbook
